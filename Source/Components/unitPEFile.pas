(*======================================================================*
 | unitPEFile                                                           |
 |                                                                      |
 | Windows PE File Decoder unit                                         |
 |                                                                      |
 | Copyright(c) Colin Wilson 2001                                      |
 |                                                                      |
 | All rights reserved                                                  |
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ----------------------------------------- |
 | 1.0      19/10/2000  CPWW  Original                                  |
 | 1.1      31/05/2001  CPWW  Fixed crash when no resource Section.     |
 |                      CPWW  Fixed crash when 'VirtualSize' contains   |
 |                            0, and SizeOfRawData doesn't.             |
 *======================================================================*)

// A PE file looks like this...
//
//   [ DOS Header      ]     First word is 'MK'
//   [ COFF header     ]     Starts at DOSHdr._lfaNew.  First dword is COFF signature
//   [ Optional header ]     Follows COFF header.  First word is IMAGE_NT_OPTIONAL_HDR_MAGIC
//     [ Data Directory  ]   Really part of the optional header

//   [ Image Sections Headers ] Starts at optionalHeader + COFFHdr.SizeOfOptionalHeader

//   [ Section Data]         Each one pointed to by it's Image Section Header

unit unitPEFile;

interface

uses
  Windows, Classes, SysUtils, Contnrs, unitResourceDetails, ImageHlp;

type
  TPEModule = class;

  //----------------------------------------------------------------------
  // TImageSection class

  TImageSection = class
  private
    FParent: TPEModule;
    FSectionHeader: TImageSectionHeader;
    FRawData: TMemoryStream;
    FUninitializedDataSize: Integer;

    function GetSectionName: string;
  public
    constructor Create(AParent: TPEModule; const AHeader: TImageSectionHeader; rawData: pointer);
    destructor Destroy; override;
    property Parent: TPEModule read FParent;

    property SectionName: string read GetSectionName;
    property SectionHeader: TImageSectionHeader read FSectionHeader;
    property RawData: TMemoryStream read FRawData;
  end;

  TImageImportDirectory = packed record
    Characteristics: DWORD; // This is an RVA to a list of pointers. Each of these points to there function name
    TimeDateStamp: DWORD;   // The time/date stamp indicating when the file was built
    ForwarderChain: DWORD;  // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
    Name: DWORD;            // This is an RVA to a NULL-terminated ASCII string containing the imported DLL's name
    FirstThunk: DWORD;      //  Another RVA to a list pointers. Each of these points to their function name
  end;
  PImageImportDirectory = ^TImageImportDirectory;

  //----------------------------------------------------------------------
  // TPEModule class

  TPEModule = class (TResourceModule)
  private
    FDOSHeader: TImageDosHeader;
    FCOFFHeader: TImageFileHeader;
    FOptionalHeader: PImageOptionalHeader32;
    FSectionList: TObjectList;                   // List of TImageSection objects
    FDOSStub: TMemoryStream;
    FCommentBlock: PChar;
    FCommentSize: Integer;
    FEndComment: PChar;
    FEndCommentSize: Integer;

    function GetOptionalHeader: TImageOptionalHeader;
    function GetImageSection(index: Integer): TImageSection;
    function GetImageSectionCount: Integer;
    function GetDataDictionary(index: Integer): PImageDataDirectory;
    function GetDataDictionaryCount: Integer;
    function GetDOSHeader: TImageDosHeader;
    function GetCOFFHeader: TImageFileHeader;
    function GetExportCount: Integer;
    function GetImportCount: Integer;
    function GetResourceSection (var Offset: Integer): TImageSection;
    function GetImportSection (var Offset: Integer): TImageSection;
    function GetExportSection (var Offset: Integer): TImageSection;
    function GetImport(idx: Integer): PImageImportDirectory;
    function GetImportSectionData: PChar;
    function GetExportSectionData: PChar;

  protected
    procedure Decode(memory: pointer; exeSize: Integer); virtual;
    procedure Encode; virtual;
    property OptionalHeaderPtr: PImageOptionalHeader32 read FOptionalHeader;
    function FindDictionaryEntrySection (entryNo: Integer; var Offset: Integer): Integer;

  public
    constructor Create;
    destructor Destroy; override;

    property DOSHeader: TImageDosHeader read GetDOSHeader;
    property COFFHeader: TImageFileHeader read GetCOFFHeader;
    property OptionalHeader: TImageOptionalHeader read GetOptionalHeader;

    property ImageSectionCount: Integer read GetImageSectionCount;
    property ImageSection [index: Integer]: TImageSection read GetImageSection;

    property DataDictionaryCount: Integer read GetDataDictionaryCount;
    property DataDictionary [index: Integer]: PImageDataDirectory read GetDataDictionary;

    property ImportCount: Integer read GetImportCount;
    property Import [idx: Integer]: PImageImportDirectory read GetImport;
    property ImportSectionData: PChar read GetImportSectionData;
    property ExportSectionData: PChar read GetExportSectionData;
    property ExportCount: Integer read GetExportCount;

    procedure GetExportDetails (idx: Integer; var name: string; var ordinal: DWORD);


    procedure LoadFromStream (s: TStream); override;
    procedure LoadFromFile(const name: string); override;

    procedure SaveToStream (s: TStream); override;
  //  procedure SaveToFile(const name: string); override;
  end;

  //----------------------------------------------------------------------
  // TResourceDirectoryTable record

  TResourceDirectoryTable = packed record
    characteristics: DWORD; // Resource flags, reserved for future use; currently set to zero.
    timeDateStamp: DWORD;   // Time the resource Data was created by the resource compiler.
    versionMajor: WORD;     // Major version number, set by the user.
    versionMinor: WORD;     // Minor version number.
    cNameEntries: WORD;     // Number of directory entries, immediately following the table, that use strings to identify Type, Name, or Language(depending on the level of the table).
    cIDEntries: WORD;       // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
  end;
  PResourceDirectoryTable = ^TResourceDirectoryTable;

  //----------------------------------------------------------------------
  // TPEModule record

  TResourceDirectoryEntry = packed record
    name: DWORD;         // RVA Address of integer or string that gives the Type, Name, or Language identifier, depending on level of table.
    RVA: DWORD;          // RVA High bit 0. Address of a Resource Data Entry(a leaf).
                          // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table(the next level down).
  end;
  PResourceDirectoryEntry = ^TResourceDirectoryEntry;

  //----------------------------------------------------------------------
  // TResourceDirectoryEntry record

  TResourceDataEntry = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;
  PResourceDataEntry = ^TResourceDataEntry;

  //----------------------------------------------------------------------
  // TPEResourceModule class

  TPEResourceModule = class (TPEModule)
  private
    FDetailList: TObjectList;             // List of TResourceDetails objects
  protected
    procedure Decode(memory: pointer; exeSize: Integer); override;
    procedure Encode; override;
    function GetResourceCount: Integer;  override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails[idx: Integer]: TResourceDetails read GetResourceDetails;
    procedure DeleteResource(resourceNo: Integer); override;
    procedure InsertResource(idx: Integer; details: TResourceDetails); override;
    function AddResource(details: TResourceDetails): Integer; override;
    function IndexOfResource(details: TResourceDetails): Integer; override;
    procedure SortResources; override;
  end;

  EPEException = class (Exception);

implementation

{ TPEModule }
resourcestring
  rstInvalidDOSSignature   = 'Invalid DOS signature';
  rstInvalidCOFFSignature  = 'Invalid COFF signature';
  rstInvalidOptionalHeader = 'Invalid Windows Image';
  rstBadDictionaryIndex    = 'Index exceeds data dictionary count';
  rstBadLangID             = 'Unsupported non-integer language ID in resource';
  rstEncode                = 'Error encoding module';

type
  TResourceNode = class
    count: Integer;
    nodes: array of record
      id: string;
      intID: Boolean;
      case leaf: Boolean of
        false: (next: TResourceNode);
        true: (Data: TMemoryStream; CodePage: DWORD)
      end;

    constructor Create(const AType, AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateNameNode(const AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateLangNode(ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure Add (const AType, AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddName(const AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddLang (ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    function IsID (idx: Integer): Boolean;
    destructor Destroy; override;
  end;

(*----------------------------------------------------------------------*
 | constructor PEModule.Create                                          |
 |                                                                      |
 | Constructor for TPEModule instance.  Create empty Section list       |
 *----------------------------------------------------------------------*)
constructor TPEModule.Create;
begin
  inherited Create;
  FSectionList := TObjectList.Create;
  FDOSStub := TMemoryStream.Create;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Decode                                            |
 |                                                                      |
 | Decode the PE file.  Load the DOS header, the COFF header and the    |
 | 'optional' header, then load each Section into FSectionList          |
 *----------------------------------------------------------------------*)
procedure TPEModule.Decode(Memory: pointer; exeSize: Integer);
var
  Offset: LongInt;
  i: Integer;
  sectionHeader: PImageSectionHeader;
  commentOffset: Integer;
begin
  FSectionList.Clear;

  // Check it's really a PE file.
  if PWORD (Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create(rstInvalidDOSSignature);

  // Load the DOS header
  FDOSHeader := PImageDosHeader (Memory)^;

  Offset := FDOSHeader._lfanew;
  FDOSStub.Write((PChar (Memory) + SizeOf(FDOSHeader))^, FDOSHeader._lfanew - SizeOf(FDOSHeader));

  // Check the COFF signature
  if PDWORD (PChar (Memory) + Offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create(rstInvalidCOFFSignature);

  // Load the COFF header
  Inc(Offset, SizeOf(DWORD));
  FCOFFHeader := PImageFileHEader (PChar (Memory) + Offset)^;

  Inc(Offset, SizeOf(FCOFFHeader));

  // Check the Optional Header signature.  nb
  // the optional header is compulsory for
  // 32 bit windows modules!
  if PWORD (PChar (Memory) + Offset)^ <> IMAGE_NT_OPTIONAL_HDR_MAGIC then
    raise EPEException.Create(rstInvalidOptionalHeader);

  // Save the 'optional' header
  ReallocMem (FOptionalHeader, FCOFFHeader.SizeOfOptionalHeader);
  Move((PChar (Memory) + Offset)^, FOptionalHeader^, FCOFFHeader.SizeOfOptionalHeader);

  Inc(Offset, FCOFFHeader.SizeOfOptionalHeader);

  sectionHeader := PImageSectionHeader (PChar (memory) + Offset);
  commentOffset := Offset + FCOFFHeader.NumberOfSections * SizeOf(TImageSectionHeader);

// Save padding between the end of the Section headers, and the start of the
// 1st Section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  FCommentSize := Integer (sectionHeader^.PointerToRawData) - commentOffset;

  if FCommentSize > 0 then
  begin
    GetMem (FCommentBlock, FCommentSize);
    Move((PChar (memory) + commentOffset)^, FCommentBlock^, FCommentSize)
  end;

  // Now save each image Section in the FSectionList
  for i := 0 to FCOFFHeader.NumberOfSections - 1 do
  begin
    sectionHeader := PImageSectionHeader(PChar (memory) + Offset);
    FSectionList.Add (TImageSection.Create(Self, sectionHeader^, PChar (memory) + sectionHeader^.PointertoRawData));
    Inc(Offset, SizeOf(TImageSectionHeader));
  end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

  // Save the padding between the last Section and the end of the file.
  // This appears to hold debug info and things ??

  FEndCommentSize := exeSize - i;
  if FEndCommentSize > 0 then
  begin
    GetMem (FEndComment, FEndCommentSize);
    Move((PChar (memory) + i)^, FEndComment^, FEndCommentSize)
  end
end;

(*----------------------------------------------------------------------*
 | destructor PEModule.Destroy                                          |
 |                                                                      |
 | Destructor for TPEModule instance.                                   |
 *----------------------------------------------------------------------*)
destructor TPEModule.Destroy;
begin
  ReallocMem (FOptionalHeader, 0);
  FSectionList.Free;
  FDOSStub.Free;
  ReallocMem (FCommentBlock, 0);
  ReallocMem (FEndComment, 0);
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure PEModule.Encode                                            |
 |                                                                      |
 | Fix up the Data prior to writing to stream.                          |
 |                                                                      |
 | Ensure that the headers match what we've got...                      |
 *----------------------------------------------------------------------*)
procedure TPEModule.Encode;
var
  Offset: DWORD;
  i: Integer;
  Section: TImageSection;
  align: Integer;
  addrAlign: Integer;
  address: Integer;
  alignedSize, AddrAlignedSize: Integer;
  codeSize, iDataSize, uDataSize, iSize: Integer;
begin
  codeSize := 0;
  iDataSize := 0;
  uDataSize := 0;
                                               // Use the DOS stub from their .EXE
  FDOSHeader._lfanew := SizeOf(FDOSHeader) + FDOSStub.Size;

                                               // Fixup sections count
  FCOFFHeader.NumberOfSections := FSectionList.Count;

  iSize := FDOSHeader._lfanew +               // File Offset for start of sections
           SizeOf(DWORD) +                   // NT signature
           SizeOf(FCOFFHeader) +
           FCOFFHeader.SizeOfOptionalHeader +
           FSectionList.Count * SizeOf(TImageSectionHeader);

  Offset := iSize + FCommentSize;

  align := FOptionalHeader^.FileAlignment;
  addrAlign := FOptionalHeader^.SectionAlignment;

  address := addrAlign;
  Offset := DWORD ((integer (Offset) + align - 1) div align * align);

  // First Section starts at $1000 (when loaded)
  // and at 'offset' in file.

  FOptionalHeader^.SizeOfHeaders := DWORD ((integer (iSize) + align - 1) div align * align);

  FOptionalHeader^.BaseOfCode := $FFFFFFFF;
  FOptionalHeader^.CheckSum := 0;               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize  := DWORD ((integer (iSize) + addrAlign - 1) div addrAlign * addrAlign);

  for i := 0 to FSectionList.Count - 1 do      // Recalculate the Section offsets
  begin
    Section := TImageSection (FSectionList [i]);

    Section.FSectionHeader.PointerToRawData := Offset;
    Section.FSectionHeader.VirtualAddress := address;

// Virtual size is size of Data in memory, and is not padded to an 'alignment'.
//
// SizeOfRawData is size of Data in file, padded to (file) alignment.
//
// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.
//
// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with zeros when it's loaded.
//
// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.
//
// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    Section.FSectionHeader.Misc.VirtualSize := Section.FRawData.Size + Section.FUninitializedDataSize;
    Section.FSectionHeader.SizeOfRawData := (Section.FRawData.Size + align - 1) div align * align;

    alignedSize := (Integer (Section.FSectionHeader.Misc.VirtualSize) + align - 1) div align * align;
    addrAlignedSize := (Integer (Section.FSectionHeader.Misc.VirtualSize) + addrAlign - 1) div addrAlign * addrAlign;

    if (Section.FSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
    begin
      Inc(codeSize, alignedSize);
      if DWORD (address) < FOptionalHeader^.BaseOfCode then
        FOptionalHeader^.BaseOfCode := address
    end
    else
      if (Section.FSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
        Inc(iDataSize, alignedSize)
      else
        if (Section.FSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
          Inc(uDataSize, alignedSize);

    Inc(iSize, addrAlignedSize);
    Inc(Offset, Section.FSectionHeader.SizeOfRawData);
    Inc(address, (Integer (Section.FSectionHeader.Misc.VirtualSize) + addrAlign - 1) div addrAlign * addrAlign);
  end;

  FOptionalHeader^.SizeOfCode := codeSize;
  FOptionalHeader^.SizeOfInitializedData := iDataSize;
  FOptionalHeader^.SizeOfUninitializedData := uDataSize;

  i := SizeOf(DWORD) +                   // NT signature
       SizeOf(FCOFFHeader) +
       FCOFFHeader.SizeOfOptionalHeader +
       codeSize;

  i := (i + addrAlign - 1) div addrAlign * addrAlign;

  // With explorer.exe, codeSize is $14800, i is 148E8, so aligned 'i' is $15000
  // .. so BaseOfData should be $15000 + BaseOfCode($1000) = $16000.
  //
  // ... but it's not - it's $15000, which means that the last $8e8 bytes of code
  // should be stampled over by the Data!
  //
  // But obviously explorer.exe works, so I'm, missing a trick here.  Never mind - it
  // doesn't do any harm making it $16000 instead, and the formula works for everything
  // else I've tested...

  FOptionalHeader^.BaseOfData := FOptionalHeader.BaseOfCode + DWORD (i);

  FOptionalHeader^.SizeOfImage := iSize;
end;

(*----------------------------------------------------------------------*
 | function PEModule.FindDictionaryEntrySection                         |
 |                                                                      |
 | Return the index of the specified Section.  The 'entryNo' to find    |
 | should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in         |
 | Windows.pas.                                                         |
 *----------------------------------------------------------------------*)
function TPEModule.FindDictionaryEntrySection (entryNo: Integer; var Offset: Integer): Integer;
var
  i: Integer;
  p: PImageDataDirectory;
begin
  Result := -1;
  p := DataDictionary [entryNo];
                                // Find Section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (p^.VirtualAddress >= ImageSection [i].FSectionHeader.VirtualAddress) and (p^.VirtualAddress < ImageSection [i].FSectionHeader.VirtualAddress + ImageSection [i].FSectionHeader.Misc.VirtualSize) then
    begin
      Result := i;
      Offset := p^.VirtualAddress - ImageSection [i].FSectionHeader.VirtualAddress;
      Break
    end
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetCOFFHeader                                     |
 |                                                                      |
 | Return COFF header                                                   |
 *----------------------------------------------------------------------*)
function TPEModule.GetCOFFHeader: TImageFileHeader;
begin
  Result := FCOFFHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionary                                 |
 |                                                                      |
 | Return the Data dictionary for a specified                           |
 | IMAGE_DIRECTORY_ENTRY_xxxx  index                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionary(index: Integer): PImageDataDirectory;
var
  p: PImageDataDirectory;
begin
  if index < DataDictionaryCount then
  begin
    p := @FOptionalHeader.DataDirectory [0];
    Inc(p, index);
    Result := p
  end
  else
    raise ERangeError.Create(rstBadDictionaryIndex);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionaryCount                            |
 |                                                                      |
 | Return no of entries in the Data Directory                           |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionaryCount: Integer;
begin
  Result := FOptionalHeader^.NumberOfRvaAndSizes
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDosHeader                                      |
 |                                                                      |
 | Return DOS header                                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  Result := FDOSHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSection (): TImageSection                |
 |                                                                      |
 | Get the specified image Section                                      |
 *----------------------------------------------------------------------*)
function TPEModule.GetExportCount: Integer;
var
  ExportSection: PImageExportDirectory;
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetExportSection (Offset);
  if Assigned(Section) then
  begin
    ExportSection := PImageExportDirectory(PChar (Section.FRawData.memory) + Offset);
    Result := ExportSection^.NumberOfNames
  end
  else
    Result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var name: string;
  var ordinal: DWORD);
var
  ExportSection: PImageExportDirectory;
  Section: TImageSection;
  Offset: Integer;
  po: DWORD;
  pw: PWORD;
  p: PDWORD;
  Data: PChar;
begin
  Section := GetExportSection (Offset);
  if Assigned(Section) then
  begin
    Data := GetExportSectionData;
    ExportSection := PImageExportDirectory(PChar (Section.FRawData.memory) + Offset);
    po := DWORD (ExportSection^.AddressOfNameOrdinals);
    pw := PWORD (Data + po);
    Inc(pw, idx);
    ordinal := pw^;

    po := DWORD (ExportSection^.AddressOfNames);
    p := PDWORD (Data + po);
    Inc(p, idx);
    name := Data + p^
  end
end;

function TPEModule.GetExportSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  Offset := 0;
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_EXPORT, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

function TPEModule.GetExportSectionData: PChar;
var
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetExportSection (Offset);
  Result := PChar (Section.FRawData.Memory) - Section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetImageSection(index: Integer): TImageSection;
begin
  Result := TImageSection (FSectionList [index]);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Return no of image sections                                          |
 *----------------------------------------------------------------------*)
function TPEModule.GetImageSectionCount: Integer;
begin
  Result := FSectionList.Count
end;

function DirValid (dir: PImageImportDirectory): Boolean;
begin
  DirValid := (dir^.Characteristics <> 0) or (dir^.TimeDateStamp <> 0) or (dir^.ForwarderChain <> 0) or
              (dir^.Name <> 0) or (dir^.FirstThunk <> 0)
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Get the optional header                                              |
 *----------------------------------------------------------------------*)
function TPEModule.GetImport(idx: Integer): PImageImportDirectory;
var
  ImportSection: PImageImportDirectory;
  Section: TImageSection;
  Offset: Integer;

begin
  Section := GetImportSection (Offset);
  Result := nil;
  if Assigned(Section) then
  begin
    ImportSection := PImageImportDirectory(PChar (Section.FRawData.memory) + Offset);

    while DirValid (ImportSection) and (idx > 0) do
    begin
      Inc(ImportSection);
      Dec(idx)
    end;

    if DirValid (ImportSection) then
      Result := ImportSection
  end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection: PImageImportDirectory;
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetImportSection (Offset);
  Result := 0;
  if Assigned(Section) then
  begin
    ImportSection := PImageImportDirectory(PChar (Section.FRawData.memory) + Offset);

    while DirValid (ImportSection) do
    begin
      Inc(Result);
      Inc(ImportSection)
    end
  end
end;

function TPEModule.GetImportSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_IMPORT, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

function TPEModule.GetImportSectionData: PChar;
var
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetImportSection (Offset);
  Result := PChar (Section.FRawData.Memory) - Section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TImageOptionalHeader;
begin
  Result := FOptionalHeader^
end;

function TPEModule.GetResourceSection (var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection (IMAGE_DIRECTORY_ENTRY_RESOURCE, Offset);
  if idx = -1 then
    Result := Nil
  else
    Result := ImageSection [idx]
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a file                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromFile(const name: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(name, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream (FileStream)
  finally
    FileStream.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a stream                                        |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromStream(s: TStream);
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.CopyFrom (s, 0);

    Decode(MemoryStream.memory, MemoryStream.size)
  finally
    MemoryStream.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToFile                                       |
 |                                                                      |
 | Save the module to a file                                            |
 *----------------------------------------------------------------------*)
(*
procedure TPEModule.SaveToFile(const name: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(name, fmCreate);
  try
    SaveToStream (f)
  finally
    f.Free
  end
end;
*)
(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToStream                                     |
 |                                                                      |
 | Save the module to a stream                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.SaveToStream(s: TStream);
var
  NTSignature: DWORD;
  i: Integer;
  Section: TImageSection;
  paddingSize, paddingLen: Integer;
  padding: PChar;
  f: TMemoryStream;
  oldCheckSum, newCheckSum: DWORD;
  ntHeaders: PImageNTHEaders;
  ckOffset: DWORD;
begin
  // Encode the Data.
  Encode;

  NTSignature := IMAGE_NT_SIGNATURE;

  // Write the DOS stub
  s.Write(FDOSHeader, SizeOf(FDOSHeader));
  s.CopyFrom (FDOSStub, 0);

  // Write NT sig and COFF header
  s.Write(NTSignature, SizeOf(NTSignature));
  s.Write(FCOFFHeader, SizeOf(FCOFFHeader));
  ckOffset := s.Position + Integer (@FOptionalHeader^.CheckSum) - Integer (@FOptionalHeader^);
  s.Write(FOptionalHeader^, FCOFFHeader.SizeOfOptionalHeader);

  // Write the Section headers
  for i := 0 to FSectionList.Count - 1 do
  begin
    Section := TImageSection (FSectionList [i]);
    s.Write(Section.FSectionHeader, SizeOf(Section.FSectionHeader))
  end;

  // Save the 'comment' Section.  See 'Decode' for details
  if FCommentSize > 0 then
    s.Write(FCommentBlock^, FCommentSize);

  // Write the sections
  padding := nil;
  paddingLen := 0;
  try
    for i := 0 to FSectionList.Count - 1 do
    begin
      // Write padding up to file Offset of the Section
      Section := TImageSection (FSectionList [i]);
      paddingSize := Section.FSectionHeader.PointerToRawData - DWORD (s.Position);

      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory(padding, paddingLen);
      end;

      if paddingSize > 0 then   // Put our signature at the start of the first
          s.Write(padding^, paddingSize);

      // Write the Section Data.
      s.CopyFrom (Section.FRawData, 0);

      // Write Data
      with Section.FSectionHeader do
        paddingSize := SizeOfRawData - misc.VirtualSize;

      // Pad Data
      if paddingSize > paddingLen then
      begin
        paddingLen := paddingSize + 65536;
        ReallocMem (padding, paddingLen);
        ZeroMemory(padding, paddingLen);
      end;

      if paddingSize > 0 then
        s.Write(padding^, paddingSize)

    end;

    // Save the debug info.
    if FEndCommentSize > 0 then
      s.Write(FEndComment^, FEndCommentSize)
  finally
    ReallocMem (padding, 0)
  end;

  // Now calculate the checksum....
  f := TMemoryStream.Create;
  try
    s.Position := 0;
    f.LoadFromStream(s);
    ntHeaders := ChecksumMappedFile(f.Memory, f.Size, @oldCheckSum, @newCheckSum);

    if Assigned(ntHeaders) then
    begin
      s.Position := ckOffset;
      s.Write(newChecksum, SizeOf(newChecksum))
    end
  finally
    f.Free
  end;

  s.Seek(0, soFromEnd);
end;


{ TImageSection }

(*----------------------------------------------------------------------*
 | constructor TImageSection.Create                                     |
 |                                                                      |
 | Constructor for TImageSection.                                       |
 *----------------------------------------------------------------------*)
constructor TImageSection.Create(AParent: TPEModule;
  const AHeader: TImageSectionHeader; rawData: pointer);
begin
  FSectionHeader := AHeader;
  FRawData := TMemoryStream.Create;

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if FSectionHeader.Misc.VirtualSize <= FSectionHeader.SizeOfRawData then
  begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if FSectionHeader.Misc.VirtualSize = 0 then
      FSectionHeader.Misc.VirtualSize := FSectionHeader.SizeOfRawData;
    FRawData.Write(rawData^, FSectionHeader.Misc.VirtualSize)
  end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
  begin
    FRawData.Write(rawData^, FSectionHeader.SizeOfRawData);
    FUninitializedDataSize := FSectionHeader.Misc.VirtualSize - FSectionHeader.SizeOfRawData;
  end;

  FParent := AParent;
end;

(*----------------------------------------------------------------------*
 | function TImageSection.GetSectionName                                |
 |                                                                      |
 | Return the Section name - eg. .Data                                  |
 *----------------------------------------------------------------------*)
function TImageSection.GetSectionName: string;
begin
  Result := PChar (@FSectionHeader.Name)
end;

(*----------------------------------------------------------------------*
 | destructor TImageSection.Destroy                                     |
 |                                                                      |
 | destructor for TImageSection.                                        |
 *----------------------------------------------------------------------*)
destructor TImageSection.destroy;
begin
  FRawData.Free;
  inherited;
end;

{ TPEResourceModule }

(*----------------------------------------------------------------------*
 | procedure TPEResourceModule.DeleteResource                           |
 |                                                                      |
 | Delete the specified resource(by index)                             |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.DeleteResource(resourceNo: Integer);
var
  res: TResourceDetails;
begin
  res := ResourceDetails[resourceNo];
  inherited;
  resourceNo := IndexOfResource(Res);
  if resourceNo <> -1 then
    FDetailList.Delete(resourceNo);
end;

(*----------------------------------------------------------------------*
 | constructor TPEResourceModule.Create                                 |
 |                                                                      |
 | Constructor for TPEResourceModule                                    |
 *----------------------------------------------------------------------*)
constructor TPEResourceModule.Create;
begin
  inherited Create;
  FDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TPEResourceModule.Destroy                                 |
 |                                                                      |
 | Destructor for TPEResourceModule                                     |
 *----------------------------------------------------------------------*)
destructor TPEResourceModule.Destroy;
begin
  FDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Decode                                    |
 |                                                                      |
 | Decode the Section's resource tree into a list of resource details   |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Decode;
var
  Section: TImageSection;
  tp, name: string;
  lang: Integer;
  Offset: Integer;

  // Get string resource name
  function GetResourceStr (IdorName: Boolean; Section: TImageSection; n: DWORD): string;
  var
    p: PWideChar;
  begin
    if IdorName then
      Result := IntToStr (n)
    else
    begin
      p := PWideChar (PChar (Section.FRawData.Memory) + (n and $7fffffff));
      Result := ResourceWideCharToStr (p, CP_ACP)
    end
  end;

  // (recursively) get resources
  procedure GetResource(Offset, level: Integer);
  var
    entry: PResourceDirectoryEntry;
    i, count: Integer;
    IDorName: Boolean;
    dataEntry: PResourceDataEntry;
    table: PResourceDirectoryTable;
    details: TResourceDetails;
  begin
    table := PResourceDirectoryTable(PChar (Section.FRawData.memory) + Offset);
    with table^ do
      count := cNameEntries + cIDEntries;

    entry := PResourceDirectoryEntry(PChar (Section.FRawData.memory) + Offset + SizeOf(TResourceDirectoryTable));
    for i := 0 to count - 1 do
    begin
      idOrName := i >= table^.cNameEntries;
      case level of
        0: tp := GetResourceStr (IDOrName, Section, entry^.name);
        1 :
            name := GetResourceStr (IDOrName, Section, entry^.name);
        2 :
          begin
            if not IdOrName then
              raise EPEException.Create(rstBadLangID);

            lang := entry^.name
          end
      end;

      if (entry^.RVA and $80000000) > 0 then // Not a leaf node - traverse the tree
        GetResource(entry^.RVA and $7fffffff, level + 1)
      else
      begin
                                             // It's a leaf node - create resource details
        dataEntry := PResourceDataEntry(PChar (Section.FRawData.Memory) + entry^.RVA);
        details := TResourceDetails.CreateResourceDetails (Self, lang, name, tp, dataEntry^.Size, PChar (Section.FRawData.Memory) + dataEntry^.OffsetToData - Section.FSectionHeader.VirtualAddress);
        details.CodePage := dataEntry^.CodePage;
        details.Characteristics := table^.characteristics;
        details.DataVersion := DWORD (table^.versionMajor) * 65536 + DWORD (table^.versionMinor);
        FDetailList.Add (details);

      end;

      Inc(entry)
    end
  end;

begin
  inherited;
  Section := GetResourceSection (Offset);
  if Section <> nil then
    GetResource(Offset, 0)
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceCount                          |
 |                                                                      |
 | Return the number of resources in the resource Section               |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceCount: Integer;
begin
  Result := FDetailList.Count
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceDetails                        |
 |                                                                      |
 | Get the resource details for the specified resource.                 |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceDetails(
  idx: Integer): TResourceDetails;
begin
  Result := TResourceDetails (FDetailList [idx]);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.IndexOfResource                           |
 |                                                                      |
 | Return the index of the specified resource details in the resource   |
 *----------------------------------------------------------------------*)
function TPEResourceModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.IndexOf (details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.InsertResource                            |
 |                                                                      |
 | Insert a resource in the list.                                       |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  FDetailList.Insert(idx, details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Encode                                    |
 |                                                                      |
 | Complicated?  I'll give you complicated ...                          |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Encode;
var
  i: Integer;
  details: TResourceDetails;
  Section: TImageSection;
  root: TResourceNode;
  versMajor, versMinor: word;
  TimeStamp: DWORD;
  nameSize, nameOffset, namePos, tableOffset: DWORD;
  deOffset, dePos, deSize: DWORD;
  dataOffset, dataPos, dataSize: DWORD;
  Offset: Integer;

  nameTable: PChar;
  deTable: PChar;
  Data: PChar;
  zeros: PChar;

  //------------------------------------------------------------------
  // Calculate Offset and size of name table and DirectoryEntry table.
  // Calculate size of Data

  procedure GetNameTableSize(node: TResourceNode);
  var
    i: Integer;
  begin
    Inc(nameOffset, SizeOf(TResourceDirectoryTable));
    Inc(deOffset, SizeOf(TResourceDirectoryTable));

    for i := 0 to node.count - 1 do
    begin
      Inc(nameOffset, SizeOf(TResourceDirectoryEntry));
      Inc(deOffset, SizeOf(TResourceDirectoryEntry));

      if not node.nodes[i].intID then
        Inc(nameSize, Length(node.nodes[i].id) * SizeOf(WideChar) + SizeOf(word));

      if not node.nodes[i].leaf then
        GetNameTableSize(node.nodes[i].next)
      else
      begin
        Inc(nameOffset, SizeOf(TResourceDataEntry));
        Inc(deSize, SizeOf(TResourceDataEntry));
        dataSize := (dataSize + DWORD (node.nodes[i].Data.Size) + 3) div 4 * 4;
      end
    end
  end;

  //------------------------------------------------------------------
  // Save a node to Section.FRawData(and save it's child nodes recursively)

  procedure SaveToSection (node: TResourceNode);
  var
    table: TResourceDirectoryTable;
    entry: TResourceDirectoryEntry;
    dataEntry: PResourceDataEntry;
    i, n: Integer;
    w: WideString;
    wl: word;

  //------------------------------------------------------------------
  // Save entry(i), and the child nodes

    procedure SaveNode(i: Integer);
    begin
      if node.nodes[i].intID then      // id is a simple integer
        entry.name := StrToInt(node.nodes[i].id)
      else
      begin                             // id is an Offset to a name in the
                                        // name table.
        entry.name := nameOffset + namePos + $80000000;
        w := node.nodes[i].id;
        wl := Length(node.nodes[i].id);
        Move(wl, nameTable [namePos], SizeOf(wl));
        Inc(namePos, SizeOf(wl));
        Move(w [1], nameTable [namePos], wl * SizeOf(WideChar));
        Inc(namePos, wl * SizeOf(WideChar))
      end;

      if node.nodes[i].leaf then       // RVA points to a TResourceDataEntry in the
      begin                             // Data entry table.
        entry.RVA := deOffset + dePos;
        dataEntry := PResourceDataEntry(deTable + dePos);
        dataEntry^.CodePage := node.nodes[i].CodePage;
        dataEntry^.Reserved := 0;
        dataEntry^.Size := node.nodes[i].Data.Size;
        dataEntry^.OffsetToData := dataOffset + dataPos + Section.FSectionHeader.VirtualAddress;

        Move(node.nodes[i].Data.memory^, Data [dataPos], dataEntry^.Size);

        Inc(dePos, SizeOf(TResourceDataEntry));
        dataPos := (dataPos + dataEntry^.size + 3) div 4 * 4;
        Section.FRawData.Write(entry, SizeOf(entry));
      end
      else                              // RVA points to another table.
      begin
        entry.RVA := $80000000 + tableOffset;
        Section.FRawData.Write(entry, SizeOf(entry));
        n := Section.FRawData.Position;
        SaveToSection (node.nodes[i].next);
        Section.FRawData.Seek(n, soFromBeginning);
      end
    end;

  begin { SaveToSection }
    table.characteristics := 0;
    table.timeDateStamp := TimeStamp;
    table.versionMajor := versMajor;
    table.versionMinor := versMinor;
    table.cNameEntries := 0;
    table.cIDEntries := 0;

                                        // Calculate no of integer and string IDs
    for i := 0 to node.count - 1 do
      if node.nodes[i].intID then
        Inc(table.cIDEntries)
      else
        Inc(table.cNameEntries);

    Section.FRawData.Position := tableOffset;
    Section.FRawData.Write(table, SizeOf(table));

    tableOffset := tableOffset + SizeOf(TResourceDirectoryTable) + DWORD (node.Count) * SizeOf(TResourceDirectoryEntry);

                                        // The docs suggest that you save the nodes
                                        // with string entries first.  Goodness knows why,
                                        // but play along...
    for i := 0 to node.count - 1 do
      if not node.nodes[i].intID then
       SaveNode(i);

    for i := 0 to node.count - 1 do
      if node.nodes[i].intID then
       SaveNode(i);

    Section.FRawData.Seek(0, soFromEnd);
  end;


begin { Encode }
  Section := GetResourceSection (Offset);

                                        // Get the details in a tree structure
  root := nil;
  Data := nil;
  deTable := nil;
  zeros := nil;

  try
    for i := 0 to FDetailList.Count - 1 do
    begin
      details := TResourceDetails (FDetailList.Items[i]);
      if root = Nil then
        root := TResourceNode.Create(details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
      else
        root.Add (details.ResourceType, details.ResourceName, details.ResourceLanguage, details.Data, details.CodePage)
    end;

                                          // Save elements of their original EXE
    versMajor := PResourceDirectoryTable(Section.FRawData.Memory)^.versionMajor;
    versMinor := PResourceDirectoryTable(Section.FRawData.Memory)^.versionMinor;
    TimeStamp := PResourceDirectoryTable(Section.FRawData.Memory)^.timeDateStamp;


    Section.FRawData.Clear;               // Clear the Data.  We're gonna recreate
                                          // it from our resource details.

    nameSize := 0; nameOffset := Offset;
    deSize := 0; deOffset := Offset;
    dataSize := 0;

    GetNameTableSize(root);              // Calculate sizes and offsets of the
                                          // name table, the Data entry table and
                                          // the size of the Data.

                                          // Calculate the Data Offset.  Must be aligned.
    dataOffset := (nameOffset + nameSize + 15) div 16 * 16;

                                          // Initialize globals...
    namePos := 0;                         //   Offset of next entry in the string table
    dePos := 0;                           //   Offset of next entry in the Data entry table
    dataPos := 0;                         //   Offset of next Data block.
    tableOffset := 0;                     //   Offset of next TResourceDirectoryTable


    GetMem (nameTable, nameSize);         // Allocate buffers for tables
    GetMem (Data, dataSize);
    GetMem (deTable, deSize);

    SaveToSection (root);               // Do the work.

                                        // Save the tables
    Section.FRawData.Write(deTable^, deSize);
    Section.FRawData.Write(nameTable^, nameSize);

                                        // Add padding so the Data goes on a
                                        // 16 byte boundary.
    if DWORD (Section.FRawData.Position) < dataOffset then
    begin
      GetMem (zeros, dataOffset - DWORD (Section.FRawData.Position));
      ZeroMemory(zeros, dataOffset - DWORD (Section.FRawData.Position));
      Section.FRawData.Write(zeros^, dataOffset - DWORD (Section.FRawData.Position))
    end;

                                        // Write the Data.
    Section.FRawData.Write(Data^, dataSize);

    inherited; // **** Must call inherited !

  finally       // Tidy up.
    ReallocMem (zeros, 0);
    FreeMem (nameTable);
    FreeMem (deTable);
    FreeMem (Data);
    root.Free
  end
end;


{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: string; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;

begin
  for i := 0 to count - 1 do
    if AType = nodes[i].id then
    begin
      nodes[i].next.AddName(AName, ALang, aData, codePage);
      exit
    end;

  Inc(count);
  SetLength(nodes, count);
  nodes[count - 1].id := AType;
  nodes[count - 1].intID := isID (count - 1);
  nodes[count - 1].leaf := False;
  nodes[count - 1].next := TResourceNode.CreateNameNode(AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if IntToStr (ALang) = nodes[i].id then
    begin
      nodes[i].Data := aData;
      exit
    end;

  Inc(count);
  SetLength(nodes, count);
  nodes[count - 1].id := IntToStr (ALang);
  nodes[count - 1].intId := True;
  nodes[count - 1].leaf := True;
  nodes[count - 1].Data := aData;
  nodes[count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: string; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if AName = nodes[i].id then
    begin
      nodes[i].next.AddLang (ALang, aData, codePage);
      exit
    end;

  Inc(count);
  SetLength(nodes, count);
  nodes[count - 1].id := AName;
  nodes[count - 1].intID := isID (count - 1);
  nodes[count - 1].leaf := False;
  nodes[count - 1].next := TResourceNode.CreateLangNode(ALang, aData, codePage)
end;

constructor TResourceNode.Create(const AType, AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
  count := 1;
  SetLength(nodes, 1);
  nodes[0].id := AType;
  nodes[count - 1].intID := isID (count - 1);
  nodes[0].leaf := False;
  nodes[0].next := TResourceNode.CreateNameNode(AName, ALang, aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
begin
  count := 1;
  SetLength(nodes, 1);
  nodes[0].id := IntToStr (ALang);
  nodes[count - 1].intID := True;
  nodes[0].leaf := True;
  nodes[0].Data := aData;
  nodes[0].CodePage := codePage
end;

constructor TResourceNode.CreateNameNode(const AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
  count := 1;
  SetLength(nodes, 1);
  nodes[0].id := AName;
  nodes[count - 1].intID := isID (count - 1);

  nodes[0].leaf := False;
  nodes[0].next := TResourceNode.CreateLangNode(ALang, aData, codePage)
end;

destructor TResourceNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    if not nodes[i].leaf then
      nodes[i].next.Free;

  inherited;
end;

function TResourceNode.IsID (idx: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(nodes[idx].id) do
    if not (nodes[idx].id [i] in ['0'..'9']) then
    begin
      Result := False;
      Break
    end;

  if Result then
    Result := IntToStr (StrToInt(nodes[idx].id)) = nodes[idx].id;
end;

function TPEResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.Add (details);
end;

procedure TPEResourceModule.SortResources;
begin
  FDetailList.Sort(compareDetails);
end;

end.
