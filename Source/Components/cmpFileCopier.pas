unit cmpFileCopier;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Classes, SysUtils, Forms;

type
  TOnDuplicates = (duCopy, duAsk, duAbort);
  TCopyMode = (cmCopy, cmMove, cmNothing);
  TOnCopyFile = procedure(Sender: TObject; const srcfileName, dstFileName: string; fileSize: Integer) of object;
  TForEachProc = procedure(const Directory, Dest: string; const SearchRec: TSearchRec; var Continue: Boolean) of object;
  TOnException = procedure(Sender: TObject; e: Exception) of object;
  TFileCopier = class (TComponent)
  private
    FCopierThread: TThread;
    FProgressThread: TThread;
    FRecursive: Boolean;
    FDestFiles: string;
    FDestDir: string;
    FSourceFiles: TStrings;
    FSourceMask: string;
    FOnStartCopy: TNotifyEvent;
    FOnEndCopy: TNotifyEvent;
    FOnEndCopyFile: TOnCopyFile;
    FOnStartCopyFile: TOnCopyFile;
    FOnDuplicates: TOnDuplicates;
    FExcep: Exception;

    FAnalyzedFileCount: Integer;
    FAnalyzedFileSize: int64;
    FLeeway: Int64;
    FCopyMode: TCopyMode;
    FCancelled: Boolean;
    FOnStartAnalysis: TNotifyEvent;
    FOnEndAnalysis: TNotifyEvent;
    FCurrentFileName: string;
    FDestFileName: string;
    FCurrentFileSize: Integer;
    FCheckedDrive: Boolean;
    FOnException: TOnException;

    procedure AnalyzeFile(const Directory, Dest: string; const SearchRec: TSearchRec; var Continue: Boolean);
    procedure CopyFile(const Directory, Dest: string; const SearchRec: TSearchRec; var Continue: Boolean);
    procedure TidyDir(const Directory, Dest: string; const SearchRec: TSearchRec; var Continue: Boolean);
    function ForEach(const Mask, destPath: string; proc, proc1: TForEachProc): Boolean;
    procedure AnalyzeFiles(const SourceFiles: string);
    procedure CopyFiles(const SourceFiles: string);
    procedure DoOnException;
    procedure DoOnStartAnalysis;
    procedure DoOnEndAnalysis;
    procedure DoOnStartCopy;
    procedure DoOnEndCopy;
    procedure DoOnStartCopyFile;
    procedure DoOnEndCopyFile;

    procedure DoMoveFile;
    procedure DoCopyFile;
    procedure SetSourceFiles(const Value: TStrings);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Cancel;

    property AnalyzedFileSize: Int64 read FAnalyzedFileSize;
    property AnalyzedFileCount: Integer read FAnalyzedFileCount;

  published
    property SourceFiles: TStrings read FSourceFiles write SetSourceFiles;
    property DestFiles: string read FDestFiles write FDestFiles;
    property Recursive: Boolean read FRecursive write FRecursive default True;
    property OnDuplicates: TOnDuplicates read FOnDuplicates write FOnDuplicates;
    property Leeway: Int64 read FLeeway write FLeeway default 1024*1024;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode;

    property OnStartAnalysis: TNotifyEvent read FOnStartAnalysis write FOnStartAnalysis;
    property OnEndAnalysis: TNotifyEvent read FOnEndAnalysis write FOnEndAnalysis;
    property OnStartCopy: TNotifyEvent read FOnStartCopy write FOnStartCopy;
    property OnEndCopy: TNotifyEvent read FOnEndCopy write FOnEndCopy;
    property OnStartCopyFile: TOnCopyFile read FOnStartCopyFile write FOnStartCopyFile;
    property OnEndCopyFile: TOnCopyFile read FOnEndCopyFile write FOnEndCopyFile;
    property OnException: TOnException read FOnException write FOnException;
  end;

  EFileCopier = class (Exception)
  end;

implementation

resourcestring
  rstAlreadyCopying = 'Already copying files';
  rstNoDest = 'No Destination';
  rstNoSource = 'No Source';
  rstFileNotFound = 'File %s not found';

type
  TCopierThread = class (TThread)
  private
    FOwner: TFileCopier;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TFileCopier);
  end;

  TCopierProgressThread = class (TThread)
  public
    constructor Create(AOwner: TFileCopier);
  end;

{ TFileCopier }

procedure TFileCopier.AnalyzeFile(const Directory, Dest: string; const SearchRec: TSearchRec; var Continue: Boolean);
var
  destFileName: string;
  fa: Integer;
begin
  Inc(FAnalyzedFileCount);
  FAnalyzedFileSize := FAnalyzedFileSize + SearchRec.Size;

  // Check we can open the source file for reading.
  with TFileStream.Create(Directory + SearchRec.Name, fmOpenRead or fmShareDenyNone) do
    Free;

  if not FCheckedDrive then
  begin
                // Check the Dest drive or share is valid
    FCheckedDrive := True;
    destFileName := Dest + SearchRec.Name;
    fa := FileGetAttr (ExtractFileDrive(destFileName));
    if fa = -1 then
      raise Exception.Create('Destination drive or share not valid');

                // Check we can create files on the Dest share or drive
    destFileName := ExtractFileDrive(destFilename) + 'copier.woozle.1122';
    if FileExists (destFileName) then
      DeleteFile(destFileName);
    with TFileStream.Create(destFileName, fmCreate or fmShareDenyNone) do
      Free;
    if not DeleteFile(destFileName) then
      RaiseLastOSError;
  end;

  destFileName := Dest + SearchRec.Name;
  fa := FileGetAttr (destFileName);
  if fa <> -1 then
    if OnDuplicates = duAbort then // Complain if Dest file exists
      raise EFileCopier.Create('Duplicate file ' + destFileName + ' aborted')
    else
                                // Check we can overwrite the existing Dest file when the
                                // time comes.
      with TFileStream.Create(destFileName, fmOpenReadWrite or fmShareDenyNone) do
        Free;

end;

procedure TFileCopier.AnalyzeFiles (const SourceFiles: string);
var
  st: string;
  Continue: Boolean;
  SearchRec: TSearchRec;
  err: Integer;
  available, a1: Int64;
  a2: TLargeInteger;
begin
  if Length(SourceFiles) = 0 then
    raise EFileCopier.Create(rstNoSource);

  if Length(FDestFiles) = 0 then
    raise EFileCopier.Create(rstNoDest);

  FSourceMask := '';
  if (Pos ('?', SourceFiles) > 0) or (Pos ('*', SourceFiles) > 0) then
    FSourceMask := SourceFiles
  else
  begin
    st := Copy(SourceFiles, Length(SourceFiles), 1);
    if (st = '\') or (st = ':') then
      FSourceMask := SourceFiles + '*.*'
  end;

  FDestDir := FDestFiles;
  err := FileGetAttr (FDestDir);
  if (err <> -1) and ((err and faDirectory) = 0) then
    raise EFileCopier.Create('Destination must be a directory');

  st := Copy(FDestDir, Length(FDestDir), 1);
  if (st = '\') or (st = ':') then
    FDestDir := FDestDir
  else
    FDestDir := FDestDir + '\';

  if FSourceMask = '' then
  begin
    err := FileGetAttr (SourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [SourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      FSourceMask := SourceFiles + '\*.*';
      FDestDir := FDestDir + ExtractFileName(SourceFiles) + '\'
    end
  end;


  if FSourceMask <> '' then
    ForEach(FSourceMask, FDestDir, AnalyzeFile, Nil)
  else
    if FindFirst(SourceFiles, faAnyFile, SearchRec) = 0 then
    try
      Continue := True;
      AnalyzeFile(ExtractFilePath(SourceFiles), FDestDir, SearchRec, Continue)
    finally
      FindClose(SearchRec)
    end
    else
      raise EFOpenError.CreateFmt(rstFileNotFound, [SourceFiles]);

  if (CopyMode = cmCopy) or (ExtractFileDrive(SourceFiles) <> ExtractFileDrive(FDestDir)) then
  begin
    SysUtils.GetDiskFreeSpaceEx (PChar (ExtractFileDrive(FDestDir)), available, a1, @a2);

    if FAnalyzedFileSize + FLeeway > available then
      raise EFileCopier.Create('Not enough space to copy files')
  end
end;

procedure TFileCopier.Cancel;
begin
  FCancelled := True;
end;

procedure TFileCopier.CopyFile(const Directory, Dest: string;
  const SearchRec: TSearchRec; var Continue: Boolean);
begin
  FCurrentFileName := Directory + SearchRec.Name;
  FDestFileName := Dest + SearchRec.Name;
  FCurrentFileSize := SearchRec.Size;

  if Assigned(FOnStartCopyFile) then
    FCopierThread.Synchronize(FCopierThread, DoOnStartCopyFile);

  if CopyMode <> cmNothing then
    ForceDirectories (Dest);

  case CopyMode of
    cmNothing:;
    cmMove: DoMoveFile;
    cmCopy: DoCopyFile;
  end;
  if Assigned(FOnEndCopyFile) then
    FCopierThread.Synchronize(FCopierThread, DoOnEndCopyFile);
end;

procedure TFileCopier.CopyFiles (const SourceFiles: string);
var
  SearchRec: TSearchRec;
  Continue: Boolean;
  st: string;
  err: Integer;
begin
  FSourceMask := '';
  if (Pos ('?', SourceFiles) > 0) or (Pos ('*', SourceFiles) > 0) then
    FSourceMask := SourceFiles
  else
  begin
    st := Copy(SourceFiles, Length(SourceFiles), 1);
    if (st = '\') or (st = ':') then
      FSourceMask := SourceFiles + '*.*'
  end;

  FDestDir := FDestFiles;
  err := FileGetAttr (FDestDir);
  if (err <> -1) and ((err and faDirectory) = 0) then
    raise EFileCopier.Create('Destination must be a directory');

  st := Copy(FDestDir, Length(FDestDir), 1);
  if (st = '\') or (st = ':') then
    FDestDir := FDestDir
  else
    FDestDir := FDestDir + '\';

  if FSourceMask = '' then
  begin
    err := FileGetAttr (SourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [SourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      FSourceMask := SourceFiles + '\*.*';
      FDestDir := FDestDir + ExtractFileName(SourceFiles) + '\'
    end
  end;

  if FSourceMask <> '' then
    ForEach(FSourceMask, FDestDir, CopyFile, TidyDir)
  else
    if FindFirst(SourceFiles, faAnyFile, SearchRec) = 0 then
    try
      Continue := True;
      CopyFile(ExtractFilePath(SourceFiles), FDestDir, SearchRec, Continue)
    finally
      FindClose(SearchRec)
    end
end;

constructor TFileCopier.Create(AOwner: TComponent);
begin
  inherited;

  FSourceFiles := TStringList.Create;
  FRecursive := True;
  FLeeway := 1024*1024;

end;

destructor TFileCopier.Destroy;
begin
  while Assigned(FCopierThread) do
    Sleep (500);
  FreeAndNil (FSourceFiles);
  inherited;
end;

procedure TFileCopier.DoCopyFile;
begin
  if not Windows.CopyFile(PChar (FCurrentFileName), PChar (FDestFileName), False) then
    RaiseLastOSError
end;

procedure TFileCopier.DoMoveFile;
begin
  if not SameText(ExtractFileDrive(FCurrentFileName), ExtractFileDrive(FDestFileName)) then
  begin
    if not Windows.CopyFile(PChar (FCurrentFileName), PChar (FDestFileName), False) then
      RaiseLastOSError;

    if not Windows.DeleteFile(PChar (FCurrentFileName)) then
      RaiseLastOSError;

  end
  else
  begin
    DeleteFile(FDestFileName);
    if not Windows.MoveFile(PChar (FCurrentFileName), PChar (FDestFileName)) then
      RaiseLastOSError
  end
end;

procedure TFileCopier.DoOnEndAnalysis;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnEndAnalysis) then
    OnEndAnalysis(Self);
end;

procedure TFileCopier.DoOnEndCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnEndCopy) then
    OnEndCopy(Self);
end;

procedure TFileCopier.DoOnEndCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnEndCopyFile) then
    OnEndCopyFile(Self, FCurrentFileName, FDestFileName, FCurrentFileSize);
end;

procedure TFileCopier.DoOnException;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnException) then
    OnException (Self, FExcep)
  else
    Application.ShowException(FExcep);
end;

procedure TFileCopier.DoOnStartAnalysis;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartAnalysis) then
    OnStartAnalysis (Self);
end;

procedure TFileCopier.DoOnStartCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartCopy) then
    OnStartCopy(Self);
end;

procedure TFileCopier.DoOnStartCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartCopyFile) then
    OnStartCopyFile(Self, FCurrentFileName, FDestFileName, FCurrentFileSize);
end;

function TFileCopier.ForEach(const Mask, destPath: string; proc, proc1: TForEachProc): Boolean;
var
  SearchRec: TSearchRec;
  st: string;
begin
  Result := True;
  begin
    if FindFirst(Mask, faAnyFile, SearchRec) = 0 then
    try
      repeat
        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and Recursive then
          begin
            st := destPath + SearchRec.Name + '\';

            Result := ForEach(ExtractFilePath(Mask) + SearchRec.Name + '\' + ExtractFileName(Mask), st, proc, proc1)
          end
        end
        else
          proc (ExtractFilePath(Mask), destPath, SearchRec, Result);
        if FCancelled then
          Result := False;
      until not Result or (FindNext(SearchRec) <> 0)
    finally
      FindClose(SearchRec);
      if Assigned(proc1) then
        proc1 (ExtractFilePath(Mask), '', SearchRec, Result)
    end
    else
      RaiseLastOSError
  end
end;

procedure TFileCopier.SetSourceFiles(const Value: TStrings);
begin
  FSourceFiles.Assign(Value);
end;

procedure TFileCopier.Start;
begin
  FCancelled := False;
  FCheckedDrive := False;
  if not Assigned(FCopierThread) then
  begin
    FCopierThread := TCopierThread.Create(Self);
  end
  else
    raise EFileCopier.Create(rstAlreadyCopying);
end;

procedure TFileCopier.TidyDir(const Directory, Dest: string;
  const SearchRec: TSearchRec; var Continue: Boolean);
var
  st: string;
begin
  st := Directory;
  if Copy(st, Length(st), 1) = '\' then
  begin
    Delete(st, Length(st), 1);
    RemoveDirectory(PChar (st)) // Don't error check this.  It may be open in a
                                 // command window or something...
  end
end;

{ TCopierThread }

constructor TCopierThread.Create(AOwner: TFileCopier);
begin
  FOwner := AOwner;
  inherited Create(True);
  FreeOnTerminate := True;
  resume;
end;

procedure TCopierThread.Execute;
var
  i: Integer;
begin
  try
    try
      Synchronize(FOwner.DoOnStartAnalysis);
      for i := 0 to FOwner.FSourceFiles.Count - 1 do
        FOwner.AnalyzeFiles (FOwner.FSourceFiles[i]);
      Synchronize(FOwner.DoOnEndAnalysis);
      Synchronize(FOwner.DoOnStartCopy);

      for i := 0 to FOwner.FSourceFiles.Count - 1 do
        FOwner.CopyFiles (FOwner.FSourceFiles[i]);
      Synchronize(FOwner.DoOnEndCopy)
    except
      on E: Exception do
      begin
        FOwner.FExcep := E;
        Synchronize(FOwner.DoOnException)
      end
    end
  finally
    FOwner.FCopierThread := Nil
  end
end;

{ TCopierProgressThread }

constructor TCopierProgressThread.Create(AOwner: TFileCopier);
begin

end;

end.
