unit cmpFileCopier;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Classes, SysUtils, Forms;

type
  TOnDuplicates = (duCopy, duAsk, duAbort);
  TCopyMode = (cmCopy, cmMove, cmNothing);
  TOnCopyFile = procedure(sender: TObject; const srcfileName, dstFileName: string; fileSize: Integer) of object;
  TForEachProc = procedure(const Directory, dest: string; const sr: TSearchRec; var Continue: Boolean) of object;
  TOnException = procedure(sender: TObject; e: Exception) of object;
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

    procedure AnalyzeFile(const Directory, dest: string; const sr: TSearchRec; var Continue: Boolean);
    procedure CopyFile(const Directory, dest: string; const sr: TSearchRec; var Continue: Boolean);
    procedure TidyDir (const Directory, dest: string; const sr: TSearchRec; var Continue: Boolean);
    function ForEach(const Mask, destPath: string; proc, proc1: TForEachProc): Boolean;
    procedure AnalyzeFiles (const sourceFiles: string);
    procedure CopyFiles (const sourceFiles: string);
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

procedure TFileCopier.AnalyzeFile(const Directory, dest: string; const sr: TSearchRec; var Continue: Boolean);
var
  destFileName: string;
  fa: Integer;
begin
  Inc(FAnalyzedFileCount);
  FAnalyzedFileSize := FAnalyzedFileSize + sr.Size;

  // Check we can open the source file for reading.
  with TFileStream.Create(Directory + sr.Name, fmOpenRead or fmShareDenyNone) do
    Free;

  if not FCheckedDrive then
  begin
                // Check the dest drive or share is valid
    FCheckedDrive := True;
    destFileName := dest + sr.Name;
    fa := FileGetAttr (ExtractFileDrive(destFileName));
    if fa = -1 then
      raise Exception.Create('Destination drive or share not valid');

                // Check we can create files on the dest share or drive
    destFileName := ExtractFileDrive(destFilename) + 'copier.woozle.1122';
    if FileExists (destFileName) then
      DeleteFile(destFileName);
    with TFileStream.Create(destFileName, fmCreate or fmShareDenyNone) do
      Free;
    if not DeleteFile(destFileName) then
      RaiseLastOSError;
  end;

  destFileName := dest + sr.Name;
  fa := FileGetAttr (destFileName);
  if fa <> -1 then
    if OnDuplicates = duAbort then // Complain if dest file exists
      raise EFileCopier.Create('Duplicate file ' + destFileName + ' aborted')
    else
                                // Check we can overwrite the existing dest file when the
                                // time comes.
      with TFileStream.Create(destFileName, fmOpenReadWrite or fmShareDenyNone) do
        Free;

end;

procedure TFileCopier.AnalyzeFiles (const sourcefiles: string);
var
  st: string;
  Continue: Boolean;
  sr: TSearchRec;
  err: Integer;
  available, a1: Int64;
  a2: TLargeInteger;
begin
  if Length(sourceFiles) = 0 then
    raise EFileCopier.Create(rstNoSource);

  if Length(FDestFiles) = 0 then
    raise EFileCopier.Create(rstNoDest);

  FSourceMask := '';
  if (Pos ('?', sourceFiles) > 0) or (Pos ('*', sourceFiles) > 0) then
    FSourceMask := sourceFiles
  else
  begin
    st := Copy(sourceFiles, Length(sourceFiles), 1);
    if (st = '\') or (st = ':') then
      FSourceMask := sourceFiles + '*.*'
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
    err := FileGetAttr (sourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [sourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      FSourceMask := sourceFiles + '\*.*';
      FDestDir := FDestDir + ExtractFileName(sourceFiles) + '\'
    end
  end;


  if FSourceMask <> '' then
    ForEach(FSourceMask, FDestDir, AnalyzeFile, Nil)
  else
    if FindFirst(sourceFiles, faAnyFile, sr) = 0 then
    try
      Continue := True;
      AnalyzeFile(ExtractFilePath(sourceFiles), FDestDir, sr, Continue)
    finally
      FindClose(sr)
    end
    else
      raise EFOpenError.CreateFmt(rstFileNotFound, [sourceFiles]);

  if (CopyMode = cmCopy) or (ExtractFileDrive(sourceFiles) <> ExtractFileDrive(FDestDir)) then
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

procedure TFileCopier.CopyFile(const Directory, dest: string;
  const sr: TSearchRec; var Continue: Boolean);
begin
  FCurrentFileName := Directory + sr.Name;
  FDestFileName := dest + sr.Name;
  FCurrentFileSize := sr.Size;

  if Assigned(FOnStartCopyFile) then
    FCopierThread.Synchronize(FCopierThread, DoOnStartCopyFile);

  if CopyMode <> cmNothing then
    ForceDirectories (dest);

  case CopyMode of
    cmNothing:;
    cmMove: DoMoveFile;
    cmCopy: DoCopyFile;
  end;
  if Assigned(FOnEndCopyFile) then
    FCopierThread.Synchronize(FCopierThread, DoOnEndCopyFile);
end;

procedure TFileCopier.CopyFiles (const sourceFiles: string);
var
  sr: TSearchRec;
  Continue: Boolean;
  st: string;
  err: Integer;
begin
  FSourceMask := '';
  if (Pos ('?', sourceFiles) > 0) or (Pos ('*', sourceFiles) > 0) then
    FSourceMask := sourceFiles
  else
  begin
    st := Copy(sourceFiles, Length(sourceFiles), 1);
    if (st = '\') or (st = ':') then
      FSourceMask := sourceFiles + '*.*'
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
    err := FileGetAttr (sourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [sourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      FSourceMask := sourceFiles + '\*.*';
      FDestDir := FDestDir + ExtractFileName(sourceFiles) + '\'
    end
  end;

  if FSourceMask <> '' then
    ForEach(FSourceMask, FDestDir, CopyFile, TidyDir)
  else
    if FindFirst(sourceFiles, faAnyFile, sr) = 0 then
    try
      Continue := True;
      CopyFile(ExtractFilePath(sourceFiles), FDestDir, sr, Continue)
    finally
      FindClose(sr)
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
    OnEndAnalysis (self);
end;

procedure TFileCopier.DoOnEndCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnEndCopy) then
    OnEndCopy(self);
end;

procedure TFileCopier.DoOnEndCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnEndCopyFile) then
    OnEndCopyFile(self, FCurrentFileName, FDestFileName, FCurrentFileSize);
end;

procedure TFileCopier.DoOnException;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnException) then
    OnException (self, FExcep)
  else
    Application.ShowException(FExcep);
end;

procedure TFileCopier.DoOnStartAnalysis;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartAnalysis) then
    OnStartAnalysis (self);
end;

procedure TFileCopier.DoOnStartCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartCopy) then
    OnStartCopy(self);
end;

procedure TFileCopier.DoOnStartCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned(OnStartCopyFile) then
    OnStartCopyFile(self, FCurrentFileName, FDestFileName, FCurrentFileSize);
end;

function TFileCopier.ForEach(const Mask, destPath: string; proc, proc1: TForEachProc): Boolean;
var
  sr: TSearchRec;
  st: string;
begin
  Result := True;
  begin
    if FindFirst(Mask, faAnyFile, sr) = 0 then
    try
      repeat
        if (sr.Attr and faDirectory) <> 0 then
        begin
          if (sr.Name <> '.') and (sr.Name <> '..') and Recursive then
          begin
            st := destPath + sr.Name + '\';

            Result := ForEach(ExtractFilePath(Mask) + sr.Name + '\' + ExtractFileName(Mask), st, proc, proc1)
          end
        end
        else
          proc (ExtractFilePath(Mask), destPath, sr, Result);
        if FCancelled then
          Result := False;
      until not Result or (FindNext(sr) <> 0)
    finally
      FindClose(sr);
      if Assigned(proc1) then
        proc1 (ExtractFilePath(Mask), '', sr, Result)
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
    FCopierThread := TCopierThread.Create(self);
  end
  else
    raise EFileCopier.Create(rstAlreadyCopying);
end;

procedure TFileCopier.TidyDir(const Directory, dest: string;
  const sr: TSearchRec; var Continue: Boolean);
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
