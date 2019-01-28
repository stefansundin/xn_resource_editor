unit cmpFileCopier;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses Windows, Classes, SysUtils, Forms;

type
TOnDuplicates = (duCopy, duAsk, duAbort);
TCopyMode = (cmCopy, cmMove, cmNothing);
TOnCopyFile = procedure (sender : TObject; const srcfileName, dstFileName : string; fileSize : Integer) of object;
TForEachProc = procedure (const directory, dest : string; const sr : TSearchRec; var continue : boolean) of object;
TOnException = procedure (sender : TObject; e : Exception) of object;
TFileCopier = class (TComponent)
private
  fCopierThread : TThread;
  fProgressThread : TThread;
  fRecursive: boolean;
  fDestFiles: string;
  fDestDir : string;
  fSourceFiles: TStrings;
  fSourceMask : string;
  fOnStartCopy: TNotifyEvent;
  fOnEndCopy: TNotifyEvent;
  fOnEndCopyFile: TOnCopyFile;
  fOnStartCopyFile: TOnCopyFile;
  fOnDuplicates: TOnDuplicates;
  fExcep : Exception;

  fAnalyzedFileCount : Integer;
  fAnalyzedFileSize : int64;
  fLeeway: Int64;
  fCopyMode: TCopyMode;
  fCancelled : boolean;
  fOnStartAnalysis: TNotifyEvent;
  fOnEndAnalysis: TNotifyEvent;
  fCurrentFileName : string;
  fDestFileName : string;
  fCurrentFileSize : Integer;
  fCheckedDrive : boolean;
  fOnException: TOnException;

  procedure AnalyzeFile (const directory, dest : string; const sr : TSearchRec; var continue : boolean);
  procedure CopyFile (const directory, dest : string; const sr : TSearchRec; var continue : boolean);
  procedure TidyDir (const directory, dest : string; const sr : TSearchRec; var continue : boolean);
  function ForEach (const mask, destPath : string; proc, proc1 : TForEachProc) : boolean;
  procedure AnalyzeFiles (const sourceFiles : string);
  procedure CopyFiles (const sourceFiles : string);
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
  constructor Create (AOwner : TComponent); override;
  destructor Destroy; override;
  procedure Start;
  procedure Cancel;

  property AnalyzedFileSize : Int64 read fAnalyzedFileSize;
  property AnalyzedFileCount : Integer read fAnalyzedFileCount;

published
  property SourceFiles : TStrings read fSourceFiles write SetSourceFiles;
  property DestFiles : string read fDestFiles write fDestFiles;
  property Recursive : boolean read fRecursive write fRecursive default True;
  property OnDuplicates : TOnDuplicates read fOnDuplicates write fOnDuplicates;
  property Leeway : Int64 read fLeeway write fLeeway default 1024*1024;
  property CopyMode : TCopyMode read fCopyMode write fCopyMode;

  property OnStartAnalysis : TNotifyEvent read fOnStartAnalysis write fOnStartAnalysis;
  property OnEndAnalysis : TNotifyEvent read fOnEndAnalysis write fOnEndAnalysis;
  property OnStartCopy : TNotifyEvent read fOnStartCopy write fOnStartCopy;
  property OnEndCopy : TNotifyEvent read fOnEndCopy write fOnEndCopy;
  property OnStartCopyFile : TOnCopyFile read fOnStartCopyFile write fOnStartCopyFile;
  property OnEndCopyFile : TOnCopyFile read fOnEndCopyFile write fOnEndCopyFile;
  property OnException : TOnException read fOnException write fOnException;
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
  fOwner : TFileCopier;
protected
  procedure Execute; override;
public
  constructor Create (AOwner : TFileCopier);
end;

TCopierProgressThread = class (TThread)
public
  constructor Create (AOwner : TFileCopier);
end;

{ TFileCopier }

procedure TFileCopier.AnalyzeFile(const directory, dest : string; const sr : TSearchRec; var continue : boolean);
var
  destFileName : string;
  fa : Integer;
begin
  Inc (fAnalyzedFileCount);
  fAnalyzedFileSize := fAnalyzedFileSize + sr.Size;

  // Check we can open the source file for reading.
  with TFileStream.Create (directory + sr.Name, fmOpenRead or fmShareDenyNone) do
    Free;

  if not fCheckedDrive then
  begin
                // Check the dest drive or share is valid
    fCheckedDrive := True;
    destFileName := dest + sr.Name;
    fa := FileGetAttr (ExtractFileDrive (destFileName));
    if fa = -1 then
      raise Exception.Create('Destination drive or share not valid');

                // Check we can create files on the dest share or drive
    destFileName := ExtractFileDrive (destFilename) + 'copier.woozle.1122';
    if FileExists (destFileName) then
      DeleteFile (destFileName);
    with TFileStream.Create(destFileName, fmCreate or fmShareDenyNone) do
      Free;
    if not DeleteFile (destFileName) then
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

procedure TFileCopier.AnalyzeFiles (const sourcefiles : string);
var
  st : string;
  continue : boolean;
  sr : TSearchRec;
  err : Integer;
  available, a1 : Int64;
  a2 : TLargeInteger;
begin
  if Length (sourceFiles) = 0 then
    raise EFileCopier.Create(rstNoSource);

  if Length (fDestFiles) = 0 then
    raise EFileCopier.Create(rstNoDest);

  fSourceMask := '';
  if (Pos ('?', sourceFiles) > 0) or (Pos ('*', sourceFiles) > 0) then
    fSourceMask := sourceFiles
  else
  begin
    st := Copy (sourceFiles, Length (sourceFiles), 1);
    if (st = '\') or (st = ':') then
      fSourceMask := sourceFiles + '*.*'
  end;

  fDestDir := fDestFiles;
  err := FileGetAttr (fDestDir);
  if (err <> -1) and ((err and faDirectory) = 0) then
    raise EFileCopier.Create ('Destination must be a directory');

  st := Copy (fDestDir, Length (fDestDir), 1);
  if (st = '\') or (st = ':') then
    fDestDir := fDestDir
  else
    fDestDir := fDestDir + '\';

  if fSourceMask = '' then
  begin
    err := FileGetAttr (sourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [sourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      fSourceMask := sourceFiles + '\*.*';
      fDestDir := fDestDir + ExtractFileName (sourceFiles) + '\'
    end
  end;


  if fSourceMask <> '' then
    ForEach (fSourceMask, fDestDir, AnalyzeFile, Nil)
  else
    if FindFirst (sourceFiles, faAnyFile, sr) = 0 then
    try
      continue := True;
      AnalyzeFile (ExtractFilePath (sourceFiles), fDestDir, sr, continue)
    finally
      FindClose (sr)
    end
    else
      raise EFOpenError.CreateFmt (rstFileNotFound, [sourceFiles]);

  if (CopyMode = cmCopy) or (ExtractFileDrive (sourceFiles) <> ExtractFileDrive (fDestDir)) then
  begin
    SysUtils.GetDiskFreeSpaceEx (PChar (ExtractFileDrive (fDestDir)), available, a1, @a2);

    if fAnalyzedFileSize + fLeeway > available then
      raise EFileCopier.Create('Not enough space to copy files')
  end
end;

procedure TFileCopier.Cancel;
begin
  fCancelled := True;
end;

procedure TFileCopier.CopyFile(const directory, dest: string;
  const sr: TSearchRec; var continue: boolean);
begin
  fCurrentFileName := directory + sr.Name;
  fDestFileName := dest + sr.Name;
  fCurrentFileSize := sr.Size;

  if Assigned (fOnStartCopyFile) then
    fCopierThread.Synchronize(fCopierThread, DoOnStartCopyFile);

  if CopyMode <> cmNothing then
    ForceDirectories (dest);

  case CopyMode of
    cmNothing:;
    cmMove : DoMoveFile;
    cmCopy : DoCopyFile;
  end;
  if Assigned (fOnEndCopyFile) then
    fCopierThread.Synchronize(fCopierThread, DoOnEndCopyFile);
end;

procedure TFileCopier.CopyFiles (const sourceFiles : string);
var
  sr : TSearchRec;
  continue : boolean;
  st : string;
  err : Integer;
begin
  fSourceMask := '';
  if (Pos ('?', sourceFiles) > 0) or (Pos ('*', sourceFiles) > 0) then
    fSourceMask := sourceFiles
  else
  begin
    st := Copy (sourceFiles, Length (sourceFiles), 1);
    if (st = '\') or (st = ':') then
      fSourceMask := sourceFiles + '*.*'
  end;

  fDestDir := fDestFiles;
  err := FileGetAttr (fDestDir);
  if (err <> -1) and ((err and faDirectory) = 0) then
    raise EFileCopier.Create ('Destination must be a directory');

  st := Copy (fDestDir, Length (fDestDir), 1);
  if (st = '\') or (st = ':') then
    fDestDir := fDestDir
  else
    fDestDir := fDestDir + '\';

  if fSourceMask = '' then
  begin
    err := FileGetAttr (sourceFiles);
    if err = -1 then
      raise EFOpenError.CreateFmt(rstFileNotFound, [sourceFiles]);
    if (err and faDirectory) <> 0 then
    begin
      fSourceMask := sourceFiles + '\*.*';
      fDestDir := fDestDir + ExtractFileName (sourceFiles) + '\'
    end
  end;

  if fSourceMask <> '' then
    ForEach (fSourceMask, fDestDir, CopyFile, TidyDir)
  else
    if FindFirst (sourceFiles, faAnyFile, sr) = 0 then
    try
      continue := True;
      CopyFile (ExtractFilePath (sourceFiles), fDestDir, sr, continue)
    finally
      FindClose (sr)
    end
end;

constructor TFileCopier.Create(AOwner: TComponent);
begin
  inherited;

  fSourceFiles := TStringList.Create;
  fRecursive := True;
  fLeeway := 1024*1024;

end;

destructor TFileCopier.Destroy;
begin
  while Assigned (fCopierThread) do
    Sleep (500);
  FreeAndNil (fSourceFiles);
  inherited;
end;

procedure TFileCopier.DoCopyFile;
begin
  if not Windows.CopyFile(PChar (fCurrentFileName), PChar (fDestFileName), False) then
    RaiseLastOSError
end;

procedure TFileCopier.DoMoveFile;
begin
  if not SameText (ExtractFileDrive (fCurrentFileName), ExtractFileDrive (fDestFileName)) then
  begin
    if not Windows.CopyFile(PChar (fCurrentFileName), PChar (fDestFileName), False) then
      RaiseLastOSError;

    if not Windows.DeleteFile(PChar (fCurrentFileName)) then
      RaiseLastOSError;

  end
  else
  begin
    DeleteFile (fDestFileName);
    if not Windows.MoveFile(PChar (fCurrentFileName), PChar (fDestFileName)) then
      RaiseLastOSError
  end
end;

procedure TFileCopier.DoOnEndAnalysis;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnEndAnalysis) then
    OnEndAnalysis (self);
end;

procedure TFileCopier.DoOnEndCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnEndCopy) then
    OnEndCopy (self);
end;

procedure TFileCopier.DoOnEndCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnEndCopyFile) then
    OnEndCopyFile (self, fCurrentFilename, fDestFileName, fCurrentFileSize);
end;

procedure TFileCopier.DoOnException;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnException) then
    OnException (self, fExcep)
  else
    Application.ShowException(fExcep);
end;

procedure TFileCopier.DoOnStartAnalysis;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnStartAnalysis) then
    OnStartAnalysis (self);
end;

procedure TFileCopier.DoOnStartCopy;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnStartCopy) then
    OnStartCopy (self);
end;

procedure TFileCopier.DoOnStartCopyFile;
begin
  if (not (csDesigning in ComponentState)) and Assigned (OnStartCopyFile) then
    OnStartCopyFile (self, fCurrentFilename, fDestFileName, fCurrentFileSize);
end;

function TFileCopier.ForEach(const mask, destPath : string; proc, proc1: TForEachProc) : boolean;
var
  sr : TSearchRec;
  st : string;
begin
  result := True;
  begin
    if FindFirst (mask, faAnyFile, sr) = 0 then
    try
      repeat
        if (sr.Attr and faDirectory) <> 0 then
        begin
          if (sr.Name <> '.') and (sr.Name <> '..') and Recursive then
          begin
            st := destPath + sr.Name + '\';

            result := ForEach (ExtractFilePath (mask) + sr.Name + '\' + ExtractFileName (mask), st, proc, proc1)
          end
        end
        else
          proc (ExtractFilePath (mask), destPath, sr, result);
        if fCancelled then
          result := False;
      until not result or (FindNext (sr) <> 0)
    finally
      FindClose (sr);
      if Assigned (proc1) then
        proc1 (ExtractFilePath (mask), '', sr, result)
    end
    else
      RaiseLastOSError
  end
end;

procedure TFileCopier.SetSourceFiles(const Value: TStrings);
begin
  fSourceFiles.Assign(Value);
end;

procedure TFileCopier.Start;
begin
  fCancelled := False;
  fCheckedDrive := False;
  if not Assigned (fCopierThread) then
  begin
    fCopierThread := TCopierThread.Create(self);
  end
  else
    raise EFileCopier.Create(rstAlreadyCopying);
end;

procedure TFileCopier.TidyDir(const directory, dest: string;
  const sr: TSearchRec; var continue: boolean);
var
  st : string;
begin
  st := directory;
  if Copy (st, Length (st), 1) = '\' then
  begin
    Delete (st, Length (st), 1);
    RemoveDirectory (PChar (st)) // Don't error check this.  It may be open in a
                                 // command window or something...
  end
end;

{ TCopierThread }

constructor TCopierThread.Create(AOwner: TFileCopier);
begin
  fOwner := AOwner;
  inherited Create (True);
  FreeOnTerminate := True;
  resume;
end;

procedure TCopierThread.Execute;
var
  i : Integer;
begin
  try
    try
      Synchronize (fOwner.DoOnStartAnalysis);
      for i := 0 to fOwner.fSourceFiles.Count - 1 do
        fOwner.AnalyzeFiles (fOwner.fSourceFiles [i]);
      Synchronize (fOwner.DoOnEndAnalysis);
      Synchronize (fOwner.DoOnStartCopy);

      for i := 0 to fOwner.fSourceFiles.Count - 1 do
        fOwner.CopyFiles (fOwner.fSourceFiles [i]);
      Synchronize (fOwner.DoOnEndCopy)
    except
      on E : Exception do
      begin
        fOwner.fExcep := E;
        Synchronize (fOwner.DoOnException)
      end
    end
  finally
    fOwner.fCopierThread := Nil
  end
end;

{ TCopierProgressThread }

constructor TCopierProgressThread.Create(AOwner: TFileCopier);
begin

end;

end.
