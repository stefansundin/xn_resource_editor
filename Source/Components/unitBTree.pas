(*======================================================================*
 | unitBTree                                                            |
 |                                                                      |
 | Cached, variable length key B-Tree index unit.  Public classes are:  |
 |                                                                      |
 | TBTree               Associate an integer with a variable length     |
 |                      string                                          |
 |                                                                      |
 | TBTreeIterator       Iterate through a TBTree                        |
 |                                                                      |
 | TDataTree            Associate a variable length string with an      |
 |                      integer                                         |
 |                                                                      |
 | TDataTreeIterator    Iterate through a TDataTree                     |
 |                                                                      |
 | TIndexTree           Sparse array of integers (ie. associate an      |
 |                      integer with an integer)                        |
 |                                                                      |
 | The contents of this file are subject to the Mozilla Public License  |
 | Version 1.1 (the "License"); you may not use this file except in     |
 | compliance with the License. You may obtain a copy of the License    |
 | at http://www.mozilla.org/MPL/                                       |
 |                                                                      |
 | Software distributed under the License is distributed on an "AS IS"  |
 | basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See  |
 | the License for the specific language governing rights and           |
 | limitations under the License.                                       |
 |                                                                      |
 | Copyright © Colin Wilson 2003  All Rights Reserved
 |                                                                      |
 | Version  Date        By    Description                               |
 | -------  ----------  ----  ------------------------------------------|
 | 1.0      05/03/2003  CPWW  Original                                  |
 | 1.1      21/03/2002  CPWW  All working!                              |
 *======================================================================*)


unit unitBTree;
interface

uses Windows, Classes, SysUtils, ConTnrs, unitObjectCache;

type

//----------------------------------------------------------------
// Header for B-Tree page (in index file)
TPageHeader = packed record
  Flags : Integer;
  KeysOnPage : Integer;
  PrevPage : Integer;          // -1 = leaf
  PrevPageHeight : Integer;
end;

//----------------------------------------------------------------
// Header for B-Tree node (in index file)
TNodeHeader = packed record
  NextPage : Integer;
  NextPageHeight : Integer;
  KeyLen : Word;
end;
PNodeHeader = ^TNodeHeader;

const
  flgDirty = 1;                 // Page flags

  fflgCaseSensitive = 1;        // File flags
  fflgDupAccept = 2;
  fflgDupError = 4;
  fflgDupReplace = 8;
  fflgDupFlags = 14;

  PAGE_SIZE = 4096;
//  PAGE_SIZE = 256;
  PAGE_DATA_SIZE = PAGE_SIZE - sizeof (TPageHeader);

  MAX_KEY_LEN = (PAGE_DATA_SIZE div 2) - SizeOf (TNodeHeader);

  NO_CACHED_PAGES = 64;

type

//----------------------------------------------------------------
// Index file header
TFileInfo = packed record
  id : array [0..7] of char;
  Flags : Integer;
  PageCount : Integer;
  RootPage : Integer;
  RecordCount : Integer;
  FirstDeletedPage : Integer;
                                // Let the user use the otherwise unused
                                // space in the file header.
  ExtraDataSize : word;
  ExtraData : array [0..PAGE_SIZE - 5 * SizeOf (Integer) - 8 - 1 - sizeof (word)] of byte;
end;

//----------------------------------------------------------------
// B-Tree page (in index file)
TPageRec = packed record
  Header : TPageHeader;
  Data : Array [0..PAGE_DATA_SIZE - 1] of byte ;// The node data
end;

//----------------------------------------------------------------
// B-Tree node (in memory)
TNode = record
  key : string;
  NextPage : Integer;
  NextPageHeight : Integer;
end;

TRawBTree = class;

//----------------------------------------------------------------
// B-Tree page (in memory)
TPage = class
private
  fFlags : Integer;
  fPrevPage : Integer;
  fPrevPageHeight : Integer;
  fNodes : array of TNode;
  fNodeCount : Integer;

  fOwner : TRawBTree;
  fIdx : Integer;
  fTotalDataLen : Integer;
  function GetFlags(bits: Integer): boolean;
  procedure SetFlags(bits: Integer; const Value: boolean);

  function FindNode (const st : string; var idx : Integer) : boolean;
  procedure InsertNode (idx : Integer; const node : TNode);
  function GetNode (idx  :Integer) : TNode;
  function GetHeight: Integer;
public
  constructor Create (AOwner : TRawBTree; AIdx : Integer);
  destructor Destroy; override;

  procedure Load;
  procedure Save;

  property Flags [bits : Integer] : boolean read GetFlags write SetFlags;
  property Height : Integer read GetHeight;
  property Idx : Integer read fIDX;
  property NodeCount : Integer read fNodeCount;
  property Node [idx : Integer] : TNode read GetNode;
  property Owner : TRawBTree read fOwner;
  property PrevPage : Integer read fPrevPage;
  property PrevPageHeight : Integer read fPrevPageHeight;
end;

//----------------------------------------------------------------
// Page cache stores TPage objects.  CanRemove is overridden to save
// a page on disk befor it's removed
TPageCache = class (TObjectCache)
protected
  function CanRemove (AObject : TObject) : boolean; override;
end;

TRawBTreeForEachProc = procedure (const key : string; param : Integer; var continue : boolean) of object;

TBTreeDuplicates = (dupIgnore, dupAccept, dupError, dupReplace);

//----------------------------------------------------------------
// TRawBTree object
TRawBTree = class
private
  fFileName : string;
  f : TFileStream;
  fFileInfo : TFileInfo;
  fPageCache : TPageCache;

  fUpdateCount : Integer;

  fDelIDx, fDelPIdx : Integer;  // Used during delete.
  fOK : boolean;

  procedure Open;
  procedure Close;
  procedure Flush (clearPageCache : boolean);
  procedure SaveFileInfo;

  function GetPage (pageNo : Integer) : TPage;
  function GetCaseSensitive: boolean;
  procedure SetCaseSensitive(const Value: boolean);
  procedure CacheCheckProc (obj : TObject; idx, param : Integer; var continue : boolean);
  procedure CacheSaveProc (obj : TObject; idx, param : Integer; var continue : boolean);
  function CreateNewPage : TPage;
  procedure DeleteOldPage (page : TPage);

  function PutKeyOnPage (pg : TPage; idx : Integer; const memNode : TNode) : TNode;
  function PutKeyInTree(pg : TPage; const key : string) : TNode;
  function DeleteKeyFromTree (pg : TPage; const key : string) : Integer;
  function DeleteKeyFromPage (page : TPage; idx : Integer) : Integer;
  function GetRecordCount: Integer;
  function GetRootPage: TPage;
  procedure SetDuplicates(const Value: TBTreeDuplicates);
  function GetDuplicates: TBTreeDuplicates;
  function GetExtraData: string;
  procedure ResetNodeHeight (pg : TPage; idx : Integer);
  procedure SetExtraData(Value: string);
  function GetIndexOfKey(var key: string): Integer;
protected
  function GetKey(idx: Integer): string;
  function CompareKeys (const k1, k2 : string) : Integer; virtual;

  function AddKey (const key : string) : boolean;
  function DeleteKey (const key : string) : boolean;
  procedure ForEach (proc : TRawBTreeForEachProc; param : Integer);
  function Find (key : string; var fKey : string) : boolean;

  property Key [idx : Integer] : string read GetKey;
public
  constructor Create (const AFileName : string); virtual;
  destructor Destroy; override;

  procedure BeginUpdate;
  procedure EndUpdate;


  property ExtraData : string read GetExtraData write SetExtraData;

  property RecordCount : Integer read GetRecordCount;

// ------ May not always be public, but handy for diagnostics
  property RootPage : TPage read GetRootPage;
  property Page [pageNo : Integer] : TPage read GetPage;


  property FileName : string read fFileName;
  property CaseSensitive : boolean read GetCaseSensitive write SetCaseSensitive;
  property Duplicates : TBTreeDuplicates read GetDuplicates write SetDuplicates;
end;

//----------------------------------------------------------------
// TIteratorNode.  An element on the iterator's stack
TIteratorNode = class
private
  fPageNo: Integer;
  fKeyIdx: Integer;
public
  constructor Create (APageNo : Integer; AKeyIdx : Integer);

  property PageNo : Integer read fPageNo;
  property KeyIdx : Integer read fKeyIdx;
end;

//----------------------------------------------------------------
// TRawBTreeIterator.  Class for iterating through the BTree
TRawBTreeIterator = class
private
  fBTree : TRawBTree;
  fStack : TObjectStack;
  procedure ClearPageStack;
public
  constructor Create (ABTree : TRawBTree);
  destructor Destroy; override;

  function First (var key : string) : boolean;
  function Last (var key : string) : boolean;
  function Next (var key : string) : boolean;
  function Prev (var key : string) : boolean;
  function Find (var key : string) : boolean;

  property BTree : TRawBTree read fBTree;
end;

TBTreeForEachProc = procedure (const key : string; dataRec : Integer; var continue : boolean) of object;
TBTree = class (TRawBTree)
private
  function IntToBin (i : Integer) : string;
  function ExtractDataRec (const key : string) : Integer;
  function InternalGetKey(idx: Integer): string;
  function GetDataRec(const key: string): Integer;
  procedure SetDataRec(const key: string; const Value: Integer);
protected
  function CompareKeys (const k1, k2 : string) : Integer; override;
public
  function AddKey (const key : string; DataRec : Integer) : boolean;
  function DeleteKey (const key : string) : boolean;
  procedure ForEach (proc : TBTreeForEachProc);
  function Find (key : string; var dataRec : Integer) : boolean;

  function GetKey(idx: Integer; var dataRec : Integer): string;
  function GetIndexOfKey(var key: string; var dataRec : Integer): Integer;

  property Key [idx : Integer] : string read InternalGetKey;
  property DataRec [const key : string] : Integer read GetDataRec write SetDataRec;
end;

TDataTreeForEachProc = procedure (n : Integer; const st : string; var continue : boolean) of object;
TDataTree = class (TRawBTree)
private
  function IntToBin (i : Integer) : string;
  function BinToInt (const st : string) : Integer;
protected
  function CompareKeys (const k1, k2 : string) : Integer; override;
public
  function AddKey (n : Integer; const st : string) : boolean;
  function DeleteKey (n : Integer) : boolean;
  procedure ForEach (proc : TDataTreeForEachProc);
  function Find (n : Integer; var st : string) : boolean;
  function GetKey(idx: Integer; var dataRec : Integer): string;
end;

TBTreeIterator = class (TRawBTreeIterator)
private
  procedure SplitKey (var key : string; var dataRec : Integer);
public
  constructor Create (ABTree : TBTree);
  function First (var key : string; var dataRec : Integer) : boolean;
  function Last (var key : string; var dataRec : Integer) : boolean;
  function Next (var key : string; var dataRec : Integer) : boolean;
  function Prev (var key : string; var dataRec : Integer) : boolean;
  function Find (var key : string; var dataRec : Integer) : boolean;
end;

TDataTreeIterator = class (TRawBTreeIterator)
private
  procedure SplitKey (var n : Integer; var key : string);
public
  constructor Create (ADataTree : TDataTree);
  function First (var n : Integer; var st : string) : boolean;
  function Last (var n : Integer; var st : string) : boolean;
  function Next (var n : Integer; var st : string) : boolean;
  function Prev (var n : Integer; var st : string) : boolean;
  function Find (n : Integer; var st : string) : boolean;
end;

TIndexTreeForEachProc = procedure (i : Integer; var continue : boolean) of object;
TIndexTree = class (TRawBTree)
private
  fBinBuffer : string;
  procedure IntToBinBuffer (i : Integer);
  function BinToInt (const st : string) : Integer;
  function GetValue(n: Integer): Integer;
  function GetIndexOf(i: Integer): Integer;
protected
  function CompareKeys (const k1, k2 : string) : Integer; override;
public
  constructor Create (const AFileName : string); override;
  function AddKey (i : Integer) : boolean;
  function DeleteKey (i : Integer) : boolean;
  procedure ForEach (proc : TIndexTreeForEachProc);
  function Find (i : Integer) : boolean;

  function Delete (n : Integer) : boolean;
  property Value [n : Integer] : Integer read GetValue; default;
  property IndexOf [i : Integer] : Integer read GetIndexOf;
end;

EBTree = class (Exception);

implementation

resourcestring
  rstMustBeEmpty  = 'The BTree must be empty to perform this operation';
  rstKeyTooLong   = 'Key Too Long';
  rstDuplicateKey = 'Duplicate key %s';
  rstIndexExceedsBounds = 'Index exceeds bounds';

{ TRawBTree }

(*----------------------------------------------------------------------*
 | function TRawBTree.AddKey                                            |
 |                                                                      |
 | Add a key to the index                                               |
 |                                                                      |
 | Parameters:                                                          |
 |   key: string;               The key to add                          |
 *----------------------------------------------------------------------*)
function TRawBTree.AddKey(const key: string) : boolean;
var
  passout : TNode;
  newPage0 : TPage;
begin
  if Length (key) > MAX_KEY_LEN then
    raise EBTree.Create(rstKeyTooLong);

  BeginUpdate;
  try
    fOK := True;
    passout := PutKeyInTree (RootPage, key);

    if passout.NextPage <> -1 then
    begin
      newPage0 := CreateNewPage;
      newPage0.fPrevPage := fFileInfo.RootPage;
      fFileInfo.RootPage := newPage0.fIdx;
      PutKeyOnPage (newPage0, 0, passout);

      ResetNodeHeight (newPage0, -1);
      ResetNodeHeight (newPage0, 0);
    end;
    if fOK then
      Inc (fFileInfo.RecordCount)
  finally
    EndUpdate
  end;
  result := fOK
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.BeginUpdate                                      |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.BeginUpdate;
begin
  Inc (fUpdateCount);
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheCheckProc                                   |
 |                                                                      |
 | Callback for cache ForEach function - to find whether a page is      |
 | already in the cache.                                                |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheCheckProc(obj: TObject; idx, param: Integer;
  var continue: boolean);
begin
  continue := TPage (obj).fIdx <> param
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheSaveProc                                    |
 |                                                                      |
 | Callback for cache ForEach function - called to save each (dirty)    |
 | page when flushing the cache                                         |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheSaveProc(obj: TObject; idx, param: Integer;
  var continue: boolean);
begin
  TPage (obj).Save;
  continue := True
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Close                                            |
 |                                                                      |
 | Close the BTree                                                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Close;
begin
  Flush (True);         // Flush and clear cached pages
  FreeAndNil (f)        // Close the index file
end;

(*----------------------------------------------------------------------*
 | constructor TRawBTree.Create                                         |
 |                                                                      |
 | constructor for TRawBTree                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const AFileName: string    The index file name                     |
 *----------------------------------------------------------------------*)
function TRawBTree.CompareKeys(const k1, k2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr (k1, k2)
  else
    result := CompareText (k1, k2)
end;

constructor TRawBTree.Create(const AFileName: string);
begin
  fFileName := AFileName;
  fPageCache := TPageCache.Create(NO_CACHED_PAGES, True);
  Open
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.CreateNewPage                                     |
 |                                                                      |
 | Create a new page and cache it.                                      |
 |                                                                      |
 | The function returns the new TPage                                   |
 *----------------------------------------------------------------------*)
function TRawBTree.CreateNewPage: TPage;
begin
  result := TPage.Create (self, fFileInfo.PageCount);
  if fFileInfo.FirstDeletedPage <> -1 then
  begin
    result.fIdx := fFileInfo.FirstDeletedPage;
    result.Load;
    fFileInfo.FirstDeletedPage := result.PrevPage;
    result.fPrevPage := -1
  end;
  fPageCache.Add(result);
  result.Flags [flgDirty] := True;
  Inc (fFileInfo.PageCount);
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTree.Destroy                                         |
 |                                                                      |
 | Destructor for TRawBTree.                                            |
 *----------------------------------------------------------------------*)
function TRawBTree.DeleteKey(const key: string) : boolean;
var
  page0 : TPage;
begin
  fDelPIdx := -1;
  BeginUpdate;
  try
    fOK := True;
    DeleteKeyFromTree (RootPage, key);
    if fOK then
    begin
      page0 := RootPage;
      if page0.NodeCount = 0 then
      begin
        if page0.PrevPage <> -1 then
        begin
          fFileInfo.RootPage := page0.PrevPage;
          ResetNodeHeight (RootPage, -1);
          DeleteOldPage (page0)
        end
      end;

      if fOK then
        Dec (fFileInfo.RecordCount)
    end
  finally
    EndUpdate
  end;

  result := fOK
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.DeleteKeyFromPage                                 |
 |                                                                      |
 | Delete a key from a page.                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;               The page to delete from                 |
 |   idx : Integer              The node index on the page              |
 |                                                                      |
 | The function returns the number of nodes remaining on the page       |
 *----------------------------------------------------------------------*)
function TRawBTree.DeleteKeyFromPage(page: TPage; idx : Integer) : Integer;
var
  dl : Integer;
begin
  dl := SizeOf (TNodeHeader) + Length (page.fNodes [idx].key);
  finalize (page.fNodes [idx]);
  FillChar (page.fNodes [idx], SizeOf (TNode), 0);

  if idx < page.fNodeCount - 1 then
  begin
    Move (page.fNodes [idx + 1], page.fNodes [idx], (page.fNodeCount - idx - 1) * sizeof (TNode));
    FillChar (page.fNodes [page.fNodeCount - 1], Sizeof (TNode), 0)
  end;

  Dec (page.fTotalDataLen, dl);
  Dec (page.fNodeCount);
  page.Flags [flgDirty] := True;
  result := page.fNodeCount
end;

function TRawBTree.DeleteKeyFromTree(pg: TPage; const key: string) : Integer;
var
  idx, pidx, nidx, tidx, mp : Integer;
  tn : TNode;
  tp, br : TPage;
  found : boolean;

begin
  pidx := pg.Idx;
  found := pg.FindNode (key, idx);
  if found then
  begin
    fDelPidx := pg.Idx;
    fDelIdx := idx;
    nidx := idx + 1
  end
  else
    nidx := idx;

  if pg.PrevPage <> -1 then
  begin
    if nidx = 0 then
      tp := Page [pg.PrevPage]
    else
      tp := Page [pg.Node [nidx - 1].NextPage];

    tidx := tp.Idx;

    if DeleteKeyFromTree (tp, key) = 0 then

    begin       // The pg we deleted from is now empty.  Some adjustments must
                // be made...

      tp := Page [tidx];        // Make sure these still exist in the cache
      pg := Page [pidx];

      if tp.Idx = pg.PrevPage then
      begin
        idx := 0;
        br := Page [pg.Node [0].NextPage];
        mp := 0;
      end
      else
      begin
        idx := nidx - 1;
        if idx = 0 then
          br := Page [pg.PrevPage]
        else
          br := Page [pg.Node [idx - 1].NextPage];
        mp := br.NodeCount;
      end;

      // Here:
      // tp is the empty pg
      // br is the non-empty pg to borrow or merge with
      // pg.fNodes [idx] is the node on this pg to rotate around
      // mp is the insertion pos on br.
      // if mp=0 then we're rotating left to the PrevPage - otherwise we're
      // rotating right to pg.fNodes [idx]'s Next pg

      if br.NodeCount = 1 then  // merge with neighbour
      begin
        if mp = 0 then          // PageLeft is empty.  Move current key into it's
        begin                   // right branch
          pg.fNodes [idx].NextPage := br.fPrevPage;
          br.fPrevPage := tp.PrevPage;
          pg.fPrevPage := br.Idx;
        end
        else                    // Current key's pg right is empty.  Move current
                               // key to it's left sibling's children
          pg.fNodes [idx].NextPage := tp.PrevPage;

        PutKeyOnPage (br, mp, pg.Node [idx]);
        ResetNodeHeight (br, mp);
        ResetNodeHeight (br, -1);
        DeleteKeyFromPage (pg, idx);
        DeleteOldPage (tp);
      end
      else                      // Borrow from neighbour
      begin
        PutKeyOnPage (tp, 0, pg.Node [idx]);
        if mp = 0 then
        begin
          tp.fNodes [0].NextPage := br.PrevPage;
          br.fPrevPage := br.Node [mp].NextPage;
          br.fPrevPageHeight := br.Node [mp].NextPageHeight
        end
        else
        begin
          Dec (mp);
          tp.fNodes [0].NextPage := tp.PrevPage;
          tp.fPrevPage := br.Node [mp].NextPage;
        end;
        pg.fNodes [idx].key := br.Node [mp].key;
        DeleteKeyFromPage (br, mp);

        ResetNodeHeight (tp, -1);
        ResetNodeHeight (tp, 0)
      end;
      pg.Flags [flgDirty] := True;
      br.Flags [flgDirty] := True;

      if idx < pg.NodeCount then
        ResetNodeHeight (pg, idx);
      if idx > 0 then
        ResetNodeHeight (pg, idx - 1);
      ResetNodeHeight (pg, -1);
    end
    else
    begin
      pg := Page [pidx];
      ResetNodeHeight (pg, nidx - 1);
    end
  end
  else
  begin
    if found then
    begin
      if fDelPIdx <> -1 then
      begin
        if fDelPIdx <> pidx then
        begin       // Move node to delete to the leaves
          tp := Page [fDelPIdx];
          tn := pg.Node [idx];
          pg.fNodes [idx].key := tp.Node [fDelIdx].key;
          tp.fNodes [fDelIdx].key := tn.Key;
          tp.Flags [flgDirty] := True;
        end;

        DeleteKeyFromPage (pg, idx)
      end
    end
    else
      fOK := False
  end;

  result := pg.NodeCount
end;

procedure TRawBTree.DeleteOldPage(page: TPage);
begin
  page.fPrevPage := fFileInfo.FirstDeletedPage;
  fFileInfo.FirstDeletedPage := page.fIdx;
  page.Flags [flgDirty] := True;
  page.fNodeCount := 0;
  SetLength (page.fNodes, 0);
  fPageCache.Remove(page);
end;

destructor TRawBTree.Destroy;
begin
  Close;
  fPageCache.Free
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.EndUpdate                                        |
 |                                                                      |
 | Pair to BeginUpdate.  If updated have finished, flush the cached     |
 | pages.                                                               |
 *----------------------------------------------------------------------*)
procedure TRawBTree.EndUpdate;
begin
  if fUpdateCount > 0 then
  begin
    Dec (fUpdateCount);
    if fUpdateCount = 0 then
      Flush (False)
  end
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.Find                                              |
 |                                                                      |
 | Find a key                                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   key: string;               The key to find                         |
 |   var DataRec: Integer       Returns the associated data rec         |
 |                                                                      |
 | The function returns True if the key was found.                      |
 *----------------------------------------------------------------------*)
function TRawBTree.Find(key: string; var fKey : string): boolean;
var
  pg : TPage;
  idx : Integer;
begin
  pg := RootPage;
  while pg <> Nil do
  begin
    if pg.FindNode(key, idx) then
    begin                               // Found it!
      fKey := pg.Node [idx].key;
      break;
    end;

    if idx = 0 then
      idx := pg.PrevPage                      // Search pg left
    else
      idx := pg.node [idx-1].NextPage;        // Search node right

    if idx > -1 then
      pg := Page [idx]
    else
      pg := Nil
  end;

  result := pg <> Nil
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Flush                                            |
 |                                                                      |
 | Flush the cached pages, and optionally clear the cache.              |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Flush (clearPageCache : boolean);
begin
  if fUpdateCount > 0 then Exit;

  if clearPageCache then
    fPageCache.Clear
  else
    fPageCache.ForEach(CacheSaveProc, 0);

  SaveFileInfo
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.ForEach                                          |
 |                                                                      |
 | Call 'proc' for each key in the tree in key order.                   |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TBTreeForEachProc            The procedure to call for each  |
 |                                      key.                            |
 *----------------------------------------------------------------------*)
procedure TRawBTree.ForEach(proc: TRawBTreeForEachProc; param : Integer);
var
  continue : boolean;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach (pageNo : Integer);
  var
    i : Integer;
    node : TNode;
    pg : TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach (pg.PrevPage);
      pg := Page [pageNo]               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node [i];
        Proc (node.key, param, continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach (node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach (fFileInfo.RootPage)
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetCaseSensitive                                  |
 |                                                                      |
 | 'Get' method for CaseSensitive property                              |
 *----------------------------------------------------------------------*)
function TRawBTree.GetCaseSensitive: boolean;
begin
  result := (fFileInfo.Flags and fflgCaseSensitive) <> 0;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetDuplicates                                     |
 |                                                                      |
 | 'Get' method for Duplicates property.                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetDuplicates: TBTreeDuplicates;
begin
  if (fFileInfo.Flags and fflgDupAccept) <> 0 then
    result := dupAccept
  else
    if (fFileInfo.Flags and fflgDupError) <> 0 then
      result := dupError
    else
      if (fFileInfo.Flags and fflgDupReplace) <> 0 then
        result := dupReplace
      else
        result := dupIgnore
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetExtraData                                      |
 |                                                                      |
 | 'Get' method for ExtraData property                                  |
 *----------------------------------------------------------------------*)
function TRawBTree.GetExtraData: string;
begin
  SetLength (result, fFileInfo.ExtraDataSize);
  Move (fFileInfo.ExtraData [0], result [1], fFileInfo.ExtraDataSize)
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetPage                                           |
 |                                                                      |
 | 'Get' method for Page property.  Return Page [pageno]                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetIndexOfKey(var key: string): Integer;
var
  pg : TPage;
  i, idx : Integer;
begin
  pg := RootPage;
  result := 0;
  while pg <> Nil do
  begin
    if pg.FindNode(key, idx) then
    begin                               // Found it!
      key := pg.Node [idx].key;
      Inc (result, pg.PrevPageHeight);
      Inc (result, idx);
      for i := 0 to idx - 2 do
        Inc (result, pg.Node [i].NextPageHeight);
      break;
    end;

    if idx = 0 then
      idx := pg.PrevPage                      // Search pg left
    else
    begin
      Inc (result, pg.PrevPageHeight);

      Inc (result, idx);
      for i := 0 to idx - 2 do
        Inc (result, pg.Node [i].NextPageHeight);
      idx := pg.node [idx-1].NextPage;        // Search node right
    end;

    if idx > -1 then
      pg := Page [idx]
    else
      pg := Nil
  end;

  if pg = Nil then
    result := -1
end;

function TRawBTree.GetKey(idx: Integer): string;

  function gk (root : TPage; idx : Integer) : string;
  var
    i : Integer;
  begin
    if root.PrevPage = -1 then
      result := root.Node [idx].key
    else
    begin
      if idx < root.PrevPageHeight then
        result := gk (Page [root.PrevPage], idx)
      else
      begin
        Dec (idx, root.PrevPageHeight);

        i := 0;
        while (i < root.NodeCount) and (idx > 0) do
        begin
          if idx <= root.Node [i].NextPageHeight then
          begin
            result := gk (Page [root.Node [i].NextPage], idx - 1);
            break
          end;

          Dec (idx, 1 + root.Node [i].NextPageHeight);
          Inc (i)
        end;

        if idx = 0 then
          result := root.Node [i].key
      end
    end
  end;
begin
  if idx >= RecordCount then
    raise EBTree.Create(rstIndexExceedsBounds);
  result := gk (RootPage, idx)
end;

function TRawBTree.GetPage(pageNo: Integer) : TPage;
var
  idx : Integer;
begin
  idx := fPageCache.ForEachIdx(CacheCheckProc, pageNo);

  if idx >= 0 then
  begin
    result := TPage (fPageCache.ObjectAt (idx));
    fPageCache.BringToFrontObject(idx)
  end
  else
  begin
    result := TPage.Create(self, pageNo);
    result.Load;
    fPageCache.Add(result);
  end
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRecordCount                                    |
 |                                                                      |
 | 'Get' method for RecordCount property                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRecordCount: Integer;
begin
  result := fFileInfo.RecordCount;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRootPage                                       |
 |                                                                      |
 | 'Get' method for RootPage property                                   |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRootPage: TPage;
begin
  result := Page [fFileInfo.RootPage];
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Open                                             |
 |                                                                      |
 | Open the BTree                                                       |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Open;
var
  page0 : TPage;
  id : string;
begin
  if not FileExists (FileName) then
  begin                                 // New file
    f := TFileStream.Create (FileName, fmCreate);
    try
                                        // Create file info page and empty root page
      id := 'BTWoozle';
      Move (id [1], fFileInfo.id [0], 8);
      page0 := TPage.Create (self, 0);
      try
        page0.Flags [flgDirty] := True;
        page0.Save;
      finally
        page0.Free
      end;
      fFileInfo.PageCount := 1;
      fFileInfo.FirstDeletedPage := -1;
      fFileInfo.Flags := fflgCaseSensitive;
      SaveFileInfo;
    finally
      FreeAndNil (f)                    // Close newly created file
    end
  end;

                                        // Open existing file
  f := TFileStream.Create (FileName, fmOpenReadWrite or fmShareDenyWrite);
                                        // Read file info page.
  f.Read(fFileInfo, sizeof (fFileInfo));
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.PutKeyInTree                                      |
 |                                                                      |
 | Put key in the tree at page.  This may return a node which should be |
 | put in the previous page - or form the new root.  If the returned    |
 | node.NextOage is -1 then no node was returned.                       |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;                       Root page of tree               |
 |   const key: string;                 The key to add                  |
 |   DataRec: Integer                   DataRec associated with the key |
 |                                                                      |
 | The function returns a new root node if retval.NextPage <> -1        |
 *----------------------------------------------------------------------*)
function TRawBTree.PutKeyInTree(pg : TPage; const key: string): TNode;
var
  pidx, idx : Integer;
begin
  pidx := pg.Idx;
  if pg.FindNode (key, idx) then
    case Duplicates of
      dupIgnore, dupReplace :
        begin
          fOK := False;

          if (Duplicates = dupReplace) and (key <> pg.fNodes [idx].key) then
          begin
            pg.fNodes [idx].key := key;
            pg.Flags[flgDirty] := True
          end;
          result.NextPage := -1;
          Exit;
        end;
      dupAccept :;
      dupError  : raise EBTree.Create(Format (rstDuplicateKey, [key]));
    end;

  if pg.PrevPage = -1 then            // We're at the leaves.
  begin
    result.key := key;
    result.NextPage := -1;
    result.NextPageHeight := 0;
    result := PutKeyOnPage (pg, idx, result)
  end
  else
  begin                                 // Not at leaves - put key in pg left
    if idx > 0 then                     // or node right
    begin
      result := PutKeyInTree (Page [pg.Node [idx - 1].NextPage], key);
      ResetNodeHeight (pg, idx - 1);
    end
    else
    begin
      result := PutKeyInTree (Page [pg.PrevPage], key);
      ResetNodeHeight (pg, -1)
    end;

                                        // If a node was passed out by inserting to
                                        // the child, put it on this pg.
    if result.NextPage <> -1 then
    begin
      pg := Page [pidx];                // Just possible that the original pg is
                                        // no longer in the cache.  Make sure it is.
      result := PutKeyOnPage (pg, idx, result);
    end
  end
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.PutKeyOnPage                                      |
 |                                                                      |
 | Put a node on the page.                                              |
 |                                                                      |
 | If the page is full, split the page.        |                        |
 | The original page contains the lower nodes - up to the midpoint.     |
 | The function returns the midpoint node, pointing to a new page       |
 | containing the upper nodes.                                          |
 |                                                                      |
 | If the page is not full, the node is put on the page, and the        |
 | function returns a node with NextPage = -1 - indicating that no      |
 | split took place.                                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;               The page to put the node on.            |
 |   idx : Integer;             The position on the page to put the     |
 |                              node.                                   |
 |   const memNode : TNode      The node to put.                        |
 |                                                                      |
 | The function returns a TNode.  If retval.NextPage is -1 then         |
 | retval is meaningless - otherwise it contains a node pointing to a   |
 | page of higher nodes to be inserted in a lower page.                 |
 *----------------------------------------------------------------------*)
function TRawBTree.PutKeyOnPage(pg: TPage;  idx : Integer; const memNode : TNode) : TNode;
var
  i, iidx, ts : Integer;
  newPage : TPage;
begin
  if pg.fTotalDataLen + SizeOf (TNodeHeader) + Length (memNode.key) > PAGE_DATA_SIZE then

  begin                 // Key doesn't fit on the pg.
                        // Split the pg into pg & newPage, and
                        // passout the midpoint key, pointing to the new pg.

    ts := 0;
    iidx := 0;          // Find mid point - iidx
    while (ts <= PAGE_DATA_SIZE div 2) and (iidx < pg.NodeCount) do
    begin
      Inc (ts, Length (pg.Node [iidx].key) + SizeOf (TNodeHeader));
      Inc (iidx)
    end;

    Dec (iidx);

                        // Passout mid point key
    if idx = iidx then
    begin
      result := memNode;
      ts := iidx
    end
    else
    begin
      if idx > iidx then
        ts := iidx + 1
      else
      begin
        ts := iidx;
        Dec (iidx)
      end;
      result := pg.Node [iidx];
    end;

    newPage := CreateNewPage;
    newPage.fPrevPage := result.NextPage;
    result.NextPage := newPage.fIdx;

                        // Move keys above midpoint to new pg.

    newPage.fNodeCount := pg.NodeCount - ts;
    if Length (newPage.fNodes) < newPage.fNodeCount then
      SetLength (newPage.fNodes, newPage.fNodeCount);

    for i := ts to pg.NodeCount - 1 do
    begin
      newPage.fNodes [i - ts] := pg.Node [i];
//      newPage.InsertNode (i - ts, pg.Node [i]);
      Inc (newPage.fTotalDataLen, SizeOf (TNodeHeader) + Length (pg.Node [i].key));
    end;

                        // Truncate current pg

    pg.fNodeCount := iidx;

    pg.Flags[flgDirty] := True;
                        // Recalc TotalDataLen for truncated pg
    pg.fTotalDataLen := 0;
    for i := 0 to iidx - 1 do
      Inc (pg.fTotalDataLen, SizeOf (TNodeHeader) + Length (pg.Node [i].key));

    if ts <> iidx then
      if idx <= iidx then // Put the new key on the old or new pg.
        PutKeyOnPage (pg, idx, memNode)
      else
        PutKeyOnPage (newPage, idx - pg.NodeCount - 1, memNode);

    ResetNodeHeight (newPage, -1);
    result.NextPageHeight := newPage.Height;
  end
  else                  // pg not full - just add the node
  begin
    pg.InsertNode (idx, memNode);
    Inc (pg.fTotalDataLen, SizeOf (TNodeHeader) + Length (memNode.key));
    pg.Flags [flgDirty] := True;
    result.NextPage := -1;  // Don't pass out anything.
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SaveFileInfo                                     |
 |                                                                      |
 | Save the FileInfo record at the start of the index file.             |
 *----------------------------------------------------------------------*)
procedure TRawBTree.ResetNodeHeight(pg: TPage; idx: Integer);
var
  node : TNode;
  pint : PInteger;
  val : Integer;
begin
  if idx = -1 then
  begin
    PInt := @pg.fPrevPageHeight;
    if pg.PrevPage = -1 then
      val := 0
    else
      val := Page [pg.PrevPage].Height
  end
  else
  begin
    if idx >= pg.NodeCount then
      exit;
    PInt := @pg.fNodes [idx].NextPageHeight;
    node := pg.Node [idx];
    if node.NextPage = -1 then
      val := 0
    else
      val := Page [node.NextPage].Height
  end;

  if PInt^ <> val then
  begin
    PInt^ := val;
    pg.Flags [flgDirty] := True
  end
end;

procedure TRawBTree.SaveFileInfo;
begin
  if fUpdateCount > 0 then Exit;
  if Assigned (f) then
  begin
    f.Seek(0, soFromBeginning);
    f.Write (fFileInfo, SizeOf (fFileInfo))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SetCaseSensitive                                 |
 |                                                                      |
 | 'Set' method for CaseSensitive property.  The btree must be empty    |
 | to call this.                                                        |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetCaseSensitive(const Value: boolean);
begin
  if Value <> CaseSensitive then
  begin
    if Assigned (f) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create (rstMustBeEmpty);

    if Value then
      fFileInfo.Flags := fFileInfo.Flags or fflgCaseSensitive
    else
      fFileInfo.Flags := fFileInfo.Flags and (not fflgCaseSensitive);

    if Assigned (f) then
      SaveFileInfo
  end
end;

(*----------------------------------------------------------------------*
 | TRawBTree.SetDuplicates                                              |
 |                                                                      |
 | 'Set' method for Duplicates property.  The b-tree must be empty to   |
 | call this.                                                           |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetDuplicates(const Value: TBTreeDuplicates);
begin
  if Value <> Duplicates then
  begin
    if Assigned (f) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create (rstMustBeEmpty);

    fFileInfo.Flags := fFileInfo.Flags and not fflgDupFlags;

    case Value of
      dupAccept : fFileInfo.Flags := fFileInfo.Flags or fflgDupAccept;
      dupError : fFileInfo.Flags := fFileInfo.Flags or fflgDupError;
      dupReplace : fFileInfo.Flags := fFileInfo.Flags or fflgDupReplace;
    end;

    if Assigned (f) then
      SaveFileInfo
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SetExtraData                                     |
 |                                                                      |
 | 'Set' method for ExtraData property                                  |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetExtraData(Value: string);
begin
  if Length (value) > SizeOf (fFileInfo.ExtraData) then
    SetLength (value, SizeOf (fFileInfo.ExtraData));

  fFileInfo.ExtraDataSize := Length (Value);
  if Length (Value) > 0 then
    Move (Value [1], fFileInfo.ExtraData [0], Length (value));

  SaveFileInfo
end;

{ TPage }

(*----------------------------------------------------------------------*
 | constructor TPage.Create                                             |
 |                                                                      |
 | Constructor for TPage                                                |
 *----------------------------------------------------------------------*)
constructor TPage.Create (AOwner : TRawBTree; AIdx : Integer);
begin
  fOwner := AOwner;
  fPrevPage := -1;
  fIdx := AIdx;
  SetLength (fNodes, 384);
end;

(*----------------------------------------------------------------------*
 | destructor TPage.Destroy                                             |
 |                                                                      |
 | Destructor for TPage                                                 |
 *----------------------------------------------------------------------*)
destructor TPage.Destroy;
begin
  Save;

  finalize (fNodes);            // Free memory used by the fNodes dynamic array
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPage.FindNode                                              |
 |                                                                      |
 | Find a node on the page that matches the key.  If no match is found  |
 | return False and set idx to the position where the key should be     |
 | inserted.                                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   const st: string;          The key to find                         |
 |  var idx: Integer            If the key was found, returns the       |
 |                              index.  If it wasn't found, returns the |
 |                              insertion position.                     |
 |                                                                      |
 | The function returns True if the key was found.                      |
 *----------------------------------------------------------------------*)
function TPage.FindNode(const st: string; var idx: Integer): boolean;

//------------------------------------------------------
// Binary search the page
  function bsearch (s, e : Integer) : boolean;
  var
    cmp : Integer;
  begin
    if e >= s then
    begin
      idx := s + (e - s) div 2;

      cmp := Owner.CompareKeys (st, Node [idx].key);

      if cmp < 0 then
        result := bsearch (s, idx - 1)
      else
        if cmp > 0 then
          result := bsearch (idx + 1, e)
        else
          result := True
    end
    else
      result := False;
  end;

begin
  idx := 0;
  result := bsearch (0, NodeCount - 1);

  if (not result) and (idx < NodeCount) then
  // Adjust 'idx' so that it contains the correct insertion point
    if Owner.CompareKeys (Node [idx].key, st) < 0 then
      Inc (idx)
end;

(*----------------------------------------------------------------------*
 | function TPage.GetFlags                                              |
 |                                                                      |
 | 'Get' method for Flags property                                      |
 *----------------------------------------------------------------------*)
function TPage.GetFlags(bits: Integer): boolean;
begin
  result := (fFlags and bits) = bits;
end;

(*----------------------------------------------------------------------*
 | function TPage.GetNode                                               |
 |                                                                      |
 | 'Get' method for the 'Node' property                                 |
 *----------------------------------------------------------------------*)
function TPage.GetHeight: Integer;
var
  i : Integer;
begin
  result := NodeCount + PrevPageHeight;
  for i := 0 to NodeCount - 1 do
    Inc (result, Node [i].NextPageHeight)
end;

function TPage.GetNode(idx: Integer): TNode;
begin
  if idx = -1 then
    idx := NodeCount - 1;
  result := fNodes [idx];
end;

(*----------------------------------------------------------------------*
 | procedure TPage.InsertNode                                           |
 |                                                                      |
 | Insert a node on the page.                                           |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer;              Where to insert the node                |
 |   const node: TNode          The node to insert.                     |
 *----------------------------------------------------------------------*)
procedure TPage.InsertNode(idx: Integer; const node: TNode);
begin
  if idx > NodeCount then               // Reality check
    raise EBTree.Create('Error in Insert Node');

                                        // Grow the dynamic array if there's
                                        // no room
  if NodeCount = Length (fNodes) then
    SetLength (fNodes, Length (fNodes) + 64);

  if idx < NodeCount then               // If we're inserting within the existing
                                        // nodes, create a space by moving nodes.
  begin
    Move (fNodes [idx], fNodes [idx + 1], (NodeCount - idx) * sizeof (TNode));
                                        // Very important to clear the space -
                                        // otherwise stings will go mad.
    FillChar (fNodes [idx], SizeOf (TNode), 0);
  end;

  fNodes [idx] := node;
  Inc (fNodeCount)
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Load                                                 |
 |                                                                      |
 | Load a page.                                                         |
 *----------------------------------------------------------------------*)
procedure TPage.Load;
var
  pr : TPageRec;
  i : Integer;
  p : PChar;
  pnh : PNodeHeader;
  st : string;
begin
        // Read the page
  Owner.f.Seek(SizeOf (TFileInfo) + fIdx * sizeof (TPageRec), soFromBeginning);
  Owner.f.Read(pr, sizeof (pr));

        // Get headeer info
  self.fFlags := pr.Header.Flags;
  self.fPrevPage := pr.Header.PrevPage;
  self.fPrevPageHeight := pr.Header.PrevPageHeight;

  p := @pr.Data [0];
  if pr.Header.KeysOnPage > Length (fNodes) then
    SetLength (fNodes, pr.Header.KeysOnPage);

  fNodeCount := 0;
        // Decode the nodes.
  for i := 0 to Integer (pr.Header.KeysOnPage) - 1 do
  begin
    pnh := PNodeHeader (p);
    Inc (p, SizeOf (TNodeHeader));
    Inc (fTotalDataLen, sizeof (TNodeHeader) + pnh^.KeyLen);

    SetString (st, p, pnh^.KeyLen);
    Inc (p, pnh^.KeyLen);

    fNodes [fNodeCount].key := st;
    fNodes [fNodeCount].NextPage := pnh^.NextPage;
    fNodes [fNodeCount].NextPageHeight := pnh^.NextPageHeight;

    Inc (fNodeCount)
  end;

  Flags [flgDirty] := False
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Save                                                 |
 |                                                                      |
 | Save a page                                                          |
 *----------------------------------------------------------------------*)
procedure TPage.Save;
var
  pr : TPageRec;
  i : Integer;
  p : PChar;
  pnh : PNodeHeader;
  st : string;
  nd : TNode;
begin
  if not Flags [flgDirty] then Exit;    // No need to save

  pr.Header.Flags := fFlags;            // Set the header info.
  pr.Header.KeysOnPage := NodeCount;
  pr.Header.PrevPage := PrevPage;
  pr.Header.PrevPageHeight := PrevPageHeight;
  FillChar (pr.Data, SizeOf (pr.Data), 0);

  p := @pr.Data [0];
                                        // Encode the nodes
  for i := 0 to NodeCount - 1 do
  begin
    nd := Node [i];
    pnh := PNodeHeader (p);
    pnh^.NextPage := nd.NextPage;
    pnh^.NextPageHeight := nd.NextPageHeight;
    st := nd.key;
    pnh^.KeyLen := Length (st);

    Inc (p, SizeOf (TNodeHeader));
    Move (st [1], p^, pnh^.KeyLen);
    Inc (p, pnh^.KeyLen)
  end;
                                        // Write the data
  Owner.f.Seek(SizeOf (TFileInfo) + fIdx * sizeof (TPageRec), soFromBeginning);
  Owner.f.Write(pr, sizeof (pr));

  Flags [flgDirty] := False
end;

(*----------------------------------------------------------------------*
 | procedure TPage.SetFlags                                             |
 |                                                                      |
 | 'Set' method for flags property                                      |
 *----------------------------------------------------------------------*)
procedure TPage.SetFlags(bits: Integer; const Value: boolean);
begin
  if Value then
    fFlags := fFlags or bits
  else
    fFlags := fFlags and not bits
end;

{ TPageCache }

(*----------------------------------------------------------------------*
 | function TPageCache.CanRemove                                        |
 |                                                                      |
 | CanRemove is called when the cache removes a page from itself.       |
 | Override it to save the page.                                        |
 *----------------------------------------------------------------------*)
function TPageCache.CanRemove(AObject: TObject): boolean;
begin
  TPage (AObject).Save;
  result := True
end;

{ TRawBTreeIterator }

(*----------------------------------------------------------------------*
 | procedure TRawBTreeIterator.ClearPageStack                           |
 |                                                                      |
 | Clear the iterator stack.                                            |
 *----------------------------------------------------------------------*)
procedure TRawBTreeIterator.ClearPageStack;
begin
  while fStack.Count > 0 do
    fStack.Pop.Free
end;

(*----------------------------------------------------------------------*
 | constructor TRawBTreeIterator.Create                                 |
 |                                                                      |
 | Constructor for TRawBTreeIterator                                    |
 *----------------------------------------------------------------------*)
constructor TRawBTreeIterator.Create(ABTree: TRawBTree);
begin
  fBTree := ABTree;
  fStack := TObjectStack.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTreeIterator.Destroy                                 |
 |                                                                      |
 | Destructor for TRawBTreeIterator                                     |
 *----------------------------------------------------------------------*)
destructor TRawBTreeIterator.Destroy;
begin
  ClearPageStack;
  fStack.Free;

  inherited;
end;

function TRawBTreeIterator.Find(var key: string): boolean;
var
  pageNo, idx : Integer;
  pg : TPage;
  found : boolean;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page [pageNo];
    found := pg.FindNode(key, idx);
    fStack.Push(TIteratorNode.Create(pageNo, idx));
    if not found then
      if idx = 0 then
        pageNo := pg.PrevPage
      else
        pageNo := pg.Node [idx - 1].NextPage
  until found or (pageNo = -1);

  result := found;
  if found then
    key := pg.Node [idx].key
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.First                                     |
 |                                                                      |
 | Iterate to the first node in the index and return it's data          |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: string;           Returns the key of the first node       |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a first node.                 |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.First(var key: string): boolean;
var
  pageNo : Integer;
  pg : TPage;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      break;    // Empty tree containing empty root pg.

    fStack.Push(TIteratorNode.Create (pageNo, 0));
    pageNo := pg.PrevPage
  until pageNo = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node [0].key;
    result := True
  end
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | TRawBTreeIterator.Last
 |                                                                      |
 | Iterate to the last node in the index and return it's data           |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: string;           Returns the key of the last node        |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a last node.                  |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Last(var key: string): boolean;
var
  pageNo : Integer;
  pg : TPage;
begin
  ClearPageStack;
  pageNo := BTree.fFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      break;

    fStack.Push(TIteratorNode.Create (pageNo, pg.NodeCount - 1));
    pageNo := pg.Node [-1].NextPage
  until pg.PrevPage = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node [-1].key;
    result := True
  end
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.Next                                      |
 |                                                                      |
 | Iterate to the next node in the index and return it's data.          |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: string;           Returns the key of the next node        |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a next node.                  |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Next(var key: string): boolean;
var
  pg : TPage;
  node : TIteratorNode;
  tmp, tmp1 : Integer;

begin
  pg := Nil;
  node := Nil;
  if fStack.Count > 0 then
  begin
    node := TIteratorNode (fStack.Pop);         // Pop previously returned node
    pg := BTree.Page[node.PageNo];

                                                // Are there children?
    if pg.Node [node.fKeyIdx].NextPage <> -1 then
    begin
      fStack.Push(node);
                                                // Goto children...
      pg := BTree.Page[pg.Node[node.fKeyIdx].NextPage];
      node := TIteratorNode.Create(pg.fIdx, 0);

                                                // ... then Left, Left, Left!
      while pg.PrevPage <> -1 do
      begin
        fStack.Push(node);
        pg := BTree.Page [pg.PrevPage];
        node := TIteratorNode.Create(pg.fIdx, 0)
      end;

      node.fPageNo := pg.Idx;
    end                                         // Can we go right ?
    else
      if node.fKeyIdx < pg.NodeCount - 1 then
        Inc (node.fKeyIdx)
      else
      repeat                                    // No children - and can't go right
        tmp := node.PageNo;
        FreeAndNil (node);
        if fStack.Count > 0 then
        begin                                   // Pop a lower node.
          node := TIteratorNode (fStack.Pop);
          pg := BTree.Page[node.PageNo];

          if (node.fKeyIdx = 0) then
            tmp1 := pg.PrevPage
          else
            tmp1 := pg.Node [node.fKeyIdx - 1].NextPage;

          if tmp <> tmp1 then                   // If we we came from the node's children...
            Inc (node.fKeyIdx);                 // .. go right, or cycle round to pop another
                                                // pg.
          if node.fKeyIdx < pg.NodeCount then
            break
        end
        else break
      until False
  end;

  if Assigned (node) then                       // Did we find a next node?
  begin
    fStack.Push(node);                          // Push it, for next 'next' or 'prev'
    key := pg.Node [node.fKeyIdx].key;
    result := True
  end
  else
    result := False
end;

(*----------------------------------------------------------------------*
 | function TRawBTreeIterator.Next                                      |
 |                                                                      |
 | Iterate to the previous node in the index and return it's data.      |
 |                                                                      |
 | Parameters:                                                          |
 |   var key: string;           Returns the key of the previous node    |
 |   var value: Integer         Returns the DataRec associated with the |
 |                              key                                     |
 |                                                                      |
 | The function returns True if there was a previous node.              |
 *----------------------------------------------------------------------*)
function TRawBTreeIterator.Prev(var key: string): boolean;
begin
  result := False;              // Not yet implemented!
end;

{ TIteratorNode }

(*----------------------------------------------------------------------*
 | constructor TIteratorNode.Create                                     |
 |                                                                      |
 | Create an iterator (stack) node.                                     |
 *----------------------------------------------------------------------*)
constructor TIteratorNode.Create(APageNo, AKeyIdx: Integer);
begin
  fPageNo := APageNo;
  fKeyIdx := AKeyIdx;
end;

{ TBTree }

function TBTree.AddKey(const key: string; DataRec: Integer) : boolean;
begin
  result := inherited AddKey (key + IntToBin (DataRec))
end;

function TBTree.CompareKeys(const k1, k2: string): Integer;
begin
  if CaseSensitive then
    result := CompareStr (Copy (k1, 1, Length (k1) - SizeOf (Integer)), Copy (k2, 1, Length (k2) - SizeOf (Integer)))
  else
    result := CompareText (Copy (k1, 1, Length (k1) - SizeOf (Integer)), Copy (k2, 1, Length (k2) - SizeOf (Integer)))
end;

function TBTree.DeleteKey(const key: string) : boolean;
begin
  result := inherited DeleteKey (key + StringOfChar (#0, SizeOf (Integer)));
end;

function TBTree.ExtractDataRec(const key: string): Integer;
begin
  Move ((PChar (key) + Length (key) - 4)^, result, SizeOf (result))
end;

function TBTree.Find(key: string; var dataRec: Integer): boolean;
var
  k : string;
begin
  result := inherited Find (key + StringOfChar (#0, SizeOf (Integer)), k);
  if result then
    dataRec := ExtractDataRec (k)
end;

procedure TBTree.ForEach(proc: TBTreeForEachProc);
var
  continue : boolean;
  k : string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach (pageNo : Integer);
  var
    i : Integer;
    node : TNode;
    pg : TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach (pg.PrevPage);
      pg := Page [pageNo]               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node [i];
        k := Copy (node.key, 1, Length (node.key) - 4);

        Proc (k, ExtractDataRec (node.key), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach (node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach (fFileInfo.RootPage)
end;

function TBTree.GetDataRec(const key: string): Integer;
begin
  if not Find (key, result) then
    result := -1
end;

function TBTree.GetIndexOfKey(var key: string;
  var dataRec: Integer): Integer;
var
  k : string;
begin
  k := key + StringOfChar (#0, 4);
  result := inherited GetIndexOfKey (k);
  if result > -1 then
  begin
    dataRec := ExtractDataRec (k);
    key := Copy (k, 1, Length (k) - SizeOf (Integer))
  end
end;

function TBTree.GetKey(idx: Integer; var dataRec : Integer): string;
begin
  result := inherited GetKey (idx);
  if result <> '' then
  begin
    dataRec := ExtractDataRec (result);
    SetLength (result, Length (result) - sizeof (Integer))
  end
end;

function TBTree.InternalGetKey(idx: Integer): string;
var
  dr : Integer;
begin
  result := GetKey (idx, dr)
end;

function TBTree.IntToBin(i: Integer): string;
begin
  SetLength (result, SizeOf (Integer));
  Move (i, result [1], SizeOf (Integer));
end;

procedure TBTree.SetDataRec(const key: string; const Value: Integer);
var
  pg : TPage;
  idx : Integer;
  k : string;
begin
  pg := RootPage;
  k := key + StringOfChar (#0, SizeOf (Integer));

  BeginUpdate;
  try
    while Assigned (pg) do
    begin
      if pg.FindNode(k, idx) then
      begin
        pg.fNodes [idx].key := key + IntToBin (Value);
        pg.Flags [flgDirty] := True;
        break
      end;

      if pg.PrevPage <> -1 then
      begin
        if idx = 0 then
          pg := Page [pg.PrevPage]
        else
          pg := Page [pg.Node [idx - 1].NextPage]
      end
      else
        pg := Nil
    end
  finally
    EndUpdate
  end
end;

{ TBTreeIterator }

constructor TBTreeIterator.Create(ABTree: TBTree);
begin
  inherited Create (ABTree);
end;

function TBTreeIterator.Find(var key: string;
  var dataRec: Integer): boolean;
var
  k : string;
begin
  k := key + StringOfChar (#0, SizeOf (Integer));

  result := inherited Find (k);
  if result then
  begin
    key := k;
    SplitKey (key, dataRec)
  end
end;

function TBTreeIterator.First(var key: string; var dataRec: Integer): boolean;
begin
  result := inherited First (key);
  if result then
    SplitKey (key, dataRec)
end;

function TBTreeIterator.Last(var key: string;
  var dataRec: Integer): boolean;
begin
  result := inherited Last (key);
  if result then
    SplitKey (key, dataRec)
end;

function TBTreeIterator.Next(var key: string;
  var dataRec: Integer): boolean;
begin
  result := inherited Next (key);
  if result then
    SplitKey (key, dataRec)
end;

function TBTreeIterator.Prev(var key: string;
  var dataRec: Integer): boolean;
begin
  result := inherited Prev (key);
  if result then
    SplitKey (key, dataRec)
end;

procedure TBTreeIterator.SplitKey(var key : string; var dataRec: Integer);
begin
  dataRec := TBTree (BTree).ExtractDataRec(key);
  key := Copy (key, 1, Length (key) - SizeOf (Integer))
end;

{ TDataTree }

function TDataTree.AddKey(n: Integer; const st: string) : boolean;
begin
  result := inherited AddKey (IntToBin (n) + st)
end;

function TDataTree.BinToInt(const st: string): Integer;
begin
  move (st [1], result, SizeOf (Integer))
end;

function TDataTree.CompareKeys(const k1, k2: string): Integer;
begin
  result := BinToInt (k1) - BinToInt (k2)
end;

function TDataTree.DeleteKey(n: Integer) : boolean;
begin
  result := inherited DeleteKey (IntToBin (n))
end;

function TDataTree.Find(n: Integer; var st: string): boolean;
var
  k : string;
begin
  result := inherited Find (IntToBin (n), k);
  if result then
    st := Copy (k, 5, MaxInt)
end;

procedure TDataTree.ForEach(proc: TDataTreeForEachProc);
var
  continue : boolean;
  k : string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach (pageNo : Integer);
  var
    i : Integer;
    node : TNode;
    pg : TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach (pg.PrevPage);
      pg := Page [pageNo]               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node [i];
        k := node.key;
        Proc (BinToInt (k), Copy (k, SizeOf (Integer) + 1, MaxInt), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach (node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach (fFileInfo.RootPage)
end;

function TDataTree.GetKey(idx: Integer; var DataRec : Integer): string;
begin
  result := inherited GetKey (idx);
  DataRec := BinToInt (result);
  result := Copy (result, SizeOf (Integer) + 1, MaxInt);
end;

function TDataTree.IntToBin(i: Integer): string;
begin
  SetLength (result, SizeOf (Integer));
  Move (i, result [1], SizeOf (Integer));
end;

{ TDataTreeIterator }

constructor TDataTreeIterator.Create(ADataTree: TDataTree);
begin
  inherited Create (ADataTree)
end;

function TDataTreeIterator.Find(n: Integer; var st: string): boolean;
var
  k : string;
begin
  SetLength (k, Sizeof (Integer));
  Move (n, k [1], SizeOf (Integer));
  result := inherited Find (k);
  if result then
  begin
    st := k;
    SplitKey (n, st)
  end
end;

function TDataTreeIterator.First(var n : Integer; var st : string) : boolean;
begin
  result := inherited First (st);
  if result then
    SplitKey (n, st)
end;

function TDataTreeIterator.Last (var n : Integer; var st : string) : boolean;
begin
  result := inherited Last (st);
  if result then
    SplitKey (n, st)
end;

function TDataTreeIterator.Next (var n : Integer; var st : string) : boolean;
begin
  result := inherited Next (st);
  if result then
    SplitKey (n, st)
end;

function TDataTreeIterator.Prev (var n : Integer; var st : string) : boolean;
begin
  result := inherited Prev (st);
  if result then
    SplitKey (n, st)
end;

procedure TDataTreeIterator.SplitKey(var n : Integer; var key : string);
begin
  Move (key [1], n, SizeOf (Integer));
  key := Copy (key, SizeOf (Integer) + 1, MaxInt)
end;

{ TIndexTree }

function TIndexTree.AddKey(i : Integer) : boolean;
begin
  IntToBinBuffer (i);
  result := inherited AddKey (fBinBuffer)
end;

function TIndexTree.BinToInt(const st: string): Integer;
begin
  move (st [1], result, SizeOf (Integer))
end;

function TIndexTree.CompareKeys(const k1, k2: string): Integer;
begin
  result := BinToInt (k1) - BinToInt (k2)
end;

constructor TIndexTree.Create (const AFileName : string);
begin
  inherited Create (AFileName);
  fBinBuffer := #0#0#0#0
end;

function TIndexTree.Delete(n: Integer) : boolean;
var
  i : Integer;
begin
  i := BinToInt (GetKey (n));
  result := DeleteKey (i)
end;

function TIndexTree.DeleteKey(i: Integer) : boolean;
begin
  IntToBinBuffer (i);
  result := inherited DeleteKey (fBinBuffer)
end;

function TIndexTree.Find(i: Integer): boolean;
var
  fKey : string;
begin
  IntToBinBuffer (i);
  result := inherited Find (fBinBuffer, fKey)
end;

procedure TIndexTree.ForEach(proc: TIndexTreeForEachProc);
var
  continue : boolean;
  k : string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach (pageNo : Integer);
  var
    i : Integer;
    node : TNode;
    pg : TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach (pg.PrevPage);
      pg := Page [pageNo]               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node [i];
        k := node.key;
        Proc (BinToInt (k), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach (node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach (fFileInfo.RootPage)
end;

function TIndexTree.GetIndexOf(i: Integer): Integer;
begin
  IntToBinBuffer (i);
  result := GetIndexOfKey (fBinBuffer)
end;

function TIndexTree.GetValue(n: Integer): Integer;
begin
  result := BinToInt (GetKey (n))
end;

procedure TIndexTree.IntToBinBuffer(i: Integer);
begin
  Move (i, fBinBuffer [1], SizeOf (Integer));
end;

end.
