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

uses
  Windows, Classes, SysUtils, Contnrs, unitObjectCache;

type
  //----------------------------------------------------------------
  // Header for B-Tree page(in index file)
  TPageHeader = packed record
    Flags: Integer;
    KeysOnPage: Integer;
    PrevPage: Integer;          // -1 = leaf
    PrevPageHeight: Integer;
  end;

  //----------------------------------------------------------------
  // Header for B-Tree node(in index file)
  TNodeHeader = packed record
    NextPage: Integer;
    NextPageHeight: Integer;
    KeyLen: Word;
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
  PAGE_DATA_SIZE = PAGE_SIZE - SizeOf(TPageHeader);

  MAX_KEY_LEN = (PAGE_DATA_SIZE div 2) - SizeOf(TNodeHeader);

  NO_CACHED_PAGES = 64;

type
  //----------------------------------------------------------------
  // Index file header
  TFileInfo = packed record
    id: array [0..7] of char;
    Flags: Integer;
    PageCount: Integer;
    RootPage: Integer;
    RecordCount: Integer;
    FirstDeletedPage: Integer;
                                  // Let the user use the otherwise unused
                                  // space in the file header.
    ExtraDataSize: word;
    ExtraData: array [0..PAGE_SIZE - 5 * SizeOf(Integer) - 8 - 1 - SizeOf(word)] of byte;
  end;

  //----------------------------------------------------------------
  // B-Tree page(in index file)
  TPageRec = packed record
    Header: TPageHeader;
    Data: Array [0..PAGE_DATA_SIZE - 1] of byte ;// The node data
  end;

  //----------------------------------------------------------------
  // B-Tree node(in memory)
  TNode = record
    key: string;
    NextPage: Integer;
    NextPageHeight: Integer;
  end;

  TRawBTree = class;

  //----------------------------------------------------------------
  // B-Tree page(in memory)
  TPage = class
  private
    FFlags: Integer;
    FPrevPage: Integer;
    FPrevPageHeight: Integer;
    FNodes: array of TNode;
    FNodeCount: Integer;

    FOwner: TRawBTree;
    FIdx: Integer;
    FTotalDataLen: Integer;
    function GetFlags(bits: Integer): Boolean;
    procedure SetFlags(bits: Integer; const Value: Boolean);

    function FindNode(const st: string; var idx: Integer): Boolean;
    procedure InsertNode(idx: Integer; const node: TNode);
    function GetNode(idx  :Integer): TNode;
    function GetHeight: Integer;
  public
    constructor Create(AOwner: TRawBTree; AIdx: Integer);
    destructor Destroy; override;

    procedure Load;
    procedure Save;

    property Flags[bits: Integer]: Boolean read GetFlags write SetFlags;
    property Height: Integer read GetHeight;
    property Idx: Integer read FIdx;
    property NodeCount: Integer read FNodeCount;
    property Node [idx: Integer]: TNode read GetNode;
    property Owner: TRawBTree read FOwner;
    property PrevPage: Integer read FPrevPage;
    property PrevPageHeight: Integer read FPrevPageHeight;
  end;

  //----------------------------------------------------------------
  // Page cache stores TPage objects.  CanRemove is overridden to save
  // a page on disk befor it's removed
  TPageCache = class (TObjectCache)
  protected
    function CanRemove(AObject: TObject): Boolean; override;
  end;

  TRawBTreeForEachProc = procedure(const key: string; param: Integer; var continue: Boolean) of object;

  TBTreeDuplicates = (dupIgnore, dupAccept, dupError, dupReplace);

  //----------------------------------------------------------------
  // TRawBTree object
  TRawBTree = class
  private
    FFileName: string;
    FFileStream: TFileStream;
    FFileInfo: TFileInfo;
    FPageCache: TPageCache;

    FUpdateCount: Integer;

    FDelIDx, FDelPIdx: Integer;  // Used during delete.
    FOK: Boolean;

    procedure Open;
    procedure Close;
    procedure Flush(clearPageCache: Boolean);
    procedure SaveFileInfo;

    function GetPage(pageNo: Integer): TPage;
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure CacheCheckProc (obj: TObject; idx, param: Integer; var continue: Boolean);
    procedure CacheSaveProc (obj: TObject; idx, param: Integer; var continue: Boolean);
    function CreateNewPage: TPage;
    procedure DeleteOldPage(page: TPage);

    function PutKeyOnPage(pg: TPage; idx: Integer; const memNode: TNode): TNode;
    function PutKeyInTree(pg: TPage; const key: string): TNode;
    function DeleteKeyFromTree(pg: TPage; const key: string): Integer;
    function DeleteKeyFromPage(page: TPage; idx: Integer): Integer;
    function GetRecordCount: Integer;
    function GetRootPage: TPage;
    procedure SetDuplicates(const Value: TBTreeDuplicates);
    function GetDuplicates: TBTreeDuplicates;
    function GetExtraData: string;
    procedure ResetNodeHeight(pg: TPage; idx: Integer);
    procedure SetExtraData(Value: string);
    function GetIndexOfKey(var key: string): Integer;
  protected
    function GetKey(idx: Integer): string;
    function CompareKeys (const k1, k2: string): Integer; virtual;

    function AddKey(const key: string): Boolean;
    function DeleteKey(const key: string): Boolean;
    procedure ForEach(proc: TRawBTreeForEachProc; param: Integer);
    function Find (key: string; var fKey: string): Boolean;

    property Key [idx: Integer]: string read GetKey;
  public
    constructor Create(const AFileName: string); virtual;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;


    property ExtraData: string read GetExtraData write SetExtraData;

    property RecordCount: Integer read GetRecordCount;

  // ------ May not always be public, but handy for diagnostics
    property RootPage: TPage read GetRootPage;
    property Page [pageNo: Integer]: TPage read GetPage;


    property FileName: string read FFileName;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property Duplicates: TBTreeDuplicates read GetDuplicates write SetDuplicates;
  end;

  //----------------------------------------------------------------
  // TIteratorNode.  An element on the iterator's stack
  TIteratorNode = class
  private
    FPageNo: Integer;
    FKeyIdx: Integer;
  public
    constructor Create(APageNo: Integer; AKeyIdx: Integer);

    property PageNo: Integer read FPageNo;
    property KeyIdx: Integer read FKeyIdx;
  end;

  //----------------------------------------------------------------
  // TRawBTreeIterator.  Class for iterating through the BTree
  TRawBTreeIterator = class
  private
    FBTree: TRawBTree;
    FStack: TObjectStack;
    procedure ClearPageStack;
  public
    constructor Create(ABTree: TRawBTree);
    destructor Destroy; override;

    function First(var key: string): Boolean;
    function Last(var key: string): Boolean;
    function Next(var key: string): Boolean;
    function Prev (var key: string): Boolean;
    function Find (var key: string): Boolean;

    property BTree: TRawBTree read FBTree;
  end;

  TBTreeForEachProc = procedure(const key: string; dataRec: Integer; var continue: Boolean) of object;
  TBTree = class (TRawBTree)
  private
    function IntToBin (i: Integer): string;
    function ExtractDataRec (const key: string): Integer;
    function InternalGetKey(idx: Integer): string;
    function GetDataRec(const key: string): Integer;
    procedure SetDataRec(const key: string; const Value: Integer);
  protected
    function CompareKeys (const k1, k2: string): Integer; override;
  public
    function AddKey(const key: string; DataRec: Integer): Boolean;
    function DeleteKey(const key: string): Boolean;
    procedure ForEach(proc: TBTreeForEachProc);
    function Find (key: string; var dataRec: Integer): Boolean;

    function GetKey(idx: Integer; var dataRec: Integer): string;
    function GetIndexOfKey(var key: string; var dataRec: Integer): Integer;

    property Key [idx: Integer]: string read InternalGetKey;
    property DataRec [const key: string]: Integer read GetDataRec write SetDataRec;
  end;

  TDataTreeForEachProc = procedure(n: Integer; const st: string; var continue: Boolean) of object;
  TDataTree = class (TRawBTree)
  private
    function IntToBin (i: Integer): string;
    function BinToInt(const st: string): Integer;
  protected
    function CompareKeys (const k1, k2: string): Integer; override;
  public
    function AddKey(n: Integer; const st: string): Boolean;
    function DeleteKey(n: Integer): Boolean;
    procedure ForEach(proc: TDataTreeForEachProc);
    function Find (n: Integer; var st: string): Boolean;
    function GetKey(idx: Integer; var dataRec: Integer): string;
  end;

  TBTreeIterator = class (TRawBTreeIterator)
  private
    procedure SplitKey(var key: string; var dataRec: Integer);
  public
    constructor Create(ABTree: TBTree);
    function First(var key: string; var dataRec: Integer): Boolean;
    function Last(var key: string; var dataRec: Integer): Boolean;
    function Next(var key: string; var dataRec: Integer): Boolean;
    function Prev (var key: string; var dataRec: Integer): Boolean;
    function Find (var key: string; var dataRec: Integer): Boolean;
  end;

  TDataTreeIterator = class (TRawBTreeIterator)
  private
    procedure SplitKey(var n: Integer; var key: string);
  public
    constructor Create(ADataTree: TDataTree);
    function First(var n: Integer; var st: string): Boolean;
    function Last(var n: Integer; var st: string): Boolean;
    function Next(var n: Integer; var st: string): Boolean;
    function Prev (var n: Integer; var st: string): Boolean;
    function Find (n: Integer; var st: string): Boolean;
  end;

  TIndexTreeForEachProc = procedure(i: Integer; var continue: Boolean) of object;
  TIndexTree = class (TRawBTree)
  private
    FBinBuffer: string;
    procedure IntToBinBuffer (i: Integer);
    function BinToInt(const st: string): Integer;
    function GetValue(n: Integer): Integer;
    function GetIndexOf(i: Integer): Integer;
  protected
    function CompareKeys (const k1, k2: string): Integer; override;
  public
    constructor Create(const AFileName: string); override;
    function AddKey(i: Integer): Boolean;
    function DeleteKey(i: Integer): Boolean;
    procedure ForEach(proc: TIndexTreeForEachProc);
    function Find (i: Integer): Boolean;

    function Delete(n: Integer): Boolean;
    property Value [n: Integer]: Integer read GetValue; default;
    property IndexOf [i: Integer]: Integer read GetIndexOf;
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
function TRawBTree.AddKey(const key: string): Boolean;
var
  passout: TNode;
  newPage0: TPage;
begin
  if Length(key) > MAX_KEY_LEN then
    raise EBTree.Create(rstKeyTooLong);

  BeginUpdate;
  try
    FOK := True;
    passout := PutKeyInTree(RootPage, key);

    if passout.NextPage <> -1 then
    begin
      newPage0 := CreateNewPage;
      newPage0.FPrevPage := FFileInfo.RootPage;
      FFileInfo.RootPage := newPage0.FIdx;
      PutKeyOnPage(newPage0, 0, passout);

      ResetNodeHeight(newPage0, -1);
      ResetNodeHeight(newPage0, 0);
    end;
    if FOK then
      Inc(FFileInfo.RecordCount)
  finally
    EndUpdate
  end;
  Result := FOK
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.BeginUpdate                                      |
 |                                                                      |
 | Start updating.  Must be matched with EndUpdate                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheCheckProc                                   |
 |                                                                      |
 | Callback for cache ForEach function - to find whether a page is      |
 | already in the cache.                                                |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheCheckProc(obj: TObject; idx, param: Integer;
  var continue: Boolean);
begin
  continue := TPage(obj).FIdx <> param
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.CacheSaveProc                                    |
 |                                                                      |
 | Callback for cache ForEach function - called to save each(dirty)    |
 | page when flushing the cache                                         |
 *----------------------------------------------------------------------*)
procedure TRawBTree.CacheSaveProc(obj: TObject; idx, param: Integer;
  var continue: Boolean);
begin
  TPage(obj).Save;
  continue := True
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Close                                            |
 |                                                                      |
 | Close the BTree                                                      |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Close;
begin
  Flush(True);         // Flush and clear cached pages
  FreeAndNil (FFileStream)        // Close the index file
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
    Result := CompareText(k1, k2)
end;

constructor TRawBTree.Create(const AFileName: string);
begin
  FFileName := AFileName;
  FPageCache := TPageCache.Create(NO_CACHED_PAGES, True);
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
  Result := TPage.Create(Self, FFileInfo.PageCount);
  if FFileInfo.FirstDeletedPage <> -1 then
  begin
    Result.FIdx := FFileInfo.FirstDeletedPage;
    Result.Load;
    FFileInfo.FirstDeletedPage := Result.PrevPage;
    Result.FPrevPage := -1
  end;
  FPageCache.Add(Result);
  Result.Flags[flgDirty] := True;
  Inc(FFileInfo.PageCount);
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTree.Destroy                                         |
 |                                                                      |
 | Destructor for TRawBTree.                                            |
 *----------------------------------------------------------------------*)
function TRawBTree.DeleteKey(const key: string): Boolean;
var
  page0: TPage;
begin
  FDelPIdx := -1;
  BeginUpdate;
  try
    FOK := True;
    DeleteKeyFromTree(RootPage, key);
    if FOK then
    begin
      page0 := RootPage;
      if page0.NodeCount = 0 then
      begin
        if page0.PrevPage <> -1 then
        begin
          FFileInfo.RootPage := page0.PrevPage;
          ResetNodeHeight(RootPage, -1);
          DeleteOldPage(page0)
        end
      end;

      if FOK then
        Dec(FFileInfo.RecordCount)
    end
  finally
    EndUpdate
  end;

  Result := FOK
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.DeleteKeyFromPage                                 |
 |                                                                      |
 | Delete a key from a page.                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   page: TPage;               The page to delete from                 |
 |   idx: Integer              The node index on the page              |
 |                                                                      |
 | The function returns the number of nodes remaining on the page       |
 *----------------------------------------------------------------------*)
function TRawBTree.DeleteKeyFromPage(page: TPage; idx: Integer): Integer;
var
  dl: Integer;
begin
  dl := SizeOf(TNodeHeader) + Length(page.FNodes[idx].key);
  finalize(page.FNodes[idx]);
  FillChar (page.FNodes[idx], SizeOf(TNode), 0);

  if idx < page.FNodeCount - 1 then
  begin
    Move(page.FNodes[idx + 1], page.FNodes[idx], (page.FNodeCount - idx - 1) * SizeOf(TNode));
    FillChar (page.FNodes[page.FNodeCount - 1], SizeOf(TNode), 0)
  end;

  Dec(page.FTotalDataLen, dl);
  Dec(page.FNodeCount);
  page.Flags[flgDirty] := True;
  Result := page.FNodeCount
end;

function TRawBTree.DeleteKeyFromTree(pg: TPage; const key: string): Integer;
var
  idx, pidx, nidx, tidx, mp: Integer;
  tn: TNode;
  tp, br: TPage;
  found: Boolean;

begin
  pidx := pg.Idx;
  found := pg.FindNode(key, idx);
  if found then
  begin
    FDelPIdx := pg.Idx;
    FDelIDx := idx;
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

    if DeleteKeyFromTree(tp, key) = 0 then

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
      // pg.FNodes[idx] is the node on this pg to rotate around
      // mp is the insertion pos on br.
      // if mp=0 then we're rotating left to the PrevPage - otherwise we're
      // rotating right to pg.FNodes[idx]'s Next pg

      if br.NodeCount = 1 then  // merge with neighbour
      begin
        if mp = 0 then          // PageLeft is empty.  Move current key into it's
        begin                   // right branch
          pg.FNodes[idx].NextPage := br.FPrevPage;
          br.FPrevPage := tp.PrevPage;
          pg.FPrevPage := br.Idx;
        end
        else                    // Current key's pg right is empty.  Move current
                               // key to it's left sibling's children
          pg.FNodes[idx].NextPage := tp.PrevPage;

        PutKeyOnPage(br, mp, pg.Node [idx]);
        ResetNodeHeight(br, mp);
        ResetNodeHeight(br, -1);
        DeleteKeyFromPage(pg, idx);
        DeleteOldPage(tp);
      end
      else                      // Borrow from neighbour
      begin
        PutKeyOnPage(tp, 0, pg.Node [idx]);
        if mp = 0 then
        begin
          tp.FNodes[0].NextPage := br.PrevPage;
          br.FPrevPage := br.Node [mp].NextPage;
          br.FPrevPageHeight := br.Node [mp].NextPageHeight
        end
        else
        begin
          Dec(mp);
          tp.FNodes[0].NextPage := tp.PrevPage;
          tp.FPrevPage := br.Node [mp].NextPage;
        end;
        pg.FNodes[idx].key := br.Node [mp].key;
        DeleteKeyFromPage(br, mp);

        ResetNodeHeight(tp, -1);
        ResetNodeHeight(tp, 0)
      end;
      pg.Flags[flgDirty] := True;
      br.Flags[flgDirty] := True;

      if idx < pg.NodeCount then
        ResetNodeHeight(pg, idx);
      if idx > 0 then
        ResetNodeHeight(pg, idx - 1);
      ResetNodeHeight(pg, -1);
    end
    else
    begin
      pg := Page [pidx];
      ResetNodeHeight(pg, nidx - 1);
    end
  end
  else
  begin
    if found then
    begin
      if FDelPIdx <> -1 then
      begin
        if FDelPIdx <> pidx then
        begin       // Move node to delete to the leaves
          tp := Page [FDelPIdx];
          tn := pg.Node [idx];
          pg.FNodes[idx].key := tp.Node [FDelIDx].key;
          tp.FNodes[FDelIDx].key := tn.Key;
          tp.Flags[flgDirty] := True;
        end;

        DeleteKeyFromPage(pg, idx)
      end
    end
    else
      FOK := False
  end;

  Result := pg.NodeCount
end;

procedure TRawBTree.DeleteOldPage(page: TPage);
begin
  page.FPrevPage := FFileInfo.FirstDeletedPage;
  FFileInfo.FirstDeletedPage := page.FIdx;
  page.Flags[flgDirty] := True;
  page.FNodeCount := 0;
  SetLength(page.FNodes, 0);
  FPageCache.Remove(page);
end;

destructor TRawBTree.Destroy;
begin
  Close;
  FPageCache.Free
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.EndUpdate                                        |
 |                                                                      |
 | Pair to BeginUpdate.  If updated have finished, flush the cached     |
 | pages.                                                               |
 *----------------------------------------------------------------------*)
procedure TRawBTree.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      Flush(False)
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
function TRawBTree.Find(key: string; var fKey: string): Boolean;
var
  pg: TPage;
  idx: Integer;
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

  Result := pg <> Nil
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Flush                                            |
 |                                                                      |
 | Flush the cached pages, and optionally clear the cache.              |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Flush(clearPageCache: Boolean);
begin
  if FUpdateCount > 0 then Exit;

  if clearPageCache then
    FPageCache.Clear
  else
    FPageCache.ForEach(CacheSaveProc, 0);

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
procedure TRawBTree.ForEach(proc: TRawBTreeForEachProc; param: Integer);
var
  continue: Boolean;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
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
          DoForEach(node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach(FFileInfo.RootPage)
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetCaseSensitive                                  |
 |                                                                      |
 | 'Get' method for CaseSensitive property                              |
 *----------------------------------------------------------------------*)
function TRawBTree.GetCaseSensitive: Boolean;
begin
  Result := (FFileInfo.Flags and fflgCaseSensitive) <> 0;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetDuplicates                                     |
 |                                                                      |
 | 'Get' method for Duplicates property.                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetDuplicates: TBTreeDuplicates;
begin
  if (FFileInfo.Flags and fflgDupAccept) <> 0 then
    Result := dupAccept
  else
    if (FFileInfo.Flags and fflgDupError) <> 0 then
      Result := dupError
    else
      if (FFileInfo.Flags and fflgDupReplace) <> 0 then
        Result := dupReplace
      else
        Result := dupIgnore
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetExtraData                                      |
 |                                                                      |
 | 'Get' method for ExtraData property                                  |
 *----------------------------------------------------------------------*)
function TRawBTree.GetExtraData: string;
begin
  SetLength(Result, FFileInfo.ExtraDataSize);
  Move(FFileInfo.ExtraData [0], Result [1], FFileInfo.ExtraDataSize)
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetPage                                           |
 |                                                                      |
 | 'Get' method for Page property.  Return Page [pageno]                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetIndexOfKey(var key: string): Integer;
var
  pg: TPage;
  i, idx: Integer;
begin
  pg := RootPage;
  Result := 0;
  while pg <> Nil do
  begin
    if pg.FindNode(key, idx) then
    begin                               // Found it!
      key := pg.Node [idx].key;
      Inc(Result, pg.PrevPageHeight);
      Inc(Result, idx);
      for i := 0 to idx - 2 do
        Inc(Result, pg.Node [i].NextPageHeight);
      break;
    end;

    if idx = 0 then
      idx := pg.PrevPage                      // Search pg left
    else
    begin
      Inc(Result, pg.PrevPageHeight);

      Inc(Result, idx);
      for i := 0 to idx - 2 do
        Inc(Result, pg.Node [i].NextPageHeight);
      idx := pg.node [idx-1].NextPage;        // Search node right
    end;

    if idx > -1 then
      pg := Page [idx]
    else
      pg := Nil
  end;

  if pg = Nil then
    Result := -1
end;

function TRawBTree.GetKey(idx: Integer): string;

  function gk(root: TPage; idx: Integer): string;
  var
    i: Integer;
  begin
    if root.PrevPage = -1 then
      Result := root.Node [idx].key
    else
    begin
      if idx < root.PrevPageHeight then
        Result := gk(Page [root.PrevPage], idx)
      else
      begin
        Dec(idx, root.PrevPageHeight);

        i := 0;
        while(i < root.NodeCount) and (idx > 0) do
        begin
          if idx <= root.Node [i].NextPageHeight then
          begin
            Result := gk(Page [root.Node [i].NextPage], idx - 1);
            break
          end;

          Dec(idx, 1 + root.Node [i].NextPageHeight);
          Inc(i)
        end;

        if idx = 0 then
          Result := root.Node [i].key
      end
    end
  end;
begin
  if idx >= RecordCount then
    raise EBTree.Create(rstIndexExceedsBounds);
  Result := gk(RootPage, idx)
end;

function TRawBTree.GetPage(pageNo: Integer): TPage;
var
  idx: Integer;
begin
  idx := FPageCache.ForEachIdx(CacheCheckProc, pageNo);

  if idx >= 0 then
  begin
    Result := TPage(FPageCache.ObjectAt(idx));
    FPageCache.BringToFrontObject(idx)
  end
  else
  begin
    Result := TPage.Create(Self, pageNo);
    Result.Load;
    FPageCache.Add(Result);
  end
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRecordCount                                    |
 |                                                                      |
 | 'Get' method for RecordCount property                                |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRecordCount: Integer;
begin
  Result := FFileInfo.RecordCount;
end;

(*----------------------------------------------------------------------*
 | function TRawBTree.GetRootPage                                       |
 |                                                                      |
 | 'Get' method for RootPage property                                   |
 *----------------------------------------------------------------------*)
function TRawBTree.GetRootPage: TPage;
begin
  Result := Page [FFileInfo.RootPage];
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.Open                                             |
 |                                                                      |
 | Open the BTree                                                       |
 *----------------------------------------------------------------------*)
procedure TRawBTree.Open;
var
  page0: TPage;
  id: string;
begin
  if not FileExists (FileName) then
  begin                                 // New file
    FFileStream := TFileStream.Create(FileName, fmCreate);
    try
                                        // Create file info page and empty root page
      id := 'BTWoozle';
      Move(id [1], FFileInfo.id [0], 8);
      page0 := TPage.Create(Self, 0);
      try
        page0.Flags[flgDirty] := True;
        page0.Save;
      finally
        page0.Free
      end;
      FFileInfo.PageCount := 1;
      FFileInfo.FirstDeletedPage := -1;
      FFileInfo.Flags := fflgCaseSensitive;
      SaveFileInfo;
    finally
      FreeAndNil (FFileStream)                    // Close newly created file
    end
  end;

                                        // Open existing file
  FFileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite);
                                        // Read file info page.
  FFileStream.Read(FFileInfo, SizeOf(FFileInfo));
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
function TRawBTree.PutKeyInTree(pg: TPage; const key: string): TNode;
var
  pidx, idx: Integer;
begin
  pidx := pg.Idx;
  if pg.FindNode(key, idx) then
    case Duplicates of
      dupIgnore, dupReplace :
        begin
          FOK := False;

          if (Duplicates = dupReplace) and (key <> pg.FNodes[idx].key) then
          begin
            pg.FNodes[idx].key := key;
            pg.Flags[flgDirty] := True
          end;
          Result.NextPage := -1;
          Exit;
        end;
      dupAccept :;
      dupError:
        raise EBTree.Create(Format(rstDuplicateKey, [key]));
    end;

  if pg.PrevPage = -1 then            // We're at the leaves.
  begin
    Result.key := key;
    Result.NextPage := -1;
    Result.NextPageHeight := 0;
    Result := PutKeyOnPage(pg, idx, Result)
  end
  else
  begin                                 // Not at leaves - put key in pg left
    if idx > 0 then                     // or node right
    begin
      Result := PutKeyInTree(Page [pg.Node [idx - 1].NextPage], key);
      ResetNodeHeight(pg, idx - 1);
    end
    else
    begin
      Result := PutKeyInTree(Page [pg.PrevPage], key);
      ResetNodeHeight(pg, -1)
    end;

                                        // If a node was passed out by inserting to
                                        // the child, put it on this pg.
    if Result.NextPage <> -1 then
    begin
      pg := Page [pidx];                // Just possible that the original pg is
                                        // no longer in the cache.  Make sure it is.
      Result := PutKeyOnPage(pg, idx, Result);
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
 |   idx: Integer;             The position on the page to put the     |
 |                              node.                                   |
 |   const memNode: TNode      The node to put.                        |
 |                                                                      |
 | The function returns a TNode.  If retval.NextPage is -1 then         |
 | retval is meaningless - otherwise it contains a node pointing to a   |
 | page of higher nodes to be inserted in a lower page.                 |
 *----------------------------------------------------------------------*)
function TRawBTree.PutKeyOnPage(pg: TPage;  idx: Integer; const memNode: TNode): TNode;
var
  i, iidx, ts: Integer;
  newPage: TPage;
begin
  if pg.FTotalDataLen + SizeOf(TNodeHeader) + Length(memNode.key) > PAGE_DATA_SIZE then

  begin                 // Key doesn't fit on the pg.
                        // Split the pg into pg & newPage, and
                        // passout the midpoint key, pointing to the new pg.

    ts := 0;
    iidx := 0;          // Find mid point - iidx
    while(ts <= PAGE_DATA_SIZE div 2) and (iidx < pg.NodeCount) do
    begin
      Inc(ts, Length(pg.Node [iidx].key) + SizeOf(TNodeHeader));
      Inc(iidx)
    end;

    Dec(iidx);

                        // Passout mid point key
    if idx = iidx then
    begin
      Result := memNode;
      ts := iidx
    end
    else
    begin
      if idx > iidx then
        ts := iidx + 1
      else
      begin
        ts := iidx;
        Dec(iidx)
      end;
      Result := pg.Node [iidx];
    end;

    newPage := CreateNewPage;
    newPage.FPrevPage := Result.NextPage;
    Result.NextPage := newPage.FIdx;

                        // Move keys above midpoint to new pg.

    newPage.FNodeCount := pg.NodeCount - ts;
    if Length(newPage.FNodes) < newPage.FNodeCount then
      SetLength(newPage.FNodes, newPage.FNodeCount);

    for i := ts to pg.NodeCount - 1 do
    begin
      newPage.FNodes[i - ts] := pg.Node [i];
//      newPage.InsertNode(i - ts, pg.Node [i]);
      Inc(newPage.FTotalDataLen, SizeOf(TNodeHeader) + Length(pg.Node [i].key));
    end;

                        // Truncate current pg

    pg.FNodeCount := iidx;

    pg.Flags[flgDirty] := True;
                        // Recalc TotalDataLen for truncated pg
    pg.FTotalDataLen := 0;
    for i := 0 to iidx - 1 do
      Inc(pg.FTotalDataLen, SizeOf(TNodeHeader) + Length(pg.Node [i].key));

    if ts <> iidx then
      if idx <= iidx then // Put the new key on the old or new pg.
        PutKeyOnPage(pg, idx, memNode)
      else
        PutKeyOnPage(newPage, idx - pg.NodeCount - 1, memNode);

    ResetNodeHeight(newPage, -1);
    Result.NextPageHeight := newPage.Height;
  end
  else                  // pg not full - just add the node
  begin
    pg.InsertNode(idx, memNode);
    Inc(pg.FTotalDataLen, SizeOf(TNodeHeader) + Length(memNode.key));
    pg.Flags[flgDirty] := True;
    Result.NextPage := -1;  // Don't pass out anything.
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SaveFileInfo                                     |
 |                                                                      |
 | Save the FileInfo record at the start of the index file.             |
 *----------------------------------------------------------------------*)
procedure TRawBTree.ResetNodeHeight(pg: TPage; idx: Integer);
var
  node: TNode;
  pint: PInteger;
  val: Integer;
begin
  if idx = -1 then
  begin
    PInt := @pg.FPrevPageHeight;
    if pg.PrevPage = -1 then
      val := 0
    else
      val := Page [pg.PrevPage].Height
  end
  else
  begin
    if idx >= pg.NodeCount then
      exit;
    PInt := @pg.FNodes[idx].NextPageHeight;
    node := pg.Node [idx];
    if node.NextPage = -1 then
      val := 0
    else
      val := Page [node.NextPage].Height
  end;

  if PInt^ <> val then
  begin
    PInt^ := val;
    pg.Flags[flgDirty] := True
  end
end;

procedure TRawBTree.SaveFileInfo;
begin
  if FUpdateCount > 0 then Exit;
  if Assigned(FFileStream) then
  begin
    FFileStream.Seek(0, soFromBeginning);
    FFileStream.Write(FFileInfo, SizeOf(FFileInfo))
  end
end;

(*----------------------------------------------------------------------*
 | procedure TRawBTree.SetCaseSensitive                                 |
 |                                                                      |
 | 'Set' method for CaseSensitive property.  The btree must be empty    |
 | to call this.                                                        |
 *----------------------------------------------------------------------*)
procedure TRawBTree.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> CaseSensitive then
  begin
    if Assigned(FFileStream) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create(rstMustBeEmpty);

    if Value then
      FFileInfo.Flags := FFileInfo.Flags or fflgCaseSensitive
    else
      FFileInfo.Flags := FFileInfo.Flags and (not fflgCaseSensitive);

    if Assigned(FFileStream) then
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
    if Assigned(FFileStream) then
      if RootPage.NodeCount > 0 then
        raise EBTree.Create(rstMustBeEmpty);

    FFileInfo.Flags := FFileInfo.Flags and not fflgDupFlags;

    case Value of
      dupAccept: FFileInfo.Flags := FFileInfo.Flags or fflgDupAccept;
      dupError: FFileInfo.Flags := FFileInfo.Flags or fflgDupError;
      dupReplace: FFileInfo.Flags := FFileInfo.Flags or fflgDupReplace;
    end;

    if Assigned(FFileStream) then
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
  if Length(value) > SizeOf(FFileInfo.ExtraData) then
    SetLength(value, SizeOf(FFileInfo.ExtraData));

  FFileInfo.ExtraDataSize := Length(Value);
  if Length(Value) > 0 then
    Move(Value [1], FFileInfo.ExtraData [0], Length(value));

  SaveFileInfo
end;

{ TPage }

(*----------------------------------------------------------------------*
 | constructor TPage.Create                                             |
 |                                                                      |
 | Constructor for TPage                                                |
 *----------------------------------------------------------------------*)
constructor TPage.Create(AOwner: TRawBTree; AIdx: Integer);
begin
  FOwner := AOwner;
  FPrevPage := -1;
  FIdx := AIdx;
  SetLength(FNodes, 384);
end;

(*----------------------------------------------------------------------*
 | destructor TPage.Destroy                                             |
 |                                                                      |
 | Destructor for TPage                                                 |
 *----------------------------------------------------------------------*)
destructor TPage.Destroy;
begin
  Save;

  finalize(FNodes);            // Free memory used by the FNodes dynamic array
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
function TPage.FindNode(const st: string; var idx: Integer): Boolean;

//------------------------------------------------------
// Binary search the page
  function bsearch(s, e: Integer): Boolean;
  var
    cmp: Integer;
  begin
    if e >= s then
    begin
      idx := s + (e - s) div 2;

      cmp := Owner.CompareKeys (st, Node [idx].key);

      if cmp < 0 then
        Result := bsearch(s, idx - 1)
      else
        if cmp > 0 then
          Result := bsearch(idx + 1, e)
        else
          Result := True
    end
    else
      Result := False;
  end;

begin
  idx := 0;
  Result := bsearch(0, NodeCount - 1);

  if (not Result) and (idx < NodeCount) then
  // Adjust 'idx' so that it contains the correct insertion point
    if Owner.CompareKeys (Node [idx].key, st) < 0 then
      Inc(idx)
end;

(*----------------------------------------------------------------------*
 | function TPage.GetFlags                                              |
 |                                                                      |
 | 'Get' method for Flags property                                      |
 *----------------------------------------------------------------------*)
function TPage.GetFlags(bits: Integer): Boolean;
begin
  Result := (FFlags and bits) = bits;
end;

(*----------------------------------------------------------------------*
 | function TPage.GetNode                                               |
 |                                                                      |
 | 'Get' method for the 'Node' property                                 |
 *----------------------------------------------------------------------*)
function TPage.GetHeight: Integer;
var
  i: Integer;
begin
  Result := NodeCount + PrevPageHeight;
  for i := 0 to NodeCount - 1 do
    Inc(Result, Node [i].NextPageHeight)
end;

function TPage.GetNode(idx: Integer): TNode;
begin
  if idx = -1 then
    idx := NodeCount - 1;
  Result := FNodes[idx];
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
  if NodeCount = Length(FNodes) then
    SetLength(FNodes, Length(FNodes) + 64);

  if idx < NodeCount then               // If we're inserting within the existing
                                        // nodes, create a space by moving nodes.
  begin
    Move(FNodes[idx], FNodes[idx + 1], (NodeCount - idx) * SizeOf(TNode));
                                        // Very important to clear the space -
                                        // otherwise stings will go mad.
    FillChar (FNodes[idx], SizeOf(TNode), 0);
  end;

  FNodes[idx] := node;
  Inc(FNodeCount)
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Load                                                 |
 |                                                                      |
 | Load a page.                                                         |
 *----------------------------------------------------------------------*)
procedure TPage.Load;
var
  pr: TPageRec;
  i: Integer;
  p: PChar;
  pnh: PNodeHeader;
  st: string;
begin
        // Read the page
  Owner.FFileStream.Seek(SizeOf(TFileInfo) + FIdx * SizeOf(TPageRec), soFromBeginning);
  Owner.FFileStream.Read(pr, SizeOf(pr));

        // Get headeer info
  Self.FFlags := pr.Header.Flags;
  Self.FPrevPage := pr.Header.PrevPage;
  Self.FPrevPageHeight := pr.Header.PrevPageHeight;

  p := @pr.Data [0];
  if pr.Header.KeysOnPage > Length(FNodes) then
    SetLength(FNodes, pr.Header.KeysOnPage);

  FNodeCount := 0;
        // Decode the nodes.
  for i := 0 to Integer (pr.Header.KeysOnPage) - 1 do
  begin
    pnh := PNodeHeader (p);
    Inc(p, SizeOf(TNodeHeader));
    Inc(FTotalDataLen, SizeOf(TNodeHeader) + pnh^.KeyLen);

    SetString (st, p, pnh^.KeyLen);
    Inc(p, pnh^.KeyLen);

    FNodes[FNodeCount].key := st;
    FNodes[FNodeCount].NextPage := pnh^.NextPage;
    FNodes[FNodeCount].NextPageHeight := pnh^.NextPageHeight;

    Inc(FNodeCount)
  end;

  Flags[flgDirty] := False
end;

(*----------------------------------------------------------------------*
 | procedure TPage.Save                                                 |
 |                                                                      |
 | Save a page                                                          |
 *----------------------------------------------------------------------*)
procedure TPage.Save;
var
  pr: TPageRec;
  i: Integer;
  p: PChar;
  pnh: PNodeHeader;
  st: string;
  nd: TNode;
begin
  if not Flags[flgDirty] then Exit;    // No need to save

  pr.Header.Flags := FFlags;            // Set the header info.
  pr.Header.KeysOnPage := NodeCount;
  pr.Header.PrevPage := PrevPage;
  pr.Header.PrevPageHeight := PrevPageHeight;
  FillChar (pr.Data, SizeOf(pr.Data), 0);

  p := @pr.Data [0];
                                        // Encode the nodes
  for i := 0 to NodeCount - 1 do
  begin
    nd := Node [i];
    pnh := PNodeHeader (p);
    pnh^.NextPage := nd.NextPage;
    pnh^.NextPageHeight := nd.NextPageHeight;
    st := nd.key;
    pnh^.KeyLen := Length(st);

    Inc(p, SizeOf(TNodeHeader));
    Move(st [1], p^, pnh^.KeyLen);
    Inc(p, pnh^.KeyLen)
  end;
                                        // Write the data
  Owner.FFileStream.Seek(SizeOf(TFileInfo) + FIdx * SizeOf(TPageRec), soFromBeginning);
  Owner.FFileStream.Write(pr, SizeOf(pr));

  Flags[flgDirty] := False
end;

(*----------------------------------------------------------------------*
 | procedure TPage.SetFlags                                             |
 |                                                                      |
 | 'Set' method for flags property                                      |
 *----------------------------------------------------------------------*)
procedure TPage.SetFlags(bits: Integer; const Value: Boolean);
begin
  if Value then
    FFlags := FFlags or bits
  else
    FFlags := FFlags and not bits
end;

{ TPageCache }

(*----------------------------------------------------------------------*
 | function TPageCache.CanRemove                                        |
 |                                                                      |
 | CanRemove is called when the cache removes a page from itself.       |
 | Override it to save the page.                                        |
 *----------------------------------------------------------------------*)
function TPageCache.CanRemove(AObject: TObject): Boolean;
begin
  TPage(AObject).Save;
  Result := True
end;

{ TRawBTreeIterator }

(*----------------------------------------------------------------------*
 | procedure TRawBTreeIterator.ClearPageStack                           |
 |                                                                      |
 | Clear the iterator stack.                                            |
 *----------------------------------------------------------------------*)
procedure TRawBTreeIterator.ClearPageStack;
begin
  while FStack.Count > 0 do
    FStack.Pop.Free
end;

(*----------------------------------------------------------------------*
 | constructor TRawBTreeIterator.Create                                 |
 |                                                                      |
 | Constructor for TRawBTreeIterator                                    |
 *----------------------------------------------------------------------*)
constructor TRawBTreeIterator.Create(ABTree: TRawBTree);
begin
  FBTree := ABTree;
  FStack := TObjectStack.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TRawBTreeIterator.Destroy                                 |
 |                                                                      |
 | Destructor for TRawBTreeIterator                                     |
 *----------------------------------------------------------------------*)
destructor TRawBTreeIterator.Destroy;
begin
  ClearPageStack;
  FStack.Free;

  inherited;
end;

function TRawBTreeIterator.Find(var key: string): Boolean;
var
  pageNo, idx: Integer;
  pg: TPage;
  found: Boolean;
begin
  ClearPageStack;
  pageNo := BTree.FFileInfo.RootPage;
  repeat
    pg := BTree.Page [pageNo];
    found := pg.FindNode(key, idx);
    FStack.Push(TIteratorNode.Create(pageNo, idx));
    if not found then
      if idx = 0 then
        pageNo := pg.PrevPage
      else
        pageNo := pg.Node [idx - 1].NextPage
  until found or (pageNo = -1);

  Result := found;
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
function TRawBTreeIterator.First(var key: string): Boolean;
var
  pageNo: Integer;
  pg: TPage;
begin
  ClearPageStack;
  pageNo := BTree.FFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      break;    // Empty tree containing empty root pg.

    FStack.Push(TIteratorNode.Create(pageNo, 0));
    pageNo := pg.PrevPage
  until pageNo = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node [0].key;
    Result := True
  end
  else
    Result := False
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
function TRawBTreeIterator.Last(var key: string): Boolean;
var
  pageNo: Integer;
  pg: TPage;
begin
  ClearPageStack;
  pageNo := BTree.FFileInfo.RootPage;
  repeat
    pg := BTree.Page[pageNo];

    if (pg.PrevPage = -1) and (pg.NodeCount = 0) then
      break;

    FStack.Push(TIteratorNode.Create(pageNo, pg.NodeCount - 1));
    pageNo := pg.Node [-1].NextPage
  until pg.PrevPage = -1;

  if pg.NodeCount > 0 then
  begin
    key := pg.Node [-1].key;
    Result := True
  end
  else
    Result := False
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
function TRawBTreeIterator.Next(var key: string): Boolean;
var
  pg: TPage;
  node: TIteratorNode;
  tmp, tmp1: Integer;

begin
  pg := nil;
  node := nil;
  if FStack.Count > 0 then
  begin
    node := TIteratorNode(FStack.Pop);         // Pop previously returned node
    pg := BTree.Page[node.PageNo];

                                                // Are there children?
    if pg.Node [node.FKeyIdx].NextPage <> -1 then
    begin
      FStack.Push(node);
                                                // Goto children...
      pg := BTree.Page[pg.Node[node.FKeyIdx].NextPage];
      node := TIteratorNode.Create(pg.FIdx, 0);

                                                // ... then Left, Left, Left!
      while pg.PrevPage <> - 1 do
      begin
        FStack.Push(node);
        pg := BTree.Page [pg.PrevPage];
        node := TIteratorNode.Create(pg.FIdx, 0)
      end;

      node.FPageNo := pg.Idx;
    end                                         // Can we go right ?
    else
      if node.FKeyIdx < pg.NodeCount - 1 then
        Inc(node.FKeyIdx)
      else
      repeat                                    // No children - and can't go right
        tmp := node.PageNo;
        FreeAndNil (node);
        if FStack.Count > 0 then
        begin                                   // Pop a lower node.
          node := TIteratorNode(FStack.Pop);
          pg := BTree.Page[node.PageNo];

          if (node.FKeyIdx = 0) then
            tmp1 := pg.PrevPage
          else
            tmp1 := pg.Node [node.FKeyIdx - 1].NextPage;

          if tmp <> tmp1 then                   // If we we came from the node's children...
            Inc(node.FKeyIdx);                 // .. go right, or cycle round to pop another
                                                // pg.
          if node.FKeyIdx < pg.NodeCount then
            break
        end
        else break
      until False
  end;

  if Assigned(node) then                       // Did we find a next node?
  begin
    FStack.Push(node);                          // Push it, for next 'next' or 'prev'
    key := pg.Node [node.FKeyIdx].key;
    Result := True
  end
  else
    Result := False
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
function TRawBTreeIterator.Prev(var key: string): Boolean;
begin
  Result := False;              // Not yet implemented!
end;

{ TIteratorNode }

(*----------------------------------------------------------------------*
 | constructor TIteratorNode.Create                                     |
 |                                                                      |
 | Create an iterator (stack) node.                                     |
 *----------------------------------------------------------------------*)
constructor TIteratorNode.Create(APageNo, AKeyIdx: Integer);
begin
  FPageNo := APageNo;
  FKeyIdx := AKeyIdx;
end;

{ TBTree }

function TBTree.AddKey(const key: string; DataRec: Integer): Boolean;
begin
  Result := inherited AddKey(key + IntToBin (DataRec))
end;

function TBTree.CompareKeys(const k1, k2: string): Integer;
begin
  if CaseSensitive then
    Result := CompareStr (Copy(k1, 1, Length(k1) - SizeOf(Integer)), Copy(k2, 1, Length(k2) - SizeOf(Integer)))
  else
    Result := CompareText(Copy(k1, 1, Length(k1) - SizeOf(Integer)), Copy(k2, 1, Length(k2) - SizeOf(Integer)))
end;

function TBTree.DeleteKey(const key: string): Boolean;
begin
  Result := inherited DeleteKey(key + StringOfChar (#0, SizeOf(Integer)));
end;

function TBTree.ExtractDataRec(const key: string): Integer;
begin
  Move((PChar (key) + Length(key) - 4)^, Result, SizeOf(Result))
end;

function TBTree.Find(key: string; var dataRec: Integer): Boolean;
var
  k: string;
begin
  Result := inherited Find (key + StringOfChar (#0, SizeOf(Integer)), k);
  if Result then
    dataRec := ExtractDataRec (k)
end;

procedure TBTree.ForEach(proc: TBTreeForEachProc);
var
  continue: Boolean;
  k: string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
      pg := Page [pageNo]               // Original 'pg' may no have been
                                        // deleted from the cache.  So make sure
                                        // it's reloaded
    end;

    for i := 0 to pg.NodeCount - 1 do
      if continue then
      begin
                                        // Call the callback for the node
        node := pg.Node [i];
        k := Copy(node.key, 1, Length(node.key) - 4);

        Proc (k, ExtractDataRec (node.key), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach(FFileInfo.RootPage)
end;

function TBTree.GetDataRec(const key: string): Integer;
begin
  if not Find (key, Result) then
    Result := -1
end;

function TBTree.GetIndexOfKey(var key: string;
  var dataRec: Integer): Integer;
var
  k: string;
begin
  k := key + StringOfChar (#0, 4);
  Result := inherited GetIndexOfKey(k);
  if Result > -1 then
  begin
    dataRec := ExtractDataRec (k);
    key := Copy(k, 1, Length(k) - SizeOf(Integer))
  end
end;

function TBTree.GetKey(idx: Integer; var dataRec: Integer): string;
begin
  Result := inherited GetKey(idx);
  if Result <> '' then
  begin
    dataRec := ExtractDataRec (Result);
    SetLength(Result, Length(Result) - SizeOf(Integer))
  end
end;

function TBTree.InternalGetKey(idx: Integer): string;
var
  dr: Integer;
begin
  Result := GetKey(idx, dr)
end;

function TBTree.IntToBin(i: Integer): string;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Result [1], SizeOf(Integer));
end;

procedure TBTree.SetDataRec(const key: string; const Value: Integer);
var
  pg: TPage;
  idx: Integer;
  k: string;
begin
  pg := RootPage;
  k := key + StringOfChar (#0, SizeOf(Integer));

  BeginUpdate;
  try
    while Assigned(pg) do
    begin
      if pg.FindNode(k, idx) then
      begin
        pg.FNodes[idx].key := key + IntToBin (Value);
        pg.Flags[flgDirty] := True;
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
  inherited Create(ABTree);
end;

function TBTreeIterator.Find(var key: string;
  var dataRec: Integer): Boolean;
var
  k: string;
begin
  k := key + StringOfChar (#0, SizeOf(Integer));

  Result := inherited Find (k);
  if Result then
  begin
    key := k;
    SplitKey(key, dataRec)
  end
end;

function TBTreeIterator.First(var key: string; var dataRec: Integer): Boolean;
begin
  Result := inherited First(key);
  if Result then
    SplitKey(key, dataRec)
end;

function TBTreeIterator.Last(var key: string;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Last(key);
  if Result then
    SplitKey(key, dataRec)
end;

function TBTreeIterator.Next(var key: string;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Next(key);
  if Result then
    SplitKey(key, dataRec)
end;

function TBTreeIterator.Prev(var key: string;
  var dataRec: Integer): Boolean;
begin
  Result := inherited Prev (key);
  if Result then
    SplitKey(key, dataRec)
end;

procedure TBTreeIterator.SplitKey(var key: string; var dataRec: Integer);
begin
  dataRec := TBTree(BTree).ExtractDataRec(key);
  key := Copy(key, 1, Length(key) - SizeOf(Integer))
end;

{ TDataTree }

function TDataTree.AddKey(n: Integer; const st: string): Boolean;
begin
  Result := inherited AddKey(IntToBin (n) + st)
end;

function TDataTree.BinToInt(const st: string): Integer;
begin
  move(st [1], Result, SizeOf(Integer))
end;

function TDataTree.CompareKeys(const k1, k2: string): Integer;
begin
  Result := BinToInt(k1) - BinToInt(k2)
end;

function TDataTree.DeleteKey(n: Integer): Boolean;
begin
  Result := inherited DeleteKey(IntToBin (n))
end;

function TDataTree.Find(n: Integer; var st: string): Boolean;
var
  k: string;
begin
  Result := inherited Find (IntToBin (n), k);
  if Result then
    st := Copy(k, 5, MaxInt)
end;

procedure TDataTree.ForEach(proc: TDataTreeForEachProc);
var
  continue: Boolean;
  k: string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
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
        Proc (BinToInt(k), Copy(k, SizeOf(Integer) + 1, MaxInt), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach(FFileInfo.RootPage)
end;

function TDataTree.GetKey(idx: Integer; var DataRec: Integer): string;
begin
  Result := inherited GetKey(idx);
  DataRec := BinToInt(Result);
  Result := Copy(Result, SizeOf(Integer) + 1, MaxInt);
end;

function TDataTree.IntToBin(i: Integer): string;
begin
  SetLength(Result, SizeOf(Integer));
  Move(i, Result [1], SizeOf(Integer));
end;

{ TDataTreeIterator }

constructor TDataTreeIterator.Create(ADataTree: TDataTree);
begin
  inherited Create(ADataTree)
end;

function TDataTreeIterator.Find(n: Integer; var st: string): Boolean;
var
  k: string;
begin
  SetLength(k, SizeOf(Integer));
  Move(n, k [1], SizeOf(Integer));
  Result := inherited Find (k);
  if Result then
  begin
    st := k;
    SplitKey(n, st)
  end
end;

function TDataTreeIterator.First(var n: Integer; var st: string): Boolean;
begin
  Result := inherited First(st);
  if Result then
    SplitKey(n, st)
end;

function TDataTreeIterator.Last(var n: Integer; var st: string): Boolean;
begin
  Result := inherited Last(st);
  if Result then
    SplitKey(n, st)
end;

function TDataTreeIterator.Next(var n: Integer; var st: string): Boolean;
begin
  Result := inherited Next(st);
  if Result then
    SplitKey(n, st)
end;

function TDataTreeIterator.Prev (var n: Integer; var st: string): Boolean;
begin
  Result := inherited Prev (st);
  if Result then
    SplitKey(n, st)
end;

procedure TDataTreeIterator.SplitKey(var n: Integer; var key: string);
begin
  Move(key [1], n, SizeOf(Integer));
  key := Copy(key, SizeOf(Integer) + 1, MaxInt)
end;

{ TIndexTree }

function TIndexTree.AddKey(i: Integer): Boolean;
begin
  IntToBinBuffer (i);
  Result := inherited AddKey(FBinBuffer)
end;

function TIndexTree.BinToInt(const st: string): Integer;
begin
  move(st [1], Result, SizeOf(Integer))
end;

function TIndexTree.CompareKeys(const k1, k2: string): Integer;
begin
  Result := BinToInt(k1) - BinToInt(k2)
end;

constructor TIndexTree.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FBinBuffer := #0#0#0#0
end;

function TIndexTree.Delete(n: Integer): Boolean;
var
  i: Integer;
begin
  i := BinToInt(GetKey(n));
  Result := DeleteKey(i)
end;

function TIndexTree.DeleteKey(i: Integer): Boolean;
begin
  IntToBinBuffer (i);
  Result := inherited DeleteKey(FBinBuffer)
end;

function TIndexTree.Find(i: Integer): Boolean;
var
  fKey: string;
begin
  IntToBinBuffer (i);
  Result := inherited Find (FBinBuffer, fKey)
end;

procedure TIndexTree.ForEach(proc: TIndexTreeForEachProc);
var
  continue: Boolean;
  k: string;

//----------------------------------------------------------
// Recursively call 'proc' for all the keys and children.
  procedure DoForEach(pageNo: Integer);
  var
    i: Integer;
    node: TNode;
    pg: TPage;

  begin
    if not Continue then Exit;

    pg := Page [pageNo];
    if pg.PrevPage <> -1 then
    begin                               // Do subkeys on pg left
      DoForEach(pg.PrevPage);
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
        Proc (BinToInt(k), continue);

                                        // Do subkeys on node right
        if continue and (node.NextPage <> -1) then
        begin
          DoForEach(node.NextPage);
          pg := Page [pageNo]
        end
      end
  end;

begin
  continue := True;
  DoForEach(FFileInfo.RootPage)
end;

function TIndexTree.GetIndexOf(i: Integer): Integer;
begin
  IntToBinBuffer (i);
  Result := GetIndexOfKey(FBinBuffer)
end;

function TIndexTree.GetValue(n: Integer): Integer;
begin
  Result := BinToInt(GetKey(n))
end;

procedure TIndexTree.IntToBinBuffer(i: Integer);
begin
  Move(i, FBinBuffer [1], SizeOf(Integer));
end;

end.
