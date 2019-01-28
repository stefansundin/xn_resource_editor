(*======================================================================*
 | unitObjectCache                                                      |
 |                                                                      |
 | Object caching & association classes:                                |
 |                                                                      |
 | TObjectCache                 Implements a flexible cache of objects  |
 | TClassAssociations           Associates pairs of classes             |
 | TClassStringAssociations     Associates a string/class pairs         |
 | TObjectProcessor             Process a list of objects in a          |
 |                              background thread.                      |
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
 | 1.0      10/12/2003  CPWW  Original                                  |
 *======================================================================*)


unit unitObjectCache;

interface

uses Windows, Classes, SysUtils, ConTnrs, SyncObjs;

type
TObjectCacheProc = procedure (obj : TObject; idx, param : Integer; var continue : boolean) of object;

TObjectCache = class;

TObjectCacheEnumerator = class
private
  fObjectCache : TObjectCache;
  fIdx : Integer;
public
  constructor Create (AObjectCache : TObjectCache);
  function MoveNext : boolean;
  function GetCurrent : TObject;

  property Current : TObject read GetCurrent;
end;

//---------------------------------------------------------------
TObjectCache = class
private
  fOrigCapacity : Integer;
  fObjects : TObjectList;
  function GetOwnsObjects: boolean;
  procedure SetOwnsObjects(const Value: boolean);
  function GetCapacity: Integer;
  procedure SetCapacity(const Value: Integer);
  function GetCount: Integer;
protected
  function CanRemove (AObject : TObject) : boolean; virtual;
  function Matches (ObjA, ObjB : TObject) : boolean; virtual;
public
  constructor Create (ACapacity : Integer; OwnsObjects : boolean);
  destructor Destroy; override;
  function IndexOfObject (AObject : TObject) : Integer;
  procedure Add (AObject : TObject); virtual;
  procedure Clear;
  function ForEach (proc : TObjectCacheProc; param : Integer) : TObject;
  function ForEachIdx (proc : TObjectCacheProc; param : Integer) : Integer;
  function GetEnumerator : TObjectCacheEnumerator;
  procedure BringToFrontObject (idx : Integer);
  function ObjectAt (idx : Integer) : TObject;

  procedure Remove (AObject : TObject);
  function Extract (AObject : TObject) : TObject;

  procedure Push (AObject : TObject);
  function Pop : TObject;

  property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  property Capacity : Integer read GetCapacity write SetCapacity;
  property Count : Integer read GetCount;
end;

//---------------------------------------------------------------
TClassAssociation = class
private
  fClassA, fClassB : TClass;
public
  constructor Create (AClassA, AClassB : TClass);

  property ClassA : TClass read fClassA;
  property ClassB : TClass read fClassB;
end;

//---------------------------------------------------------------
TClassAssociations = class
private
  fAssociations : TObjectList;
  function GetCount: Integer;
  function GetAssociation(idx: Integer): TClassAssociation;
  function GetIndexOf(classA, classB: TClass): Integer;
  function GetIndexOfClassA(classA: TClass): Integer;
  function GetIndexOfClassB(classB: TClass): Integer;
protected
  property Association [idx : Integer] : TClassAssociation read GetAssociation;
  property Count : Integer read GetCount;
  property IndexOf [classA, classB : TClass] : Integer read GetIndexOf;
  property IndexOfClassA [classA : TClass] : Integer read GetIndexOfClassA;
  property IndexOfClassB [classB : TClass] : Integer read GetIndexOfClassB;
public
  constructor Create;
  destructor Destroy; override;

  procedure Associate (classA, classB : TClass);
  procedure DisAssociate (classA, classB : TClass);

  function FindClassBFor (classA : TClass) : TClass;
  function FindClassAFor (classB : TClass) : TClass;
end;

//---------------------------------------------------------------
TClassStringAssociations = class
private
  fAssociations : TStringList;
  function GetIndexOf(const st: string; cls: TClass): Integer;
  function GetCount: Integer;
  function GetString(idx: Integer): string;
  function GetClass(idx: Integer): TClass;
protected
  property IndexOf [const st : string; cls : TClass] : Integer read GetIndexOf;
public
  constructor Create;
  destructor Destroy; override;

  procedure Associate (const st : string; cls : TClass);
  procedure DisAssociate (const st : string; cls : TClass);

  function FindStringFor (cls : TClass) : string;
  function FindClassFor (const st : string) : TClass;

  property Count : Integer read GetCount;
  property Strings [idx : Integer] : string read GetString;
  property Classes [idx : Integer] : TClass read GetClass;
end;

TObjectProcessorState = (opsIdle, opsBusy);
//---------------------------------------------------------------
TObjectProcessor = class (TThread)
private
  fSync : TCriticalSection;
  fSignal : TEvent;
  fObjects : TObjectList;
  fState : TObjectProcessorState;
  procedure SetOwnsObjects(const Value: boolean);
  function GetOwnsObjects: boolean;
  function GetCount: Integer;

protected
  procedure Execute; override;

  procedure Reset (obj : TObject); virtual;
  procedure Process (obj : TObject); virtual;
  procedure ObjectsProcessed; virtual;
public
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure Terminate;
  procedure AddObjectToQueue (obj : TObject);

  property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  property State : TObjectProcessorState read fState;
  property Count : Integer read GetCount;
end;

TLog = class
private
  fLock : TCriticalSection;
  fStrings : TStrings;
  fLocked : boolean;
  fInLock : boolean;
  fCapacity: Integer;
  procedure Init;
  procedure Lock;
  procedure LimitCapacity;
  function GetStrings(idx: Integer): string;
  procedure SetCapacity(const Value: Integer);

public
  destructor Destroy; override;
  function LockGetCount : Integer;
  procedure Add (const st : string);
  procedure Clear;
  procedure Unlock;

  property Capacity : Integer read fCapacity write SetCapacity;
  property Strings [idx : Integer] : string read GetStrings;

end;

implementation

{ TObjectCache }

(*----------------------------------------------------------------------*
 | TObjectCache.Add                                                     |
 |                                                                      |
 | Add an object to the cache.                                          |
 |                                                                      |
 | Note that the cache capacity will automatically be increased if      |
 | it is full, and no objects can be removed (see the CanRemove method) |
 |                                                                      |
 | Parameters:                                                          |
 |   AObject: TObject           The object to add                       |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Add(AObject: TObject);
var
  idx, c : Integer;
  b : boolean;
begin
  idx := IndexOfObject (AObject);
  if idx = 0 then       // Already in the cache at the front
  begin
    if OwnsObjects then
      AObject.Free;
    Exit
  end;

  if idx = -1 then
  begin                 // Not already in cache.  Add it.
    b := False;
    c := fObjects.Count;
    while c >= fOrigCapacity do
    begin               // There's not room.  Remove old objects (if we're allowed)
      repeat            // Try to get back to the original capacity if it's been
        Dec (c);        // exceeded.
        if CanRemove (fObjects [c]) then
        begin
          fObjects.Delete(c);
          b := True
        end
      until b or (c = 0);
    end;

    if b then   // Shrink the cache if it's bulged.
      if fObjects.Capacity > fOrigCapacity then
        if fObjects.Count < fOrigCapacity then
            fObjects.Capacity := fOrigCapacity;

    if fObjects.Capacity = fObjects.Count then // Bulge the cache
      fObjects.Capacity := fObjects.Capacity + 1;

    fObjects.Insert (0, AObject)
  end
  else                  // The object was already in the cache.  So bring it to
  begin                 // the front
    BringToFrontObject (idx);
    if OwnsObjects then
      AObject.Free
  end
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.BringToFrontObject                            |
 |                                                                      |
 | Bring object 'idx' to the front of the cache.                        |
 |                                                                      |
 | Parameters:                                                          |
 |   idx: Integer       // Index of the object to bring to the front.   |
 *----------------------------------------------------------------------*)
procedure TObjectCache.BringToFrontObject(idx: Integer);
var
  b : boolean;
  obj : TObject;
begin
  if (idx > 0) then
  begin
    obj := fObjects [idx];
    b := OwnsObjects;
    OwnsObjects := False;       // Temporarily turn off 'owns objects' so we
    try                         // can delete and reinsert safely.
      fObjects.Delete (idx);
      fObjects.Insert (0, obj)
    finally
      OwnsObjects := b
    end
  end
end;

(*----------------------------------------------------------------------*
 | TObjectCache.CanRemove                                               |
 |                                                                      |
 | Override this to prevent objects from being removed from the cache   |
 | - maybe because another reference to the object still exists.        |
 |                                                                      |
 | Parameters:                                                          |
 |   AObject: TObject           The object to test                      |
 |                                                                      |
 | The function returns True if the object can be safely removed.       |
 *----------------------------------------------------------------------*)
function TObjectCache.CanRemove(AObject: TObject) : boolean;
begin
  result := True
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.Clear                                         |
 |                                                                      |
 | Clear the cache - or as much of it as can safely be cleared.         |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Clear;
var
  i : Integer;
begin
  i := 0;
  while i < fObjects.Count do
    if CanRemove (fObjects [i]) then
      fObjects.Delete(i)
    else
      Inc (i)
end;

(*----------------------------------------------------------------------*
 | constructor TObjectCache.Create                                      |
 *----------------------------------------------------------------------*)
constructor TObjectCache.Create(ACapacity : Integer; OwnsObjects : boolean);
begin
  fObjects := TObjectList.Create (OwnsObjects);
  fOrigCapacity := ACapacity;
  fObjects.Capacity := ACapacity;
end;

(*----------------------------------------------------------------------*
 | destructor TObjectCache.Destroy                                      |
 *----------------------------------------------------------------------*)
destructor TObjectCache.Destroy;
begin
  fObjects.Free;

  inherited;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.Extract                                        |
 |                                                                      |
 | Extract an object from the cache.                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   AObject: TObject           The object to extract                   |
 |                                                                      |
 | The function returns the extracted object.  nb.  Even if OwnsObjects |
 | true, the object is *not* deleted                                    |
 *----------------------------------------------------------------------*)
function TObjectCache.Extract(AObject: TObject) : TObject;
begin
  result := fObjects.Extract(AObject)
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.ForEach                                        |
 |                                                                      |
 | Call 'proc' for each object in the cache                             |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TObjectCacheProc     procedure to call                       |
 |   param : Integer            User parameter to pass to the procedure |
 |                                                                      |
 | The function returns the object that caused 'proc' to return with    |
 | 'continue=False'.  You can use this eg. to search the cache.         |
 *----------------------------------------------------------------------*)
function TObjectCache.ForEach(proc: TObjectCacheProc; param : Integer) : TObject;
var
  i : Integer;
  continue : boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < fObjects.Count) do
  begin
    proc (fObjects [i], i, param, continue);
    if continue then
      Inc (i)
  end;

  if not continue then
    result := fObjects [i]
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.ForEachIdx                                     |
 |                                                                      |
 | Call 'proc' for each object in the cache.  nb. this differs from     |
 | ForEach only in the return value.                                    |
 |                                                                      |
 | Parameters:                                                          |
 |   proc: TObjectCacheProc     procedure to call                       |
 |   param : Integer            User parameter to pass to the procedure |
 |                                                                      |
 | The function returns the index of the object that caused 'proc'      |
 | to return with 'continue=False'.  You can use this eg. to search the |
 | cache.                                                               |
 *----------------------------------------------------------------------*)
function TObjectCache.ForEachIdx(proc: TObjectCacheProc;
  param: Integer): Integer;
var
  i : Integer;
  continue : boolean;
begin
  i := 0;
  continue := True;

  while continue and (i < fObjects.Count) do
  begin
    proc (fObjects [i], i, param, continue);
    if continue then
      Inc (i)
  end;

  if not continue then
    result := i
  else
    result := -1
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.GetCapacity                                    |
 |                                                                      |
 | Return the (preferred) capacity of the cache.                        |
 *----------------------------------------------------------------------*)
function TObjectCache.GetCapacity: Integer;
begin
  result := fOrigCapacity
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.GetCount                                       |
 |                                                                      |
 | Returns the number of cached objects                                 |
 *----------------------------------------------------------------------*)
function TObjectCache.GetCount: Integer;
begin
  result := fObjects.Count;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.GetEnumerator                                  |
 |                                                                      |
 | Create and return a TObjecTCacheEnumerator to support                |
 | for..in in Delphi 2005                                               |
 *----------------------------------------------------------------------*)
function TObjectCache.GetEnumerator: TObjectCacheEnumerator;
begin
  result := TObjectCacheEnumerator.Create(self);
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.GetOwnsObjects                                 |
 |                                                                      |
 | Returns the 'OwnsObjects' state of the cache.                        |
 *----------------------------------------------------------------------*)
function TObjectCache.GetOwnsObjects: boolean;
begin
  result := fObjects.OwnsObjects;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.IndexOfObject                                  |
 |                                                                      |
 | Returns the index of an object in the cache                          |
 *----------------------------------------------------------------------*)
function TObjectCache.IndexOfObject(AObject: TObject): Integer;
var
  i, c : Integer;
begin
  result := -1;
  c := fObjects.Count;
  for i := 0 to c - 1 do
    if Matches (fObjects [i], AObject) then
    begin
      result := i;
      break
    end;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.Matches                                        |
 |                                                                      |
 | Return 'True' if ObjA matches ObB.  Override this to provide more    |
 | complicated matching of objects.                                     |
 *----------------------------------------------------------------------*)
function TObjectCache.Matches(ObjA, ObjB: TObject): boolean;
begin
  result := ObjA = ObjB;
end;

(*----------------------------------------------------------------------*
 | function TObjectCache.ObjectAt                                       |
 |                                                                      |
 | Return the idx'th object in the cache                                |
 *----------------------------------------------------------------------*)
function TObjectCache.ObjectAt(idx: Integer): TObject;
begin
  result := fObjects [idx]
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.Remove                                        |
 |                                                                      |
 | Remove an object from the cache.  nb.  Even if OwnsObjects is True   |
 | the object isn't deleted.                                            |
 *----------------------------------------------------------------------*)
function TObjectCache.Pop: TObject;
begin
  if Count > 0 then
  begin
    result := fObjects [0];
    Extract (result)
  end
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | TObjectCache.Push                                                    |
 |                                                                      |
 | Alias for 'Add' - see above.                                         |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Push(AObject: TObject);
begin
  Add (AObject);
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.Remove                                        |
 |                                                                      |
 | Remove an object from the cache                                      |
 *----------------------------------------------------------------------*)
procedure TObjectCache.Remove(AObject: TObject);
begin
  if (fObjects.Count > 0) and CanRemove (AObject) then
    fObjects.Remove(AObject)
end;

(*----------------------------------------------------------------------*
 | prcoedure TObjectCache.SetCapacity                                   |
 |                                                                      |
 | Set the (preferred) capacity of the cache                            |
 *----------------------------------------------------------------------*)
procedure TObjectCache.SetCapacity(const Value: Integer);
begin
  if Value <> fOrigCapacity then
  begin
    while fObjects.Count > Value do
      fObjects.Delete(fObjects.Count - 1);
    fObjects.Capacity := Value;
    fOrigCapacity := Value
  end
end;

(*----------------------------------------------------------------------*
 | procedure TObjectCache.SetOwnsObjects                                |
 |                                                                      |
 | If 'OwnsObjects' is true, the object cache assumes responsibility    |
 | for deleting objects added to it (with Add or Push).  Whenever       |
 | objects are removed from the cache, either with 'Remove' or because  |
 | the cache overflowed, the get deleted (freed).                       |
 *----------------------------------------------------------------------*)
procedure TObjectCache.SetOwnsObjects(const Value: boolean);
begin
  fObjects.OwnsObjects := Value
end;

{ TClassAssociations }

// TClassAssociations allows you to associate a class with another class.
// eg. you could associate a TGraphicsForm with TGraphic, etc.
//
// TClassAssociations support inheritance, so as TIcon derives from
// TGraphic, then TGraphicsForm will be returned for TIcon - unless a
// separate TIconForm is registered for TIcon.

(*----------------------------------------------------------------------*
 | TClassAssociations.Associate                                         |
 |                                                                      |
 | Associate ClassA with ClassB                                         |
 |                                                                      |
 | Parameters:                                                          |
 |   classA, classB: TClass             The classes to associate        |
 *----------------------------------------------------------------------*)
procedure TClassAssociations.Associate(classA, classB: TClass);
var
  i : Integer;
begin
  i := IndexOf [classA, classB];

  if i = -1 then
    fAssociations.Insert(0, TClassAssociation.Create(classA, classB));
end;

(*----------------------------------------------------------------------*
 | constructor TClassAssociations.Create                                 |
 *----------------------------------------------------------------------*)
constructor TClassAssociations.Create;
begin
  fAssociations := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TClassAssociations.Destroy                                |
 *----------------------------------------------------------------------*)
destructor TClassAssociations.Destroy;
begin
  fAssociations.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TClassAssociations.DisAssociate                            |
 |                                                                      |
 | Remove the association between classA and classB                     |
 *----------------------------------------------------------------------*)
procedure TClassAssociations.DisAssociate(classA, classB: TClass);
var
  idx : Integer;
begin
  idx := IndexOf [classA, classB];
  if idx >= 0 then
    fAssociations.Delete(idx);
end;

(*----------------------------------------------------------------------*
 | TClassAssociations.FindClassAFor                                     |
 |                                                                      |
 | eg. FindClassAFor (TGraphicsForm) will return TGraphic.  Note that   |
 | this way round there is no inheritance.  There's either an           |
 | association or there's not.                                          |
 |                                                                      |
 | Parameters:                                                          |
 |   classB: TClass             The ClassB to find.                     |
 |                                                                      |
 | The function returns the classA that matches classB                  |
 *----------------------------------------------------------------------*)
function TClassAssociations.FindClassAFor(classB: TClass): TClass;
var
  idx : Integer;
begin
  idx := IndexOfClassB [classB];
  if idx >= 0 then
    result := Association [idx].ClassA
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.FindClassBFor                            |
 |                                                                      |
 | eg. FindClassAFor (TGraphic) will return TGraphicForm.               |
 |                                                                      |
 | nb. this supports inheritance, so as TIcon derives from TGraphic,    |
 | TGraphicsForm will be returned for TIcon - unless a separate         |
 | TIconForm is registered for TIcon.                                   |
 |                                                                      |
 | Parameters:                                                          |
 |   classA: TClass              The ClassA to find                     |
 |                                                                      |
 | The function returns the classB that matches classA.  If no match is |
 | found, classA's Ancestor classes are searched too.                   |
 *----------------------------------------------------------------------*)
function TClassAssociations.FindClassBFor(classA: TClass): TClass;
var
  idx : Integer;
begin
  idx := IndexOfClassA [classA];
  if idx >= 0 then
    result := Association [idx].ClassB
  else
    result := Nil
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.GetAssociation                           |
 |                                                                      |
 | 'Get' method for Association property                                |
 *----------------------------------------------------------------------*)
function TClassAssociations.GetAssociation(
  idx: Integer): TClassAssociation;
begin
  result := TClassAssociation (fAssociations [idx]);
end;

(*----------------------------------------------------------------------*
 | function TClassAssociations.GetCount                                 |
 |                                                                      |
 | 'Get' method for Count property                                      |
 *----------------------------------------------------------------------*)
function TClassAssociations.GetCount: Integer;
begin
  result := fAssociations.Count;
end;

function TClassAssociations.GetIndexOf(classA, classB: TClass): Integer;
var
  i : Integer;
  ass : TClassAssociation;
begin
  result := -1;

  for i := 0 to Count - 1 do
  begin
    ass := Association [i];
    if (ass.ClassA = classA) and (ass.ClassB = classB) then
    begin
      result := i;
      break
    end
  end
end;

function TClassAssociations.GetIndexOfClassA(classA: TClass): Integer;
var
  i : Integer;
  ass : TClassAssociation;
begin
  result := -1;

  while (result = -1) and Assigned (classA) do
  begin
    for i := 0 to Count - 1 do
    begin
      ass := Association [i];
      if ass.ClassA = classA then
      begin
        result := i;
        break
      end
    end;

    if result = -1 then
      classA := classA.ClassParent
  end
end;

function TClassAssociations.GetIndexOfClassB(classB: TClass): Integer;
var
  i : Integer;
  ass : TClassAssociation;
begin
  result := -1;

  for i := 0 to Count - 1 do
  begin
    ass := Association [i];
    if ass.ClassB = classB then
    begin
      result := i;
      break
    end
  end
end;

{ TClassAssociation }

constructor TClassAssociation.Create(AClassA, AClassB: TClass);
begin
  fClassA := AClassA;
  fClassB := AClassB
end;

{ TClassStringAssociations }

procedure TClassStringAssociations.Associate(const st: string; cls: TClass);
var
  i : Integer;
begin
  i := IndexOf [st, cls];

  if i = -1 then
    fAssociations.InsertObject (0, st, TObject (cls))
end;

constructor TClassStringAssociations.Create;
begin
  fAssociations := TStringList.Create;
end;

destructor TClassStringAssociations.Destroy;
begin
  fAssociations.Free;

  inherited;
end;

procedure TClassStringAssociations.DisAssociate(const st: string;
  cls: TClass);
var
  idx : Integer;
begin
  idx := IndexOf [st, cls];
  if idx >= 0 then
    fAssociations.Delete(idx);
end;

function TClassStringAssociations.FindClassFor(const st: string): TClass;
var
  idx : Integer;
begin
  idx := fAssociations.IndexOf (st);
  if idx >= 0 then
    result := TClass (fAssociations.Objects [idx])
  else
    result := Nil
end;

function TClassStringAssociations.FindStringFor(cls: TClass): string;
var
  i : Integer;
begin
  result := '';
  for i := 0 to Count - 1 do
    if TClass (fAssociations.Objects [i]) = cls then
    begin
      result := fAssociations [i];
      break
    end
end;

function TClassStringAssociations.GetClass(idx: Integer): TClass;
begin
  result := TClass (fAssociations.Objects [idx])
end;

function TClassStringAssociations.GetCount: Integer;
begin
  result := fAssociations.Count
end;

function TClassStringAssociations.GetIndexOf(const st: string;
  cls: TClass): Integer;
var
  i : Integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    if (fAssociations.Objects [i] = TObject (cls)) and SameText (st, fAssociations [i]) then
    begin
      result := i;
      break
    end
end;

function TClassStringAssociations.GetString(idx: Integer): string;
begin
  result := fAssociations [idx];
end;

{ TObjectProcessor }

procedure TObjectProcessor.AddObjectToQueue(obj: TObject);
begin
  fSync.Enter;
  try
    fObjects.Add(obj);
    if fState = opsIdle then
      fSignal.SetEvent;
  finally
    fSync.Leave
  end
end;

procedure TObjectProcessor.Clear;
var
  i : Integer;
begin
  fSync.Enter;
  try
    for i := 0 to fObjects.Count - 1 do
      Reset (fObjects [i]);
    fObjects.Clear;
  finally
    fSync.Leave
  end
end;

constructor TObjectProcessor.Create;
begin
  fSync := TCriticalSection.Create;
  fSignal := TEvent.Create (Nil, false, false, '');
  fObjects := TObjectList.Create;
  fObjects.OwnsObjects := False;

  inherited Create (false);
end;

destructor TObjectProcessor.Destroy;
begin
  fSync.Free;
  fSignal.Free;

  inherited;
end;

procedure TObjectProcessor.Execute;
begin
  while not Terminated do
  begin
    try
      if fObjects.Count = 0 then
        fSignal.WaitFor(INFINITE);

      fState := opsBusy;
      try
        while not Terminated and (fObjects.Count > 0) do
        begin
          fSync.Enter;
          try
            if fObjects.Count > 0 then
            begin
              Process (fObjects [0]);
              fObjects.Delete(0)
            end
          finally
            fSync.Leave
          end;
        end;
        if not Terminated then
          ObjectsProcessed;
      finally
        fState := opsIdle
      end
    except
      try
        Clear
      except
      end
    end
  end
end;

function TObjectProcessor.GetCount: Integer;
begin
  result := fObjects.Count
end;

function TObjectProcessor.GetOwnsObjects: boolean;
begin
  result := fObjects.OwnsObjects
end;

procedure TObjectProcessor.ObjectsProcessed;
begin
// Stub - called when a batch of objects has been processed
end;

procedure TObjectProcessor.Process(obj: TObject);
begin
// Stub - called to process each object
end;

procedure TObjectProcessor.Reset(obj: TObject);
begin
// Stub - called when un-processed objects are removed from the queue (by Clear)
end;

procedure TObjectProcessor.SetOwnsObjects(const Value: boolean);
begin
  fObjects.OwnsObjects := Value
end;

procedure TObjectProcessor.Terminate;
begin
  Clear;
  inherited Terminate;
  fSignal.SetEvent;
  WaitFor
end;

{ TLog }

procedure TLog.Add(const st: string);
begin
  Lock;
  try
    fStrings.Add(st);
    LimitCapacity
  finally
    Unlock
  end
end;

procedure TLog.Clear;
begin
  Lock;
  try
    fStrings.Clear
  finally
    Unlock
  end
end;

destructor TLog.Destroy;
begin
  fLock.Free;
  fStrings.Free;

  inherited;
end;

function TLog.GetStrings(idx: Integer): string;
begin
  if not fLocked then
    raise Exception.Create ('Must call LockGetCount');
  result := fStrings [idx];
end;

procedure TLog.Init;
begin
  if not Assigned (fStrings) then
    fStrings := TStringList.Create;

  if not Assigned (fLock) then
    fLock := TCriticalSection.Create;
end;

procedure TLog.LimitCapacity;
var
  needLock : boolean;
begin
  if fCapacity < 1 then
    Exit;

  needLock := not fInLock;

  if needLock then
    Lock;

  try
    while fStrings.Count > fCapacity do
      fStrings.Delete(0);
  finally
    if needLock then
      Unlock
  end
end;

procedure TLog.Lock;
begin
  Init;
  fLock.Enter;
  fInLock := True;
end;

function TLog.LockGetCount: Integer;
begin
  Lock;
  fLocked := True;
  result := fStrings.Count
end;

procedure TLog.SetCapacity(const Value: Integer);
begin
  if Value <> fCapacity then
  begin
    fCapacity := Value;
    LimitCapacity
  end
end;

procedure TLog.Unlock;
begin
  fLock.Leave;
  fLocked := False;
  fInLock := False;
end;

{ TObjectCacheEnumerator }

constructor TObjectCacheEnumerator.Create(AObjectCache: TObjectCache);
begin
  fIdx := -1;
  fObjectCache := AObjectCache;
end;

function TObjectCacheEnumerator.GetCurrent: TObject;
begin
  result := fObjectCache.ObjectAt(fIdx)
end;

function TObjectCacheEnumerator.MoveNext: boolean;
begin
  result := fIDx < fObjectCache.Count - 1;
  if result then
    Inc (fIdx)
end;

end.
