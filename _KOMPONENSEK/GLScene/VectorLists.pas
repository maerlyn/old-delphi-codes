{: VectorLists<p>

	Lists of vectors<p>

	<b>Historique : </b><font size=-1><ul>
      <li>04/03/01 - Egg - Optimized TAffineVectorList.Normalize (x2 speed on K7)
      <li>26/02/01 - Egg - VectorArrayLerp 3DNow optimized (x2 speed on K7) 
      <li>08/08/00 - Egg - Added TSingleList
	   <li>20/07/00 - Egg - Creation
	</ul></font>
}
unit VectorLists;

interface

uses Classes, Geometry;

type

   // TBaseList
   //
   {: Base class for lists, introduces common behaviours. }
   TBaseList = class (TPersistent)
		private
         { Private Declarations }
			FCount : Integer;
			FCapacity : Integer;
			FGrowthDelta : integer;
         FBufferItem : PByteArray;

		protected
         { Protected Declarations }
         //: The base list pointer (untyped)
         FBaseList : PByteArray;
         //: Must be defined by all subclasses in their constructor(s)
         FItemSize : Integer;

         procedure SetCount(val : Integer);
         {: Only function where list may be alloc'ed & freed.<p>
            Resizes the array pointed by FBaseList, adjust the subclass's
            typed pointer accordingly if any. }
			procedure SetCapacity(NewCapacity: Integer); virtual;
         function BufferItem : PByteArray;

		public
         { Public Declarations }
			destructor Destroy; override;
         procedure Assign(Src: TPersistent); override;

         procedure AddNulls(nbVals : Integer);
         procedure Clear;

         procedure Delete(Index : Integer);
         procedure Exchange(Index1, Index2 : Integer);

         {: Nb of items in the list }
			property Count: Integer read FCount write SetCount;
			property Capacity: Integer read FCapacity write SetCapacity;
			{: Granularité de croissance de la liste }
			property GrowthDelta : integer read FGrowthDelta write FGrowthDelta;

   end;

   // TBaseVectorList
   //
   {: Base class for lists, introduces common behaviours. }
   TBaseVectorList = class (TBaseList)
		private
         { Private Declarations }

		protected
         { Protected Declarations }
         function GetItemAddress(index : Integer) : PFloatArray;

		public
         { Public Declarations }
         procedure GetExtents(var min, max : TAffineVector); dynamic;
         function Sum : TAffineVector; dynamic;
         procedure Normalize; dynamic;
         function MaxSpacing(list2 : TBaseVectorList) : Single; dynamic;
         procedure Translate(const delta : TAffineVector); dynamic;
         {: Replace content of the list with lerp results between the two given lists }
         procedure Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single); dynamic;

         property ItemAddress[index : Integer] : PFloatArray read GetItemAddress;
   end;

  	// TAffineVectorList
	//
	{: A list of TAffineVector.<p>
		Similar to TList, but using TAffineVector as items.<p>
      The list has stack-like push/pop methods. }
	TAffineVectorList = class (TBaseVectorList)		private
         { Private Declarations }
			FList: PAffineVectorArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TAffineVector;
			procedure Put(Index: Integer; const item : TAffineVector);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TAffineVector) : Integer; overload;
			procedure Add(const i1, i2, i3 : TAffineVector); overload;
			function Add(const item : TTexPoint) : Integer; overload;
			function Add(const x, y, z : Single) : Integer; overload;
			procedure Push(const val : TAffineVector);
			function Pop : TAffineVector;			procedure Insert(Index: Integer; const item : TAffineVector);

			property Items[Index: Integer] : TAffineVector read Get write Put; default;
			property List: PAffineVectorArray read FList;

         //: Translates the given item
         procedure TranslateItem(index : Integer; const delta : TAffineVector);

         procedure Normalize; override;
         procedure Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single); override;
	end;

  	// TVectorList
	//
	{: A list of TVector.<p>
		Similar to TList, but using TVector as items.<p>
      The list has stack-like push/pop methods. }
	TVectorList = class (TBaseVectorList)		private
         { Private Declarations }
			FList: PVectorArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TVector;
			procedure Put(Index: Integer; const item : TVector);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TVector): Integer; overload;
			function Add(const item : TAffineVector; w : Single): Integer; overload;
			function Add(const x, y, z, w : Single): Integer; overload;
			procedure Add(const i1, i2, i3 : TAffineVector; w : Single); overload;
			procedure Push(const val : TVector);
			function Pop : TVector;			procedure Insert(Index: Integer; const item : TVector);

			property Items[Index: Integer] : TVector read Get write Put; default;
			property List: PVectorArray read FList;

         procedure Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single); override;
	end;

  	// TTexPointList
	//
	{: A list of TTexPoint.<p>
		Similar to TList, but using TTexPoint as items.<p>
      The list has stack-like push/pop methods. }
	TTexPointList = class (TBaseList)		private
         { Private Declarations }
			FList: PTexPointArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : TTexPoint;
			procedure Put(Index: Integer; const item : TTexPoint);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : TTexPoint): Integer;
			procedure Push(const val : TTexPoint);
			function Pop : TTexPoint;			procedure Insert(Index: Integer; const item : TTexPoint);

			property Items[Index: Integer] : TTexPoint read Get write Put; default;
			property List: PTexPointArray read FList;
	end;

  	// TIntegerList
	//
	{: A list of Integers.<p>
		Similar to TList, but using TTexPoint as items.<p>
      The list has stack-like push/pop methods. }
	TIntegerList = class (TBaseList)		private
         { Private Declarations }
			FList: PIntegerArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : Integer;
			procedure Put(Index: Integer; const item : Integer);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create;
			procedure Assign(Src: TPersistent); override;

			function Add(const item : Integer) : Integer;
			procedure Push(const val : Integer);
			function Pop : Integer;			procedure Insert(Index : Integer; const item : Integer);

			property Items[Index: Integer] : Integer read Get write Put; default;
			property List: PIntegerArray read FList;
	end;

   TSingleArray = array [0..MaxInt shr 4] of Single;
   PSingleArray = ^TSingleArray;

  	// TSingleList
	//
	{: A list of Single.<p>
		Similar to TList, but using Single as items.<p>
      The list has stack-like push/pop methods. }
	TSingleList = class (TBaseList)		private
         { Private Declarations }
			FList: PSingleArray;

		protected
         { Protected Declarations }
			function  Get(Index: Integer) : Single;
			procedure Put(Index: Integer; const item : Single);
			procedure SetCapacity(NewCapacity: Integer); override;

		public
         { Public Declarations }
			constructor Create;
			procedure Assign(Src : TPersistent); override;

			function Add(const item : Single) : Integer;
			procedure Push(const val : Single);
			function Pop : Single;			procedure Insert(Index : Integer; const item : Single);

			property Items[Index: Integer] : Single read Get write Put; default;
			property List: PSingleArray read FList;
	end;

{: Sort the refList in ascending order, ordering objList on the way. } 
procedure QuickSortLists(startIndex, endIndex : Integer;
							    refList : TSingleList; objList : TList);


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

const
   cDefaultListGrowthDelta = 16;

// QuickSortLists
//
procedure QuickSortLists(startIndex, endIndex : Integer;
								 refList : TSingleList; objList : TList);
var
	I, J : Integer;
	P : single;
begin
	if endIndex-startIndex>1 then begin
		repeat
			I:=startIndex; J:=endIndex;
			P:=refList.List[(I + J) shr 1];
			repeat
				while Single(refList.List[I])<P do Inc(I);
				while Single(refList.List[J])>P do Dec(J);
				if I <= J then begin
					refList.Exchange(i, J);
					objList.Exchange(i, j);
					Inc(I); Dec(J);
				end;
			until I > J;
			if startIndex < J then
				QuickSortLists(startIndex, J, refList, objList);
			startIndex:=I;
		until I >= endIndex;
	end else if endIndex-startIndex>0 then begin
		p:=refList.List[startIndex];
		if refList.List[endIndex]<p then begin
			refList.Exchange(startIndex, endIndex);
			objList.Exchange(startIndex, endIndex);
		end;
	end;
end;

// ------------------
// ------------------ TBaseList ------------------
// ------------------

// Destroy
//
destructor TBaseList.Destroy;
begin
   Clear;
   if Assigned(FBufferItem) then
      FreeMem(FBufferItem);
   inherited;
end;

// Assign
//
procedure TBaseList.Assign(Src: TPersistent);
begin
   if (Src is TBaseList) then begin
      SetCapacity(TBaseList(Src).Count);
   	FGrowthDelta:=TAffineVectorList(Src).FGrowthDelta;
		FCount:=FCapacity;
   end else inherited;
end;

// SetCount
//
procedure TBaseList.SetCount(val : Integer);
begin
   Assert(val>=0);
	if val>FCapacity then SetCapacity(val);
   if val>FCount then
      AddNulls(val-FCount)
   else FCount:=val;
end;

// SetCapacity
//
procedure TBaseList.SetCapacity(NewCapacity: Integer);
begin
	if NewCapacity <> FCapacity then	begin
		ReallocMem(FBaseList, NewCapacity * FItemSize);
		FCapacity:=NewCapacity;
	end;
end;

// AddNulls
//
procedure TBaseList.AddNulls(nbVals : Integer);
begin
   if nbVals+Count>Capacity then
      SetCapacity(nbVals+Count);
   FillChar(FBaseList[FCount*FItemSize], nbVals * FItemSize, 0);
   FCount:=FCount+nbVals;
end;

// BufferItem
//
function TBaseList.BufferItem : PByteArray;
begin
   if not Assigned(FBufferItem) then
      GetMem(FBufferItem, FItemSize);
   Result:=FBufferItem;
end;

// Clear
//
procedure TBaseList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;

// Delete
//
procedure TBaseList.Delete(Index: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	Dec(FCount);
	if Index < FCount then
		System.Move(FBaseList[(Index+1)*FItemSize],
                  FBaseList[Index*FItemSize],
                  (FCount-Index)*FItemSize);
end;

// Exchange
//
procedure TBaseList.Exchange(Index1, Index2: Integer);
begin
{$IFOPT R+}
	Assert((Cardinal(Index1)<Cardinal(FCount)) and (Cardinal(Index2)<Cardinal(FCount)));
{$ENDIF}
   System.Move(FBaseList[Index1*FItemSize], BufferItem[0], FItemSize);
   System.Move(FBaseList[Index2*FItemSize], FBaseList[Index1*FItemSize], FItemSize);
   System.Move(BufferItem[0], FBaseList[Index2*FItemSize], FItemSize);
end;

// ------------------
// ------------------ TBaseVectorList ------------------
// ------------------

// GetExtents
//
procedure TBaseVectorList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
   ref : PFloatArray;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      ref:=ItemAddress[i];
      for k:=0 to 2 do begin
         f:=ref[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// Sum
//
function TBaseVectorList.Sum : TAffineVector;
var
   i : Integer;
begin
   Result:=NullVector;
   for i:=0 to Count-1 do
      AddVector(Result, PAffineVector(ItemAddress[i])^);
end;

// Normalize
//
procedure TBaseVectorList.Normalize;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      NormalizeVector(PAffineVector(ItemAddress[i])^);
end;

// MaxSpacing
//
function TBaseVectorList.MaxSpacing(list2 : TBaseVectorList) : Single;
var
   i : Integer;
   s : Single;
begin
   Assert(list2.Count=Count);
   Result:=0;
   for i:=0 to Count-1 do begin
      s:=VectorSpacing(PAffineVector(ItemAddress[i])^,
                       PAffineVector(list2.ItemAddress[i])^);
      if s>Result then Result:=s;
   end;
end;

// Translate
//
procedure TBaseVectorList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AddVector(PAffineVector(ItemAddress[i])^, delta);
end;

// Lerp
//
procedure TBaseVectorList.Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single);
var
   i : Integer;
begin
   Assert(list1.Count=list2.Count);
   Clear;
   Capacity:=list1.Count;
   FCount:=list1.Count;
   for i:=0 to list1.Count-1 do
      VectorLerp(PAffineVector(list1.ItemAddress[i])^,
                 PAffineVector(list2.ItemAddress[i])^,
                 lerpFactor, PAffineVector(ItemAddress[i])^);
end;

// GetItemAddress
//
function TBaseVectorList.GetItemAddress(index : Integer) : PFloatArray;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
   Result:=PFloatArray(@FBaseList[index*FItemSize]);
end;

// ------------------
// ------------------ TAffineVectorList ------------------
// ------------------

// Create
//
constructor TAffineVectorList.Create;
begin
   FItemSize:=SizeOf(TAffineVector);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TAffineVectorList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TAffineVectorList) then
			System.Move(TAffineVectorList(Src).FList^, FList^, FCount*SizeOf(TAffineVector));
	end else Clear;
end;

// Add (affine)
//
function TAffineVectorList.Add(const item : TAffineVector): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
  	Inc(FCount);
end;

// Add (3 affine)
//
procedure TAffineVectorList.Add(const i1, i2, i3 : TAffineVector);
begin
  	Inc(FCount, 3);
   while FCount>FCapacity do SetCapacity(FCapacity + FGrowthDelta);
	FList[FCount-3] := i1;
	FList[FCount-2] := i2;
	FList[FCount-1] := i3;
end;

// Add (texpoint)
//
function TAffineVectorList.Add(const item : TTexPoint) : Integer;
begin
   Result:=Add(AffineVectorMake(item.S, item.T, 0));
end;

// Add
//
function TAffineVectorList.Add(const x, y, z : Single) : Integer;
begin
   Result:=Add(AffineVectorMake(x, y, z));
end;

// Get
//
function TAffineVectorList.Get(Index: Integer): TAffineVector;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList^[Index];
end;

// Insert
//
procedure TAffineVectorList.Insert(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount - Index) * SizeOf(TAffineVector));
	FList^[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TAffineVectorList.Put(Index: Integer; const Item: TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList^[Index] := Item;
end;

// SetCapacity
//
procedure TAffineVectorList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PAffineVectorArray(FBaseList);
end;

// Push
//
procedure TAffineVectorList.Push(const val : TAffineVector);
begin
	Add(val);
end;

// Pop
//
function TAffineVectorList.Pop : TAffineVector;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullVector;
end;

// TranslateItem
//
procedure TAffineVectorList.TranslateItem(index : Integer; const delta : TAffineVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
   AddVector(FList^[Index], delta);
end;

// Normalize
//
procedure TAffineVectorList.Normalize;
begin
   NormalizeVectorArray(List, Count);
end;

// Lerp
//
procedure TAffineVectorList.Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single);
begin
   if (list1 is TAffineVectorList) and (list2 is TAffineVectorList) then begin
      Assert(list1.Count=list2.Count);
      Capacity:=list1.Count;
      FCount:=list1.Count;
      VectorArrayLerp(TAffineVectorList(list1).List, TAffineVectorList(list2).List,
                      lerpFactor, FCount, List);
   end else inherited;
end;

// ------------------
// ------------------ TVectorList ------------------
// ------------------

// Create
//
constructor TVectorList.Create;
begin
   FItemSize:=SizeOf(TVector);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TVectorList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TVectorList) then
			System.Move(TVectorList(Src).FList^, FList^, FCount*SizeOf(TVector));
	end else Clear;
end;

// Add
//
function TVectorList.Add(const item : TVector): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
  	Inc(FCount);
end;

// Add
//
function TVectorList.Add(const item : TAffineVector; w : Single): Integer;
begin
   Result:=Add(VectorMake(item, w));
end;

// Add
//
function TVectorList.Add(const x, y, z, w : Single): Integer;
begin
   Result:=Add(VectorMake(x, y, z, w));
end;

// Add (3 affine)
//
procedure TVectorList.Add(const i1, i2, i3 : TAffineVector; w : Single);
begin
  	Inc(FCount, 3);
   while FCount>FCapacity do SetCapacity(FCapacity + FGrowthDelta);
	PAffineVector(@FList[FCount-3])^:=i1;   FList[FCount-3][3]:=w;
	PAffineVector(@FList[FCount-2])^:=i2;   FList[FCount-2][3]:=w;
	PAffineVector(@FList[FCount-1])^:=i3;   FList[FCount-1][3]:=w;
end;

// Get
//
function TVectorList.Get(Index: Integer): TVector;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList^[Index];
end;

// Insert
//
procedure TVectorList.Insert(Index: Integer; const Item: TVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount - Index) * SizeOf(TVector));
	FList^[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TVectorList.Put(Index: Integer; const Item: TVector);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList^[Index] := Item;
end;

// SetCapacity
//
procedure TVectorList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PVectorArray(FBaseList);
end;

// Push
//
procedure TVectorList.Push(const val : TVector);
begin
	Add(val);
end;

// Pop
//
function TVectorList.Pop : TVector;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullHmgVector;
end;

// Lerp
//
procedure TVectorList.Lerp(list1, list2 : TBaseVectorList; lerpFactor : Single);
begin
   if (list1 is TVectorList) and (list2 is TVectorList) then begin
      Assert(list1.Count=list2.Count);
      Capacity:=list1.Count;
      FCount:=list1.Count;
      VectorArrayLerp(TVectorList(list1).List, TVectorList(list2).List,
                      lerpFactor, FCount, List);
   end else inherited;
end;

// ------------------
// ------------------ TTexPointList ------------------
// ------------------

// Create
//
constructor TTexPointList.Create;
begin
   FItemSize:=SizeOf(TTexPoint);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TTexPointList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TTexPointList) then
			System.Move(TTexPointList(Src).FList^, FList^, FCount*SizeOf(TTexPoint));
	end else Clear;
end;

// Add
//
function TTexPointList.Add(const item : TTexPoint): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
  	Inc(FCount);
end;

// Get
//
function TTexPointList.Get(Index: Integer): TTexPoint;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList^[Index];
end;

// Insert
//
procedure TTexPointList.Insert(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount - Index) * SizeOf(TTexPoint));
	FList^[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TTexPointList.Put(Index: Integer; const Item: TTexPoint);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList^[Index] := Item;
end;

// SetCapacity
//
procedure TTexPointList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PTexPointArray(FBaseList);
end;

// Push
//
procedure TTexPointList.Push(const val : TTexPoint);
begin
	Add(val);
end;

// Pop
//
function TTexPointList.Pop : TTexPoint;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=NullTexPoint;
end;

// ------------------
// ------------------ TIntegerList ------------------
// ------------------

// Create
//
constructor TIntegerList.Create;
begin
   FItemSize:=SizeOf(Integer);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TIntegerList.Assign(Src: TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TIntegerList) then
			System.Move(TIntegerList(Src).FList^, FList^, FCount*SizeOf(Integer));
	end else Clear;
end;

// Add
//
function TIntegerList.Add(const item : Integer): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
  	Inc(FCount);
end;

// Get
//
function TIntegerList.Get(Index: Integer): Integer;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList^[Index];
end;

// Insert
//
procedure TIntegerList.Insert(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount = FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	if Index < FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount - Index) * SizeOf(Integer));
	FList^[Index] := Item;
	Inc(FCount);
end;

// Put
//
procedure TIntegerList.Put(Index: Integer; const Item: Integer);
begin
{$IFOPT R+}
   Assert(Cardinal(Index) < Cardinal(FCount));
{$ENDIF}
	FList^[Index] := Item;
end;

// SetCapacity
//
procedure TIntegerList.SetCapacity(NewCapacity: Integer);
begin
   inherited;
   FList:=PIntegerArray(FBaseList);
end;

// Push
//
procedure TIntegerList.Push(const val : Integer);
begin
	Add(val);
end;

// Pop
//
function TIntegerList.Pop : Integer;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=0;
end;

// ------------------
// ------------------ TSingleList ------------------
// ------------------

// Create
//
constructor TSingleList.Create;
begin
   FItemSize:=SizeOf(Single);
	inherited Create;
	FGrowthDelta:=cDefaultListGrowthDelta;
end;

// Assign
//
procedure TSingleList.Assign(Src : TPersistent);
begin
	if Assigned(Src) then begin
      inherited;
		if (Src is TSingleList) then
			System.Move(TSingleList(Src).FList^, FList^, FCount*SizeOf(Single));
	end else Clear;
end;

// Add
//
function TSingleList.Add(const item : Single): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then SetCapacity(FCapacity + FGrowthDelta);
	FList^[Result] := Item;
  	Inc(FCount);
end;

// Get
//
function TSingleList.Get(Index : Integer) : Single;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$endif}
	Result := FList^[Index];
end;

// Insert
//
procedure TSingleList.Insert(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	if FCount=FCapacity then SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList^[Index], FList^[Index + 1],
						(FCount-Index)*SizeOf(Single));
	FList^[Index]:=Item;
	Inc(FCount);
end;

// Put
//
procedure TSingleList.Put(Index : Integer; const Item : Single);
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
	FList^[Index] := Item;
end;

// SetCapacity
//
procedure TSingleList.SetCapacity(NewCapacity : Integer);
begin
   inherited;
   FList:=PSingleArray(FBaseList);
end;

// Push
//
procedure TSingleList.Push(const val : Single);
begin
	Add(val);
end;

// Pop
//
function TSingleList.Pop : Single;
begin
	if FCount>0 then begin
		Result:=Get(FCount-1);
		Delete(FCount-1);
	end else Result:=0;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


end.

