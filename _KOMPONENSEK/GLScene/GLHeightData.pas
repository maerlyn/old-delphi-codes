// GLHeightData
{: Classes for height data access.<p>

   The components and classes in the unit are the core data providers for
   height-based objects (terrain rendering mainly), they are independant
   from the rendering stage.<p>

   In short: access to raw height data is performed by a THeightDataSource
   subclass, that must take care of performing all necessary data access,
   cacheing and manipulation to provide THeightData objects. A THeightData
   is basicly a square, poxer of two dimensionned raster heightfield, and
   holds the data a renderer needs.<p>

	<b>History : </b><font size=-1><ul>
      <li>04/03/01 - Egg - Added InterpolatedHeight
	   <li>11/02/01 - Egg - Creation
	</ul></font>
}
unit GLHeightData;

interface

uses Windows, Classes, Graphics, Geometry;

type

   TByteArray = array [0..MaxInt shr 1] of Byte;
   PByteArray = ^TByteArray;
   TByteRaster = array [0..MaxInt shr 3] of PByteArray;
   PByteRaster = ^TByteRaster;
   TWordArray = array [0..MaxInt shr 2] of Word;
   PWordArray = ^TWordArray;
   TWordRaster = array [0..MaxInt shr 3] of PWordArray;
   PWordRaster = ^TWordRaster;
   TSingleArray = array [0..MaxInt shr 3] of Single;
   PSingleArray = ^TSingleArray;
   TSingleRaster = array [0..MaxInt shr 3] of PSingleArray;
   PSingleRaster = ^TSingleRaster;

   THeightData = class;
   THeightDataClass = class of THeightData;

   // THeightDataType
   //
   {: Determines the type of data stored in a THeightData.<p>
      There are 3 data types (8 bits, 16 bits and 32 bits) possible, the 8 and
      16 bits types are unsigned, the 32 bits type (single) is signed. }
   THeightDataType = (hdtByte, hdtWord, hdtSingle);

	// THeightDataSource
	//
   {: Base class for height datasources.<p>
      This class is abstract and presents the standard interfaces for height
      data retrieval (THeightData objects). The class offers the following
      features (that a subclass may decide to implement or not, what follow
      is the complete feature set, check subclass doc to see what is actually
      supported):<ul>
      <li>Pooling / Cacheing (return a THeightData with its "Release" method)
      <li>Pre-loading : specify a list of THeightData you want to preload
      <li>Multi-threaded preload/queueing : specified list can be loaded in
         a background task.
      </p> }
	THeightDataSource = class (TComponent)
	   private
	      { Private Declarations }
         FData : TThreadList; // stores all THeightData, whatever their state/type
         FThread : TThread;   // queue manager
         FMaxThreads : Integer;
         FMaxPoolSize : Integer;
         FHeightDataClass : THeightDataClass;

	   protected
	      { Protected Declarations }
         procedure SetMaxThreads(const val : Integer);

         {: Adjust this property in you subclasses. }
         property HeightDataClass : THeightDataClass read FHeightDataClass write FHeightDataClass;

         {: Access to currently pooled THeightData objects. }
         property Data : TThreadList read FData;

         {: Looks up the list and returns the matching THeightData, if any. }
         function FindMatchInList(xLeft, yTop, size : Integer;
                                  dataType : THeightDataType) : THeightData;

         {: Request to start preparing data.<p>
            If your subclass is thread-enabled, this is here that you'll create
            your thread and fire it (don't forget the requirements), if not,
            that'll be here you'll be doing your work.<br>
            Either way, you are responsible for adjusting the DataState to
            hdsReady when you're done (DataState will be hdsPreparing when this
            method will be invoked). }
         procedure StartPreparingData(heightData : THeightData); virtual;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         {: Empties the Data list, terminating thread if necessary.<p>
            If some THeightData are hdsInUse, triggers an exception and does
            nothing. }
         procedure Clear;
         {: Removes less used TDataHeight objects from the pool.<p>
            Only removes objects whose state is hdsReady and UseCounter is zero,
            starting from the end of the list untill total data size gets below
            MaxPoolSize (or nothing can be removed). }
         procedure CleanUp;

         {: Base THeightData requester method.<p>
            Returns (by rebuilding it or from the cache) a THeightData
            corresponding to the given area. Size must be a power of two.<p>
            Subclasses may choose to publish it or just publish datasource-
            specific requester method using specific parameters. }
         function GetData(xLeft, yTop, size : Integer;
                          dataType : THeightDataType) : THeightData; virtual;
         {: Preloading request.<p>
            See GetData for details. }
         function PreLoad(xLeft, yTop, size : Integer;
                          dataType : THeightDataType) : THeightData; virtual;
         {: Notification that the data is no longer used by the renderer.<p>
            Default behaviour is just to change DataState to hdsReady (ie. return
            the data to the pool) }
         procedure Release(aHeightData : THeightData);

         {: Maximum number of background threads.<p>
            If 0 (zero), multithreading is disabled and StartPreparingData
            will be called from the mainthread, and all preload requirements
            (queued THeightData objects) will be loaded in sequence from
            the main thread.<p>
            If 1, basic multithreading and queueing gets enabled,
            ie. StartPreparingData will be called from a thread, but from one
            thread only (ie. there is no need to implement a THeightDataThread,
            just make sure StartPreparingData code is thread-safe).<p>
            Other values (2 and more) are relevant only if you implement
            a THeightDataThread subclass and fire it in StartPreparingData. }
         property MaxThreads : Integer read FMaxThreads write SetMaxThreads;
         {: Maximum size of TDataHeight pool in bytes.<p>
            The pool (cache) can actually get larger if more data than the pool
            can accomodate is used, but as soon as data gets released and returns
            to the pool, TDataHeight will be freed untill total pool size gets
            below this figure.<br>
            The pool manager frees TDataHeight objects who haven't been requested
            for the longest time first.<p>
            The default value of zero effectively disables pooling. }
         property MaxPoolSize : Integer read FMaxPoolSize write FMaxPoolSize;

         {: Interpolates height for the given point. } 
         function InterpolatedHeight(x, y : Single) : Single; virtual;
	end;

   // THeightDataState
   //
   {: Possible states for a THeightData.<p>
      <ul>
      <li>hdsQueued : the data has been queued for loading
      <li>hdsPreparing : the data is currently loading or being prepared for use
      <li>hdsReady : the data is fully loaded and ready for use
      </ul> }
   THeightDataState = ( hdsQueued, hdsPreparing, hdsReady);

   THeightDataThread = class;

	// THeightData
	//
   {: Base class for height data, stores a height-field raster.<p>
      The raster is a square, whose size must be a power of two. Data can be
      accessed through a base pointer ("ByteData[n]" f.i.), or through pointer
      indirections ("ByteRaster[y][x]" f.i.), this are the fastest way to access
      height data (and the most unsecure).<br>
      Secure (with range checking) data access is provided by specialized
      methods (f.i. "ByteHeight"), in which coordinates (x & y) are always
      considered relative (like in raster access).<p>
      The class offers conversion facility between the types (as a whole data
      conversion), but in any case, the THeightData should be directly requested
      from the THeightDataSource with the appropriate format.<p>
      Though this class can be instantiated, you will usually prefer to subclass
      it in real-world cases, f.i. to add texturing data. }
	THeightData = class (TObject)
	   private
	      { Private Declarations }
         FOwner : THeightDataSource;
         FDataState : THeightDataState;
         FSize : Integer;
         FXLeft, FYTop : Integer;
         FUseCounter : Integer;
         FDataType : THeightDataType;
         FByteData : PByteArray;
         FByteRaster : PByteRaster;
         FWordData : PWordArray;
         FWordRaster : PWordRaster;
         FSingleData : PSingleArray;
         FSingleRaster : PSingleRaster;
         FObjectTag : TObject;
         FTag, FTag2 : Integer;
         FOnDestroy : TNotifyEvent;

         procedure BuildByteRaster;
         procedure BuildWordRaster;
         procedure BuildSingleRaster;

         procedure ConvertByteToWord;
         procedure ConvertByteToSingle;
         procedure ConvertWordToByte;
         procedure ConvertWordToSingle;
         procedure ConvertSingleToByte;
         procedure ConvertSingleToWord;

	   protected
	      { Protected Declarations }
         FThread : THeightDataThread; // thread used for multi-threaded processing (if any)

         procedure SetDataType(const val : THeightDataType);

	   public
	      { Public Declarations }
	      constructor Create(aOwner : THeightDataSource;
                            aXLeft, aYTop, aSize : Integer;
                            aDataType : THeightDataType); virtual;
         destructor Destroy; override;

         {: The component who created and maintains this data. }
         property Owner : THeightDataSource read FOwner;

         {: Fired when the object is destroyed. }
         property OnDestroy : TNotifyEvent read FOnDestroy write FOnDestroy;

         {: Counter for use registration.<p>
            A THeightData is not returned to the pool untill this counter reaches
            a value of zero. }
         property UseCounter : Integer read FUseCounter;
         {: Increments UseCounter.<p> }
         procedure RegisterUse;
         {: Decrements UseCounter.<p>
            When the counter reaches zero, notifies the Owner THeightDataSource
            that the data is no longer used.<p>
            The renderer should call Release when it no longer needs a THeighData,
            and never free/destroy the object directly. }
         procedure Release;

         {: World X coordinate of top left point. }
         property XLeft : Integer read FXLeft;
         {: World Y coordinate of top left point. }
         property YTop : Integer read FYTop;
         {: Type of the data.<p>
            Assigning a new datatype will result in the data being converted. }
         property DataType : THeightDataType read FDataType write SetDataType;
         {: Current state of the data. }
         property DataState : THeightDataState read FDataState write FDataState;
         {: Size of the data square, in data units. }
         property Size : Integer read FSize;

         {: Memory size of the raw data in bytes. }
         function DataSize : Integer;

         {: Access to data as a byte array (n = y*Size+x).<p>
            If THeightData is not of type hdtByte, this value is nil. }
         property ByteData : PByteArray read FByteData;
         {: Access to data as a byte raster (y, x).<p>
            If THeightData is not of type hdtByte, this value is nil. }
         property ByteRaster : PByteRaster read FByteRaster;
         {: Access to data as a Word array (n = y*Size+x).<p>
            If THeightData is not of type hdtWord, this value is nil. }
         property WordData : PWordArray read FWordData;
         {: Access to data as a Word raster (y, x).<p>
            If THeightData is not of type hdtWord, this value is nil. }
         property WordRaster : PWordRaster read FWordRaster;
         {: Access to data as a Single array (n = y*Size+x).<p>
            If THeightData is not of type hdtSingle, this value is nil. }
         property SingleData : PSingleArray read FSingleData;
         {: Access to data as a Single raster (y, x).<p>
            If THeightData is not of type hdtSingle, this value is nil. }
         property SingleRaster : PSingleRaster read FSingleRaster;

         {: Height of point x, y as a Byte.<p> }
	      function ByteHeight(x, y : Integer) : Byte;
         {: Height of point x, y as a Word.<p> }
	      function WordHeight(x, y : Integer) : Word;
         {: Height of point x, y as a Single.<p> }
	      function SingleHeight(x, y : Integer) : Single;
         {: Interopolated height of point x, y as a Single.<p> }
         function InterpolatedHeight(x, y : Single) : Single;

         {: Returns the height as a single, whatever the DataType (slow). }
	      function Height(x, y : Integer) : Single;

         {: Calculates and returns the normal for point x, y.<p>
            Sub classes may provide normal cacheing, the default implementation
            being rather blunt. }
         function Normal(x, y : Integer; const scale : TAffineVector) : TAffineVector; virtual;

         {: Reserved for renderer use. }
         property ObjectTag : TObject read FObjectTag write FObjectTag;
         {: Reserved for renderer use. }
         property Tag : Integer read FTag write FTag;
         {: Reserved for renderer use. }
         property Tag2 : Integer read FTag2 write FTag2;
	end;

   // THeightDataThread
   //
   {: A thread specialized for processing THeightData in background.<p>
      Requirements:<ul>
      <li>must have FreeOnTerminate set to true,
      <li>must check and honour Terminated swiftly
      </ul> }
   THeightDataThread = class (TThread)
      protected
	      { Protected Declarations }
         FHeightData : THeightData;

      public
	      { Public Declarations }
         destructor Destroy; override;
   end;

	// TGLBitmapHDS
	//
   {: Bitmap-based Height Data Source.<p>
      The image is automatically wrapped if requested data is out of picture size,
      or if requested data is larger than the picture.<p>
      The internal format is an 8 bit bitmap whose dimensions are a power of two,
      if the original image does not comply, it is StretchDraw'ed on a monochrome
      (gray) bitmap. }
	TGLBitmapHDS = class (THeightDataSource)
	   private
	      { Private Declarations }
         FBitmap : TBitmap;
         FPicture : TPicture;

	   protected
	      { Protected Declarations }
         procedure SetPicture(const val : TPicture);
         procedure OnPictureChanged(Sender : TObject);

         procedure CreateMonochromeBitmap(size : Integer);
         procedure FreeMonochromeBitmap;

         procedure StartPreparingData(heightData : THeightData); override;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	   published
	      { Published Declarations }
         {: The picture serving as Height field data reference.<p>
            The picture is (if not already) internally converted to a 8 bit
            bitmap (grayscale). For better performance and to save memory,
            feed it this format! }
         property Picture : TPicture read FPicture write SetPicture;

         property MaxPoolSize;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, GLMisc;

// ------------------
// ------------------ THeightDataSourceThread ------------------
// ------------------

type
   THeightDataSourceThread = class (TThread)
      FOwner : THeightDataSource;
      FIdleLoops : Integer;
      procedure Execute; override;
   end;

// Execute
//
procedure THeightDataSourceThread.Execute;
var
   i, n : Integer;
   isIdle : Boolean;
begin
   while not Terminated do begin
      isIdle:=True;
      if FOwner.MaxThreads>0 then begin
         // current estimated nb threads running
         n:=0;
         with FOwner.FData.LockList do begin
            try
               for i:=0 to Count-1 do
                  if THeightData(Items[i]).FThread<>nil then Inc(n);
               i:=0;
               while (n<FOwner.MaxThreads) and (i<Count) do begin
                  if THeightData(Items[i]).DataState=hdsQueued then begin
                     FOwner.StartPreparingData(THeightData(Items[i]));
                     isIdle:=False;
                     Inc(n);
                  end;
                  Inc(i);
               end;
            finally
               FOwner.FData.UnlockList;
            end;
         end;
      end;
      if isIdle then
         Inc(FIdleLoops)
      else FIdleLoops:=0;
      if FIdleLoops<10 then
         Sleep(0) // "fast" mode
      else Sleep(10); // "doze" mode
   end;
end;

// ------------------
// ------------------ THeightDataSource ------------------
// ------------------

// Create
//
constructor THeightDataSource.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FHeightDataClass:=THeightData;
   FData:=TThreadList.Create;
   FThread:=THeightDataSourceThread.Create(True);
   FThread.FreeOnTerminate:=False;
   THeightDataSourceThread(FThread).FOwner:=Self;
   FThread.Resume;
end;

// Destroy
//
destructor THeightDataSource.Destroy;
begin
   FThread.Terminate;
   FThread.WaitFor;
   FThread.Free;
   Clear;
   FData.Free;
	inherited Destroy;
end;

// Clear
//
procedure THeightDataSource.Clear;
var
   i : Integer;
begin
   with FData.LockList do begin
      try
         for i:=0 to Count-1 do
            if THeightData(Items[i]).UseCounter>0 then
               raise Exception.Create('ERR: HeightData still in use');
         for i:=0 to Count-1 do begin
            THeightData(Items[i]).FOwner:=nil;
            THeightData(Items[i]).Free;
         end;
         Clear;
      finally
         FData.UnlockList;
      end;
   end;
end;

// FindMatchInList
//
function THeightDataSource.FindMatchInList(xLeft, yTop, size : Integer;
                                           dataType : THeightDataType) : THeightData;
var
   i : Integer;
   hd : THeightData;
begin
   Result:=nil;
   with FData.LockList do begin
      try
         for i:=0 to Count-1 do begin
            hd:=THeightData(Items[i]);
            if (hd.XLeft=xLeft) and (hd.YTop=yTop) and (hd.Size=size) and (hd.DataType=dataType) then begin
               Result:=hd;
               Break;               
            end;
         end;
      finally
         FData.UnlockList;
      end;
   end;
end;

// GetData
//
function THeightDataSource.GetData(xLeft, yTop, size : Integer;
                                   dataType : THeightDataType) : THeightData;
begin
   Result:=FindMatchInList(xLeft, yTop, size, dataType);
   if not Assigned(Result) then
      Result:=PreLoad(xLeft, yTop, size, dataType)
   else with FData.LockList do begin
      try
         Move(IndexOf(Result), 0);
      finally
         FData.UnlockList;
      end;
   end;
   // got one... can be used ?
   while Result.DataState<>hdsReady do Sleep(0);
   Result.RegisterUse;
end;

// PreLoad
//
function THeightDataSource.PreLoad(xLeft, yTop, size : Integer;
                  dataType : THeightDataType) : THeightData;
begin
   CleanUp;
   Result:=HeightDataClass.Create(Self, xLeft, yTop, size, dataType);
   FData.LockList.Insert(0, Result);
   FData.UnlockList;
   if MaxThreads=0 then
      StartPreparingData(Result);
end;

// Release
//
procedure THeightDataSource.Release(aHeightData : THeightData);
begin
   CleanUp;
end;

// CleanUp
//
procedure THeightDataSource.CleanUp;
var
   i : Integer;
   usedMemory : Integer;
begin
   with FData.LockList do begin
      try
         usedMemory:=0;
         for i:=0 to Count-1 do
            usedMemory:=usedMemory+THeightData(Items[i]).DataSize;
         if usedMemory>MaxPoolSize then begin
            for i:=Count-1 downto 0 do with THeightData(Items[i]) do begin
               if (DataState=hdsReady) and (UseCounter=0) then begin
                  usedMemory:=usedMemory-DataSize;
                  FOwner:=nil;
                  Free;
                  Delete(i);
                  if usedMemory<=MaxPoolSize then Break;
               end;
            end;
         end;
      finally
         FData.UnlockList;
      end;
   end;
end;

// SetMaxThreads
//
procedure THeightDataSource.SetMaxThreads(const val : Integer);
begin
   if val<0 then
      FMaxThreads:=0
   else FMaxThreads:=val;
end;

// StartPreparingData
//
procedure THeightDataSource.StartPreparingData(heightData : THeightData);
begin
   heightData.FDataState:=hdsReady;
end;

// InterpolatedHeight
//
function THeightDataSource.InterpolatedHeight(x, y : Single) : Single;
var
   i : Integer;
   hd, foundHd : THeightData;
begin
   with FData.LockList do begin
      try
         // first, lookup data list to find if aHeightData contains our point
         foundHd:=nil;
         for i:=0 to Count-1 do begin
            hd:=THeightData(Items[i]);
            if (hd.XLeft<=x) and (hd.YTop<=y)
                  and (hd.XLeft+hd.Size-1>x) and (hd.YTop+hd.Size-1>y) then begin
               foundHd:=hd;
               Break;
            end;
         end;
      finally
         FData.UnlockList;
      end;
   end;
   if foundHd=nil then begin
      // not found, request one... slowest mode (should be avoided)
      foundHd:=GetData(Trunc(x)-1, Trunc(y)-1, 4, hdtSingle);
   end else begin
      // request it using "standard" way (takes care of threads)
      foundHd:=GetData(foundHd.XLeft, foundHd.YTop, foundHd.Size, foundHd.DataType);
   end;
   try
      Result:=foundHd.InterpolatedHeight(x-foundHd.XLeft, y-foundHd.YTop);
   finally
      foundHd.Release;
   end;
end;

// ------------------
// ------------------ THeightData ------------------
// ------------------

// Create
//
constructor THeightData.Create(aOwner : THeightDataSource;
                               aXLeft, aYTop, aSize : Integer;
                               aDataType : THeightDataType);
begin
	inherited Create;
   FOwner:=aOwner;
   FXLeft:=aXLeft;
   FYTop:=aYTop;
   FSize:=aSize;
   FDataType:=aDataType;
   FDataState:=hdsQueued;
   case DataType of
      hdtByte : begin
         FByteData:=AllocMem(Size*Size*SizeOf(Byte));
         BuildByteRaster;
      end;
      hdtWord : begin
         FWordData:=AllocMem(Size*Size*SizeOf(Word));
         BuildWordRaster;
      end;
      hdtSingle : begin
         FSingleData:=AllocMem(Size*Size*SizeOf(Single));
         BuildSingleRaster;
      end;
   else
      Assert(False);
   end;
end;

// Destroy
//
destructor THeightData.Destroy;
begin
   Assert(not Assigned(FOwner), 'You should *not* free a THeightData, use "Release" instead');
   if Assigned(FThread) then begin
      FThread.Terminate;
      FThread.WaitFor;
   end;
   if Assigned(FOnDestroy) then
      FOnDestroy(Self);   
   case DataType of
      hdtByte : begin
         FreeMem(FByteData);
         FreeMem(FByteRaster);
      end;
      hdtWord : begin
         FreeMem(FWordData);
         FreeMem(FWordRaster);
      end;
      hdtSingle : begin
         FreeMem(FSingleData);
         FreeMem(FSingleRaster);
      end;
   else
      Assert(False);
   end;
	inherited Destroy;
end;

// RegisterUse
//
procedure THeightData.RegisterUse;
begin
   Inc(FUseCounter);
end;

// Release
//
procedure THeightData.Release;
begin
   Dec(FUseCounter);
   Assert(FUseCounter>=0);
   if FUseCounter=0 then
      Owner.Release(Self);
end;

// SetDataType
//
procedure THeightData.SetDataType(const val : THeightDataType);
begin
   if val<>FDataType then begin
      case FDataType of
         hdtByte : case val of
               hdtWord : ConvertByteToWord;
               hdtSingle : ConvertByteToSingle;
            else
               Assert(False);
            end;
         hdtWord : case val of
               hdtByte : ConvertWordToByte;
               hdtSingle : ConvertWordToSingle;
            else
               Assert(False);
            end;
         hdtSingle : case val of
               hdtByte : ConvertSingleToByte;
               hdtWord : ConvertSingleToWord;
            else
               Assert(False);
            end;
      else
         Assert(False);
      end;
      FDataType:=val;
   end;
end;

// DataSize
//
function THeightData.DataSize : Integer;
begin
   case DataType of
      hdtByte : Result:=Size*Size*SizeOf(Byte);
      hdtWord : Result:=Size*Size*SizeOf(Word);
      hdtSingle : Result:=Size*Size*SizeOf(Single);
   else
      Result:=0;
      Assert(False);
   end;
end;

// BuildByteRaster
//
procedure THeightData.BuildByteRaster;
var
   i : Integer;
begin
   FByteRaster:=AllocMem(Size*SizeOf(PByteArray));
   for i:=0 to Size-1 do
      FByteRaster[i]:=@FByteData[i*Size]
end;

// BuildWordRaster
//
procedure THeightData.BuildWordRaster;
var
   i : Integer;
begin
   FWordRaster:=AllocMem(Size*SizeOf(PWordArray));
   for i:=0 to Size-1 do
      FWordRaster[i]:=@FWordData[i*Size]
end;

// BuildSingleRaster
//
procedure THeightData.BuildSingleRaster;
var
   i : Integer;
begin
   FSingleRaster:=AllocMem(Size*SizeOf(PSingleArray));
   for i:=0 to Size-1 do
      FSingleRaster[i]:=@FSingleData[i*Size]
end;

// ConvertByteToWord
//
procedure THeightData.ConvertByteToWord;
var
   i : Integer;
begin
   FreeMem(FByteRaster);
   FByteRaster:=nil;
   FWordData:=AllocMem(Size*Size*SizeOf(Word));
   for i:=0 to Size*Size-1 do
      FWordData[i]:=FByteData[i];
   FreeMem(FByteData);
   FByteData:=nil;
   BuildWordRaster;
end;

// ConvertByteToSingle
//
procedure THeightData.ConvertByteToSingle;
var
   i : Integer;
begin
   FreeMem(FByteRaster);
   FByteRaster:=nil;
   FSingleData:=AllocMem(Size*Size*SizeOf(Single));
   for i:=0 to Size*Size-1 do
      FSingleData[i]:=FByteData[i];
   FreeMem(FByteData);
   FByteData:=nil;
   BuildSingleRaster;
end;

// ConvertWordToByte
//
procedure THeightData.ConvertWordToByte;
var
   i : Integer;
begin
   FreeMem(FWordRaster);
   FWordRaster:=nil;
   FByteData:=Pointer(FWordData);
   for i:=0 to Size*Size-1 do
      FByteData[i]:=FWordData[i];
   ReallocMem(FByteData, Size*Size*SizeOf(Byte));
   FWordData:=nil;
   BuildByteRaster;
end;

// ConvertWordToSingle
//
procedure THeightData.ConvertWordToSingle;
var
   i : Integer;
begin
   FreeMem(FWordRaster);
   FWordRaster:=nil;
   FSingleData:=AllocMem(Size*Size*SizeOf(Single));
   for i:=0 to Size*Size-1 do
      FSingleData[i]:=FWordData[i];
   FreeMem(FWordData);
   FWordData:=nil;
   BuildSingleRaster;
end;

// ConvertSingleToByte
//
procedure THeightData.ConvertSingleToByte;
var
   i : Integer;
begin
   FreeMem(FSingleRaster);
   FSingleRaster:=nil;
   FByteData:=Pointer(FSingleData);
   for i:=0 to Size*Size-1 do
      FByteData[i]:=Round(FSingleData[i]);
   ReallocMem(FByteData, Size*Size*SizeOf(Byte));
   FSingleData:=nil;
   BuildByteRaster;
end;

// ConvertSingleToWord
//
procedure THeightData.ConvertSingleToWord;
var
   i : Integer;
begin
   FreeMem(FSingleRaster);
   FSingleRaster:=nil;
   FWordData:=Pointer(FSingleData);
   for i:=0 to Size*Size-1 do
      FWordData[i]:=Round(FSingleData[i]);
   ReallocMem(FWordData, Size*Size*SizeOf(Word));
   FSingleData:=nil;
   BuildWordRaster;
end;

// ByteHeight
//
function THeightData.ByteHeight(x, y : Integer) : Byte;
begin
   Assert((Cardinal(x)<Cardinal(Size)) and (Cardinal(y)<Cardinal(Size)));
	Result:=ByteRaster[y][x];
end;

// WordHeight
//
function THeightData.WordHeight(x, y : Integer) : Word;
begin
   Assert((Cardinal(x)<Cardinal(Size)) and (Cardinal(y)<Cardinal(Size)));
	Result:=WordRaster[y][x];
end;

// SingleHeight
//
function THeightData.SingleHeight(x, y : Integer) : Single;
begin
   Assert((Cardinal(x)<Cardinal(Size)) and (Cardinal(y)<Cardinal(Size)));
	Result:=SingleRaster[y][x];
end;

// InterpolatedHeight
//
function THeightData.InterpolatedHeight(x, y : Single) : Single;
var
   ix, iy : Integer;
   h1, h2, h3 : Single;
begin
   ix:=Trunc(x);  x:=Frac(x);
   iy:=Trunc(y);  y:=Frac(y);
   if x+y<=1 then begin
      // top-left triangle
      h1:=Height(ix,    iy);
      h2:=Height(ix+1,  iy);
      h3:=Height(ix,    iy+1);
      Result:=h1+(h2-h1)*x+(h3-h1)*y;
   end else begin
      // bottom-right triangle
      h1:=Height(ix+1,  iy+1);
      h2:=Height(ix,    iy+1);
      h3:=Height(ix+1,  iy);
      Result:=h1+(h2-h1)*(1-x)+(h3-h1)*(1-y);
   end;
end;

// Height
//
function THeightData.Height(x, y : Integer) : Single;
begin
   case DataType of
      hdtByte : Result:=ByteHeight(x, y);
      hdtWord : Result:=WordHeight(x, y);
      hdtSingle : Result:=SingleHeight(x, y);
   else
      Result:=0;
      Assert(False);
   end;
end;

// Normal
//
function THeightData.Normal(x, y : Integer; const scale : TAffineVector) : TAffineVector;
var
   dx, dy : Single;
begin
   if x>0 then
      if x<Size-1 then
         dx:=(Height(x+1, y)-Height(x-1, y))*scale[0]*scale[2]
      else dx:=(Height(x, y)-Height(x-1, y))*scale[0]*scale[2]
   else dx:=(Height(x+1, y)-Height(x, y))*scale[0]*scale[2];
   if y>0 then
      if y<Size-1 then
         dy:=(Height(x, y+1)-Height(x, y-1))*scale[1]*scale[2]
      else dy:=(Height(x, y)-Height(x, y-1))*scale[1]*scale[2]
   else dy:=(Height(x, y+1)-Height(x, y))*scale[1]*scale[2];
   Result[0]:=dx;
   Result[1]:=dy;
   Result[2]:=1;
   NormalizeVector(Result);
end;

// ------------------
// ------------------ THeightDataThread ------------------
// ------------------

// Destroy
//
destructor THeightDataThread.Destroy;
begin
   if Assigned(FHeightData) then
      FHeightData.FThread:=nil;
   inherited;
end;

// ------------------
// ------------------ TGLBitmapHDS ------------------
// ------------------

// Create
//
constructor TGLBitmapHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FPicture:=TPicture.Create;
   FPicture.OnChange:=OnPictureChanged;
end;

// Destroy
//
destructor TGLBitmapHDS.Destroy;
begin
   FreeMonochromeBitmap;
   FPicture.Free;
	inherited Destroy;
end;

// SetPicture
//
procedure TGLBitmapHDS.SetPicture(const val : TPicture);
begin
   FPicture.Assign(val);
end;

// OnPictureChanged
//
procedure TGLBitmapHDS.OnPictureChanged(Sender : TObject);
var
   oldPoolSize, size : Integer;
begin
   // cleanup pool
   oldPoolSize:=MaxPoolSize;
   MaxPoolSize:=0;
   CleanUp;
   MaxPoolSize:=oldPoolSize;
   // prepare MonoChromeBitmap
   FreeMonochromeBitmap;
   size:=Picture.Width;
   if size>0 then
      CreateMonochromeBitmap(size);
end;

// CreateMonochromeBitmap
//
procedure TGLBitmapHDS.CreateMonochromeBitmap(size : Integer);
type
   TLogPal = record
      lpal : TLogPalette;
      pe : Array[0..255] of TPaletteEntry;
   end;
var
   x : Integer;
   logpal : TLogPal;
begin
   size:=RoundUpToPowerOf2(size);
   FBitmap:=TBitmap.Create;
   FBitmap.PixelFormat:=pf8bit;
   FBitmap.Width:=size;
   FBitmap.Height:=size;
   for x:=0 to 255 do with logPal.lpal.palPalEntry[x] do begin
      peRed:=x;
      peGreen:=x;
      peBlue:=x;
      peFlags:=0;
   end;
   with logpal.lpal do begin
      palVersion:=$300;
      palNumEntries:=256;
   end;
   FBitmap.Palette:=CreatePalette(logPal.lpal);
   FBitmap.Canvas.StretchDraw(Rect(0, 0, size, size), Picture.Graphic);
end;

// FreeMonochromeBitmap
//
procedure TGLBitmapHDS.FreeMonochromeBitmap;
begin
   FBitmap.Free;
   FBitmap:=nil;
end;

// StartPreparingData
//
procedure TGLBitmapHDS.StartPreparingData(heightData : THeightData);
var
   y, x : Integer;
   bmpSize, wrapMask : Integer;
   bitmapLine, rasterLine : PByteArray;
   oldType : THeightDataType;
   b : Byte;
begin
   if FBitmap=nil then Exit;
   bmpSize:=FBitmap.Width;
   wrapMask:=bmpSize-1;
   // retrieve data
   with heightData do begin
      oldType:=DataType;
      DataType:=hdtByte;
      for y:=YTop to YTop+Size-1 do begin
         bitmapLine:=FBitmap.ScanLine[y and wrapMask];
         rasterLine:=ByteRaster[y-YTop];
         // *BIG CAUTION HERE* : Don't remove the intermediate variable here!!!
         // or Delphi compiler will "optimize" to 32 bits access with clamping
         // resulting in possible reads of stuff beyon bitmapLine length!!!! 
         for x:=XLeft to XLeft+Size-1 do begin
            b:=bitmapLine[x and wrapMask];
            rasterLine[x-XLeft]:=b;
         end;
      end;
      if oldType<>hdtByte then
         DataType:=oldType;
   end;
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TGLBitmapHDS);

end.
