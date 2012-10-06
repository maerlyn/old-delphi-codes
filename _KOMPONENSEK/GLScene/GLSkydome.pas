// GLSkydome
{: Skydome object<p>

	<b>Historique : </b><font size=-1><ul>
      <li>12/03/01 - EG - Reversed polar caps orientation
      <li>28/01/01 - EG - Fixed TSkyDomeBand rendering (vertex coordinates)
      <li>18/01/01 - EG - First working version of TEarthSkyDome
	   <li>14/01/01 - EG - Creation
	</ul></font>
}
unit GLSkydome;

interface

uses Classes, GLScene, GLMisc, GLTexture;

type

	// TSkyDomeBand
	//
	TSkyDomeBand = class (TCollectionItem)
	   private
	      { Private Declarations }
         FStartAngle : Single;
         FStopAngle : Single;
         FStartColor : TGLColor;
         FStopColor : TGLColor;
         FSlices : Integer;
         FStacks : Integer;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetStartAngle(const val : Single);
         procedure SetStartColor(const val : TGLColor);
         procedure SetStopAngle(const val : Single);
         procedure SetStopColor(const val : TGLColor);
         procedure SetSlices(const val : Integer);
         procedure SetStacks(const val : Integer);
         procedure OnColorChange(sender : TObject);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo);

	   published
	      { Published Declarations }
         property StartAngle : Single read FStartAngle write SetStartAngle;
         property StartColor : TGLColor read FStartColor write SetStartColor;
         property StopAngle : Single read FStopAngle write SetStopAngle;
         property StopColor : TGLColor read FStopColor write SetStopColor;
         property Slices : Integer read FSlices write SetSlices default 12;
         property Stacks : Integer read FStacks write SetStacks default 1;
	end;

	// TSkyDomeBands
	//
	TSkyDomeBands = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TSkyDomeBand);
	      function GetItems(index : Integer) : TSkyDomeBand;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);
         function Add: TSkyDomeBand;
	      function FindItemID(ID: Integer): TSkyDomeBand;
	      property Items[index : Integer] : TSkyDomeBand read GetItems write SetItems; default;

         procedure NotifyChange;
         procedure BuildList(var rci : TRenderContextInfo);
   end;

	// TSkyDome
	//
   {: Renders a sky dome always centered on the camera.<p>
      If you use this object make sure it is rendered *first*, as it ignores
      depth buffering and overwrites everything.<p>
      The skydome is described by "bands", each "band" is an horizontal cut
      of a sphere, and you can have as many bands as you wish.<p>
      Estimated CPU cost (K7-500, GeForce SDR, default bands):<ul>
      <li>800x600 fullscreen filled: 4.5 ms (220 FPS, worst case)
      <li>Geometry cost (0% fill): 0.7 ms (1300 FPS, best case)
      </ul> }
	TSkyDome = class (TGLImmaterialSceneObject)
	   private
	      { Private Declarations }
         FBands : TSkyDomeBands;

	   protected
	      { Protected Declarations }
         procedure SetBands(const val : TSkyDomeBands);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         procedure DoRender(var rci : TRenderContextInfo); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

	   published
	      { Published Declarations }
         property Bands : TSkyDomeBands read FBands write SetBands;
	end;

   // TEarthSkyDome
   //
   {: Render a skydome like what can be seen on earth.<p>
      Color is based on sun position and turbidity, to "mimic" atmospheric
      Rayleigh and Mie scatterings. The colors can be adjusted to render
      weird/extra-terrestrial atmospheres too.<p>
      The default slices/stacks values make for an average quality rendering,
      for a very clean rendering, use 64/64 (more is overkill in most cases).
      The complexity is quite high though, making a T&L 3D board a necessity
      for using TEarthSkyDome. }
	TEarthSkyDome = class (TSkyDome)
	   private
	      { Private Declarations }
         FSunElevation : Single;
         FTurbidity : Single;
         FCurSunColor, FCurSkyColor, FCurHazeColor : TColorVector;
         FCurHazeTurbid, FCurSunSkyTurbid : Single;
         FSunZenithColor : TGLColor;
         FSunDawnColor : TGLColor;
         FHazeColor : TGLColor;
         FSkyColor : TGLColor;
         FNightColor : TGLColor;
         FSlices, FStacks : Integer;

	   protected
	      { Protected Declarations }
         procedure SetSunElevation(const val : Single);
         procedure SetTurbidity(const val : Single);
         procedure SetSunZenithColor(const val : TGLColor);
         procedure SetSunDawnColor(const val : TGLColor);
         procedure SetHazeColor(const val : TGLColor);
         procedure SetSkyColor(const val : TGLColor);
         procedure SetNightColor(const val : TGLColor);
         procedure SetSlices(const val : Integer);
         procedure SetStacks(const val : Integer);

         procedure OnColorChanged(Sender : TObject);
         procedure PreCalculate;
         procedure RenderDome;
         function CalculateColor(const theta, cosGamma : Single) : TColorVector;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;

	   published
	      { Published Declarations }
         {: Elevation of the sun, measured in degrees. }
         property SunElevation : Single read FSunElevation write SetSunElevation;
         {: Expresses the purity of air.<p>
            Value range is from 1 (pure athmosphere) to 120 (very nebulous) }
         property Turbidity : Single read FTurbidity write SetTurbidity;

         property SunZenithColor : TGLColor read FSunZenithColor write SetSunZenithColor;
         property SunDawnColor : TGLColor read FSunDawnColor write SetSunDawnColor;
         property HazeColor : TGLColor read FHazeColor write SetHazeColor;
         property SkyColor : TGLColor read FSkyColor write SetSkyColor;
         property NightColor : TGLColor read FNightColor write SetNightColor;

         property Slices : Integer read FSlices write SetSlices default 24;
         property Stacks : Integer read FStacks write SetStacks default 48;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Geometry, OpenGL12;

// ------------------
// ------------------ TSkyDomeBand ------------------
// ------------------

// Create
//
constructor TSkyDomeBand.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FStartColor:=TGLColor.Create(Self);
   FStartColor.Initialize(clrBlue);
   FStartColor.OnNotifyChange:=OnColorChange;
   FStopColor:=TGLColor.Create(Self);
   FStopColor.Initialize(clrBlue);
   FStopColor.OnNotifyChange:=OnColorChange;
   FSlices:=12;
   FStacks:=1;
end;

// Destroy
//
destructor TSkyDomeBand.Destroy;
begin
   FStartColor.Free;
   FStopColor.Free;
	inherited Destroy;
end;

// Assign
//
procedure TSkyDomeBand.Assign(Source: TPersistent);
begin
	if Source is TSkyDomeBand then begin
      FStartAngle:=TSkyDomeBand(Source).FStartAngle;
      FStopAngle:=TSkyDomeBand(Source).FStopAngle;
      FStartColor.Assign(TSkyDomeBand(Source).FStartColor);
      FStopColor.Assign(TSkyDomeBand(Source).FStopColor);
      FSlices:=TSkyDomeBand(Source).FSlices;
      FStacks:=TSkyDomeBand(Source).FStacks;
	end;
	inherited Destroy;
end;

// GetDisplayName
//
function TSkyDomeBand.GetDisplayName : String;
begin
	Result:=Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

// SetStartAngle
//
procedure TSkyDomeBand.SetStartAngle(const val : Single);
begin
   FStartAngle:=ClampValue(val, -90, 90);
   if FStartAngle>FStopAngle then
      FStopAngle:=FStartAngle;
   TSkyDomeBands(Collection).NotifyChange;
end;

// SetStartColor
//
procedure TSkyDomeBand.SetStartColor(const val : TGLColor);
begin
   FStartColor.Assign(val);
end;

// SetStopAngle
//
procedure TSkyDomeBand.SetStopAngle(const val : Single);
begin
   FStopAngle:=ClampValue(val, -90, 90);
   if FStopAngle<FStartAngle then
      FStartAngle:=FStopAngle;
   TSkyDomeBands(Collection).NotifyChange;
end;

// SetStopColor
//
procedure TSkyDomeBand.SetStopColor(const val : TGLColor);
begin
   FStopColor.Assign(val);
end;

// SetSlices
//
procedure TSkyDomeBand.SetSlices(const val : Integer);
begin
   if val<3 then
      FSlices:=3
   else FSlices:=val;
   TSkyDomeBands(Collection).NotifyChange;
end;

// SetStacks
//
procedure TSkyDomeBand.SetStacks(const val : Integer);
begin
   if val<1 then
      FStacks:=1
   else FStacks:=val;
   TSkyDomeBands(Collection).NotifyChange;
end;

// OnColorChange
//
procedure TSkyDomeBand.OnColorChange(sender : TObject);
begin
   TSkyDomeBands(Collection).NotifyChange;
end;

// BuildList
//
procedure TSkyDomeBand.BuildList(var rci : TRenderContextInfo);

   // coordinates system note: X is forward, Y is left and Z is up
   // always rendered as sphere of radius 1

   procedure RenderBand(start, stop : Single; const colStart, colStop : TColorVector);
   var
      i : Integer;
      f, r, r2 : Single;
      vertex1, vertex2 : TVector;
   begin
      vertex1[3]:=1;
      if start=-90 then begin
         // triangle fan with south pole
         glBegin(GL_TRIANGLE_FAN);
            glColor4fv(@colStart);
            glVertex3f(0, 0, -1);
            f:=2*PI/Slices;
            SinCos(DegToRad(stop), vertex1[2], r);
            glColor4fv(@colStop);
            for i:=0 to Slices do begin
               SinCos(i*f, r, vertex1[1], vertex1[0]);
               glVertex4fv(@vertex1);
            end;
         glEnd;
      end else if stop=90 then begin
         // triangle fan with north pole
         glBegin(GL_TRIANGLE_FAN);
            glColor4fv(@colStop);
            glVertex3fv(@ZHmgPoint);
            f:=2*PI/Slices;
            SinCos(DegToRad(start), vertex1[2], r);
            glColor4fv(@colStart);
            for i:=Slices downto 0 do begin
               SinCos(i*f, r, vertex1[1], vertex1[0]);
               glVertex4fv(@vertex1);
            end;
         glEnd;
      end else begin
         vertex2[3]:=1;
         // triangle strip
         glBegin(GL_TRIANGLE_STRIP);
            f:=2*PI/Slices;
            SinCos(DegToRad(start), vertex1[2], r);
            SinCos(DegToRad(stop), vertex2[2], r2);
            for i:=0 to Slices do begin
               SinCos(i*f, r, vertex1[1], vertex1[0]);
               glColor4fv(@colStart);
               glVertex4fv(@vertex1);
               SinCos(i*f, r2, vertex2[1], vertex2[0]);
               glColor4fv(@colStop);
               glVertex4fv(@vertex2);
            end;
         glEnd;
      end;
   end;

var
   n : Integer;
   t, t2 : Single;
begin
   if StartAngle=StopAngle then Exit;
   for n:=0 to Stacks-1 do begin
      t:=n/Stacks;
      t2:=(n+1)/Stacks;
      RenderBand(Lerp(StartAngle, StopAngle, t),
                 Lerp(StartAngle, StopAngle, t2),
                 VectorLerp(StartColor.Color, StopColor.Color, t),
                 VectorLerp(StartColor.Color, StopColor.Color, t2));
   end;
end;

// ------------------
// ------------------ TSkyDomeBands ------------------
// ------------------

constructor TSkyDomeBands.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TSkyDomeBand);
end;

function TSkyDomeBands.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TSkyDomeBands.SetItems(index : Integer; const val : TSkyDomeBand);
begin
	inherited Items[index]:=val;
end;

function TSkyDomeBands.GetItems(index : Integer) : TSkyDomeBand;
begin
	Result:=TSkyDomeBand(inherited Items[index]);
end;

function TSkyDomeBands.Add: TSkyDomeBand;
begin
	Result:=(inherited Add) as TSkyDomeBand;
end;

function TSkyDomeBands.FindItemID(ID: Integer): TSkyDomeBand;
begin
	Result:=(inherited FindItemID(ID)) as TSkyDomeBand;
end;

procedure TSkyDomeBands.NotifyChange;
begin
   if Assigned(owner) and (owner is TGLBaseSceneObject) then
      TGLBaseSceneObject(owner).StructureChanged;
end;

// BuildList
//
procedure TSkyDomeBands.BuildList(var rci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TSkyDome ------------------
// ------------------

// CreateOwned
//
constructor TSkyDome.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FBands:=TSkyDomeBands.Create(Self);
   with FBands.Add do begin
      StartAngle:=0;
      StartColor.Color:=clrWhite;
      StopAngle:=15;
      StopColor.Color:=clrBlue;
   end;
   with FBands.Add do begin
      StartAngle:=15;
      StartColor.Color:=clrBlue;
      StopAngle:=90;
      Stacks:=4;
      StopColor.Color:=clrNavy;
   end;
end;

// Destroy
//
destructor TSkyDome.Destroy;
begin
   FBands.Free;
	inherited Destroy;
end;

// Assign
//
procedure TSkyDome.Assign(Source: TPersistent);
begin
   if Source is TSkyDome then begin
      FBands.Assign(TSkyDome(Source).FBands);
   end;
   inherited;
end;

// SetBands
//
procedure TSkyDome.SetBands(const val : TSkyDomeBands);
begin
   FBands.Assign(val);
   StructureChanged;
end;

// BuildList
//
procedure TSkyDome.BuildList(var rci : TRenderContextInfo);
begin
   Bands.BuildList(rci);
end;

// DoRender
//
procedure TSkyDome.DoRender(var rci : TRenderContextInfo);
var
   f : Single;
begin
   // prepare
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_LIGHTING);
   glDisable(GL_DEPTH_TEST);
   glDisable(GL_FOG);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentViewer.ModelViewMatrix);
   glTranslatef(rci.cameraPosition[0], rci.cameraPosition[1], rci.cameraPosition[2]);
   with Scene.CurrentGLCamera do
      f:=(NearPlane+DepthOfView)*0.9;
   glScalef(f, f, f);
   glMultMatrixf(@LocalMatrix);
   // render
   glCallList(GetHandle(rci));
   // restore
   glPopMatrix;
   glPopAttrib;
   // process childs
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// ------------------
// ------------------ TEarthSkyDome ------------------
// ------------------

// CreateOwned
//
constructor TEarthSkyDome.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   Bands.Clear;
   FSunElevation:=75;
   FTurbidity:=15;
   FSunZenithColor:=TGLColor.CreateInitialized(Self, clrWhite, OnColorChanged);
   FSunDawnColor:=TGLColor.CreateInitialized(Self, clrCoral, OnColorChanged);
   FHazeColor:=TGLColor.CreateInitialized(Self, clrWhite, OnColorChanged);
   FSkyColor:=TGLColor.CreateInitialized(Self, clrBlue, OnColorChanged);
   FNightColor:=TGLColor.CreateInitialized(Self, clrBlack, OnColorChanged);
   FStacks:=24;
   FSlices:=48;
   PreCalculate;
end;

// Destroy
//
destructor TEarthSkyDome.Destroy;
begin
   FSunZenithColor.Free;
   FSunDawnColor.Free;
   FHazeColor.Free;
   FSkyColor.Free;
   FNightColor.Free;
	inherited Destroy;
end;

// Assign
//
procedure TEarthSkyDome.Assign(Source: TPersistent);
begin
   if Source is TSkyDome then begin
      FSunElevation:=TEarthSkyDome(Source).SunElevation;
      FTurbidity:=TEarthSkyDome(Source).Turbidity;
      FSunZenithColor.Assign(TEarthSkyDome(Source).FSunZenithColor);
      FSunDawnColor.Assign(TEarthSkyDome(Source).FSunDawnColor);
      FHazeColor.Assign(TEarthSkyDome(Source).FHazeColor);
      FSkyColor.Assign(TEarthSkyDome(Source).FSkyColor);
      FNightColor.Assign(TEarthSkyDome(Source).FNightColor);
      FSlices:=TEarthSkyDome(Source).FSlices;
      FStacks:=TEarthSkyDome(Source).FStacks;
      PreCalculate;
   end;
   inherited;
end;

// SetSunElevation
//
procedure TEarthSkyDome.SetSunElevation(const val : Single);
begin
   FSunElevation:=ClampValue(val, -90, 90);
   PreCalculate;
end;

// SetTurbidity
//
procedure TEarthSkyDome.SetTurbidity(const val : Single);
begin
   FTurbidity:=ClampValue(val, 1, 120);
   PreCalculate;
end;

// SetSunZenithColor
//
procedure TEarthSkyDome.SetSunZenithColor(const val : TGLColor);
begin
   FSunZenithColor.Assign(val);
   PreCalculate;
end;

// SetSunDawnColor
//
procedure TEarthSkyDome.SetSunDawnColor(const val : TGLColor);
begin
   FSunDawnColor.Assign(val);
   PreCalculate;
end;

// SetHazeColor
//
procedure TEarthSkyDome.SetHazeColor(const val : TGLColor);
begin
   FHazeColor.Assign(val);
   PreCalculate;
end;

// SetSkyColor
//
procedure TEarthSkyDome.SetSkyColor(const val : TGLColor);
begin
   FSkyColor.Assign(val);
   PreCalculate;
end;

// SetNightColor
//
procedure TEarthSkyDome.SetNightColor(const val : TGLColor);
begin
   FNightColor.Assign(val);
   PreCalculate;
end;

// SetSlices
//
procedure TEarthSkyDome.SetSlices(const val : Integer);
begin
   if val>6 then
      FSlices:=val
   else FSlices:=6;
   StructureChanged;
end;

// SetStacks
//
procedure TEarthSkyDome.SetStacks(const val : Integer);
begin
   if val>1 then
      FStacks:=val
   else FStacks:=1;
   StructureChanged;
end;

// BuildList
//
procedure TEarthSkyDome.BuildList(var rci : TRenderContextInfo);
begin
   RenderDome;
   inherited;
end;

// OnColorChanged
//
procedure TEarthSkyDome.OnColorChanged(Sender : TObject);
begin
   PreCalculate;
end;

// PreCalculate
//
procedure TEarthSkyDome.PreCalculate;
var
   ts : Single;
   fts : Single;
begin
   ts:=DegToRad(90-SunElevation);
   // Precompose base colors
   fts:=exp(-3*(PI/2-ts));
   VectorLerp(clrWhite, clrYellow, fts, FCurSunColor);
   fts:=1-cos(ts-0.5);
   VectorLerp(clrWhite, clrBlack, fts, FCurHazeColor);
   VectorLerp(clrBlue, clrBlack, fts, FCurSkyColor);
   // Precalculate Turbidity factors
   FCurHazeTurbid:=-sqrt(121-Turbidity)*2;
   FCurSunSkyTurbid:=-(121-Turbidity);
   StructureChanged;
end;

// CalculateColor
//
function TEarthSkyDome.CalculateColor(const theta, cosGamma : Single) : TColorVector;
var
   t : Single;
begin
   t:=PI/2-theta;
   // mix to get haze/sky
   VectorLerp(FCurSkyColor, FCurHazeColor, exp(FCurHazeTurbid*t), Result);
   // then mix sky with sun
   VectorLerp(Result, FCurSunColor, exp(FCurSunSkyTurbid*cosGamma*(1+t)), Result);
end;

// SetSunElevation
//
procedure TEarthSkyDome.RenderDome;
var
   ts : Single;
   steps : Integer;
   sunPos : TAffineVector;
   sinTable, cosTable : PFloatArray;

   // coordinates system note: X is forward, Y is left and Z is up
   // always rendered as sphere of radius 1

   function CalculateCosGamma(const p : TVector) : Single;
   begin
      Result:=1-VectorAngle(PAffineVector(@p)^, sunPos);
   end;

   procedure RenderBand(start, stop : Single);
   var
      i : Integer;
      r, r2, thetaStart, thetaStop : Single;
      vertex1, vertex2 : TVector;
      color : TColorVector;
   begin
      vertex1[3]:=1;
      if stop=90 then begin
         // triangle fan with north pole
         glBegin(GL_TRIANGLE_FAN);
            color:=CalculateColor(0, CalculateCosGamma(ZHmgPoint));
            glColor4fv(@color);
            glVertex4fv(@ZHmgPoint);
            SinCos(DegToRad(start), vertex1[2], r);
            thetaStart:=DegToRad(90-start);
            for i:=0 to steps do begin
               vertex1[0]:=r*cosTable[i];
               vertex1[1]:=r*sinTable[i];
               color:=CalculateColor(thetaStart, CalculateCosGamma(vertex1));
               glColor4fv(@color);
               glVertex4fv(@vertex1);
            end;
         glEnd;
      end else begin
        vertex2[3]:=1;
         // triangle strip
         glBegin(GL_TRIANGLE_STRIP);
            SinCos(DegToRad(start), vertex1[2], r);
            SinCos(DegToRad(stop), vertex2[2], r2);
            thetaStart:=DegToRad(90-start);
            thetaStop:=DegToRad(90-stop);
            for i:=0 to steps do begin
               vertex1[0]:=r*cosTable[i];
               vertex1[1]:=r*sinTable[i];
               color:=CalculateColor(thetaStart, CalculateCosGamma(vertex1));
               glColor4fv(@color);
               glVertex4fv(@vertex1);
               vertex2[0]:=r2*cosTable[i];
               vertex2[1]:=r2*sinTable[i];
               color:=CalculateColor(thetaStop, CalculateCosGamma(vertex2));
               glColor4fv(@color);
               glVertex4fv(@vertex2);
            end;
         glEnd;
      end;
   end;

var
   n, i : Integer;
   t, t2, p : Single;
begin
   ts:=DegToRad(90-SunElevation);
   SetVector(sunPos, sin(ts), 0, cos(ts));
   // prepare sin/cos LUT, with a higher sampling around 0°
   n:=Slices div 2;
   steps:=2*n+1;
   GetMem(sinTable, steps*SizeOf(Single));
   GetMem(cosTable, steps*SizeOf(Single));
   for i:=1 to n do begin
      p:=(1-sqrt(cos(i/n*PI/2)))*PI;
      SinCos(p, sinTable[n+i], cosTable[n+i]);
      sinTable[n-i]:=-sinTable[n+i];
      cosTable[n-i]:=cosTable[n+i];
   end;
   sinTable[n]:=0;
   cosTable[n]:=1;
   // start render
   for n:=0 to Stacks-1 do begin
      t:=n/Stacks;
      t2:=(n+1)/Stacks;
      RenderBand(Lerp(1, 90, t), Lerp(1, 90, t2));
   end;
   FreeMem(sinTable);
   FreeMem(cosTable);
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TSkyDome, TEarthSkyDome]);

end.

