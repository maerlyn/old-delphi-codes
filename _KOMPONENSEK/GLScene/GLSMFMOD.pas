{: GLSMFMOD<p>

	FMOD based sound-manager (http://www.fmod.org/, free for freeware).<p>

   Unsupported feature(s) :<ul>
      <li>sound source velocity
      <li>looping (sounds are played either once or forever)
      <li>sound cones
   </ul><p>

	<b>Historique : </b><font size=-1><ul>
      <li>13/01/01 - Egg - Updated for API 3.3 compatibility
	   <li>09/06/00 - Egg - Creation
	</ul></font>
}
unit GLSMFMOD;

interface

uses Classes, GLSound, GLScene;

type

	// TGLSMFMOD
	//
	TGLSMFMOD = class (TGLSoundManager)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;
         procedure NotifyMasterVolumeChange; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;
         procedure MuteSource(aSource : TGLBaseSoundSource; muted : Boolean); override;
         procedure PauseSource(aSource : TGLBaseSoundSource; paused : Boolean); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

         procedure UpdateSources; override;

         function CPUUsagePercent : Single; override;

	   published
	      { Published Declarations }
         property MaxChannels default 32;
	end;

procedure Register;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils, FMod, Geometry;

type
   TFMODInfo =  record
      channel : Integer;
      pfs : pFSOUND_SAMPLE;
   end;
   PFMODInfo = ^TFMODInfo;

procedure Register;
begin
  RegisterComponents('GLScene', [TGLSMFMOD]);
end;

// VectorToFMODVector
//
procedure VectorToFMODVector(const aVector : TVector; var aFMODVector : TFSoundVector);
begin
   aFMODVector.x:=aVector[0];
   aFMODVector.y:=aVector[1];
   aFMODVector.z:=-aVector[2];
end;

// ------------------
// ------------------ TGLSMFMOD ------------------
// ------------------

// Create
//
constructor TGLSMFMOD.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=32;
end;

// Destroy
//
destructor TGLSMFMOD.Destroy;
begin
	inherited Destroy;
end;

// DoActivate
//
function TGLSMFMOD.DoActivate : Boolean;
begin
   if not FSOUND_SetOutput(FSOUND_OUTPUT_WINMM) then Assert(False);
   if not FSOUND_SetDriver(0) then Assert(False);
   if not FSOUND_Init(OutputFrequency, MaxChannels, 0) then Assert(False);
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMFMOD.DoDeActivate;
begin
   FSOUND_StopSound(FSOUND_ALL);
   FSOUND_Close;
end;

// NotifyMasterVolumeChange
//
procedure TGLSMFMOD.NotifyMasterVolumeChange;
begin
   FSOUND_SetSFXMasterVolume(Round(MasterVolume*255));
end;

// KillSource
//
procedure TGLSMFMOD.KillSource(aSource : TGLBaseSoundSource);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      if p.channel<>-1 then
         if not FSOUND_StopSound(p.channel) then Assert(False);
      FSOUND_Sample_Free(p.pfs);
      FreeMem(p);
      aSource.ManagerTag:=0;
   end;
end;

// UpdateSource
//
procedure TGLSMFMOD.UpdateSource(aSource : TGLBaseSoundSource);
var
   p : PFMODInfo;
   objPos, objVel : TVector;
   position, velocity : TFSoundVector;
begin
   if (aSource.Sample=nil) or (aSource.Sample.Data.WAVDataSize=0) then Exit;
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      if not FSOUND_IsPlaying(p.channel) then begin
         aSource.Free;
         Exit;
      end;
   end else begin
      p:=AllocMem(SizeOf(TFMODInfo));
      p.channel:=-1;
      p.pfs:=FSOUND_Sample_Load(FSOUND_FREE, aSource.Sample.Data.WAVData,
                                FSOUND_HW3D+FSOUND_LOOP_OFF+FSOUND_LOADMEMORY,
                                aSource.Sample.Data.WAVDataSize);
      if aSource.NbLoops>1 then
         FSOUND_Sample_SetLoopMode(p.pfs, FSOUND_LOOP_NORMAL);
      FSOUND_Sample_SetMinMaxDistance(p.pfs, aSource.MinDistance, aSource.MaxDistance);
      aSource.ManagerTag:=Integer(p);
   end;
   if aSource.Origin<>nil then begin
      objPos:=aSource.Origin.AbsolutePosition;
      objVel:=NullHmgVector;
   end else begin
      objPos:=NullHmgPoint;
      objVel:=NullHmgVector;
   end;
   VectorToFMODVector(objPos, position);
   VectorToFMODVector(objVel, velocity);
   if p.channel=-1 then
      p.channel:=FSOUND_PlaySound(FSOUND_FREE, p.pfs);
   FSOUND_3D_SetAttributes(p.channel, @position, @velocity);
   if p.channel<>-1 then begin
      FSOUND_SetVolume(p.channel, Round(aSource.Volume*255));
      FSOUND_SetPriority(p.channel, aSource.Priority);
   end else aSource.Free;
end;

// MuteSource
//
procedure TGLSMFMOD.MuteSource(aSource : TGLBaseSoundSource; muted : Boolean);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      FSOUND_SetMute(p.channel, muted);
   end;
end;

// PauseSource
//
procedure TGLSMFMOD.PauseSource(aSource : TGLBaseSoundSource; paused : Boolean);
var
   p : PFMODInfo;
begin
   if aSource.ManagerTag<>0 then begin
      p:=PFMODInfo(aSource.ManagerTag);
      FSOUND_SetPaused(p.channel, paused);
   end;
end;

// UpdateSources
//
procedure TGLSMFMOD.UpdateSources;
var
   objPos, objVel, objDir, objUp : TVector;
   position, velocity, fwd, top : TFSoundVector;
begin
   // update listener
   ListenerCoordinates(objPos, objVel, objDir, objUp);
   VectorToFMODVector(objPos, position);
   VectorToFMODVector(objVel, velocity);
   VectorToFMODVector(objDir, fwd);
   VectorToFMODVector(objUp, top);
   FSOUND_3D_Listener_SetAttributes(@position, @velocity,
                                    fwd.x, fwd.y, fwd.z,
                                    top.x, top.y, top.z);
   // update sources
   inherited;
   FSOUND_3D_Update;
end;

// CPUUsagePercent
//
function TGLSMFMOD.CPUUsagePercent : Single;
begin
   Result:=FSOUND_GetCPUUsage;
end;

end.

