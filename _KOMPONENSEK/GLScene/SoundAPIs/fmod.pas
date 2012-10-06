//==========================================================================================
// FMOD Main header file. Copyright (c), FireLight Multimedia 1999-2001.
//==========================================================================================

unit FMOD;

interface

uses
  Windows;

//===============================================================================================
// DEFINITIONS
//===============================================================================================

const
  FMOD_VERSION = 3.31;

// fmod defined types
type
  PFSoundSample = Pointer;
  PFSoundStream = Pointer;
  PFSoundDSPUnit = Pointer;
  PFMusicModule = Pointer;
  PFSoundMaterial = Pointer;
  PFSoundGeomList = Pointer;

  PFSoundVector = ^TFSoundVector;
  TFSoundVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

  // callback types
  TFSoundStreamCallback = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool; cdecl;
  TFSoundDSPCallback = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; cdecl;
  TFMusicCallback = procedure (Module: PFMusicModule; Param: Byte); cdecl;

  TFSoundOpenCallback = function (Name: PChar): Cardinal; cdecl;
  TFSoundCloseCallback = procedure (Handle: Cardinal); cdecl;
  TFSoundReadCallback = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal; cdecl;
  TFSoundSeekCallback = procedure (Handle: Cardinal; Pos: Cardinal; Mode: ByteBool); cdecl;
  TFSoundTellCallback = function (Handle: Cardinal): Cardinal; cdecl;

  // To maintain compatability with existing Delphi code
  PFSOUND_SAMPLE = PFSoundSample;
  PFSOUND_STREAM = PFSoundStream;
  PFSOUND_DSPUNIT = PFSoundDSPUnit;
  PFMUSIC_MODULE = PFMusicModule;
  PFSOUND_MATERIAL = PFSoundMaterial;
  PFSOUND_GEOMLIST = PFSoundGeomList;
  PFSOUND_VECTOR = PFSoundVector;

  FSOUND_STREAMCALLBACK = TFSoundStreamCallback;
  FSOUND_DSPCALLBACK = TFSoundDSPCallback;
  FMUSIC_CALLBACK = TFMusicCallback;

  FSOUND_OPENCALLBACK = TFSoundOpenCallback;
  FSOUND_CLOSECALLBACK = TFSoundCloseCallback;
  FSOUND_READCALLBACK = TFSoundReadCallback;
  FSOUND_SEEKCALLBACK = TFSoundSeekCallback;
  FSOUND_TELLCALLBACK = TFSoundTellCallback;

{
[ENUM]
[
  [DESCRIPTION]
  On failure of commands in FMOD, use FSOUND_GetError to attain what happened.

  [SEE_ALSO]
  FSOUND_GetError
]
}

type
  TFModErrors = (
    FMOD_ERR_NONE, // No errors
    FMOD_ERR_BUSY, // Cannot call this command after FSOUND_Init.  Call FSOUND_Close first.
    FMOD_ERR_UNINITIALIZED, // This command failed because FSOUND_Init was not called
    FMOD_ERR_INIT, // Error initializing output device.
    FMOD_ERR_ALLOCATED, // Error initializing output device, but more specifically, the output device is already in use and cannot be reused.
    FMOD_ERR_PLAY, // Playing the sound failed.
    FMOD_ERR_OUTPUT_FORMAT, // Soundcard does not support the features needed for this soundsystem (16bit stereo output)
    FMOD_ERR_COOPERATIVELEVEL, // Error setting cooperative level for hardware.
    FMOD_ERR_CREATEBUFFER, // Error creating hardware sound buffer.
    FMOD_ERR_FILE_NOTFOUND, // File not found
    FMOD_ERR_FILE_FORMAT, // Unknown file format
    FMOD_ERR_FILE_BAD, // Error loading file
    FMOD_ERR_MEMORY, // Not enough memory
    FMOD_ERR_VERSION, // The version number of this file format is not supported
    FMOD_ERR_INVALID_PARAM, // An invalid parameter was passed to this function
    FMOD_ERR_NO_EAX, // Tried to use an EAX command on a non EAX enabled channel or output.
    FMOD_ERR_NO_EAX2, // Tried to use an advanced EAX2 command on a non EAX2 enabled channel or output.
    FMOD_ERR_CHANNEL_ALLOC, // Failed to allocate a new channel
    FMOD_ERR_RECORD, // Recording is not supported on this machine
    FMOD_ERR_MEDIAPLAYER  // Required Mediaplayer codec is not installed
  );

  FMOD_ERRORS = TFModErrors;

{
[ENUM]
[
  [DESCRIPTION]
  These output types are used with FSOUND_SetOutput, to choose which output driver to use.

  FSOUND_OUTPUT_A3D will cause FSOUND_Init to FAIL if you have not got a vortex
  based A3D card. The suggestion for this is to immediately try and reinitialize FMOD with
  FSOUND_OUTPUT_DSOUND, and if this fails, try initializing FMOD with FSOUND_OUTPUT_WAVEOUT.

  FSOUND_OUTPUT_DSOUND will not support hardware 3d acceleration if the sound card driver
  does not support DirectX 6 Voice Manager Extensions.

  [SEE_ALSO]
  FSOUND_SetOutput
  FSOUND_GetOutput
]
}

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND, // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM, // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND, // DirectSound driver.  You need this to get EAX or EAX2 support.
    FSOUND_OUTPUT_A3D, // A3D driver.  You need this to get geometry support.
    FSOUND_OUTPUT_OSS, // Linux/Unix OSS (Open Sound System) driver, i.e. the kernel sound drivers.
    FSOUND_OUTPUT_ESD // Linux/Unix ESD (Enlightment Sound Daemon) driver.
  );

  FSOUND_OUTPUTTYPES = TFSoundOutputTypes;

{
[ENUM]
[
  [DESCRIPTION]
  These mixer types are used with FSOUND_SetMixer, to choose which mixer to use, or to act
  upon for other reasons using FSOUND_GetMixer.

  [SEE_ALSO]
  FSOUND_SetMixer
  FSOUND_GetMixer
]
}
type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,  // Enables autodetection of the fastest mixer based on your cpu.
    FSOUND_MIXER_BLENDMODE, // Enables the standard non mmx, blendmode mixer.
    FSOUND_MIXER_MMXP5, // Enables the mmx, pentium optimized blendmode mixer.
    FSOUND_MIXER_MMXP6, // Enables the mmx, ppro/p2/p3 optimized mixer.
    FSOUND_MIXER_QUALITY_AUTODETECT,  // Enables autodetection of the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU, // Enables the interpolating FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5, // Enables the interpolating p5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6  // Enables the interpolating ppro/p2/p3 MMX mixer.
  );

  FSOUND_MIXERTYPES = TFSoundMixerTypes;

{
[ENUM]
[
  [DESCRIPTION]
  These definitions describe the type of song being played.

  [SEE_ALSO]
  FMUSIC_GetType
]
}
type
  TFMusicTypes = (
    FMUSIC_TYPE_NONE,
    FMUSIC_TYPE_MOD,  // Protracker / FastTracker
    FMUSIC_TYPE_S3M,  // ScreamTracker 3
    FMUSIC_TYPE_XM,   //  FastTracker 2
    FMUSIC_TYPE_IT,   // Impulse Tracker
    FMUSIC_TYPE_MIDI  // MIDI file
  );

  FMUSIC_TYPES = TFMusicTypes;

{
[DEFINE_START]
[
  [NAME]
  FSOUND_DSP_PRIORITIES

  [DESCRIPTION]
  These default priorities are

  [SEE_ALSO]
  FSOUND_DSP_Create
  FSOUND_DSP_SetPriority
]
}
const
  FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT = 0; // DSP CLEAR unit - done first
  FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT = 100; // DSP SFX unit - done second
  FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT = 200; // DSP MUSIC unit - done third
  FSOUND_DSP_DEFAULTPRIORITY_USER = 300; // User priority, use this as reference
  FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT = 1000; // DSP CLIP AND COPY unit - last
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CAPS

  [DESCRIPTION]
  Driver description bitfields. Use FSOUND_Driver_GetCaps to determine if a driver enumerated
  has the settings you are after. The enumerated driver depends on the output mode, see
  FSOUND_OUTPUTTYPES

  [SEE_ALSO]
  FSOUND_GetDriverCaps
  FSOUND_OUTPUTTYPES
]
}
const
  FSOUND_CAPS_HARDWARE = $1; // This driver supports hardware accelerated 3d sound.
  FSOUND_CAPS_EAX = $2; // This driver supports EAX reverb
  FSOUND_CAPS_GEOMETRY_OCCLUSIONS = $4; // This driver supports (A3D) geometry occlusions
  FSOUND_CAPS_GEOMETRY_REFLECTIONS = $8; // This driver supports (A3D) geometry reflections
  FSOUND_CAPS_EAX2 = $10; // This driver supports EAX2/A3D3 reverb
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_MODES

  [DESCRIPTION]
  Sample description bitfields, OR them together for loading and describing samples.
]
}
const
  FSOUND_LOOP_OFF = $00000001; // For non looping samples.
  FSOUND_LOOP_NORMAL = $00000002; // For forward looping samples.
  FSOUND_LOOP_BIDI = $00000004; // For bidirectional looping samples. (no effect if in hardware).
  FSOUND_8BITS = $00000008; // For 8 bit samples.
  FSOUND_16BITS = $00000010; // For 16 bit samples.
  FSOUND_MONO = $00000020; // For mono samples.
  FSOUND_STEREO = $00000040; // For stereo samples.
  FSOUND_UNSIGNED = $00000080; // For source data containing unsigned samples.
  FSOUND_SIGNED = $00000100; // For source data containing signed data.
  FSOUND_DELTA = $00000200; // For source data stored as delta values.
  FSOUND_IT214 = $00000400; // For source data stored using IT214 compression.
  FSOUND_IT215 = $00000800; // For source data stored using IT215 compression.
  FSOUND_HW3D = $00001000; // Attempts to make samples use 3d hardware acceleration. (if the card supports it)
  FSOUND_2D = $00002000; // Ignores any 3d processing. overrides FSOUND_HW3D. Located in software.
  FSOUND_STREAMABLE = $00004000; // For realtime streamable samples. If you dont supply this sound may come out corrupted.
  FSOUND_LOADMEMORY = $00008000; // For FSOUND_Sample_Load - 'name' will be interpreted as a pointer to data
  FSOUND_LOADRAW = $00010000; // For FSOUND_Sample_Load/FSOUND_Stream_Open - will ignore file format and treat as raw pcm.
  FSOUND_MPEGACCURATE = $00020000; // For FSOUND_Stream_Open - scans MP2/MP3 (VBR also) for accurate FSOUND_Stream_GetLengthMs/FSOUND_Stream_SetTime.

// Default sample type. Loop off, 8bit mono, signed, not hardware accelerated.
// Some API functions ignore 8bits and mono, as it may be an mpeg/wav/etc which has its format predetermined.
const
  FSOUND_NORMAL = (FSOUND_LOOP_OFF or FSOUND_8BITS or FSOUND_MONO);
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CDPLAYMODES

  [DESCRIPTION]
  Playback method for a CD Audio track, using FSOUND_CD_Play

  [SEE_ALSO]
  FSOUND_CD_Play
]
}
const
  FSOUND_CD_PLAYCONTINUOUS = 0;   // Starts from the current track and plays to end of CD.
  FSOUND_CD_PLAYONCE = 1;         // Plays the specified track then stops.
  FSOUND_CD_PLAYLOOPED = 2;       // Plays the specified track looped, forever until stopped manually.
  FSOUND_CD_PLAYRANDOM = 3;       // Plays tracks in random order
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CHANNELSAMPLEMODE

  [DESCRIPTION]
  Miscellaneous values for FMOD functions.

  [SEE_ALSO]
  FSOUND_PlaySound
  FSOUND_PlaySound3DAttrib
  FSOUND_Sample_Alloc
  FSOUND_Sample_Load
  FSOUND_SetPan
]
}
const
  FSOUND_FREE = -1; // value to play on any free channel, or to allocate a sample in a free sample slot.
  FSOUND_UNMANAGED = -2; // value to allocate a sample that is NOT managed by FSOUND or placed in a sample slot.
  FSOUND_ALL = -3; // for a channel index , this flag will affect ALL channels available! Not supported by every function.
  FSOUND_STEREOPAN = -1; // value for FSOUND_SetPan so that stereo sounds are not played at half volume. See FSOUND_SetPan for more on this.
// [DEFINE_END]


{
[ENUM]
[
  [DESCRIPTION]
  These are environment types defined for use with the FSOUND_Reverb API.

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
  FSOUND_Reverb_SetEnvironmentAdvanced
]
}
type
  TFSoundReverbEnvironments = (
    FSOUND_ENVIRONMENT_GENERIC,
    FSOUND_ENVIRONMENT_PADDEDCELL,
    FSOUND_ENVIRONMENT_ROOM,
    FSOUND_ENVIRONMENT_BATHROOM,
    FSOUND_ENVIRONMENT_LIVINGROOM,
    FSOUND_ENVIRONMENT_STONEROOM,
    FSOUND_ENVIRONMENT_AUDITORIUM,
    FSOUND_ENVIRONMENT_CONCERTHALL,
    FSOUND_ENVIRONMEN,
    FSOUND_ENVIRONMENT_COUNT
  );

  FSOUND_REVERB_ENVIRONMENTS = TFSoundReverbEnvironments;

{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERBMIX_USEDISTANCE

  [DESCRIPTION]
  Used with FSOUND_Reverb_SetMix, this setting allows reverb to attenuate based on distance from the listener.
  Instead of hard coding a value with FSOUND_Reverb_SetMix, this value can be used instead, for a more natural
  reverb dropoff.

  [SEE_ALSO]
  FSOUND_Reverb_SetMix
]
}
const
  FSOUND_REVERBMIX_USEDISTANCE = -1.0; // used with FSOUND_Reverb_SetMix to scale reverb by distance
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERB_IGNOREPARAM

  [DESCRIPTION]
  Used with FSOUND_Reverb_SetEnvironment and FSOUND_Reverb_SetEnvironmentAdvanced, this can
  be placed in the place of a specific parameter for the reverb setting. It allows you to
  not set any parameters except the ones you are interested in .. and example would be this.
  FSOUND_Reverb_SetEnvironment(FSOUND_REVERB_IGNOREPARAM,
  FSOUND_REVERB_IGNOREPARAM,
  FSOUND_REVERB_IGNOREPARAM,
  0.0f);
  This means env, vol and decay are left alone, but 'damp' is set to 0.

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
  FSOUND_Reverb_SetEnvironmentAdvanced
]
}
const
  FSOUND_REVERB_IGNOREPARAM = -9999999; // used with FSOUND_Reverb_SetEnvironmentAdvanced to ignore certain parameters by choice.
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_REVERB_PRESETS

  [DESCRIPTION]
  A set of predefined environment PARAMETERS, created by Creative Labs
  These can be placed directly into the FSOUND_Reverb_SetEnvironment call

  [SEE_ALSO]
  FSOUND_Reverb_SetEnvironment
]
}
{
const
  FSOUND_PRESET_OFF = FSOUND_ENVIRONMENT_GENERIC, 0.0, 0.0 f, 0.0 f;
  FSOUND_PRESET_GENERIC = FSOUND_ENVIRONMENT_GENERIC, 0.5, 1.493 f, 0.5 f;
  FSOUND_PRESET_PADDEDCELL = FSOUND_ENVIRONMENT_PADDEDCELL, 0.25, 0.1 f, 0.0 f;
  FSOUND_PRESET_ROOM = FSOUND_ENVIRONMENT_ROOM, 0.417, 0.4 f, 0.666 f;
  FSOUND_PRESET_BATHROOM = FSOUND_ENVIRONMENT_BATHROOM, 0.653, 1.499 f, 0.166 f;
  FSOUND_PRESET_LIVINGROOM = FSOUND_ENVIRONMENT_LIVINGROOM, 0.208, 0.478 f, 0.0 f;
  FSOUND_PRESET_STONEROOM = FSOUND_ENVIRONMENT_STONEROOM, 0.5, 2.309 f, 0.888 f;
  FSOUND_PRESET_AUDITORIUM = FSOUND_ENVIRONMENT_AUDITORIUM, 0.403, 4.279 f, 0.5 f;
  FSOUND_PRESET_CONCERTHALL = FSOUND_ENVIRONMENT_CONCERTHALL, 0.5, 3.961 f, 0.5 f;
  FSOUND_PRESET_CAVE = FSOUND_ENVIRONMENT_CAVE, 0.5, 2.886 f, 1.304 f;
  FSOUND_PRESET_ARENA = FSOUND_ENVIRONMENT_ARENA, 0.361, 7.284 f, 0.332 f;
  FSOUND_PRESET_HANGAR = FSOUND_ENVIRONMENT_HANGAR, 0.5, 10.0 f, 0.3 f;
  FSOUND_PRESET_CARPETEDHALLWAY = FSOUND_ENVIRONMENT_CARPETEDHALLWAY, 0.153, 0.259 f, 2.0 f;
  FSOUND_PRESET_HALLWAY = FSOUND_ENVIRONMENT_HALLWAY, 0.361, 1.493 f, 0.0 f;
  FSOUND_PRESET_STONECORRIDOR = FSOUND_ENVIRONMENT_STONECORRIDOR, 0.444, 2.697 f, 0.638 f;
  FSOUND_PRESET_ALLEY = FSOUND_ENVIRONMENT_ALLEY, 0.25, 1.752 f, 0.776 f;
  FSOUND_PRESET_FOREST = FSOUND_ENVIRONMENT_FOREST, 0.111, 3.145 f, 0.472 f;
  FSOUND_PRESET_CITY = FSOUND_ENVIRONMENT_CITY, 0.111, 2.767 f, 0.224 f;
  FSOUND_PRESET_MOUNTAINS = FSOUND_ENVIRONMENT_MOUNTAINS, 0.194, 7.841 f, 0.472 f;
  FSOUND_PRESET_QUARRY = FSOUND_ENVIRONMENT_QUARRY, 1.0, 1.499 f, 0.5 f;
  FSOUND_PRESET_PLAIN = FSOUND_ENVIRONMENT_PLAIN, 0.097, 2.767 f, 0.224 f;
  FSOUND_PRESET_PARKINGLOT = FSOUND_ENVIRONMENT_PARKINGLOT, 0.208, 1.652 f, 1.5 f;
  FSOUND_PRESET_SEWERPIPE = FSOUND_ENVIRONMENT_SEWERPIPE, 0.652, 2.886 f, 0.25 f;
  FSOUND_PRESET_UNDERWATER = FSOUND_ENVIRONMENT_UNDERWATER, 1.0, 1.499 f, 0.0 f;
  FSOUND_PRESET_DRUGGED = FSOUND_ENVIRONMENT_DRUGGED, 0.875, 8.392 f, 1.388 f;
  FSOUND_PRESET_DIZZY = FSOUND_ENVIRONMENT_DIZZY, 0.139, 17.234 f, 0.666 f;
  FSOUND_PRESET_PSYCHOTIC = FSOUND_ENVIRONMENT_PSYCHOTIC, 0.486, 7.563 f, 0.806 f;
}
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_GEOMETRY_MODES

  [DESCRIPTION]
  Geometry flags, used as the mode flag in FSOUND_Geometry_AddPolygon

  [SEE_ALSO]
  FSOUND_Geometry_AddPolygon
]
}
const
  FSOUND_GEOMETRY_NORMAL = $0; // Default geometry type. Occluding polygon
  FSOUND_GEOMETRY_REFLECTIVE = $01; // This polygon is reflective
  FSOUND_GEOMETRY_OPENING = $02; // Overlays a transparency over the previous polygon. The 'openingfactor' value supplied is copied internally.
  FSOUND_GEOMETRY_OPENING_REFERENCE = $04; // Overlays a transparency over the previous polygon. The 'openingfactor' supplied is pointed to (for access when building a list)
// [DEFINE_END]

{
[DEFINE_START]
[
  [NAME]
  FSOUND_INIT_FLAGS

  [DESCRIPTION]
  Initialization flags. Use them with FSOUND_Init in the flags parameter to change various behaviour.

  [SEE_ALSO]
  FSOUND_Init
]
}
const
  FSOUND_INIT_USEDEFAULTMIDISYNTH = $01; // Causes MIDI playback to force software decoding.
// [DEFINE_END]

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

// ==================================
// Initialization / Global functions.
// ==================================

// Pre FSOUND_Init functions. These can't be called after FSOUND_Init is called (they will fail)
// They set up FMOD system functionality.

function FSOUND_SetOutput(OutputType: TFSoundOutputTypes): ByteBool; stdcall;
function FSOUND_SetDriver(Driver: Integer): ByteBool; stdcall;
function FSOUND_SetMixer(Mixer: TFSoundMixerTypes): ByteBool; stdcall;
function FSOUND_SetBufferSize(LenMs: Integer): ByteBool; stdcall;
function FSOUND_SetHWND(Hwnd: THandle): ByteBool; stdcall;
function FSOUND_SetMinHardwareChannels(Min: Integer): ByteBool; stdcall;
function FSOUND_SetMaxHardwareChannels(Max: Integer): ByteBool; stdcall;

// Main initialization / closedown functions

function FSOUND_Init(MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; stdcall;
procedure FSOUND_Close; stdcall;

// Runtime

procedure FSOUND_SetSFXMasterVolume(Volume: Integer); stdcall;
procedure FSOUND_SetPanSeperation(PanSep: Single); stdcall;

// System information

function FSOUND_GetError: TFModErrors; stdcall;
function FSOUND_GetVersion: Single; stdcall;
function FSOUND_GetOutput: TFSoundOutputTypes; stdcall;
function FSOUND_GetDriver: Integer; stdcall;
function FSOUND_GetMixer: TFSoundMixerTypes; stdcall;
function FSOUND_GetNumDrivers: Integer; stdcall;
function FSOUND_GetDriverName(Id: Integer): PChar; stdcall;
function FSOUND_GetDriverCaps(Id: Integer; var Caps: Cardinal): ByteBool; stdcall;

function FSOUND_GetOutputRate: Integer; stdcall;
function FSOUND_GetMaxChannels: Integer; stdcall;
function FSOUND_GetMaxSamples: Integer; stdcall;
function FSOUND_GetSFXMasterVolume: Integer; stdcall;
function FSOUND_GetNumHardwareChannels: Integer; stdcall;
function FSOUND_GetChannelsPlaying: Integer; stdcall;
function FSOUND_GetCPUUsage: Single; stdcall;

// ===================================
// Sample management / load functions.
// ===================================

// Sample creation and management functions

function FSOUND_Sample_Load(Index: Integer; const Name: PChar; Mode: Cardinal; MemLength: Integer): PFSoundSample; stdcall;
function FSOUND_Sample_Alloc(Index: Integer;
  Length: Integer;
  Mode: Cardinal;
  DefFreq: Integer;
  DefVol: Integer;
  DefPan: Integer;
  DefPri: Integer): PFSoundSample; stdcall;
procedure FSOUND_Sample_Free(Sptr: PFSoundSample); stdcall;
function FSOUND_Sample_Upload(Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; stdcall;
function FSOUND_Sample_Lock(Sptr: PFSoundSample;
  Offset: Integer;
  Length: Integer;
  var Ptr1: Pointer;
  var Ptr2: Pointer;
  var Len1: Cardinal;
  var Len2: Cardinal): ByteBool; stdcall;
function FSOUND_Sample_Unlock(Sptr: PFSoundSample;
  Ptr1: Pointer;
  Ptr2: Pointer;
  Len1: Cardinal;
  Len2: Cardinal): ByteBool; stdcall;

// Sample control functions

function FSOUND_Sample_SetLoopMode(Sptr: PFSoundSample; LoopMode: Cardinal): ByteBool; stdcall;
function FSOUND_Sample_SetLoopPoints(Sptr: PFSoundSample;
  LoopStart: Integer; LoopEnd: Integer): ByteBool; stdcall;
function FSOUND_Sample_SetDefaults(Sptr: PFSoundSample;
  DefFreq: Integer;
  DefVol: Integer;
  DefPan: Integer;
  DefPri: Integer): ByteBool; stdcall;
function FSOUND_Sample_SetMinMaxDistance(Sptr: PFSoundSample;
  Min: Single; Max: Single): ByteBool; stdcall;

// Sample information

function FSOUND_Sample_Get(SampNo: Integer): PFSoundSample; stdcall;
function FSOUND_Sample_GetName(Sptr: PFSoundSample): PCHAR; stdcall;
function FSOUND_Sample_GetLength(Sptr: PFSoundSample): Cardinal; stdcall;
function FSOUND_Sample_GetLoopPoints(Sptr: PFSoundSample;
  var LoopStart: Integer; var LoopEnd: Integer): ByteBool; stdcall;
function FSOUND_Sample_GetDefaults(Sptr: PFSoundSample;
  var DefFreq: Integer;
  var DefVol: Integer;
  var DefPan: Integer;
  var DefPri: Integer): ByteBool; stdcall;
function FSOUND_Sample_GetMode(Sptr: PFSoundSample): Cardinal; stdcall;

// ============================
// Channel control functions.
// ============================

// Playing and stopping sounds.

function FSOUND_PlaySound(Channel: Integer; Sptr: PFSoundSample): Integer; stdcall;
function FSOUND_PlaySound3DAttrib(Channel: Integer;
  Sptr: PFSoundSample;
  Freq: Integer;
  Vol: Integer;
  Pan: Integer;
  var Pos: TFSoundVector;
  var Vel: TFSoundVector): Integer; stdcall;
function FSOUND_StopSound(Channel: Integer): ByteBool; stdcall;

// Functions to control playback of a channel.

function FSOUND_SetFrequency(Channel: Integer; Freq: Integer): ByteBool; stdcall;
function FSOUND_SetVolume(Channel: Integer; Vol: Integer): ByteBool; stdcall;
function FSOUND_SetVolumeAbsolute(Channel: Integer; Vol: Integer): ByteBool; stdcall;
function FSOUND_SetPan(Channel: Integer; Pan: Integer): ByteBool; stdcall;
function FSOUND_SetSurround(Channel: Integer; Surround: ByteBool): ByteBool; stdcall;
function FSOUND_SetMute(Channel: Integer; Mute: ByteBool): ByteBool; stdcall;
function FSOUND_SetPriority(Channel: Integer; Priority: Integer): ByteBool; stdcall;
function FSOUND_SetReserved(Channel: Integer; Reserved: ByteBool): ByteBool; stdcall;
function FSOUND_SetPaused(Channel: Integer; Paused: ByteBool): ByteBool; stdcall;
function FSOUND_SetLoopMode(Channel: Integer; LoopMode: Cardinal): ByteBool; stdcall;

// Channel information

function FSOUND_IsPlaying(Channel: Integer): ByteBool; stdcall;
function FSOUND_GetFrequency(Channel: Integer): Integer; stdcall;
function FSOUND_GetVolume(Channel: Integer): Integer; stdcall;
function FSOUND_GetPan(Channel: Integer): Integer; stdcall;
function FSOUND_GetSurround(Channel: Integer): ByteBool; stdcall;
function FSOUND_GetMute(Channel: Integer): ByteBool; stdcall;
function FSOUND_GetPriority(Channel: Integer): Integer; stdcall;
function FSOUND_GetReserved(Channel: Integer): ByteBool; stdcall;
function FSOUND_GetPaused(Channel: Integer): ByteBool; stdcall;
function FSOUND_GetCurrentPosition(Channel: Integer): Cardinal; stdcall;
function FSOUND_GetCurrentSample(Channel: Integer): PFSoundSample; stdcall;
function FSOUND_GetCurrentVU(Channel: Integer): Single; stdcall;

// ===================
// 3D sound functions.
// ===================
// see also FSOUND_PlaySound3DAttrib (above)
// see also FSOUND_Sample_SetMinMaxDistance (above)

procedure FSOUND_3D_Update; stdcall;
function FSOUND_3D_SetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; stdcall;
function FSOUND_3D_GetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; stdcall;
procedure FSOUND_3D_Listener_SetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: Single;
  fy: Single;
  fz: Single;
  tx: Single;
  ty: Single;
  tz: Single); stdcall;
procedure FSOUND_3D_Listener_GetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: PSingle;
  fy: PSingle;
  fz: PSingle;
  tx: PSingle;
  ty: PSingle;
  tz: PSingle); stdcall;
procedure FSOUND_3D_Listener_SetDopplerFactor(Scale: Single); stdcall;
procedure FSOUND_3D_Listener_SetDistanceFactor(Scale: Single); stdcall;
procedure FSOUND_3D_Listener_SetRolloffFactor(Scale: Single); stdcall;

// =========================
// File Streaming functions.
// =========================

function FSOUND_Stream_OpenFile(const Filename: PChar; Mode: Cardinal;
  MemLength: Integer): PFSoundStream; stdcall;
function FSOUND_Stream_Create(Callback: TFSoundStreamCallback;
  Length: Integer;
  Mode: Cardinal;
  SampleRate: Integer;
  UserData: Integer): PFSoundStream; stdcall;
function FSOUND_Stream_Play(Channel: Integer; Stream: PFSoundStream): Integer; stdcall;
function FSOUND_Stream_Play3DAttrib(Channel: Integer;
  Stream: PFSoundStream;
  Freq: Integer;
  Vol: Integer;
  Pan: Integer;
  var Pos: TFSoundVector;
  var Vel: TFSoundVector): Integer; stdcall;
function FSOUND_Stream_Stop(Stream: PFSoundStream): ByteBool; stdcall;
function FSOUND_Stream_Close(Stream: PFSoundStream): ByteBool; stdcall;
function FSOUND_Stream_SetEndCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; stdcall;
function FSOUND_Stream_SetSynchCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; stdcall;
function FSOUND_Stream_GetSample(Stream: PFSoundStream): PFSOUND_SAMPLE; stdcall;
function FSOUND_Stream_CreateDSP(Stream: PFSoundStream; Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; stdcall;

function FSOUND_Stream_SetPaused(Stream: PFSoundStream; Paused: ByteBool): ByteBool; stdcall;
function FSOUND_Stream_GetPaused(Stream: PFSoundStream): ByteBool; stdcall;
function FSOUND_Stream_SetPosition(Stream: PFSoundStream; Position: Integer): ByteBool; stdcall;
function FSOUND_Stream_GetPosition(Stream: PFSoundStream): Integer; stdcall;
function FSOUND_Stream_SetTime(Stream: PFSoundStream; Ms: Integer): ByteBool; stdcall;
function FSOUND_Stream_GetTime(Stream: PFSoundStream): Integer; stdcall;
function FSOUND_Stream_GetLength(Stream: PFSoundStream): Integer; stdcall;
function FSOUND_Stream_GetLengthMs(Stream: PFSoundStream): Integer; stdcall;

// ===================
// CD audio functions.
// ===================

function FSOUND_CD_Play(Track: Integer): ByteBool; stdcall;
procedure FSOUND_CD_SetPlayMode(Mode: Integer); stdcall;
function FSOUND_CD_Stop: ByteBool; stdcall;
function FSOUND_CD_SetPaused(Paused: ByteBool): ByteBool; stdcall;
function FSOUND_CD_SetVolume(Volume: Integer): ByteBool; stdcall;
function FSOUND_CD_Eject: ByteBool; stdcall;

function FSOUND_CD_GetPaused: ByteBool; stdcall;
function FSOUND_CD_GetTrack: Integer; stdcall;
function FSOUND_CD_GetNumTracks: Integer; stdcall;
function FSOUND_CD_GetVolume: Integer; stdcall;
function FSOUND_CD_GetTrackLength(Track: Integer): Integer; stdcall;
function FSOUND_CD_GetTrackTime: Integer; stdcall;

// ==============
// DSP functions.
// ==============

// DSP Unit control and information functions.

function FSOUND_DSP_Create(Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; stdcall;
procedure FSOUND_DSP_Free(DSPUnit: PFSoundDSPUnit); stdcall;
procedure FSOUND_DSP_SetPriority(DSPUnit: PFSoundDSPUnit; Priority: Integer); stdcall;
function FSOUND_DSP_GetPriority(DSPUnit: PFSoundDSPUnit): Integer; stdcall;
procedure FSOUND_DSP_SetActive(DSPUnit: PFSoundDSPUnit; Active: ByteBool); stdcall;
function FSOUND_DSP_GetActive(DSPUnit: PFSoundDSPUnit): ByteBool; stdcall;

// Functions to get hold of FSOUND 'system DSP unit' handles.

function FSOUND_DSP_GetClearUnit: PFSoundDSPUnit; stdcall;
function FSOUND_DSP_GetSFXUnit: PFSoundDSPUnit; stdcall;
function FSOUND_DSP_GetMusicUnit: PFSoundDSPUnit; stdcall;
function FSOUND_DSP_GetClipAndCopyUnit: PFSoundDSPUnit; stdcall;

// misc DSP functions

function FSOUND_DSP_MixBuffers(DestBuffer: Pointer;
  SrcBuffer: Pointer;
  Len: Integer;
  Freq: Integer;
  Vol: Integer;
  Pan: Integer;
  Mode: Cardinal): ByteBool; stdcall;
procedure FSOUND_DSP_ClearMixBuffer; stdcall;
function FSOUND_DSP_GetBufferLength: Integer; stdcall;

// ===================
// Geometry functions.
// ===================

// scene/polygon functions

function FSOUND_Geometry_AddPolygon(P1: PFSoundVector;
  P2: PFSoundVector;
  P3: PFSoundVector;
  P4: PFSoundVector;
  Normal: PFSoundVector;
  Mode: Cardinal;
  OpeningFactor: PSingle): ByteBool; stdcall;
function FSOUND_Geometry_AddList(GeomList: PFSoundGeomList): Integer; stdcall;

// polygon list functions

function FSOUND_Geometry_List_Create(BoundingVolume: ByteBool): PFSoundGeomList; stdcall;
function FSOUND_Geometry_List_Free(GeomList: PFSoundGeomList): ByteBool; stdcall;
function FSOUND_Geometry_List_Begin(GeomList: PFSoundGeomList): ByteBool; stdcall;
function FSOUND_Geometry_List_End(GeomList: PFSoundGeomList): ByteBool; stdcall;
function FSOUND_Geometry_List_Add(GeomList: PFSoundGeomList): ByteBool; stdcall;

// material functions

function FSOUND_Geometry_Material_Create: PFSoundMaterial; stdcall;
function FSOUND_Geometry_Material_Free(Material: PFSoundMaterial): ByteBool; stdcall;
function FSOUND_Geometry_Material_SetAttributes(Material: PFSoundMaterial;
  ReflectanceGain: Single;
  ReflectanceFreq: Single;
  TransmittanceGain: Single;
  TransmittanceFreq: Single): ByteBool; stdcall;
function FSOUND_Geometry_Material_GetAttributes(Material: PFSoundMaterial;
  var ReflectanceGain: Single;
  var ReflectanceFreq: Single;
  var TransmittanceGain: Single;
  var TransmittanceFreq: Single): ByteBool; stdcall;
function FSOUND_Geometry_Material_Set(Material: PFSoundMaterial): ByteBool; stdcall;

// ==============================================
// Reverb functions. (eax, eax2, a3d 3.0 reverb)
// ==============================================

// eax1, eax2, a3d 3.0 (use FSOUND_REVERB_PRESETS if you like), (eax2 support through emulation/parameter conversion)

function FSOUND_Reverb_SetEnvironment(Env: TFSoundReverbEnvironments;
  Vol: Single; Decay: Single; Damp: Single): ByteBool; stdcall;
// eax2, a3d 3.0 only, does not work on eax1
function FSOUND_Reverb_SetEnvironmentAdvanced(Env: TFSoundReverbEnvironments;
  Room: Integer;
  RoomHF: Integer;
  RoomRolloffFactor: Single;
  DecayTime: Single;
  DecayHFRatio: Single;
  Reflections: Integer;
  ReflectionsDelay: Single;
  Reverb: Integer;
  ReverbDelay: Single;
  Environment: Single): ByteBool; stdcall;
function FSOUND_Reverb_SetMix(Channel: Integer; Mix: Single): ByteBool; stdcall;

// information functions

function FSOUND_Reverb_GetEnvironment(var Env: TFSoundReverbEnvironments;
  var Vol: Single; var Decay: Single; var Damp: Single): ByteBool; stdcall;
function FSOUND_Reverb_GetEnvironmentAdvanced(var Env: TFSoundReverbEnvironments;
  var Room: Integer;
  var RoomHF: Integer;
  var RoomRolloffFactor: Single;
  var DecayTime: Single;
  var DecayHFRatio: Single;
  var Reflections: Integer;
  var ReflectionsDelay: Single;
  var Reverb: Integer;
  var ReverbDelay: Single;
  var EnvironmentSize: Single;
  var EnvironmentDiffusion: Single;
  var AirAbsorptionHF: Single): ByteBool; stdcall;
function FSOUND_Reverb_GetMix(Channel: Integer; var Mix: Single): ByteBool; stdcall;

// =========================
// Recording functions
// =========================

// initialization functions

function FSOUND_Record_SetDriver(OutputType: Integer): ByteBool; stdcall;
function FSOUND_Record_GetNumDrivers: Integer; stdcall;
function FSOUND_Record_GetDriverName(Id: Integer): PChar; stdcall;
function FSOUND_Record_GetDriver: Integer; stdcall;

// recording functionality. Only one recording session will work at a time

function FSOUND_Record_StartSample(Sptr: PFSoundSample; Loop: ByteBool): ByteBool; stdcall;
function FSOUND_Record_Stop: ByteBool; stdcall;
function FSOUND_Record_GetPosition: Integer; stdcall;

// =========================
// File system override
// =========================

procedure FSOUND_File_SetCallbacks(
        OpenCallback: TFSoundOpenCallback;
        CloseCallback: TFSoundCloseCallback;
        ReadCallback: TFSoundReadCallback;
        SeekCallback: TFSoundSeekCallback;
        TellCallback: TFSoundTellCallback); stdcall;

// =============================================================================================
// FMUSIC API
// =============================================================================================

// Song management / playback functions.
// =====================================

function FMUSIC_LoadSong(const Name: PChar): PFMUSIC_MODULE; stdcall;
function FMUSIC_LoadSongMemory(Data: Pointer; Length: Integer): PFMusicModule; stdcall;
function FMUSIC_FreeSong(Module: PFMusicModule): ByteBool; stdcall;
function FMUSIC_PlaySong(Module: PFMusicModule): ByteBool; stdcall;
function FMUSIC_StopSong(Module: PFMusicModule): ByteBool; stdcall;
procedure FMUSIC_StopAllSongs; stdcall;
function FMUSIC_SetZxxCallback(Module: PFMusicModule;
  Callback: TFMusicCallback): ByteBool; stdcall;
function FMUSIC_SetRowCallback(Module: PFMusicModule;
  Callback: TFMusicCallback; RowStep: Integer): ByteBool; stdcall;
function FMUSIC_SetOrderCallback(Module: PFMusicModule;
  Callback: TFMusicCallback; OrderStep: Integer): ByteBool; stdcall;
function FMUSIC_SetInstCallback(Module: PFMusicModule;
  Callback: TFMusicCallback; Instrument: Integer): ByteBool; stdcall;
function FMUSIC_SetSample(Module: PFMusicModule; SampNo: Integer;
  Sptr: PFSoundSample): ByteBool; stdcall;
function FMUSIC_OptimizeChannels(Module: PFMusicModule;
  MaxChannels: Integer; MinVolume: Integer): ByteBool; stdcall;

// Runtime song functions.
// =======================

function FMUSIC_SetReverb(Reverb: ByteBool): ByteBool; stdcall;
function FMUSIC_SetOrder(Module: PFMusicModule; Order: Integer): ByteBool; stdcall;
function FMUSIC_SetPaused(Module: PFMusicModule; Pause: ByteBool): ByteBool; stdcall;
function FMUSIC_SetMasterVolume(Module: PFMusicModule; Volume: Integer): ByteBool; stdcall;
function FMUSIC_SetPanSeperation(Module: PFMusicModule; PanSep: Single): ByteBool; stdcall;

// Static song information functions.
// ==================================

function FMUSIC_GetName(Module: PFMusicModule): PCHAR; stdcall;
function FMUSIC_GetType(Module: PFMusicModule): TFMusicTypes; stdcall;
function FMUSIC_GetNumOrders(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetNumPatterns(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetNumInstruments(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetNumSamples(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetNumChannels(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetSample(Module: PFMusicModule; SampNo: Integer): PFSoundSample; stdcall;
function FMUSIC_GetPatternLength(Module: PFMusicModule; OrderNo: Integer): Integer; stdcall;

// Runtime song information.
// =========================

function FMUSIC_IsFinished(Module: PFMusicModule): ByteBool; stdcall;
function FMUSIC_IsPlaying(Module: PFMusicModule): ByteBool; stdcall;
function FMUSIC_GetMasterVolume(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetGlobalVolume(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetOrder(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetPattern(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetSpeed(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetBPM(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetRow(Module: PFMusicModule): Integer; stdcall;
function FMUSIC_GetPaused(Module: PFMusicModule): ByteBool; stdcall;
function FMUSIC_GetTime(Module: PFMusicModule): Integer; stdcall;

implementation

const
  FMOD_DLL = 'fmod.dll';

function FSOUND_SetOutput; external FMOD_DLL name '_FSOUND_SetOutput@4';
function FSOUND_SetDriver; external FMOD_DLL name '_FSOUND_SetDriver@4';
function FSOUND_SetMixer; external FMOD_DLL name '_FSOUND_SetMixer@4';
function FSOUND_SetBufferSize; external FMOD_DLL name '_FSOUND_SetBufferSize@4';
function FSOUND_SetHWND; external FMOD_DLL name '_FSOUND_SetHWND@4';
function FSOUND_SetMinHardwareChannels; external FMOD_DLL name '_FSOUND_SetMinHardwareChannels@4';
function FSOUND_SetMaxHardwareChannels; external FMOD_DLL name '_FSOUND_SetMaxHardwareChannels@4';
function FSOUND_Init; external FMOD_DLL name '_FSOUND_Init@12';
procedure FSOUND_Close; external FMOD_DLL name '_FSOUND_Close@0';
procedure FSOUND_SetSFXMasterVolume; external FMOD_DLL name '_FSOUND_SetSFXMasterVolume@4';
procedure FSOUND_SetPanSeperation; external FMOD_DLL name '_FSOUND_SetPanSeperation@4';
function FSOUND_GetError; external FMOD_DLL name '_FSOUND_GetError@0';
function FSOUND_GetVersion; external FMOD_DLL name '_FSOUND_GetVersion@0';
function FSOUND_GetOutput; external FMOD_DLL name '_FSOUND_GetOutput@0';
function FSOUND_GetDriver; external FMOD_DLL name '_FSOUND_GetDriver@0';
function FSOUND_GetMixer; external FMOD_DLL name '_FSOUND_GetMixer@0';
function FSOUND_GetNumDrivers; external FMOD_DLL name '_FSOUND_GetNumDrivers@0';
function FSOUND_GetDriverName; external FMOD_DLL name '_FSOUND_GetDriverName@4';
function FSOUND_GetDriverCaps; external FMOD_DLL name '_FSOUND_GetDriverCaps@8';
function FSOUND_GetOutputRate; external FMOD_DLL name '_FSOUND_GetOutputRate@0';
function FSOUND_GetMaxChannels; external FMOD_DLL name '_FSOUND_GetMaxChannels@0';
function FSOUND_GetMaxSamples; external FMOD_DLL name '_FSOUND_GetMaxSamples@0';
function FSOUND_GetSFXMasterVolume; external FMOD_DLL name '_FSOUND_GetSFXMasterVolume@0';
function FSOUND_GetNumHardwareChannels; external FMOD_DLL name '_FSOUND_GetNumHardwareChannels@0';
function FSOUND_GetChannelsPlaying; external FMOD_DLL name '_FSOUND_GetChannelsPlaying@0';
function FSOUND_GetCPUUsage; external FMOD_DLL name '_FSOUND_GetCPUUsage@0';
function FSOUND_Sample_Load; external FMOD_DLL name '_FSOUND_Sample_Load@16';
function FSOUND_Sample_Alloc; external FMOD_DLL name '_FSOUND_Sample_Alloc@28';
procedure FSOUND_Sample_Free; external FMOD_DLL name '_FSOUND_Sample_Free@4';
function FSOUND_Sample_Upload; external FMOD_DLL name '_FSOUND_Sample_Upload@12';
function FSOUND_Sample_Lock; external FMOD_DLL name '_FSOUND_Sample_Lock@28';
function FSOUND_Sample_Unlock; external FMOD_DLL name '_FSOUND_Sample_Unlock@20';
function FSOUND_Sample_SetLoopMode; external FMOD_DLL name '_FSOUND_Sample_SetLoopMode@8';
function FSOUND_Sample_SetLoopPoints; external FMOD_DLL name '_FSOUND_Sample_SetLoopPoints@12';
function FSOUND_Sample_SetDefaults; external FMOD_DLL name '_FSOUND_Sample_SetDefaults@20';
function FSOUND_Sample_SetMinMaxDistance; external FMOD_DLL name '_FSOUND_Sample_SetMinMaxDistance@12';
function FSOUND_Sample_Get; external FMOD_DLL name '_FSOUND_Sample_Get@4';
function FSOUND_Sample_GetName; external FMOD_DLL name '_FSOUND_Sample_GetName@4';
function FSOUND_Sample_GetLength; external FMOD_DLL name '_FSOUND_Sample_GetLength@4';
function FSOUND_Sample_GetLoopPoints; external FMOD_DLL name '_FSOUND_Sample_GetLoopPoints@12';
function FSOUND_Sample_GetDefaults; external FMOD_DLL name '_FSOUND_Sample_GetDefaults@20';
function FSOUND_Sample_GetMode; external FMOD_DLL name '_FSOUND_Sample_GetMode@4';
function FSOUND_PlaySound; external FMOD_DLL name '_FSOUND_PlaySound@8';
function FSOUND_PlaySound3DAttrib; external FMOD_DLL name '_FSOUND_PlaySound3DAttrib@28';
function FSOUND_StopSound; external FMOD_DLL name '_FSOUND_StopSound@4';
function FSOUND_SetFrequency; external FMOD_DLL name '_FSOUND_SetFrequency@8';
function FSOUND_SetVolume; external FMOD_DLL name '_FSOUND_SetVolume@8';
function FSOUND_SetVolumeAbsolute; external FMOD_DLL name '_FSOUND_SetVolumeAbsolute@8';
function FSOUND_SetPan; external FMOD_DLL name '_FSOUND_SetPan@8';
function FSOUND_SetSurround; external FMOD_DLL name '_FSOUND_SetSurround@8';
function FSOUND_SetMute; external FMOD_DLL name '_FSOUND_SetMute@8';
function FSOUND_SetPriority; external FMOD_DLL name '_FSOUND_SetPriority@8';
function FSOUND_SetReserved; external FMOD_DLL name '_FSOUND_SetReserved@8';
function FSOUND_SetPaused; external FMOD_DLL name '_FSOUND_SetPaused@8';
function FSOUND_SetLoopMode; external FMOD_DLL name '_FSOUND_SetLoopMode@8';
function FSOUND_IsPlaying; external FMOD_DLL name '_FSOUND_IsPlaying@4';
function FSOUND_GetFrequency; external FMOD_DLL name '_FSOUND_GetFrequency@4';
function FSOUND_GetVolume; external FMOD_DLL name '_FSOUND_GetVolume@4';
function FSOUND_GetPan; external FMOD_DLL name '_FSOUND_GetPan@4';
function FSOUND_GetSurround; external FMOD_DLL name '_FSOUND_GetSurround@4';
function FSOUND_GetMute; external FMOD_DLL name '_FSOUND_GetMute@4';
function FSOUND_GetPriority; external FMOD_DLL name '_FSOUND_GetPriority@4';
function FSOUND_GetReserved; external FMOD_DLL name '_FSOUND_GetReserved@4';
function FSOUND_GetPaused; external FMOD_DLL name '_FSOUND_GetPaused@4';
function FSOUND_GetCurrentPosition; external FMOD_DLL name '_FSOUND_GetCurrentPosition@4';
function FSOUND_GetCurrentSample; external FMOD_DLL name '_FSOUND_GetCurrentSample@4';
function FSOUND_GetCurrentVU; external FMOD_DLL name '_FSOUND_GetCurrentVU@4';
procedure FSOUND_3D_Update; external FMOD_DLL name '_FSOUND_3D_Update@0';
function FSOUND_3D_SetAttributes; external FMOD_DLL name '_FSOUND_3D_SetAttributes@12';
function FSOUND_3D_GetAttributes; external FMOD_DLL name '_FSOUND_3D_GetAttributes@12';
procedure FSOUND_3D_Listener_SetAttributes; external FMOD_DLL name '_FSOUND_3D_Listener_SetAttributes@32';
procedure FSOUND_3D_Listener_GetAttributes; external FMOD_DLL name '_FSOUND_3D_Listener_GetAttributes@32';
procedure FSOUND_3D_Listener_SetDopplerFactor; external FMOD_DLL name '_FSOUND_3D_Listener_SetDopplerFactor@4';
procedure FSOUND_3D_Listener_SetDistanceFactor; external FMOD_DLL name '_FSOUND_3D_Listener_SetDistanceFactor@4';
procedure FSOUND_3D_Listener_SetRolloffFactor; external FMOD_DLL name '_FSOUND_3D_Listener_SetRolloffFactor@4';
function FSOUND_Stream_OpenFile; external FMOD_DLL name '_FSOUND_Stream_OpenFile@12';
function FSOUND_Stream_Create; external FMOD_DLL name '_FSOUND_Stream_Create@20';
function FSOUND_Stream_Play; external FMOD_DLL name '_FSOUND_Stream_Play@8';
function FSOUND_Stream_Play3DAttrib; external FMOD_DLL name '_FSOUND_Stream_Play3DAttrib@28';
function FSOUND_Stream_Stop; external FMOD_DLL name '_FSOUND_Stream_Stop@4';
function FSOUND_Stream_Close; external FMOD_DLL name '_FSOUND_Stream_Close@4';
function FSOUND_Stream_SetEndCallback; external FMOD_DLL name '_FSOUND_Stream_SetEndCallback@12';
function FSOUND_Stream_SetSynchCallback; external FMOD_DLL name '_FSOUND_Stream_SetSynchCallback@12';
function FSOUND_Stream_GetSample; external FMOD_DLL name '_FSOUND_Stream_GetSample@4';
function FSOUND_Stream_CreateDSP; external FMOD_DLL name '_FSOUND_Stream_CreateDSP@16';
function FSOUND_Stream_SetPaused; external FMOD_DLL name '_FSOUND_Stream_SetPaused@8';
function FSOUND_Stream_GetPaused; external FMOD_DLL name '_FSOUND_Stream_GetPaused@4';
function FSOUND_Stream_SetPosition; external FMOD_DLL name '_FSOUND_Stream_SetPosition@8';
function FSOUND_Stream_GetPosition; external FMOD_DLL name '_FSOUND_Stream_GetPosition@4';
function FSOUND_Stream_SetTime; external FMOD_DLL name '_FSOUND_Stream_SetTime@8';
function FSOUND_Stream_GetTime; external FMOD_DLL name '_FSOUND_Stream_GetTime@4';
function FSOUND_Stream_GetLength; external FMOD_DLL name '_FSOUND_Stream_GetLength@4';
function FSOUND_Stream_GetLengthMs; external FMOD_DLL name '_FSOUND_Stream_GetLengthMs@4';
function FSOUND_CD_Play; external FMOD_DLL name '_FSOUND_CD_Play@4';
procedure FSOUND_CD_SetPlayMode; external FMOD_DLL name '_FSOUND_CD_SetPlayMode@4';
function FSOUND_CD_Stop; external FMOD_DLL name '_FSOUND_CD_Stop@0';
function FSOUND_CD_SetPaused; external FMOD_DLL name '_FSOUND_CD_SetPaused@4';
function FSOUND_CD_SetVolume; external FMOD_DLL name '_FSOUND_CD_SetVolume@4';
function FSOUND_CD_Eject; external FMOD_DLL name '_FSOUND_CD_Eject@0';
function FSOUND_CD_GetPaused; external FMOD_DLL name '_FSOUND_CD_GetPaused@0';
function FSOUND_CD_GetTrack; external FMOD_DLL name '_FSOUND_CD_GetTrack@0';
function FSOUND_CD_GetNumTracks; external FMOD_DLL name '_FSOUND_CD_GetNumTracks@0';
function FSOUND_CD_GetVolume; external FMOD_DLL name '_FSOUND_CD_GetVolume@0';
function FSOUND_CD_GetTrackLength; external FMOD_DLL name '_FSOUND_CD_GetTrackLength@4';
function FSOUND_CD_GetTrackTime; external FMOD_DLL name '_FSOUND_CD_GetTrackTime@0';
function FSOUND_DSP_Create; external FMOD_DLL name '_FSOUND_DSP_Create@12';
procedure FSOUND_DSP_Free; external FMOD_DLL name '_FSOUND_DSP_Free@4';
procedure FSOUND_DSP_SetPriority; external FMOD_DLL name '_FSOUND_DSP_SetPriority@8';
function FSOUND_DSP_GetPriority; external FMOD_DLL name '_FSOUND_DSP_GetPriority@4';
procedure FSOUND_DSP_SetActive; external FMOD_DLL name '_FSOUND_DSP_SetActive@8';
function FSOUND_DSP_GetActive; external FMOD_DLL name '_FSOUND_DSP_GetActive@4';
function FSOUND_DSP_GetClearUnit; external FMOD_DLL name '_FSOUND_DSP_GetClearUnit@0';
function FSOUND_DSP_GetSFXUnit; external FMOD_DLL name '_FSOUND_DSP_GetSFXUnit@0';
function FSOUND_DSP_GetMusicUnit; external FMOD_DLL name '_FSOUND_DSP_GetMusicUnit@0';
function FSOUND_DSP_GetClipAndCopyUnit; external FMOD_DLL name '_FSOUND_DSP_GetClipAndCopyUnit@0';
function FSOUND_DSP_MixBuffers; external FMOD_DLL name '_FSOUND_DSP_MixBuffers@28';
procedure FSOUND_DSP_ClearMixBuffer; external FMOD_DLL name '_FSOUND_DSP_ClearMixBuffer@0';
function FSOUND_DSP_GetBufferLength; external FMOD_DLL name '_FSOUND_DSP_GetBufferLength@0';
function FSOUND_Geometry_AddPolygon; external FMOD_DLL name '_FSOUND_Geometry_AddPolygon@28';
function FSOUND_Geometry_AddList; external FMOD_DLL name '_FSOUND_Geometry_AddList@4';
function FSOUND_Geometry_List_Create; external FMOD_DLL name '_FSOUND_Geometry_List_Create@4';
function FSOUND_Geometry_List_Free; external FMOD_DLL name '_FSOUND_Geometry_List_Free@4';
function FSOUND_Geometry_List_Begin; external FMOD_DLL name '_FSOUND_Geometry_List_Begin@4';
function FSOUND_Geometry_List_End; external FMOD_DLL name '_FSOUND_Geometry_List_End@4';
function FSOUND_Geometry_List_Add; external FMOD_DLL name '_FSOUND_Geometry_List_Add@4';
function FSOUND_Geometry_Material_Create; external FMOD_DLL name '_FSOUND_Geometry_Material_Create@0';
function FSOUND_Geometry_Material_Free; external FMOD_DLL name '_FSOUND_Geometry_Material_Free@4';
function FSOUND_Geometry_Material_SetAttributes; external FMOD_DLL name '_FSOUND_Geometry_Material_SetAttributes@20';
function FSOUND_Geometry_Material_GetAttributes; external FMOD_DLL name '_FSOUND_Geometry_Material_GetAttributes@20';
function FSOUND_Geometry_Material_Set; external FMOD_DLL name '_FSOUND_Geometry_Material_Set@4';
function FSOUND_Reverb_SetEnvironment; external FMOD_DLL name '_FSOUND_Reverb_SetEnvironment@16';
function FSOUND_Reverb_SetEnvironmentAdvanced; external FMOD_DLL name '_FSOUND_Reverb_SetEnvironmentAdvanced@52';
function FSOUND_Reverb_SetMix; external FMOD_DLL name '_FSOUND_Reverb_SetMix@8';
function FSOUND_Reverb_GetEnvironment; external FMOD_DLL name '_FSOUND_Reverb_GetEnvironment@16';
function FSOUND_Reverb_GetEnvironmentAdvanced; external FMOD_DLL name '_FSOUND_Reverb_GetEnvironmentAdvanced@52';
function FSOUND_Reverb_GetMix; external FMOD_DLL name '_FSOUND_Reverb_GetMix@8';
function FSOUND_Record_SetDriver; external FMOD_DLL name '_FSOUND_Record_SetDriver@4';
function FSOUND_Record_GetNumDrivers; external FMOD_DLL name '_FSOUND_Record_GetNumDrivers@0';
function FSOUND_Record_GetDriverName; external FMOD_DLL name '_FSOUND_Record_GetDriverName@4';
function FSOUND_Record_GetDriver; external FMOD_DLL name '_FSOUND_Record_GetDriver@0';
function FSOUND_Record_StartSample; external FMOD_DLL name '_FSOUND_Record_StartSample@8';
function FSOUND_Record_Stop; external FMOD_DLL name '_FSOUND_Record_Stop@0';
function FSOUND_Record_GetPosition; external FMOD_DLL name '_FSOUND_Record_GetPosition@0';
procedure FSOUND_File_SetCallbacks; external FMOD_DLL name '_FSOUND_File_SetCallbacks@20';
function FMUSIC_LoadSong; external FMOD_DLL name '_FMUSIC_LoadSong@4';
function FMUSIC_LoadSongMemory; external FMOD_DLL name '_FMUSIC_LoadSongMemory@8';
function FMUSIC_FreeSong; external FMOD_DLL name '_FMUSIC_FreeSong@4';
function FMUSIC_PlaySong; external FMOD_DLL name '_FMUSIC_PlaySong@4';
function FMUSIC_StopSong; external FMOD_DLL name '_FMUSIC_StopSong@4';
procedure FMUSIC_StopAllSongs; external FMOD_DLL name '_FMUSIC_StopAllSongs@0';
function FMUSIC_SetZxxCallback; external FMOD_DLL name '_FMUSIC_SetZxxCallback@8';
function FMUSIC_SetRowCallback; external FMOD_DLL name '_FMUSIC_SetRowCallback@12';
function FMUSIC_SetOrderCallback; external FMOD_DLL name '_FMUSIC_SetOrderCallback@12';
function FMUSIC_SetInstCallback; external FMOD_DLL name '_FMUSIC_SetInstCallback@12';
function FMUSIC_SetSample; external FMOD_DLL name '_FMUSIC_SetSample@12';
function FMUSIC_OptimizeChannels; external FMOD_DLL name '_FMUSIC_OptimizeChannels@12';
function FMUSIC_SetReverb; external FMOD_DLL name '_FMUSIC_SetReverb@4';
function FMUSIC_SetOrder; external FMOD_DLL name '_FMUSIC_SetOrder@8';
function FMUSIC_SetPaused; external FMOD_DLL name '_FMUSIC_SetPaused@8';
function FMUSIC_SetMasterVolume; external FMOD_DLL name '_FMUSIC_SetMasterVolume@8';
function FMUSIC_SetPanSeperation; external FMOD_DLL name '_FMUSIC_SetPanSeperation@8';
function FMUSIC_GetName; external FMOD_DLL name '_FMUSIC_GetName@4';
function FMUSIC_GetType; external FMOD_DLL name '_FMUSIC_GetType@4';
function FMUSIC_GetNumOrders; external FMOD_DLL name '_FMUSIC_GetNumOrders@4';
function FMUSIC_GetNumPatterns; external FMOD_DLL name '_FMUSIC_GetNumPatterns@4';
function FMUSIC_GetNumInstruments; external FMOD_DLL name '_FMUSIC_GetNumInstruments@4';
function FMUSIC_GetNumSamples; external FMOD_DLL name '_FMUSIC_GetNumSamples@4';
function FMUSIC_GetNumChannels; external FMOD_DLL name '_FMUSIC_GetNumChannels@4';
function FMUSIC_GetSample; external FMOD_DLL name '_FMUSIC_GetSample@8';
function FMUSIC_GetPatternLength; external FMOD_DLL name '_FMUSIC_GetPatternLength@8';
function FMUSIC_IsFinished; external FMOD_DLL name '_FMUSIC_IsFinished@4';
function FMUSIC_IsPlaying; external FMOD_DLL name '_FMUSIC_IsPlaying@4';
function FMUSIC_GetMasterVolume; external FMOD_DLL name '_FMUSIC_GetMasterVolume@4';
function FMUSIC_GetGlobalVolume; external FMOD_DLL name '_FMUSIC_GetGlobalVolume@4';
function FMUSIC_GetOrder; external FMOD_DLL name '_FMUSIC_GetOrder@4';
function FMUSIC_GetPattern; external FMOD_DLL name '_FMUSIC_GetPattern@4';
function FMUSIC_GetSpeed; external FMOD_DLL name '_FMUSIC_GetSpeed@4';
function FMUSIC_GetBPM; external FMOD_DLL name '_FMUSIC_GetBPM@4';
function FMUSIC_GetRow; external FMOD_DLL name '_FMUSIC_GetRow@4';
function FMUSIC_GetPaused; external FMOD_DLL name '_FMUSIC_GetPaused@4';
function FMUSIC_GetTime; external FMOD_DLL name '_FMUSIC_GetTime@4';

end.

