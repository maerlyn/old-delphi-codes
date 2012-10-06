{ =============================================================================================== }
{ FMOD Main header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2002.               }
{ =============================================================================================== }
{
  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.5.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.5.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.5.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.5.so libfmod.so.
}
{ =============================================================================================== }
{ HISTORY                                                                                         }
{ =============================================================================================== }
{
  10-Jul-2002 Sly
  - Added ability to load library dynamically
  - Added FMOD_Load and FMOD_Unload functions
  - Still needs a bit of help with the Linux version.  See FIXME comments
    for details

  11-Jul-2002 Sly
  - Cleaned up uses clauses
  - Added optional assertions to find missing functions
  - Assertions found the redundant FSOUND_Geometry_List_Add, so it was removed

  21-Jul-2002 Sly
  - Added Linux functionality for loading functions from a shared object library
}
{ =============================================================================================== }

unit fmod;

{
  Define FMOD_DYNAMIC_LOAD to load the FMOD library dynamically
  Call FMOD_Load to load the library and FMOD_Unload to unload.
}
{.$DEFINE FMOD_DYNAMIC_LOAD}

{
  Disable assertions by changing the following compiler directive to OFF.
  Assertions are used to check the functions are correctly loaded when using
  dynamic loading.
}
{$ASSERTIONS ON}

interface

{$IFDEF WIN32}
uses
  Windows;
{$ENDIF}

{ =============================================================================================== }
{ DEFINITIONS                                                                                     }
{ =============================================================================================== }

{
  Force four-byte enums
}
{$Z4}

const
  FMOD_VERSION: Single = 3.5;

{
  FMOD defined types
}

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

  {
    Callback types
  }

  TFSoundStreamCallback   = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool; cdecl;
  TFSoundDSPCallback      = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; cdecl;
  TFMusicCallback         = procedure (Module: PFMusicModule; Param: Byte); cdecl;

  TFSoundOpenCallback     = function (Name: PChar): Cardinal; cdecl;
  TFSoundCloseCallback    = procedure (Handle: Cardinal); cdecl;
  TFSoundReadCallback     = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal; cdecl;
  TFSoundSeekCallback     = procedure (Handle: Cardinal; Pos: Cardinal; Mode: Byte); cdecl;
  TFSoundTellCallback     = function (Handle: Cardinal): Cardinal; cdecl;

  TFSoundAllocCallback    = function(Size: Cardinal): Pointer; cdecl;
  TFSoundReallocCallback  = function(Ptr: Pointer; Size: Cardinal): Pointer; cdecl;
  TFSoundFreeCallback     = procedure(Ptr: Pointer);

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
    FMOD_ERR_NONE,              // No errors
    FMOD_ERR_BUSY,              // Cannot call this command after FSOUND_Init.  Call FSOUND_Close first.
    FMOD_ERR_UNINITIALIZED,     // This command failed because FSOUND_Init was not called
    FMOD_ERR_INIT,              // Error initializing output device.
    FMOD_ERR_ALLOCATED,         // Error initializing output device, but more specifically, the output device is already in use and cannot be reused.
    FMOD_ERR_PLAY,              // Playing the sound failed.
    FMOD_ERR_OUTPUT_FORMAT,     // Soundcard does not support the features needed for this soundsystem (16bit stereo output)
    FMOD_ERR_COOPERATIVELEVEL,  // Error setting cooperative level for hardware.
    FMOD_ERR_CREATEBUFFER,      // Error creating hardware sound buffer.
    FMOD_ERR_FILE_NOTFOUND,     // File not found
    FMOD_ERR_FILE_FORMAT,       // Unknown file format
    FMOD_ERR_FILE_BAD,          // Error loading file
    FMOD_ERR_MEMORY,            // Not enough memory or resources
    FMOD_ERR_VERSION,           // The version number of this file format is not supported
    FMOD_ERR_INVALID_PARAM,     // An invalid parameter was passed to this function
    FMOD_ERR_NO_EAX,            // Tried to use an EAX command on a non EAX enabled channel or output.
    FMOD_ERR_CHANNEL_ALLOC,     // Failed to allocate a new channel
    FMOD_ERR_RECORD,            // Recording is not supported on this machine
    FMOD_ERR_MEDIAPLAYER        // Required Mediaplayer codec is not installed
  );

{
[ENUM]
[
  [DESCRIPTION]
  These output types are used with FSOUND_SetOutput, to choose which output driver to use.

	FSOUND_OUTPUT_DSOUND will not support hardware 3d acceleration if the sound card driver
	does not support DirectX 6 Voice Manager Extensions.

  FSOUND_OUTPUT_WINMM is recommended for NT and CE.

  [SEE_ALSO]
  FSOUND_SetOutput
  FSOUND_GetOutput
]
}

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND,  // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM,    // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND,   // DirectSound driver.  You need this to get EAX2 or EAX3 support, or FX api support.
    FSOUND_OUTPUT_A3D,      // A3D driver.  You need this to get geometry support.

    FSOUND_OUTPUT_OSS,      // Linux/Unix OSS (Open Sound System) driver, i.e. the kernel sound drivers.
    FSOUND_OUTPUT_ESD,      // Linux/Unix ESD (Enlightment Sound Daemon) driver.
    FSOUND_OUTPUT_ALSA,     // Linux Alsa driver.

    FSOUND_OUTPUT_XBOX      // Xbox driver
  );

{
[ENUM]
[
  [DESCRIPTION]
  These mixer types are used with FSOUND_SetMixer, to choose which mixer to use, or to act
  upon for other reasons using FSOUND_GetMixer.
  It is not necessary to set the mixer.  FMOD will autodetect the best mixer for you.

  [SEE_ALSO]
  FSOUND_SetMixer
  FSOUND_GetMixer
]
}
type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,        // Enables autodetection of the fastest mixer based on your cpu.
    FSOUND_MIXER_BLENDMODE,         // Enables the standard non mmx, blendmode mixer.
    FSOUND_MIXER_MMXP5,             // Enables the mmx, pentium optimized blendmode mixer.
    FSOUND_MIXER_MMXP6,             // Enables the mmx, ppro/p2/p3 optimized mixer.

    FSOUND_MIXER_QUALITY_AUTODETECT,// Enables autodetection of the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU,       // Enables the interpolating FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5,     // Enables the interpolating p5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6,     // Enables the interpolating ppro/p2/p3 MMX mixer.

    FSOUND_MIXER_MONO,              // Windows CE only - MONO non interpolating/low quality mixer. For speed
    FSOUND_MIXER_QUALITY_MONO       // Windows CE only - MONO Interpolating mixer.  For speed
  );

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
    FMUSIC_TYPE_XM,   // FastTracker 2
    FMUSIC_TYPE_IT,   // Impulse Tracker
    FMUSIC_TYPE_MIDI  // MIDI file
  );

{
[DEFINE_START]
[
  [NAME]
  FSOUND_DSP_PRIORITIES

  [DESCRIPTION]
  These default priorities are used by FMOD internal system DSP units.  They describe the
  position of the DSP chain, and the order of how audio processing is executed.
  You can actually through the use of FSOUND_DSP_GetxxxUnit (where xxx is the name of the DSP
  unit), disable or even change the priority of a DSP unit.

  [SEE_ALSO]
  FSOUND_DSP_Create
  FSOUND_DSP_SetPriority
  FSOUND_DSP_GetSpectrum
]
}
const
  FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT        = 0;    // DSP CLEAR unit - done first
  FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT          = 100;  // DSP SFX unit - done second
  FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT        = 200;  // DSP MUSIC unit - done third
  FSOUND_DSP_DEFAULTPRIORITY_USER             = 300;  // User priority, use this as reference
  FSOUND_DSP_DEFAULTPRIORITY_FFTUNIT          = 900;  // This reads data for FSOUND_DSP_GetSpectrum, so it comes after user units
  FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT  = 1000; // DSP CLIP AND COPY unit - last
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
  FSOUND_CAPS_HARDWARE              = $1;  // This driver supports hardware accelerated 3d sound.
  FSOUND_CAPS_EAX2                  = $2;  // This driver supports EAX 2 reverb
  FSOUND_CAPS_GEOMETRY_OCCLUSIONS   = $4;  // This driver supports (A3D) geometry occlusions
  FSOUND_CAPS_GEOMETRY_REFLECTIONS  = $8;  // This driver supports (A3D) geometry reflections
  FSOUND_CAPS_EAX3                  = $10; // This driver supports EAX 3 reverb
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_MODES

  [DESCRIPTION]
  Sample description bitfields, OR them together for loading and describing samples.
    NOTE.  If the file format being loaded already has a defined format, such as WAV or MP3, then
    trying to override the pre-defined format with a new set of format flags will not work.  For
    example, an 8 bit WAV file will not load as 16bit if you specify FSOUND_16BITS.  It will just
    ignore the flag and go ahead loading it as 8bits.  For these type of formats the only flags
    you can specify that will really alter the behaviour of how it is loaded, are the following.

    FSOUND_LOOP_OFF
    FSOUND_LOOP_NORMAL
    FSOUND_LOOP_BIDI
    FSOUND_HW3D
    FSOUND_2D
    FSOUND_STREAMABLE
    FSOUND_LOADMEMORY
    FSOUND_LOADRAW
    FSOUND_MPEGACCURATE

    See flag descriptions for what these do.
]
}
const
  FSOUND_LOOP_OFF     = $00000001;  // For non looping samples.
  FSOUND_LOOP_NORMAL  = $00000002;  // For forward looping samples.
  FSOUND_LOOP_BIDI    = $00000004;  // For bidirectional looping samples.  (no effect if in hardware).
  FSOUND_8BITS        = $00000008;  // For 8 bit samples.
  FSOUND_16BITS       = $00000010;  // For 16 bit samples.
  FSOUND_MONO         = $00000020;  // For mono samples.
  FSOUND_STEREO       = $00000040;  // For stereo samples.
  FSOUND_UNSIGNED     = $00000080;  // For user created source data containing unsigned samples.
  FSOUND_SIGNED       = $00000100;  // For user created source data containing signed data.
  FSOUND_DELTA        = $00000200;  // For user created source data stored as delta values.
  FSOUND_IT214        = $00000400;  // For user created source data stored using IT214 compression.
  FSOUND_IT215        = $00000800;  // For user created source data stored using IT215 compression.
  FSOUND_HW3D         = $00001000;  // Attempts to make samples use 3d hardware acceleration. (if the card supports it)
  FSOUND_2D           = $00002000;  // Ignores any 3d processing.  Overrides FSOUND_HW3D.  Located in software.
  FSOUND_STREAMABLE   = $00004000;  // For a streamomg sound where you feed the data to it.  If you dont supply this sound may come out corrupted.  (only affects a3d output)
  FSOUND_LOADMEMORY   = $00008000;  // "name" will be interpreted as a pointer to data for streaming and samples.
  FSOUND_LOADRAW      = $00010000;  // Will ignore file format and treat as raw pcm.
  FSOUND_MPEGACCURATE = $00020000;  // For FSOUND_Stream_OpenFile - for accurate FSOUND_Stream_GetLengthMs/FSOUND_Stream_SetTime.  WARNING, see FSOUND_Stream_OpenFile for inital opening time performance issues.
  FSOUND_FORCEMONO    = $00040000;  // For forcing stereo streams and samples to be mono - needed if using FSOUND_HW3D and stereo data - incurs a small speed hit for streams
  FSOUND_HW2D         = $00080000;  // 2D hardware sounds.  allows hardware specific effects
  FSOUND_ENABLEFX     = $00100000;  // Allows DX8 FX to be played back on a sound.  Requires DirectX 8 - Note these sounds cannot be played more than once, be 8 bit, be less than a certain size, or have a changing frequency

  FSOUND_MPEGHALFRATE = $00200000;  // For FMODCE only - decodes mpeg streams using a lower quality decode, but faster execution
  FSOUND_XADPCM       = $00400000;  // For XBOX only - Describes a user sample that its contents are compressed as XADPCM

{
    FSOUND_NORMAL is a default sample type.  Loop off, 8bit mono, signed, not hardware
    accelerated.  Some API functions ignore 8bits and mono, as it may be an mpeg/wav/etc which
    has its format predetermined.
}
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
  FSOUND_FREE           = -1;     // value to play on any free channel, or to allocate a sample in a free sample slot.
  FSOUND_UNMANAGED      = -2;     // value to allocate a sample that is NOT managed by FSOUND or placed in a sample slot.
  FSOUND_ALL            = -3;     // for a channel index , this flag will affect ALL channels available! Not supported by every function.
  FSOUND_STEREOPAN      = -1;     // value for FSOUND_SetPan so that stereo sounds are not played at half volume. See FSOUND_SetPan for more on this.
  FSOUND_SYSTEM_CHANNEL = -1000;  // special 'channel' ID for all channel based functions that want to alter the global FSOUND software mixing output
// [DEFINE_END]


{
[STRUCT_START]
[
    [NAME]
    FSOUND_REVERB_PROPERTIES

    [DESCRIPTION]
    Structure defining a reverb environment.

    [REMARKS]
    For more indepth descriptions of the reverb properties under win32, please see the EAX2/EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.
    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Linux and FMODCE do not support the reverb api.

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
    FSOUND_Reverb_GetProperties
    FSOUND_REVERB_PROPERTYFLAGS
]
}
type
  TFSoundReverbProperties = record          // MIN     MAX    DEFAULT DESCRIPTION
    Environment: Cardinal;                  // 0       25     0       sets all listener properties (win32 only)
    EnvSize: Single;                        // 1.0     100.0  7.5     environment size in meters (win32 only)
    EnvDiffusion: Single;                   // 0.0     1.0    1.0     environment diffusion (win32/xbox)
    Room: Integer;                          // -10000  0      -1000   room effect level (at mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      -100    relative room effect level at high frequencies (win32/xbox)
    RoomLF: Integer;                        // -10000  0      0       relative room effect level at low frequencies (win32 only)
    DecayTime: Single;                      // 0.1     20.0   1.49    reverberation decay time at mid frequencies (win32/xbox)
    DecayHFRatio: Single;                   // 0.1     2.0    0.83    high-frequency to mid-frequency decay time ratio (win32/xbox)
    DecayLFRatio: Single;                   // 0.1     2.0    1.0     low-frequency to mid-frequency decay time ratio (win32 only)
    Reflections: Integer;                   // -10000  1000   -2602   early reflections level relative to room effect (win32/xbox)
    ReflectionsDelay: Single;               // 0.0     0.3    0.007   initial reflection delay time (win32/xbox)
    ReflectionsPan: array [0..2] of Single; //                0,0,0   early reflections panning vector (win32 only)
    Reverb: Integer;                        // -10000  2000   200     late reverberation level relative to room effect (win32/xbox)
    ReverbDelay: Single;                    // 0.0     0.1    0.011   late reverberation delay time relative to initial reflection (win32/xbox)
    ReverbPan: array [0..2] of Single;      //                0,0,0   late reverberation panning vector (win32 only)
    EchoTime: Single;                       // .075    0.25   0.25    echo time (win32 only)
    EchoDepth: Single;                      // 0.0     1.0    0.0     echo depth (win32 only)
    ModulationTime: Single;                 // 0.04    4.0    0.25    modulation time (win32 only)
    ModulationDepth: Single;                // 0.0     1.0    0.0     modulation depth (win32 only)
    AirAbsorptionHF: Single;                // -100    0.0    -5.0    change in level per meter at high frequencies (win32 only)
    HFReference: Single;                    // 1000.0  20000  5000.0  reference high frequency (hz) (win32/xbox)
    LFReference: Single;                    // 20.0    1000.0 250.0   reference low frequency (hz) (win32 only)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like FSOUND_3D_Listener_SetRolloffFactor but for room effect (win32/xbox)
    Diffusion: Single;                      // 0.0     100.0  100.0   Value that controls the echo density in the late reverberation decay. (xbox only)
    Density: Single;                        // 0.0     100.0  100.0   Value that controls the modal density in the late reverberation decay (xbox only)
    Flags: Cardinal;                        // FSOUND_REVERB_PROPERTYFLAGS - modifies the behavior of above properties (win32 only)
  end;
// [STRUCT_END]


{
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_FLAGS

    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_PROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_PROPERTIES
]
}
const
  FSOUND_REVERBFLAGS_DECAYTIMESCALE         = $00000001;  // EnvironmentSize affects reverberation decay time
  FSOUND_REVERBFLAGS_REFLECTIONSSCALE       = $00000002;  // EnvironmentSize affects reflection level
  FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE  = $00000004;  // EnvironmentSize affects initial reflection delay time
  FSOUND_REVERBFLAGS_REVERBSCALE            = $00000008;  // EnvironmentSize affects reflections level
  FSOUND_REVERBFLAGS_REVERBDELAYSCALE       = $00000010;  // EnvironmentSize affects late reverberation delay time
  FSOUND_REVERBFLAGS_DECAYHFLIMIT           = $00000020;  // AirAbsorptionHF affects DecayHFRatio
  FSOUND_REVERBFLAGS_ECHOTIMESCALE          = $00000040;  // EnvironmentSize affects echo time
  FSOUND_REVERBFLAGS_MODULATIONTIMESCALE    = $00000080;  // EnvironmentSize affects modulation time
  FSOUND_REVERBFLAGS_DEFAULT                = FSOUND_REVERBFLAGS_DECAYTIMESCALE or FSOUND_REVERBFLAGS_REFLECTIONSSCALE or
                                              FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE or FSOUND_REVERBFLAGS_REVERBSCALE or
                                              FSOUND_REVERBFLAGS_REVERBDELAYSCALE or FSOUND_REVERBFLAGS_DECAYHFLIMIT;
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
//                                 Env  Size    Diffus  Room   RoomHF  RmLF DecTm   DecHF  DecLF   Refl  RefDel  RefPan           Revb  RevDel  ReverbPan       EchoTm  EchDp  ModTm  ModDp  AirAbs  HFRef    LFRef  RRlOff Diffus  Densty  FLAGS 
  FSOUND_PRESET_OFF              = 0,	7.5f,	1.00f, -10000, -10000, 0,   1.00f,  1.00f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,   0.0f,   0.0f, 0x3f ;
  FSOUND_PRESET_GENERIC          = 0,	7.5f,	1.00f, -1000,  -100,   0,   1.49f,  0.83f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PADDEDCELL       = 1,	1.4f,	1.00f, -1000,  -6000,  0,   0.17f,  0.10f, 1.0f,  -1204, 0.001f, 0.0f,0.0f,0.0f,   207, 0.002f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ROOM             = 2,	1.9f,	1.00f, -1000,  -454,   0,   0.40f,  0.83f, 1.0f,  -1646, 0.002f, 0.0f,0.0f,0.0f,    53, 0.003f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_BATHROOM         = 3,	1.4f,	1.00f, -1000,  -1200,  0,   1.49f,  0.54f, 1.0f,   -370, 0.007f, 0.0f,0.0f,0.0f,  1030, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_LIVINGROOM       = 4,	2.5f,	1.00f, -1000,  -6000,  0,   0.50f,  0.10f, 1.0f,  -1376, 0.003f, 0.0f,0.0f,0.0f, -1104, 0.004f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONEROOM        = 5,	11.6f,	1.00f, -1000,  -300,   0,   2.31f,  0.64f, 1.0f,   -711, 0.012f, 0.0f,0.0f,0.0f,    83, 0.017f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_AUDITORIUM       = 6,	21.6f,	1.00f, -1000,  -476,   0,   4.32f,  0.59f, 1.0f,   -789, 0.020f, 0.0f,0.0f,0.0f,  -289, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CONCERTHALL      = 7,	19.6f,	1.00f, -1000,  -500,   0,   3.92f,  0.70f, 1.0f,  -1230, 0.020f, 0.0f,0.0f,0.0f,    -2, 0.029f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CAVE             = 8,	14.6f,	1.00f, -1000,  0,      0,   2.91f,  1.30f, 1.0f,   -602, 0.015f, 0.0f,0.0f,0.0f,  -302, 0.022f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_ARENA            = 9,	36.2f,	1.00f, -1000,  -698,   0,   7.24f,  0.33f, 1.0f,  -1166, 0.020f, 0.0f,0.0f,0.0f,    16, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HANGAR           = 10,	50.3f,	1.00f, -1000,  -1000,  0,   10.05f, 0.23f, 1.0f,   -602, 0.020f, 0.0f,0.0f,0.0f,   198, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CARPETTEDHALLWAY = 11,	1.9f,	1.00f, -1000,  -4000,  0,   0.30f,  0.10f, 1.0f,  -1831, 0.002f, 0.0f,0.0f,0.0f, -1630, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HALLWAY          = 12,	1.8f,	1.00f, -1000,  -300,   0,   1.49f,  0.59f, 1.0f,  -1219, 0.007f, 0.0f,0.0f,0.0f,   441, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONECORRIDOR    = 13,	13.5f,	1.00f, -1000,  -237,   0,   2.70f,  0.79f, 1.0f,  -1214, 0.013f, 0.0f,0.0f,0.0f,   395, 0.020f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ALLEY 	         = 14,	7.5f,	0.30f, -1000,  -270,   0,   1.49f,  0.86f, 1.0f,  -1204, 0.007f, 0.0f,0.0f,0.0f,    -4, 0.011f, 0.0f,0.0f,0.0f, 0.125f, 0.95f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_FOREST 	         = 15,	38.0f,	0.30f, -1000,  -3300,  0,   1.49f,  0.54f, 1.0f,  -2560, 0.162f, 0.0f,0.0f,0.0f,  -229, 0.088f, 0.0f,0.0f,0.0f, 0.125f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  79.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CITY             = 16,	7.5f,	0.50f, -1000,  -800,   0,   1.49f,  0.67f, 1.0f,  -2273, 0.007f, 0.0f,0.0f,0.0f, -1691, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  50.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_MOUNTAINS        = 17,	100.0f, 0.27f, -1000,  -2500,  0,   1.49f,  0.21f, 1.0f,  -2780, 0.300f, 0.0f,0.0f,0.0f, -1434, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  27.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_QUARRY           = 18,	17.5f,	1.00f, -1000,  -1000,  0,   1.49f,  0.83f, 1.0f, -10000, 0.061f, 0.0f,0.0f,0.0f,   500, 0.025f, 0.0f,0.0f,0.0f, 0.125f, 0.70f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PLAIN            = 19,	42.5f,	0.21f, -1000,  -2000,  0,   1.49f,  0.50f, 1.0f,  -2466, 0.179f, 0.0f,0.0f,0.0f, -1926, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  21.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PARKINGLOT       = 20,	8.3f,	1.00f, -1000,  0,      0,   1.65f,  1.50f, 1.0f,  -1363, 0.008f, 0.0f,0.0f,0.0f, -1153, 0.012f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_SEWERPIPE        = 21,	1.7f,	0.80f, -1000,  -1000,  0,   2.81f,  0.14f, 1.0f,    429, 0.014f, 0.0f,0.0f,0.0f,  1023, 0.021f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  80.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_UNDERWATER       = 22,	1.8f,	1.00f, -1000,  -4000,  0,   1.49f,  0.10f, 1.0f,   -449, 0.007f, 0.0f,0.0f,0.0f,  1700, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 1.18f, 0.348f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;

// Non I3DL2 presets 

  FSOUND_PRESET_DRUGGED          = 23,	1.9f,	0.50f, -1000,  0,      0,   8.39f,  1.39f, 1.0f,  -115,  0.002f, 0.0f,0.0f,0.0f,   985, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_DIZZY            = 24,	1.8f,	0.60f, -1000,  -400,   0,   17.23f, 0.56f, 1.0f,  -1713, 0.020f, 0.0f,0.0f,0.0f,  -613, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.81f, 0.310f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_PSYCHOTIC        = 25,	1.0f,	0.50f, -1000,  -151,   0,   7.56f,  0.91f, 1.0f,  -626,  0.020f, 0.0f,0.0f,0.0f,   774, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 4.00f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
}
// [DEFINE_END]


{
[STRUCT_START]
[
    [NAME]
    FSOUND_REVERB_CHANNELPROPERTIES

    [DESCRIPTION]
    Structure defining the properties for a reverb source, related to a FSOUND channel.

    [REMARKS]
    For more indepth descriptions of the reverb properties under win32, please see the EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.
    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Linux and FMODCE do not support the reverb api.

    [SEE_ALSO]
    FSOUND_Reverb_SetChannelProperties
    FSOUND_Reverb_GetChannelProperties
    FSOUND_REVERB_PROPERTYFLAGS
]
}
type
  TFSoundReverbChannelProperties = record   // MIN     MAX    DEFAULT
    Direct: Integer;                        // -10000  1000   0       direct path level (at low and mid frequencies) (win32/xbox)
    DirectHF: Integer;                      // -10000  0      0       relative direct path level at high frequencies (win32/xbox)
    Room: Integer;                          // -10000  1000   0       room effect level (at low and mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      0       relative room effect level at high frequencies (win32/xbox)
    Obstruction: Integer;                   // -10000  0      0       main obstruction control (attenuation at high frequencies)  (win32/xbox)
    ObstructionLFRatio: Single;             // 0.0     1.0    0.0     obstruction low-frequency level re. main control (win32/xbox)
    Occlusion: Integer;                     // -10000  0      0       main occlusion control (attenuation at high frequencies) (win32/xbox)
    OcclusionLFRatio: Single;               // 0.0     1.0    0.25    occlusion low-frequency level re. main control (win32/xbox)
    OcclusionRoomRatio: Single;             // 0.0     10.0   1.5     relative occlusion control for room effect (win32)
    OcclusionDirectRatio: Single;           // 0.0     10.0   1.0     relative occlusion control for direct path (win32)
    Exclusion: Integer;                     // -10000  0      0       main exlusion control (attenuation at high frequencies) (win32)
    ExclusionLFRatio: Single;               // 0.0     1.0    1.0     exclusion low-frequency level re. main control (win32)
    OutsideVolumeHF: Integer;               // -10000  0      0       outside sound cone level at high frequencies (win32)
    DopplerFactor: Single;                  // 0.0     10.0   0.0     like DS3D flDopplerFactor but per source (win32)
    RolloffFactor: Single;                  // 0.0     10.0   0.0     like DS3D flRolloffFactor but per source (win32)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like DS3D flRolloffFactor but for room effect (win32/xbox)
    AirAbsorptionFactor: Single;            // 0.0     10.0   1.0     multiplies AirAbsorptionHF member of FSOUND_REVERB_PROPERTIES (win32)
    Flags: Integer;                         // FSOUND_REVERB_CHANNELFLAGS - modifies the behavior of properties (win32)
  end;
// [STRUCT_END]

{
[DEFINE_START] 
[
    [NAME] 
    FSOUND_REVERB_CHANNELFLAGS
    
    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_CHANNELPROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_PROPERTIES
]
}
const
  FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO  = $01;  // Automatic setting of 'Direct'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO      = $02;  // Automatic setting of 'Room'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO    = $04;  // Automatic setting of 'RoomHF' due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_DEFAULT       = FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO or 
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO or
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO;
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
  FSOUND_GEOMETRY_NORMAL            = $00;  // Default geometry type. Occluding polygon
  FSOUND_GEOMETRY_REFLECTIVE        = $01;  // This polygon is reflective
  FSOUND_GEOMETRY_OPENING           = $02;  // Overlays a transparency over the previous polygon. The 'openingfactor' value supplied is copied internally.
  FSOUND_GEOMETRY_OPENING_REFERENCE = $04;  // Overlays a transparency over the previous polygon. The 'openingfactor' supplied is pointed to (for access when building a list)
// [DEFINE_END]

{
[DEFINE_START]
[
[ENUM] 
[
	[DESCRIPTION]
    These values are used with FSOUND_FX_Enable to enable DirectX 8 FX for a channel.

	[SEE_ALSO]
    FSOUND_FX_Enable
    FSOUND_FX_SetChorus
    FSOUND_FX_SetCompressor
    FSOUND_FX_SetDistortion
    FSOUND_FX_SetEcho
    FSOUND_FX_SetFlanger
    FSOUND_FX_SetGargle
    FSOUND_FX_SetI3DL2Reverb
    FSOUND_FX_SetParamEQ
    FSOUND_FX_SetWavesReverb
]
}

type
  TFSoundFXModes = (
    FSOUND_FX_CHORUS,
    FSOUND_FX_COMPRESSOR,
    FSOUND_FX_DISTORTION,
    FSOUND_FX_ECHO,
    FSOUND_FX_FLANGER,
    FSOUND_FX_GARGLE,
    FSOUND_FX_I3DL2REVERB,
    FSOUND_FX_PARAMEQ,
    FSOUND_FX_WAVES_REVERB
  );
// [DEFINE_END]


{
[ENUM]
[
	[DESCRIPTION]
	These are speaker types defined for use with the FSOUND_SetSpeakerMode command.
    Note that this only works with FSOUND_OUTPUT_DSOUND output mode.

	[SEE_ALSO]
    FSOUND_SetSpeakerMode

    [REMARKS]
    Only works with FSOUND_OUTPUT_DSOUND output mode.
]
}
type
  TFSoundSpeakerModes =
  (
    FSOUND_SPEAKERMODE_5POINT1,       // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
    FSOUND_SPEAKERMODE_HEADPHONE,     // The speakers are headphones.
    FSOUND_SPEAKERMODE_MONO,          // The speakers are monaural.
    FSOUND_SPEAKERMODE_QUAD,          // The speakers are quadraphonic.
    FSOUND_SPEAKERMODE_STEREO,        // The speakers are stereo (default value).
    FSOUND_SPEAKERMODE_SURROUND       // The speakers are surround sound.
  );
  FSOUND_SPEAKERMODES = TFSoundSpeakerModes;


{
[DEFINE_START]
[
    [NAME]
    FSOUND_INIT_FLAGS

    [DESCRIPTION]
    Initialization flags.  Use them with FSOUND_Init in the flags parameter to change various behaviour.

    FSOUND_INIT_ENABLEOUTPUTFX Is an init mode which enables the FSOUND mixer buffer to be affected by DirectX 8 effects.
    Note that due to limitations of DirectSound, FSOUND_Init may fail if this is enabled because the buffersize is too small.
    This can be fixed with FSOUND_SetBufferSize.  Increase the BufferSize until it works.
    When it is enabled you can use the FSOUND_FX api, and use FSOUND_SYSTEMCHANNEL as the channel id when setting parameters.

    [SEE_ALSO]
    FSOUND_Init
]
}
const
  FSOUND_INIT_USEDEFAULTMIDISYNTH  = $01;  // Causes MIDI playback to force software decoding.
  FSOUND_INIT_GLOBALFOCUS          = $02;  // For DirectSound output - sound is not muted when window is out of focus.
  FSOUND_INIT_ENABLEOUTPUTFX       = $04;  // For DirectSound output - Allows FSOUND_FX api to be used on global software mixer output!
  FSOUND_INIT_ACCURATEGETCURRENTVU = $08;  // This latency adjusts FSOUND_GetCurrentVU, but incurs a cpu and very small memory hit

// [DEFINE_END]

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

{ ================================== }
{ Library load/unload functions.     }
{ ================================== }

{
  If no library name is passed to FMOD_Load, then the default library name
  used.
}

function FMOD_Load(LibName: PChar = nil): Boolean;
procedure FMOD_Unload;

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_SetOutput: function (OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetDriver: function (Driver: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMixer: function (Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetBufferSize: function (LenMs: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetHWND: function (Hwnd: THandle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMinHardwareChannels: function (Min: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMaxHardwareChannels: function (Max: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMemorySystem: function (Pool: Pointer; PoolLen: Integer;
        UserAlloc: TFSoundAllocCallback;
        UserRealloc: TFSoundReallocCallback;
        UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_SetOutput(OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetDriver(Driver: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMixer(Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetBufferSize(LenMs: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetHWND(Hwnd: THandle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMinHardwareChannels(Min: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMaxHardwareChannels(Max: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMemorySystem(Pool: Pointer; PoolLen: Integer;
        UserAlloc: TFSoundAllocCallback;
        UserRealloc: TFSoundReallocCallback;
        UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Main initialization / closedown functions
  Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
       : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible
         no matter which window is in focus. (FSOUND_OUTPUT_DSOUND only)
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Init: function (MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Close: procedure; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Init(MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Close; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Runtime system level functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_SetSpeakerMode: procedure (SpeakerMode: Cardinal); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSFXMasterVolume: procedure (Volume: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPanSeperation: procedure (PanSep: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_File_SetCallbacks: procedure (
        OpenCallback: TFSoundOpenCallback;
        CloseCallback: TFSoundCloseCallback;
        ReadCallback: TFSoundReadCallback;
        SeekCallback: TFSoundSeekCallback;
        TellCallback: TFSoundTellCallback); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
procedure FSOUND_SetSpeakerMode(SpeakerMode: Cardinal); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetSFXMasterVolume(Volume: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_SetPanSeperation(PanSep: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_File_SetCallbacks(
        OpenCallback: TFSoundOpenCallback;
        CloseCallback: TFSoundCloseCallback;
        ReadCallback: TFSoundReadCallback;
        SeekCallback: TFSoundSeekCallback;
        TellCallback: TFSoundTellCallback); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  System information functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_GetError: function: TFModErrors; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVersion: function: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutput: function: TFSoundOutputTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutputHandle: function: Pointer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriver: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMixer: function: TFSoundMixerTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumDrivers: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverName: function (Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverCaps: function (Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_GetError: TFModErrors; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVersion: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutput: TFSoundOutputTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetOutputHandle: Pointer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriver: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMixer: TFSoundMixerTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumDrivers: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverName(Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetDriverCaps(Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_GetOutputRate: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxChannels: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxSamples: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSFXMasterVolume: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumHardwareChannels: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetChannelsPlaying: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCPUUsage: function: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMemoryStats: Procedure (var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_GetOutputRate: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxChannels: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMaxSamples: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSFXMasterVolume: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetNumHardwareChannels: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetChannelsPlaying: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCPUUsage: Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_GetMemoryStats(var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ =================================== }
{ Sample management / load functions. }
{ =================================== }

{
  Sample creation and management functions
  Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
         Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Sample_Load: function (Index: Integer; const NameOrData: PChar; Mode: Cardinal; MemLength: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Alloc: function (Index: Integer;
    Length: Integer;
    Mode: Cardinal;
    DefFreq: Integer;
    DefVol: Integer;
    DefPan: Integer;
    DefPri: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Free: procedure (Sptr: PFSoundSample); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Upload: function (Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Lock: function (Sptr: PFSoundSample;
    Offset: Integer;
    Length: Integer;
    var Ptr1: Pointer;
    var Ptr2: Pointer;
    var Len1: Cardinal;
    var Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Unlock: function (Sptr: PFSoundSample;
    Ptr1: Pointer;
    Ptr2: Pointer;
    Len1: Cardinal;
    Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Sample_Load(Index: Integer; const NameOrData: PChar; Mode: Cardinal; MemLength: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Alloc(Index: Integer;
  Length: Integer;
  Mode: Cardinal;
  DefFreq: Integer;
  DefVol: Integer;
  DefPan: Integer;
  DefPri: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_Sample_Free(Sptr: PFSoundSample); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Upload(Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Lock(Sptr: PFSoundSample;
  Offset: Integer;
  Length: Integer;
  var Ptr1: Pointer;
  var Ptr2: Pointer;
  var Len1: Cardinal;
  var Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_Unlock(Sptr: PFSoundSample;
  Ptr1: Pointer;
  Ptr2: Pointer;
  Len1: Cardinal;
  Len2: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Sample control functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Sample_SetLoopMode: function (Sptr: PFSoundSample; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetLoopPoints: function (Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaults: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMinMaxDistance: function (Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Sample_SetLoopMode(Sptr: PFSoundSample; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetLoopPoints(Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetDefaults(Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance(Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Sample information functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Sample_Get: function (SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetName: function (Sptr: PFSoundSample): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLength: function (Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLoopPoints: function (Sptr: PFSoundSample;
    var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaults: function (Sptr: PFSoundSample;
    var DefFreq: Integer;
    var DefVol: Integer;
    var DefPan: Integer;
    var DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMode: function (Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Sample_Get(SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetName(Sptr: PFSoundSample): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLength(Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetLoopPoints(Sptr: PFSoundSample;
  var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetDefaults(Sptr: PFSoundSample;
  var DefFreq: Integer;
  var DefVol: Integer;
  var DefPan: Integer;
  var DefPri: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Sample_GetMode(Sptr: PFSoundSample): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ============================ }
{ Channel control functions.   }
{ ============================ }

{
  Playing and stopping sounds.
  Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
         Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_PlaySound: function (Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_PlaySoundEx: function (Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_StopSound: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_PlaySound(Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_PlaySoundEx(Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_StopSound(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Functions to control playback of a channel.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_SetFrequency: function (Channel: Integer; Freq: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolume: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolumeAbsolute: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPan: function (Channel: Integer; Pan: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSurround: function (Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMute: function (Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPriority: function (Channel: Integer; Priority: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetReserved: function (Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPaused: function (Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetLoopMode: function (Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetCurrentPosition: function (Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_SetFrequency(Channel: Integer; Freq: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolume(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetVolumeAbsolute(Channel: Integer; Vol: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPan(Channel: Integer; Pan: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetSurround(Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetMute(Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPriority(Channel: Integer; Priority: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetReserved(Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetPaused(Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetLoopMode(Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_SetCurrentPosition(Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLEOUTPUTFX can be used to apply hardware effect processing to the
      global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
      it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - Stopping or starting a sound resets all FX and they must be re-enabled each time
      if this happens.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_FX_Enable: function (Channel: Integer; Fx: Cardinal): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }

  FSOUND_FX_SetChorus: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetCompressor: function (FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetDistortion: function (FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetEcho: function (FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetFlanger: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetGargle: function (FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetI3DL2Reverb: function (FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetParamEQ: function (FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetWavesReverb: function (FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_FX_Enable(Channel: Integer; Fx: Cardinal): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }

function FSOUND_FX_SetChorus(FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetCompressor(FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetDistortion(FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetEcho(FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetFlanger(FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetGargle(FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb(FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetParamEQ(FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_FX_SetWavesReverb(FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Channel information functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_IsPlaying: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetFrequency: function (Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVolume: function (Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPan: function (Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSurround: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMute: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPriority: function (Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetReserved: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPaused: function (Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetLoopMode: function (Channel: Integer): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentPosition: function (Channel: Integer): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentSample: function (Channel: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentVU: function (Channel: Integer): Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_IsPlaying(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetFrequency(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetVolume(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPan(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetSurround(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetMute(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPriority(Channel: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetReserved(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetPaused(Channel: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetLoopMode(Channel: Integer): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentPosition(Channel: Integer): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentSample(Channel: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_GetCurrentVU(Channel: Integer): Single; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ =================== }
{ 3D sound functions. }
{ =================== }

{
  See also FSOUND_Sample_SetMinMaxDistance (above)
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_3D_Update: procedure; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};  // You must call this once a frame
  FSOUND_3D_SetAttributes: function (Channel: Integer;
    Pos: PFSoundVector;
    Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetAttributes: function (Channel: Integer;
    Pos: PFSoundVector;
    Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetAttributes: procedure (Pos: PFSoundVector;
    Vel: PFSoundVector;
    fx: Single;
    fy: Single;
    fz: Single;
    tx: Single;
    ty: Single;
    tz: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_GetAttributes: procedure (Pos: PFSoundVector;
    Vel: PFSoundVector;
    fx: PSingle;
    fy: PSingle;
    fz: PSingle;
    tx: PSingle;
    ty: PSingle;
    tz: PSingle); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetDopplerFactor: procedure (Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetDistanceFactor: procedure (Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetRolloffFactor: procedure (Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
procedure FSOUND_3D_Update; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};  // You must call this once a frame
function FSOUND_3D_SetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_3D_GetAttributes(Channel: Integer;
  Pos: PFSoundVector;
  Vel: PFSoundVector): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: Single;
  fy: Single;
  fz: Single;
  tx: Single;
  ty: Single;
  tz: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes(Pos: PFSoundVector;
  Vel: PFSoundVector;
  fx: PSingle;
  fy: PSingle;
  fz: PSingle;
  tx: PSingle;
  ty: PSingle;
  tz: PSingle); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetDopplerFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetDistanceFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_3D_Listener_SetRolloffFactor(Scale: Single); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ========================= }
{ File Streaming functions. }
{ ========================= }

{
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_OpenFile to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_OpenFile to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_OpenFile to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
           Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Stream_OpenFile: function(const Filename: PChar; Mode: Cardinal;
    MemLength: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Create: function (Callback: TFSoundStreamCallback;
    Length: Integer;
    Mode: Cardinal;
    SampleRate: Integer;
    UserData: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Play: function(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_PlayEx: function (Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Stop: function(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Close: function(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetEndCallback: function (Stream: PFSoundStream;
    Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSynchCallback: function (Stream: PFSoundStream;
    Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSample: function (Stream: PFSoundStream): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
  FSOUND_Stream_CreateDSP: function (Stream: PFSoundStream; Callback: TFSoundDSPCallback;
    Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetBufferSize: function (Ms: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetPosition: function (Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetPosition: function (Stream: PFSoundStream): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetTime: function (Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTime: function (Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLength: function (Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLengthMs: function (Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Stream_OpenFile(const Filename: PChar; Mode: Cardinal;
  MemLength: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Create(Callback: TFSoundStreamCallback;
  Length: Integer;
  Mode: Cardinal;
  SampleRate: Integer;
  UserData: Integer): PFSoundStream; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Play(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_PlayEx(Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Stop(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_Close(Stream: PFSoundStream): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetEndCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetSynchCallback(Stream: PFSoundStream;
  Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetSample(Stream: PFSoundStream): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
function FSOUND_Stream_CreateDSP(Stream: PFSoundStream; Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetBufferSize(Ms: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetPosition(Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetPosition(Stream: PFSoundStream): Cardinal; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_SetTime(Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetTime(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLength(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Stream_GetLengthMs(Stream: PFSoundStream): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ =================== }
{ CD audio functions. }
{ =================== }

{
  Note: 0 = first drive
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_CD_Play: function (Drive: Byte; Track: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPlayMode: procedure (Drive: Byte; Mode: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Stop: function (Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPaused: function (Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetVolume: function (Drive: Byte; Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Eject: function (Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_CD_Play(Drive: Byte; Track: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_CD_SetPlayMode(Drive: Byte; Mode: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_Stop(Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetPaused(Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_SetVolume(Drive: Byte; Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_Eject(Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_CD_GetPaused: function (Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrack: function (Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetNumTracks: function (Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetVolume: function (Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackLength: function (Drive: Byte; Track: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackTime: function (Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_CD_GetPaused(Drive: Byte): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrack(Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetNumTracks(Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetVolume(Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackLength(Drive: Byte; Track: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_CD_GetTrackTime(Drive: Byte): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ============== }
{ DSP functions. }
{ ============== }

{
  DSP Unit control and information functions.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_DSP_Create: function (Callback: TFSoundDSPCallback;
    Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_Free: procedure (DSPUnit: PFSoundDSPUnit); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetPriority: procedure (DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetPriority: function (DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetActive: procedure (DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetActive: function (DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_DSP_Create(Callback: TFSoundDSPCallback;
  Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_Free(DSPUnit: PFSoundDSPUnit); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetPriority(DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetPriority(DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_SetActive(DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetActive(DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Functions to get hold of FSOUND 'system DSP unit' handles.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_DSP_GetClearUnit: function: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetSFXUnit: function: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetMusicUnit: function: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetClipAndCopyUnit: function: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetFFTUnit: function: PFSoundDSPUnit;
{$ELSE}
function FSOUND_DSP_GetClearUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetSFXUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetMusicUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit: PFSoundDSPUnit; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetFFTUnit: PFSoundDSPUnit;
{$ENDIF}

{
  Miscellaneous DSP functions
  Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
  the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
  It is off by default to save cpu usage.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_DSP_MixBuffers: function (DestBuffer: Pointer;
    SrcBuffer: Pointer;
    Len: Integer;
    Freq: Integer;
    Vol: Integer;
    Pan: Integer;
    Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_ClearMixBuffer: procedure; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetBufferLength: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
  FSOUND_DSP_GetBufferLengthTotal: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
  FSOUND_DSP_GetSpectrum: function: PSingle; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }
{$ELSE}
function FSOUND_DSP_MixBuffers(DestBuffer: Pointer;
  SrcBuffer: Pointer;
  Len: Integer;
  Freq: Integer;
  Vol: Integer;
  Pan: Integer;
  Mode: Cardinal): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_DSP_GetBufferLength: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
function FSOUND_DSP_GetBufferLengthTotal: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
function FSOUND_DSP_GetSpectrum: PSingle; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }
{$ENDIF}

{ ================================================ }
{ Geometry functions.  (NOT SUPPORTED IN LINUX/CE) }
{ ================================================ }

{
  Scene/polygon functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Geometry_AddPolygon: function (P1: PFSoundVector;
    P2: PFSoundVector;
    P3: PFSoundVector;
    P4: PFSoundVector;
    Normal: PFSoundVector;
    Mode: Cardinal;
    OpeningFactor: PSingle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_AddList: function (GeomList: PFSoundGeomList): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Geometry_AddPolygon(P1: PFSoundVector;
  P2: PFSoundVector;
  P3: PFSoundVector;
  P4: PFSoundVector;
  Normal: PFSoundVector;
  Mode: Cardinal;
  OpeningFactor: PSingle): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_AddList(GeomList: PFSoundGeomList): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Polygon list functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Geometry_List_Create: function (BoundingVolume: ByteBool): PFSoundGeomList; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_List_Free: function (GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_List_Begin: function (GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_List_End: function (GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Geometry_List_Create(BoundingVolume: ByteBool): PFSoundGeomList; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_Free(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_Begin(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_List_End(GeomList: PFSoundGeomList): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Material functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Geometry_Material_Create: function: PFSoundMaterial; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_Material_Free: function (Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_Material_SetAttributes: function (Material: PFSoundMaterial;
    ReflectanceGain: Single;
    ReflectanceFreq: Single;
    TransmittanceGain: Single;
    TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_Material_GetAttributes: function (Material: PFSoundMaterial;
    var ReflectanceGain: Single;
    var ReflectanceFreq: Single;
    var TransmittanceGain: Single;
    var TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Geometry_Material_Set: function (Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Geometry_Material_Create: PFSoundMaterial; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_Free(Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_SetAttributes(Material: PFSoundMaterial;
  ReflectanceGain: Single;
  ReflectanceFreq: Single;
  TransmittanceGain: Single;
  TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_GetAttributes(Material: PFSoundMaterial;
  var ReflectanceGain: Single;
  var ReflectanceFreq: Single;
  var TransmittanceGain: Single;
  var TransmittanceFreq: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Geometry_Material_Set(Material: PFSoundMaterial): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ========================================================================== }
{ Reverb functions. (eax2/3 reverb)  (NOT SUPPORTED IN LINUX/CE)               }
{ ========================================================================== }

{
  See structures above for definitions and information on the reverb parameters.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Reverb_SetProperties: function (var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetProperties: function (var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_SetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Reverb_SetProperties(var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetProperties(var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_SetChannelProperties(Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Reverb_GetChannelProperties(Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ================================================ }
{ Recording functions  (NOT SUPPORTED IN LINUX/CE) }
{ ================================================ }

{
  Recording initialization functions
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Record_SetDriver: function (OutputType: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetNumDrivers: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriverName: function (Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriver: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Record_SetDriver(OutputType: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetNumDrivers: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriverName(Id: Integer): PChar; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetDriver: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Recording functionality. Only one recording session will work at a time.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FSOUND_Record_StartSample: function (Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_Stop: function: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetPosition: function: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FSOUND_Record_StartSample(Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_Stop: ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FSOUND_Record_GetPosition: Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{ ============================================================================================= }
{ FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      }
{ ============================================================================================= }

{
  Song management / playback functions.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_LoadSong: function (const Name: PChar): PFMusicModule; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_LoadSongMemory: function (Data: Pointer; Length: Integer): PFMusicModule; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_FreeSong: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_PlaySong: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopSong: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopAllSongs: procedure; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_LoadSong(const Name: PChar): PFMusicModule; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_LoadSongMemory(Data: Pointer; Length: Integer): PFMusicModule; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_FreeSong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_PlaySong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_StopSong(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
procedure FMUSIC_StopAllSongs; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_SetZxxCallback: function (Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetRowCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrderCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetInstCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_SetZxxCallback(Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetRowCallback(Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrderCallback(Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetInstCallback(Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_SetSample: function (Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_OptimizeChannels: function (Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_SetSample(Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_OptimizeChannels(Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Runtime song functions.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_SetReverb: function (Reverb: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrder: function (Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPaused: function (Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterVolume: function (Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPanSeperation: function (Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_SetReverb(Reverb: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetOrder(Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPaused(Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetMasterVolume(Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_SetPanSeperation(Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Static song information functions.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_GetName: function (Module: PFMusicModule): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetType: function (Module: PFMusicModule): TFMusicTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumOrders: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumPatterns: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumInstruments: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumSamples: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumChannels: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSample: function (Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPatternLength: function (Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_GetName(Module: PFMusicModule): PCHAR; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetType(Module: PFMusicModule): TFMusicTypes; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumOrders(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumPatterns(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumInstruments(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumSamples(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetNumChannels(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSample(Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPatternLength(Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

{
  Runtime song information.
}

{$IFDEF FMOD_DYNAMIC_LOAD}
var
  FMUSIC_IsFinished: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_IsPlaying: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetMasterVolume: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetGlobalVolume: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOrder: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPattern: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSpeed: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetBPM: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRow: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPaused: function (Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetTime: function (Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ELSE}
function FMUSIC_IsFinished(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_IsPlaying(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetMasterVolume(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetGlobalVolume(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetOrder(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPattern(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetSpeed(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetBPM(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetRow(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetPaused(Module: PFMusicModule): ByteBool; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
function FMUSIC_GetTime(Module: PFMusicModule): Integer; {$IFDEF LINUX} cdecl {$ELSE} stdcall {$ENDIF};
{$ENDIF}

implementation

{$IFDEF FMOD_DYNAMIC_LOAD}
{$IFDEF LINUX}
uses
  Libc;
{$ENDIF}
{$ENDIF}

const
{$IFDEF LINUX}
  FMOD_DLL = 'libfmod.so';
{$ELSE}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}

{$IFDEF FMOD_DYNAMIC_LOAD}
type
{$IFDEF LINUX}
  TFMODModuleHandle = Pointer;
{$ELSE}
  TFMODModuleHandle = HINST;
{$ENDIF}

const
{$IFDEF LINUX}
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(nil);
{$ELSE}
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(0);
{$ENDIF}

var
  FMODHandle: TFMODModuleHandle;

function GetAddress(Handle: TFMODModuleHandle; FuncName: PChar): Pointer;
begin
{$IFDEF WIN32}
  Result := GetProcAddress(Handle, FuncName);
{$ELSE}
  Result := dlsym(Handle, FuncName);
{$ENDIF}
  Assert(Result <> nil, 'Failed to find ' + FuncName + ' in ' + FMOD_DLL);
end;

function FMOD_Load(LibName: PChar): Boolean;
begin
  Result := False;

  { Make sure the previous library is unloaded }
  FMOD_Unload;

  { If no library name given, use the default library names }
  if LibName = nil then
    LibName := FMOD_DLL;

  { Load the library }
{$IFDEF WIN32}
  FMODHandle := LoadLibrary(LibName);
{$ELSE}
  FMODHandle := dlopen(LibName, RTLD_NOW);
{$ENDIF}
  if FMODHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

  { Get all the function addresses from the library }
  FSOUND_SetOutput                := GetAddress(FMODHandle, '_FSOUND_SetOutput@4');
  FSOUND_SetDriver                := GetAddress(FMODHandle, '_FSOUND_SetDriver@4');
  FSOUND_SetMixer                 := GetAddress(FMODHandle, '_FSOUND_SetMixer@4');
  FSOUND_SetBufferSize            := GetAddress(FMODHandle, '_FSOUND_SetBufferSize@4');
  FSOUND_SetHWND                  := GetAddress(FMODHandle, '_FSOUND_SetHWND@4');
  FSOUND_SetMinHardwareChannels   := GetAddress(FMODHandle, '_FSOUND_SetMinHardwareChannels@4');
  FSOUND_SetMaxHardwareChannels   := GetAddress(FMODHandle, '_FSOUND_SetMaxHardwareChannels@4');
  FSOUND_SetMemorySystem          := GetAddress(FMODHandle, '_FSOUND_SetMemorySystem@20');
  FSOUND_Init                     := GetAddress(FMODHandle, '_FSOUND_Init@12');
  FSOUND_Close                    := GetAddress(FMODHandle, '_FSOUND_Close@0');
  FSOUND_SetSFXMasterVolume       := GetAddress(FMODHandle, '_FSOUND_SetSFXMasterVolume@4');
  FSOUND_SetPanSeperation         := GetAddress(FMODHandle, '_FSOUND_SetPanSeperation@4');
  FSOUND_SetSpeakerMode           := GetAddress(FMODHandle, '_FSOUND_SetSpeakerMode@4');
  FSOUND_GetError                 := GetAddress(FMODHandle, '_FSOUND_GetError@0');
  FSOUND_GetVersion               := GetAddress(FMODHandle, '_FSOUND_GetVersion@0');
  FSOUND_GetOutput                := GetAddress(FMODHandle, '_FSOUND_GetOutput@0');
  FSOUND_GetOutputHandle          := GetAddress(FMODHandle, '_FSOUND_GetOutputHandle@0');
  FSOUND_GetDriver                := GetAddress(FMODHandle, '_FSOUND_GetDriver@0');
  FSOUND_GetMixer                 := GetAddress(FMODHandle, '_FSOUND_GetMixer@0');
  FSOUND_GetNumDrivers            := GetAddress(FMODHandle, '_FSOUND_GetNumDrivers@0');
  FSOUND_GetDriverName            := GetAddress(FMODHandle, '_FSOUND_GetDriverName@4');
  FSOUND_GetDriverCaps            := GetAddress(FMODHandle, '_FSOUND_GetDriverCaps@8');
  FSOUND_GetOutputRate            := GetAddress(FMODHandle, '_FSOUND_GetOutputRate@0');
  FSOUND_GetMaxChannels           := GetAddress(FMODHandle, '_FSOUND_GetMaxChannels@0');
  FSOUND_GetMaxSamples            := GetAddress(FMODHandle, '_FSOUND_GetMaxSamples@0');
  FSOUND_GetSFXMasterVolume       := GetAddress(FMODHandle, '_FSOUND_GetSFXMasterVolume@0');
  FSOUND_GetNumHardwareChannels   := GetAddress(FMODHandle, '_FSOUND_GetNumHardwareChannels@0');
  FSOUND_GetChannelsPlaying       := GetAddress(FMODHandle, '_FSOUND_GetChannelsPlaying@0');
  FSOUND_GetCPUUsage              := GetAddress(FMODHandle, '_FSOUND_GetCPUUsage@0');
  FSOUND_GetMemoryStats           := GetAddress(FMODHandle, '_FSOUND_GetMemoryStats@8');
  FSOUND_Sample_Load              := GetAddress(FMODHandle, '_FSOUND_Sample_Load@16');
  FSOUND_Sample_Alloc             := GetAddress(FMODHandle, '_FSOUND_Sample_Alloc@28');
  FSOUND_Sample_Free              := GetAddress(FMODHandle, '_FSOUND_Sample_Free@4');
  FSOUND_Sample_Upload            := GetAddress(FMODHandle, '_FSOUND_Sample_Upload@12');
  FSOUND_Sample_Lock              := GetAddress(FMODHandle, '_FSOUND_Sample_Lock@28');
  FSOUND_Sample_Unlock            := GetAddress(FMODHandle, '_FSOUND_Sample_Unlock@20');
  FSOUND_Sample_SetLoopMode       := GetAddress(FMODHandle, '_FSOUND_Sample_SetLoopMode@8');
  FSOUND_Sample_SetLoopPoints     := GetAddress(FMODHandle, '_FSOUND_Sample_SetLoopPoints@12');
  FSOUND_Sample_SetDefaults       := GetAddress(FMODHandle, '_FSOUND_Sample_SetDefaults@20');
  FSOUND_Sample_SetMinMaxDistance := GetAddress(FMODHandle, '_FSOUND_Sample_SetMinMaxDistance@12');
  FSOUND_Sample_Get               := GetAddress(FMODHandle, '_FSOUND_Sample_Get@4');
  FSOUND_Sample_GetName           := GetAddress(FMODHandle, '_FSOUND_Sample_GetName@4');
  FSOUND_Sample_GetLength         := GetAddress(FMODHandle, '_FSOUND_Sample_GetLength@4');
  FSOUND_Sample_GetLoopPoints     := GetAddress(FMODHandle, '_FSOUND_Sample_GetLoopPoints@12');
  FSOUND_Sample_GetDefaults       := GetAddress(FMODHandle, '_FSOUND_Sample_GetDefaults@20');
  FSOUND_Sample_GetMode           := GetAddress(FMODHandle, '_FSOUND_Sample_GetMode@4');
  FSOUND_PlaySound                := GetAddress(FMODHandle, '_FSOUND_PlaySound@8');
  FSOUND_PlaySoundEx              := GetAddress(FMODHandle, '_FSOUND_PlaySoundEx@16');
  FSOUND_StopSound                := GetAddress(FMODHandle, '_FSOUND_StopSound@4');
  FSOUND_SetFrequency             := GetAddress(FMODHandle, '_FSOUND_SetFrequency@8');
  FSOUND_SetVolume                := GetAddress(FMODHandle, '_FSOUND_SetVolume@8');
  FSOUND_SetVolumeAbsolute        := GetAddress(FMODHandle, '_FSOUND_SetVolumeAbsolute@8');
  FSOUND_SetPan                   := GetAddress(FMODHandle, '_FSOUND_SetPan@8');
  FSOUND_SetSurround              := GetAddress(FMODHandle, '_FSOUND_SetSurround@8');
  FSOUND_SetMute                  := GetAddress(FMODHandle, '_FSOUND_SetMute@8');
  FSOUND_SetPriority              := GetAddress(FMODHandle, '_FSOUND_SetPriority@8');
  FSOUND_SetReserved              := GetAddress(FMODHandle, '_FSOUND_SetReserved@8');
  FSOUND_SetPaused                := GetAddress(FMODHandle, '_FSOUND_SetPaused@8');
  FSOUND_SetLoopMode              := GetAddress(FMODHandle, '_FSOUND_SetLoopMode@8');
  FSOUND_IsPlaying                := GetAddress(FMODHandle, '_FSOUND_IsPlaying@4');
  FSOUND_GetFrequency             := GetAddress(FMODHandle, '_FSOUND_GetFrequency@4');
  FSOUND_GetVolume                := GetAddress(FMODHandle, '_FSOUND_GetVolume@4');
  FSOUND_GetPan                   := GetAddress(FMODHandle, '_FSOUND_GetPan@4');
  FSOUND_GetSurround              := GetAddress(FMODHandle, '_FSOUND_GetSurround@4');
  FSOUND_GetMute                  := GetAddress(FMODHandle, '_FSOUND_GetMute@4');
  FSOUND_GetPriority              := GetAddress(FMODHandle, '_FSOUND_GetPriority@4');
  FSOUND_GetReserved              := GetAddress(FMODHandle, '_FSOUND_GetReserved@4');
  FSOUND_GetPaused                := GetAddress(FMODHandle, '_FSOUND_GetPaused@4');
  FSOUND_GetLoopMode              := GetAddress(FMODHandle, '_FSOUND_GetLoopMode@4');
  FSOUND_GetCurrentPosition       := GetAddress(FMODHandle, '_FSOUND_GetCurrentPosition@4');
  FSOUND_SetCurrentPosition       := GetAddress(FMODHandle, '_FSOUND_SetCurrentPosition@8');
  FSOUND_GetCurrentSample         := GetAddress(FMODHandle, '_FSOUND_GetCurrentSample@4');
  FSOUND_GetCurrentVU             := GetAddress(FMODHandle, '_FSOUND_GetCurrentVU@4');
  FSOUND_FX_Enable                := GetAddress(FMODHandle, '_FSOUND_FX_Enable@8');
  FSOUND_FX_SetChorus             := GetAddress(FMODHandle, '_FSOUND_FX_SetChorus@32');
  FSOUND_FX_SetCompressor         := GetAddress(FMODHandle, '_FSOUND_FX_SetCompressor@28');
  FSOUND_FX_SetDistortion         := GetAddress(FMODHandle, '_FSOUND_FX_SetDistortion@24');
  FSOUND_FX_SetEcho               := GetAddress(FMODHandle, '_FSOUND_FX_SetEcho@24');
  FSOUND_FX_SetFlanger            := GetAddress(FMODHandle, '_FSOUND_FX_SetFlanger@32');
  FSOUND_FX_SetGargle             := GetAddress(FMODHandle, '_FSOUND_FX_SetGargle@12');
  FSOUND_FX_SetI3DL2Reverb        := GetAddress(FMODHandle, '_FSOUND_FX_SetI3DL2Reverb@52');
  FSOUND_FX_SetParamEQ            := GetAddress(FMODHandle, '_FSOUND_FX_SetParamEQ@16');
  FSOUND_FX_SetWavesReverb        := GetAddress(FMODHandle, '_FSOUND_FX_SetWavesReverb@20');
  FSOUND_3D_Update                := GetAddress(FMODHandle, '_FSOUND_3D_Update@0');
  FSOUND_3D_SetAttributes         := GetAddress(FMODHandle, '_FSOUND_3D_SetAttributes@12');
  FSOUND_3D_GetAttributes         := GetAddress(FMODHandle, '_FSOUND_3D_GetAttributes@12');
  FSOUND_3D_Listener_SetAttributes := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetAttributes@32');
  FSOUND_3D_Listener_GetAttributes := GetAddress(FMODHandle, '_FSOUND_3D_Listener_GetAttributes@32');
  FSOUND_3D_Listener_SetDopplerFactor := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetDopplerFactor@4');
  FSOUND_3D_Listener_SetDistanceFactor := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetDistanceFactor@4');
  FSOUND_3D_Listener_SetRolloffFactor := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetRolloffFactor@4');
  FSOUND_Stream_OpenFile          := GetAddress(FMODHandle, '_FSOUND_Stream_OpenFile@12');
  FSOUND_Stream_Create            := GetAddress(FMODHandle, '_FSOUND_Stream_Create@20');
  FSOUND_Stream_Play              := GetAddress(FMODHandle, '_FSOUND_Stream_Play@8');
  FSOUND_Stream_PlayEx            := GetAddress(FMODHandle, '_FSOUND_Stream_PlayEx@16');
  FSOUND_Stream_Stop              := GetAddress(FMODHandle, '_FSOUND_Stream_Stop@4');
  FSOUND_Stream_Close             := GetAddress(FMODHandle, '_FSOUND_Stream_Close@4');
  FSOUND_Stream_SetEndCallback    := GetAddress(FMODHandle, '_FSOUND_Stream_SetEndCallback@12');
  FSOUND_Stream_SetSynchCallback  := GetAddress(FMODHandle, '_FSOUND_Stream_SetSynchCallback@12');
  FSOUND_Stream_GetSample         := GetAddress(FMODHandle, '_FSOUND_Stream_GetSample@4');
  FSOUND_Stream_CreateDSP         := GetAddress(FMODHandle, '_FSOUND_Stream_CreateDSP@16');
  FSOUND_Stream_SetBufferSize     := GetAddress(FMODHandle, '_FSOUND_Stream_SetBufferSize@4');
  FSOUND_Stream_SetPosition       := GetAddress(FMODHandle, '_FSOUND_Stream_SetPosition@8');
  FSOUND_Stream_GetPosition       := GetAddress(FMODHandle, '_FSOUND_Stream_GetPosition@4');
  FSOUND_Stream_SetTime           := GetAddress(FMODHandle, '_FSOUND_Stream_SetTime@8');
  FSOUND_Stream_GetTime           := GetAddress(FMODHandle, '_FSOUND_Stream_GetTime@4');
  FSOUND_Stream_GetLength         := GetAddress(FMODHandle, '_FSOUND_Stream_GetLength@4');
  FSOUND_Stream_GetLengthMs       := GetAddress(FMODHandle, '_FSOUND_Stream_GetLengthMs@4');
  FSOUND_CD_Play                  := GetAddress(FMODHandle, '_FSOUND_CD_Play@8');
  FSOUND_CD_SetPlayMode           := GetAddress(FMODHandle, '_FSOUND_CD_SetPlayMode@8');
  FSOUND_CD_Stop                  := GetAddress(FMODHandle, '_FSOUND_CD_Stop@4');
  FSOUND_CD_SetPaused             := GetAddress(FMODHandle, '_FSOUND_CD_SetPaused@8');
  FSOUND_CD_SetVolume             := GetAddress(FMODHandle, '_FSOUND_CD_SetVolume@8');
  FSOUND_CD_Eject                 := GetAddress(FMODHandle, '_FSOUND_CD_Eject@4');
  FSOUND_CD_GetPaused             := GetAddress(FMODHandle, '_FSOUND_CD_GetPaused@4');
  FSOUND_CD_GetTrack              := GetAddress(FMODHandle, '_FSOUND_CD_GetTrack@4');
  FSOUND_CD_GetNumTracks          := GetAddress(FMODHandle, '_FSOUND_CD_GetNumTracks@4');
  FSOUND_CD_GetVolume             := GetAddress(FMODHandle, '_FSOUND_CD_GetVolume@4');
  FSOUND_CD_GetTrackLength        := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackLength@8');
  FSOUND_CD_GetTrackTime          := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackTime@4');
  FSOUND_DSP_Create               := GetAddress(FMODHandle, '_FSOUND_DSP_Create@12');
  FSOUND_DSP_Free                 := GetAddress(FMODHandle, '_FSOUND_DSP_Free@4');
  FSOUND_DSP_SetPriority          := GetAddress(FMODHandle, '_FSOUND_DSP_SetPriority@8');
  FSOUND_DSP_GetPriority          := GetAddress(FMODHandle, '_FSOUND_DSP_GetPriority@4');
  FSOUND_DSP_SetActive            := GetAddress(FMODHandle, '_FSOUND_DSP_SetActive@8');
  FSOUND_DSP_GetActive            := GetAddress(FMODHandle, '_FSOUND_DSP_GetActive@4');
  FSOUND_DSP_GetClearUnit         := GetAddress(FMODHandle, '_FSOUND_DSP_GetClearUnit@0');
  FSOUND_DSP_GetSFXUnit           := GetAddress(FMODHandle, '_FSOUND_DSP_GetSFXUnit@0');
  FSOUND_DSP_GetMusicUnit         := GetAddress(FMODHandle, '_FSOUND_DSP_GetMusicUnit@0');
  FSOUND_DSP_GetClipAndCopyUnit   := GetAddress(FMODHandle, '_FSOUND_DSP_GetClipAndCopyUnit@0');
  FSOUND_DSP_GetFFTUnit           := GetAddress(FMODHandle, '_FSOUND_DSP_GetFFTUnit@0');
  FSOUND_DSP_MixBuffers           := GetAddress(FMODHandle, '_FSOUND_DSP_MixBuffers@28');
  FSOUND_DSP_ClearMixBuffer       := GetAddress(FMODHandle, '_FSOUND_DSP_ClearMixBuffer@0');
  FSOUND_DSP_GetBufferLength      := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLength@0');
  FSOUND_DSP_GetBufferLengthTotal := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLengthTotal@0');
  FSOUND_DSP_GetSpectrum          := GetAddress(FMODHandle, '_FSOUND_DSP_GetSpectrum@0');
  FSOUND_Geometry_AddPolygon      := GetAddress(FMODHandle, '_FSOUND_Geometry_AddPolygon@28');
  FSOUND_Geometry_AddList         := GetAddress(FMODHandle, '_FSOUND_Geometry_AddList@4');
  FSOUND_Geometry_List_Create     := GetAddress(FMODHandle, '_FSOUND_Geometry_List_Create@4');
  FSOUND_Geometry_List_Free       := GetAddress(FMODHandle, '_FSOUND_Geometry_List_Free@4');
  FSOUND_Geometry_List_Begin      := GetAddress(FMODHandle, '_FSOUND_Geometry_List_Begin@4');
  FSOUND_Geometry_List_End        := GetAddress(FMODHandle, '_FSOUND_Geometry_List_End@4');
  FSOUND_Geometry_Material_Create := GetAddress(FMODHandle, '_FSOUND_Geometry_Material_Create@0');
  FSOUND_Geometry_Material_Free   := GetAddress(FMODHandle, '_FSOUND_Geometry_Material_Free@4');
  FSOUND_Geometry_Material_SetAttributes := GetAddress(FMODHandle, '_FSOUND_Geometry_Material_SetAttributes@20');
  FSOUND_Geometry_Material_GetAttributes := GetAddress(FMODHandle, '_FSOUND_Geometry_Material_GetAttributes@20');
  FSOUND_Geometry_Material_Set    := GetAddress(FMODHandle, '_FSOUND_Geometry_Material_Set@4');
  FSOUND_Reverb_SetProperties     := GetAddress(FMODHandle, '_FSOUND_Reverb_SetProperties@4');
  FSOUND_Reverb_GetProperties     := GetAddress(FMODHandle, '_FSOUND_Reverb_GetProperties@4');
  FSOUND_Reverb_SetChannelProperties := GetAddress(FMODHandle, '_FSOUND_Reverb_SetChannelProperties@8');
  FSOUND_Reverb_GetChannelProperties := GetAddress(FMODHandle, '_FSOUND_Reverb_GetChannelProperties@8');
  FSOUND_Record_SetDriver         := GetAddress(FMODHandle, '_FSOUND_Record_SetDriver@4');
  FSOUND_Record_GetNumDrivers     := GetAddress(FMODHandle, '_FSOUND_Record_GetNumDrivers@0');
  FSOUND_Record_GetDriverName     := GetAddress(FMODHandle, '_FSOUND_Record_GetDriverName@4');
  FSOUND_Record_GetDriver         := GetAddress(FMODHandle, '_FSOUND_Record_GetDriver@0');
  FSOUND_Record_StartSample       := GetAddress(FMODHandle, '_FSOUND_Record_StartSample@8');
  FSOUND_Record_Stop              := GetAddress(FMODHandle, '_FSOUND_Record_Stop@0');
  FSOUND_Record_GetPosition       := GetAddress(FMODHandle, '_FSOUND_Record_GetPosition@0');
  FSOUND_File_SetCallbacks        := GetAddress(FMODHandle, '_FSOUND_File_SetCallbacks@20');
  FMUSIC_LoadSong                 := GetAddress(FMODHandle, '_FMUSIC_LoadSong@4');
  FMUSIC_LoadSongMemory           := GetAddress(FMODHandle, '_FMUSIC_LoadSongMemory@8');
  FMUSIC_FreeSong                 := GetAddress(FMODHandle, '_FMUSIC_FreeSong@4');
  FMUSIC_PlaySong                 := GetAddress(FMODHandle, '_FMUSIC_PlaySong@4');
  FMUSIC_StopSong                 := GetAddress(FMODHandle, '_FMUSIC_StopSong@4');
  FMUSIC_StopAllSongs             := GetAddress(FMODHandle, '_FMUSIC_StopAllSongs@0');
  FMUSIC_SetZxxCallback           := GetAddress(FMODHandle, '_FMUSIC_SetZxxCallback@8');
  FMUSIC_SetRowCallback           := GetAddress(FMODHandle, '_FMUSIC_SetRowCallback@12');
  FMUSIC_SetOrderCallback         := GetAddress(FMODHandle, '_FMUSIC_SetOrderCallback@12');
  FMUSIC_SetInstCallback          := GetAddress(FMODHandle, '_FMUSIC_SetInstCallback@12');
  FMUSIC_SetSample                := GetAddress(FMODHandle, '_FMUSIC_SetSample@12');
  FMUSIC_OptimizeChannels         := GetAddress(FMODHandle, '_FMUSIC_OptimizeChannels@12');
  FMUSIC_SetReverb                := GetAddress(FMODHandle, '_FMUSIC_SetReverb@4');
  FMUSIC_SetOrder                 := GetAddress(FMODHandle, '_FMUSIC_SetOrder@8');
  FMUSIC_SetPaused                := GetAddress(FMODHandle, '_FMUSIC_SetPaused@8');
  FMUSIC_SetMasterVolume          := GetAddress(FMODHandle, '_FMUSIC_SetMasterVolume@8');
  FMUSIC_SetPanSeperation         := GetAddress(FMODHandle, '_FMUSIC_SetPanSeperation@8');
  FMUSIC_GetName                  := GetAddress(FMODHandle, '_FMUSIC_GetName@4');
  FMUSIC_GetType                  := GetAddress(FMODHandle, '_FMUSIC_GetType@4');
  FMUSIC_GetNumOrders             := GetAddress(FMODHandle, '_FMUSIC_GetNumOrders@4');
  FMUSIC_GetNumPatterns           := GetAddress(FMODHandle, '_FMUSIC_GetNumPatterns@4');
  FMUSIC_GetNumInstruments        := GetAddress(FMODHandle, '_FMUSIC_GetNumInstruments@4');
  FMUSIC_GetNumSamples            := GetAddress(FMODHandle, '_FMUSIC_GetNumSamples@4');
  FMUSIC_GetNumChannels           := GetAddress(FMODHandle, '_FMUSIC_GetNumChannels@4');
  FMUSIC_GetSample                := GetAddress(FMODHandle, '_FMUSIC_GetSample@8');
  FMUSIC_GetPatternLength         := GetAddress(FMODHandle, '_FMUSIC_GetPatternLength@8');
  FMUSIC_IsFinished               := GetAddress(FMODHandle, '_FMUSIC_IsFinished@4');
  FMUSIC_IsPlaying                := GetAddress(FMODHandle, '_FMUSIC_IsPlaying@4');
  FMUSIC_GetMasterVolume          := GetAddress(FMODHandle, '_FMUSIC_GetMasterVolume@4');
  FMUSIC_GetGlobalVolume          := GetAddress(FMODHandle, '_FMUSIC_GetGlobalVolume@4');
  FMUSIC_GetOrder                 := GetAddress(FMODHandle, '_FMUSIC_GetOrder@4');
  FMUSIC_GetPattern               := GetAddress(FMODHandle, '_FMUSIC_GetPattern@4');
  FMUSIC_GetSpeed                 := GetAddress(FMODHandle, '_FMUSIC_GetSpeed@4');
  FMUSIC_GetBPM                   := GetAddress(FMODHandle, '_FMUSIC_GetBPM@4');
  FMUSIC_GetRow                   := GetAddress(FMODHandle, '_FMUSIC_GetRow@4');
  FMUSIC_GetPaused                := GetAddress(FMODHandle, '_FMUSIC_GetPaused@4');
  FMUSIC_GetTime                  := GetAddress(FMODHandle, '_FMUSIC_GetTime@4');

  Result := True;
end;

procedure FMOD_Unload;
begin
  { Only free the library if it was already loaded }
  if FMODHandle <> INVALID_MODULEHANDLE_VALUE then
{$IFDEF WIN32}
    FreeLibrary(FMODHandle);
{$ELSE}
    dlclose(FMODHandle);
{$ENDIF}
  FMODHandle := INVALID_MODULEHANDLE_VALUE;
end;

{$ELSE}

{
  Stub functions to allow applications to swap between static and dynamic with
  no code changes at all.
}
function FMOD_Load(LibName: PChar): Boolean;
begin
  Result := True;
end;

procedure FMOD_Unload;
begin
end;

function FSOUND_SetOutput; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetOutput@4' {$ENDIF};
function FSOUND_SetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetDriver@4' {$ENDIF};
function FSOUND_SetMixer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMixer@4' {$ENDIF};
function FSOUND_SetBufferSize; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetBufferSize@4' {$ENDIF};
function FSOUND_SetHWND; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetHWND@4' {$ENDIF};
function FSOUND_SetMinHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMinHardwareChannels@4' {$ENDIF};
function FSOUND_SetMaxHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMaxHardwareChannels@4' {$ENDIF};
function FSOUND_SetMemorySystem; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMemorySystem@20' {$ENDIF};
function FSOUND_Init; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Init@12' {$ENDIF};
procedure FSOUND_Close; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Close@0' {$ENDIF};
procedure FSOUND_SetSFXMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSFXMasterVolume@4' {$ENDIF};
procedure FSOUND_SetPanSeperation; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPanSeperation@4' {$ENDIF};
procedure FSOUND_SetSpeakerMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSpeakerMode@4' {$ENDIF};
function FSOUND_GetError; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetError@0' {$ENDIF};
function FSOUND_GetVersion; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetVersion@0' {$ENDIF};
function FSOUND_GetOutput; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutput@0' {$ENDIF};
function FSOUND_GetOutputHandle; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutputHandle@0' {$ENDIF};
function FSOUND_GetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriver@0' {$ENDIF};
function FSOUND_GetMixer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMixer@0' {$ENDIF};
function FSOUND_GetNumDrivers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetNumDrivers@0' {$ENDIF};
function FSOUND_GetDriverName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriverName@4' {$ENDIF};
function FSOUND_GetDriverCaps; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetDriverCaps@8' {$ENDIF};
function FSOUND_GetOutputRate; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetOutputRate@0' {$ENDIF};
function FSOUND_GetMaxChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMaxChannels@0' {$ENDIF};
function FSOUND_GetMaxSamples; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMaxSamples@0' {$ENDIF};
function FSOUND_GetSFXMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetSFXMasterVolume@0' {$ENDIF};
function FSOUND_GetNumHardwareChannels; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetNumHardwareChannels@0' {$ENDIF};
function FSOUND_GetChannelsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetChannelsPlaying@0' {$ENDIF};
function FSOUND_GetCPUUsage; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCPUUsage@0' {$ENDIF};
procedure FSOUND_GetMemoryStats; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMemoryStats@8' {$ENDIF};
function FSOUND_Sample_Load; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Load@16' {$ENDIF};
function FSOUND_Sample_Alloc; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Alloc@28' {$ENDIF};
procedure FSOUND_Sample_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Free@4' {$ENDIF};
function FSOUND_Sample_Upload; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Upload@12' {$ENDIF};
function FSOUND_Sample_Lock; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Lock@28' {$ENDIF};
function FSOUND_Sample_Unlock; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Unlock@20' {$ENDIF};
function FSOUND_Sample_SetLoopMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetLoopMode@8' {$ENDIF};
function FSOUND_Sample_SetLoopPoints; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_SetDefaults; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetDefaults@20' {$ENDIF};
function FSOUND_Sample_SetMinMaxDistance; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_SetMinMaxDistance@12' {$ENDIF};
function FSOUND_Sample_Get; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_Get@4' {$ENDIF};
function FSOUND_Sample_GetName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetName@4' {$ENDIF};
function FSOUND_Sample_GetLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetLength@4' {$ENDIF};
function FSOUND_Sample_GetLoopPoints; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetLoopPoints@12' {$ENDIF};
function FSOUND_Sample_GetDefaults; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetDefaults@20' {$ENDIF};
function FSOUND_Sample_GetMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Sample_GetMode@4' {$ENDIF};
function FSOUND_PlaySound; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_PlaySound@8' {$ENDIF};
function FSOUND_PlaySoundEx; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_PlaySoundEx@16' {$ENDIF};
function FSOUND_StopSound; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_StopSound@4' {$ENDIF};
function FSOUND_SetFrequency; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetFrequency@8' {$ENDIF};
function FSOUND_SetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetVolume@8' {$ENDIF};
function FSOUND_SetVolumeAbsolute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetVolumeAbsolute@8' {$ENDIF};
function FSOUND_SetPan; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPan@8' {$ENDIF};
function FSOUND_SetSurround; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetSurround@8' {$ENDIF};
function FSOUND_SetMute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetMute@8' {$ENDIF};
function FSOUND_SetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPriority@8' {$ENDIF};
function FSOUND_SetReserved; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetReserved@8' {$ENDIF};
function FSOUND_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetPaused@8' {$ENDIF};
function FSOUND_SetLoopMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetLoopMode@8' {$ENDIF};
function FSOUND_IsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_IsPlaying@4' {$ENDIF};
function FSOUND_GetFrequency; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetFrequency@4' {$ENDIF};
function FSOUND_GetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetVolume@4' {$ENDIF};
function FSOUND_GetPan; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPan@4' {$ENDIF};
function FSOUND_GetSurround; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetSurround@4' {$ENDIF};
function FSOUND_GetMute; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetMute@4' {$ENDIF};
function FSOUND_GetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPriority@4' {$ENDIF};
function FSOUND_GetReserved; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetReserved@4' {$ENDIF};
function FSOUND_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetPaused@4' {$ENDIF};
function FSOUND_GetLoopMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetLoopMode@4' {$ENDIF};
function FSOUND_GetCurrentPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentPosition@4' {$ENDIF};
function FSOUND_SetCurrentPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_SetCurrentPosition@8' {$ENDIF};
function FSOUND_GetCurrentSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentSample@4' {$ENDIF};
function FSOUND_GetCurrentVU; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_GetCurrentVU@4' {$ENDIF};
function FSOUND_FX_Enable; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_Enable@8' {$ENDIF};
function FSOUND_FX_SetChorus; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetChorus@32' {$ENDIF};
function FSOUND_FX_SetCompressor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetCompressor@28' {$ENDIF};
function FSOUND_FX_SetDistortion; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetDistortion@24' {$ENDIF};
function FSOUND_FX_SetEcho; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetEcho@24' {$ENDIF};
function FSOUND_FX_SetFlanger; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetFlanger@32' {$ENDIF};
function FSOUND_FX_SetGargle; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetGargle@12' {$ENDIF};
function FSOUND_FX_SetI3DL2Reverb; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetI3DL2Reverb@52' {$ENDIF};
function FSOUND_FX_SetParamEq; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetParamEq@16' {$ENDIF};
function FSOUND_FX_SetWavesReverb; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_FX_SetWavesReverb@20' {$ENDIF};
procedure FSOUND_3D_Update; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Update@0' {$ENDIF};
function FSOUND_3D_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_SetAttributes@12' {$ENDIF};
function FSOUND_3D_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_GetAttributes@12' {$ENDIF};
procedure FSOUND_3D_Listener_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetAttributes@32' {$ENDIF};
procedure FSOUND_3D_Listener_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_GetAttributes@32' {$ENDIF};
procedure FSOUND_3D_Listener_SetDopplerFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetDopplerFactor@4' {$ENDIF};
procedure FSOUND_3D_Listener_SetDistanceFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetDistanceFactor@4' {$ENDIF};
procedure FSOUND_3D_Listener_SetRolloffFactor; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_3D_Listener_SetRolloffFactor@4' {$ENDIF};
function FSOUND_Stream_OpenFile; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_OpenFile@12' {$ENDIF};
function FSOUND_Stream_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Create@20' {$ENDIF};
function FSOUND_Stream_Play; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Play@8' {$ENDIF};
function FSOUND_Stream_PlayEx; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_PlayEx@16' {$ENDIF};
function FSOUND_Stream_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Stop@4' {$ENDIF};
function FSOUND_Stream_Close; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_Close@4' {$ENDIF};
function FSOUND_Stream_SetEndCallback; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetEndCallback@12' {$ENDIF};
function FSOUND_Stream_SetSynchCallback; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetSynchCallback@12' {$ENDIF};
function FSOUND_Stream_GetSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetSample@4' {$ENDIF};
function FSOUND_Stream_CreateDSP; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_CreateDSP@16' {$ENDIF};
function FSOUND_Stream_SetBufferSize; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetBufferSize@4' {$ENDIF};
function FSOUND_Stream_SetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetPosition@8' {$ENDIF};
function FSOUND_Stream_GetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetPosition@4' {$ENDIF};
function FSOUND_Stream_SetTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_SetTime@8' {$ENDIF};
function FSOUND_Stream_GetTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetTime@4' {$ENDIF};
function FSOUND_Stream_GetLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetLength@4' {$ENDIF};
function FSOUND_Stream_GetLengthMs; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Stream_GetLengthMs@4' {$ENDIF};
function FSOUND_CD_Play; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Play@8' {$ENDIF};
procedure FSOUND_CD_SetPlayMode; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetPlayMode@8' {$ENDIF};
function FSOUND_CD_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Stop@4' {$ENDIF};
function FSOUND_CD_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetPaused@8' {$ENDIF};
function FSOUND_CD_SetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_SetVolume@8' {$ENDIF};
function FSOUND_CD_Eject; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_Eject@4' {$ENDIF};
function FSOUND_CD_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetPaused@4' {$ENDIF};
function FSOUND_CD_GetTrack; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrack@4' {$ENDIF};
function FSOUND_CD_GetNumTracks; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetNumTracks@4' {$ENDIF};
function FSOUND_CD_GetVolume; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetVolume@4' {$ENDIF};
function FSOUND_CD_GetTrackLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrackLength@8' {$ENDIF};
function FSOUND_CD_GetTrackTime; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_CD_GetTrackTime@4' {$ENDIF};
function FSOUND_DSP_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_Create@12' {$ENDIF};
procedure FSOUND_DSP_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_Free@4' {$ENDIF};
procedure FSOUND_DSP_SetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_SetPriority@8' {$ENDIF};
function FSOUND_DSP_GetPriority; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetPriority@4' {$ENDIF};
procedure FSOUND_DSP_SetActive; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_SetActive@8' {$ENDIF};
function FSOUND_DSP_GetActive; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetActive@4' {$ENDIF};
function FSOUND_DSP_GetClearUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetClearUnit@0' {$ENDIF};
function FSOUND_DSP_GetSFXUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetSFXUnit@0' {$ENDIF};
function FSOUND_DSP_GetMusicUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetMusicUnit@0' {$ENDIF};
function FSOUND_DSP_GetClipAndCopyUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetClipAndCopyUnit@0' {$ENDIF};
function FSOUND_DSP_GetFFTUnit; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetFFTUnit@0' {$ENDIF};
function FSOUND_DSP_MixBuffers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_MixBuffers@28' {$ENDIF};
procedure FSOUND_DSP_ClearMixBuffer; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_ClearMixBuffer@0' {$ENDIF};
function FSOUND_DSP_GetBufferLength; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetBufferLength@0' {$ENDIF};
function FSOUND_DSP_GetBufferLengthTotal; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetBufferLengthTotal@0' {$ENDIF};
function FSOUND_DSP_GetSpectrum; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_DSP_GetSpectrum@0' {$ENDIF};
function FSOUND_Geometry_AddPolygon; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_AddPolygon@28' {$ENDIF};
function FSOUND_Geometry_AddList; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_AddList@4' {$ENDIF};
function FSOUND_Geometry_List_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Create@4' {$ENDIF};
function FSOUND_Geometry_List_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Free@4' {$ENDIF};
function FSOUND_Geometry_List_Begin; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_Begin@4' {$ENDIF};
function FSOUND_Geometry_List_End; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_List_End@4' {$ENDIF};
function FSOUND_Geometry_Material_Create; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Create@0' {$ENDIF};
function FSOUND_Geometry_Material_Free; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Free@4' {$ENDIF};
function FSOUND_Geometry_Material_SetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_SetAttributes@20' {$ENDIF};
function FSOUND_Geometry_Material_GetAttributes; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_GetAttributes@20' {$ENDIF};
function FSOUND_Geometry_Material_Set; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Geometry_Material_Set@4' {$ENDIF};
function FSOUND_Reverb_SetProperties; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_SetProperties@4' {$ENDIF};
function FSOUND_Reverb_GetProperties; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_GetProperties@4' {$ENDIF};
function FSOUND_Reverb_SetChannelProperties; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_SetChannelProperties@8' {$ENDIF};
function FSOUND_Reverb_GetChannelProperties; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Reverb_GetChannelProperties@8' {$ENDIF};
function FSOUND_Record_SetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_SetDriver@4' {$ENDIF};
function FSOUND_Record_GetNumDrivers; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetNumDrivers@0' {$ENDIF};
function FSOUND_Record_GetDriverName; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetDriverName@4' {$ENDIF};
function FSOUND_Record_GetDriver; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetDriver@0' {$ENDIF};
function FSOUND_Record_StartSample; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_StartSample@8' {$ENDIF};
function FSOUND_Record_Stop; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_Stop@0' {$ENDIF};
function FSOUND_Record_GetPosition; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_Record_GetPosition@0' {$ENDIF};
procedure FSOUND_File_SetCallbacks; external FMOD_DLL {$IFDEF WIN32} name '_FSOUND_File_SetCallbacks@20' {$ENDIF};
function FMUSIC_LoadSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_LoadSong@4' {$ENDIF};
function FMUSIC_LoadSongMemory; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_LoadSongMemory@8' {$ENDIF};
function FMUSIC_FreeSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_FreeSong@4' {$ENDIF};
function FMUSIC_PlaySong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_PlaySong@4' {$ENDIF};
function FMUSIC_StopSong; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_StopSong@4' {$ENDIF};
procedure FMUSIC_StopAllSongs; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_StopAllSongs@0' {$ENDIF};
function FMUSIC_SetZxxCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetZxxCallback@8' {$ENDIF};
function FMUSIC_SetRowCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetRowCallback@12' {$ENDIF};
function FMUSIC_SetOrderCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetOrderCallback@12' {$ENDIF};
function FMUSIC_SetInstCallback; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetInstCallback@12' {$ENDIF};
function FMUSIC_SetSample; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetSample@12' {$ENDIF};
function FMUSIC_OptimizeChannels; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_OptimizeChannels@12' {$ENDIF};
function FMUSIC_SetReverb; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetReverb@4' {$ENDIF};
function FMUSIC_SetOrder; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetOrder@8' {$ENDIF};
function FMUSIC_SetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetPaused@8' {$ENDIF};
function FMUSIC_SetMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetMasterVolume@8' {$ENDIF};
function FMUSIC_SetPanSeperation; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_SetPanSeperation@8' {$ENDIF};
function FMUSIC_GetName; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetName@4' {$ENDIF};
function FMUSIC_GetType; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetType@4' {$ENDIF};
function FMUSIC_GetNumOrders; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumOrders@4' {$ENDIF};
function FMUSIC_GetNumPatterns; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumPatterns@4' {$ENDIF};
function FMUSIC_GetNumInstruments; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumInstruments@4' {$ENDIF};
function FMUSIC_GetNumSamples; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumSamples@4' {$ENDIF};
function FMUSIC_GetNumChannels; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetNumChannels@4' {$ENDIF};
function FMUSIC_GetSample; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetSample@8' {$ENDIF};
function FMUSIC_GetPatternLength; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPatternLength@8' {$ENDIF};
function FMUSIC_IsFinished; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_IsFinished@4' {$ENDIF};
function FMUSIC_IsPlaying; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_IsPlaying@4' {$ENDIF};
function FMUSIC_GetMasterVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetMasterVolume@4' {$ENDIF};
function FMUSIC_GetGlobalVolume; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetGlobalVolume@4' {$ENDIF};
function FMUSIC_GetOrder; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetOrder@4' {$ENDIF};
function FMUSIC_GetPattern; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPattern@4' {$ENDIF};
function FMUSIC_GetSpeed; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetSpeed@4' {$ENDIF};
function FMUSIC_GetBPM; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetBPM@4' {$ENDIF};
function FMUSIC_GetRow; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetRow@4' {$ENDIF};
function FMUSIC_GetPaused; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetPaused@4' {$ENDIF};
function FMUSIC_GetTime; external FMOD_DLL {$IFDEF WIN32} name '_FMUSIC_GetTime@4' {$ENDIF};
{$ENDIF}

var
  Saved8087CW: Word;

initialization
{$IFDEF FMOD_DYNAMIC_LOAD}
  FMODHandle := INVALID_MODULEHANDLE_VALUE;
{$ENDIF}
  { Save the current FPU state and then disable FPU exceptions }
  Saved8087CW := Default8087CW;
  Set8087CW($133f); { Disable all fpu exceptions }

finalization
  { Make sure the library is unloaded }
  FMOD_Unload;
  { Reset the FPU to the previous state }
  Set8087CW(Saved8087CW);

end.
