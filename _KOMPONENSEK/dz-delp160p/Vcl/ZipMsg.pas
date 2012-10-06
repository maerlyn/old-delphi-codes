unit ZipMsg;

Interface

Const

GE_FatalZip   		 =10101;
GE_NoZipSpecified	 =10102;
GE_NoMem      		 =10103;
GE_WrongPassword	 =10104;
GE_CopyFile   		 =10105;

RN_ZipSFXData            =10140;  // new v1.6    Rename
RN_NoRenOnSpan           =10141;  // new v1.6    Rename
RN_ProcessFile           =10142;  // new v1.6    Rename
RN_RenameTo              =10143;  // new v1.6    Rename

PW_UnatAddPWMiss	 =10150;
PW_UnatExtPWMiss	 =10151;
PW_Ok                    =10152;  // new v1.6    Password dialog
PW_Cancel                =10153;  // new v1.6    Password dialog
PW_Caption               =10154;  // new v1.6    Password dialog
PW_MessageEnter          =10155;  // new v1.6    Password dialog
PW_MessageConfirm        =10156;  // new v1.6    Password dialog
PW_CancelAll		 =10157;  // new v1.6    Password dialog
PW_Abort		 =10158;  // new v1.6    Password dialog
PW_ForFile		 =10159;  // new v1.6    Password dialog

CF_SourceIsDest          = 10180; // new v1.6	 CopyZippedFiles
CF_OverwriteYN           = 10181; // new v1.6	 CopyZippedFiles
CF_CopyFailed            = 10182; // new v1.6	 CopyZippedFiles
CF_SourceNotFound        = 10183; // new v1.6	 CopyZippedFiles
CF_SFXCopyError          = 10184; // new v1.6	 CopyZippedFiles
CF_DestFileNoOpen        = 10185; // new v1.6	 CopyZippedFiles

LI_ReadZipError		 =10201;
LI_ErrorUnknown		 =10202;
LI_WrongZipStruct	 =10203;
LI_GarbageAtEOF		 =10204;

AD_NothingToZip		 =10301;
AD_UnattPassword	 =10302;
AD_NoFreshenUpdate	 =10303;
AD_AutoSFXWrong          =10304;  // new v1.6    Add AutoSFX
AD_NoStreamDLL		 =10305;	// new v1.6		Add Stream
AD_InIsOutStream	 =10306;	// new v1.6		Add Stream
AD_InvalidName		 =10307;	// new v1.6		Add Stream

DL_NothingToDel		 =10401;

EX_FatalUnZip		 =10501;
EX_UnAttPassword	 =10502;
EX_NoStreamDLL           =10503;  // new v1.6    MemoryExtract

LZ_ZipDllLoaded		 =10601;
LZ_NoZipDllExec		 =10602;
LZ_NoZipDllVers		 =10603;
LZ_NoZipDll   		 =10604;

LU_UnzDllLoaded		 =10701;
LU_NoUnzDllExec		 =10702;
LU_NoUnzDllVers		 =10703;
LU_NoUnzDll		 =10704;

SF_StringToLong		 =10801;		// Changed v1.6
SF_NoZipSFXBin		 =10802;
SF_InputIsNoZip		 =10803;

CZ_NoExeSpecified	 =10901;
CZ_InputNotExe		 =10902;
CZ_SFXTypeUnknown	 =10903;

DS_NoInFile		 =11001;
DS_FileOpen		 =11002;
DS_NotaDrive		 =11003; // Changed a bit v1.52c
DS_DriveNoMount		 =11004; // Changed a bit v1.52c
DS_NoVolume		 =11005;
DS_NoMem		 =11006;
DS_Canceled		 =11007;
DS_FailedSeek		 =11008;
DS_NoOutFile		 =11009;
DS_NoWrite		 =11010;
DS_EOCBadRead		 =11011;
DS_LOHBadRead		 =11012;
DS_CEHBadRead		 =11013;
DS_LOHWrongSig		 =11014;
DS_CEHWrongSig		 =11015;
DS_LONameLen		 =11016;
DS_CENameLen		 =11017;
DS_LOExtraLen		 =11018;
DS_CEExtraLen		 =11019;
DS_DataDesc	         =11020;
DS_ZipData	         =11021;
DS_CECommentLen		 =11022;
DS_EOArchComLen		 =11023;
DS_ErrorUnknown		 =11024;
DS_NoUnattSpan		 =11025;
DS_EntryLost		 =11026;
DS_NoTempFile		 =11027;
DS_LOHBadWrite		 =11028;
DS_CEHBadWrite		 =11029;
DS_EOCBadWrite		 =11030;
DS_ExtWrongSig		 =11031;
DS_NoDiskSpace		 =11032;
DS_InsertDisk		 =11033;
DS_InsertVolume		 =11034;
DS_InDrive     		 =11035;
DS_NoValidZip		 =11036;
DS_FirstInSet		 =11037;
DS_NotLastInSet		 =11038;
DS_AskDeleteFile	 =11039;
DS_AskPrevFile		 =11040;
DS_NoSFXSpan   		 =11041;
DS_CEHBadCopy            =11042;
DS_EOCBadSeek            =11043;
DS_EOCBadCopy            =11044;
DS_FirstFileOnHD         =11045;
DS_InsertAVolume         =11046;  // new v1.52c  DiskSpan
DS_CopyCentral           =11047;  // new v1.52i  DiskSpan
Implementation

End.
