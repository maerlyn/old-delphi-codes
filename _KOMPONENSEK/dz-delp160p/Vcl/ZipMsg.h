#ifndef ZipMsgH
#define ZipMsgH

#define GE_FatalZip			 10101
#define GE_NoZipSpecified	 10102
#define GE_NoMem				 10103
#define GE_WrongPassword	 10104
#define GE_CopyFile			 10105

#define RN_ZipSFXData		 10140   // new v1.6    Rename
#define RN_NoRenOnSpan		 10141   // new v1.6    Rename
#define RN_ProcessFile		 10142   // new v1.6    Rename
#define RN_RenameTo			 10143   // new v1.6    Rename

#define PW_UnatAddPWMiss	 10150
#define PW_UnatExtPWMiss	 10151
#define PW_Ok					 10152   // new v1.6    Password change
#define PW_Cancel				 10153   // new v1.6    Password change
#define PW_Caption			 10154   // new v1.6    Password change
#define PW_MessageEnter		 10155   // new v1.6    Password change
#define PW_MessageConfirm	 10156   // new v1.6    Password change
#define PW_CancelAll			 10157   // new v1.6    Password change
#define PW_Abort				 10158   // new v1.6    Password change
#define PW_ForFile			 10159   // new v1.6    Password change

#define CF_SourceIsDest		 10180	// new v1.6		CopyZippedFiles
#define CF_OverwriteYN		 10181	// new v1.6		CopyZippedFiles
#define CF_CopyFailed		 10182	// new v1.6		CopyZippedFiles
#define CF_SourceNotFound	 10183	// new v1.6		CopyZippedFiles
#define CF_SFXCopyError		 10184	// new v1.6		CopyZippedFiles
#define CF_DestFileNoOpen	 10185	// new v1.6		CopyZippedFiles

#define LI_ReadZipError		 10201
#define LI_ErrorUnknown		 10202	// changed a bit v1.6d
#define LI_WrongZipStruct	 10203
#define LI_GarbageAtEOF		 10204

#define AD_NothingToZip		 10301
#define AD_UnattPassword	 10302
#define AD_NoFreshenUpdate	 10303
#define AD_AutoSFXWrong		 10304	// new v1.6		Add AutoSFX
#define AD_NoStreamDLL		 10305	// new v1.6		Add Stream
#define AD_InIsOutStream	 10306	// new v1.6		Add Stream
#define AD_InvalidName		 10307	// new v1.6		Add Stream

#define DL_NothingToDel		 10401

#define EX_FatalUnZip		 10501
#define EX_UnAttPassword	 10502
#define EX_NoStreamDLL		 10503   // new v1.6    MemoryStream

#define LZ_ZipDllLoaded		 10601
#define LZ_NoZipDllExec		 10602
#define LZ_NoZipDllVers		 10603
#define LZ_NoZipDll			 10604

#define LU_UnzDllLoaded		 10701
#define LU_NoUnzDllExec		 10702
#define LU_NoUnzDllVers		 10703
#define LU_NoUnzDll			 10704

#define SF_StringToLong		 10801	// changed v1.6
#define SF_NoZipSFXBin		 10802
#define SF_InputIsNoZip		 10803

#define CZ_NoExeSpecified	 10901
#define CZ_InputNotExe		 10902
#define CZ_SFXTypeUnknown	 10903

#define DS_NoInFile			 11001
#define DS_FileOpen			 11002
#define DS_NotaDrive			 11003	// Changed a bit v1.52c
#define DS_DriveNoMount		 11004	// Changed a bit v1.52c
#define DS_NoVolume			 11005
#define DS_NoMem				 11006
#define DS_Canceled			 11007
#define DS_FailedSeek		 11008
#define DS_NoOutFile			 11009
#define DS_NoWrite			 11010
#define DS_EOCBadRead		 11011
#define DS_LOHBadRead		 11012
#define DS_CEHBadRead		 11013
#define DS_LOHWrongSig		 11014
#define DS_CEHWrongSig		 11015
#define DS_LONameLen			 11016
#define DS_CENameLen			 11017
#define DS_LOExtraLen		 11018
#define DS_CEExtraLen		 11019
#define DS_DataDesc			 11020
#define DS_ZipData			 11021
#define DS_CECommentLen		 11022
#define DS_EOArchComLen		 11023
#define DS_ErrorUnknown		 11024	// changed a bit v1.6d
#define DS_NoUnattSpan		 11025
#define DS_EntryLost			 11026
#define DS_NoTempFile		 11027
#define DS_LOHBadWrite		 11028
#define DS_CEHBadWrite		 11029
#define DS_EOCBadWrite		 11030
#define DS_ExtWrongSig		 11031
#define DS_NoDiskSpace		 11032
#define DS_InsertDisk		 11033
#define DS_InsertVolume		 11034
#define DS_InDrive			 11035
#define DS_NoValidZip		 11036
#define DS_FirstInSet		 11037
#define DS_NotLastInSet		 11038
#define DS_AskDeleteFile	 11039
#define DS_AskPrevFile		 11040
#define DS_NoSFXSpan			 11041
#define DS_CEHBadCopy		 11042
#define DS_EOCBadSeek		 11043
#define DS_EOCBadCopy		 11044
#define DS_FirstFileOnHD	 11045
#define DS_InsertAVolume	 11046   // new v1.52c  DiskSpan
#define DS_CopyCentral		 11047   // new v1.52j  DiskSpan

#endif
