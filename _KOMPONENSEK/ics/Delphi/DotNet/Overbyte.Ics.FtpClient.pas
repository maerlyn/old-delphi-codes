{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     May 1996 (Win32 version)
Version:      V2.97
Object:       TFtpClient is a FTP client (RFC 959 implementation)
              Support FTPS (SSL) if ICS-SSL is used (RFC 2228 implementation)
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2005 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Overbyte.Ics.FtpClient platform;

interface

uses
    System.IO,
    System.Threading,
    Borland.Vcl.Windows,
    Borland.Vcl.Messages,
    Borland.Vcl.Classes,
    Borland.Vcl.SysUtils,
    Borland.Vcl.WinUtils,
{$IFNDEF NOFORMS}
    Borland.Vcl.Forms,
{$ENDIF}
    Overbyte.Ics.Component,
    OverByte.Ics.WinSock,
    OverByte.Ics.WSocket;

const
  FtpCliVersion      = 288;
  CopyRight : String = ' TFtpCli (c) 1996-2005 F. Piette V2.88 ';

const
  BLOCK_SIZE          = 1460; { 1514 - TCP header size }
  WM_FTP_REQUEST_DONE = WM_USER + 1;
  WM_FTP_SENDDATA     = WM_USER + 2;
{$IFDEF VER80}
  { Delphi 1 has a 255 characters string limitation }
  FTP_RCV_BUF_SIZE = 255;
{$ELSE}
  FTP_RCV_BUF_SIZE = 4096;
{$ENDIF}

type
    TFtpWSocket = TWSocket;

    TFtpOption          = (ftpAcceptLF, ftpNoAutoResumeAt);
    TFtpOptions         = set of TFtpOption;
    TFtpState           = (ftpNotConnected,    ftpReady,
                           ftpInternalReady,   ftpDnsLookup,
                           ftpConnected,       ftpAbort,
                           ftpInternalAbort,   ftpWaitingBanner,
                           ftpWaitingResponse, ftpPasvReady);
    TFtpRequest         = (ftpNone,          ftpOpenAsync,     ftpUserAsync,
                           ftpPassAsync,     ftpCwdAsync,      ftpConnectAsync,
                           ftpReceiveAsync,  ftpDirAsync,      ftpLsAsync,
                           ftpPortAsync,     ftpGetAsync,      ftpDirectoryAsync,
                           ftpListAsync,     ftpSystemAsync,   ftpSystAsync,
                           ftpQuitAsync,     ftpAbortXferAsync,
                           ftpSizeAsync,     ftpPutAsync,      ftpAppendAsync,
                           ftpFileSizeAsync, ftpRqAbort,       ftpMkdAsync,
                           ftpRmdAsync,      ftpRenameAsync,   ftpDeleAsync,
                           ftpRenAsync,      ftpRenToAsync,    ftpRenFromAsync,
                           ftpDeleteAsync,   ftpMkdirAsync,    ftpRmdirAsync,
                           ftpPwdAsync,      ftpQuoteAsync,    ftpCDupAsync,
                           ftpDoQuoteAsync,  ftpTransmitAsync, ftpTypeSetAsync,
                           ftpRestAsync,     ftpRestGetAsync,  ftpRestartGetAsync,
                           ftpRestPutAsync,  ftpRestartPutAsync,
                           ftpMlsdAsync,     ftpFeatAsync,     ftpMlstAsync, 
                           ftpMdtmAsync,     ftpMdtmyyAsync);
    TFtpFct             = (ftpFctNone,       ftpFctOpen,       ftpFctUser,
                           ftpFctPass,       ftpFctCwd,        ftpFctSize,
                           ftpFctMkd,        ftpFctRmd,        ftpFctRenFrom,
                           ftpFctRenTo,      ftpFctGet,        ftpFctDir,
                           ftpFctQuit,       ftpFctSyst,       ftpFctDele,
                           ftpFctPwd,        ftpFctQuote,      ftpFctPut,
                           ftpFctTypeSet,    ftpFctRest,       ftpFctCDup,
                           ftpFctLs,         ftpFctAppend,     ftpFctPort,
                           ftpFctAbortXfer,  ftpFctMlsd,       ftpFctFeat,
                           ftpFctMlst,       ftpFctMdtm,       ftpFctMdtmyy);
    TFtpFctSet          = set of TFtpFct;
    TFtpShareMode       = (ftpShareCompat,    ftpShareExclusive,
                           ftpShareDenyWrite, ftpShareDenyRead,
                           ftpShareDenyNone);
    TFtpDisplayFileMode = (ftpLineByLine, ftpBinary);
    TFtpConnectionType  = (ftpDirect, ftpProxy, ftpSocks4,
                           ftpSocks4A, ftpSocks5);
    TFtpDisplay         = procedure(Sender    : TObject;
                                    var Msg   : String) of object;
    TFtpProgress        = procedure(Sender    : TObject;
                                    Count     : LongInt;
                                    var Abort : Boolean) of object;
    TFtpCommand         = procedure(Sender    : TObject;
                                    var Cmd   : String) of object;
    TFtpRequestDone     = procedure(Sender    : TObject;
                                    RqType    : TFtpRequest;
                                    ErrCode   : Word) of object;
    TFtpReadyToTransmit = procedure(Sender      : TObject;
                                    var bCancel : Boolean) of object;
    TFtpNextProc        = procedure of object;
    FtpException        = class(Exception);

    TCustomFtpCli = class(TIcsComponent)
    protected
        FHostName           : String;
        FPort               : String;
        FDataPortRangeStart : DWORD;  {JT}
        FDataPortRangeEnd   : DWORD;  {JT}
        FLastDataPort       : DWORD;  {JT}
        FLocalAddr          : String; {bb}
        FUserName           : String;
        FPassWord           : String;
        FLocalFileName      : String;
        FHostFileName       : String;
        FHostDirName        : String;
        FDnsResult          : String;
        FType               : Char;
        FShareMode          : Word;
        FDisplayFileMode    : TFtpDisplayFileMode;
        FConnectionType     : TFTPConnectionType;
        FProxyServer        : String;
        FProxyPort          : String;
        FAppendFlag         : Boolean;
        FDisplayFileFlag    : Boolean;
        FControlSocket      : TFtpWSocket;
        FDataSocket         : TFtpWSocket;
        FStartTime          : LongInt;
        FStopTime           : LongInt;
        FState              : TFtpState;
        FStatusCode         : LongInt;
        FRequestResult      : Integer;
        FFctSet             : TFtpFctSet;
        FFctPrv             : TFtpFct;
        FHighLevelResult    : Integer;
        FHighLevelFlag      : Boolean;
        FRestartFlag        : Boolean;
        FOptions            : TFtpOptions;
        FOnDisplay          : TFtpDisplay;
        FOnDisplayFile      : TFtpDisplay;
        FOnError            : TFtpDisplay;
        FOnCommand          : TFtpCommand;
        FOnResponse         : TNotifyEvent;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnStateChange      : TNotifyEvent;
        FOnRequestDone      : TFtpRequestDone;
        FOnProgress         : TFtpProgress;
        FOnReadyToTransmit  : TFtpReadyToTransmit;
        FLocalStream        : TStream;
        FRequestType        : TFtpRequest;
        FRequestDoneFlag    : Boolean;
        FReceiveBuffer      : TBytes;
        FReceiveLen         : Integer;
        FLastResponse       : String;
        FLastResponseSave   : String;  { To save FLastResponse when quitting }
        FPasvResponse       : String;  { To fix REST + PASV transfers }
        FStatusCodeSave     : LongInt; { To save FStatusCode when quitting }
        FErrorMessage       : String;
        FError              : Word;    { To save Error when data connection closed }
        FGetCommand         : String;
        FConnected          : Boolean;
        FSendBuffer         : TBytes;
        FByteCount          : LongInt;
        FSizeResult         : LongInt;
        FDirResult          : String;
        FResumeAt           : LongInt;
        FNext               : TFtpNextProc;
        FWhenConnected      : TFtpNextProc;
        FDoneAsync          : TFtpNextProc;
        FOkResponses        : array [0..15] of Integer;
        FNextRequest        : TFtpNextProc;
        FServerSaidDone     : Boolean;
        FFileReceived       : Boolean;
        FFileSent           : Boolean;
        FPassive            : Boolean;
        FEofFlag            : Boolean;
        FStorAnswerRcvd     : Boolean;
        FPutSessionOpened   : Boolean;
        FStreamFlag         : Boolean;
        FSupportMDTMYY      : Boolean;    { V2.90  does server support set remote date }
        FSupportMDTM        : Boolean;    { V2.90  does server support get remote date }
        FSupportMLST        : Boolean;    { V2.90  does server support new list stuff  }
        FMLSTFacts          : String;     { V2.90  specific new list stuff supported   }
        FSupportSIZE        : Boolean;    { V2.90  does server support remote file size }
        FRemFileDT          : TDateTime;  { V2.90  date/time for MdtmAsync and MdtmYYYYAsync; }
        FRemFacts           : String;     { V2.90 response to MLST command, facts about remote file }
        FLastMultiResponse  : String;     { V2.90  last command response, may be multiple lines, all with CRLF }
        procedure   SetErrorMessage;
        procedure   DataSocketGetDataAvailable(Sender: TObject; ErrCode : word);
        procedure   DataSocketGetSessionConnected(Sender: TObject; ErrCode : word);
        procedure   DataSocketPutSessionConnected(Sender: TObject; ErrCode : word);
        procedure   DataSocketGetSessionAvailable(Sender: TObject; ErrCode : word);
        procedure   DataSocketGetSessionClosed(Sender: TObject; ErrCode : word);
        procedure   DataSocketPutDataAvailable(Sender: TObject; ErrCode : word);
        procedure   DataSocketPutDataSent(Sender: TObject; ErrCode : word);
        procedure   DataSocketPutSessionAvailable(Sender: TObject; ErrCode : word);
        procedure   DataSocketPutSessionClosed(Sender: TObject; ErrCode : word);
        procedure   SendCommand(Cmd : String); virtual;
        procedure   TriggerDisplay(Msg : String); virtual;
        procedure   TriggerReadyToTransmit(var bCancel : Boolean); virtual;
        procedure   TriggerDisplayFile(Msg : String); virtual;
        procedure   TriggerError(Msg: string); virtual;
        procedure   DisplayLastResponse;
{$IFDEF ICS_COMPONENT}
        procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
        function    Progress : Boolean; virtual;
        procedure   ControlSocketDnsLookupDone(Sender: TObject; ErrCode: Word);
        procedure   ControlSocketSessionConnected(Sender: TObject; ErrCode: Word);
        procedure   ControlSocketDataAvailable(Sender: TObject; ErrCode: Word);
        procedure   ControlSocketSessionClosed(Sender: TObject; ErrCode: Word);
        procedure   TriggerRequestDone(ErrCode: Word);
        procedure   TriggerStateChange;
        procedure   StateChange(NewState : TFtpState);
        procedure   PortAsync; virtual;
        procedure   DoneQuitAsync;
        procedure   ExecAsync(RqType      : TFtpRequest;
                              Cmd         : String;
                              OkResponses : array of Word;
                              DoneAsync   : TFtpNextProc);
        procedure   NextExecAsync;
        procedure   DoGetAsync(RqType : TFtpRequest);
        procedure   Next1GetAsync;
        procedure   Next2GetAsync;
        procedure   Next3GetAsync;
        procedure   Next1PutAsync;
        procedure   Next2PutAsync;
        procedure   Next3PutAsync;
        procedure   DoHighLevelAsync;
        procedure   DoPutAppendAsync;
        procedure   HighLevelAsync(RqType : TFtpRequest; Fcts : TFtpFctSet);
        procedure   HandleError(const Msg : String);
        function    CheckReady : Boolean;
        procedure   TransfertStats; virtual;
        procedure   ExtractMoreResults;
        procedure   SetBinary(Value: Boolean);
        function    GetBinary: Boolean;
        function    GetConnected: Boolean;
        procedure   SetShareMode(newValue: TFtpShareMode);
        function    GetShareMode: TFtpShareMode;
        procedure   SetDisplayFileMode(NewValue: TFtpDisplayFileMode);
        function    GetDisplayFileMode: TFtpDisplayFileMode;
        procedure   SetConnectionType(NewValue: TFtpConnectionType);
        function    GetConnectionType: TFtpConnectionType;
        procedure   SetSocksPassword(NewValue: string);
        function    GetSocksPassword: string;
        procedure   SetSocksPort(NewValue: string);
        function    GetSocksPort: string;
        procedure   SetSocksServer(NewValue: string);
        function    GetSocksServer: string;
        procedure   SetSocksUserCode(NewValue: string);
        function    GetSocksUserCode: string;
        procedure   SetPassive(NewValue: Boolean);
        procedure   WndProc(var MsgRec: TMessage); override;
        procedure   WMFtpRequestDone(var msg: TMessage);
                    //message WM_FTP_REQUEST_DONE;
        procedure   WMFtpSendData(var msg: TMessage);
                    //message WM_FTP_SENDDATA;
        procedure   DestroyLocalStream;
        procedure   SetLocalStream (Stream:TStream);
        procedure   SetLocalFileName (FileName:String);
        procedure   SetDataPortRangeStart (NewValue:DWord); {JT}
        procedure   SetDataPortRangeEnd (NewValue:DWord); {JT}
      public
        constructor Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent
                                   {$ELSE}TObject{$ENDIF}); override;
        destructor  Destroy; override;

        procedure   OpenAsync;       virtual;
        procedure   UserAsync;       virtual;
        procedure   PassAsync;       virtual;
        procedure   ConnectAsync;    virtual;
        procedure   QuitAsync;       virtual;
        procedure   AbortAsync;      virtual;
        procedure   GetAsync;        virtual;
        procedure   ExecGetAsync;    virtual;
        procedure   ReceiveAsync;    virtual;
        procedure   PutAsync;        virtual;
        procedure   ExecPutAsync;    virtual;  
        procedure   TransmitAsync;   virtual;
        procedure   AppendAsync;     virtual;
        procedure   ExecAppendAsync; virtual;
        procedure   AppendFileAsync; virtual;
        procedure   ExecDirAsync;    virtual;
        procedure   DirAsync;        virtual;
        procedure   ExecLsAsync;     virtual;
        procedure   LsAsync;         virtual;
        procedure   TypeSetAsync;    virtual;
        procedure   TypeBinaryAsync; virtual;
        procedure   TypeAsciiAsync;  virtual;
        procedure   PwdAsync;        virtual;
        procedure   CwdAsync;        virtual;
        procedure   CDupAsync;       virtual;
        procedure   DirectoryAsync;  virtual;
        procedure   ListAsync;       virtual;
        procedure   SystAsync;       virtual;
        procedure   SystemAsync;     virtual;
        procedure   SizeAsync;       virtual;
        procedure   FileSizeAsync;   virtual;
        procedure   MkdAsync;        virtual;
        procedure   MkdirAsync;      virtual;
        procedure   RmdAsync;        virtual;
        procedure   RmdirAsync;      virtual;
        procedure   DeleAsync;       virtual;
        procedure   DeleteAsync;     virtual;
        procedure   RenFromAsync;    virtual;
        procedure   RenToAsync;      virtual;
        procedure   RenAsync;        virtual;
        procedure   RenameAsync;     virtual;
        procedure   QuoteAsync;      virtual;
        procedure   DoQuoteAsync;    virtual;
        procedure   AbortXferAsync;  virtual;
        procedure   RestAsync;       virtual;
        procedure   RestGetAsync;    virtual;
        procedure   RestartGetAsync; virtual;
        procedure   RestPutAsync;    virtual;
        procedure   RestartPutAsync; virtual;
        procedure   ExecMlsdAsync;   virtual;
        procedure   MlsdAsync;       virtual;  
        procedure   MlstAsync;       virtual;  
        procedure   FeatAsync;       virtual;
        procedure   MdtmAsync;       virtual;  
        procedure   MdtmyyAsync;     virtual;  

        property    LastResponse    : String                 read  FLastResponse;
        property    LastMultiResponse : String               read  FLastMultiResponse;  { V2.90  multiple lines }
        property    ErrorMessage    : String                 read  FErrorMessage;
        property    DnsResult       : String                 read  FDnsResult;
        property    SizeResult      : LongInt                read  FSizeResult;
        property    DirResult       : String                 read  FDirResult;
        property    ControlSocket   : TFtpWSocket            read  FControlSocket;
        property    DataSocket      : TFtpWSocket            read  FDataSocket;
        property    Connected       : Boolean                read  GetConnected;
        property    StatusCode      : LongInt                read  FStatusCode;
        property    ByteCount       : LongInt                read  FByteCount;
        property    State           : TFtpState              read  FState;
        property    RequestType     : TFtpRequest            read  FRequestType;
        property    SupportMDTMYY   : Boolean                read  FSupportMDTMYY ; { V2.90 does server support set remote date }
        property    SupportMDTM     : Boolean                read  FSupportMDTM ;   { V2.90 does server support get remote date }
        property    SupportMLST     : Boolean                read  FSupportMLST ;   { V2.90 does server support new list stuff  }
        property    MLSTFacts       : String                 read  FMLSTFacts ;     { V2.90 specific new list stuff supported   }
        property    RemFacts        : String                 read  FRemFacts ;      { V2.90 facts about remote file             }
        property    SupportSIZE     : Boolean                read  FSupportSIZE ;   { V2.90 does server support remote file size }
        property    RemFileDT       : TDateTime              read  FRemFileDT       { V2.90 date/time for MdtmAsync }
                                                             write FRemFileDT ;     {       and MdtmYYYYAsync; }
        property    Options         : TFtpOptions            read  FOptions
                                                             write FOptions;
        property    LocalStream     : TStream                read  FLocalStream
                                                             write SetLocalStream;
        property ResumeAt           : LongInt                read  FResumeAt
                                                             write FResumeAt;
        property HostName           : String                 read  FHostName 
                                                             write FHostName;
        property Port               : String                 read  FPort
                                                             write FPort;
        property DataPortRangeStart : DWORD                  read  FDataPortRangeStart
                                                             write SetDataPortRangeStart; {JT}
        property DataPortRangeEnd   : DWORD                  read  FDataPortRangeEnd
                                                             write SetDataPortRangeEnd; {JT}
        property LocalAddr          : String                 read  FLocalAddr
                                                             write FLocalAddr; {bb}
        property UserName           : String                 read  FUserName
                                                             write FUserName;
        property PassWord           : String                 read  FPassWord  
                                                             write FPassWord;
        property HostDirName        : String                 read  FHostDirName 
                                                             write FHostDirName;
        property HostFileName       : String                 read  FHostFileName 
                                                             write FHostFileName;
        property LocalFileName      : String                 read  FLocalFileName 
                                                             write SetLocalFileName;
        property DisplayFileFlag    : Boolean                read  FDisplayFileFlag 
                                                             write FDisplayFileFlag;
        property Binary             : Boolean                read  GetBinary 
                                                             write SetBinary;
        property Passive            : Boolean                read  FPassive
                                                             write SetPassive;
        property ShareMode          : TFtpShareMode          read  GetShareMode
                                                             write SetShareMode;
        property DisplayFileMode    : TFtpDisplayFileMode    read  GetDisplayFileMode
                                                             write SetDisplayFileMode;
        property ConnectionType     : TFtpConnectionType     read  GetConnectionType
                                                             write SetConnectionType;
        property ProxyServer        : string                 read  FProxyServer
                                                             write FProxyServer;
        property ProxyPort          : string                 read  FProxyPort
                                                             write FProxyPort;
        property SocksPassword      : string                 read  GetSocksPassword
                                                             write SetSocksPassword;
        property SocksPort          : string                 read  GetSocksPort
                                                             write SetSocksPort;
        property SocksServer        : string                 read  GetSocksServer
                                                             write SetSocksServer;
        property SocksUserCode      : string                 read  GetSocksUserCode
                                                             write SetSocksUserCode;
        property OnDisplay          : TFtpDisplay            read  FOnDisplay
                                                             write FOnDisplay;
        property OnDisplayFile      : TFtpDisplay            read  FOnDisplayFile
                                                             write FOnDisplayFile;
        property OnError            : TFTPDisplay            read  FOnError
                                                             write FOnError;
        property OnCommand          : TFtpCommand            read  FOnCommand
                                                             write FOnCommand;
        property OnResponse         : TNotifyEvent           read  FOnResponse
                                                             write FOnResponse;
        property OnProgress         : TFtpProgress           read  FOnProgress
                                                             write FOnProgress;
        property OnSessionConnected : TSessionConnected      read  FOnSessionConnected
                                                             write FOnSessionConnected;
        property OnSessionClosed    : TSessionClosed         read  FOnSessionClosed
                                                             write FOnSessionClosed;
        property OnRequestDone      : TFtpRequestDone        read  FOnRequestDone
                                                             write FOnRequestDone;
        property OnStateChange      : TNotifyEvent           read  FOnStateChange
                                                             write FOnStateChange;
        property OnReadyToTransmit  : TFtpReadyToTransmit    read  FOnReadyToTransmit
                                                             write FOnReadyToTransmit;
    end;

    TFtpClient = class(TCustomFtpCli)
    protected
        FTimeout       : Integer;                 { Given in seconds }
        FTimeStop      : LongInt;                 { Milli-seconds    }
        FMultiThreaded : Boolean;
        FTerminated    : Boolean;
        //FOnMessagePump : TNotifyEvent;
        function    Progress : Boolean; override;
        function    Synchronize(Proc : TFtpNextProc) : Boolean; virtual;
        function    WaitUntilReady : Boolean; virtual;
      public
        constructor Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent
                                   {$ELSE}TObject{$ENDIF}); override;
        //procedure   MessagePump;
        function    Open       : Boolean;
        function    User       : Boolean;
        function    Pass       : Boolean;
        function    Connect    : Boolean;
        function    Cwd        : Boolean;
        function    Pwd        : Boolean;
        function    CDup       : Boolean;
        function    TypeSet    : Boolean;
        function    TypeBinary : Boolean;
        function    TypeAscii  : Boolean;
        function    Get        : Boolean;
        function    Put        : Boolean;
        function    RestPut    : Boolean;
        function    RestartPut : Boolean;
        function    Quit       : Boolean;
        function    Abort      : Boolean;
        function    Receive    : Boolean;
        function    Transmit   : Boolean;
        function    Append     : Boolean;
        function    AppendFile : Boolean;
        function    Dir        : Boolean;
        function    Directory  : Boolean;
        function    Ls         : Boolean;
        function    List       : Boolean;
        function    Mkd        : Boolean;
        function    Mkdir      : Boolean;
        function    Ren        : Boolean;
        function    Rename     : Boolean;
        function    Dele       : Boolean;
        function    Delete     : Boolean;
        function    Rmd        : Boolean;
        function    Rmdir      : Boolean;
        function    Syst       : Boolean;
        function    System     : Boolean;
        function    Size       : Boolean;
        function    FileSize   : Boolean;
        function    Quote      : Boolean;
        function    DoQuote    : Boolean;
        function    AbortXfer  : Boolean;
        function    RestGet    : Boolean;
        function    RestartGet : Boolean;
        function    Mlsd       : Boolean;    { V2.90 machine list directory     }
        function    Mlst       : Boolean;    { V2.90 machine list file          }
        function    Feat       : Boolean;    { V2.90 supported extensions       }
        function    Mdtm       : Boolean;    { V2.90 get file modification time }
        function    Mdtmyy     : Boolean;    { V2.90 set file modification time }
    {$IFDEF NOFORMS}
        property    Terminated         : Boolean        read  FTerminated
                                                        write FTerminated;
        property    OnMessagePump      : TNotifyEvent   read  FOnMessagePump
                                                        write FOnMessagePump;
    {$ENDIF}
    published
        property Timeout       : Integer read FTimeout       write FTimeout;
        property MultiThreaded : Boolean read FMultiThreaded write FMultiThreaded;
        property HostName;
        property Port;
        property DataPortRangeStart; {JT}
        property DataPortRangeEnd; {JT}
        property LocalAddr; {bb}
        property UserName;
        property PassWord;
        property HostDirName;
        property HostFileName;
        property LocalFileName;
        property DisplayFileFlag;
        property Binary;
        property ErrorMessage;
        property ShareMode;
        property Options;
        property ConnectionType;
        property ProxyServer;
        property SocksPassword;
        property SocksPort;
        property SocksServer;
        property SocksUserCode;
        property OnDisplay;
        property OnDisplayFile;
        property OnCommand;
        property OnError;
        property OnResponse;
        property OnProgress;
        property OnSessionConnected;
        property OnSessionClosed;
        property OnRequestDone;
        property OnStateChange;
        property OnReadyToTransmit;
        property OnBgException;
    end;

{$IFDEF ICS_COMPONENT}
procedure Register;
{$ENDIF}

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF ICS_COMPONENT}
procedure Register;
begin
  RegisterComponents('FPiette', [TFtpClient]);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(const FileName: string): LongInt;
var
    SearchRec: TSearchRec;
begin
    if Borland.Vcl.SysUtils.FindFirst(
           ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then begin
        Result := SearchRec.Size;
        Borland.Vcl.SysUtils.FindClose(SearchRec);
    end
    else
        Result := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Step over blank spaces                                                    }
{ Return -1 if end of string found before non blank character               }
function StpBlk(const Data : String; var nIndex : Integer) : Integer;
begin
    while (nIndex <= Length(Data)) and
          ((Data[nIndex] = ' ') or
           (Data[nIndex] = #9) or
           (Data[nIndex] = #13) or
           (Data[nIndex] = #10)) do
        Inc(nIndex);
    if nIndex > Length(Data) then
        Result := -1
    else
        Result := nIndex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(
    const Data   : String;
    var   nIndex : Integer;
    out   Number : LongInt) : Integer;
var
    bSign : Boolean;
begin
    Number := 0;
    Result := StpBlk(Data, nIndex);

    if Result < 0 then
        Exit;

    { Remember the sign }
    if (Data[nIndex] = '-') or (Data[nIndex] = '+') then begin
        bSign := (Data[nIndex] = '-');
        Inc(nIndex);
    end
    else
        bSign  := FALSE;

    { Convert any number }
    while (nIndex <= Length(Data)) and
          (Data[nIndex] >= '0') and (Data[nIndex] <= '9') do begin
        Number := Number * 10 + ord(Data[nIndex]) - ord('0');
        Inc(nIndex);
    end;

    { Correct for sign }
    if bSign then
        Number := -Number;

    if nIndex > Length(Data) then
        Result := -1
    else
        Result := nIndex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetQuotedString(
    const Data   : String;
    var   nIndex : Integer;
    out   Dst    : String) : Integer;
begin
    Dst := '';
    Result := StpBlk(Data, nIndex);

    if Result < 0 then
        Exit;

    if Data[nIndex] <> '"' then
        Exit;
    Inc(nIndex);

    while nIndex <= Length(Data) do begin
        if Data[nIndex] <> '"' then
            Dst := Dst + Data[nIndex]
        else begin
            Inc(nIndex);
            if Data[nIndex] <> '"' then
                Break;
            Dst := Dst + Data[nIndex];
        end;
        Inc(nIndex);
    end;

    if nIndex > Length(Data) then
        Result := -1
    else
        Result := nIndex;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DataToString(Data : TBytes; Len : Integer) : String;
var
    I  : Integer;
    Ch : Byte;
begin
    Result := '';
    I := 0;
    while I < Len do begin
         Ch := Data[I];
         if (Ch < 32) or (Ch > 126) or (Ch = 35) then
             Result := Result + '#' + Format('%02.2d', [Ch])
         else
             Result := Result + Char(Ch);
         Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* *                                                                     * *}
{* *                            TCustomFtpCli                            * *}
{* *                                                                     * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomFtpCli.Create(
    AOwner: {$IFDEF ICS_COMPONENT}TComponent
            {$ELSE}TObject{$ENDIF});
begin
    inherited Create(AOwner);
    FOnDisplay          := nil;
    FOnDisplayFile      := nil;
    FType               := 'I';
    FPort               := 'ftp';
    FDataPortRangeStart := 0; {JT}
    FDataPortRangeEnd   := 0; {JT}
    FProxyPort          := 'ftp';
    FState              := ftpReady;
    FShareMode          := fmShareExclusive;
    FConnectionType     := ftpDirect;
    FProxyServer        := '';    { Should Socks properties be set to '' as well? }
    FOptions            := [ftpAcceptLF];
    FLocalAddr          := '0.0.0.0'; {bb}
    FControlSocket      := TFtpWSocket.Create(Self);
    FControlSocket.OnSessionConnected := ControlSocketSessionConnected;
    FControlSocket.OnDataAvailable    := ControlSocketDataAvailable;
    FControlSocket.OnSessionClosed    := ControlSocketSessionClosed;
    FControlSocket.OnDnsLookupDone    := ControlSocketDnsLookupDone;
    FDataSocket                       := TFtpWSocket.Create(Self);
    FStreamFlag                       := FALSE;
    SetLength(FReceiveBuffer, FTP_RCV_BUF_SIZE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomFtpCli.Destroy;
begin
    { Be sure to have LocalStream closed }
{$IFDEF TODO}
    DestroyLocalStream;
{$ENDIF}
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WndProc(var MsgRec: TMessage);
begin
    try
         with MsgRec do begin
             case Msg of
             WM_FTP_REQUEST_DONE : WMFtpRequestDone(MsgRec);
             WM_FTP_SENDDATA     : WMFtpSendData(MsgRec);
             else
                 inherited WndProc(MsgRec);
             end;
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WMFtpRequestDone(var msg: TMessage);
begin
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, Msg.LParam);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF ICS_COMPONENT}
procedure TCustomFtpCli.Notification(AComponent: TComponent; Operation: TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FControlSocket then
            FControlSocket := nil
        else if AComponent = FDataSocket then
            FDataSocket := nil;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetErrorMessage;
begin
    if FErrorMessage = '' then
        FErrorMessage := FLastResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DestroyLocalStream;
begin
    if Assigned(FLocalStream) and (FStreamFlag = FALSE) then begin
        FLocalStream.Free;
        FLocalStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetLocalFileName(FileName: String);
begin
    FLocalFileName := FileName;
    if FileName <> '' then
        FStreamFlag := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetLocalStream(Stream: TStream);
begin
    FLocalStream := Stream;
    FStreamFlag  := (Stream <> nil);
    if FStreamFlag then
        FLocalFileName := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDataPortRangeStart(NewValue: DWORD); {JT}
begin
    if NewValue > 65535 then
        HandleError('DataPortRangeStart must be in the range 0..65535')
    else
        FDataPortRangeStart := NewValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDataPortRangeEnd(NewValue: DWORD); {JT}
begin
    if NewValue > 65535 then
        HandleError('DataPortRangeEnd must be in the range 0..65535')
    else
        FDataPortRangeEnd := NewValue
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerDisplayFile(Msg : String);
begin
    if Assigned(FOnDisplayFile) then
        FOnDisplayFile(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerError(Msg : String);
begin
    if Assigned(FOnError) then
        FOnError(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DisplayLastResponse;
begin
     TriggerDisplay('< ' + FLastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.StateChange(NewState : TFtpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetBinary : Boolean;
begin
     Result := (FType = 'I');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetBinary(Value : Boolean);
begin
     if Value then
         FType := 'I'
     else
         FType := 'A';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.Progress : Boolean;
var
    Abort : Boolean;
begin
    Abort := FALSE;
    if Assigned(FOnProgress) then
        FOnProgress(Self, FByteCount + FResumeAt, Abort);

    if Abort then begin
        TriggerDisplay('! Abort requested');
        FDataSocket.Close;
    end;

    Result := not Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SendCommand(Cmd : String);
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self, Cmd);
    TriggerDisplay('> ' + Cmd);
    if FControlSocket.State = wsConnected then
        FControlSocket.SendStr(Cmd + #13 + #10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HandleError(const Msg : String);
begin
      if Assigned(FOnError) then
           TriggerError(Msg)
      else
           raise FtpException.Create(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* Return TRUE if component is ready for next operation.                    }
{* Trigger an error or return FALSE if not ready                            }
function TCustomFtpCli.CheckReady : Boolean;
begin
    Result := (FState in [ftpReady, ftpInternalReady, ftpPasvReady]);
    if not Result then
        HandleError('FTP component not ready');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.OpenAsync;
begin
    if not CheckReady then
        Exit;
    if FConnected then begin
        HandleError('FTP component already connected');
        Exit;
    end;

    if not FHighLevelFlag then
        FRequestType := ftpOpenAsync;

    FRequestDoneFlag  := FALSE;
    FReceiveLen       := 0;
    FRequestResult    := 0;
    FDnsResult        := '';
    StateChange(ftpDnsLookup);
    case FConnectionType of
      ftpDirect, ftpSocks4, ftpSocks4A, ftpSocks5: FControlSocket.DnsLookup(FHostName);
      ftpProxy: FControlSocket.DnsLookup(FProxyServer);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecAsync(
    RqType      : TFtpRequest;
    Cmd         : String;         { Command to execute                      }
    OkResponses : array of Word;  { List of responses like '200 221 342'    }
    DoneAsync   : TFtpNextProc);  { What to do when done                    }
var
    I : Integer;
begin
    if not((Cmd = 'ABOR') or (Cmd = 'STAT') or (Cmd = 'QUIT')) then begin
        if not CheckReady then
            Exit;

        if not FConnected then begin
            HandleError('FTP component not connected');
            Exit;
        end;
    end;

    if not FHighLevelFlag then
        FRequestType := RqType;

    for I := 0 to High(OkResponses) do
        FOkResponses[I] := OkResponses[I];
    FOkResponses[High(OkResponses) + 1] := 0;

    { V2.90 some FTP responses are multiline, welcome banner, FEAT command, }
    { keep them all                                                         }
    FLastMultiResponse := '' ;
    FRequestDoneFlag  := FALSE;
    FNext             := NextExecAsync;
    FDoneAsync        := DoneAsync;
    FErrormessage     := '';
    StateChange(ftpWaitingResponse);
    SendCommand(Cmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExtractMoreResults;
var
    NumericCode : LongInt;
    nIndex      : Integer;
begin
    if FRequestResult = 0 then begin
        if FFctPrv in [ftpFctSize] then begin
            nIndex := 1;
            GetInteger(FLastResponse, nIndex, NumericCode);
            GetInteger(FLastResponse, nIndex, FSizeResult);
        end;
        if FFctPrv in [ftpFctCDup, ftpFctPwd, ftpFctMkd, ftpFctCwd] then begin
            nIndex := 1;
            GetInteger(FLastResponse, nIndex, NumericCode);
            GetQuotedString(FLastResponse, nIndex, FDirResult);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.NextExecAsync;
var
    I      : Integer;
    nIndex : Integer;
begin
WSocketTriggerDebugEvent(Self, 'NextExecAsync');
    DisplayLastResponse;
    if (FLastResponse[1] < '0') or (FLastResponse[1] > '9') then
        Exit; { Continuation line, nothing to do }
    nIndex := 1;
    GetInteger(FLastResponse, nIndex, FStatusCode);
    if FLastResponse[nIndex] = '-' then
        Exit; { Continuation line, nothing to do }

    if FOkResponses[0] = 0 then begin
        { The list of ok responses is empty }
        if FStatusCode >= 500 then begin
            { Not a good response }
            FRequestResult := FStatusCode;
            SetErrorMessage;
        end
        else
            FRequestResult := 0;
    end
    else begin
        { We have a list of ok response codes }
        for I := 0 to High(FOkResponses) do begin
            if FOkResponses[I] = 0 then begin
                { No good response found }
                FRequestResult := FStatusCode;
                SetErrorMessage;
                break;
            end;
            if FOkResponses[I] = FStatusCode then begin
                { Good response found }
                FRequestResult := 0;
                Break;
            end;
        end;
    end;

    if FPassive and (FStatusCode = 227) then begin
        StateChange(ftpPasvReady);               { 19.09.2002 }
        FPasvResponse := FLastResponse;
    end;

    ExtractMoreResults;

    if Assigned(FDoneAsync) then
        FDoneAsync
    else
        TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.QuitAsync;
begin
    DestroyLocalStream;
    FFctPrv := ftpFctQuit;
    ExecAsync(ftpQuitAsync, 'QUIT', [221], DoneQuitAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoneQuitAsync;
begin
    FControlSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CwdAsync;
begin
    if Length(FHostDirName) <= 0 then begin
        HandleError('HostDirName empty');
        Exit;
    end;

    FFctPrv := ftpFctCwd;
    ExecAsync(ftpCwdAsync, 'CWD '+ FHostDirName, [200, 250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.UserAsync;
var
    CmdBuf : String;
begin
    if Length(FUserName) <= 0 then begin
        HandleError('UserName empty');
        Exit;
    end;
    FFctPrv := ftpFctUser;
    if FConnectionType = ftpProxy then begin
        if (CompareText(FPort, 'ftp') = 0) or
           (CompareText(FPort, '21') = 0) then
            CmdBuf := 'USER ' + FUserName + '@' + FHostName
        else
            CmdBuf := 'USER ' + FUserName + '@' + FHostName + ':' + FPort;
    end
    else
        CmdBuf := 'USER ' + FUserName;
    ExecAsync(ftpUserAsync, CmdBuf, [331, 230], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PassAsync;
begin
    if Length(FPassword) <= 0 then begin
        HandleError('Password empty');
        Exit;
    end;
    FFctPrv := ftpFctPass;
    ExecAsync(ftpPassAsync, 'PASS '+ FPassword, [230], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SystAsync;
begin
    FFctPrv := ftpFctSyst;
    ExecAsync(ftpSystAsync, 'SYST', [215], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestAsync;
begin
    FFctPrv   := ftpFctRest;
    { When restarting a download, we always start from current local file   }
    { size. When restarting a upload, we restart from ResumeAt property     }
    { value. This property could be initialized using Size command.         }
    if (not (FRequestType in [ftpRestartPutAsync, ftpRestPutAsync])) and
       (not (ftpNoAutoResumeAt in FOptions)) then
        FResumeAt := GetFileSize(FLocalFileName);
    if FResumeAt > 0 then
        ExecAsync(ftpRestAsync, 'REST ' + IntToStr(FResumeAt), [0], nil)
    else begin
        FRequestDoneFlag  := FALSE;
        FNext             := NextExecAsync;
        FDoneAsync        := nil;
        TriggerRequestDone(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SizeAsync;
begin
    FSizeResult := 0;
    FFctPrv := ftpFctSize;
    ExecAsync(ftpSizeAsync, 'SIZE ' + FHostFileName, [213], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeSetAsync;
begin
    FFctPrv := ftpFctTypeSet;
    ExecAsync(ftpTypeSetAsync, 'TYPE ' + FType, [200], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeBinaryAsync;
begin
    Binary := TRUE;
    TypeSetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TypeAsciiAsync;
begin
    Binary := FALSE;
    TypeSetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MkdAsync;
begin
    FFctPrv := ftpFctMkd;
    ExecAsync(ftpMkdAsync, 'MKD ' + FHostFileName, [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RmdAsync;
begin
    FFctPrv := ftpFctRmd;
    ExecAsync(ftpRmdAsync, 'RMD ' + FHostFileName, [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DeleAsync;
begin
    FFctPrv := ftpFctDele;
    ExecAsync(ftpDeleAsync, 'DELE ' + FHostFileName, [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AbortXferAsync;
begin
    FFctPrv := ftpFctAbortXfer;
    ExecAsync(ftpAbortXferAsync, 'ABOR', [0], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.QuoteAsync;
begin
    FFctPrv := ftpFctQuote;
    ExecAsync(ftpQuoteAsync, FLocalFileName, [0], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PwdAsync;
begin
    FFctPrv := ftpFctPwd;
    ExecAsync(ftpPwdAsync, 'PWD', [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.CDupAsync;
begin
    FFctPrv := ftpFctCDup;
    ExecAsync(ftpCDupAsync, 'CDUP', [250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenFromAsync;
begin
    FFctPrv := ftpFctRenFrom;
    ExecAsync(ftpRenFromAsync, 'RNFR ' + FHostFileName, [350], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenToAsync;
begin
    FFctPrv := ftpFctRenTo;
    ExecAsync(ftpRenToAsync, 'RNTO ' + FLocalFileName, [200, 250, 257], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MlstAsync;     { V2.90 machine list one file        }
begin
    FFctPrv   := ftpFctMlst;
    FRemFacts := '' ;
    ExecAsync(ftpRenToAsync, 'MLST ' + FHostFileName, [250], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.FeatAsync;     { V2.90 supported extensions         }
begin
    FFctPrv := ftpFctFeat;
    ExecAsync(ftpRenToAsync, 'FEAT', [211], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MdtmAsync;     { V2.90 get file modification time   }
begin
    FFctPrv := ftpFctMdtm;
    ExecAsync(ftpRenToAsync, 'MDTM ' + FHostFileName, [213], nil);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MdtmyyAsync;   { V2.90 set file modification time - RhinoSoft Serv-U }
var
    S: String ;
begin
    if FRemFileDT < 10 then begin
        HandleError('Modification date empty');
        Exit;
    end;
    FFctPrv := ftpFctMdtmyy;
    S       := FormatDateTime('yyyymmddhhnnss', FRemFileDT) + '+0' ;  { no time offset=UTC }
    ExecAsync(ftpRenToAsync, 'MDTM ' + S + ' ' + FHostFileName, [213], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AbortAsync;
var
    bFlag : Boolean;
begin
{$IFDEF TRACE} TriggerDisplay('! Aborting'); {$ENDIF}
    bFlag := (FState = ftpDnsLookup);
    StateChange(ftpAbort);
    DestroyLocalStream;
    if bFlag then
        FControlSocket.CancelDnsLookup;
    if FControlSocket.State <> wsClosed then
        FControlSocket.Close;
    if FDataSocket.State <> wsClosed then
        FDataSocket.Close;
    DestroyLocalStream;
    FConnected := FALSE;
    StateChange(ftpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoHighLevelAsync;
begin
{$IFDEF TRACE} TriggerDisplay('! HighLevelAsync ' + IntToStr(FRequestResult)); {$ENDIF}
    if FState = ftpAbort then begin
        {$IFDEF TRACE} TriggerDisplay('! Abort detected'); {$ENDIF}
        FFctSet := [];
        FHighLevelResult := 426;
        FErrorMessage    := '426 Operation aborted.';
    end;

    FNextRequest := DoHighLevelAsync;

    if FRequestResult <> 0 then begin
        { Previous command had errors }
        FHighLevelResult := FRequestResult;
        if (FFctPrv = ftpFctQuit) or (not (ftpFctQuit in FFctSet)) then
            FFctSet := []
        else
            FFctSet := [ftpFctQuit];
    end;

    ExtractMoreResults;

    if ftpFctOpen in FFctSet then begin
        FFctPrv := ftpFctOpen;
        FFctSet := FFctSet - [FFctPrv];
        OpenAsync;
        Exit;
    end;

    if ftpFctUser in FFctSet then begin
        FFctPrv := ftpFctUser;
        FFctSet := FFctSet - [FFctPrv];
        UserAsync;
        Exit;
    end;

    if ftpFctPass in FFctSet then begin
        FFctSet := FFctSet - [ftpFctPass];
        if (FFctPrv <> ftpFctUser) or
           ((FfctPrv = ftpFctUser) and (FStatusCode = 331)) then begin
            FFctPrv := ftpFctPass;
            PassAsync;
            Exit;
        end;
    end;

    if ftpFctCwd in FFctSet then begin
        FFctSet := FFctSet - [ftpFctCwd];
        if Length(FHostDirName) > 0 then begin
            FFctPrv := ftpFctCwd;
            CwdAsync;
            Exit;
        end;
    end;

    if ftpFctCDup in FFctSet then begin
        FFctPrv := ftpFctCDup;
        FFctSet := FFctSet - [FFctPrv];
        CDupAsync;
        Exit;
    end;

    if ftpFctTypeSet in FFctSet then begin
        FFctPrv := ftpFctTypeSet;
        FFctSet := FFctSet - [FFctPrv];
        TypeSetAsync;
        Exit;
    end;

    if ftpFctPort in FFctSet then begin
        FFctPrv := ftpFctPort;
        FFctSet := FFctSet - [FFctPrv];
        PortAsync;
        Exit;
    end;

    if ftpFctRest in FFctSet then begin
        FFctPrv := ftpFctRest;
        FFctSet := FFctSet - [FFctPrv];
        RestAsync;
        Exit;
    end;

    if ftpFctGet in FFctSet then begin
        if (FFctPrv <> ftpFctRest) or
           (FResumeAt < 0) or
           ((FFctPrv = ftpFctRest) and (FStatusCode <> 350)) then
            FResumeAt := 0;

        FFctPrv   := ftpFctGet;
        FFctSet   := FFctSet - [FFctPrv];
        ExecGetAsync;
        Exit;
    end;

    if ftpFctPut in FFctSet then begin
        FFctPrv := ftpFctPut;
        FFctSet := FFctSet - [FFctPrv];
        ExecPutAsync;
        Exit;
    end;

    if ftpFctAppend in FFctSet then begin
        FFctPrv := ftpFctAppend;
        FFctSet := FFctSet - [FFctPrv];
        ExecAppendAsync;
        Exit;
    end;

    if ftpFctDir in FFctSet then begin
        FFctPrv := ftpFctDir;
        FFctSet := FFctSet - [FFctPrv];
        ExecDirAsync;
        Exit;
    end;

    if ftpFctLs in FFctSet then begin
        FFctPrv := ftpFctLs;
        FFctSet := FFctSet - [FFctPrv];
        ExecLsAsync;
        Exit;
    end;

    if ftpFctSyst in FFctSet then begin
        FFctPrv := ftpFctSyst;
        FFctSet := FFctSet - [FFctPrv];
        SystAsync;
        Exit;
    end;

    if ftpFctMkd in FFctSet then begin
        FFctPrv := ftpFctMkd;
        FFctSet := FFctSet - [FFctPrv];
        MkdAsync;
        Exit;
    end;

    if ftpFctRmd in FFctSet then begin
        FFctPrv := ftpFctRmd;
        FFctSet := FFctSet - [FFctPrv];
        RmdAsync;
        Exit;
    end;

    if ftpFctRenFrom in FFctSet then begin
        FFctPrv := ftpFctRenFrom;
        FFctSet := FFctSet - [FFctPrv];
        RenFromAsync;
        Exit;
    end;

    if ftpFctRenTo in FFctSet then begin
        FFctPrv := ftpFctRenTo;
        FFctSet := FFctSet - [FFctPrv];
        RenToAsync;
        Exit;
    end;

    if ftpFctSize in FFctSet then begin
        FFctPrv := ftpFctSize;
        FFctSet := FFctSet - [FFctPrv];
        SizeAsync;
        Exit;
    end;

    if ftpFctDele in FFctSet then begin
        FFctPrv := ftpFctDele;
        FFctSet := FFctSet - [FFctPrv];
        DeleAsync;
        Exit;
    end;

    if ftpFctPwd in FFctSet then begin
        FFctPrv := ftpFctPwd;
        FFctSet := FFctSet - [FFctPrv];
        PwdAsync;
        Exit;
    end;

    if ftpFctAbortXfer in FFctSet then begin
        FFctPrv := ftpFctAbortXfer;
        FFctSet := FFctSet - [FFctPrv];
        AbortXferAsync;
        Exit;
    end;

    if ftpFctQuote in FFctSet then begin
        FFctPrv := ftpFctQuote;
        FFctSet := FFctSet - [FFctPrv];
        QuoteAsync;
        Exit;
    end;

    if ftpFctQuit in FFctSet then begin
        FFctPrv := ftpFctQuit;
        FFctSet := FFctSet - [FFctPrv];
        FLastResponseSave := FLastResponse;
        FStatusCodeSave   := FStatusCode;
        QuitAsync;
        Exit;
    end;

    if ftpFctMlsd in FFctSet then begin     { V2.90 } 
        FFctPrv := ftpFctMlsd;
        FFctSet := FFctSet - [FFctPrv];
        ExecMlsdAsync;
        Exit;
    end;

    {$IFDEF TRACE} TriggerDisplay('! HighLevelAsync done'); {$ENDIF}
    FFctSet          := [];
    FNextRequest     := nil;
    FRequestDoneFlag := FALSE;
    TriggerRequestDone(FHighLevelResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.HighLevelAsync(RqType : TFtpRequest; Fcts : TFtpFctSet);
begin
    if FConnected and (ftpFctOpen in Fcts) then begin
        HandleError('FTP component already connected');
        Exit;
    end;
    if not CheckReady then
        Exit;
    FLastResponseSave := FLastResponse;
    FStatusCodeSave   := -1;
    FRequestType      := RqType;
    FRequestResult    := 0;
    FFctSet           := Fcts;
    FFctPrv           := ftpFctNone;
    FHighLevelResult  := 0;
    FHighLevelFlag    := TRUE;
    FLastResponse     := '';
    FErrorMessage     := '';
    FRestartFlag      := FALSE;
    FNextRequest      := nil;
    DoHighLevelAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ConnectAsync;
begin
    HighLevelAsync(ftpConnectAsync, [ftpFctOpen, ftpFctUser, ftpFctPass]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ReceiveAsync;
begin
    HighLevelAsync(ftpReceiveAsync,
                   [ftpFctOpen, ftpFctUser,    ftpFctPass,
                    ftpFctCwd,  ftpFctTypeSet, ftpFctPort, ftpFctGet,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PutAsync;
begin
    HighLevelAsync(ftpPutAsync,
                   [ftpFctPort, ftpFctPut]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ By A.Burlakov: new procedure for resuming uploads                         }
{ Uses REST + STOR commands instead APPEND                                  }
procedure TCustomFtpCli.RestPutAsync;
begin
    HighLevelAsync(ftpRestPutAsync,
                   [ftpFctRest, ftpFctPort, ftpFctPut]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestartPutAsync;
begin
    HighLevelAsync(ftpRestartPutAsync,
                   [ftpFctOpen,    ftpFctUser, ftpFctPass, ftpFctCwd,
                    ftpFctTypeSet, ftpFctRest, ftpFctPort, ftpFctPut,
                    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TransmitAsync;
begin
    HighLevelAsync(ftpTransmitAsync,
                   [ftpFctOpen, ftpFctUser,    ftpFctPass,
                    ftpFctCwd,  ftpFctTypeSet, ftpFctPort,
                    ftpFctPut,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AppendAsync;
begin
    HighLevelAsync(ftpAppendAsync,
                   [ftpFctPort, ftpFctAppend]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.AppendFileAsync;
begin
    HighLevelAsync(ftpTransmitAsync,
                   [ftpFctOpen,   ftpFctUser,    ftpFctPass,
                    ftpFctCwd,    ftpFctTypeSet, ftpFctPort,
                    ftpFctAppend, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DirAsync;
begin
    HighLevelAsync(ftpDirAsync,
                   [ftpFctPort, ftpFctDir]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DirectoryAsync;
begin
    HighLevelAsync(ftpDirectoryAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctPort, ftpFctDir,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.LsAsync;
begin
    HighLevelAsync(ftpLsAsync,
                   [ftpFctPort, ftpFctLs]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ListAsync;
begin
    HighLevelAsync(ftpListAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctPort, ftpFctLs,   ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SystemAsync;
begin
    HighLevelAsync(ftpSystemAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctSyst, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestartGetAsync;
begin
    HighLevelAsync(ftpRestartGetAsync,
                   [ftpFctOpen,    ftpFctUser, ftpFctPass, ftpFctCwd,
                    ftpFctTypeSet, ftpFctRest, ftpFctPort, ftpFctGet,
                    ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RestGetAsync;
begin
    HighLevelAsync(ftpRestGetAsync,
                   [ftpFctRest, ftpFctPort, ftpFctGet]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.GetAsync;
begin
    HighLevelAsync(ftpGetAsync,
                   [ftpFctPort, ftpFctGet]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MkdirAsync;
begin
    HighLevelAsync(ftpMkdirAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctMkd,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RmdirAsync;
begin
    HighLevelAsync(ftpRmdirAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctRmd,  ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DeleteAsync;
begin
    HighLevelAsync(ftpDeleteAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctDele, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoQuoteAsync;
begin
    HighLevelAsync(ftpDoQuoteAsync,
                   [ftpFctOpen,  ftpFctUser,  ftpFctPass,
                    ftpFctCwd,   ftpFctQuote, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenameAsync;
begin
    HighLevelAsync(ftpRenameAsync,
                   [ftpFctOpen,    ftpFctUser,  ftpFctPass, ftpFctCwd,
                    ftpFctRenFrom, ftpFctRenTo, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.RenAsync;
begin
    HighLevelAsync(ftpRenAsync, [ftpFctRenFrom, ftpFctRenTo]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.FileSizeAsync;
begin
    HighLevelAsync(ftpSizeAsync,
                   [ftpFctOpen, ftpFctUser, ftpFctPass,
                    ftpFctCwd,  ftpFctSize, ftpFctQuit]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.MlsdAsync;    { V2.90 machine list directory        }
begin
    HighLevelAsync(ftpMlsdAsync,
                   [ftpFctPort, ftpFctMlsd]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetDataAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    Len     : Integer;
    Buffer  : TBytes;
    aSocket : TFtpWSocket;
    I, J, K : Integer;
    Line    : String;
begin
    if not Progress then
        Exit;

    aSocket := Sender as TFtpWSocket;

    SetLength(Buffer, 4096);
    Len := aSocket.Receive(Buffer, Length(Buffer));
{TriggerDisplay('! Data received ' + IntToStr(Len));}
    if Len = 0 then
        { Remote has closed, ignore }
    else if Len < 0 then begin
        { An error has occured }
        if (aSocket.State = wsConnected) and
           (aSocket.LastError <> WSAEWOULDBLOCK) then begin
            TriggerDisplay('! Data: Receive error ' + IntToStr(aSocket.LastError));
            aSocket.Shutdown(2);
            Exit;
        end;
    end
    else begin
        { Update our statistics }
        FByteCount := FByteCount + Len;

        if FLocalStream <> nil then begin
            try
                FLocalStream.WriteBuffer(Buffer, Len);
            except
                TriggerDisplay('! Error writing local file');
                aSocket.Shutdown(2);
                Exit;
            end;
        end;

        { If requested to display the received data, do it line by line }
        if FDisplayFileFlag then begin
            case FDisplayFileMode of
            ftpBinary:
                begin
                    {$IFDEF VER80}
                    { 16 bit has max 255 characters per string }
                    if Len > 255 then
                        SetLength(Line, 255)
                    else
                    {$ENDIF}
                    SetLength(Line, Len);
                    for I := 1 to Len do
                        Line[I] := Char(Buffer[I - 1]);
                    TriggerDisplayFile(Line);
                end;
            ftpLineByLine:
                if Len > 0 then begin
                    I := 1;
                    while (I <= Len) do begin
                        J := 1;
                        while (I <= Len) and
                              (Buffer[I] <> 10) and
                              (Buffer[I] <> 13) do begin
                            I := I + 1;
                            J := J + 1;
                        end;
                        {$IFDEF VER80}
                        if (j - 1) > 255 then
                            SetLength(Line, 255)
                        else
                        {$ENDIF}
                        SetLength(Line, J - 1);
                        if Length(Line) > 0 then begin
                            for K := 1 to J - 1 do
                                Line[K] := Char(Buffer[I - J + K - 1]);
                        end;
                        TriggerDisplayFile(Line);
                        while (I <= Len) and
                              ((Buffer[I] = 10) or (Buffer[I] = 13)) do
                            I := I + 1;
                    end;
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionConnected(
    Sender  : TObject;
    ErrCode : word);
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session opened (Get)'); {$ENDIF}

    { Use the socket for the data transmission }
    FDataSocket.OnSessionClosed := DataSocketGetSessionClosed;
    FDataSocket.OnDataAvailable := DataSocketGetDataAvailable;
    FDataSocket.OnDataSent      := nil;

    { Record the starting time }
    FStartTime := LongInt(GetTickCount);

    if ErrCode <> 0 then begin
        FLastResponse := 'Unable to establish data connection, error #' +
                         IntToStr(ErrCode);
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Used for passive mode                                                     }
procedure TCustomFtpCli.DataSocketPutSessionConnected(
    Sender  : TObject;
    ErrCode : word);
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session opened (Put)'); {$ENDIF}

    { Use the socket for the data transmission }
    FDataSocket.OnSessionClosed := DataSocketPutSessionClosed;
    FDataSocket.OnDataAvailable := nil;
    FDataSocket.OnDataSent      := nil;

    { Record we opened data session }
    FPutSessionOpened := TRUE;

    { Record the starting time }
    FStartTime := LongInt(GetTickCount);

    if ErrCode <> 0 then begin
        FLastResponse := 'Unable to establish data connection, error #' +
                         IntToStr(ErrCode);
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    StateChange(ftpWaitingResponse);
    FNext := Next1PutAsync;

    if FAppendFlag then
        SendCommand('APPE ' + FHostFileName)
    else
        SendCommand('STOR ' + FHostFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    aSocket : TSocket;
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session requested'); {$ENDIF}
    { Accept the incomming connection initiated by the FTP server for data }
    aSocket := FDataSocket.Accept;

    { Close the listening socket, we don't need it anymore }
    FDataSocket.Close;

    { Reuse the socket for the data transmission }
    FDataSocket.OnSessionClosed  := DataSocketGetSessionClosed;
    FDataSocket.OnDataAvailable  := DataSocketGetDataAvailable;
    FDataSocket.OnDataSent       := nil;
    FDataSocket.HSocket          := aSocket;
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop];   { 26/10/02 }

    { Record the starting time }
    FStartTime := LongInt(GetTickCount);
    {$IFDEF TRACE} TriggerDisplay('! Data Session opened'); {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketGetSessionClosed(
    Sender  : TObject;
    ErrCode : word);
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session closed'); {$ENDIF}

    DestroyLocalStream;
    FFileReceived := TRUE;
    FError        := ErrCode;
    Next3GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutSessionAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    aSocket : TSocket;
    SndBufSize : Integer;
    OptLen     : Integer;
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session requested'); {$ENDIF}
    { Accept the incomming connection initiated by the FTP server for data }
    aSocket := FDataSocket.Accept;

    { Close the listening socket, we don't need it anymore }
    FDataSocket.Close;

    { Reuse the socket for the data transmission }
    FDataSocket.OnSessionClosed  := DataSocketPutSessionClosed;
    FDataSocket.OnDataAvailable  := DataSocketPutDataAvailable;
    FDataSocket.OnDataSent       := DataSocketPutDataSent;
{   FDataSocket.OnDisplay        := FOnDisplay; } { Debugging only }
    FDataSocket.HSocket          := aSocket;
    FDataSocket.ComponentOptions := [wsoNoReceiveLoop];   { 26/10/02 }

    OptLen := SizeOf(SndBufSize);
    if WSocket_getsockopt(FDataSocket.HSocket, SOL_SOCKET,
                          SO_SNDBUF,
                          SndBufSize, OptLen) = SOCKET_ERROR then begin
        HandleError('winsock.getsockopt(SO_SNDBUF) failed');
        Exit;
    end;

    { Be sure to gracefully close the socket }
    FDataSocket.LingerOnOff   := wsLingerOff;
    FDataSocket.LingerTimeout := 10;
    FDataSocket.SetLingerOption;
{   FStorAnswerRcvd := TRUE; } { V240 INSERTED line }
    FPutSessionOpened := TRUE;
    if FStorAnswerRcvd and (FStartTime = 0) then
        PostMessage(Handle, WM_FTP_SENDDATA, 0, 0);

    {$IFDEF TRACE} TriggerDisplay('! Data Session opened'); {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.WMFtpSendData(var msg: TMessage);
begin
    { Record the starting time }
    FStartTime := LongInt(GetTickCount);

    { Send first data block }
    DataSocketPutDataSent(FDataSocket, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutDataSent(
    Sender  : TObject;
    ErrCode : word);
var
    Count : Integer;
begin
    if (FLocalStream = nil) or (not Progress) then
        Exit;
    if FLocalStream = nil then
        Exit;   { Could be set to nil by Progress function }

    if ErrCode <> 0 then begin
        TriggerDisplay('! Error #' + IntToStr(ErrCode) + ' sending data');
        FDataSocket.Close;
        Exit;
    end;

    if FEofFlag or (not FStorAnswerRcvd) or (not FPutSessionOpened) then
       Exit;

    try
        SetLength(FSendBuffer, BLOCK_SIZE);
        Count := FLocalStream.Read(FSendBuffer, BLOCK_SIZE);
        if Count > 0 then begin
            FByteCount := FByteCount + Count;
            FDataSocket.Send(FSendBuffer, Count);
        end
        else begin { EOF }
            {$IFNDEF VER80}
            { For an unknown reason, winsock need time to send last data }
            { buffer. Without this delay, we may end with a partial file }
            { transfer. See comments in DoPutAppendAsync function.       }
            { Normally using Linger option would handle this case. But   }
            { many winsock implementations will end with a 10055 error   }
            { after a lot of consecutive file transfers.                 }
            Sleep(100);
            {$ENDIF}
            FDataSocket.CloseDelayed;
            FEofFlag := TRUE;
        end;
    except
        TriggerDisplay('! Error reading file');
        FDataSocket.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutSessionClosed(
    Sender  : TObject;
    ErrCode : word);
begin
    {$IFDEF TRACE} TriggerDisplay('! Data Session closed'); {$ENDIF}
    { close the local file }
    DestroyLocalStream;
    FFileSent := TRUE;
    FError    := ErrCode;
    Next3PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DataSocketPutDataAvailable(
    Sender  : TObject;
    ErrCode : word);
var
    Buffer  : TBytes;
    aSocket : TFtpWSocket;
begin
    { We don't wants to receive data here because we are sending, not       }
    { receiving. But in order to not crash if we receive something, just    }
    { get it and do nothing with it !                                       }
    SetLength(Buffer, 2048);
    aSocket := Sender as TFtpWSocket;
    aSocket.Receive(Buffer, Length(Buffer));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TransfertStats;
var
    Buffer   : String;
    BytesSec : LongInt;
begin
    FStopTime := LongInt(GetTickCount);
    Buffer    := IntToSTr(FByteCount) + ' bytes received/sent in ' +
                 IntToStr((FStopTime - FStartTime) div 1000) + ' seconds';

    if FStopTime <> FStartTime then begin
        if FByteCount > 32767 then
            BytesSec := 1000 * (FByteCount div (FStopTime - FStartTime))
        else
            BytesSec := (1000 * FByteCount) div (FStopTime - FStartTime);
        Buffer := Buffer + ' (' + IntToStr(BytesSec) + ' Bytes/sec)';
    end;
    TriggerDisplay('! ' + Buffer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecGetAsync;
begin
    DoGetAsync(ftpGetAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecDirAsync;
begin
    DoGetAsync(ftpDirAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecLsAsync;
begin
    DoGetAsync(ftpLsAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecMlsdAsync;     { V2.90 }
begin
    DoGetAsync(ftpMlsdAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetShareMode(newValue : TFtpShareMode);
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case newValue of
    ftpShareCompat    : FShareMode := fmShareCompat;
    ftpShareExclusive : FShareMode := fmShareExclusive;
    ftpShareDenyWrite : FShareMode := fmShareDenyWrite;
    ftpShareDenyRead  : FShareMode := fmShareDenyRead;
    ftpShareDenyNone  : FShareMode := fmShareDenyNone;
    else
        FShareMode := fmShareExclusive;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetShareMode : TFtpShareMode;
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    case FShareMode of
    fmShareCompat    : Result := ftpShareCompat;
    fmShareExclusive : Result := ftpShareExclusive;
    fmShareDenyWrite : Result := ftpShareDenyWrite;
    fmShareDenyRead  : Result := ftpShareDenyRead;
    fmShareDenyNone  : Result := ftpShareDenyNone;
    else
        Result := ftpShareExclusive;
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetDisplayFileMode(NewValue : TFtpDisplayFileMode);
begin
    case NewValue of
        ftpLineByLine, ftpBinary : FDisplayFileMode := NewValue;
    else
        FDisplayFileMode := ftpLineByLine;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetDisplayFileMode : TFtpDisplayFileMode;
begin
    case FDisplayFileMode of
      ftpLineByLine: Result := ftpLineByLine;
      ftpBinary: Result := ftpBinary;
    else
      Result := ftpLineByLine;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetConnectionType(NewValue: TFtpConnectionType);
begin
    { Must be disconnected to change the connection type }
    if FConnected then begin
        HandleError('FTP component connected');
        Exit;
    end;
{$IFDEF FIX_THIS_TODO}
    { Change connection type }
    case NewValue of
    ftpDirect: begin
                 FConnectionType                    := NewValue;
                 FControlSocket.SocksAuthentication := socksNoAuthentication;
                 FDataSocket.SocksAuthentication    := socksNoAuthentication;
               end;
    ftpProxy: begin
                FConnectionType                     := NewValue;
                FPassive                            := TRUE;
                FControlSocket.SocksAuthentication  := socksNoAuthentication;
                FDataSocket.SocksAuthentication     := socksNoAuthentication;
              end;
    ftpSocks4: begin
                 FConnectionType := NewValue;
                 FPassive        := TRUE;
                 with FControlSocket do begin
                     SocksLevel          := '4';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
                 with FDataSocket do begin
                     SocksLevel          := '4';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
               end;
    ftpSocks4A: begin
                  FConnectionType := NewValue;
                  FPassive        := TRUE;
                  with FControlSocket do begin
                      SocksLevel          := '4A';
                      SocksAuthentication := socksAuthenticateUsercode;
                  end;
                  with FDataSocket do begin
                      SocksLevel          := '4A';
                      SocksAuthentication := socksAuthenticateUsercode;
                  end;
                end;
    ftpSocks5: begin
                 FConnectionType := NewValue;
                 FPassive        := TRUE;
                 with FControlSocket do begin
                     SocksLevel          := '5';
                     SocksAuthentication := socksAuthenticateUsercode;
                 end;
                 with FDataSocket do begin
                   SocksLevel          := '5';
                   SocksAuthentication := socksAuthenticateUsercode;
                 end;
               end;
    else
        FConnectionType                    := ftpDirect;
        FControlSocket.SocksAuthentication := socksNoAuthentication;
        FDataSocket.SocksAuthentication    := socksNoAuthentication;
    end;
{$ELSE}
    FConnectionType                    := NewValue;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetConnectionType: TFtpConnectionType;
begin
  case FConnectionType of
    ftpDirect: Result := ftpDirect;
    ftpProxy: Result := ftpProxy;
    ftpSocks4: Result := ftpSocks4;
    ftpSocks4A: Result := ftpSocks4A;
    ftpSocks5: Result := ftpSocks5;
  else
    Result := ftpDirect;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksPassword(NewValue: string);
begin
{$IFDEF FIX_THIS_TODO}
    FControlSocket.SocksPassword := NewValue;
    FDataSocket.SocksPassword := NewValue;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksPassword: string;
begin
{$IFDEF FIX_THIS_TODO}
    Result := FControlSocket.SocksPassword;
{$ELSE}
    Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksPort(NewValue: string);
begin
{$IFDEF FIX_THIS_TODO}
    FControlSocket.SocksPort := NewValue;
    FDataSocket.SocksPort := NewValue;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksPort: string;
begin
{$IFDEF FIX_THIS_TODO}
    Result := FControlSocket.SocksPort;
{$ELSE}
    Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksServer(NewValue: string);
begin
{$IFDEF FIX_THIS_TODO}
    FControlSocket.SocksServer := NewValue;
    FDataSocket.SocksServer := NewValue;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksServer: string;
begin
{$IFDEF FIX_THIS_TODO}
    Result := FControlSocket.SocksServer;
{$ELSE}
    Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetSocksUserCode(NewValue: string);
begin
{$IFDEF FIX_THIS_TODO}
    FControlSocket.SocksUserCode := NewValue;
    FDataSocket.SocksUserCode := NewValue;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetSocksUserCode: string;
begin
{$IFDEF FIX_THIS_TODO}
    Result := FControlSocket.SocksUserCode;
{$ELSE}
    Result := '';
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.SetPassive(NewValue: Boolean);
begin
  { Passive state must not be changed if Proxy or Socks connection type is selected }
  case FConnectionType of
    ftpDirect: FPassive := NewValue;
    ftpProxy, ftpSocks4, ftpSocks4A, ftpSocks5: FPassive := TRUE;
  end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Receive a file or a directory list of a file list                         }
procedure TCustomFtpCli.DoGetAsync(RqType : TFtpRequest);
{$IFDEF VER80}
const
    FILE_END = 2;
{$ENDIF}
var
    Temp       : String;
    I          : Integer;
    TargetPort : WORD;    { 10/30/99 }
    TargetIP   : String;
begin
    if not FConnected then begin
        HandleError(FGetCommand + ': not connected');
        Exit;
    end;

    FServerSaidDone    := FALSE;
    FFileReceived      := FALSE;
    FRequestDoneFlag   := FALSE;
    FByteCount         := 0;
    FError             := 0;

    { If no filename was assigned, check if maybe we wanna view it, }
    { meaning - FDisplayFileFlag }
    if (not Assigned(FLocalFileName) or (Length(FLocalFileName) <= 0))
       and not (FDisplayFileFlag or FStreamFlag) then begin
        FLastResponse := 'Get or dir command require LocalFileName to be set';
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if not FHighLevelFlag then
        FRequestType := RqType;

    case RqType of
    ftpGetAsync:  FGetCommand := 'RETR';
    ftpDirAsync:  FGetCommand := 'LIST';
    ftpLsAsync:   FGetCommand := 'NLST';
    end;

    FDataSocket.OnSessionAvailable := DataSocketGetSessionAvailable;

{$IFDEF VER80}
    { With Delphi 1 you need to nul terminate each string }
    FLocalFileName[Length(FLocalFileName) + 1] := chr(0);
{$ENDIF}

    { open the destination file }
    { Don't open a file if we're on FDisplayFileFlag }
    if not FDisplayFileFlag then
    try
        DestroyLocalStream;
        if FResumeAt <= 0 then begin
            if not Assigned(FLocalStream) and not FStreamFlag then begin
                FLocalStream := TFileStream.Create(FLocalFileName, fmCreate);
                if FShareMode <> 0 then begin
                    { Not default mode, need to close and reopen file with }
                    { the given mode                                       }
                    FLocalStream.Free;
                    FLocalStream := TFileStream.Create(FLocalFileName,
                                                       fmOpenWrite + FShareMode);
                end;
            end;
        end
        else begin
            if not Assigned(FLocalStream) and not FStreamFlag then
               FLocalStream := TFileStream.Create(FLocalFileName,
                                                  fmOpenWrite + FShareMode);
            FLocalStream.Seek(FResumeAt, soFromBeginning)
        end;
    except
        FLastResponse := 'Unable to open local file ' + FLocalFileName;
        FStatusCode   := 550;
        SetErrorMessage;
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if FPassive then begin
        Temp := FPasvResponse;
        Delete(Temp, 1, Pos('(', Temp));

        TargetIP := '';
        for I := 1 to 4 do begin
            TargetIP := TargetIP + Copy(Temp, 1, Pos(',',Temp) - 1) + '.';
            Delete(Temp, 1, Pos(',', Temp));
        end;
        TargetIP := Copy(TargetIP, 1, Length(TargetIP) - 1);

        TargetPort := StrToInt(Copy(Temp, 1, Pos(',', Temp) - 1)) * 256;
        Delete(Temp, 1, Pos(',', Temp));
        TargetPort := TargetPort + StrToInt(Copy(Temp, 1, Pos(')', Temp) - 1));

        FDataSocket.Port               := IntToStr(TargetPort);
        FDataSocket.Addr               := TargetIP; {ControlSocket.Addr;}
        FDataSocket.LocalAddr          := FLocalAddr; {bb}        
        FDataSocket.OnSessionConnected := DataSocketGetSessionConnected;
        FDataSocket.LingerOnOff        := wsLingerOff;
        FDataSocket.LingerTimeout      := 0;
        FDataSocket.ComponentOptions   := [wsoNoReceiveLoop];   { 26/10/02 }
        try
            FDataSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '550 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 550;
                SetErrorMessage;
                FDataSocket.Close;
                FRequestResult := FStatusCode;
                TriggerRequestDone(FRequestResult);
                exit;
            end;
        end;
    end;

    StateChange(ftpWaitingResponse);
    FNext := Next1GetAsync;
    if Length(FHostFileName) > 0 then
        SendCommand(FGetCommand + ' ' + FHostFileName)
    else
        SendCommand(FGetCommand);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when receiving the response for the RETR command we sent    }
procedure TCustomFtpCli.Next1GetAsync;
var
    nIndex : Integer;
begin
    DisplayLastResponse;
    nIndex := 1;
    GetInteger(FLastResponse, nIndex, FStatusCode);
    if not (((FStatusCode div 10) = 15) or   { Accept range 150-159 }
            (FStatusCode = 125)) then begin  { Accept code 125      }
        SetErrorMessage;
        FNext := nil;
        FDataSocket.Close;
        DestroyLocalStream;
        { Reset the starting position }
        FResumeAt      := 0;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FNext := Next2GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the FTP server has sent the file we asked to GET       }
procedure TCustomFtpCli.Next2GetAsync;
var
    nIndex : Integer;
begin
    DisplayLastResponse;
    nIndex := 1;
    GetInteger(FLastResponse, nIndex, FStatusCode);
    if not ((FStatusCode = 125) or (FStatusCode = 226) or
            (FStatusCode = 250)) then begin
        SetErrorMessage;
        DestroyLocalStream;
        FDataSocket.Close;
        TriggerDisplay('! RETR/LIST/NLST Failed');
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FServerSaidDone := TRUE;
    Next3GetAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here either if the file has been received of the FTP server has  }
{ his response.                                                             }
procedure TCustomFtpCli.Next3GetAsync;
begin
    {$IFDEF TRACE} TriggerDisplay('! Next3GetAsync'); {$ENDIF}
    if (not FServerSaidDone) or (not FFileReceived) then
        Exit;

    { Display statistics }
    TransfertStats;

    { Reset the starting position }
    FResumeAt      := 0;
    FRequestResult := FError;
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecPutAsync;
begin
    FAppendFlag  := FALSE;
    FRequestType := ftpPutAsync;
    DoPutAppendAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ExecAppendAsync;
begin
    FAppendFlag  := TRUE;
    FRequestType := ftpAppendAsync;
    DoPutAppendAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.DoPutAppendAsync;
{$IFDEF VER80}
const
    FILE_END = 2;
{$ENDIF}
var
    Temp        : String;
    I           : Integer;
    TargetPort  : WORD;   { 10/30/99 }
    TargetIP    : String;
    bCancel     : Boolean;
begin
    if not FConnected then begin
        HandleError('STOR/APPE: not connected');
        Exit;
    end;

    if (not FStreamFlag) and (Length(FLocalFileName) <= 0) then begin
        HandleError('LocalFileName empty');
        Exit;
    end;

    FServerSaidDone    := FALSE;
    FFileSent          := FALSE;
    FRequestDoneFlag   := FALSE;
    FPutSessionOpened  := FALSE;
    FStorAnswerRcvd    := FALSE;
    FStartTime         := 0;
    FByteCount         := 0;
    FError             := 0;

    bCancel := FALSE;
    TriggerReadyToTransmit(bCancel);
    if bCancel then begin
        FErrorMessage := '426 Transmit cancelled by application';
        FStatusCode   := 426;
        TriggerDisplay('! ' + FErrorMessage);
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    FDataSocket.OnSessionAvailable := DataSocketPutSessionAvailable;
{$IFDEF VER80}
    { With Delphi 1 you need to nul terminate each string }
    FLocalFileName[Length(FLocalFileName) + 1] := chr(0);
{$ENDIF}

    { open the local source file }
    try
        { Be sure to have previous instance closed }
        DestroyLocalStream;
        if not Assigned(FLocalStream) and not FStreamFlag then
            FLocalStream := TFileStream.Create(FLocalFileName,
                                               fmOpenRead + FShareMode);
        FEofFlag     := FALSE;
        if FResumeAt > 0 then
            FLocalStream.Seek(FResumeAt, soFromBeginning);
    except
        FErrorMessage := '426 Unable to open local file ' + FLocalFileName;
        FStatusCode   := 426;
        TriggerDisplay('! ' + FErrorMessage);
        FDataSocket.Close;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if FPassive then begin
        Temp := FPasvResponse;  { 26/12/99 }
        Delete(Temp, 1, Pos('(', Temp));
        TargetIP := '';
        for I := 1 to 4 do begin
            TargetIP := TargetIP + Copy(Temp, 1, Pos(',',Temp) - 1) + '.';
            Delete(Temp, 1, Pos(',', Temp));
        end;
        TargetIP := Copy(TargetIP, 1, Length(TargetIP) - 1);

        TargetPort := StrToInt(Copy(Temp, 1, Pos(',', Temp) - 1)) * 256;
        Delete(Temp, 1, Pos(',', Temp));
        TargetPort := TargetPort + StrToInt(Copy(Temp, 1, Pos(')', Temp) - 1));

        FDataSocket.Port               := IntToStr(TargetPort);
        FDataSocket.Addr               := TargetIP; {ControlSocket.Addr;}
        FDataSocket.LocalAddr          := FLocalAddr; {bb}
        FDataSocket.OnSessionConnected := DataSocketPutSessionConnected;
        { Normally we should use LingerOn with a timeout. But doing so will }
        { often result in error 10055 triggered after a lot of consecutive  }
        { file transfers. There is code in DataSocketPutDataSent to make    }
        { sure last packet is sent completely.                              }
        FDataSocket.LingerOnOff        := wsLingerOff;
        FDataSocket.LingerTimeout      := 0;
        FDataSocket.ComponentOptions   := [wsoNoReceiveLoop];   { 26/10/02 }
        try
            FDataSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '426 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 426;
                SetErrorMessage;
                FDataSocket.Close;
                FRequestResult := FStatusCode;
                TriggerRequestDone(FRequestResult);
                Exit;
            end;
        end;
        Exit;
    end;

    StateChange(ftpWaitingResponse);
    FNext := Next1PutAsync;

    if FAppendFlag then
        SendCommand('APPE ' + FHostFileName)
    else
        SendCommand('STOR ' + FHostFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when receiving the response for the STOR command we sent    }
procedure TCustomFtpCli.Next1PutAsync;
var
    nIndex : Integer;
begin
    DisplayLastResponse;
    if (FLastResponse[1] < '0') or (FLastResponse[1] > '9') then
        Exit; { Continuation line, nothing to do }
    nIndex := 1;
    GetInteger(FLastResponse, nIndex, FStatusCode);
    if FLastResponse[nIndex] = '-' then
        Exit; { Continuation line, nothing to do }

    if not ((FStatusCode = 150) or (FStatusCode = 125)) then begin
        SetErrorMessage;
        FNext := nil;
        FDataSocket.Close;
        DestroyLocalStream;
        { Reset the starting position }
        FResumeAt      := 0;
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;

    if FPassive then begin
        { Send the first data block }
        {$IFDEF TRACE} TriggerDisplay('! Send first block'); {$ENDIF}
        FStorAnswerRcvd        := TRUE;
        FDataSocket.OnDataSent := DataSocketPutDataSent;
        DataSocketPutDataSent(FDataSocket, 0);
    end
    else begin
        { V240 FStorAnswerRcvd := TRUE; }
        FStorAnswerRcvd := TRUE;
        if FPutSessionOpened and (FStartTime = 0) then
            PostMessage(Handle, WM_FTP_SENDDATA, 0, 0);
    end;


    FNext := Next2PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the FTP server has received the file we sent (STOR)    }
procedure TCustomFtpCli.Next2PutAsync;
var
    nIndex : Integer;
begin
    DisplayLastResponse;
    if (FLastResponse[1] < '0') or (FLastResponse[1] > '9') then
        Exit; { Continuation line, nothing to do }
    nIndex := 1;
    GetInteger(FLastResponse, nIndex, FStatusCode);
    if FLastResponse[nIndex] = '-' then
        Exit; { Continuation line, nothing to do }
    if not ((FStatusCode = 226) or (FStatusCode = 250)) then begin
        SetErrorMessage;
        DestroyLocalStream;
        FDataSocket.Close;
        TriggerDisplay('! STOR Failed');
        FRequestResult := FStatusCode;
        TriggerRequestDone(FRequestResult);
        Exit;
    end;
    FServerSaidDone := TRUE;
    Next3PutAsync;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We comes here when the file has been sent or when the FTP server tell us  }
{ he recived the file.                                                      }
procedure TCustomFtpCli.Next3PutAsync;
begin
    {$IFDEF TRACE} TriggerDisplay('! Next3PutAsync'); {$ENDIF}
    if (not FServerSaidDone) or (not FFileSent) then
        Exit;

    { Display statistics }
    TransfertStats;

    { Reset the starting position }
    FResumeAt      := 0;
    FRequestResult := FError;
    TriggerRequestDone(FRequestResult);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.PortAsync;
var
    Msg          : String;
    saddr        : TSockAddrIn;
    saddrlen     : Integer;
    DataPort     : DWORD;  { 10/30/99 }
    IPAddr       : TInAddr;
    StartDataPort: DWORD;
begin
    { Makes the data socket listening for data connection }
    FDataSocket.Proto              := 'tcp';
    FDataSocket.Addr               := '0.0.0.0';  { INADDR_ANY }
    FDataSocket.Port               := '0';        { IPPORT_ANY }
    FDataSocket.OnSessionAvailable := nil;
    FDataSocket.OnSessionClosed    := nil;
    FDataSocket.OnDataAvailable    := nil;

    if FPassive then
        DataPort := 0    { Not needed, makes compiler happy }
    else begin
        if (ftpFctGet in FFctSet) or (ftpFctDir in FFctSet) then {G.B. 2002/07/12}
            FDataSocket.OnSessionAvailable := DataSocketGetSessionAvailable
        else if ftpFctPut in FFctSet then
            FDataSocket.OnSessionAvailable := DataSocketPutSessionAvailable;
        FDataSocket.LingerOnOff        := wsLingerOn;
        FDataSocket.LingerTimeout      := 10;

        if (FDataPortRangeStart = 0) and (FDataPortRangeEnd = 0) then begin
            FDataSocket.Listen;
            { Get the port number as assigned by Windows }
            saddrLen  := SizeOf(saddr);
            FDataSocket.GetSockName(saddr, saddrLen);
            DataPort  := WSocket_ntohs(saddr.sin_port);
        end
        else begin
            { We use a data port range. Check if the range is valid }
            if FDataPortRangeStart > FDataPortRangeEnd then begin
                HandleError('DataPortRangeEnd must be greater than DataPortRangeStart');
                Exit;
            end;
            if (FLastDataPort < FDataPortRangeStart) or
               (FLastDataPort > FDataPortRangeEnd) then
                FLastDataPort := FDataPortRangeStart;
            DataPort      := FLastDataPort;
            StartDataPort := DataPort;
            while TRUE do begin
                FDataSocket.Port := IntToStr(DataPort);
                try
                    FDataSocket.Listen;
                    break;                { Found a free port }
                except
                    if FDataSocket.LastError = WSAEADDRINUSE then begin
                        DataPort := DataPort + 1;
                        if DataPort > FDataPortRangeEnd then
                            DataPort := FDataPortRangeStart;
                        if DataPort = StartDataPort then begin
                            HandleError('All ports in DataPortRange are in use');
                            Exit;
                        end;
                    end
                    else begin
                        HandleError('Data connection winsock.bind failed, error #' +
                                    IntToStr(FDataSocket.LastError));
                        Exit;
                    end;
                end;
            end;
            FLastDataPort := DataPort + 1;
            if FLastDataPort > FDataPortRangeEnd then
                FLastDataPort := FDataPortRangeStart;
        end;
    end;

    { Get our IP address from our control socket }
    saddrlen := SizeOf(saddr);
    FControlSocket.GetSockName(saddr, saddrlen);
    IPAddr   := saddr.sin_addr;

    { Strange behaviour of PWS (FrontPage 97 Web Server for W95) }
    { which do not like effective address when localhost is used }
    if FPassive then
        Msg := 'PASV'
    else begin
        if FControlSocket.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Msg := Format('PORT 127,0,0,1,%d,%d',
                          [HiByte(DataPort),
                           LoByte(DataPort)])
        else
            Msg := Format('PORT %d,%d,%d,%d,%d,%d',
                          [(IPAddr.S_addr shr  0) and 255,
                           (IPAddr.S_addr shr  8) and 255,
                           (IPAddr.S_addr shr 16) and 255,
                           (IPAddr.S_addr shr 24) and 255,
                           HiByte(DataPort),
                           LoByte(DataPort)]);
    end;

    FByteCount := 0;
    FFctPrv    := ftpFctPort;
    ExecAsync(ftpPortAsync, Msg, [200, 227], nil);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketDnsLookupDone(
    Sender  : TObject;
    ErrCode : Word);
begin
    if ErrCode <> 0 then begin
        FLastResponse  := '500 ' + WSocketErrorDesc(ErrCode) +
                          ' (Winsock error #' + IntToStr(ErrCode) + ')';
        FStatusCode    := 500;
        FRequestResult :=  FStatusCode;    { 21/05/99 }
        SetErrorMessage;
        TriggerRequestDone(ErrCode);
    end
    else begin
        FDnsResult               := FControlSocket.DnsResult;
        FControlSocket.Addr      := FDnsResult;
        FControlSocket.LocalAddr := FLocalAddr; {bb}
        FControlSocket.Proto     := 'tcp';
        if (FConnectionType = ftpProxy) and (FProxyPort <> '') then
            FControlSocket.Port  := FProxyPort
        else
            FControlSocket.Port  := FPort;
{       FControlSocket.OnDisplay := FOnDisplay; } { Debugging only }
        StateChange(ftpWaitingBanner);
        try
            FControlSocket.Connect;
        except
            on E:Exception do begin
                FLastResponse := '500 ' + E.ClassName + ': ' + E.Message;
                FStatusCode   := 500;
                FRequestResult :=  FStatusCode;    { 21/05/99 }
                SetErrorMessage;
                TriggerRequestDone(FStatusCode);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketSessionConnected(Sender: TObject; ErrCode: Word);
begin
    { Do not trigger the client SessionConnected from here. We must wait }
    { to have received the server banner.                                }
    if ErrCode <> 0 then begin
        FLastResponse  := '500 ' + WSocketErrorDesc(ErrCode) +
                          ' (Winsock error #' + IntToStr(ErrCode) + ')';
        FStatusCode    := 500;
        FRequestResult := FStatusCode;  { Heedong Lim, 05/14/1999 }
        SetErrorMessage; { Heedong Lim, 05/14/1999 }
        FNextRequest   := nil;
        TriggerRequestDone(ErrCode);
        FControlSocket.Close;
        StateChange(ftpReady);
    end
    else
        FConnected := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Len      : Integer;
    I, J     : Integer;
    nIndex   : Integer;
    LocalBuf : TBytes;
begin
    if FReceiveLen = 0 then
        Len := FControlSocket.Receive(FReceiveBuffer,
                                      Length(FReceiveBuffer))
    else begin
        SetLength(LocalBuf, Length(FReceiveBuffer));
        Len := FControlSocket.Receive(LocalBuf,
                                      Length(LocalBuf) - FReceiveLen);
        for I := 0 to Len - 1 do
            FReceiveBuffer[FReceiveLen + I] := LocalBuf[I];
    end;

    if FRequestType = ftpRqAbort then
        Exit;

    if Len = 0 then begin
        { Remote has closed. We will soon receive FD_CLOSE (OnSessionClosed) }
        { FControlSocket.Close;                                              }
        Exit;
    end;
    if Len < 0 then
        Exit;

    FReceiveLen := FReceiveLen + Len;

    while FReceiveLen > 0 do begin
//WSocketTriggerDebugEvent(Self, 'FReceiveLen    = ' + IntToStr(FReceiveLen));
//WSocketTriggerDebugEvent(Self, 'FReceiveBuffer = ' + DataToString(FReceiveBuffer, FReceiveLen));
        if ftpAcceptLF in FOptions then begin
            I := 0;
            while (I < FReceiveLen) and (FReceiveBuffer[I] <> 10) do
                Inc(I);
            J := I;
        end
        else begin
            I := 0;
            while (I < (FReceiveLen - 1)) and
                  (FReceiveBuffer[I] <> 13) and
                  (FReceiveBuffer[I + 1] <> 10) do
                Inc(I);
            J := I + 1;
        end;
        if I >= FReceiveLen then   // Not found
            break;

//WSocketTriggerDebugEvent(Self, 'I = ' + IntToStr(I) + '  J = ' + IntToStr(J));

        FLastResponse := Copy(FReceiveBuffer, 0, I - 1);
//WSocketTriggerDebugEvent(Self, FLastResponse);
        { Remove trailing control chars }
        while (Length(FLastResponse) > 0) and
              ((FLastResponse[Length(FLastResponse) - 1] = #10) or
               (FLastResponse[Length(FLastResponse) - 1] = #13)) do
             SetLength(FLastResponse, Length(FLastResponse) - 1);

        if Assigned(FOnResponse) then
            FOnresponse(Self);

        FReceiveLen := FReceiveLen - J - 1;
        if FReceiveLen > 0 then begin
            for I := 0 to FReceiveLen - 1 do
                FReceiveBuffer[I] := FReceiveBuffer[I + J + 1];
//WSocketTriggerDebugEvent(Self, 'FReceiveBuffer = ' + DataToString(FReceiveBuffer, FReceiveLen));
        end
        else if FReceiveLen < 0 then
            FReceiveLen := 0;

        if FState = ftpWaitingBanner then begin
            DisplayLastResponse;
            if (FLastResponse = '') or                       { 15/02/03 }
               (FLastResponse[1] < '0') or
               (FLastResponse[1] > '9') then
                Continue;  { Continuation line, ignore }
            nIndex := 1;
            GetInteger(FLastResponse, nIndex, FStatusCode);
            if FLastResponse[nIndex] = '-' then
                Continue;  { Continuation line, ignore }
            if FStatusCode <> 220 then begin
                SetErrorMessage;
                FRequestResult := FStatusCode;
                FControlSocket.Close;
                Exit;
            end;

            StateChange(ftpConnected);
            if Assigned(FOnSessionConnected) then
                FOnSessionConnected(Self, ErrCode);

            if Assigned(FWhenConnected) then
                FWhenConnected
            else begin
                TriggerRequestDone(0);
            end;
        end
        else if FState = ftpWaitingResponse then begin
            if (FLastResponse = '') or                            { 15/02/03 }
               (FLastResponse[1] < '0') or
               (FLastResponse[1] > '9') then begin  { 22/11/99 }
                DisplayLastResponse; { 02/11/01 }
                Continue;  { Continuation line, ignore }
            end;
            nIndex := 1;
            GetInteger(FLastResponse, nIndex, FStatusCode);
            if FLastResponse[nIndex] = '-' then begin
                DisplayLastResponse; { 02/11/01 }
                Continue;  { Continuation line, ignore }
            end;
            if Assigned(FNext) then
                FNext
            else begin
                HandleError('Program error: FNext is nil');
                Exit;
            end;
        end
        else { Unexpected data received }
            DisplayLastResponse;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.ControlSocketSessionClosed(
    Sender  : TObject;
    ErrCode : Word);
begin
    if FConnected then begin
        FConnected := FALSE;
        if FState <> ftpAbort then
            StateChange(ftpNotConnected);
        if Assigned(FOnSessionClosed) then
            FOnSessionClosed(Self, ErrCode);
    end;
    if FState <> ftpAbort then
        StateChange(ftpInternalReady);
    if not (FRequestType in [ftpRqAbort]) then begin
        if ErrCode <> 0 then begin
            FLastResponse  := '500 Control connection closed. ' +
                              WSocketErrorDesc(ErrCode) +
                              ' (Winsock error #' + IntToStr(ErrCode) + ')';
            FStatusCode    := 500;
            FRequestResult :=  FStatusCode;    { 06 apr 2002 }
            SetErrorMessage;
        end;
        TriggerRequestDone(FRequestResult);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerRequestDone(ErrCode: Word);
begin
    if not FRequestDoneFlag then begin
        FRequestDoneFlag := TRUE;
        if (ErrCode = 0) and Assigned(FNextRequest) then begin
            if (FState <> ftpAbort)
               and (FState <> ftpPasvReady) { 19.09.2002 }
              { and     28/06/2002
               not ((ftpFctPut in FFctSet) and (FPassive = TRUE))} then
                StateChange(ftpInternalReady);
            FNextRequest;
        end
        else begin
            StateChange(ftpReady);
            if FDataSocket.State <> wsClosed then
                FDataSocket.Close;
            { Restore the lastresponse saved before quit command }
            if FHighLevelFlag and (FStatusCodeSave >= 0) then begin
                 FLastResponse := FLastResponseSave;
                 FStatusCode   := FStatusCodeSave;
            end;
            FHighLevelFlag := FALSE;
            FNextRequest   := nil;
            PostMessage(Handle, WM_FTP_REQUEST_DONE, 0, ErrCode);
            { if Assigned(FOnRequestDone) then
                FOnRequestDone(Self, FRequestType, ErrCode); }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomFtpCli.TriggerReadyToTransmit(var bCancel : Boolean);
begin
    if Assigned(FOnReadyToTransmit) then
        FOnReadyToTransmit(Self, bCancel);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomFtpCli.GetConnected : Boolean;
begin
    Result := FControlSocket.State <> wsClosed;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* *                                                                     * *}
{* *                              TFtpClient                             * *}
{* *                                                                     * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpClient.Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent
                                                     {$ELSE}TObject{$ENDIF});
begin
    inherited Create(AOwner);
    FTimeout := 15;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Open : Boolean;
begin
    Result := Synchronize(OpenAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.User : Boolean;
begin
    Result := Synchronize(UserAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Pass : Boolean;
begin
    Result := Synchronize(PassAsync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Connect : Boolean;
begin
    Result := Synchronize(ConnectASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Cwd : Boolean;
begin
    Result := Synchronize(CwdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Pwd : Boolean;
begin
    Result := Synchronize(PwdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.CDup : Boolean;
begin
    Result := Synchronize(CDupASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeSet : Boolean;
begin
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeBinary : Boolean;
begin
    Binary := TRUE;
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.TypeAscii : Boolean;
begin
    Binary := FALSE;
    Result := Synchronize(TypeSetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Get : Boolean;
begin
    Result := Synchronize(GetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Put : Boolean;
begin
    Result := Synchronize(PutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ By A.Burlakov: new function for resuming uploads                          }
{ Uses REST + STOR commands instead APPEND                                  }
function TFtpClient.RestPut : Boolean;
begin
    Result := Synchronize(RestPutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestartPut : Boolean;
begin
    Result := Synchronize(RestartPutASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Append : Boolean;
begin
    Result := Synchronize(AppendASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Quit : Boolean;
begin
    Result := Synchronize(QuitASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Abort : Boolean;
begin
    Result := Synchronize(AbortASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Receive : Boolean;
begin
    Result := Synchronize(ReceiveASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Transmit : Boolean;
begin
    Result := Synchronize(TransmitASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.AppendFile : Boolean;
begin
    Result := Synchronize(AppendFileASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Dir : Boolean;
begin
    Result := Synchronize(DirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Directory : Boolean;
begin
    Result := Synchronize(DirectoryASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Ls : Boolean;
begin
    Result := Synchronize(LsASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.List : Boolean;
begin
    Result := Synchronize(ListASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mkd : Boolean;
begin
    Result := Synchronize(MkdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mkdir : Boolean;
begin
    Result := Synchronize(MkdirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Ren : Boolean;
begin
    Result := Synchronize(RenASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rename : Boolean;
begin
    Result := Synchronize(RenameASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Dele : Boolean;
begin
    Result := Synchronize(DeleASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Delete : Boolean;
begin
    Result := Synchronize(DeleteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rmd : Boolean;
begin
    Result := Synchronize(RmdASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Rmdir : Boolean;
begin
    Result := Synchronize(RmdirASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Syst : Boolean;
begin
    Result := Synchronize(SystASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.System : Boolean;
begin
    Result := Synchronize(SystemASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Size : Boolean;
begin
    Result := Synchronize(SizeASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.FileSize : Boolean;
begin
    Result := Synchronize(FileSizeASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.AbortXfer: Boolean;
begin
    Result := Synchronize(AbortXferASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Quote : Boolean;
begin
    Result := Synchronize(QuoteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.DoQuote : Boolean;
begin
    Result := Synchronize(DoQuoteASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestGet : Boolean;
begin
    Result := Synchronize(RestGetASync);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.RestartGet : Boolean;
begin
    Result := Synchronize(RestartGetASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mlsd       : Boolean;    { V2.90 machine list directory }
begin
    Result := Synchronize(MlsdASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mlst       : Boolean;    { V2.90 machine list file      }
begin
    Result := Synchronize(MlstASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Feat       : Boolean;    { V2.90 supported extensions   }
begin
    Result := Synchronize(FeatASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mdtm       : Boolean;    { V2.90 get file modification time }
begin
    Result := Synchronize(MdtmASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Mdtmyy     : Boolean;    { V2.90 set file modification time }
begin
    Result := Synchronize(MdtmyyASync);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Progress : Boolean;
begin
    Result := inherited Progress;
    { Evaluate the timeout period again }
    if FTimeout > 0 then
        FTimeStop := LongInt(GetTickCount) + LongInt(FTimeout) * 1000;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
procedure TFtpClient.MessagePump;
begin
{$IFDEF VER80}
    Application.ProcessMessages;
{$ELSE}
    if FMultiThreaded then
        FControlSocket.ProcessMessages
    else
{$IFDEF NOFORMS}
        { The Forms unit (TApplication object) has not been included.           }
        { We used either an external message pump or our internal message pump. }
        { External message pump has to set Terminated property to TRUE when the }
        { application is terminated.                                            }
        if Assigned(FOnMessagePump) then
            FOnMessagePump(Self)
        else
            FControlSocket.ProcessMessages;
{$ELSE}
        Application.ProcessMessages;
{$ENDIF}
{$ENDIF}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.WaitUntilReady : Boolean;
begin
    Result    := TRUE;           { Assume success }
    FTimeStop := LongInt(GetTickCount) + LongInt(FTimeout) * 1000;
    while TRUE do begin
        if FState in [ftpReady, ftpInternalReady] then begin
            { Back to ready state, the command is finished }
            Result := (FRequestResult = 0);
            break;
        end;

        if {$IFNDEF NOFORMS} Application.Terminated or
           {$ELSE}           Terminated or
           {$ENDIF}
           ((FTimeout > 0) and (LongInt(GetTickCount) > FTimeStop)) then begin
            { Timeout occured }
            AbortAsync;
            FErrorMessage := '426 Timeout';
            FStatusCode   := 426;
            Result        := FALSE; { Command failed }
            break;
        end;
        MessagePump;
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        {$IFNDEF VER80}
        Sleep(0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpClient.Synchronize(Proc : TFtpNextProc) : Boolean;
begin
    try
        Proc;
        Result := WaitUntilReady;
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

