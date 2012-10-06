{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     November 23, 1997 (Win32 version)
Version:      1.80
Description:  THttpCli is an implementation for the HTTP protocol
              RFC 1945 (V1.0), and some of RFC 2068 (V1.1)
Credit:       This component was based on a freeware from by Andreas
              Hoerstemeier and used with his permission.
              andy@hoerstemeier.de http://www.hoerstemeier.com/index.htm
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2005 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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
unit Overbyte.Ics.HttpClient platform;

interface

uses
    System.IO,
    System.Threading,
    Borland.Vcl.Windows,
    Borland.Vcl.Messages,
    Borland.Vcl.Classes,
    Borland.Vcl.SysUtils,
    Borland.Vcl.WinUtils,
    Overbyte.Ics.Component,
    OverByte.Ics.WinSock,
    OverByte.Ics.WSocket;

const
    HttpCliVersion     = 154;
    CopyRight : String = ' THttpCli (c) 1997-2005 F. Piette V1.54 ';
    DefaultProxyPort   = '80';
{$IFDEF VER80}
    { Delphi 1 has a 255 characters string limitation }
    HTTP_RCV_BUF_SIZE = 255;
    HTTP_SND_BUF_SIZE = 8193;
{$ELSE}
    HTTP_RCV_BUF_SIZE = 8193;
    HTTP_SND_BUF_SIZE = 8193;
{$ENDIF}
    WM_HTTP_REQUEST_DONE = WM_USER + 1;
    WM_HTTP_SET_READY    = WM_USER + 2;
    WM_HTTP_LOGIN        = WM_USER + 3;
    httperrNoError  = 0;
    httperrBusy     = 1;
    httperrNoData   = 2;
    httperrAborted  = 3;
    httperrOverflow = 4;
    httperrVersion  = 5;
    httperrWindow   = 6;

type
    EHttpException = class(Exception)
        ErrorCode : Word;
        constructor Create(const Msg : String; ErrCode : Word);
    end;

    THttpEncoding    = (encUUEncode, encBase64, encMime);
    THttpRequest     = (httpABORT, httpGET, httpPOST, httpPUT,
                        httpHEAD,  httpCLOSE);
    THttpState       = (httpReady,         httpNotConnected, httpConnected,
                        httpDnsLookup,     httpDnsLookupDone,
                        httpWaitingHeader, httpWaitingBody,  httpBodyReceived,
                        httpWaitingProxyConnect,
                        httpClosing,       httpAborting);

    TOnCommand       = procedure (Sender : TObject;
                                  var S: String) of object;
    TDocDataEvent    = procedure (Sender : TObject;
                                  const Buffer : TBytes;
                                  Offset       : Integer;
                                  Len          : Integer) of object;
    TCookieRcvdEvent = procedure (Sender       : TObject;
                                  const Data   : String;
                                  var   Accept : Boolean) of object;
    THttpRequestDone = procedure (Sender  : TObject;
                                  RqType  : THttpRequest;
                                  ErrCode : Word) of object;
    TBeforeHeaderSendEvent = procedure (Sender       : TObject;
                                        const Method : String;
                                        Headers      : TStrings) of object;

type
    THttpClient = class(TIcsComponent)
    protected
        FCtrlSocket           : TWSocket;
        FMultiThreaded        : Boolean;
        FState                : THttpState;
        FLocalAddr            : String;
        FHostName             : String;
        FTargetHost           : String;
        FTargetPort           : String;
        FPort                 : String;
        FProtocol             : String;
        FProxy                : String;
        FProxyPort            : String;
        FUsername             : String;
        FPassword             : String;
        FProxyUsername        : String;
        FProxyPassword        : String;
        FProxyConnected       : Boolean;
        FLocation             : String;
        FCurrentHost          : String;
        FCurrentPort          : String;
        FCurrentProtocol      : String;
        FConnected            : Boolean;
        FDnsResult            : String;
        FSendBuffer           : TBytes;
        FRequestType          : THttpRequest;
        FReceiveBuffer        : TBytes;
        FReceiveLen           : Integer;
        FLastResponse         : String;
        FHeaderLineCount      : Integer;
        FBodyLineCount        : Integer;
        FAllowedToSend        : Boolean;
        FURL                  : String;
        FPath                 : String;
        FDocName              : String;
        FSender               : String;
        FReference            : String;
        FConnection           : String;         { for Keep-alive }
        FAgent                : String;
        FAccept               : String;
        FAcceptLanguage       : String;
        FModifiedSince        : TDateTime;      { Warning ! Use GMT date/Time }
        FNoCache              : Boolean;
        FStatusCode           : Integer;
        FReasonPhrase         : String;
        FResponseVer          : String;
        FRequestVer           : String;
        FContentLength        : LongInt;
        FContentType          : String;
        FDoAuthor             : TStringList;
        FContentPost          : String;         { Also used for PUT }
        FContentRangeBegin    : String;
        FContentRangeEnd      : String;
        FAcceptRanges         : String;
        FCookie               : String;
        FLocationFlag         : Boolean;
        FHeaderEndFlag        : Boolean;
        FRcvdHeader           : TStrings;
        FRcvdStream           : TStream; { If assigned, will recv the answer }
        FRcvdCount            : LongInt; { Number of rcvd bytes for the body }
        FSentCount            : LongInt;
        FSendStream           : TStream; { Contains the data to send         }
        FReqStream            : TMemoryStream;
        FRequestDoneError     : Integer;
        FNext                 : procedure of object;
        FBodyData             : Integer;
        FBodyDataLen          : Integer;
        FOnStateChange        : TNotifyEvent;
        FOnSessionConnected   : TNotifyEvent;
        FOnRequestHeaderBegin : TNotifyEvent;
        FOnRequestHeaderEnd   : TNotifyEvent;
        FOnHeaderBegin        : TNotifyEvent;
        FOnHeaderEnd          : TNotifyEvent;
        FOnHeaderData         : TNotifyEvent;
        FOnDocBegin           : TNotifyEvent;
        FOnDocEnd             : TNotifyEvent;
        FOnDocData            : TDocDataEvent;
        FOnSendBegin          : TNotifyEvent;
        FOnSendEnd            : TNotifyEvent;
        FOnSendData           : TDocDataEvent;
        FOnTrace              : TNotifyEvent;
        FOnCommand            : TOnCommand;
        FOnCookie             : TCookieRcvdEvent;
        FOnDataAvailable      : TDataAvailable;
        FOnRequestDone        : THttpRequestDone;
        FOnLocationChange     : TNotifyEvent;
        { Added by Eugene Mayevski }
        FOnSocksConnected     : TSessionConnected;
        FOnSocksAuthState     : TSocksAuthStateEvent;
        FOnSocksError         : TSocksErrorEvent;
        FOnSocketError        : TNotifyEvent;
        FOnBeforeHeaderSend   : TBeforeHeaderSendEvent;     { Wilfried 9 sep 02}
        procedure   WndProc(var MsgRec: TMessage); override;
        procedure CreateSocket; virtual;
        procedure DoBeforeConnect; virtual;
        procedure DoSocksConnected(Sender: TObject; ErrCode: Word);
        procedure DoSocksAuthState(Sender : TObject; AuthState : TSocksAuthState);
        procedure DoSocksError(Sender : TObject; ErrCode : Integer; Msg : String);
        procedure SocketErrorTransfer(Sender : TObject);
        procedure SetSocksServer(value : String);
        procedure SetSocksLevel(value : String);
        procedure SetSocksPort(value : String);
        procedure SetSocksUsercode(value : String);
        procedure SetSocksPassword(value : String);
        procedure SetSocksAuthentication(value : TSocksAuthentication);
        function  GetSocksServer : String;
        function  GetSocksLevel : String;
        function  GetSocksPort : String;
        function  GetSocksUsercode : String;
        function  GetSocksPassword : String;
        function  GetSocksAuthentication : TSocksAuthentication;
        { Mayevski additions end }
        procedure SendRequest(const method, Version: String);
        procedure GetHeaderLineNext; virtual;
        procedure GetBodyLineNext; virtual;
        procedure SendCommand(const Cmd : String); virtual;
        procedure Login; virtual;
        procedure Logout; virtual;
        procedure InternalClear; virtual;
        procedure SocketDNSLookupDone(Sender: TObject; ErrCode: Word); virtual;
        procedure SocketSessionClosed(Sender: TObject; ErrCode: Word); virtual;
        procedure SocketSessionConnected(Sender : TObject; ErrCode : Word); virtual;
        procedure SocketDataSent(Sender : TObject; ErrCode : Word); virtual;
        procedure SocketDataAvailable(Sender: TObject; ErrCode: Word); virtual;
        procedure LocationSessionClosed(Sender: TObject; ErrCode: Word); virtual;
        procedure DoRequestAsync(Rq : THttpRequest); virtual;
        procedure DoRequestSync(Rq : THttpRequest); virtual;
        procedure SetMultiThreaded(newValue : Boolean); virtual;
        procedure StateChange(NewState : THttpState); virtual;
        procedure TriggerStateChange; virtual;
        procedure TriggerCookie(const Data : String;
                                var   bAccept : Boolean); virtual;
        procedure TriggerSessionConnected; virtual;
        procedure TriggerBeforeHeaderSend(const Method : String;
                                          Headers : TStrings); virtual;
        procedure TriggerRequestHeaderBegin; virtual;
        procedure TriggerRequestHeaderEnd; virtual;
        procedure TriggerHeaderBegin; virtual;
        procedure TriggerHeaderEnd; virtual;
        procedure TriggerDocBegin; virtual;
        procedure TriggerDocData(Data : TBytes; Offset, Len : Integer); virtual;
        procedure TriggerDocEnd; virtual;
        procedure TriggerSendBegin; virtual;
        procedure TriggerSendData(var Data : TBytes; Offset, Len : Integer); virtual;
        procedure TriggerSendEnd; virtual;
        procedure TriggerRequestDone; virtual;
        procedure SetReady; virtual;
        procedure AdjustDocName; virtual;
        procedure SetRequestVer(const Ver : String);
        procedure WMHttpRequestDone(var msg: TMessage);
                  message WM_HTTP_REQUEST_DONE;
        procedure WMHttpSetReady(var msg: TMessage);
                  message WM_HTTP_SET_READY;
        procedure WMHttpLogin(var msg: TMessage);
                  message WM_HTTP_LOGIN;
        procedure   AbortComponent; override;
{$IFDEF USE_SSL}
        procedure SslHandshakeDone(Sender : TObject; ErrCode : Word);
{$ENDIF}
    public
        constructor Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent
                                   {$ELSE}TObject{$ENDIF}); override;
        destructor  Destroy; override;
        procedure   Get;        { Synchronous blocking Get        }
        procedure   Post;       { Synchronous blocking Post       }
        procedure   Put;        { Synchronous blocking Put        }
        procedure   Head;       { Synchronous blocking Head       }
        procedure   Close;      { Synchronous blocking Close      }
        procedure   GetASync;   { Asynchronous, non-blocking Get  }
        procedure   PostASync;  { Asynchronous, non-blocking Post }
        procedure   PutASync;   { Asynchronous, non-blocking Put  }
        procedure   HeadASync;  { Asynchronous, non-blocking Head }
        procedure   CloseAsync; { Synchronous blocking Close      }
        procedure   Abort;

        property CtrlSocket      : TWSocket          read  FCtrlSocket;
        property State           : THttpState        read  FState;
        property LastResponse    : String            read  FLastResponse;
        property ContentLength   : LongInt           read  FContentLength;
        property ContentType     : String            read  FContentType;
        property RcvdCount       : LongInt           read  FRcvdCount;
        property SentCount       : LongInt           read  FSentCount;
        property StatusCode      : Integer           read  FStatusCode;
        property ReasonPhrase    : String            read  FReasonPhrase;
        property DnsResult       : String            read  FDnsResult;
        property AuthorizationRequest : TStringList  read  FDoAuthor;
        property DocName              : String       read  FDocName;
        property Location             : String       read  FLocation
                                                     write FLocation;
        property RcvdStream           : TStream      read  FRcvdStream
                                                     write FRcvdStream;
        property SendStream           : TStream      read  FSendStream
                                                     write FSendStream;
        property RcvdHeader           : TStrings     read  FRcvdHeader;
        property Hostname             : String       read  FHostname;
        property Protocol             : String       read  FProtocol;
    published
        property URL             : String            read  FURL
                                                     write FURL;
        property LocalAddr       : String            read  FLocalAddr   {bb}
                                                     write FLocalAddr;  {bb}
        property Proxy           : String            read  FProxy
                                                     write FProxy;
        property ProxyPort       : String            read  FProxyPort
                                                     write FProxyPort;
        property Sender          : String            read  FSender
                                                     write FSender;
        property Agent           : String            read  FAgent
                                                     write FAgent;
        property Accept          : String            read  FAccept
                                                     write FAccept;
        property AcceptLanguage  : String            read  FAcceptLanguage
                                                     write FAcceptLanguage;
        property Reference       : String            read  FReference
                                                     write FReference;
        property Connection      : String            read  FConnection
                                                     write FConnection;
        property Username        : String            read  FUsername
                                                     write FUsername;
        property Password        : String            read  FPassword
                                                     write FPassword;
        property ProxyUsername   : String            read  FProxyUsername
                                                     write FProxyUsername;
        property ProxyPassword   : String            read  FProxyPassword
                                                     write FProxyPassword;
        property NoCache         : Boolean           read  FNoCache
                                                     write FNoCache;
        property ModifiedSince   : TDateTime         read  FModifiedSince
                                                     write FModifiedSince;
        property Cookie          : String            read  FCookie
                                                     write FCookie;
        property ContentTypePost : String            read  FContentPost
                                                     write FContentPost;
        property ContentRangeBegin: String           read  FContentRangeBegin  {JMR!! Added this line!!!}
                                                     write FContentRangeBegin; {JMR!! Added this line!!!}
        property ContentRangeEnd  : String           read  FContentRangeEnd    {JMR!! Added this line!!!}
                                                     write FContentRangeEnd;   {JMR!! Added this line!!!}
        property AcceptRanges     : String           read  FAcceptRanges;
        property MultiThreaded    : Boolean          read  FMultiThreaded
                                                     write SetMultiThreaded;
        property RequestVer       : String           read  FRequestVer
                                                     write SetRequestVer;
        property OnTrace            : TNotifyEvent   read  FOnTrace
                                                     write FOnTrace;
        property OnSessionConnected : TNotifyEvent   read  FOnSessionConnected
                                                     write FOnSessionConnected;
        property OnHeaderData       : TNotifyEvent   read  FOnHeaderData
                                                     write FOnHeaderData;
        property OnCommand          : TOnCommand     read  FOnCommand
                                                     write FOnCommand;
        property OnHeaderBegin      : TNotifyEvent   read  FOnHeaderBegin
                                                     write FOnHeaderBegin;
        property OnHeaderEnd        : TNotifyEvent   read  FOnHeaderEnd
                                                     write FOnHeaderEnd;
        property OnRequestHeaderBegin : TNotifyEvent read  FOnRequestHeaderBegin
                                                     write FOnRequestHeaderBegin;
        property OnRequestHeaderEnd   : TNotifyEvent read  FOnRequestHeaderEnd
                                                     write FOnRequestHeaderEnd;
        property OnDocBegin      : TNotifyEvent      read  FOnDocBegin
                                                     write FOnDocBegin;
        property OnDocData       : TDocDataEvent     read  FOnDocData
                                                     write FOnDocData;
        property OnDocEnd        : TNotifyEvent      read  FOnDocEnd
                                                     write FOnDocEnd;
        property OnSendBegin     : TNotifyEvent      read  FOnSendBegin
                                                     write FOnSendBegin;
        property OnSendData      : TDocDataEvent     read  FOnSendData
                                                     write FOnSendData;
        property OnSendEnd       : TNotifyEvent      read  FOnSendEnd
                                                     write FOnSendEnd;
        property OnStateChange   : TNotifyEvent      read  FOnStateChange
                                                     write FOnStateChange;
        property OnRequestDone   : THttpRequestDone  read  FOnRequestDone
                                                     write FOnRequestDone;
        property OnLocationChange : TNotifyEvent     read  FOnLocationChange
                                                     write FOnLocationChange;
        property OnCookie         : TCookieRcvdEvent read  FOnCookie
                                                     write FOnCookie;
        property SocksServer     : String            read  GetSocksServer
                                                     write SetSocksServer;
        property SocksLevel      : String            read  GetSocksLevel
                                                     write SetSocksLevel;
        property SocksPort       : String            read  GetSocksPort
                                                     write SetSocksPort;
        property SocksUsercode   : String            read  GetSocksUsercode
                                                     write SetSocksUsercode;
        property SocksPassword   : String            read  GetSocksPassword
                                                     write SetSocksPassword;
        property SocksAuthentication : TSocksAuthentication read GetSocksAuthentication
                                                            write SetSocksAuthentication;
        property OnSocksConnected    : TSessionConnected    read FOnSocksConnected
                                                            write FOnSocksConnected;
        property OnSocksAuthState    : TSocksAuthStateEvent read FOnSocksAuthState
                                                            write FOnSocksAuthState;
        property OnSocksError        : TSocksErrorEvent     read FOnSocksError
                                                            write FOnSocksError;
        property OnSocketError       : TNotifyEvent         read FOnSocketError
                                                            write FOnSocketError;
        property OnBeforeHeaderSend  : TBeforeHeaderSendEvent read  FOnBeforeHeaderSend
                                                              write FOnBeforeHeaderSend;
  end;

{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path }
procedure ParseURL(const URL : String;
                   var Proto, User, Pass, Host, Port, Path : String);
function  Posn(const s, t : String; count : Integer) : Integer;
procedure ReplaceExt(var FName : String; const newExt : String);
function  EncodeLine(Encoding : THttpEncoding;
                     const SrcData : String;
                     Size : Integer):String;
function  EncodeStr(Encoding : THttpEncoding; const Value : String) : String;
function  RFC1123_Date(aDate : TDateTime) : String;
function  UrlEncode(S : String) : String;
function  UrlDecode(S : String) : String;

implementation

const
    bin2uue  : String = '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
    bin2b64  : String = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
    uue2bin  : String = ' !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_ ';
    b642bin  : String = '~~~~~~~~~~~^~~~_TUVWXYZ[\]~~~|~~~ !"#$%&''()*+,-./0123456789~~~~~~:;<=>?@ABCDEFGHIJKLMNOPQRS';
    linesize = 45;
//type
//    TCharSet = set of Char;
const
    UriProtocolSchemeAllowedChars = '0123456789abcdefghijklmnopqrstuvwxyz+-.';

function GetBaseUrl(const Url : String) : String; forward;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure OutputDebugString(const Msg : String);
begin
{$IFDEF DEBUG_OUTPUT}
    if IsConsole then
        WriteLn(Msg);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor EHttpException.Create(const Msg : String; ErrCode : Word);
begin
    inherited Create(Msg);
    ErrorCode := ErrCode;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We cannot use Delphi own function because the date must be specified in   }
{ english and Delphi use the current language.                              }
function RFC1123_Date(aDate : TDateTime) : String;
const
   StrWeekDay : String = 'MonTueWedThuFriSatSun';
   StrMonth   : String = 'JanFebMarAprMayJunJulAugSepOctNovDec';
var
   Year, Month, Day       : Word;
   Hour, Min,   Sec, MSec : Word;
   DayOfWeek              : Word;
begin
   DecodeDate(aDate, Year, Month, Day);
   DecodeTime(aDate, Hour, Min,   Sec, MSec);
   DayOfWeek := aDate.DayOfWeek; //((Trunc(aDate) - 2) mod 7);
   Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
             Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                    [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
                     Year, Hour, Min, Sec]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpClient.Create(
    AOwner: {$IFDEF ICS_COMPONENT}TComponent
            {$ELSE}TObject{$ENDIF});
begin
    inherited Create(AOwner);
    FProxyPort                     := DefaultProxyPort;
    FRequestVer                    := '1.0';
    FContentPost                   := 'application/x-www-form-urlencoded';
    FAccept                        := 'image/gif, image/x-xbitmap, ' +
                                      'image/jpeg, image/pjpeg, */*';
    FAgent                         := 'Mozilla/4.0 (compatible; ICS)';
    FDoAuthor                      := TStringlist.Create;
    FRcvdHeader                    := TStringList.Create;
    FReqStream                     := TMemoryStream.Create;
    FState                         := httpReady;
    FLocalAddr                     := '0.0.0.0';
    CreateSocket;
    FCtrlSocket.OnSessionClosed    := SocketSessionClosed;
    FCtrlSocket.OnDataAvailable    := SocketDataAvailable;
    FCtrlSocket.OnSessionConnected := SocketSessionConnected;
    FCtrlSocket.OnDataSent         := SocketDataSent;
    FCtrlSocket.OnDnsLookupDone    := SocketDNSLookupDone;
    FCtrlSocket.OnSocksError       := DoSocksError;
    FCtrlSocket.OnSocksConnected   := DoSocksConnected;
    FCtrlSocket.OnError            := SocketErrorTransfer;
    SetLength(FReceiveBuffer, HTTP_RCV_BUF_SIZE);
    SetLength(FSendBuffer, HTTP_SND_BUF_SIZE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpClient.Destroy;
begin
    FDoAuthor.Free;
    FCtrlSocket.Free;
    FRcvdHeader.Free;
    FReqStream.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.CreateSocket;
begin
    FCtrlSocket := TWSocket.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.WndProc(var MsgRec: TMessage);
begin
    try
         with MsgRec do begin
             case Msg of
             WM_HTTP_REQUEST_DONE : WMHttpRequestDone(MsgRec);
             WM_HTTP_SET_READY    : WMHttpSetReady(MsgRec);
             WM_HTTP_LOGIN        : WMHttpLogin(MsgRec);
             else
                 Result := DefWindowProc(Handle, Msg, wParam, lParam);
             end;
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoSocksConnected(Sender: TObject; ErrCode: Word);
begin
    if Assigned(FOnSocksConnected) then
        FOnSocksConnected(Sender, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketErrorTransfer(Sender : TObject);
begin
    if (assigned(FOnSocketError)) then
        FOnSocketError(Self);  { Substitute Self for subcomponent's Sender. }
end;  { SocketErrorTransfer }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoSocksAuthState(
    Sender    : TObject;
    AuthState : TSocksAuthState);
begin
    if Assigned(FOnSocksAuthState) then
        FOnSocksAuthState(Sender, AuthState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoSocksError(
    Sender  : TObject;
    ErrCode : Integer;
    Msg     : String);
begin
    if Assigned(FOnSocksError) then
        FOnSocksError(Sender, ErrCode, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetMultiThreaded(newValue : Boolean);
begin
    FMultiThreaded            := newValue;
    FCtrlSocket.MultiThreaded := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetReady;
begin
    PostMessage(Handle, WM_HTTP_SET_READY, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.StateChange(NewState : THttpState);
begin
    if FState <> NewState then begin
        FState := NewState;
        TriggerStateChange;
        if NewState = httpReady then
            TriggerRequestDone;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerStateChange;
begin
    if Assigned(FOnStateChange) then
        FOnStateChange(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerCookie(const Data : String; var bAccept : Boolean);
begin
    if Assigned(FOnCookie) then
        FOnCookie(Self, Data, bAccept);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerSessionConnected;
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerDocBegin;
begin
    OutputDebugString('DocBegin');
    if Assigned(FOnDocBegin) then
        FOnDocBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerDocEnd;
begin
    OutputDebugString('DocEnd');
    if Assigned(FOnDocEnd) then
        FOnDocEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerDocData(Data : TBytes; Offset, Len : Integer);
begin
    if Assigned(FOnDocData) then
        FOnDocData(Self, Data, Offset, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerSendBegin;
begin
    if Assigned(FOnSendBegin) then
        FOnSendBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerSendEnd;
begin
    if Assigned(FOnSendEnd) then
        FOnSendEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerSendData(var Data : TBytes; Offset, Len : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, Data, Offset, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerHeaderBegin;
begin
    FHeaderEndFlag := TRUE;
    if Assigned(FOnHeaderBegin) then
        FOnHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerHeaderEnd;
begin
    FHeaderEndFlag := FALSE;
    if Assigned(FOnHeaderEnd) then
        FOnHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerBeforeHeaderSend(
    const Method : String;
    Headers      : TStrings);
begin
    if Assigned(FOnBeforeHeaderSend) then
        FOnBeforeHeaderSend(Self, Method, Headers);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerRequestHeaderBegin;
begin
    if Assigned(FOnRequestHeaderBegin) then
        FOnRequestHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerRequestHeaderEnd;
begin
    if Assigned(FOnRequestHeaderEnd) then
        FOnRequestHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.TriggerRequestDone;
begin
    PostMessage(Handle, WM_HTTP_REQUEST_DONE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.WMHttpRequestDone(var msg: TMessage);
begin
    OutputDebugString('RequestDone');
    if Assigned(FOnRequestDone) then
        FOnRequestDone(Self, FRequestType, FRequestDoneError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.WMHttpSetReady(var msg: TMessage);
begin
    StateChange(httpReady);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ReplaceExt(var FName : String; const newExt : String);
var
    I : Integer;
begin
    I := Posn('.', FName, -1);
    if I <= 0 then
        FName := FName + '.' + newExt
    else
        FName := Copy(FName, 1, I) + newExt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.AbortComponent;
begin
    Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.Abort;
var
    bFlag : Boolean;
    Msg   : TMessage;
begin
    if FState = httpReady then begin
        FState := httpAborting;
        if FCtrlSocket.State <> wsClosed then
            FCtrlSocket.Abort;
        FStatusCode       := 200;
        FReasonPhrase     := 'OK';
        FRequestDoneError := 0;
        FState            := httpReady;
        TriggerStateChange;
        WMHttpRequestDone(Msg);   { Synchronous operation ! }
        Exit;
    end;

    bFlag := (FState = httpDnsLookup);
    StateChange(httpAborting);

    if bFlag then begin
        try
            FCtrlSocket.CancelDnsLookup;
        except
            { Ignore any exception }
        end;
    end;

    FStatusCode       := 404;
    FReasonPhrase     := 'Connection aborted on request';
    FRequestDoneError := httperrAborted;

    if bFlag then
        SocketSessionClosed(Self, 0)
    else
        FCtrlSocket.Close;
    StateChange(httpReady);  { 13/02/99 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.Login;
begin
    OutputDebugString('Login ' + FHostName);
    FCtrlSocket.OnSessionClosed := SocketSessionClosed;

    if FCtrlSocket.State = wsConnected then begin
        SocketSessionConnected(nil, 0);
        Exit;
    end;

    FDnsResult := '';
    StateChange(httpDnsLookup);
    FCtrlSocket.LocalAddr := FLocalAddr; {bb}
    try
        FCtrlSocket.DnsLookup(FHostName);
    except
        on E: Exception do begin
            FStatusCode   := 404;
            FReasonPhrase := E.Message;
            FConnected    := FALSE;
            StateChange(httpReady);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoBeforeConnect;
begin
    FCtrlSocket.Addr      := FDnsResult;
    FCtrlSocket.LocalAddr := FLocalAddr; {bb}
    FCtrlSocket.Port      := FPort;
    FCtrlSocket.Proto     := 'tcp';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketDNSLookupDone(Sender: TObject; ErrCode: Word);
begin
    if ErrCode <> 0 then begin
        //WSocketTriggerDebugEvent(Self, 'THttpClient.SocketDNSLookupDone. Error #' + IntToStr(ErrCode));
        if FState = httpAborting then
            Exit;
        FRequestDoneError := ErrCode;
        FStatusCode       := 404;
        FReasonPhrase     := 'can''t resolve hostname to IP address';
        SocketSessionClosed(Sender, ErrCode);
    end
    else begin
        FDnsResult            := FCtrlSocket.DnsResult;
        //WSocketTriggerDebugEvent(Self, 'THttpClient.SocketDNSLookupDone. Result = "' + FDnsResult + '"');
        StateChange(httpDnsLookupDone);  { 19/09/98 }
        DoBeforeConnect;
        FCurrentHost          := FHostName;
        FCurrentPort          := FPort;
        FCurrentProtocol      := FProtocol;
{$IFDEF USE_SSL}
        FCtrlSocket.SslEnable := ((FProxy = '') and (FProtocol = 'https'));
{$ENDIF}
        OutputDebugString('connect to ' + FDnsResult + '/' + FPort);
        try
            FCtrlSocket.Connect;
        except
            FRequestDoneError := FCtrlSocket.LastError;
            FStatusCode       := 404;
            FReasonPhrase     := 'can''t connect: ' +
                                 WSocketErrorDesc(FCtrlSocket.LastError) +
                                 ' (Error #' + IntToStr(FCtrlSocket.LastError) + ')';
            FCtrlSocket.Close;
            SocketSessionClosed(Sender, FCtrlSocket.LastError);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketSessionConnected(Sender : TObject; ErrCode : Word);
begin
    OutputDebugString('SessionConnected');
    if ErrCode <> 0 then begin
        FRequestDoneError := ErrCode;
        FStatusCode       := 404;
        FReasonPhrase     := WSocketErrorDesc(ErrCode) +
                             ' (Error #' + IntToStr(ErrCode) + ')';
        SocketSessionClosed(Sender, ErrCode);
        Exit;
    end;

    FLocationFlag := FALSE;

    FConnected := TRUE;
    StateChange(httpConnected);
    TriggerSessionConnected;

    FNext := GetHeaderLineNext;
    try
        if (FProxy <> '') and
           (FProtocol = 'https') and
           (FProxyConnected = FALSE) then begin
            StateChange(httpWaitingProxyConnect);
            FReqStream.Clear;
            SendCommand('CONNECT ' + FTargetHost + ':' + FTargetPort +
                        ' HTTP/' + FRequestVer);
            SendCommand('');
            FCtrlSocket.PutDataInSendBuffer(FReqStream.Memory, FReqStream.Size);
            FReqStream.Clear;
            FCtrlSocket.Send(nil, 0);
        end
        else begin
            StateChange(httpWaitingHeader);

            case FRequestType of
            httpPOST:
                begin
                    SendRequest('POST', FRequestVer);
                    TriggerSendBegin;
                    FAllowedToSend := TRUE;
                    SocketDataSent(FCtrlSocket, 0);
                end;
            httpPUT:
                begin
                    SendRequest('PUT', FRequestVer);
                    TriggerSendBegin;
                    FAllowedToSend := TRUE;
                    SocketDataSent(FCtrlSocket, 0);
                end;
            httpHEAD:
                begin
                    SendRequest('HEAD', FRequestVer);
                end;
            httpGET:
                begin
                    SendRequest('GET', FRequestVer);
                end;
            end;
        end;
    except
        Logout;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.Logout;
begin
    FCtrlSocket.Close;
    FConnected := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SendCommand(const Cmd : String);
var
    Buf : String;
    I   : Integer;
begin
    Buf := Cmd;
    if Assigned(FOnCommand) then
        FOnCommand(Self, Buf);
    for I := 1 to Length(Buf) do
        FReqStream.Write(Byte(Buf[I]));  // We send ASCII code, not unicode
    FReqStream.Write(Byte(13));          // CR
    FReqStream.Write(Byte(10));          // LF
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SendRequest(const Method, Version: String);
var
    Headers : TStrings;
    N       : Integer;
begin
    Headers := TStringList.Create;
    try
        FReqStream.Clear;
        TriggerRequestHeaderBegin;
        OutputDebugString(method + ' ' + FPath + ' HTTP/' + Version);
        Headers.Add(method + ' ' + FPath + ' HTTP/' + Version);
        if FSender <> '' then
            Headers.Add('From: ' + FSender);
        if FAccept <> '' then
            Headers.Add('Accept: ' + FAccept);
        if FReference <> '' then
            Headers.Add('Referer: ' + FReference);
        if FConnection <> '' then
            Headers.Add('Connection: ' + FConnection);
        if FAcceptLanguage <> '' then
            Headers.Add('Accept-Language: ' + FAcceptLanguage);
        if ((FRequestType = httpPOST) or (FRequestType = httpPUT)) and
           (FContentPost <> '') then
            Headers.Add('Content-Type: ' + FContentPost);
        if FAgent <> '' then
            Headers.Add('User-Agent: ' + FAgent);
        Headers.Add('Host: ' + FTargetHost);
        if FNoCache then
            Headers.Add('Pragma: no-cache');
        if (FRequestType = httpPOST) or (FRequestType = httpPUT) then
            Headers.Add('Content-Length: ' + IntToStr(SendStream.Size));
        if FModifiedSince.Year > 1 then
 //       if FModifiedSince <> EncodeDate(0, 0, 0) then
            Headers.Add('If-Modified-Since: ' +
                        RFC1123_Date(FModifiedSince) + ' GMT');
        if FUsername <> '' then
            Headers.Add('Authorization: Basic ' +
                        EncodeStr(encBase64, FUsername + ':' + FPassword));
        if (FProxy <> '') and (FProxyUsername <> '') then
            Headers.Add('Proxy-Authorization: Basic ' +
                        EncodeStr(encBase64, FProxyUsername + ':' + FProxyPassword));
        if FCookie <> '' then
            Headers.Add('Cookie: ' + FCookie);
        if (FContentRangeBegin <> '') or (FContentRangeEnd <> '') then begin            {JMR!! Added this line!!!}
            Headers.Add('Range: bytes=' + FContentRangeBegin + '-' + FContentRangeEnd); {JMR!! Added this line!!!}
          FContentRangeBegin := '';                                                     {JMR!! Added this line!!!}
          FContentRangeEnd   := '';                                                     {JMR!! Added this line!!!}
        end;                                                                            {JMR!! Added this line!!!}
        FAcceptRanges := '';

{SendCommand('UA-pixels: 1024x768'); }
{SendCommand('UA-color: color8'); }
{SendCommand('UA-OS: Windows 95'); }
{SendCommand('UA-CPU: x86'); }
{SendCommand('Proxy-Connection: Keep-Alive'); }

        OutputDebugString(IntToStr(Headers.Count) + ' header lines to send');
        TriggerBeforeHeaderSend(Method, Headers);
        for N := 0 to Headers.Count - 1 do
            SendCommand(Headers[N]);

        TriggerRequestHeaderEnd;
        SendCommand('');
        FCtrlSocket.PutDataInSendBuffer(FReqStream.Memory, FReqStream.Size);
        FReqStream.Clear;
        FCtrlSocket.Send(nil, 0);
    finally
        Headers.Free;
        OutputDebugString('SendRequest Done');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Data is pointed by FBodyData and FBodyDataLen as length                   }
procedure THttpClient.GetBodyLineNext;
var
    I : Integer;
begin
    OutputDebugString('GetBodyLineNext begin');
    if FBodyLineCount = 0 then
        TriggerDocBegin;
    Inc(FBodyLineCount);

    OutputDebugString('GetBodyLineNext FBodyDataLen=' + IntToStr( FBodyDataLen));

    if FBodyDataLen > 0 then begin
        FRcvdCount := FRcvdCount + FBodyDataLen;
        if Assigned(FRcvdStream) then begin
            //FRcvdStream.WriteBuffer((FBodyData^, FBodyDataLen);
            for I := FBodyData to FBodyData + FBodyDataLen - 1 do
                FRcvdStream.WriteBuffer(FReceiveBuffer[I]);
        end;
        TriggerDocData(FReceiveBuffer, FBodyData, FBodyDataLen);
    end;

    if FRcvdCount = FContentLength then begin
        { End of document }
        OutputDebugString('end of document');
        FBodyLineCount := 0;
        FNext          := nil;
        StateChange(httpBodyReceived);
        TriggerDocEnd;
        if (FResponseVer = '1.0') or (FRequestVer = '1.0') then
            FCtrlSocket.CloseDelayed
        else
            SetReady;
    end;
    OutputDebugString('GetBodyLineNext end');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.GetHeaderLineNext;
var
    proto   : String;
    user    : String;
    pass    : String;
    port    : String;
    Host    : String;
    Path    : String;
    Field   : String;
    Data    : String;
    nSep    : Integer;
    tmpInt  : LongInt;
    bAccept : Boolean;
    DocExt  : String;
    SaveLoc : String;
begin
    if FHeaderLineCount = 0 then
        TriggerHeaderBegin
    else if FHeaderLineCount = -1 then   { HTTP/1.1 second header }
        FHeaderLineCount := 0;
    Inc(FHeaderLineCount);

    { Some server send HTML document without header ! I don't know if it is  }
    { legal, but it exists (AltaVista Discovery does that).                  }
    if (FHeaderLineCount = 1) and
       (UpperCase(Copy(FLastResponse, 1, 6)) = '<HTML>') then begin { 15/09/98 }
        if FContentType = '' then
            FContentType := 'text/html';
        StateChange(httpWaitingBody);
        FNext := GetBodyLineNext;
        TriggerHeaderEnd;
        FBodyData    := 0;
        FBodyDataLen := Length(FLastResponse);
        GetBodyLineNext;
        Exit;
    end;

    if FLastResponse = '' then begin
        OutputDebugString('end of header');
        if (FResponseVer = '1.1') and (FStatusCode = 100) then begin
            { HTTP/1.1 continue message. A second header follow. }
            { I should create an event to give access to this.   }
            FRcvdHeader.Clear;        { Cancel this first header }
            FHeaderLineCount := -1;   { -1 is to remember we went here }
            Exit;
        end;

        if FLocationFlag then begin
            TriggerHeaderEnd;
            FReceiveLen       := 0;
            FHeaderLineCount  := 0;
            FBodyLineCount    := 0;
            OutputDebugString('starting relocation process');
            if (FResponseVer     = '1.1') and
               (FCurrentHost     = FHostName) and
               (FCurrentPort     = FPort) and
               (FCurrentProtocol = FProtocol) then begin
                { No need to disconnect }
                { Trigger the location changed event  27/04/2003 }
                if Assigned(FOnLocationChange) then
                     FOnLocationChange(Self);
                SaveLoc := FLocation;  { 01/05/03 }
                InternalClear;
                FLocation := SaveLoc;
                FDocName  := FPath;
                AdjustDocName;
                { When relocation occurs doing a POST, new relocated page }
                { has to be GET.  01/05/03                                }
                if FRequestType = httpPOST then
                    FRequestType  := httpGET;
                PostMessage(FWindowHandle, WM_HTTP_LOGIN, 0, 0);
            end
            else begin
                FCtrlSocket.OnSessionClosed := LocationSessionClosed;
                FCtrlSocket.CloseDelayed;
            end;
            Exit;
        end;
        { FContentLength = -1 when server doesn't send a value }
        if (FContentLength = 0) or (FRequestType = httpHEAD) then begin
            TriggerHeaderEnd;
            SetReady;
            Exit;
        end;
        DocExt := LowerCase(ExtractFileExt(FDocName));
        if (DocExt = '.exe') or (DocExt = '') then begin
            if FContentType = 'text/html' then
                ReplaceExt(FDocName, 'htm');
        end;

        StateChange(httpWaitingBody);
        FNext := GetBodyLineNext;
        TriggerHeaderEnd;
        if FReceiveLen > 0 then begin
            FBodyData    := 0;
            if (FContentLength < 0) or
               ((FRcvdCount + FReceiveLen) <= FContentLength) then
                FBodyDataLen := FReceiveLen
            else
                FBodyDataLen := FContentLength - FRcvdCount;
            GetBodyLineNext;
            FReceiveLen := FReceiveLen - FBodyDataLen;
        end;
        if FStatusCode >= 400 then   { 01/11/01 }
            FCtrlSocket.Close;
        Exit;
    end;

    FRcvdHeader.Add(FLastResponse);

    nSep := pos(':', FLastResponse);
    if (FHeaderLineCount = 1) and
       ((Copy(FLastResponse, 1, 8) = 'HTTP/1.0') or
        (Copy(FLastResponse, 1, 8) = 'HTTP/1.1')) then begin
        FResponseVer  := Copy(FLastResponse, 6, 3);
        FStatusCode   := StrToInt(Copy(FLastResponse, 10, 3));
        FReasonPhrase := Copy(FLastResponse, 14, Length(FLastResponse));
    end
    else if nSep > 0 then begin
        Field := LowerCase(Copy(FLastResponse, 1, nSep - 1));
        { Skip spaces }
        Inc(nSep);
        while (nSep < Length(FLastResponse)) and
              (FLastResponse[nSep] = ' ') do
              Inc(nSep);
        Data  := Copy(FLastResponse, nSep, Length(FLastResponse));
        if Field = 'location' then begin { Change the URL ! }
            if FRequestType = httpPUT then begin
                 { Location just tell us where the document has been stored }
                 FLocation := Data;
            end
            else begin
                { OK, we have a real relocation !       }
                { URL with relocations:                 }
                { http://www.webcom.com/~wol2wol/       }
                { http://www.purescience.com/delphi/    }
                { http://www.maintron.com/              }
                { http://www.infoseek.com/AddURL/addurl }
                { http://www.micronpc.com/              }
                { http://www.amazon.com/                }
                { http://count.paycounter.com/?fn=0&si=44860&bt=msie&bv=5&    }
                { co=32&js=1.4&sr=1024x768&re=http://www.thesite.com/you.html }
                FLocationFlag := TRUE;
                if Proxy <> '' then begin
                    { We are using a proxy }
                    if Data[1] = '/' then begin
                        { Absolute location }
                        ParseURL(FPath, proto, user, pass, Host, port, Path);
                        if Proto = '' then
                            Proto := 'http';
                        FLocation := Proto + '://' + Host + Data;
                        FPath     := FLocation;

                        if (user <> '') and (pass <> '') then begin
                            { save user and password given in location @@@}
                            FUsername   := user;
                            FPassword   := pass;
                        end;
                    end
                    else if (Copy(Data, 1, 7) <> 'http://')
{$IFDEF USE_SSL}
                            and (Copy(Data, 1, 8) <> 'https://')
{$ENDIF}
                        then begin
                        { Relative location }
                        FPath     := GetBaseUrl(FPath) + Data;
                        { if Proto = '' then
                            Proto := 'http';
                          FLocation := Proto + '://' + FHostName + '/' + FPath;
                        }
                        FLocation := FPath;
                    end
                    else begin
                        ParseURL(Data, proto, user, pass, Host, port, Path);
                        if port <> '' then
                            FPort := port
                        else begin
{$IFDEF USE_SSL}
                            if proto = 'https' then
                                FPort := '443'
                            else
{$ENDIF}
                                FPort := '80';
                        end;

                        if (user <> '') and (pass <> '') then begin
                            { save user and password given in location @@@}
                            FUsername   := user;
                            FPassword   := pass;
                        end;

                        if (Proto <> '') and (Host <> '') then begin
                            { We have a full relocation URL }
                            FTargetHost := Host;
                            FLocation   := Proto + '://' + Host + Path;
                            FPath       := FLocation;
                        end
                        else begin
                            if Proto = '' then
                                Proto := 'http';
                            if FPath = '' then
                                FLocation := Proto + '://' + FTargetHost + '/' + Host
                            else if Host = '' then
                                FLocation := Proto + '://' + FTargetHost + FPath
                            else
                                FTargetHost := Host;
                        end;
                    end;
                end
                { We are not using a proxy }
                else begin
                    if Data[1] = '/' then begin
                        { Absolute location }
                        FPath     := Data;
                        if Proto = '' then
                            Proto := 'http';
                        FLocation := Proto + '://' + FHostName + FPath;
                    end
                    else if (Copy(Data, 1, 7) <> 'http://')
{$IFDEF USE_SSL}
                            and (Copy(Data, 1, 8) <> 'https://')
{$ENDIF}
                         then begin
                        { Relative location }
                        FPath     := GetBaseUrl(FPath) + Data;
                        if Proto = '' then
                            Proto := 'http';
                        FLocation := Proto + '://' + FHostName + {'/' +} FPath;
                    end
                    else begin
                        ParseURL(Data, proto, user, pass, FHostName, port, FPath);
                        if port <> '' then
                            FPort := port
                        else begin
{$IFDEF USE_SSL}
                            if proto = 'https' then
                                FPort := '443'
                            else
{$ENDIF}
                                FPort := '80';
                        end;

                        FProtocol := Proto;

                        if (user <> '') and (pass <> '') then begin
                            { save user and password given in location @@@}
                            FUsername   := user;
                            FPassword   := pass;
                        end;

                        if (Proto <> '') and (FHostName <> '') then begin
                            { We have a full relocation URL }
                            FTargetHost := FHostName;
                            if FPath = '' then begin
                                FPath := '/';
                                FLocation := Proto + '://' + FHostName;
                            end
                            else
                                FLocation := Proto + '://' + FHostName + FPath;
                        end
                        else begin
                            if Proto = '' then
                                Proto := 'http';
                            if FPath = '' then begin
                                FLocation := Proto + '://' + FTargetHost + '/' + FHostName;
                                FHostName := FTargetHost;
                                FPath     := FLocation;          { 26/11/99 }
                            end
                            else if FHostName = '' then begin
                                FLocation := Proto + '://' + FTargetHost + FPath;
                                FHostName := FTargetHost;
                            end
                            else
                                FTargetHost := FHostName;
                        end;
                    end;
                end;
            end;
        end
        else if Field = 'content-length' then
            FContentLength := StrToInt(Trim(Data))
        else if Field = 'content-range' then begin                             {JMR!! Added this line!!!}
            tmpInt := Pos('-', Data) + 1;                                      {JMR!! Added this line!!!}
            FContentRangeBegin := Copy(Data, 7, tmpInt-8);                     {JMR!! Added this line!!!}
            FContentRangeEnd   := Copy(Data, tmpInt, Pos('/', Data) - tmpInt); {JMR!! Added this line!!!}
        end                                                                    {JMR!! Added this line!!!}
        else if Field = 'accept-ranges' then
            FAcceptRanges := Data
        else if Field = 'content-type' then
            FContentType := LowerCase(Data)
        else if Field = 'www-authenticate' then
            FDoAuthor.add(Data)
        else if Field = 'set-cookie' then begin
            bAccept := TRUE;
            TriggerCookie(Data, bAccept);
        end
    {   else if Field = 'date' then }
    {   else if Field = 'mime-version' then }
    {   else if Field = 'pragma' then }
    {   else if Field = 'allow' then }
    {   else if Field = 'server' then }
    {   else if Field = 'content-encoding' then }
    {   else if Field = 'expires' then }
    {   else if Field = 'last-modified' then }
   end
   else { Ignore  all other responses }
       ;

    if Assigned(FOnHeaderData) then
        FOnHeaderData(Self);

{    if FStatusCode >= 400 then    Moved above 01/11/01 }
{        FCtrlSocket.Close;                             }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.InternalClear;
begin
    FRcvdHeader.Clear;
    FRequestDoneError := 0;
    FDocName          := '';
    FStatusCode       := 0;
    FRcvdCount        := 0;
    FSentCount        := 0;
    FHeaderLineCount  := 0;
    FBodyLineCount    := 0;
    FContentLength    := -1;
    FContentType      := '';  { 25/09/1999 }
    FAllowedToSend    := FALSE;
    FLocation         := FURL;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoRequestAsync(Rq : THttpRequest);
var
    Proto, User, Pass, Host, Port, Path: String;
begin
    if (Rq <> httpCLOSE) and (FState <> httpReady) then
        raise EHttpException.Create('HTTP component is busy', httperrBusy);

    if ((Rq = httpPOST) or (Rq = httpPUT)) and (not Assigned(FSendStream)) then
        raise EHttpException.Create('HTTP component has nothing to post or put',
                                    httpErrNoData);

    if Rq = httpCLOSE then begin
        FStatusCode   := 200;
        FReasonPhrase := 'OK';
        StateChange(httpClosing);
        if FCtrlSocket.State = wsClosed then
            SetReady
        else
           FCtrlSocket.CloseDelayed;
        Exit;
    end;

    { Clear all internal state variables }
    FRequestType := Rq;
    InternalClear;

    { Parse url and proxy to FHostName, FPath and FPort }
    if FProxy <> '' then begin
        ParseURL(FURL, Proto, User, Pass, Host, Port, Path);
        FTargetHost := Host;
        FTargetPort := Port;
        if FTargetPort = '' then begin
            if Proto = 'https' then
                FTargetPort := '443'
            else
                FTargetPort := '80';
        end;
        FPath       := FURL;
        FDocName    := Path;
        if User <> '' then
            FUserName := User;
        if Pass <> '' then
            FPassword := Pass;
        { We need to remove usercode/Password from the URL given to the proxy }
        { but preserve the port                                               }
        if Port <> '' then
            Port := ':' + Port;
        if Proto = '' then
            FPath := 'http://'+ Host + Port + Path
        else
            FPath := Proto + '://' + Host + Port + Path;
        FProtocol := Proto;
        ParseURL(FProxy, Proto, User, Pass, Host, Port, Path);
        if Port = '' then
            Port := ProxyPort;
    end
    else begin
        ParseURL(FURL, Proto, User, Pass, Host, Port, FPath);
        FTargetHost := Host;
        FDocName    := FPath;
        FProtocol   := Proto;
        if User <> '' then
            FUserName := User;
        if Pass <> '' then
            FPassword := Pass;
        if Port = '' then begin
{$IFDEF USE_SSL}
            if Proto = 'https' then
                Port := '443'
            else
{$ENDIF}
                Port := '80';
        end;
    end;
    if Proto = '' then
        Proto := 'http';
    if FPath = '' then
        FPath := '/';

    AdjustDocName;

    FHostName   := Host;
    FPort       := Port;
    //WSocketTriggerDebugEvent(Self, 'Host ="' + FHostName + '"  Port = "' + FPort + '"');
    if FCtrlSocket.State = wsConnected then begin
        if (FHostName = FCurrentHost)     and
           (FPort     = FCurrentPort)     and
           (FProtocol = FCurrentProtocol) then begin
            { We are already connected to the right host ! }
            SocketSessionConnected(Self, 0);
            Exit;
        end;
        { Connected to another website. Abort the connection }
        FCtrlSocket.Abort;
    end;

    FProxyConnected := FALSE;
    { Ask to connect. When connected, we go at SocketSeesionConnected. }
    StateChange(httpNotConnected);
    Login;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.AdjustDocName;
var
    I : Integer;
begin
    I := Pos('?', FDocName);
    if I > 0 then
        FDocName := Copy(FDocName, 1, I - 1);

    if (FDocName = '') or (FDocName[Length(FDocName)] = '/') then
        FDocName := 'document.htm'
    else begin
        if FDocName[Length(FDocName)] = '/' then
            SetLength(FDocName, Length(FDocName) - 1);
        FDocName := Copy(FDocName, Posn('/', FDocName, -1) + 1, 255);
        I := Pos('?', FDocName);
        if I > 0 then
            FDocName := Copy(FDocName, 1, I - 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.DoRequestSync(Rq : THttpRequest);
begin
    DoRequestAsync(Rq);

{$IFDEF VER80}
    { Delphi 1 has no support for multi-threading }
    while FState <> httpReady do
        Application.ProcessMessages;
{$ELSE}
    if FMultiThreaded then begin
        while FState <> httpReady do begin
            FCtrlSocket.ProcessMessages;
            Sleep(0);
        end;
    end
    else begin
        while FState <> httpReady do begin
{$IFNDEF NOFORMS}
            Application.ProcessMessages;
            if Application.Terminated then begin
                Abort;
                break;
            end;
{$ELSE}
            FCtrlSocket.ProcessMessages;
{$ENDIF}
            Sleep(0);
        end;
    end;
{$ENDIF}

    if FStatusCode >= 400 then
        raise EHttpException.Create(FReasonPhrase, FStatusCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.LocationSessionClosed(Sender: TObject; ErrCode: Word);
var
    Proto, User, Pass, Host, Port, Path: String;
    RealLocation : String;
    I            : Integer;
begin
    { Remove any bookmark from the URL }
    I := Pos('#', FLocation);
    if I > 0 then
        RealLocation := Copy(FLocation, 1, I - 1)
    else
        RealLocation := FLocation;

    { Parse the URL }
    ParseURL(RealLocation, Proto, User, Pass, Host, Port, Path);
    FDocName := Path;
    AdjustDocName;
    FConnected      := FALSE;
    FProxyConnected := FALSE;
    FLocationFlag   := FALSE;
    { When relocation occurs doing a POST, new relocated page has to be GET }
    if FRequestType = httpPOST then
        FRequestType  := httpGET;
    { Restore normal session closed event }
    FCtrlSocket.OnSessionClosed := SocketSessionClosed;
    { Trigger the location changed event }
    if Assigned(FOnLocationChange) then
         FOnLocationChange(Self);
    { Clear header from previous operation }
    FRcvdHeader.Clear;
    { Clear status variables from previous operation }
    FHeaderLineCount  := 0;
    FBodyLineCount    := 0;
    FContentLength    := -1;
    FContentType      := '';
    FStatusCode       := 0;
    { Adjust for proxy use  (Fixed: Nov 10, 2001) }
    if FProxy <> '' then
        FPort := ProxyPort;
    { Restart at login procedure }
    PostMessage(FWindowHandle, WM_HTTP_LOGIN, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.WMHttpLogin(var msg: TMessage);
begin
    Login;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketSessionClosed(Sender: TObject; ErrCode: Word);
begin
    OutputDebugString('SessionClosed');
    if ErrCode <> 0 then               { WM 15 sep 2002 }
        FRequestDoneError := ErrCode;  { WM 15 sep 2002 }
    FConnected      := FALSE;
    FProxyConnected := FALSE;
    if FHeaderEndFlag then begin
        { TriggerHeaderEnd has not been called yet }
        TriggerHeaderEnd;
        if FLocationFlag then            { 28/10/01 }
            LocationSessionClosed(Self, 0);
        Exit;
    end;
    if FBodyLineCount > 0 then
        TriggerDocEnd;
    if not FLocationFlag then
        SetReady;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketDataAvailable(Sender: TObject; ErrCode: Word);
var
    Len  : Integer;
    I, J : Integer;
begin
    //WSocketTriggerDebugEvent(Self, 'THttpClient.SocketDataAvailable');
    I := Length(FReceiveBuffer) - FReceiveLen - 1;
    if I <= 0 then
        raise EHttpException.Create('HTTP line too long', httperrOverflow);

    Len := FCtrlSocket.Receive(FReceiveBuffer, FReceiveLen, I);

    OutputDebugString('data available. Len=' + IntToStr(Len));

    if FRequestType = httpAbort then
        Exit;

    if Len <= 0 then
        Exit;

    FReceiveBuffer[FReceiveLen + Len] := 0;  // Nul terminate (useless with DotNet ?)
    FReceiveLen := FReceiveLen + Len;

{$IFDEF USE_SSL}
    if FState = httpWaitingProxyConnect then begin
        // If connection failed to remote host, then we receive a normal
        // HTTP reply from the proxy with a HTML message with error
        // message ! Something like:
        // "HTTP/1.0 200 OK<CRLF>header lines<CRLF><CRLF>document"
        // If connection success we receive
        // "HTTP/1.0 200 Connection established<CRLF><CRLF>"
        OutputDebugString('Proxy connected: "' + FReceiveBuffer + '"');
        FProxyConnected := TRUE;
        if (StrLIComp(FReceiveBuffer, 'HTTP/1.0 200 OK', 15) = 0) or
           (StrLIComp(FReceiveBuffer, 'HTTP/1.1 200 OK', 15) = 0) then
            // Continue as a normal HTTP request
            FState := httpWaitingHeader
        else begin
            // We have a connection to remote host thru proxy, we can start
            // SSL handshake
            OutputDebugString('Start SSL handshake');
            FReceiveLen                    := 0; // Clear input data
            FCtrlSocket.OnSslHandshakeDone := SslHandshakeDone;
            FCtrlSocket.SslEnable          := TRUE;
            FCtrlSocket.StartSslHandshake;
            FState := httpWaitingHeader;
            Exit;
        end;
    end;
{$ENDIF}

    if FState = httpWaitingBody then begin
        if FReceiveLen > 0 then begin
            FBodyData    := 0;
            if (FContentLength < 0) or
               ((FRcvdCount + FReceiveLen) <= FContentLength) then
                FBodyDataLen := FReceiveLen
            else
                FBodyDataLen := FContentLength - FRcvdCount;
            GetBodyLineNext;
            FReceiveLen  := FReceiveLen - FBodyDataLen;
            FBodyDataLen := 0;

            if Assigned(FNext) then
                FNext
            else
                SetReady; {StateChange(httpReady);}
        end;
        { FReceiveLen := 0;   22/02/02 }
        Exit;
    end;

    if FState <> httpWaitingHeader then
        Exit;   { Should never occur ! }

    while FReceiveLen > 0 do begin
        I := Pos(#10, FReceiveBuffer);
        if I <= 0 then
            break;
        if I > FReceiveLen then
            break;

        if (I > 1) and (FReceiveBuffer[I-2] = 13) then
            FLastResponse := Copy(FReceiveBuffer, 0, I - 2)
        else
            FLastResponse := Copy(FReceiveBuffer, 0, I - 1);

        //WSocketTriggerDebugEvent(self, 'LastResponse = "' + FLastResponse + '"');

{$IFDEF DUMP}
        FDumpBuf := '>|';
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
        FDumpStream.WriteBuffer(FLastResponse[1], Length(FLastResponse));
        FDumpBuf := '|' + #13#10;
        FDumpStream.WriteBuffer(FDumpBuf[1], Length(FDumpBuf));
{$ENDIF}
{$IFDEF VER80}
        { Add a nul byte at the end of string for Delphi 1 }
        FLastResponse[Length(FLastResponse) + 1] := #0;
{$ENDIF}
        FReceiveLen := FReceiveLen - I;
        if FReceiveLen > 0 then begin
            // Move remaining data in front of buffer
            for J := 0 to FReceiveLen do
                FReceiveBuffer[J] := FReceiveBuffer[I + J];
        end;

        if FState in [httpWaitingHeader, httpWaitingBody] then begin
            if Assigned(FNext) then
                FNext
            else
                SetReady; {StateChange(httpReady);}
        end
        else begin
            if Assigned(FOnDataAvailable) then
                FOnDataAvailable(Self, ErrCode);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
procedure THttpClient.SslHandshakeDone(Sender : TObject; ErrCode : Word);
begin
    try
        FNext := GetHeaderLineNext;
        StateChange(httpWaitingHeader);

        case FRequestType of
        httpPOST:
            begin
                SendRequest('POST', FRequestVer);
                TriggerSendBegin;
                FAllowedToSend := TRUE;
                SocketDataSent(FCtrlSocket, 0);
            end;
        httpPUT:
            begin
                SendRequest('PUT', FRequestVer);
                TriggerSendBegin;
                FAllowedToSend := TRUE;
                SocketDataSent(FCtrlSocket, 0);
            end;
        httpHEAD:
            begin
                SendRequest('HEAD', FRequestVer);
            end;
        httpGET:
            begin
                SendRequest('GET', FRequestVer);
            end;
        end;
    except
        Logout;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SocketDataSent(Sender : TObject; ErrCode : Word);
var
    Len : Integer;
begin
    if not FAllowedToSend then
        Exit;

    Len := FSendStream.Read(FSendBuffer, sizeof(FSendBuffer));
    if Len <= 0 then begin
        FAllowedToSend := FALSE;
        TriggerSendEnd;
        Exit;
    end;

    if Len > 0 then begin
        FSentCount := FSentCount + Len;
        TriggerSendData(FSendBuffer, 0, Len);
        FCtrlSocket.Send(FSendBuffer, Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Get process and wait until terminated (blocking)      }
procedure THttpClient.Get;
begin
    DoRequestSync(httpGet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Head process and wait until terminated (blocking)     }
procedure THttpClient.Head;
begin
    DoRequestSync(httpHEAD);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Post process and wait until terminated (blocking)     }
procedure THttpClient.Post;
begin
    DoRequestSync(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Put process and wait until terminated (blocking)      }
procedure THttpClient.Put;
begin
    DoRequestSync(httpPUT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the Close process and wait until terminated (blocking)    }
procedure THttpClient.Close;
begin
    DoRequestSync(httpCLOSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the get process and returns immediately (non blocking)    }
procedure THttpClient.GetAsync;
begin
    DoRequestASync(httpGet);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the head process and returns immediately (non blocking)   }
procedure THttpClient.HeadAsync;
begin
    DoRequestASync(httpHEAD);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the post process and returns immediately (non blocking)   }
procedure THttpClient.PostAsync;
begin
    DoRequestASync(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the put process and returns immediately (non blocking)    }
procedure THttpClient.PutAsync;
begin
    DoRequestASync(httpPUT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This will start the close process and returns immediately (non blocking)  }
procedure THttpClient.CloseAsync;
begin
    DoRequestASync(httpCLOSE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetBaseUrl(const Url : String) : String;
var
    I : Integer;
begin
    I := 1;
    while (I <= Length(Url)) and (Url[I] <> '?') do
        Inc(I);
    Dec(I);
    while (I > 0) and (Url[I] <> '/') and (URL[I] <> ':') do
        Dec(I);
    Result := Copy(Url, 1, I);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Added by Eugene Mayevski }
procedure THttpClient.SetSocksServer(value : String);
begin
    FCtrlSocket.SocksServer := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetSocksLevel(value : String);
begin
    FCtrlSocket.SocksLevel := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetSocksPort(value : String);
begin
    FCtrlSocket.SocksPort := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetSocksUsercode(value : String);
begin
    FCtrlSocket.SocksUsercode := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetSocksPassword(value : String);
begin
    FCtrlSocket.SocksPassword := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetSocksAuthentication(value : TSocksAuthentication);
begin
    FCtrlSocket.SocksAuthentication := value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksServer : String;
begin
    result := FCtrlSocket.SocksServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksLevel : String;
begin
    result := FCtrlSocket.SocksLevel;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksPort : String;
begin
    result := FCtrlSocket.SocksPort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksUsercode : String;
begin
    result := FCtrlSocket.SocksUsercode;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksPassword : String;
begin
    result := FCtrlSocket.SocksPassword;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpClient.GetSocksAuthentication : TSocksAuthentication;
begin
    result := FCtrlSocket.SocksAuthentication;
end;
{ Mayevski additions end }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpClient.SetRequestVer(const Ver : String);
begin
    if FRequestVer <> Ver then begin
        if (Ver = '1.0') or (Ver = '1.1') then
            FRequestVer := Ver
        else
            raise EHttpException.Create('Insupported HTTP version',
                                        httperrVersion);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
procedure ParseURL(
    const url : String;
    var Proto, User, Pass, Host, Port, Path : String);
var
    p, q, i : Integer;
    s       : String;
    CurPath : String;
begin
    CurPath := Path;
    proto   := '';
    User    := '';
    Pass    := '';
    Host    := '';
    Port    := '';
    Path    := '';

    if Length(url) < 1 then
        Exit;

    { Handle path beginning with "./" or "../".          }
    { This code handle only simple cases !               }
    { Handle path relative to current document directory }
    if (Copy(url, 1, 2) = './') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);
        Path := CurPath + Copy(url, 3, Length(url));
        Exit;
    end
    { Handle path relative to current document parent directory }
    else if (Copy(url, 1, 3) = '../') then begin
        p := Posn('/', CurPath, -1);
        if p > Length(CurPath) then
            p := 0;
        if p = 0 then
            CurPath := '/'
        else
            CurPath := Copy(CurPath, 1, p);

        s := Copy(url, 4, Length(url));
        { We could have several levels }
        while TRUE do begin
            CurPath := Copy(CurPath, 1, p-1);
            p := Posn('/', CurPath, -1);
            if p > Length(CurPath) then
                p := 0;
            if p = 0 then
                CurPath := '/'
            else
                CurPath := Copy(CurPath, 1, p);
            if (Copy(s, 1, 3) <> '../') then
                break;
            s := Copy(s, 4, Length(s));
        end;

        Path := CurPath + Copy(s, 1, Length(s));
        Exit;
    end;

    p := pos('://', url);
    q := p;
    if p <> 0 then begin
        S := Copy(url, 1, p - 1);
        for i := 1 to Length(S) do begin
            if Pos(S[i], UriProtocolSchemeAllowedChars) <= 0 then begin
                q := i;
                Break;
            end;
        end;
        if q < p then begin
            p     := 0;
            proto := 'http';
        end;
    end;
    if p = 0 then begin
        if (url[1] = '/') then begin
            { Relative path without protocol specified }
            proto := 'http';
            p     := 1;
            if (Length(url) > 1) and (url[2] <> '/') then begin
                { Relative path }
                Path := Copy(url, 1, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 5)) = 'http:' then begin
            proto := 'http';
            p     := 6;
            if (Length(url) > 6) and (url[7] <> '/') then begin
                { Relative path }
                Path := Copy(url, 6, Length(url));
                Exit;
            end;
        end
        else if lowercase(Copy(url, 1, 7)) = 'mailto:' then begin
            proto := 'mailto';
            p := pos(':', url);
        end;
    end
    else begin
        proto := Copy(url, 1, p - 1);
        inc(p, 2);
    end;
    s := Copy(url, p + 1, Length(url));

    p := pos('/', s);
    q := pos('?', s);
    if (q > 0) and ((q < p) or (p = 0)) then
        p := q;
    if p = 0 then
        p := Length(s) + 1;
    Path := Copy(s, p, Length(s));
    s    := Copy(s, 1, p-1);

    p := Posn(':', s, -1);
    if p > Length(s) then
        p := 0;
    q := Posn('@', s, -1);
    if q > Length(s) then
        q := 0;
    if (p = 0) and (q = 0) then begin   { no user, password or port }
        Host := s;
        Exit;
    end
    else if q < p then begin  { a port given }
        Port := Copy(s, p + 1, Length(s));
        Host := Copy(s, q + 1, p - q - 1);
        if q = 0 then
            Exit; { no user, password }
        s := Copy(s, 1, q - 1);
    end
    else begin
        Host := Copy(s, q + 1, Length(s));
        s := Copy(s, 1, q - 1);
    end;
    p := pos(':', s);
    if p = 0 then
        User := s
    else begin
        User := Copy(s, 1, p - 1);
        Pass := Copy(s, p + 1, Length(s));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeStr(Encoding : THttpEncoding; const Value : String) : String;
begin
    Result := EncodeLine(Encoding, Value, Length(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function EncodeLine(
    Encoding       : THttpEncoding;
    const SrcData  : String;
    Size           : Integer) : String;
var
    Offset : Integer;
    Pos1   : Integer;
    Pos2   : Integer;
    I      : Integer;
begin
    SetLength(Result, Size * 4 div 3 + 4);
    //FillChar(Result[1], Size * 4 div 3 + 2, #0);

    if Encoding = encUUEncode then begin
        Result[1] := Char(((Size - 1) and $3f) + $21);
        Size      := ((Size + 2) div 3) * 3;
    end;
    Offset := 2;
    Pos1   := 0;
    Pos2   := 0;
    case Encoding of
        encUUEncode:        Pos2 := 2;
        encBase64, encMime: Pos2 := 1;
    end;
    Result[Pos2] := #0;

    while Pos1 < Size do begin
        if Offset > 0 then begin
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and
                                  ($3f shl Offset)) shr Offset));
            Offset := Offset - 6;
            Inc(Pos2);
            Result[Pos2] := #0;
        end
        else if Offset < 0 then begin
            Offset := Abs(Offset);
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and
                                  ($3f shr Offset)) shl Offset));
            Offset := 8 - Offset;
            Inc(Pos1);
        end
        else begin
            Result[Pos2] := Char(ord(Result[Pos2]) or
                                 ((ord(SrcData[Pos1]) and $3f)));
            Inc(Pos2);
            Inc(Pos1);
            Result[Pos2] := #0;
            Offset    := 2;
        end;
    end;

    case Encoding of
    encUUEncode:
        begin
            if Offset = 2 then
                Dec(Pos2);
            for i := 2 to Pos2 do
                Result[i] := bin2uue[ord(Result[i])+1];
        end;
    encBase64, encMime:
        begin
            if Offset = 2 then
                Dec(Pos2);
            for i := 1 to Pos2 do
                Result[i] := bin2b64[ord(Result[i])+1];
            while (Pos2 and 3) <> 0  do begin
                Inc(Pos2);
                Result[Pos2] := '=';
            end;
        end;
    end;
    SetLength(Result, Pos2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Find the count'th occurence of the s string in the t string.              }
{ If count < 0 then look from the back                                      }
function Posn(const s , t : String; Count : Integer) : Integer;
var
    i, h, Last : Integer;
    u          : String;
begin
    u := t;
    if Count > 0 then begin
        Result := Length(t);
        for i := 1 to Count do begin
            h := Pos(s, u);
            if h > 0 then
                u := Copy(u, h + 1, Length(u))
            else begin
                u := '';
                Inc(Result);
            end;
        end;
        Result := Result - Length(u);
    end
    else if Count < 0 then begin
        Last := 0;
        for i := Length(t) downto 1 do begin
            u := Copy(t, i, Length(t));
            h := Pos(s, u);
            if (h <> 0) and ((h + i) <> Last) then begin
                Last := h + i - 1;
                Inc(count);
                if Count = 0 then
                    break;
            end;
        end;
        if Count = 0 then
            Result := Last
        else
            Result := 0;
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : Char) : Boolean;
begin
    Result := ((Ch >= '0') and (Ch <= '9'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsXDigit(Ch : char) : Boolean;
begin
    Result := (((Ch >= '0') and (Ch <= '9')) or
               ((Ch >= 'a') and (Ch <= 'f')) or
               ((Ch >= 'A') and (Ch <= 'F')));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function XDigit(Ch : Char) : Integer;
begin
    if (Ch >= '0') and (Ch <= '9') then
        Result := ord(Ch) - ord('0')
    else
        Result := (ord(Ch) and 15) + 9;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncode(S : String) : String;
var
    I  : Integer;
    Ch : Char;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        Ch := S[I];
        if ((Ch >= '0') and (Ch <= '9')) or
           ((Ch >= 'a') and (Ch <= 'z')) or
           ((Ch >= 'Z') and (Ch <= 'Z')) then
            Result := Result + Ch
        else
            Result := Result + '%' + IntToHex(Ord(Ch), 2);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlDecode(S : String) : String;
var
    I  : Integer;
    Ch : Char;
begin
    Result := '';
    I := 1;
    while (I <= Length(S)) and (S[I] <> '&') do begin
        Ch := S[I];
        if Ch = '%' then begin
            Ch := Char(XDigit(S[I]) * 16 + XDigit(S[I + 1]));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Result := Result + Ch;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

