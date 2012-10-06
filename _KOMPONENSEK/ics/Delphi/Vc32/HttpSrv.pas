{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  THttpServer implement the HTTP server protocol, that is a
              web server kernel.
              You must implement sucurity yourself, specially, check the URL or
              document path for valid values in your application. Be sure to
              check for '..\', '.\', drive designation and UNC.
              Do the check in OnGetDocument and similar event handlers.
Creation:     Oct 10, 1999
Version:      1.31
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
                                           francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2005 by François PIETTE
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

History:
Nov 12, 1999 Beta 3 Added Linger properties
Apr 23, 2000 Beta 4 Added Delphi 1 compatibility
             Made everything public in THttpConnection because BCB has problems
             when deriving a component from Delphi and protected functions.
Oct 29, 2000 Beta 5 Added Client[] property and IsClient() method.
Nov 11, 2000 Beta 6 Added code from Sven <schmidts@cdesign.de> to set
             Last-Modified header line. Need some more changes !
Nov 12, 2000 Beta 7 Finished Last-Modified implementation.
             Corrected TriggerServerStopped.
Jun 18, 2001 V1.01 Check if still connected before sending any data.
Jul 31, 2001 V1.02 Handle URL encoded document file (for example when there is
             a space in the file name). Bug reported by Stian Grønland
             <joepezt@berzerk.net>.
             Better handling of non existant documents or documents with invalid
             file name.
Jan 13, 2002 V1.03 Changed SetPort() to SetPortValue() to avoid a conflict with
             BCB6 which has a macro to replace SetPort with SetPortA !
Apr 15, 2002 V1.04 In SendDocument, avoid calling Send(nil, 0) when a document
             has to be sent. Bug and fix by Adam Laforge <goodge@goodge.ca>.
Jul 06, 2002 V1.05 Thomas Smyth <smythtp@netscape.net> fixed
             ExtractURLEncodedValue which had problem with names beginning with
             same sequence of chars.
Oct 12, 2002 V1.06 Added THttpConnection.PostedDataReceived to be called from
             OnPostedData event handler when application has received all data
             from client.
             Added read/only property SrcVersion with component source version
Jan 05, 2003 V1.07 Fixed isxdigit
Feb 11, 2003 V1.08 Enhanced HTTP1.1 handling (persitant connexions)
Aug 28, 2003 V1.09 Stanislav Korotky <editor@tbsoft.ru> added the
             OnHttpRequestDone event and related stuff.
Oct 25, 2003 V1.10 Added a space before 'GMT' in Lats-Modified header line.
Oct 31, 2003 V1.11 Changed ProcessRequest so that empty FPath doesn't cause
             any problem.
Dec 17, 2003 V1.12 Added THttpConnection.FServer field
Jan 03, 2004 V1.13 Added directory list feature, Options property, utility
             functions TextToHtmlText, TranslateChar, UnixPathToDosPath,
             DosPathToUnixPath, IsDirectory, UrlDecode, AbsolutisePath.
             Removed useless units in the uses clause.
             Added hoAllowDirList and hoAllowOutsideRoot options.
             Added '.' in non encoded chars in UrlEncode
             Added hg403 flag and Answer403 response.
Jan 12, 2004 V1.14 "David A. G." <davidag@arnet.com.ar> added
                - Basic authorization support (Authorization header line)
                - Icon support "image/x-icon"
                - Property MaxClients
                - Simple WAP documents support
             Ivan Turcan <iturcan@drake-it.com>Added is Content-Range support
             by Http 1.1 specs. Still buggy ! Need to check end of range.
Jan 15, 2004 V1.15 Ivan Turcan code fixed.
             Conditional compilation with "UseInt64ForHttpRange" symbol to
             allow using 64 bit integer for HTTP ranges
Jan 16, 2004 V1.16 SendStream reset FDocstream position to start of stream.
             Added SendHeader method to make things more clear.
             David A.G. Fixed a bug related to sending last stream chunk.
Jan 17, 2004 V1.18 David A. G. <davidag@arnet.com.ar> implemented multi-host
             feature. That is added properties RequestHostName and
             RequestHostPort as well as event OnBeforeProcessRequest. From this
             event you can check RequestHostName and change DocDir and
             DefaultDoc appropriately.
             Added a CRLF at the end of the line generated by MakeCookie.
Jan 26, 2004 V1.19 Reordered uses for FPC compatibility.
             Added RequestCookies with the cookies included in the request.
Mar 27, 2004 V1.20 Added THttpConnection.AnswerPage method.
Apr 09, 2004 V1.21 Added method AnswerStream.
             Renamed Answer() to AnswerString() to make it similar to
             AnswerPage and AnswerStream.
May 11, 2004 V1.22 Added text/css check in DocumentToContentType
May 15, 2004 V1.23 Jack (jlist@softhome.net) added better support for HTTP/1.0
             persitant connection
May 30, 2004 V1.24 Enhanced byte-ranges support. Thanks to Andreas Hofmann
             <andreas.hofmann@docware.de> for his code which has been optimized
             and revised to work with older Delphi versions.
Jun 06, 2004 V1.25 Andreas Hofmann fixed a memory leak in the byte-ranges code
             implemented in V1.24. He also replaced THttpPartStreamRecord by an
             object THttpPartStream.
Jun 20, 2004 V1.26 THttpRangeList.Clear is not virtual in D3. (A. Garrels)
Jul 18, 2004 V1.27 David A. G. <davidag@arnet.com.ar> revised keep-alive
             handling. See SendDocument and ConnectionDataSent.
Aug 30, 2004 V1.28 Added support function RemoveHtmlSpecialChars which replace
             characters used as delimiter in HTML by their equivalent code:
             '<',    '>',    '&',     '''',    '"' are replaced by
             '&lt;', '&gt;', '&amp;', '&#39;', '&quot;'
             Added THttpSrv.SetDocDir (setter function for DocDir property) so
             that FDocDir is never terminated by a '\'.
             Changed THttpConnection.ProcessRequest to convert FPath to DOS
             type path.
Sep 12, 2004 V1.29 Made WSocketServer runtime property available.
Jan 08, 2005 V1.30 In TextToHtmlText, replace multiple spaces by a single space
             followed by the required number of non-breaking-spaces (&nbsp;)
             Replace TAB by a non-breaking-space.
             Added methods HtmlPageProducerToString and HtmlPageProducerToStream
             which ease the generation of dynamic pages from several templates.
Jan 14, 2005 V1.31 Added function HtmlPageProducerSetTagPrefix which allows
             to change the delimiter used in the various HtmlPageProducer
             functions. You can now change the '#' to something line 'ics:'
             which is better for XHTML validation tools provided you have a
             namespace defining the '<ics:somename>' tags. By default the
             delimiter is still '#' and no change is needed in the existing
             code. The HtmlPageProducer functions also accept a better form
             of the ending TABLE_ROWS delimiter (for XHTML compatibility). You
             can now use "</ics:TABLE_ROWS>" ("ics:" being whatever you selected
             with HtmlPageProducerSetTagPrefix and begin '#' by default).


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpSrv;

{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I ICSDEFS.INC}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF COMPILER2_UP} { Not for Delphi 1                    }
    {$H+}             { Use long strings                    }
    {$J+}             { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{ DEFINE USE_ZLIB} { Experimental code, doesn't work yet }

interface

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes,
{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
    IcsSSLEAY, IcsLIBEAY,
{$ENDIF}
{$IFDEF USE_ZLIB}
    dZLib, zDeflate, ZLibh,
{$ENDIF}
    WSocket, WSocketS;

const
    THttpServerVersion = 131;
    CopyRight : String = ' THttpServer (c) 1999-2005 F. Piette V1.31 ';
    WM_HTTP_DONE       = WM_USER + 40;

type
    THttpServer          = class;
    THttpConnection      = class;
    THttpConnectionClass = class of THttpConnection;
    THttpDirEntry        = class;
    TStringIndex         = class;

    THttpGetFlag         = (hgSendDoc, hgSendStream, hgWillSendMySelf,
                            hg404, hg403, hg401, hgAcceptData, hgSendDirList);
    THttpSendType        = (httpSendHead, httpSendDoc);
    THttpGetEvent        = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      var Flags : THttpGetFlag) of object;
    THttpGetConnEvent    = procedure (Sender    : TObject;
                                      var Flags : THttpGetFlag) of object;
    THttpConnectEvent    = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      Error     : Word) of object;
    THttpPostedDataEvent = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      Error     : Word) of object;
    THttpRequestDoneEvent= procedure (Sender    : TObject;
                                      Client    : TObject) of object;
    THttpBeforeProcessEvent= procedure (Sender  : TObject;   {DAVID}
                                        Client  : TObject) of object;
    THttpFilterDirEntry  = procedure (Sender    : TObject;
                                      Client    : TObject;
                                      DirEntry  : THttpDirEntry) of object;
    THttpGetRowDataEvent = procedure (Sender          : TObject;
                                      const TableName : String;
                                      Row             : Integer;
                                      TagData         : TStringIndex;
                                      var More        : Boolean;
                                      UserData        : TObject) of object;

    THttpConnectionState = (hcRequest, hcHeader, hcPostedData);
    THttpOption          = (hoAllowDirList, hoAllowOutsideRoot);
    THttpOptions         = set of THttpOption;
{$IFDEF UseInt64ForHttpRange}
    THttpRangeInt        = Int64;
{$ELSE}
    THttpRangeInt        = LongInt;   { Limited to 2GB size }
{$ENDIF}

    {ANDREAS one byte-range}
    THttpRange = class( TObject)
    private
        FRangeTo   : THttpRangeInt;
        FRangeFrom : THttpRangeInt;
        procedure SetRangeFrom(const Value: THttpRangeInt);
        procedure SetRangeTo(const Value: THttpRangeInt);
    public
        constructor Create;
        procedure   Assign(Source: THttpRange);
        function    GetContentRangeString(CompleteDocSize : THttpRangeInt): String;
        property    RangeFrom : THttpRangeInt read FRangeFrom write SetRangeFrom;
        property    RangeTo   : THttpRangeInt read FRangeTo   write SetRangeTo;
    end;

    {ANDREAS list of byte-ranges}
    THttpRangeList = class(TList)
    private
        function  GetItems(NIndex: Integer): THttpRange;
        procedure SetItems(NIndex: Integer; const Value: THttpRange);
    public
        destructor Destroy; override;
        procedure  Clear; {$IFDEF DELPHI4_UP}override; {$ENDIF}
        procedure Assign(Source: THttpRangeList);
        function  CreateRangeStream(SourceStream    : TStream;
                                    ContentString   : String;
                                    CompleteDocSize : THttpRangeInt): TStream;
        function  Valid: Boolean;
        procedure InitFromString(AStr: String);
        property  Items[NIndex: Integer]: THttpRange read  GetItems
                                                    write SetItems;
    end;

    THttpPartStream = class(TObject)
        Stream   : TStream;
        StartPos : THttpRangeInt;
        EndPos   : THttpRangeInt;
        Offset   : THttpRangeInt;
        Size     : THttpRangeInt;
    end;

    {ANDREAS virtual Stream for the byte-range content }
    THttpRangeStream = class(TStream)
    private
        FSourceStream : TStream;
        FPosition     : THttpRangeInt;
        FSize         : THttpRangeInt;
        FPartStreams  : TList;
        procedure ClearPartStreams;
        procedure CalculateOffsets;
        function  GetPartStreams(NIndex: Integer): THttpPartStream;
    public
        constructor Create;
        destructor  Destroy; override;
        procedure AddPartStream(Value     : TStream;
                                AStartPos : Integer;
                                AEndPos   : Integer);
        function InitRangeStream(SourceStream  : TStream;
                                 RangeList     : THttpRangeList;
                                 ContentString : String): Boolean;
        function Read(var Buffer; Count: Longint): Longint; override;
        function Write(const Buffer; Count: Longint): Longint; override;
{$IFDEF DELPHI7_UP}
        function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
{$ELSE}
        function Seek(Offset: Longint; Origin: Word): Longint; override;
{$ENDIF}
        function PartStreamsCount: Integer;
        property PartStreams[NIndex : Integer] : THttpPartStream
                                                   read  GetPartStreams;
    end;

    { THttpConnection is used to handle client connections }
{$IFDEF USE_SSL}
    THttpConnection = class(TSslWSocketClient)
{$ELSE}
    THttpConnection = class(TWSocketClient)
{$ENDIF}
    protected
        FRcvdLine              : String;
        FMethod                : String;
        FVersion               : String;
        FPath                  : String;
        FParams                : String;
        FRequestHeader         : TStringList;
        FState                 : THttpConnectionState;
        FDocDir                : String;
        FTemplateDir           : String;
        FDefaultDoc            : String;
        FDocument              : String;
        FDocStream             : TStream;
        FDocBuf                : PChar;
        FLastModified          : TDateTime;
        FAnswerContentType     : String;
        FRequestContentLength  : Integer;
        FRequestContentType    : String;
        FRequestAccept         : String;
        FRequestReferer        : String;
        FRequestAcceptLanguage : String;
        FRequestAcceptEncoding : String;
        FRequestUserAgent      : String;
        FRequestAuth           : String;        {DAVID}
        FRequestCookies        : String;
        FRequestHost           : String;
        FRequestHostName       : String;        {DAVID}
        FRequestHostPort       : String;        {DAVID}
        FRequestConnection     : String;
        FAcceptPostedData      : Boolean;
{$IFDEF USE_ZLIB}
        FReplyDeflate          : Boolean;
        FCompressStream        : TCompressionStream;
        FDecompressStream      : TDecompressionStream;
        FZDocStream            : TMemoryStream;
        FZBuffer               : array [0..8191] of Char;
{$ENDIF}
        FServer                : THttpServer;
        FBasicRealm            : String;
        FOptions               : THttpOptions;
        FOutsideFlag           : Boolean;
        FRequestRangeValues    : THttpRangeList; {ANDREAS}
        FDataSent              : THttpRangeInt;  {TURCAN}
        FDocSize               : THttpRangeInt;  {TURCAN}
        FOnGetDocument         : THttpGetConnEvent;
        FOnHeadDocument        : THttpGetConnEvent;
        FOnPostDocument        : THttpGetConnEvent;
        FOnPostedData          : TDataAvailable;
        FOnHTTPRequestDone     : TNotifyEvent;
        FOnBeforeProcessRequest: TNotifyEvent;  {DAVID}
        FOnFilterDirEntry      : THttpFilterDirEntry;
        FOnGetRowData          : THttpGetRowDataEvent;
        procedure ConnectionDataAvailable(Sender: TObject; Error : Word); virtual;
        procedure ConnectionDataSent(Sender : TObject; Error : WORD); virtual;
        procedure ParseRequest; virtual;
        procedure ProcessRequest; virtual;
        procedure ProcessGet; virtual;
        procedure ProcessHead; virtual;
        procedure ProcessPost; virtual;
        procedure Answer404; virtual;
        procedure Answer403; virtual;
        procedure Answer401; virtual;
        procedure WndProc(var MsgRec: TMessage); override;
        procedure WMHttpDone(var msg: TMessage); message WM_HTTP_DONE;
        procedure TriggerGetDocument(var Flags : THttpGetFlag); virtual;
        procedure TriggerHeadDocument(var Flags : THttpGetFlag); virtual;
        procedure TriggerPostDocument(var Flags : THttpGetFlag); virtual;
        procedure TriggerHttpRequestDone; virtual;
        procedure TriggerBeforeProcessRequest; virtual; {DAVID}
        procedure TriggerFilterDirEntry(DirEntry: THttpDirEntry); virtual;
        procedure SendDirList(SendType : THttpSendType); virtual;
        function  BuildDirList: String; virtual;
        function  FormatDirEntry(F: THttpDirEntry): String; virtual;
        procedure TriggerGetRowData(const TableName : String;
                                    Row             : Integer;
                                    TagData         : TStringIndex;
                                    var More        : Boolean;
                                    UserData        : TObject); virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   SendStream; virtual;
        procedure   SendDocument(SendType : THttpSendType); virtual;
        procedure   SendHeader(Header : String); virtual;
        procedure   PostedDataReceived; virtual;
        { AnswerPage will take a HTML template and replace all tags in this
          template with data provided in the Tags argument.
          The tags in the template must have the form <#TagName>.
          The Tags argument must be an array of const having the form
          ['TAG1', 'VALUE1', 'TAG2', VALUE2', ....]
          Of course TAG1, TAG2,... and VALUE1, VALUE2,... can be replaced by
          appropriate variables.
          There is a pair of special tags in the template:
              <#TABLE_ROWS TABLENAME> and <#/TABLE_ROWS>
          When finding the first tag, AnswerPage search for the second one
          and repeatedly trigger the event OnGetRowData to get data for the
          other tags. The loop is controlled by the event handler "More"
          argument. This permit easy table insertion with a single table row
          defined in the template and repeated for each row.
          It is permiited to have <#TABLE_ROWS TABLENAME> and <#/TABLE_ROWS>
          pairs embedded to make tables inside tables.

          UserData argument is passed to the OnGetRowData as is. It is
          intended to pass any object to the event handler, for example the
          dataset which was used to query the data to populate the table.

          The Status argument is the HTTP answer status.
          Default value "200 OK" is used when Status is an empty string.

          The Header argument is used to build the HTTP header for the answer.
          You _must_ not add Content-Length nor Content-Type in the header
          because those two values are generated automatically by AnswerPage.
          You can use Header argument for cache control, cookies or anything
          else your application require.
        }
        procedure   AnswerPage(var   Flags    : THttpGetFlag;
                               const Status   : String;
                               const Header   : String;
                               const HtmlFile : String;
                               UserData       : TObject;
                               Tags           : array of const);
        procedure   AnswerStream(var   Flags      : THttpGetFlag;
                                 const Status   : String;
                                 const ContType : String;
                                 const Header   : String);
        procedure   AnswerString(var   Flags    : THttpGetFlag;
                                 const Status   : String;
                                 const ContType : String;
                                 const Header   : String;
                                 const Body     : String); virtual;
        { Mostly like AnswerPage but the result is given into a string.
          Designed to be used within a call to AnswerPage as one of the
          replacable tag value. This permit to build a page based on several
          templates. A main template given to AnswerPage and one or more
          templates given to HtmlPageProducerToString, resulting string begin
          used as tag value for the main template. Of course you can
          recursively use HtmlPageProducerToString to build complex pages. }
        function HtmlPageProducerToString(const HtmlFile: String;
                                          UserData: TObject;
                                          Tags: array of const): String;
        { Mostly like AnswerPage but the result is given into a stream }
        procedure HtmlPageProducerToStream(const HtmlFile: String;
                                           UserData: TObject;
                                           Tags: array of const;
                                           DestStream: TStream);
        { Method contains GET/POST/HEAD as requested by client }
        property Method                    : String read  FMethod;
        { Version contains HTTP version from client request }
        property Version                   : String read  FVersion;
        { The whole header as received from client }
        property RequestHeader             : TStringList
                                                    read  FRequestHeader;
        { Stream used to send reply to client }
        property DocStream                 : TStream
                                                     read  FDocStream
                                                     write FDocStream;
        { All RequestXXX are header fields from request header }
        property RequestContentLength  : Integer     read  FRequestContentLength;
        property RequestContentType    : String      read  FRequestContentType;
        property RequestAccept         : String      read  FRequestAccept;
        property RequestReferer        : String      read  FRequestReferer;
        property RequestAcceptLanguage : String      read  FRequestAcceptLanguage;
        property RequestAcceptEncoding : String      read  FRequestAcceptEncoding;
        property RequestUserAgent      : String      read  FRequestUserAgent;
        property RequestAuth           : String      read  FRequestAuth; {DAVID}
        property RequestCookies        : String      read  FRequestCookies;
        property RequestHost           : String      read  FRequestHost;
        property RequestHostName       : String      read  FRequestHostName;    {DAVID}
        property RequestHostPort       : String      read  FRequestHostPort;    {DAVID}
        property RequestConnection     : String      read  FRequestConnection;
        property RequestRangeValues    : THttpRangeList  read  FRequestRangeValues; {ANDREAS}
    published
        { Where all documents are stored. Default to c:\wwwroot }
        property DocDir         : String            read  FDocDir
                                                    write FDocDir;
        { Where all template documents are stored. Default to c:\wwwroot\templates }
        property TemplateDir    : String            read  FTemplateDir
                                                    write FTemplateDir;
        { Default document name. Default to index.html }
        property DefaultDoc     : String            read  FDefaultDoc
                                                    write FDefaultDoc;
        { Complete document path and file name on local file system }
        property Document       : String            read  FDocument
                                                    write FDocument;
        { Document path as requested by client }
        property Path           : String            read  FPath
                                                    write FPath;
        { Parameters in request (Question mark is separator) }
        property Params         : String            read  FParams
                                                    write FParams;
        { Selected HTTP server optional behaviour }
        property Options        : THttpOptions      read  FOptions
                                                    write FOptions;
        { Triggered when client sent GET request }
        property OnGetDocument  : THttpGetConnEvent read  FOnGetDocument
                                                    write FOnGetDocument;
        { Triggered when client sent HEAD request }
        property OnHeadDocument : THttpGetConnEvent read  FOnHeadDocument
                                                    write FOnHeadDocument;
        { Triggered when client sent POST request }
        property OnPostDocument : THttpGetConnEvent read  FOnPostDocument
                                                    write FOnPostDocument;
        { Triggered when client sent POST request and data is available }
        property OnPostedData   : TDataAvailable    read  FOnPostedData
                                                    write FOnPostedData;
        { Triggered when a HTTP-request is done; since a connection can
        be established as keep-alive, there could possibly be several request
        done }
        property OnHttpRequestDone : TNotifyEvent   read  FOnHttpRequestDone
                                                    write FOnHttpRequestDone;

        { Triggered before we process the HTTP-request }
        property OnBeforeProcessRequest : TNotifyEvent    {DAVID}
                                                    read  FOnBeforeProcessRequest
                                                    write FOnBeforeProcessRequest;

        { Triggered when doing a directory listing, for each entry. You
          can set Visible to FALSE to hide the entry, or even change anything
          in the data to fake the entry }
        property OnFilterDirEntry  : THttpFilterDirEntry
                                                    read  FOnFilterDirEntry
                                                    write FOnFilterDirEntry;
        { Triggered from AnswerPage when building a table, for each row }
        property OnGetRowData      : THttpGetRowDataEvent
                                                    read  FOnGetRowData
                                                    write FOnGetRowData;
    end;

    { This is the HTTP server component handling all HTTP connection }
    { service. Most of the work is delegated to a TWSocketServer     }
    THttpServer = class(TComponent)
    protected
        { FWSocketServer will handle all client management work }
        FWSocketServer      : TWSocketServer;
        FPort               : String;
        FAddr               : String;
        FMaxClients         : Integer;              {DAVID}
        FClientClass        : THttpConnectionClass;
        FDocDir             : String;
        FTemplateDir        : String;
        FDefaultDoc         : String;
        FLingerOnOff        : TSocketLingerOnOff;
        FLingerTimeout      : Integer;              { In seconds, 0 = disabled }
        FOptions            : THttpOptions;
        FOnServerStarted    : TNotifyEvent;
        FOnServerStopped    : TNotifyEvent;
        FOnClientConnect    : THttpConnectEvent;
        FOnClientDisconnect : THttpConnectEvent;
        FOnGetDocument      : THttpGetEvent;
        FOnHeadDocument     : THttpGetEvent;
        FOnPostDocument     : THttpGetEvent;
        FOnPostedData       : THttpPostedDataEvent;
        FOnHttpRequestDone  : THttpRequestDoneEvent;
        FOnBeforeProcessRequest: THttpBeforeProcessEvent;    {DAVID}
        FOnFilterDirEntry   : THttpFilterDirEntry;
        FListenBacklog      : Integer; {Bjørnar}
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure CreateSocket; virtual;
        procedure WSocketServerClientConnect(Sender : TObject;
                                             Client : TWSocketClient;
                                             Error  : Word); virtual;
        procedure WSocketServerClientCreate(Sender : TObject;
                                            Client : TWSocketClient);
        procedure WSocketServerClientDisconnect(Sender : TObject;
                                                Client : TWSocketClient;
                                                Error  : Word);
        procedure WSocketServerSessionClosed(Sender : TObject;
                                             Error  : Word);
        procedure WSocketServerChangeState(Sender : TObject;
                                           OldState, NewState : TSocketState);
        procedure TriggerServerStarted; virtual;
        procedure TriggerServerStopped; virtual;
        procedure TriggerClientConnect(Client : TObject; Error  : Word); virtual;
        procedure TriggerClientDisconnect(Client : TObject; Error : Word); virtual;
        procedure TriggerGetDocument(Sender     : TObject;
                                     var Flags  : THttpGetFlag); virtual;
        procedure TriggerHeadDocument(Sender     : TObject;
                                      var Flags  : THttpGetFlag); virtual;
        procedure TriggerPostDocument(Sender     : TObject;
                                      var Flags  : THttpGetFlag); virtual;
        procedure TriggerPostedData(Sender     : TObject;
                                    Error      : WORD); virtual;
        procedure TriggerHttpRequestDone(Client : TObject); virtual;
        procedure TriggerBeforeProcessRequest(Client : TObject); virtual; {DAVID}
        procedure TriggerFilterDirEntry(Sender   : TObject;
                                        Client   : TObject;
                                        DirEntry : THttpDirEntry); virtual;
        procedure SetPortValue(newValue : String);
        procedure SetAddr(newValue : String);
        procedure SetDocDir(const Value: String);
        function  GetClientCount : Integer;
        function  GetClient(nIndex : Integer) : THttpConnection;
        function  GetSrcVersion: String;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start; virtual;
        procedure   Stop; virtual;
        { Check  if a given object is one of our clients }
        function    IsClient(SomeThing : TObject) : Boolean;
        { Runtime readonly property which gives number of connected clients }
        property    ClientCount : Integer        read  GetClientCount;
        { Client[] give direct access to anyone of our clients }
        property    Client[nIndex : Integer] : THttpConnection
                                                 read  GetClient;
        { Runtime property which tell the component class which has to be }
        { instanciated to handle client connection                        }
        property    ClientClass : THttpConnectionClass
                                                    read  FClientClass
                                                    write FClientClass;
        property    WSocketServer : TWSocketServer  read  FWSocketServer
                                                    write FWSocketServer;
    published
        property ListenBacklog  : Integer           read  FListenBacklog
                                                    write FListenBacklog; {Bjørnar}
        { Component source version }
        property SrcVersion    : String          read GetSrcVersion;
        { We will listen to that port. Default to 80 for http service }
        property Port          : String          read  FPort
                                                 write SetPortValue;
        { We will use that interface to listen. 0.0.0.0 means all     }
        { available interfaces                                        }
        property Addr          : String          read  FAddr
                                                 write SetAddr;
        property MaxClients    : Integer         read  FMaxClients   {DAVID}
                                                 write FMaxClients;
        { Where all documents are stored. Default to c:\wwwroot }
        property DocDir        : String          read  FDocDir
                                                 write SetDocDir;
        { Where all template documents are stored. Default to c:\wwwroot\templates }
        property TemplateDir    : String            read  FTemplateDir
                                                    write FTemplateDir;
        { Default document name. Default to index.html }
        property DefaultDoc    : String          read  FDefaultDoc
                                                 write FDefaultDoc;
        property LingerOnOff   : TSocketLingerOnOff
                                                 read  FLingerOnOff
                                                 write FLingerOnOff;
        property LingerTimeout : Integer         read  FLingerTimeout
                                                 write FLingerTimeout;
        { Selected HTTP server optional behaviour }
        property Options        : THttpOptions      read  FOptions
                                                    write FOptions;
        { OnServerStrated is triggered when server has started listening }
        property OnServerStarted    : TNotifyEvent
                                                 read  FOnServerStarted
                                                 write FOnServerStarted;
        { OnServerStopped is triggered when server has stopped listening }
        property OnServerStopped    : TNotifyEvent
                                                 read  FOnServerStopped
                                                 write FOnServerStopped;
        { OnClientConnect is triggered when a client has connected }
        property OnClientConnect    : THttpConnectEvent
                                                 read  FOnClientConnect
                                                 write FOnClientConnect;
        { OnClientDisconnect is triggered when a client is about to }
        { disconnect.                                               }
        property OnClientDisconnect : THttpConnectEvent
                                                 read  FOnClientDisconnect
                                                 write FOnClientDisconnect;
        { OnGetDocument is triggered when a client sent GET request    }
        { You can either do nothing and let server handle all work, or }
        { you can build a document on the fly or refuse access.        }
        property OnGetDocument      : THttpGetEvent
                                                 read  FOnGetDocument
                                                 write FOnGetDocument;
        { OnGetDocument is triggered when a client sent HEAD request   }
        { You can either do nothing and let server handle all work, or }
        { you can build a document header on the fly or refuse access. }
        property OnHeadDocument     : THttpGetEvent
                                                 read  FOnHeadDocument
                                                 write FOnHeadDocument;
        { OnGetDocument is triggered when a client sent POST request   }
        { You have to tell if you accept data or not. If you accept,   }
        { you'll get OnPostedData event with incomming data.           }
        property OnPostDocument     : THttpGetEvent
                                                 read  FOnPostDocument
                                                 write FOnPostDocument;
        { On PostedData is triggered when client post data and you     }
        { accepted it from OnPostDocument event.                       }
        { When you've got all data, you have to build a reply to be    }
        { sent to client.                                              }
        property OnPostedData       : THttpPostedDataEvent
                                                 read  FOnPostedData
                                                 write FOnPostedData;
        property OnHttpRequestDone  : THttpRequestDoneEvent
                                                 read  FOnHttpRequestDone
                                                 write FOnHttpRequestDone;

        property OnBeforeProcessRequest : THttpBeforeProcessEvent  {DAVID}
                                                 read  FOnBeforeProcessRequest
                                                 write FOnBeforeProcessRequest;

        property OnFilterDirEntry   : THttpFilterDirEntry
                                                 read  FOnFilterDirEntry
                                                 write FOnFilterDirEntry;
    end;

    THttpDirEntry = class
        Visible   : Boolean;    { TRUE if the entry is to be shown in list  }
        Name      : String;
        SizeLow   : Cardinal;
        SizeHigh  : Cardinal;
        Year      : Integer;
        Month     : Integer;
        Day       : Integer;
        Hour      : Integer;
        Min       : Integer;
        Sec       : Integer;
        VolumeID  : Boolean;
        Directory : Boolean;
        ReadOnly  : Boolean;
        SysFile   : Boolean;
        Hidden    : Boolean;   { File is hidden, not the same as Visible !  }
    end;

    TStringIndex = class(TObject)
    protected
        FList : TStringList;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Add(const Key, Value : String);
        function  Find(const Key : String; var Value : String) : Boolean;
        function  Count : Integer;
        procedure Clear;
    end;

    TStringIndexObject = class(TObject)
    public
        Value : String;
        constructor Create(const Data : String);
    end;

    TTableRowDataGetter = procedure(const TableName : String;
                                    Row             : Integer;
                                    TagData         : TStringIndex;
                                    var More        : Boolean;
                                    UserData        : TObject);
    PTableRowDataGetter = ^TTableRowDataGetter;
    THttpSrvRowDataGetter = procedure(const TableName : String;
                                      Row             : Integer;
                                      TagData         : TStringIndex;
                                      var More        : Boolean;
                                      UserData        : TObject) of object;
    THttpSrvRowDataGetterUserData = class
    public
        Event    : THttpSrvRowDataGetter;
        UserData : TObject;
    end;


{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
    {$I HttpSrvIntfSsl.inc}
{$ENDIF}

{ Retrieve a single value by name out of an cookies string.                 }
function GetCookieValue(
    const CookieString : String;    { Cookie string from header line        }
    const Name         : String;    { Cookie name to look for               }
    var Value          : String)    { Where to put variable value           }
    : Boolean;                      { Found or not found that's the question}
{ Retrieve a single value by name out of an URL encoded data stream.        }
function ExtractURLEncodedValue(
    Msg       : PChar;             { URL Encoded stream                     }
    Name      : String;            { Variable name to look for              }
    var Value : String): Boolean;  { Where to put variable value            }
function UrlEncode(const S : String) : String;
function UrlDecode(const Url : String) : String;
function htoi2(value : PChar) : Integer;
function IsXDigit(Ch : char) : Boolean;
function XDigit(Ch : char) : Integer;
function FileDate(FileName : String) : TDateTime;
function RFC1123_Date(aDate : TDateTime) : String;
function DocumentToContentType(FileName : String) : String;
function TextToHtmlText(const Src : String) : String;
function TranslateChar(const Str: String; FromChar, ToChar: Char): String;
function UnixPathToDosPath(const Path: String): String;
function DosPathToUnixPath(const Path: String): String;
function IsDirectory(const Path : String) : Boolean;
function AbsolutisePath(const Path : String) : String;
function MakeCookie(const Name, Value : String;
                    Expires           : TDateTime;
                    const Path        : String) : String;
function HtmlPageProducer(const HtmlFileName : String;
                          Tags               : array of const;
                          RowDataGetter      : PTableRowDataGetter;
                          UserData           : TObject;
                          DestStream         : TStream) : Boolean;
function HtmlPageProducerFromMemory(
    Buf                : PChar;
    BufLen             : Integer;
    TagData            : TStringIndex;
    RowDataGetter      : PTableRowDataGetter;
    UserData           : TObject;
    DestStream         : TStream) : Boolean;
function HtmlPageProducerSetTagPrefix(const Value : String) : String;
function RemoveHtmlSpecialChars(const S : String) : String;

procedure Register;

const
    HttpConnectionStateName : array [THttpConnectionState] of String =
         ('hcRequest', 'hcHeader', 'hcPostedData');

implementation

const
    GTagPrefix : String = '#';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [THttpServer
{$IFDEF USE_SSL}
                                   ,TSslHttpServer
{$ENDIF}
                                   ]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    CreateSocket;
    FClientClass   := THttpConnection;
    FOptions       := [];
    FAddr          := '0.0.0.0';
    FPort          := '80';
    FMaxClients    := 0;                {DAVID}
    FListenBacklog := 5; {Bjørnar}
    FDefaultDoc    := 'index.html';
    FDocDir        := 'c:\wwwroot';
    FTemplateDir   := 'c:\wwwroot\templates';
    FLingerOnOff   := wsLingerNoSet;
    FLingerTimeout := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpServer.Destroy;
begin
    if Assigned(FWSocketServer) then begin
        FWSocketServer.Destroy;
        FWSocketServer := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called by destructor when child component is created or destroyed.        }
procedure THttpServer.Notification(
    AComponent : TComponent;
    Operation  : TOperation);
begin
    inherited Notification(AComponent, Operation);
    if Operation = opRemove then begin
        if AComponent = FWSocketServer then
            FWSocketServer := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.CreateSocket;
begin
    FWSocketServer := TWSocketServer.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Start the server. That is make FWSocketServer listening to the port.      }
procedure THttpServer.Start;
const
    BusyText = 'Server overloaded. Retry later.' + #13#10;
begin
    { Create a new FWSocketServer if needed }
    if not Assigned(FWSocketServer) then
        CreateSocket;
    { If already listening, then do nothing }
    if FWSocketServer.State = wsListening then
        Exit;
    { Pass all parameters to FWSocketServer and make it listen }
    FWSocketServer.ClientClass        := FClientClass;
    FWSocketServer.OnClientCreate     := WSocketServerClientCreate;
    FWSocketServer.OnClientConnect    := WSocketServerClientConnect;
    FWSocketServer.OnClientDisconnect := WSocketServerClientDisconnect;
    FWSocketServer.OnSessionClosed    := WSocketServerSessionClosed;
    FWSocketServer.OnChangeState      := WSocketServerChangeState;
    FWSocketServer.Banner             := '';
    FWSocketServer.Proto              := 'tcp';
    FWSocketServer.Port               := FPort;
    FWSocketServer.Addr               := FAddr;
    FWSocketServer.MaxClients         := FMaxClients;    {DAVID}
    FWSocketServer.ListenBacklog       := FListenBacklog; {Bjørnar}
    FWSocketServer.BannerTooBusy      :=
        'HTTP/1.0 503 Service Unavailable' + #13#10 +
        'Content-type: text/plain' + #13#10 +
        'Content-length: ' + IntToStr(Length(BusyText)) + #13#10#13#10 +
        BusyText;
    FWSocketServer.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.Stop;
var
    I : Integer;
begin
    if not Assigned(FWSocketServer) then
        Exit;
    FWSocketServer.Close;
    { Disconnect all clients }
    for I := FWSocketServer.ClientCount - 1 downto 0 do begin
        try
            FWSocketServer.Client[I].Abort;
        except
            { Ignore any exception here }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.SetPortValue(newValue : String);
begin
    if newValue = FPort then
        Exit;
    FPort := newValue;
    { If server is already listening, then stop it and restart it with      }
    { new port. Do not disconnect already connected clients.                }
    if Assigned(FWSocketServer) and
       (FWSocketServer.State = wsListening) then begin
        FWSocketServer.Close;
        Start;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.SetAddr(newValue : String);
begin
    if newValue = FAddr then
        Exit;
    FAddr := newValue;
    { If server is already listening, then stop it and restart it with      }
    { new Addr. Do not disconnect already connected clients.                }
    if Assigned(FWSocketServer) and
       (FWSocketServer.State = wsListening) then begin
        FWSocketServer.Close;
        Start;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.SetDocDir(const Value: String);
begin
    if (Value > '') and (Value[Length(Value)] = '\') then
        FDocDir := AbsolutisePath(Copy(Value, 1, Length(Value) - 1))
    else
        FDocDir := AbsolutisePath(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get function for ClientCount property. Just return value from             }
{ FWSocketServer.                                                           }
function THttpServer.GetClientCount;
begin
    if not Assigned(FWSocketServer) then
        Result := 0
    else
        Result := FWSocketServer.ClientCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get function for Client[] property. Just return value from                }
{ FWSocketServer.                                                           }
function THttpServer.GetClient(nIndex : Integer) : THttpConnection;
begin
    if not Assigned(FWSocketServer) then
        Result := nil
    else
        Result := THttpConnection(FWSocketServer.Client[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check is an object is one of our clients. Just return value from          }
{ FWSocketServer.                                                           }
function THttpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FWSocketServer) then
        Result := FALSE
    else
        Result := FWSocketServer.IsClient(SomeThing);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when state of server socket has changed.  }
{ We use it to trigger our OnServerStarted event.                           }
procedure THttpServer.WSocketServerChangeState(
    Sender : TObject;
    OldState, NewState : TSocketState);
begin
    if newState = wsListening then
        TriggerServerStarted;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.WSocketServerSessionClosed(
    Sender : TObject;
    Error  : Word);
begin
    TriggerServerStopped;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A new client component has been created                                   }
procedure THttpServer.WSocketServerClientCreate(
    Sender : TObject;
    Client : TWSocketClient);
begin
    Client.LingerOnOff                  := FLingerOnOff;
    Client.LingerTimeout                := FLingerTimeout;
    (Client as THttpConnection).Options := FOptions;
    {$IFDEF USE_SSL}
    if not (Client.Owner is TSslWSocketServer) then
        (Client as THttpConnection).SslEnable := FALSE;
    {$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A new client just connected. Setup things to handle his requests.         }
{ HTTP header is line oriented so we turn line mode on. We use LF as end of }
{ line character altough HTTP uses CR/LF pair as end of line, because many  }
{ Unix client do not respect standards and use single LF...                 }
{ HTTP is not interactive, so we turn line editing to false (faster).       }
procedure THttpServer.WSocketServerClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    THttpConnection(Client).FServer           := Self;
    THttpConnection(Client).LineMode          := TRUE;
    THttpConnection(Client).LineEdit          := FALSE;
    THttpConnection(Client).LineEnd           := #10;
    THttpConnection(Client).DocDir            := Self.DocDir;
    THttpConnection(Client).TemplateDir       := Self.TemplateDir;
    THttpConnection(Client).DefaultDoc        := Self.DefaultDoc;
    THttpConnection(Client).OnGetDocument     := TriggerGetDocument;
    THttpConnection(Client).OnHeadDocument    := TriggerHeadDocument;
    THttpConnection(Client).OnPostDocument    := TriggerPostDocument;
    THttpConnection(Client).OnPostedData      := TriggerPostedData;
    THttpConnection(Client).OnHttpRequestDone := TriggerHttpRequestDone;
    THttpConnection(Client).OnBeforeProcessRequest := TriggerBeforeProcessRequest; {DAVID}
    THttpConnection(Client).OnFilterDirEntry  := TriggerFilterDirEntry;
    TriggerClientConnect(Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A client is about to disconnect.                                          }
procedure THttpServer.WSocketServerClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    TriggerClientDisconnect(Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerServerStarted;
begin
    if Assigned(FOnServerStarted) then
        FOnServerStarted(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerServerStopped;
begin
    if Assigned(FOnServerStopped) then
        FOnServerStopped(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerClientConnect(
    Client : TObject;
    Error  : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerClientDisconnect(
    Client : TObject;
    Error  : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerGetDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnGetDocument) then
        FOnGetDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerHeadDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnHeadDocument) then
        FOnHeadDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerPostedData(Sender     : TObject;
                                        Error      : WORD);
begin
    if Assigned(FOnPostedData) then
        FOnPostedData(Self, Sender, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerPostDocument(
     Sender     : TObject;
     var Flags  : THttpGetFlag);
begin
    if Assigned(FOnPostDocument) then
        FOnPostDocument(Self, Sender, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerHTTPRequestDone(
    Client : TObject);
begin
    if Assigned(FOnHttpRequestDone) then
        FOnHttpRequestDone(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerBeforeProcessRequest(  {DAVID}
    Client : TObject);
begin
    if Assigned(FOnBeforeProcessRequest) then
        FOnBeforeProcessRequest(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpServer.TriggerFilterDirEntry(
    Sender   : TObject;
    Client   : TObject;
    DirEntry : THttpDirEntry);
begin
    if Assigned(FOnFilterDirEntry) then
        FOnFilterDirEntry(Self, Client, DirEntry);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpConnection.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    LineMode            := TRUE;
    LineEdit            := FALSE;
    LineEnd             := #10;
    FRequestHeader      := TStringList.Create;
    FState              := hcRequest;
    OnDataAvailable     := ConnectionDataAvailable;
    FRequestRangeValues := THttpRangeList.Create; {ANDREAS}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpConnection.Destroy;
begin
    if Assigned(FRequestHeader) then begin
        FRequestHeader.Free;
        FRequestHeader := nil;
    end;
    if Assigned(FDocStream) then begin
        FDocStream.Free;
        FDocStream := nil;
    end;
    if Assigned(FDocBuf) then begin
        FreeMem(FDocBuf, BufSize);
        FDocBuf := nil;
    end;
    if Assigned(FRequestRangeValues) then begin
        FRequestRangeValues.Free; {ANDREAS}
        FRequestRangeValues := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        { We *MUST* handle all exception to avoid application shutdown }
        try
            if Msg = WM_HTTP_DONE then
                WMHttpDone(MsgRec)
            else
                inherited WndProc(MsgRec);
        except
            on E:Exception do
                HandleBackGroundException(E);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.WMHttpDone(var msg: TMessage);
begin
     FState := hcRequest;
     TriggerHttpRequestDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called each time data is available from a client.       }
{ We use FState variable to keep track of the where we are in the http      }
{ protocol: request command, header line or posted data.                    }
procedure THttpConnection.ConnectionDataAvailable(Sender: TObject; Error : Word);
var
    Len     : Integer;
    I, J    : Integer;
begin
    { If we are in data state, then the application has to receive data }
    if FState = hcPostedData then begin
        if FAcceptPostedData and Assigned(FOnPostedData) then
            FOnPostedData(Self, Error)
        else
            { No one is willing data, received it and throw it away }
            FRcvdLine := ReceiveStr;
        Exit;
    end;
    { We use line mode. We will receive complete lines }
    FRcvdLine := ReceiveStr;
    { Remove trailing CR/LF }
    Len := Length(FRcvdLine);
    if (Len > 0) and (FRcvdLine[Len] = #10) then begin
        Dec(Len);
        if (Len > 0) and (FRcvdLine[Len] = #13) then
            Dec(Len);
        SetLength(FRcvdLine, Len);
    end;
    if FState = hcRequest then begin
        { We just start a new request. Initialize all header variables }
        FRequestContentType    := '';
        FRequestContentLength  := 0;
        FRequestContentType    := '';
        FRequestAccept         := '';
        FRequestReferer        := '';
        FRequestAcceptLanguage := '';
        FRequestAcceptEncoding := '';
        FRequestUserAgent      := '';
        FRequestAuth           := '';     {DAVID}
        FRequestCookies        := '';
        FRequestHost           := '';
        FRequestHostName       := '';     {DAVID}
        FRequestHostPort       := '';     {DAVID}
        FRequestConnection     := '';
        FDataSent              := 0;      {TURCAN}
        FDocSize               := 0;      {TURCAN}
        FRequestRangeValues.Clear;        {ANDREAS}
        FRequestHeader.Clear;
        { The line we just received is HTTP command, parse it  }
        ParseRequest;
        { Next lines will be header lines }
        FState := hcHeader;
        Exit;
    end;
    { We can comes here only in hcHeader state }
    if FRcvdLine = '' then begin
        { Last header line is an empty line. Then we enter data state }
        if FRequestContentLength <> 0 then    { Only if we have data  }
             FState := hcPostedData
        { With a GET method, we _never_ have any document        10/02/2004 }
        else if FMethod = 'GET' then                           { 10/02/2004 }
            FState := hcRequest;                               { 10/02/2004 }
        { We will process request before receiving data because application }
        { has to setup things to be able to receive posted data             }
        ProcessRequest;
        Exit;
    end;
    { We comes here for normal header line. Extract some interesting variables }
    I := Pos(':', FRcvdLine);
    if I > 0 then begin
        try
            repeat
                Inc(I);
            until (I > Length(FRcvdLine)) or (FRcvdLine[I] <> ' ');
            if StrLIComp(@FRcvdLine[1], 'content-type:', 13) = 0 then
                FRequestContentType := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'content-length:', 15) = 0 then
                FRequestContentLength := StrToInt(Copy(FRcvdLine, I, Length(FRcvdLine)))
            else if StrLIComp(@FRcvdLine[1], 'Accept:', 7) = 0 then
                FRequestAccept:= Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Referer:', 8) = 0 then
                FRequestReferer := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Accept-Language:', 16) = 0 then
                FRequestAcceptLanguage := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Accept-Encoding:', 16) = 0 then
                FRequestAcceptEncoding := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'User-Agent:', 11) = 0 then
                FRequestUserAgent := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Authorization:', 14) = 0 then {DAVID}
                FRequestAuth := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Cookie:', 7) = 0 then {DAVID}
                FRequestCookies := Copy(FRcvdLine, I, Length(FRcvdLine))
            else if StrLIComp(@FRcvdLine[1], 'Host:', 5) = 0 then begin
                FRequestHost := Copy(FRcvdLine, I, Length(FRcvdLine));
                J := Pos(':', FRequestHost); {DAVID}
                if J > 0 then begin
                    FRequestHostName := Copy(FRequestHost, 1, J - 1);
                    FRequestHostPort := Copy(FRequestHost, J + 1, 100);
                end
                else begin
                    FRequestHostName := FRequestHost;
                    FRequestHostPort := FServer.Port; { by default server port }
                end;
            end
            else if StrLIComp(@FRcvdLine[1], 'Connection:', 11) = 0 then
                FRequestConnection := Copy(FRcvdLine, I, Length(FRcvdLine))
            {ANDREAS}
            else if StrLIComp(@FRcvdLine[1], 'Range:', 6) = 0 then begin
                { Init the Byte-range object }
                RequestRangeValues.InitFromString(Trim(Copy(FRcvdLine, I,
                                                           Length(FRcvdLine))));
            end;
        except
            { Ignore any exception in parsing header line }
        end;
    end;
    FRequestHeader.Add(FRcvdLine);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Request is in FRcvdLine property.                                         }
{ Split it into FMethod, FPath, FVersion and parameters.                    }
procedure THttpConnection.ParseRequest;
var
    I, J : Integer;
begin
    I := 1;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FMethod := UpperCase(Copy(FRcvdLine, 1, I - 1));
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FPath := Copy(FRcvdLine, J, I - J);
    { Find parameters }
    J := Pos('?', FPath);
    if J <= 0 then
        FParams := ''
    else begin
        FParams := Copy(FPath, J + 1, Length(FPath));
        FPath   := Copy(FPath, 1, J - 1);
    end;
    Inc(I);
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] = ' ') do
        Inc(I);
    J := I;
    while (I <= Length(FRcvdLine)) and (FRcvdLine[I] <> ' ') do
        Inc(I);
    FVersion := Trim(UpperCase(Copy(FRcvdLine, J, I - J)));
    if FVersion = '' then
        FVersion := 'HTTP/1.0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RowDataGetterProc(
    const TableName : String;
    Row             : Integer;
    TagData         : TStringIndex;
    var More        : Boolean;
    UserData        : TObject);
var
    UD : THttpSrvRowDataGetterUserData;
begin
    UD := UserData as THttpSrvRowDataGetterUserData;
    if Assigned(UD.Event) then
        UD.Event(TableName, Row, TagData, More, UD.UserData)
    else
        More := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerGetRowData(
    const TableName : String;
    Row             : Integer;
    TagData         : TStringIndex;
    var More        : Boolean;
    UserData        : TObject);
begin
    if Assigned(FOnGetRowData) then
        FOnGetRowData(Self, TableName, Row, TagData, More, UserData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_ZLIB}
function zlibAllocMem(AppData: Pointer; Items, Size: Cardinal): Pointer;
begin
  GetMem(Result, Items*Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure zlibFreeMem(AppData, Block: Pointer);
begin
  FreeMem(Block);
end;

function CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise ECompressionError.Create('error');    {!!}
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.AnswerStream(
    var   Flags    : THttpGetFlag;
    const Status   : String;   { if empty, default to '200 OK'           }
    const ContType : String;   { if emtpy, default to text/html          }
    const Header   : String);  { Do not use Content-Length               }
{$IFDEF USE_ZLIB}
var
    Count : Integer;
{$ENDIF}
begin
    Flags := hgWillSendMySelf;
    if Status = '' then
        PutStringInSendBuffer(FVersion + ' 200 OK' + #13#10)
    else
        PutStringInSendBuffer(FVersion + ' ' + Status + #13#10);
    if ContType = '' then
        PutStringInSendBuffer('Content-Type: text/html' + #13#10)
    else
        PutStringInSendBuffer('Content-Type: ' + ContType + #13#10);

    if not Assigned(FDocStream) then
        PutStringInSendBuffer('Content-Length: 0' + #13#10)
    else begin
{$IFDEF USE_ZLIB}
        FReplyDeflate := (Pos('deflate', FRequestAcceptEncoding) > 0);
        if FReplyDeflate then begin
            PutStringInSendBuffer('Content-Encoding: deflate' + #13#10);
            FreeAndNil(FZDocStream);
            FreeAndNil(FCompressStream);
            FZDocStream     := TMemoryStream.Create;
            FCompressStream := TCompressionStream.Create(clDefault, FZDocStream);
            FDocStream.Seek(0, 0);
            while TRUE do begin
                Count := FDocStream.Read(FZBuffer, SizeOf(FZBuffer));
                if Count <= 0 then
                    break;
                FCompressStream.Write(FZBuffer, Count);
            end;
            FCompressStream.Free;
            FCompressStream := nil;
            FZDocStream.Seek(0, 0);
            FDocStream.Free;
            FDocStream := FZDocStream;
            FZDocStream := nil;
{
            FDecompressStream := TDecompressionStream.Create(FDocStream);
            while TRUE do begin
               Count := FDecompressStream.Read(FZBuffer, SizeOf(FZBuffer));
               if Count <= 0 then
                   break;
            end;
            FDecompressStream.Free;
            FDecompressStream := nil;
}
        end;
{$ENDIF}
        PutStringInSendBuffer('Content-Length: ' +
                              IntToStr(DocStream.Size) + #13#10);
    end;
    if Header <> '' then
        PutStringInSendBuffer(Header);
    PutStringInSendBuffer(#13#10);
    SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpConnection.HtmlPageProducerToString(
    const HtmlFile : String;
    UserData       : TObject;
    Tags           : array of const) : String;
var
    Stream : TMemoryStream;
begin
    Stream := TMemoryStream.Create;
    try
        HtmlPageProducerToStream(HtmlFile, UserData, Tags, Stream);
        SetLength(Result, Stream.Size);
        Stream.Seek(0, 0);
        Stream.Read(Result[1], Stream.Size);
    finally
        Stream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.HtmlPageProducerToStream(
    const HtmlFile : String;
    UserData       : TObject;
    Tags           : array of const;
    DestStream     : TStream);
var
    UD : THttpSrvRowDataGetterUserData;
begin
    UD := THttpSrvRowDataGetterUserData.Create;
    try
        UD.UserData := UserData;
        UD.Event    := Self.TriggerGetRowData;
        if FTemplateDir = '' then
            HtmlPageProducer(HtmlFile, Tags,
                             @RowDataGetterProc, UD, DestStream)
        else
            HtmlPageProducer(FTemplateDir + '\' + HtmlFile, Tags,
                             @RowDataGetterProc, UD, DestStream);
    finally
        UD.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.AnswerPage(
    var   Flags    : THttpGetFlag;
    const Status   : String;   { if empty, default to '200 OK'              }
    const Header   : String;   { Do not use Content-Length nor Content-Type }
    const HtmlFile : String;
    UserData       : TObject;
    Tags           : array of const);
begin
    DocStream.Free;
    DocStream := TMemoryStream.Create;
    HtmlPageProducerToStream(HtmlFile, UserData, Tags, DocStream);
    AnswerStream(Flags, Status, 'text/html', Header);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Build an answer and send it to the client.                                }
{ The header is automatically built but you can provide your own lines.     }
{ Add a CR/LF at the end of each of your header line but do not add an      }
{ empty line at the end of your header: it is added automatically.          }
{ NOTE: This method is not very good for Delphi 1 because Delphi 1 is       }
{ Limited to 255 character strings.                                         }
procedure THttpConnection.AnswerString(
    var   Flags    : THttpGetFlag;
    const Status   : String;   { if empty, default to '200 OK'              }
    const ContType : String;   { if empty, default to 'text/html'           }
    const Header   : String;   { Do not use Content-Length nor Content-Type }
    const Body     : String);  { Could be empty. No default.                }
begin
    DocStream.Free;
{$IFNDEF COMPILER3_UP}
    DocStream := TMemoryStream.Create;
    if Body <> '' then
        DocStream.Write(Body[1], Length(Body));
{$ELSE}
    DocStream := TStringStream.Create(Body);
{$ENDIF}
    AnswerStream(Flags, Status, ContType, Header);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.Answer404;
var
    Body : String;
begin
    Body := '<HTML><HEAD><TITLE>404 Not Found</TITLE></HEAD>' +
            '<BODY><H1>404 Not Found</H1>The requested URL ' +
            TextToHtmlText(FPath) +
            ' was not found on this server.<P></BODY></HTML>' + #13#10;
    SendHeader(FVersion + ' 404 Not Found' + #13#10 +
               'Content-Type: text/html' + #13#10 +
               'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
               #13#10);
    { Do not use AnswerString method because we don't want to use ranges }
    SendStr(Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.Answer403;
var
    Body : String;
begin
    Body := '<HTML><HEAD><TITLE>403 Forbidden</TITLE></HEAD>' +
            '<BODY><H1>403 Forbidden</H1>The requested URL ' +
            TextToHtmlText(FPath) +
            ' is Forbidden on this server.<P></BODY></HTML>' + #13#10;
    SendHeader(FVersion + ' 403 Forbidden' + #13#10 +
               'Content-Type: text/html' + #13#10 +
               'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
               #13#10);
    { Do not use AnswerString method because we don't want to use ranges }
    SendStr(Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.Answer401;
var
    Body : String;
begin
    Body := '<HTML><HEAD><TITLE>401 Unauthorized</TITLE></HEAD>' +
            '<BODY><H1>401 Unauthorized</H1>The requested URL ' +
            TextToHtmlText(FPath) +
            ' is Unauthorized on this server.<P></BODY></HTML>' + #13#10;
    SendHeader(FVersion + ' 401 Unauthorized' + #13#10 +
               'Content-Type: text/html' + #13#10 +
               'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
               'WWW-Authenticate: Basic Realm=' + FBasicRealm + #13#10 +
               #13#10);
    { Do not use AnswerString method because we don't want to use ranges }
    SendStr(Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ As its name implies...                                                    }
procedure THttpConnection.ProcessRequest;
var
    Status : Integer;
begin
    if Assigned(FOnBeforeProcessRequest) then
       FOnBeforeProcessRequest(Self);

    if FPath = '/' then
        FDocument := FDocDir
    else if (FPath <> '') and (FPath[1] = '/') then
        FDocument := AbsolutisePath(FDocDir +
                                    URLDecode(UnixPathToDosPath(FPath)))
    else
        FDocument := AbsolutisePath(FDocDir + '\' +
                                    URLDecode(UnixPathToDosPath(FPath)));

    if Length(FDocument) < Length(FDocDir) then
        Status := -1
    else if Length(FDocument) > Length(FDocDir) then
        Status := CompareText(Copy(FDocument, 1, Length(FDocDir) + 1),
                              FDocDir + '\')
    else
        Status := CompareText(FDocument + '\', FDocDir + '\');
    FOutsideFlag := (Status <> 0);

    { Check for default document }
    if (FDocument[Length(FDocument)] = '\') and
       (FileExists(FDocument + FDefaultDoc)) then
            FDocument := FDocument + FDefaultDoc
    else if IsDirectory(FDocument) and
       (FileExists(FDocument + '\' + FDefaultDoc)) then
            FDocument := FDocument + '\' + FDefaultDoc;

    if FMethod = 'GET' then
        ProcessGet
    else if FMethod = 'POST' then
        ProcessPost
    else if FMethod = 'HEAD' then
        ProcessHead
    else begin
        Answer404;
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerGetDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnGetDocument) then
        FOnGetDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerHeadDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnHeadDocument) then
        FOnHeadDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerPostDocument(var Flags : THttpGetFlag);
begin
    if Assigned(FOnPostDocument) then
        FOnPostDocument(Self, Flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerHttpRequestDone;
begin
    if Assigned(FOnHttpRequestDone) then
        FOnHttpRequestDone(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerBeforeProcessRequest;  {DAVID}
begin
    if Assigned(FOnBeforeProcessRequest) then
        FOnBeforeProcessRequest(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessPost;
var
    Flags : THttpGetFlag;
begin
    if FOutsideFlag and (not (hoAllowOutsideRoot in FOptions)) then
        Flags := hg403
    else
        Flags := hg404;
    FAcceptPostedData := FALSE;
    TriggerPostDocument(Flags);
    case Flags of
    hg401:
        begin
            Answer401;
            CloseDelayed;
        end;
    hg403:
        begin
            Answer403;
            CloseDelayed;
        end;
    hg404:
        begin
            Answer404;
            CloseDelayed;
        end;
    hgAcceptData:
        FAcceptPostedData := TRUE;
    else
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This method has to be called by main code when all posted data has been   }
{ received.                                                                 }
procedure THttpConnection.PostedDataReceived;
begin
    LineMode := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessHead;
var
    Flags : THttpGetFlag;
begin
    if FOutsideFlag and (not (hoAllowOutsideRoot in FOptions)) then
        Flags := hg403
    else if (hoAllowDirList in FOptions) and IsDirectory(FDocument) then
        Flags := hgSendDirList
    else
        Flags := hgSendDoc;
    TriggerHeadDocument(Flags);
    case Flags of
    hg403:
        begin
            Answer403;
            CloseDelayed;
        end;
    hg404:
        begin
            Answer404;
            CloseDelayed;
        end;
    hgSendDoc:
        begin
            if FileExists(FDocument) then
                SendDocument(httpSendHead)
            else begin
                Answer404;
                CloseDelayed;
            end;
        end;
    hgSendStream:
        SendStream;
    hgSendDirList:
        SendDirList(httpSendHead);
    hgWillSendMySelf:
        { Nothing to do };
    else
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.ProcessGet;
var
    Flags      : THttpGetFlag;
    TempStream : TFileStream;
    OK         : Boolean;
begin
    if FOutsideFlag and (not (hoAllowOutsideRoot in FOptions)) then
        Flags := hg403
    else if (hoAllowDirList in FOptions) and IsDirectory(FDocument) then
        Flags := hgSendDirList
    else
        Flags := hgSendDoc;

    TriggerGetDocument(Flags);
    case Flags of
    hg401:
        begin
            Answer401;
            CloseDelayed;
        end;
    hg403:
        begin
            Answer403;
            CloseDelayed;
        end;
    hg404:
        begin
            Answer404;
            CloseDelayed;
        end;
    hgSendDoc:
        begin
            OK := FALSE;
            try
                if not FileExists(FDocument) then begin
                    { File not found }
                    Answer404;
                    CloseDelayed;
                end
                else begin
                    TempStream := TFileStream.Create(FDocument, fmOpenRead + fmShareDenyWrite);
                    TempStream.Destroy;
                    OK := TRUE;
                end;
            except
                Answer404;
                CloseDelayed;
            end;
            if OK then
                SendDocument(httpSendDoc)
        end;
    hgSendStream:
        SendStream;
    hgSendDirList:
        SendDirList(httpSendDoc);
    hgWillSendMySelf:
        { Nothing to do };
    else
        CloseDelayed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DocumentToContentType(FileName : String) : String;
var
    Ext : String;
begin
    { We probably should use the registry to find MIME type for file types }
    Ext := LowerCase(ExtractFileExt(FileName));
    if Length(Ext) > 1 then
        Ext := Copy(Ext, 2, Length(Ext));
    if (Ext = 'htm') or (Ext = 'html') then
        Result := 'text/html'
    else if Ext = 'gif' then
        Result := 'image/gif'
    else if Ext = 'bmp' then
        Result := 'image/bmp'
    else if (Ext = 'jpg') or (Ext = 'jpeg') then
        Result := 'image/jpeg'
    else if Ext = 'txt' then
        Result := 'text/plain'
    else if Ext = 'css' then
        Result := 'text/css'
    else if Ext = 'wav' then
        Result := 'audio/x-wav'
    else if Ext = 'ico' then                       {DAVID}
        Result := 'image/x-icon'
    { WAP support }                                {DAVID}
    else if Ext = 'wml' then
        Result := 'text/vnd.wap.wml'
    else if Ext = 'wbmp' then
        Result := 'image/vnd.wap.wbmp'
    else if Ext = 'wmlc' then
        Result := 'application/vnd.wap.wmlc'
    else if Ext = 'wmlscript' then
        Result := 'text/vnd.wap.wmlscript'
    else if Ext = 'wmlscriptc' then
        Result := 'application/vnd.wap.wmlscriptc'
    else if Ext = 'pdf' then                        {ANDREAS Add PDF-Support}
        Result := 'application/pdf'
    else
        Result := 'application/binary';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ See also RFC822_DateTime function in SmtpCli component                    }
{ RFC1123 5.2.14 redefine RFC822 Section 5.                                 }
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
    DayOfWeek := ((Trunc(aDate) - 2) mod 7);
    Result := Copy(StrWeekDay, 1 + DayOfWeek * 3, 3) + ', ' +
              Format('%2.2d %s %4.4d %2.2d:%2.2d:%2.2d',
                     [Day, Copy(StrMonth, 1 + 3 * (Month - 1), 3),
                      Year, Hour, Min, Sec]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return document file date from document filename.                         }
{ Return 0 if file not found.                                               }
function FileDate(FileName : String) : TDateTime;
var
    SearchRec : TSearchRec;
    Status    : Integer;
begin
    Status := FindFirst(FileName, faAnyFile, SearchRec);
    try
        if Status <> 0 then
            Result := 0
        else
            Result := FileDateToDateTime(SearchRec.Time);
    finally
        FindClose(SearchRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ANDREAS Byte-range-separator (use the same as IIS) }
const
    ByteRangeSeparator = '[lka9uw3et5vxybtp87ghq23dpu7djv84nhls9p]';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ANDREAS Helperfunction to create the HTTP-Header }
function CreateHttpHeader(
    Version           : String;
    ProtoNumber       : Integer;
    AnswerContentType : String;
    RangeList         : THttpRangeList;
    DocSize           : THttpRangeInt;
    CompleteDocSize   : THttpRangeInt): String;
begin
    if ProtoNumber = 200 then
        Result := Version + ' 200 OK' + #13#10 +
                  'Content-Type: ' + AnswerContentType + #13#10 +
                  'Content-Length: ' + IntToStr(DocSize) + #13#10 +
                  'Accept-Ranges: bytes' + #13#10
    else if ProtoNumber = 416 then
        Result := Version + ' 416 Request range not satisfiable' + #13#10
    else if ProtoNumber = 206 then begin
        if RangeList.Count = 1 then begin
            Result := Version + ' 206 Partial Content' + #13#10 +
                      'Content-Type: ' + AnswerContentType + #13#10 +
                      'Content-Length: ' + IntToStr(DocSize) + #13#10 +
                      'Content-Range: bytes ' +
                      RangeList.Items[0].GetContentRangeString(CompleteDocSize) +
                      #13#10;
        end
        else begin
            Result := Version + ' 206 Partial Content' + #13#10 +
                      'Content-Type: multipart/byteranges; boundary=' +
                      ByteRangeSeparator + #13#10 +
                      'Content-Length: ' + IntToStr(DocSize) + #13#10;
        end;
    end
    else
        raise Exception.Create('Unexpected ProtoNumber in CreateHttpHeader');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ SendDocument will send FDocument file to remote client, build header and  }
{ sending data (if required)                                                }
procedure THttpConnection.SendDocument(SendType : THttpSendType);
var
    Header          : String;
    NewDocStream    : TStream;
    ProtoNumber     : Integer;
    CompleteDocSize : THttpRangeInt;
    ErrorSend       : Boolean;
begin
    ErrorSend          := FALSE;
    ProtoNumber        := 200;
    FLastModified      := FileDate(FDocument);
    FAnswerContentType := DocumentToContentType(FDocument);

    FDocStream.Free;
    FDocStream := TFileStream.Create(FDocument, fmOpenRead + fmShareDenyWrite);

    CompleteDocSize := FDocStream.Size;
    {ANDREAS Create the virtual 'byte-range-doc-stream', if we are ask for ranges}
    if RequestRangeValues.Valid then begin
        { NewDocStream will now be the owner of FDocStream -> don't free FDocStream }
        NewDocStream := RequestRangeValues.CreateRangeStream(
                        FDocStream, FAnswerContentType, CompleteDocSize);
        if Assigned(NewDocStream) then begin
            FDocStream := NewDocStream;
            FDocStream.Position := 0;
            ProtoNumber := 206;
        end
        else begin
            { Error in Range-definition }
            ProtoNumber := 416;
            ErrorSend   := True;
        end;
    end;
        FDocSize := FDocStream.Size;
    FDataSent := 0;       { will be incremented after each send part of data }
    { Seek to end of document because HEAD will not send actual document }
    if SendType = httpSendHead then
        FDocStream.Seek(0, soFromEnd);
    OnDataSent := ConnectionDataSent;
    { Create Header }

    {ANDREAS Create Header for the several protocols}
    Header := CreateHttpHeader(FVersion, ProtoNumber, FAnswerContentType, RequestRangeValues, FDocSize, CompleteDocSize);
    if FLastModified <> 0 then
        Header := Header +
                  'Last-Modified: ' + RFC1123_Date(FLastModified) +
                  ' GMT' + #13#10;

    { *DAVID* Revised this if/then/else }
    if FVersion = 'HTTP/1.0' then begin
        if CompareText(FRequestConnection, 'keep-alive') <> 0 then
            Header := Header + 'Connection: Close' + #13#10
        else
            Header := Header + 'Connection: Keep-Alive' + #13#10;
    end
    else if FVersion = 'HTTP/1.1' then begin
        if CompareText(FRequestConnection, 'close') = 0 then
            Header := Header + 'Connection: Close' + #13#10
        else
            Header := Header + 'Connection: Keep-Alive' + #13#10;
    end;

    Header := Header + #13#10;

    SendHeader(Header);
    if not ErrorSend then begin
      if FDocSize <= 0 then
          Send(nil, 0);
      if SendType = httpSendDoc then
          SendStream
      else
          Send(nil, 0); { Added 15/04/02 }
    end
    else
      Send(nil, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.SendHeader(Header : String);
begin
    PutStringInSendBuffer(Header);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.SendStream;
begin
    if not Assigned(FDocStream) then begin
        { No Stream to send ! Create an empty one to continue }
        FDocStream := TMemoryStream.Create;
    end
    else
        FDocStream.Seek(0, 0);        { Go to start of stream           }
    if not Assigned(FDocBuf) then
        GetMem(FDocBuf, BufSize);
    FDocSize   := FDocStream.Size;    { Should it take care of ranges ? }
    FDataSent  := 0;
    OnDataSent := ConnectionDataSent;
    ConnectionDataSent(Self, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.TriggerFilterDirEntry(DirEntry: THttpDirEntry);
begin
    if Assigned(FOnFilterDirEntry) then
        FOnFilterDirEntry(Self, Self, DirEntry);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpConnection.FormatDirEntry(F : THttpDirEntry) : String;
var
    Attr             : String;
    Link             : String;
    SizeString       : String;
const
    StrMonth : array [1..12] of String =
        ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
         'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    if F.VolumeID or
       (F.Name = '.') or
       (F.Name = '..') then begin
        { Ignore hidden files, volume ID, current and parent dir entries }
        Result := '';
        Exit;
    end;

    Attr := '-rw--';
    if F.Directory then begin
        Attr[1] := 'd';
        SizeString := '';
    end
    else
        SizeString := IntToStr(F.SizeLow);

    if F.ReadOnly then
        Attr[3] := '-';

    if F.SysFile then
        Attr[4] := 's';

    if F.Hidden then
        Attr[5] := 'h';

{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}

    if Path = '/' then
        Link := '/' + UrlEncode(F.Name)
    else if Path[Length(Path)] = '/' then
        Link := Path + UrlEncode(F.Name)
    else
        Link := Path + '/' + UrlEncode(F.Name);

    Result := '<TD>' + Attr + '</TD>' +
              '<TD ALIGN="right">' + SizeString + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD>' + Format('%s %2.2d, %4.4d', [StrMonth[F.Month], F.Day, F.Year]) + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD>' + Format('%2.2d:%2.2d:%2.2d',  [F.Hour, F.Min, F.Sec])   + '</TD>' +
              '<TD WIDTH="10"></TD>' +
              '<TD><A HREF="' + Link + '">' +
              TextToHtmlText(F.Name) + '</A></TD>' + #13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This feature is somewhat broken with Delphi 1 since strings are limited to  }
{ 255 characters ! Should replace String by TStream...                        }
function THttpConnection.BuildDirList : String;
var
    Status     : Integer;
    F          : TSearchRec;
    ParentDir  : String;
    DirList    : TStringList;
    FileList   : TStringList;
    Data       : THttpDirEntry;
    I          : Integer;
    Total      : Cardinal;
    TotalBytes : Cardinal;
begin
    { Create a list of all directories }
    DirList := TStringList.Create;
    Status  := FindFirst(Document + '\*.*', faAnyFile, F);
    while Status = 0 do begin
        if ((F.Attr and faDirectory) <> 0) and
           ((F.Attr and faVolumeID)  =  0) and
           (F.Name <> '.') and
           (F.Name <> '..') then begin
            Data           := THttpDirEntry.Create;
            Data.Visible   := TRUE;
            Data.Name      := F.Name;
            Data.SizeLow   := F.Size;
            Data.SizeHigh  := 0;
            Data.Day       := (HIWORD(F.Time) and $1F);
            Data.Month     := ((HIWORD(F.Time) shr 5) and $0F);
            Data.Year      := ((HIWORD(F.Time) shr 9) and $3F) + 1980;
            Data.Sec       := ((F.Time and $1F) shl 1);
            Data.Min       := ((F.Time shr 5) and $3F);
            Data.Hour      := ((F.Time shr 11) and $1F);
            Data.VolumeID  := ((F.Attr and faVolumeID)  <> 0);
            Data.Directory := ((F.Attr and faDirectory) <> 0);
            Data.ReadOnly  := ((F.Attr and faReadOnly)  <> 0);
            Data.SysFile   := ((F.Attr and faSysFile)   <> 0);
            Data.Hidden    := ((F.Attr and faHidden)    <> 0);
            TriggerFilterDirEntry(Data);
            if Data.Visible then
                DirList.AddObject(Data.Name, Data)
            else
                Data.Free;
        end;
        Status  := FindNext(F);
    end;
    FindClose(F);
    DirList.Sort;

    { Create a list of all files }
    FileList := TStringList.Create;
    Status  := FindFirst(Document + '\*.*', faAnyFile, F);
    while Status = 0 do begin
        if ((F.Attr and faDirectory) = 0) and
           ((F.Attr and faVolumeID)  = 0) then begin
            Data           := THttpDirEntry.Create;
            Data.Visible   := TRUE;
            Data.Name      := F.Name;
            Data.SizeLow   := F.Size;
            Data.SizeHigh  := 0;
            Data.Day       := (HIWORD(F.Time) and $1F);
            Data.Month     := ((HIWORD(F.Time) shr 5) and $0F);
            Data.Year      := ((HIWORD(F.Time) shr 9) and $3F) + 1980;
            Data.Sec       := ((F.Time and $1F) shl 1);
            Data.Min       := ((F.Time shr 5) and $3F);
            Data.Hour      := ((F.Time shr 11) and $1F);
            Data.VolumeID  := ((F.Attr and faVolumeID)  <> 0);
            Data.Directory := ((F.Attr and faDirectory) <> 0);
            Data.ReadOnly  := ((F.Attr and faReadOnly)  <> 0);
            Data.SysFile   := ((F.Attr and faSysFile)   <> 0);
            Data.Hidden    := ((F.Attr and faHidden)    <> 0);
            TriggerFilterDirEntry(Data);
            if Data.Visible then
                FileList.AddObject(Data.Name, Data)
            else
                Data.Free;
        end;
        Status  := FindNext(F);
    end;
    FindClose(F);
    FileList.Sort;

    Result   := '<HTML>' + #13#10 +
                '<HEAD>' + #13#10 +
                  '<STYLE TYPE="text/css">' + #13#10 +
                    '.dirline { font-family: arial; font-size: normal; color: black }' + #13#10 +
                  '</STYLE>' + #13#10 +
                  '<TITLE>Directory List</TITLE>' + #13#10 +
                '</HEAD>' + #13#10 +
              '<BODY><P>Directory of ' +
    TextToHtmlText(DosPathToUnixPath(AbsolutisePath(UnixPathToDosPath(Path)))) +
                       ':</P><P CLASS=dirline>' + #13#10 +
              '<TABLE>' + #13#10;
    if Path = '/' then
        ParentDir := ''
    else if Path[Length(Path)] = '/' then
        ParentDir := DosPathToUnixPath(ExtractFilePath(UnixPathToDosPath(Copy(Path, 1, Length(Path) - 1))))
    else
        ParentDir := DosPathToUnixPath(ExtractFilePath(UnixPathToDosPath(Path)));
    if (ParentDir <> '') and (ParentDir <> '/') then
        SetLength(ParentDir, Length(ParentDir) - 1);
    if ParentDir <> '' then
        Result  := Result + '<TR><TD><A HREF="' + ParentDir +
                         '">[To Parent Directrory]</A></TD></TR>';

    TotalBytes := 0;
    Total      := DirList.Count + FileList.Count;
    if Total <= 0 then
        Result := Result +'<TR><TD>No file</TD></TR>'
    else begin
        for I := 0 to DirList.Count - 1 do begin
            Data   := THttpDirEntry(DirList.Objects[I]);
            Result := Result + '<TR>' + FormatDirEntry(Data) + '</TR>' + #13#10;
            DirList.Objects[I].Free;
        end;
        DirList.Free;

        for I := 0 to FileList.Count - 1 do begin
            Data       := THttpDirEntry(FileList.Objects[I]);
            Result     := Result + '<TR>' + FormatDirEntry(Data) +
                                   '</TR>' + #13#10;
            TotalBytes := TotalBytes + Cardinal(Data.SizeLow);
            FileList.Objects[I].Free;
        end;
        FileList.Free;
        Result := Result + '<TR><TD COLSPAN="8">Total: ' +
                           IntToStr(Total)      + ' file(s), ' +
                           IntToStr(TotalBytes) + ' byte(s)</TD></TR>';
    end;

    Result := Result + '</TABLE></P></BODY></HTML>' + #13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpConnection.SendDirList(SendType : THttpSendType);
var
    Body      : String;
    Header    : String;
begin
    FDocStream.Free;
    FDocStream := nil;

    Body   := BuildDirList;
    Header := Version +  ' 200 OK' + #13#10 +
              'Content-Type: text/html' + #13#10 +
              'Content-Length: ' + IntToStr(Length(Body)) + #13#10 +
              'Pragma: no-cache' + #13#10 +
              #13#10;
    PutStringInSendBuffer(Header);
    FDocStream := TMemoryStream.Create;
    if SendType = httpSendDoc then
        FDocStream.Write(Body[1], Length(Body));
    FDocStream.Seek(0, 0);
    SendStream;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All data in TWSocket has been sent. Read next lock from stream and send.  }
{ When end of stream is reached, closed communication.                      }
procedure THttpConnection.ConnectionDataSent(Sender : TObject; Error : WORD);
var
    Count  : Integer;
    ToSend : Integer;
begin
    if not Assigned(FDocStream) then
        Exit; { End of file has been reached }

    if FDocSize <= 0 then
        Send(nil, 0);                         {Force send buffer flush}

    if FDataSent >= FDocSize then begin       {DAVID}
        { End of file found }
        if Assigned(FDocStream) then begin    {DAVID}
            FDocStream.Free;                  {DAVID}
            FDocStream := nil;                {DAVID}
{$IFDEF USE_ZLIB}

{$ENDIF}
        end;                                  {DAVID}

        { *DAVID* revised this if/then }
        if ((FVersion = 'HTTP/1.0') and
           (CompareText(FRequestConnection, 'keep-alive') <> 0)) or { Jack }
           (CompareText(FRequestConnection, 'close') = 0) then
            Shutdown(1);

        FState := hcRequest;
        PostMessage(Handle, WM_HTTP_DONE, 0, 0);
        Exit;
    end;

    { We have at least one byte to read.    }
    { Never read more than specified range. }
    ToSend := FDocSize - FDataSent;
    if ToSend > BufSize then
        ToSend := BufSize;

    Count     := FDocStream.Read(FDocBuf^, ToSend);
    FDataSent := FDataSent + Count;      { Count data which is sent         }
    if State = wsConnected then          { Be sure to be still connected... }
        Send(FDocBuf, Count);            { before actually send any data.   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function XDigit(Ch : Char) : Integer;
begin
    if Ch in ['0'..'9'] then
        Result := Ord(Ch) - Ord('0')
    else
        Result := (Ord(Ch) and 15) + 9;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsXDigit(Ch : Char) : Boolean;
begin
    Result := (Ch in ['0'..'9']) or (Ch in ['a'..'f']) or (Ch in ['A'..'F']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoin(Value : PChar; Len : Integer) : Integer;
var
    I : Integer;
begin
    Result := 0;
    I      := 0;
    while (I < Len) and (Value[I] = ' ') do
        I := I + 1;
    while (I < len) and (IsXDigit(Value[I])) do begin
        Result := Result * 16 + XDigit(Value[I]);
        I := I + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function htoi2(Value : PChar) : Integer;
begin
    Result := htoin(Value, 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Retrieve a single value by name out of an URL encoded data stream         }
{ In the stream, every space is replaced by a '+'. The '%' character is     }
{ an escape character. The next two are 2 digits hexadecimal codes ascii    }
{ code value. The stream is constitued by name=value couples separated      }
{ by a single '&' character. The special characters are coded by the '%'    }
{ followed by hex-ascii character code.                                     }
function ExtractURLEncodedValue(
    Msg       : PChar;        { URL Encoded stream                     }
    Name      : String;       { Variable name to look for              }
    var Value : String)       { Where to put variable value            }
    : Boolean;                { Found or not found that's the question }
var
    NameLen  : Integer;
    FoundLen : Integer; {tps}
    Ch       : Char;
    P, Q     : PChar;
begin
    Result  := FALSE;
    Value   := '';
    if Msg = nil then         { Empty source }
        Exit;

    NameLen := Length(Name);

    P := Msg;
    while P^ <> #0 do begin
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        FoundLen := P - Q; {tps}
        if P^ = '=' then
            Inc(P);
        if (StrLIComp(Q, @Name[1], NameLen) = 0) and
           (NameLen = FoundLen) then begin  {tps}
            while (P^ <> #0) and (P^ <> '&') do begin
                Ch := P^;
                if Ch = '%' then begin
                    Ch := chr(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                Value := Value + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
         end;
         while (P^ <> #0) and (P^ <> '&') do
             Inc(P);
        if P^ = '&' then
            Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetCookieValue(
    const CookieString : String;   { Cookie string from header line         }
    const Name         : String;   { Cookie name to look for                }
    var   Value        : String)   { Where to put variable value            }
    : Boolean;                     { Found or not found that's the question }
var
    NameLen : Integer;
    Ch      : Char;
    P, Q    : PChar;
begin
    Value   := '';
    Result  := FALSE;

    if (CookieString = '') or (Name = '') then
        Exit;

    NameLen := Length(Name);
    P := @CookieString[1];
    while P^ <> #0 do begin
        while (P^ <> #0) and (P^ = ' ') do
            Inc(P);
        Q := P;
        while (P^ <> #0) and (P^ <> '=') do
            Inc(P);
        if P^ = '=' then
            Inc(P);
        if StrLIComp(Q, @Name[1], NameLen) = 0 then begin
            while (P^ <> #0) and (P^ <> ';') do begin
                Ch := P^;
                if Ch = '%' then begin
                    Ch := chr(htoi2(P + 1));
                    Inc(P, 2);
                end
                else if Ch = '+' then
                    Ch := ' ';
                Value := Value + Ch;
                Inc(P);
            end;
            Result := TRUE;
            break;
        end;
        while (P^ <> #0) and (P^ <> ';') do
            Inc(P);
        if P^ = ';' then
            Inc(P);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert a string in Windows character set to HTML texte. That is replace  }
{ all character with code between 160 and 255 by special sequences.         }
{ For example, 'fête' is replaced by 'f&ecirc;te'                           }
{ Also handle '<', '>', quote and double quote                              }
{ Replace multiple spaces by a single space followed by the required number }
{ of non-breaking-spaces (&nbsp;)                                           }
{ Replace TAB by a non-breaking-space.                                      }
function TextToHtmlText(const Src : String) : String;
const
    HtmlSpecialChars : array [160..255] of String[6] = (
        'nbsp'   , { #160 no-break space = non-breaking space               }
        'iexcl'  , { #161 inverted exclamation mark                         }
        'cent'   , { #162 cent sign                                         }
        'pound'  , { #163 pound sign                                        }
        'curren' , { #164 currency sign                                     }
        'yen'    , { #165 yen sign = yuan sign                              }
        'brvbar' , { #166 broken bar = broken vertical bar,                 }
        'sect'   , { #167 section sign                                      }
        'uml'    , { #168 diaeresis = spacing diaeresis                     }
        'copy'   , { #169 copyright sign                                    }
        'ordf'   , { #170 feminine ordinal indicator                        }
        'laquo'  , { #171 left-pointing double angle quotation mark         }
        'not'    , { #172 not sign                                          }
        'shy'    , { #173 soft hyphen = discretionary hyphen,               }
        'reg'    , { #174 registered sign = registered trade mark sign,     }
        'macr'   , { #175 macron = spacing macron = overline = APL overbar  }
        'deg'    , { #176 degree sign                                       }
        'plusmn' , { #177 plus-minus sign = plus-or-minus sign,             }
        'sup2'   , { #178 superscript two = superscript digit two = squared }
        'sup3'   , { #179 superscript three = superscript digit three = cubed }
        'acute'  , { #180 acute accent = spacing acute,                     }
        'micro'  , { #181 micro sign                                        }
        'para'   , { #182 pilcrow sign = paragraph sign,                    }
        'middot' , { #183 middle dot = Georgian comma = Greek middle dot    }
        'cedil'  , { #184 cedilla = spacing cedilla                         }
        'sup1'   , { #185 superscript one = superscript digit one           }
        'ordm'   , { #186 masculine ordinal indicator,                      }
        'raquo'  , { #187 right-pointing double angle quotation mark = right pointing guillemet }
        'frac14' , { #188 vulgar fraction one quarter = fraction one quarter}
        'frac12' , { #189 vulgar fraction one half = fraction one half      }
        'frac34' , { #190 vulgar fraction three quarters = fraction three quarters }
        'iquest' , { #191 inverted question mark = turned question mark     }
        'Agrave' , { #192 latin capital letter A with grave = latin capital letter A grave, }
        'Aacute' , { #193 latin capital letter A with acute,                }
        'Acirc'  , { #194 latin capital letter A with circumflex,           }
        'Atilde' , { #195 latin capital letter A with tilde,                }
        'Auml'   , { #196 latin capital letter A with diaeresis,            }
        'Aring'  , { #197 latin capital letter A with ring above = latin capital letter A ring, }
        'AElig'  , { #198 latin capital letter AE = latin capital ligature AE, }
        'Ccedil' , { #199 latin capital letter C with cedilla,              }
        'Egrave' , { #200 latin capital letter E with grave,                }
        'Eacute' , { #201 latin capital letter E with acute,                }
        'Ecirc'  , { #202 latin capital letter E with circumflex,           }
        'Euml'   , { #203 latin capital letter E with diaeresis,            }
        'Igrave' , { #204 latin capital letter I with grave,                }
        'Iacute' , { #205 latin capital letter I with acute,                }
        'Icirc'  , { #206 latin capital letter I with circumflex,           }
        'Iuml'   , { #207 latin capital letter I with diaeresis,            }
        'ETH'    , { #208 latin capital letter ETH                          }
        'Ntilde' , { #209 latin capital letter N with tilde,                }
        'Ograve' , { #210 latin capital letter O with grave,                }
        'Oacute' , { #211 latin capital letter O with acute,                }
        'Ocirc'  , { #212 latin capital letter O with circumflex,           }
        'Otilde' , { #213 latin capital letter O with tilde,                }
        'Ouml'   , { #214 latin capital letter O with diaeresis,            }
        'times'  , { #215 multiplication sign                               }
        'Oslash' , { #216 latin capital letter O with stroke = latin capital letter O slash, }
        'Ugrave' , { #217 latin capital letter U with grave,                }
        'Uacute' , { #218 latin capital letter U with acute,                }
        'Ucirc'  , { #219 latin capital letter U with circumflex,           }
        'Uuml'   , { #220 latin capital letter U with diaeresis,            }
        'Yacute' , { #221 latin capital letter Y with acute,                }
        'THORN'  , { #222 latin capital letter THORN,                       }
        'szlig'  , { #223 latin small letter sharp s = ess-zed,             }
        'agrave' , { #224 latin small letter a with grave = latin small letter a grave, }
        'aacute' , { #225 latin small letter a with acute,                  }
        'acirc'  , { #226 latin small letter a with circumflex,             }
        'atilde' , { #227 latin small letter a with tilde,                  }
        'auml'   , { #228 latin small letter a with diaeresis,              }
        'aring'  , { #229 latin small letter a with ring above = latin small letter a ring, }
        'aelig'  , { #230 latin small letter ae = latin small ligature ae   }
        'ccedil' , { #231 latin small letter c with cedilla,                }
        'egrave' , { #232 latin small letter e with grave,                  }
        'eacute' , { #233 latin small letter e with acute,                  }
        'ecirc'  , { #234 latin small letter e with circumflex,             }
        'euml'   , { #235 latin small letter e with diaeresis,              }
        'igrave' , { #236 latin small letter i with grave,                  }
        'iacute' , { #237 latin small letter i with acute,                  }
        'icirc'  , { #238 latin small letter i with circumflex,             }
        'iuml'   , { #239 latin small letter i with diaeresis,              }
        'eth'    , { #240 latin small letter eth                            }
        'ntilde' , { #241 latin small letter n with tilde,                  }
        'ograve' , { #242 latin small letter o with grave,                  }
        'oacute' , { #243 latin small letter o with acute,                  }
        'ocirc'  , { #244 latin small letter o with circumflex,             }
        'otilde' , { #245 latin small letter o with tilde,                  }
        'ouml'   , { #246 latin small letter o with diaeresis,              }
        'divide' , { #247 division sign                                     }
        'oslash' , { #248 latin small letter o with stroke, = latin small letter o slash, }
        'ugrave' , { #249 latin small letter u with grave,                  }
        'uacute' , { #250 latin small letter u with acute,                  }
        'ucirc'  , { #251 latin small letter u with circumflex,             }
        'uuml'   , { #252 latin small letter u with diaeresis,              }
        'yacute' , { #253 latin small letter y with acute,                  }
        'thorn'  , { #254 latin small letter thorn,                         }
        'yuml');   { #255 latin small letter y with diaeresis,              }
var
    I, J : Integer;
    Sub  : String;
begin
    Result := '';
    I := 1;
    while I <= Length(Src) do begin
        J   := I;
        Sub := '';
        while (I <= Length(Src)) and (Ord(Src[I]) < Low(HtmlSpecialChars)) do begin
            case Src[I] of
            ' '  : begin
                       if (I > 1) and (Src[I - 1] = ' ') then begin
                           { Replace multiple spaces by &nbsp; }
                           while (I <= Length(Src)) and (Src[I] = ' ') do begin
                               Sub := Sub + '&nbsp;';
                               Inc(I);
                           end;
                           Dec(I);
                       end
                       else
                           Inc(I);
                   end;
            '<'  : Sub := '&lt;';
            '>'  : Sub := '&gt;';
            '''' : sub := '&#39;';
            '"'  : Sub := '&#34;';
            '&'  : Sub := '&amp;';
            #9   : Sub := '&nbsp;';
            #10  : Sub := #10'<BR>';
            else
                Inc(I);
            end;
            if Length(Sub) > 0 then begin
                Result := Result + Copy(Src, J, I - J) + Sub;
                Inc(I);
                J      := I;
                Sub    := '';
            end;
        end;

        if I > Length(Src) then begin
            Result := Result + Copy(Src, J, I - J);
            Exit;
        end;
        Result := Result + Copy(Src, J, I - J) + '&' +
                  HtmlSpecialChars[Ord(Src[I])] + ';';
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TranslateChar(const Str: String; FromChar, ToChar: Char): String;
var
    I : Integer;
begin
    Result := Str;
    for I := 1 to Length(Result) do
      if Result[I] = FromChar then
          Result[I] := ToChar;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UnixPathToDosPath(const Path: String): String;
begin
    Result := TranslateChar(Path, '/', '\');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DosPathToUnixPath(const Path: String): String;
begin
    Result := TranslateChar(Path, '\', '/');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function RemoveHtmlSpecialChars(const S : String) : String;
const
    SpecialChars : array [1..5] of char   = ('<',  '>',  '&',   '''',  '"');
    HtmlChars    : array [1..5] of String = ('lt', 'gt', 'amp', '#39', 'quot');
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := Low(SpecialChars);
        while J <= High(SpecialChars) do begin
            if S[I] = SpecialChars[J] then
                break;
            J := J + 1;
        end;
        if J <= High(SpecialChars) then
            Result := Result + '&' + HtmlChars[J] + ';'
        else
            Result := Result + S[I];
        I := I + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDirectory(const Path : String) : Boolean;
var
    SR       : TSearchRec;
begin
    if Path = '' then begin
        Result := FALSE;
        Exit;
    end;
    if Path[Length(Path)] = '\' then begin
        if FindFirst(Copy(Path, 1, Length(Path) - 1),
                     faDirectory or faAnyFile, SR) = 0 then
            Result := ((SR.Attr and faDirectory) <> 0)
        else
            Result := FALSE;
    end
    else begin
        if FindFirst(Path, faDirectory or faAnyFile, SR) = 0 then
            Result := ((SR.Attr and faDirectory) <> 0)
        else
            Result := FALSE;
    end;
    FindClose(SR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function AbsolutisePath(const Path : String) : String;
var
    I, J, N : Integer;
begin
    if (Path = '') or (Path = '.') or  (Path = '..') then begin
        Result := '';
        Exit;
    end;

    Result := Path;
    N      := 0;
    if (Length(Result) > 2) and
       (Copy(Result, Length(Result) - 1, 2) = '\.') then
       Result := Copy(Result, 1, Length(Result) - 2);

    if Length(Result) > 1 then begin
       if (Result[1] = '\') and (Result[2] = '\') then begin
            N := 2;
            while (N < Length(Result)) and (Result[N + 1] <> '\') do
                Inc(N);
       end
       else if Result[2] = ':' then
           N := 2;
    end;

    if (Copy(Result, N + 1, 5) = '\') or
       (Copy(Result, N + 1, 5) = '\.') then begin
       Result := Copy(Result, 1, N + 1);
       Exit;
    end;

    while TRUE do begin
        I := Pos('\.\', Result);
        if I <= N then
            break;
        Delete(Result, I, 2);
    end;
    while TRUE do begin
        I := Pos('\..', Result);
        if I <= N then
            break;
        J := I - 1;
        while (J > N) and (Result[J] <> '\') do
            Dec(J);
        if J <= N then
            Delete(Result, J + 2, I - J + 2)
        else
            Delete(Result, J, I - J + 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MakeCookie(
    const Name, Value : String;
    Expires           : TDateTime;
    const Path        : String) : String;
begin
    Result := 'Set-Cookie: ' + Name + '=' + UrlEncode(Value);
    if Length(Value) = 0 then
        Result := Result + '_NONE_; EXPIRES=' + RFC1123_Date(Date - 7) { Last week }
    else if Expires <> 0 then
        Result := Result + '; EXPIRES=' + RFC1123_Date(Expires);
    Result := Result + '; PATH=' + Path + #13#10;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlEncode(const S : String) : String;
var
    I : Integer;
begin
    Result := '';
    for I := 1 to Length(S) do begin
        if S[I] in ['0'..'9', 'A'..'Z', 'a'..'z', '.'] then
            Result := Result + S[I]
        else
            Result := Result + '%' + IntToHex(Ord(S[I]), 2);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UrlDecode(const Url : String) : String;
var
    I, J, K, L : Integer;
begin
    Result := Url;
    L      := Length(Result);
    I      := 1;
    K      := 1;
    while TRUE do begin
        J := I;
        while (J <= Length(Result)) and (Result[J] <> '%') do begin
            if J <> K then
                Result[K] := Result[J];
            Inc(J);
            Inc(K);
        end;
        if J > Length(Result) then
            break;                   { End of string }
        if J > (Length(Result) - 2) then begin
            while J <= Length(Result) do begin
                Result[K] := Result[J];
                Inc(J);
                Inc(K);
            end;
            break;
        end;
        Result[K] := Char(htoi2(@Result[J + 1]));
        Inc(K);
        I := J + 3;
        Dec(L, 2);
    end;
    SetLength(Result, L);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpServer.GetSrcVersion: String;
begin
    Result := Format('%d.%02.2d', [THttpServerVersion div 100,
                                   THttpServerVersion mod 100]);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TStringIndex.Add(const Key, Value: String);
begin
    if not Assigned(Flist) then
        Exit;
    FList.AddObject(Key, TStringIndexObject.Create(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TStringIndex.Find(const Key: String; var Value: String): Boolean;
var
    Index : Integer;
begin
    if not Assigned(Flist) then begin
        Result := FALSE;
        Exit;
    end;
    Index := FList.IndexOf(Key);
    Result := Index >= 0;
    if Result then
        Value := TStringIndexObject(FList.Objects[Index]).Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TStringIndex.Clear;
var
    I : Integer;
begin
    if not Assigned(Flist) then
        Exit;
    for I := FList.Count - 1 downto 0 do
        FList.Objects[I].Free;
    FList.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TStringIndex.Create;
begin
    inherited Create;
    FList               := TStringList.Create;
{$IFDEF DELPHI6_UP}
    FList.CaseSensitive := FALSE;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TStringIndex.Destroy;
begin
    Clear;
    if Assigned(FList) then begin
        FList.Free;
        FList := nil;
    end;
    inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TStringIndexObject.Create(const Data: String);
begin
    Value := Data;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TStringIndex.Count: Integer;
begin
    if not Assigned(Flist) then
        Result := 0
    else
        Result := FList.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WriteStream(
    DestStream : TStream;
    const S    : String);
begin
    if Length(S) > 0 then
        DestStream.Write(S[1], Length(S));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WriteLnStream(
    DestStream : TStream;
    const S    : String);
begin
    WriteStream(DestStream, S + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SearchTableRowsEnd(
    Buf    : PChar;
    BufLen : Integer) : PChar;
var
    I : Integer;
    Q : PChar;
    StartTag : String;
    End1Tag  : String;
    End2Tag  : String;
begin
    StartTag := '<'  + GTagPrefix + 'TABLE_ROWS';
    End1Tag  := '</' + GTagPrefix + 'TABLE_ROWS';
    End2Tag  := '<'  + GTagPrefix + '/TABLE_ROWS'; { Old version still supported }
    I := 0;
    while I < (BufLen - 13) do begin
        if (StrLIcomp(Buf + I, @StartTag[1], Length(StartTag)) = 0) and
           ((Buf + I + Length(StartTag))^ in [' ', '>', #9]) then begin
            { Embedded TABLE_ROWS ! }
            while (I < (BufLen - 1)) and (Buf[I] <> '>') do
                Inc(I);
            Q := SearchTableRowsEnd(Buf + I + 1, BufLen - I - 1);
            I := Q - Buf;
        end
        else if (StrLIcomp(Buf + I, @End1Tag[1], Length(End1Tag)) = 0) and
                ((Buf + I + Length(End1Tag))^ in [' ', '>', #9]) then begin
            I := I + Length(End1Tag);
            while (I < BufLen) and (Buf[I] <> '>') do
                Inc(I);
            Result := Buf + I + 1;
            Exit;
        end
        else if (StrLIcomp(Buf + I, @End2Tag[1], Length(End2Tag)) = 0) and
                ((Buf + I + Length(End2Tag))^ in [' ', '>', #9]) then begin
            I := I + Length(End2Tag);
            while (I < BufLen) and (Buf[I] <> '>') do
                Inc(I);
            Result := Buf + I + 1;
            Exit;
        end;
        Inc(I);
    end;
    Result := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function SearchTableRowsEnd(
    Buf    : PChar;
    BufLen : Integer) : PChar;
var
    I : Integer;
    Q : PChar;
begin
    I := 0;
    while I < (BufLen - 13) do begin
        if StrLIcomp(Buf + I, '<#TABLE_ROWS', 12) = 0 then begin
            { Embedded TABLE_ROWS ! }
            while (I < (BufLen - 1)) and (Buf[I] <> '>') do
                Inc(I);
            Q := SearchTableRowsEnd(Buf + I + 1, BufLen - I - 1);
            I := Q - Buf;
        end
        else if StrLIcomp(Buf + I, '<#/TABLE_ROWS', 13) = 0 then begin
            I := I + 13;
            while (I < BufLen) and (Buf[I] <> '>') do
                Inc(I);
            Result := Buf + I + 1;
            Exit;
        end;
        Inc(I);
    end;
    Result := nil;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure HandleTableRow(
    TableName          : String;
    Buf                : PChar;
    BufLen             : Integer;
    RowDataGetter      : PTableRowDataGetter;
    UserData           : TObject;
    DestStream         : TStream);
var
    More    : Boolean;
    TagData : TStringIndex;
    Row     : Integer;
begin
    More := FALSE;
    if Assigned(RowDataGetter) then begin
        TagData := TStringIndex.Create;
        try
            Row := 0;
            while TRUE do begin
                Inc(Row);
                More := FALSE;
                TTableRowDataGetter(RowDataGetter)(TableName, Row, TagData,
                                                   More, UserData);
                if not More then
                    break;
                HtmlPageProducerFromMemory(Buf, BufLen, TagData,
                                           RowDataGetter, UserData,
                                           DestStream);
                TagData.Clear;
            end;
        finally
            TagData.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HtmlPageProducerSetTagPrefix(const Value : String) : String;
begin
    Result     := GTagPrefix;
    GTagPrefix := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HtmlPageProducerFromMemory(
    Buf                : PChar;
    BufLen             : Integer;
    TagData            : TStringIndex;
    RowDataGetter      : PTableRowDataGetter;
    UserData           : TObject;
    DestStream         : TStream) : Boolean;
const
    MAX_BUF  = 50;
var
    I, J      : Integer;
    TagName   : String;
    TagParams : String;
    TagValue  : String;
    P, Q      : PChar;
    Cnt       : Integer;
begin
    Result := FALSE;
    if (not Assigned(DestStream)) then
        Exit;
    if (Buf = nil) or (BufLen <= 0) then
        Exit;

    P   := Buf;
    Cnt := BufLen;
    while TRUE do begin
        { Search starting delimiter }
        I := 0;
        repeat
            while (I < (Cnt - Length(GTagPrefix))) and ((P[I] <> '<') or (P[I + 1] <> GTagPrefix[1])) do
                Inc(I);
            Inc(I);
        until (I >= (Cnt - Length(GTagPrefix))) or
              (StrLIComp(P + I, @GTagPrefix[1], Length(GTagPrefix)) = 0);

        Dec(I);
        if P[I] <> '<' then begin
            { No starting tag found, write source to destination }
            DestStream.Write(P^, Cnt);
            break;
        end;

        { Delimiter found
          Write from source to destination until start tag }
        if I > 0 then
            DestStream.Write(P^, I);

        { Search ending delimiter }
        J := I;
        while (J < Cnt) and (P[J] <> '>') and (P[J] <> ' ') and (P[J] <> #9) do
            Inc(J);

{$IFDEF VER80}
        Move(P[I + 3], TagName[1], J - I - 2);
        TagName[0] := Char(J - I - 2);
{$ELSE}
        TagName := UpperCase(Copy(P, I + Length(GTagPrefix) + 2, J - I - Length(GTagPrefix) - 1));
{$ENDIF}
        if P[J] = '>' then
            TagParams := ''
        else begin
            I := J + 1;
            while (J < Cnt) and (P[J] <> '>') do
                Inc(J);
{$IFDEF VER80}
            Move(P[I], TagParams[1], J - I + 1);
            TagParams[0] := Char(J - I + 1);
{$ELSE}
            TagParams := Trim(UpperCase(Copy(P, I, J - I + 1)));
{$ENDIF}
        end;

        if TagName = 'TABLE_ROWS' then begin
            Q := SearchTableRowsEnd(P + J + 1, Cnt - J - 1);
            if Q = nil then
                Q := P + Cnt;
            HandleTableRow(TagParams, P + J + 1, Q - P - J,
                           RowDataGetter, UserData, DestStream);
            Cnt := P + Cnt - Q;
            P := Q;
            Continue;
        end;

        if TagData.Find(TagName, TagValue) then
            WriteStream(DestStream, TagValue);

        Inc(J);
        Inc(P, J);
        Dec(Cnt, J);
    end;
    WriteStream(DestStream, #13#10);
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function HtmlPageProducerFromMemory(
    Buf                : PChar;
    BufLen             : Integer;
    TagData            : TStringIndex;
    RowDataGetter      : PTableRowDataGetter;
    UserData           : TObject;
    DestStream         : TStream) : Boolean;
const
    MAX_BUF  = 50;
var
    I, J      : Integer;
    TagName   : String;
    TagParams : String;
    TagValue  : String;
    P, Q      : PChar;
    Cnt       : Integer;
begin
    Result := FALSE;
    if (not Assigned(DestStream)) then
        Exit;
    if (Buf = nil) or (BufLen <= 0) then
        Exit;

    P   := Buf;
    Cnt := BufLen;
    while TRUE do begin
        { Search starting delimiter }
        I := 0;
        while (I < (Cnt - 1)) and ((P[I] <> '<') or (P[I + 1] <> '#')) do
            Inc(I);

        if P[I] <> '<' then begin
            { No starting tag found, write source to destination }
            DestStream.Write(P^, Cnt);
            break;
        end;

        { Delimiter found
          Write from source to destination until start tag }
        if I > 0 then
            DestStream.Write(P^, I);

        { Search ending delimiter }
        J := I;
        while (J < Cnt) and (P[J] <> '>') and (P[J] <> ' ') and (P[J] <> #9) do
            Inc(J);

{$IFDEF VER80}
        Move(P[I + 3], TagName[1], J - I - 2);
        TagName[0] := Char(J - I - 2);
{$ELSE}
        TagName := UpperCase(Copy(P, I + 3, J - I - 2));
{$ENDIF}
        if P[J] = '>' then
            TagParams := ''
        else begin
            I := J + 1;
            while (J < Cnt) and (P[J] <> '>') do
                Inc(J);
{$IFDEF VER80}
            Move(P[I], TagParams[1], J - I + 1);
            TagParams[0] := Char(J - I + 1);
{$ELSE}
            TagParams := Trim(UpperCase(Copy(P, I, J - I + 1)));
{$ENDIF}
        end;

        if TagName = 'TABLE_ROWS' then begin
            Q := SearchTableRowsEnd(P + J + 1, Cnt - J - 1);
            if Q = nil then
                Q := P + Cnt;
            HandleTableRow(TagParams, P + J + 1, Q - P - J,
                           RowDataGetter, UserData, DestStream);
            Cnt := P + Cnt - Q;
            P := Q;
            Continue;
        end;

        if TagData.Find(TagName, TagValue) then
            WriteStream(DestStream, TagValue);

        Inc(J);
        Inc(P, J);
        Dec(Cnt, J);
    end;
    WriteStream(DestStream, #13#10);
    Result := TRUE;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function VarRecToString(V : TVarRec) : String;
const
    BooleanToString : array [Boolean] of String = ('FALSE', 'TRUE');
begin
    case V.VType of
    vtInteger:    Result := IntToStr(V.VInteger);
    vtBoolean:    Result := BooleanToString[V.VBoolean];
    vtChar:       Result := V.VChar;
    vtExtended:   Result := FloatToStr(V.VExtended^);
    vtString:     Result := V.VString^;
    vtPointer:    Result := 'Unsupported TVarRec.VType = vtPointer';
    vtPChar:      Result := StrPas(V.VPChar);
    vtObject:     Result := 'Unsupported TVarRec.VType = vtObject';
    vtClass:      Result := 'Unsupported TVarRec.VType = vtClass';
{$IFNDEF VER80}
    vtWideChar:   Result := 'Unsupported TVarRec.VType = vtWideChar';
    vtPWideChar:  Result := 'Unsupported TVarRec.VType = vtPWideChar';
    vtAnsiString: Result := StrPas(V.VPChar);
    vtCurrency:   Result := 'Unsupported TVarRec.VType = vtCurrency';
    vtVariant:    Result := 'Unsupported TVarRec.VType = vtVariant';
{$ENDIF}
{$IFDEF DELPHI3_UP}
    vtWideString: Result := 'Unsupported TVarRec.VType = vtWideString';
    vtInterface:  Result := 'Unsupported TVarRec.VType = vtInterface';
{$ENDIF}
{$IFDEF DELPHI4_UP}
    vtInt64:      Result := IntToStr(V.VInt64^);
{$ENDIF}
    else
        Result := 'Unknown TVarRec.VType = "' + IntToStr(Ord(V.VType)) + '" ';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HtmlPageProducer(
    const HtmlFileName : String;
    Tags               : array of const;
    RowDataGetter      : PTableRowDataGetter;
    UserData           : TObject;
    DestStream         : TStream) : Boolean;
var
    FromStream : TFileStream;
    Buf        : PChar;
    BufLen     : Integer;
    TagData    : TStringIndex;
    TagIndex   : Integer;
begin
    if ((High(Tags) - Low(Tags) + 1) and 1) <> 0 then begin
        WriteLnStream(DestStream, '<HTML><BODY>');
        WriteLnStream(DestStream, 'Odd number of tags for substition in ' +
                                  '''' + HtmlFileName + '''<BR>');
        WriteLnStream(DestStream, '</BODY></HTML>');
        Result := FALSE;
        Exit;
    end;
    try
        FromStream := TFileStream.Create(HtmlFileName,
                                         fmOpenRead or fmShareDenyWrite);
    except
        on E: Exception do begin
            WriteLnStream(DestStream, '<HTML><BODY>');
            WriteLnStream(DestStream, 'Unable to open ''' + HtmlFileName + '''<BR>');
            WriteLnStream(DestStream, E.ClassName + ': ' + E.Message);
            WriteLnStream(DestStream, '</BODY></HTML>');
            Result := FALSE;
            Exit;
        end;
    end;
    TagData := TStringIndex.Create;
    try
        TagIndex := Low(Tags);
        while TagIndex < High(Tags) do begin
            TagData.Add(VarRecToString(Tags[TagIndex]),
                        VarRecToString(Tags[TagIndex + 1]));
            Inc(TagIndex, 2);
        end;
        try
            BufLen := FromStream.Size;
            GetMem(Buf, BufLen + 1);
            try
                FromStream.Read(Buf^, BufLen);
                Buf[BufLen] := #0;
                Result := HtmlPageProducerFromMemory(Buf, BufLen,
                                                     TagData,
                                                     RowDataGetter, UserData,
                                                     DestStream);
            finally
                FreeMem(Buf, BufLen + 1);
            end;
        finally
            FromStream.Free;
        end;
    finally
        TagData.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You must define USE_SSL so that SSL code is included in the component.    }
{ To be able to compile the component, you must have the SSL related files  }
{ which are _NOT_ freeware. See http://www.overbyte.be for details.         }
{$IFDEF USE_SSL}
    {$I HttpSrvImplSsl.inc}
{$ENDIF}


{ANDREAS THttpRange }
procedure THttpRange.Assign(Source: THttpRange);
begin
    FRangeFrom := Source.RangeFrom;
    FRangeTo   := Source.RangeTo;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpRange.Create;
begin
    FRangeFrom := -1;
    FRangeTo   := -1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRange.GetContentRangeString(
    CompleteDocSize: THttpRangeInt): String;
begin
    if RangeFrom < 0 then
        { The Last Bytes }
        Result := IntToStr(CompleteDocSize - RangeFrom) + '-' +
                  IntToStr(CompleteDocSize - 1) + '/' + IntToStr(CompleteDocSize)
    else if RangeTo < 0 then
        { The First Bytes }
        Result := IntToStr(RangeFrom) + '-' + IntToStr(CompleteDocSize - 1) +
                  '/' + IntToStr(CompleteDocSize)
    else
        { The First Bytes }
        Result := IntToStr(RangeFrom) + '-' + IntToStr(RangeTo) +
                  '/' + IntToStr(CompleteDocSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRange.SetRangeFrom(const Value: THttpRangeInt);
begin
    FRangeFrom := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRange.SetRangeTo(const Value: THttpRangeInt);
begin
    FRangeTo := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ THttpRangeList }
const
    CRLF : String = #13#10;
procedure WriteLnToStream(Stream: TStream; const Value: String);
begin
    if Value <> '' then
        Stream.Write(Value[1], Length(Value));
    Stream.Write(CRLF[1], 2);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeList.Assign(Source: THttpRangeList);
var
    I        : Integer;
    NewRange : THttpRange;
begin
    Clear;
    for I := 0 to Source.Count - 1 do begin
        NewRange := THttpRange.Create;
        NewRange.Assign(Source[i]);
        Add(NewRange);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeList.CreateRangeStream(
    SourceStream    : TStream;
    ContentString   : String;
    CompleteDocSize : THttpRangeInt): TStream;
var
    NewStream: THttpRangeStream;
begin
    NewStream := THttpRangeStream.Create;
    if NewStream.InitRangeStream(SourceStream, Self, ContentString) then
        Result := NewStream
    else begin
        Result := nil;
        NewStream.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeList.GetItems(NIndex: Integer): THttpRange;
begin
    Result := TObject(inherited Items[NIndex]) as THttpRange;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeList.SetItems(NIndex: Integer; const Value: THttpRange);
begin
    inherited Items[NIndex] := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ParseRangeString(
    var FromStr : String;
    var ToStr   : String;
    const Value : String);
var
    SeperatorPos: integer;
begin
    FromStr := '';
    ToStr   := '';
    SeperatorPos := Pos('-', Value);
    if SeperatorPos <> 0 then begin
        FromStr := Copy(Value, 1, SeperatorPos - 1);
        ToStr   := Copy(Value, SeperatorPos + 1, Length(Value));
        FromStr := Trim(FromStr);
        ToStr   := Trim(ToStr);
        { Numeric Testing }
        if FromStr <> '' then begin
            try
                StrToInt(FromStr);
            except
                FromStr := '';
                ToStr   := '';
                exit;
            end;
        end;
        if ToStr <> '' then begin
            try
                StrToInt(ToStr);
            except
                FromStr := '';
                ToStr   := '';
                exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeList.InitFromString(AStr: String);
var
    Values     : TStrings;
    NewRange   : THttpRange;
    CommaPos   : integer;
    WorkString : String;
    FromStr    : String;
    ToStr      : String;
begin
    Clear;

    Values := TStringList.Create;

    try
        System.Delete(AStr, 1, Length('bytes='));
        Trim(AStr);

        { Parse the string valid values are:
         '-500'
         '500-'
         '500-1000'
         '200-300,450-8450,64-10'
         '200-300,-600'
         '465-,2315-8499'
        }
        while AStr <> '' do begin
            { At first get the next comma }
            CommaPos := Pos(',', AStr);
            if CommaPos = 0 then begin
                WorkString := AStr;
                AStr       := '';
            end
            else begin
                WorkString := Trim(Copy(AStr, 1, CommaPos - 1));
                AStr := Trim(Copy(AStr, CommaPos + 1, Length(AStr)));
            end;

            ParseRangeString(FromStr, ToStr, WorkString);
            if (FromStr <> '') or (ToStr <> '') then begin
                NewRange := THttpRange.Create;
                if FromStr = '' then
                    NewRange.RangeFrom := -1
                else
                    NewRange.RangeFrom := StrToInt(FromStr);
                if ToStr = '' then
                    NewRange.RangeTo := -1
                else
                    NewRange.RangeTo := StrToInt(ToStr);
                Add(NewRange);
            end;
        end;
    finally
        Values.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeList.Valid: boolean;
begin
    Result := (Count > 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeList.Clear;
var
    I: Integer;
begin
    for I := 0 to Count - 1 do begin
        if Items[I] <> nil then begin
            Items[I].Free;
            Items[I] := nil;
        end;
    end;
    inherited Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpRangeList.Destroy;
begin
    Clear;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THttpRangeStream.Create;
begin
    inherited Create;
    FPartStreams := TList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THttpRangeStream.Destroy;
begin
    if Assigned(FPartStreams) then begin
        ClearPartStreams;
        FPartStreams.Free;
        FPartStreams := nil;
    end;
    if Assigned(FSourceStream) then begin
        FSourceStream.Free;
        FSourceStream := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeStream.AddPartStream(
    Value     : TStream;
    AStartPos : Integer;
    AEndPos   : Integer);
var
    Part : THttpPartStream;
begin
    Part := THttpPartStream.Create;
    with Part do begin
        Stream   := Value;
        StartPos := AStartPos;
        EndPos   := AEndPos;
        Offset   := 0;
        Size     := 0;
    end;
    FPartStreams.Add(Part);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeStream.ClearPartStreams;
var
    I : Integer;
begin
    for I := 0 to FPartStreams.Count - 1 do begin
        with THttpPartStream(FPartStreams.Items[I]) do begin
            if Stream <> FSourceStream then
                Stream.Free;
        end;
        THttpPartStream(FPartStreams.Items[I]).Free;
    end;
    FPartStreams.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeStream.PartStreamsCount : Integer;
begin
    Result := FPartStreams.Count;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeStream.GetPartStreams(
    NIndex: Integer): THttpPartStream;
begin
    Result := FPartStreams.Items[NIndex];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeStream.InitRangeStream(
    SourceStream  : TStream;
    RangeList     : THttpRangeList;
    ContentString : String): boolean;
var
    FromVal         : THttpRangeInt;
    ToVal           : THttpRangeInt;
    I               : Integer;
    CompleteDocSize : THttpRangeInt;
    AStream         : TStream;
begin
    FSourceStream := SourceStream;
    Result        := False;
    if RangeList.Count > 0 then begin
        CompleteDocSize := SourceStream.Size;
        ClearPartStreams;

        for I := 0 to RangeList.Count-1 do begin
            if RangeList.Count > 1 then begin
                AStream := TMemoryStream.Create;
                if I <> 0 then
                    WriteLnToStream(AStream, '');
                WriteLnToStream(AStream, '--' + ByteRangeSeparator);
                WriteLnToStream(AStream, 'Content-Type: ' + ContentString);
                WriteLnToStream(AStream, 'Content-Range: bytes ' +
                    RangeList.Items[i].GetContentRangeString(CompleteDocSize));
                WriteLnToStream(AStream, '');
                AddPartStream(AStream, 0, AStream.Size);
            end;
            FromVal := RangeList.Items[I].RangeFrom;
            ToVal   := RangeList.Items[I].RangeTo;
            if (FromVal < 0) and (ToVal > 0) then begin
                { Need the last number of bytes }
                FromVal := SourceStream.Size - ToVal;
                ToVal   := SourceStream.Size;
            end
            else begin
                if ToVal < 0 then
                    ToVal := SourceStream.Size
                else
                    ToVal := ToVal + 1;
            end;
            if (FromVal > SourceStream.Size) or (FromVal < 0) or
               (ToVal > SourceStream.Size)   or (ToVal <= FromVal) then begin
                { wrong Range -> Send the whole Document }
                ClearPartStreams;
                FSourceStream := nil;
                exit;
            end;
            AddPartStream(SourceStream, FromVal, ToVal);
        end;
        if RangeList.Count > 1 then begin
            AStream := TMemoryStream.Create;
            WriteLnToStream(AStream, '');
            WriteLnToStream(AStream, '--' + ByteRangeSeparator + '--');
            WriteLnToStream(AStream, '');
            AddPartStream(AStream, 0, AStream.Size);
        end;
        CalculateOffsets;
        Result := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Min(A, B : Integer) : Integer;
begin
    if A < B then
        Result := A
    else
        Result := B;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeStream.Read(var Buffer; Count: Longint): Longint;
var
    DataRead  : THttpRangeInt;
    Index     : Integer;
    ActSize   : Integer;
    ActOffset : THttpRangeInt;
    SizeRead  : THttpRangeInt;
    Rec       : THttpPartStream;
begin
    Rec := nil;  { Just to remove a compiler warning }
    if (FPosition >= 0) and (Count >= 0) then begin
        Result := FSize - FPosition;
        if Result > 0 then begin
            Index := 0;
            DataRead := 0;
            while DataRead < Count do begin
                while TRUE do begin
                    if Index >= PartStreamsCount then begin
                        { Error }
                        Result := 0;
                        exit;
                    end;
                    Rec := PartStreams[Index];
                    if Rec.Offset +
                       Rec.Size > FPosition then
                        break;
                    Inc(Index);
                end;

                ActOffset := FPosition - Rec.Offset;
                ActSize   := min(Count - DataRead, Rec.Size - (ActOffset));
                Rec.Stream.Position := ActOffset + Rec.StartPos;
                SizeRead := Rec.Stream.Read(Pointer(DWORD(@Buffer) + DWORD(DataRead))^, ActSize);
                Inc(Index);
                Inc(DataRead, SizeRead);
                Inc(FPosition, SizeRead);
            end;
            Result := DataRead;
            Exit;
        end;
    end;
    Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI7_UP}
function THttpRangeStream.Seek(
    const Offset: Int64;
    Origin: TSeekOrigin): Int64;
{$ELSE}
function THttpRangeStream.Seek(Offset: Longint; Origin: Word): Longint;
{$ENDIF}
begin
    case WORD(Origin) of
        soFromBeginning : FPosition := Offset;
        soFromCurrent   : Inc(FPosition, Offset);
        soFromEnd       : FPosition := FSize + Offset;
    end;
    Result := FPosition;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THttpRangeStream.Write(const Buffer; Count: Longint): Longint;
begin
    raise Exception.Create('Stream is readonly');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpRangeStream.CalculateOffsets;
var
    I, J    : Integer;
    AOffset : THttpRangeInt;
    Rec     : THttpPartStream;
begin
    Rec     := nil;  { Just to remove a compiler warning }
    AOffset := 0;
    I       := 0;
    J       := PartStreamsCount;  { Costly function, optimize }
    while I < J do begin
        Rec := PartStreams[I];
        with Rec do begin
            Offset  := AOffset;
            Size    := EndPos - StartPos;
            Inc(AOffset, Size);
        end;
        Inc(I);
    end;
    FSize := Rec.Size + Rec.Offset;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
