{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Component to query DNS records.
              Implement a subset of RFC 1035 (A and MX records).
Creation:     January 29, 1999
Version:      1.03
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@overbyte.be>

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
Feb 14, 1999 V0.02 Indirectly call winsock functions using wsocket because
             wsocket provide runtime dynamic link instead of loadtime link.
             This allows a program to use DnsQuery if it discover that winsock
             is installed and still run if winsock is not installed.
Feb 24, 1999 V1.00 Added code for reverse lookup (PTR record).
Mar 07, 1999 V1.01 Adapted for Delphi 1
Aug 20, 1999 V1.02 Revise compile time option. Adapted for BCB4
Jul 27, 2001 V1.03 Holger Lembke <holger@hlembke.de> implemented a few new
                   queries or propreties (QueryAny, LongLatToDMS, Loc2Geo, Loc)
                   and related data types.
-----------------------------
Jul 27, 2002 V1.80 Holger Lembke <holger@hlembke.de> has rewritten major parts
                   after error corrections, I suggest a jump to 2.01

                   changed almost everything. goal was
                     - to have a more comprehensive componente
                     - ease to implemente future DNS extentions/missing records
                       (future in meaning of this component. not really the dns system.)

                   Users from 1.03 and before should be able to adapt code quickly.
                    - all the response... and question... things moved into a record, so
                       just add a point after response
                    - all array-properties are gone.
                      instead, you have to use a loop to run throu the desired types

                         for i:=0 to dnsquery.ResponseCount[DnsQueryNS]-1 do
                           with dnsquery.ResponseItem[DnsQueryNS,i] do begin
                             writeln(nsdname);
                           end;

                      Look up in TRRRecord, which record parts are defined for a
                      specific item.

                      To add new records, follow this guidelines:
                        1.) define the DNSQuery-Constant
                        2.) Define the needed record
                            only one rule: no dynamic memory allocation
                        3.) add it into the case structure in TRRRecord
                        4.) add a 'decoder' into GetResponseItem

Aug 30, 2002 V1.81 fixed a minor error in SubLOCgeo Holger Lembke <holger@hlembke.de>

Feb 25, 2003 V?.?? Added "Type PCardinal = ^cardinal;" in GetResponseItem

Mar 30, 2003 V?.?? Holger Lembke <holger@hlembke.de>
                   Added TCP-Protocol-support (rfc1035, 4.2.2.), this enables zone transfers.
                       To archive this, FResponseBuf has been changed into a pointer. With
                       this some parts of the buffer handling for FResponseBuf needed to be
                       rewritten. And WSocketDataAvailable has to be aware of this.

                   translated IPNummer to IPNumber, ip6nummer to ip6number

                   added property CtrlSocket : TWSocket read FWSocket;

                   changed dyn.array to simple pointer (TDataCacheItemArray)


-----------------------------


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit DnsQuery;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$R-}           { Disable range checking              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Winsock, WSocket;

const
  DnsQueryVersion    = 102;
  CopyRight : String = ' TDnsQuery  (c) 1999-2000 F. Piette V1.02 ';

  UDPBufSize         = 512+10;  // !!KAP!! 2003-03-30  max. size plus some security

  { DNS Classes }
  DnsClassIN      = 1;   { The internet                                      }
  DnsClassCS      = 2;   { The CSNET class (obsolete, used only for examples)}
  DnsClassCH      = 3;   { The CHAOS class                                   }
  DnsClassHS      = 4;   { Hesiod name service                               }
  DnsClassALL     = 255; { Any class                                         }

  { Type of query/response a DNS can handle }
  DnsQueryA       = 1;  { A     HostAddress                                  }
  DnsQueryNS      = 2;  { NS    Authoritative name server                    }
  DnsQueryMD      = 3;  { MD    MailDestination, obsolete, use Mail Exchange }
  DnsQueryMF      = 4;  { MF    MailForwarder, obsolete, use Mail Exchange   }
  DnsQueryCNAME   = 5;  { CNAME CanonicalName                                }
  DnsQuerySOA     = 6;  { SOA   Start of a Zone of Authority                 }
  DnsQueryMB      = 7;  { MB    MailBox, experimental                        }
  DnsQueryMG      = 8;  { MG    MailGroup, experimental                      }
  DnsQueryMR      = 9;  { MR    MailRename, experimental                     }
  DnsQueryNULL    = 10; { NULL  Experimental                                 }
  DnsQueryWKS     = 11; { WKS   Well Known Service Description               }
  DnsQueryPTR     = 12; { PTR   Domain Name Pointer                          }
  DnsQueryHINFO   = 13; { HINFO Host Information                             }
  DnsQueryMINFO   = 14; { MINFO Mailbox information                          }
  DnsQueryMX      = 15; { MX    Mail Exchange                                }
  DnsQueryTXT     = 16; { TXT   Text Strings                                 }
  { !!KAP!! }
  DnsQueryRP      = 17; // RFC 1183
  DnsQueryAFSDB   = 18; // RFC 1183
  DnsQueryX25     = 19; // RFC 1183
  DnsQueryISDN    = 20; // RFC 1183
  DnsQueryRT      = 21; // RFC 1183
  DnsQueryNSAP    = 22; // RFC 1706
  DnsQueryNSAPPTR = 23;
  DnsQuerySIG     = 24; { see RFC-2065                                       }
  DnsQueryKEY     = 25; { see RFC-2065                                       }
  DnsQueryPX      = 26; // rfc 1327?
  DnsQueryGPOS    = 27; { GPOS has the following format:
                          <owner> <ttl> <class> GPOS <longitude> <latitude> <altitude> }
  DnsQueryAAAA    = 28; { see IP6 Address                                    }
  DnsQueryLOC     = 29; (* see RFC-1876  http://rfc.net/rfc1876.html
                         <owner> <TTL> <class> LOC ( d1 [m1 [s1]] {"N"|"S"} d2 [m2 [s2]]
                               {"E"|"W"} alt["m"] [siz["m"] [hp["m"]
                               [vp["m"]]]] )
                        *)
  DnsQueryNXT     = 30; { see RFC-2065                                       }

  DnsQuerySRV     = 33; { see RFC-2052                                       }
  DnsQueryNAPTR   = 35; { see RFC-2168                                       }
  DnsQueryKX      = 36;

  { Some additional type only allowed in queries }
  DnsQueryAXFR    = 252; { Transfer for an entire zone                       }
  DnsQueryMAILB   = 253; { Mailbox related records (MB, MG or MR)            }
  DnsQueryMAILA   = 254; { MailAgent, obsolete, use MX instead               }
  DnsQueryALL     = 255; { Request ALL records                               }

  { Opcode field in query flags }
  DnsOpCodeQUERY  = 0;
  DnsOpCodeIQUERY = 1;
  DnsOpCodeSTATUS = 2;

type
  TDnsRequestDoneEvent = procedure (Sender : TObject; Error : WORD) of Object;

  TDnsRequestHeader = packed record
    ID      : WORD;
    flags   : word;
    QDCount : WORD;
    ANCount : WORD;
    NSCount : WORD;
    ARCount : WORD;
  end;
  PDnsRequestHeader = ^TDnsRequestHeader;

  // rfc 1035 p.10
  tname = string[255];
  ttxtstring = string[255];
  tadress = cardinal; //32bit

  // rfc 1035 p.26
  TDnsRequestAnswerHeader = record //
    len                 : integer;
    ID                  : WORD;
    qr                  : boolean;
    opcode              : byte;
    AuthoritativeAnswer : boolean;
    Truncation          : boolean;
    RecursionDesired    : boolean;
    RecursionAvailable  : boolean;
    z                   : byte;
    rcode               : byte;

(* some rrcodes
   RCODE   Name    Description                        Reference
   Decimal
     Hexadecimal
    0    NoError   No Error                           [RFC 1035]
    1    FormErr   Format Error                       [RFC 1035]
    2    ServFail  Server Failure                     [RFC 1035]
    3    NXDomain  Non-Existent Domain                [RFC 1035]
    4    NotImp    Not Implemented                    [RFC 1035]
    5    Refused   Query Refused                      [RFC 1035]
    6    YXDomain  Name Exists when it should not     [RFC 2136]
    7    YXRRSet   RR Set Exists when it should not   [RFC 2136]
    8    NXRRSet   RR Set that should exist does not  [RFC 2136]
    9    NotAuth   Server Not Authoritative for zone  [RFC 2136]
   10    NotZone   Name not contained in zone         [RFC 2136]
   11-15           available for assignment
   16    BADVERS   Bad OPT Version                    [RFC 2671]
   16    BADSIG    TSIG Signature Failure             [RFC 2845]
   17    BADKEY    Key not recognized                 [RFC 2845]
   18    BADTIME   Signature out of time window       [RFC 2845]
   19    BADMODE   Bad TKEY Mode                      [RFC 2930]
   20    BADNAME   Duplicate key name                 [RFC 2930]
   21    BADALG    Algorithm not supported            [RFC 2930]
   22-3840         available for assignment
     0x0016-0x0F00
   3841-4095       Private Use
     0x0F01-0x0FFF
   4096-65535      available for assignment
     0x1000-0xFFFF
*)
    QDCount             : WORD;
    ANCount             : WORD;
    NSCount             : WORD;
    ARCount             : WORD;
  end;

  // rfc 1035 p.19
  TSoaRecord = record
    mname   : tname;
    rname   : tname;
    serial  : Cardinal;
    refresh : Cardinal;
    retry   : Cardinal;
    expire  : Cardinal;
    minimum : Cardinal;
  end;

  // rfc 1876
  TLOCInfo = packed record { need to be 16 bytes }
    version    : byte;
    size       : byte;
    horizpre   : byte;
    vertpre    : byte;
    latitude   : longint;
    longitude  : longint;
    altitude   : longint;
  end;
  PLOCInfo = ^TLOCInfo;

  // RFC 1886 p.2
  TAAAA = array[0..3] of Cardinal; // 128 bit

  { Decoded TLOCInfo }
  TLogGeo = record
    version             : byte;
    longsize            : integer;
    latsize             : integer;
    horizpre            : integer;
    vertpre             : integer;
    { Latitude, degree, minutes, seconds, milliseconds }
    lad, lam, las, lams : integer;
    lahem               : char;
    { same for Longitude }
    lod, lom, los, loms : integer;
    lohem               : char;
    altitude            : integer;
  end;

  // Question Data rfc1035 p.28
  TQuestion = record
    QuestionType   : word;
    QuestionClass  : word;
    QuestionName   : tname;
  end;

  // rfc 1035 p.14
  THinfo = packed record
    cpu : word;
    os  : word;
  end;

  // rfc 1035 p.16
  TMinfo = packed record
    rmailbx  : tname;
    remailbx : tname;
  end;

  // rfc 1035 p.17
  TMX = record
    preference : word;
    exchange   : tname;
  end;

  // rfc 1035 p.10
  TRRInternal = packed record
    rrtype   : word;     // r due to token conflict
    rrclass  : word;     // same
    rrttl    : cardinal; // same
    rdlength : word;
  end;
  pRRInternal = ^TRRInternal;

  // Result-Record
  TRRRecord = packed record
    valid    : boolean;

    // internal for caching
    lastid    : integer;
    lastindex : integer;

    // RR record start rfc 1035 p.29
    name      : tname;
    rrtype    : word;      // r due to token conflict
    rrclass   : word;      // same
    rrttl     : cardinal;  // same
    rdlength  : word;

    case integer of // depending on rrtype, one of these structures is filled
                    // more or less following rfc 1035 p.15
      DnsQueryMD,
      DnsQueryMB,
      DnsQueryMF     : (madname   : tname);
      DnsQueryCNAME  : (cname     : tname);
      DnsQueryHINFO  : (hinfo     : thinfo);
      DnsQueryMG     : (mgmname   : tname);
      DnsQueryMINFO  : (minfo     : tminfo);
      DnsQueryMR     : (newname   : tname);
      DnsQueryNS     : (nsdname   : tname);
      DnsQueryPTR    : (ptrname   : tname);
      DnsQuerySOA    : (soa       : TSoaRecord);
      DnsQueryTXT    : (txt       : ttxtstring);
      DnsQueryMX     : (mx        : TMX);
      DnsQueryA      : (adress    : tadress;
                        ipnumber  : tname);    // interpreted
      DnsQueryAAAA   : (aaaa      : TAAAA;
                        ip6number : tname);    // interpreted
      DnsQueryLOC    : (loc       : TLOCInfo;
                        locdecode : TLogGeo);  // interpreted
  end;

  // for a quicker access
  TDataCacheItem = record
    rrtype : word;
    rrpos  : pchar;
  end;

  // !!KAP!! 2003-03-30
  TDataCacheItemArray = array[0..$FFFF div sizeof(TDataCacheItem)] of TDataCacheItem;
  PDataCacheItemArray = ^TDataCacheItemArray;

  tdatacache = record
    count  : integer;
    items  : PDataCacheItemArray;
  end;

  // !!KAP!! 2003-03-30
  TResBuf = array[0..$FFFF] of char;
  PResBuf = ^TResBuf;

  TDnsQuery = class(TComponent)
  private
    { Déclarations privées }
    frrcache          : TRRRecord;
    fdatacache        : tdatacache;
    fDnsRequestAnswer : TDnsRequestAnswerHeader;
    fquestion         : TQuestion;

    function NewExtractName(var p : pchar):string;
    function GetRepsonsecount(nid : integer):integer;
    function GetResponseItem(nid : integer; nindex : integer):TRRRecord;

  protected
    FWSocket                    : TWSocket;
    FPort                       : String;
    FAddr                       : String;
    FIDCount                    : WORD;
    FQueryBuf                   : array [0..511] of char;
    FQueryLen                   : Integer;
    fUseTCP                     : boolean;

    FResponseBuf                : PResBuf; // dynamic!   // !!KAP!! 2003-03-30
    FResponseLen                : Integer;
    FResponseBufSize            : integer;               // !!KAP!! 2003-03-30
    FResponseGotHead            : boolean;               // !!KAP!! 2003-03-30

    FOnRequestDone              : TDnsRequestDoneEvent;

    { !!KAP!! }
    fQueryPending               : boolean;

    procedure BuildRequestHeader(Dst       : PDnsRequestHeader;
                                 ID        : WORD;
                                 OPCode    : BYTE;
                                 Recursion : Boolean;
                                 QDCount   : WORD;
                                 ANCount   : WORD;
                                 NSCount   : WORD;
                                 ARCount   : WORD); virtual;
    function  BuildQuestionSection(Dst         : PChar;
                                   const QName : String;
                                   QType       : WORD;
                                   QClass      : WORD) : Integer; virtual;
    procedure WSocketDataAvailable(Sender: TObject; Error: WORD); virtual;
    procedure TriggerRequestDone(Error: WORD); virtual;
    function  GetResponseBuf : PChar;
    procedure SendQuery;

    { !!KAP!! 2002-02-15}
    procedure DNSSocketSessionClosed(Sender: TObject; Error: Word);
    // !!KAP!! 2003-03-30
    procedure DNSSocketSessionConnected(Sender: TObject; Error: Word);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Notification(AComponent: TComponent; operation: TOperation); override;
    function    MXLookup(Domain : String) : Integer;
    function    ALookup(Host : String) : Integer;
    function    PTRLookup(IP : String) : Integer;
    { !!KAP!! }
    function    QueryAny(Host : String; QNumber : Integer) : Integer;
    // Query this to see if dns-request is pending
    property    QueryPending            : Boolean read fQueryPending;

    { !!KAP!! 2002-02-15}
    procedure   AbortPending;

    { !!KAP!! 2002-07-27}
    property Response      : TDnsRequestAnswerHeader                  read fDnsRequestAnswer;
    property ResponseBuf   : PChar                                    read GetResponseBuf;
    // 0 : all items, otherwise the queryitems
    property ResponseCount[nid : integer]:integer                     read GetRepsonsecount;
    property ResponseItem[nid : integer; nindex : integer]: TRRRecord read GetResponseItem;

    // simpler
    property Question : TQuestion read fQuestion;

    { !!KAP!! 2003-03-30}
    property CtrlSocket : TWSocket read FWSocket;
  published
    property Port    : String read  FPort write FPort;
    property Addr    : String read  FAddr write FAddr;
    property OnRequestDone : TDnsRequestDoneEvent read  FOnRequestDone
                                                  write FOnRequestDone;

    { !!KAP!! 2003-03-30}
    property UseTCP  : boolean read fUseTCP write fUseTCP;
  end;


function ReverseIP(const IP : String) : String;
function LongLatToDMS(longlat : longint; hemis : String) : String; { !!KAP!! }
function Loc2Geo(loc : TLOCInfo) : TLogGeo;                        { !!KAP!! }
function LocAltToAlt(Localt : LongInt) : LongInt;                  { !!KAP!! }
procedure Register;

// should compile.
Const   RRRecordsize = sizeof(TRRRecord);

implementation

type
    PWORD  = ^WORD;
    PDWORD = ^DWORD;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function ReverseIP(const IP : String) : String;
var
    I, J : Integer;
begin
    Result := '';
    if Length(IP) = 0 then
        Exit;
    J      := Length(IP);
    I      := J;
    while I >= 0 do begin
        if (I = 0) or (IP[I] = '.') then begin
            Result := Result + '.' + Copy(IP, I + 1, J - I);
            J := I - 1;
        end;
        Dec(I);
    end;
    if Result[1] = '.' then
        Delete(Result, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TDnsQuery]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TDnsQuery.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FWSocket := TWSocket.Create(nil);
    FPort    := '53';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TDnsQuery.Destroy;
begin
  // erase cache
  if (fdatacache.count>0) then begin
    freemem(fdatacache.items,sizeof(fdatacache.items[0])*fdatacache.count);
    fdatacache.count:=0
  end;

  // !!KAP!! 2003-03-30
  if (FResponseBufSize>0) then begin
    freemem(FResponseBuf,FResponseBufSize);
    FResponseBufSize:=0;
    FResponseBuf:=nil;
  end;

    if Assigned(FWSocket) then begin
        FWSocket.Destroy;
        FWSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FWSocket then
            FWSocket := nil;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.GetResponseBuf : PChar;
begin
  Result := @FResponseBuf^[0];
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.MXLookup(Domain : String) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Domain, DnsQueryMX, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.ALookup(Host : String) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, DnsQueryA, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! }
function TDnsQuery.QueryAny(Host : String; qnumber : integer) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)], Host, qnumber, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.PTRLookup(IP : String) : Integer;
begin
    Inc(FIDCount);
    BuildRequestHeader(PDnsRequestHeader(@FQueryBuf), FIDCount, DnsOpCodeQuery, TRUE, 1, 0, 0, 0);
    FQueryLen := BuildQuestionSection(@FQueryBuf[SizeOf(TDnsRequestHeader)],
                                      ReverseIP(IP) + '.in-addr.arpa',
                                      DnsQueryPTR, DnsClassIN);
    FQueryLen := FQueryLen + SizeOf(TDnsRequestHeader);
    Result    := FIDCount;
    SendQuery;
end;

Const sendtcp = 'tcp';      // !!KAP!! 2003-03-30
      sendudp = 'udp';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.SendQuery;
begin
  // !!KAP!!
  fQueryPending:=true;

  // erase cache
  if (fdatacache.count>0) then begin
    freemem(fdatacache.items,sizeof(fdatacache.items[0])*fdatacache.count);
    fdatacache.count:=0
  end;

  FWSocket.OnDataAvailable := nil;
  FWSocket.Abort;

  // !!KAP!! 2003-03-30
  FResponseLen:=0;
  FResponseGotHead:=false;
  FWSocket.OnDataAvailable:=WSocketDataAvailable;
  FWSocket.OnSessionClosed:=DNSSocketSessionClosed;
  FWSocket.OnSessionConnected:=DNSSocketSessionConnected;

  // !!KAP!! 2003-03-30
  if (fUseTCP)
    then FWSocket.Proto:=sendtcp
    else FWSocket.Proto:=sendudp;

  FWSocket.Port:=FPort;
  FWSocket.Addr:=FAddr;

  // !!KAP!! 2003-03-30
  try
    FWSocket.Connect;
  except
    TriggerRequestDone(fwsocket.LastError); // No Connection? Bah!
  end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! 2003-03-30}
procedure TDnsQuery.DNSSocketSessionConnected(Sender: TObject; Error: Word);
Var len : word;
begin
  if (fUseTCP) then begin
    len:=WSocket_ntohs(FQueryLen);
    fwsocket.send(@len,sizeof(len));
  end;

  FWSocket.Send(@FQueryBuf, FQueryLen);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDnsQuery.BuildQuestionSection(
    Dst         : PChar;
    const QName : String;
    QType       : WORD;
    QClass      : WORD) : Integer;
var
    I   : Integer;
    p   : PChar;
    Ptr : PChar;
begin
    Ptr := Dst;
    if Ptr = nil then begin
        Result := 0;
        Exit;
    end;
    I := 1;
    while I <= Length(QName) do begin
        p := Ptr;
        Inc(Ptr);
        while (I <= Length(QName)) and (QName[I] <> '.') do begin
            Ptr^ := QName[I];
            Inc(Ptr);
            Inc(I);
        end;
        p^ := Chr(Ptr - p - 1);
        Inc(I);
    end;
    Ptr^ := #0;
    Inc(Ptr);
    PWORD(Ptr)^ := htons(QType);
    Inc(Ptr, 2);
    PWORD(Ptr)^ := htons(QClass);
    Inc(Ptr, 2);
    Result := Ptr - Dst;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.BuildRequestHeader(
    Dst       : PDnsRequestHeader;
    ID        : WORD;
    OPCode    : BYTE;
    Recursion : Boolean;
    QDCount   : WORD;
    ANCount   : WORD;
    NSCount   : WORD;
    ARCount   : WORD);
begin
    if Dst = nil then
        Exit;
    Dst^.ID      := htons(ID);
    Dst^.Flags   := htons((OpCode shl 11) + (Ord(Recursion) shl 8));
    Dst^.QDCount := htons(QDCount);
    Dst^.ANCount := htons(ANCount);
    Dst^.NSCount := htons(NSCount);
    Dst^.ARCount := htons(ARCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.TriggerRequestDone(Error: WORD);
begin
  if Assigned(FOnRequestDone) then
    FOnRequestDone(Self, Error);

  //!!KAP!!
  fQueryPending:=false;
end;

{ !!KAP!! 2002-02-15}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.AbortPending;
begin
  fQueryPending:=false;
end;

(************************************************************************************************)
(************************************************************************************************)
function TDnsQuery.NewExtractName(var p : pchar):string;
var N  : Integer;
    I  : Integer;
    pc : pchar;
begin
  result:='';

  if (P^=#0) then
    Inc(P)
   else
    repeat
      { Get name part length }
      N:=Ord(P^);
      if (N and $C0)=$C0 then begin
        { Message compression }
        N := ((N and $3F) shl 8) + Ord(P[1]);
        pc:=fResponseBuf^;  // !!KAP!! 2003-03-30
        inc(pc,n);
        result:=result+NewExtractName(pc);
        // Weiter
        Inc(P,2);
        n:=0;
       end else begin
        Inc(P);
        if (N<>0) then begin
          { Copy name part }
          i:=length(result);
          setlength(result,i+n);
          move(p^,result[i+1],n);
          inc(p,n);
          if (P^<>#0) then
            result:=result+'.';
        end;
      end;
    until (n=0);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsQuery.WSocketDataAvailable(Sender: TObject; Error: WORD);
Var Flags    : Integer;
    Ans      : PDnsRequestHeader;
    len      : word;
    i        : integer;
    rp       : PResBuf;
    REndPtr,
    RDataPtr,
    P        : PChar;
begin
  frrcache.lastid:=-1;

  // moved at the beginning
  if (Error<>0) then begin
    fQueryPending:=false;
    TriggerRequestDone(Error);
    FResponseLen:=0;
    exit;
  end;

  // !!KAP!! 2003-03-30
  if (fUseTCP) then begin
    // answer via TCP
    if (not FResponseGotHead) and (fwsocket.RcvdCount>=2) then begin
      // first datachunk, separate the length
      FWSocket.Receive(@len,sizeof(len));
      len:=WSocket_ntohs(len);

      // resize buffer
      if (len<>FResponseBufSize) then begin
        freemem(FResponseBuf,FResponseBufSize);
        FResponseBufSize:=len;
        getmem(FResponseBuf,FResponseBufSize);
      end;
      FResponseGotHead:=true;
      FResponseLen:=0;
    end;

    // We got the 2 bytes head
    if (FResponseGotHead) then begin
      rp:=FResponseBuf;
      inc(integer(rp),FResponseLen);
      // get data
      i:=FWSocket.Receive(rp,FResponseBufSize-FResponseLen);
      // got data?
      if (i>0) then
        FResponseLen:=FResponseLen+i;
      // wait for more data or close the connection....
      if (FResponseLen<>FResponseBufSize)
        then exit
        else FWSocket.close;
     end else
      // no header. Will this ever happen?
      exit;
   end else begin
    // rqeusted via UDP
    if (FResponseBufSize<>UDPBufSize) then begin
      freemem(FResponseBuf,FResponseBufSize);
      FResponseBufSize:=UDPBufSize;
      getmem(FResponseBuf,FResponseBufSize);
    end;
    FResponseLen:=FWSocket.Receive(FResponseBuf,FResponseBufSize);
  end;

  { Check for minimum response length }
  if (FResponseLen<SizeOf(TDnsRequestHeader)) then begin
    TriggerRequestDone(10);
    Exit;
  end;

  Ans:=PDnsRequestHeader(FResponseBuf);

  // ---
  Flags:=WSocket_ntohs(Ans^.Flags);
  { Check if we got a response }
  if (Flags and $8000) = 0 then begin
    TriggerRequestDone(11);
    Exit;
  end;

  { Decode response header }
  fDnsRequestAnswer.len                 := FResponseLen;
  fDnsRequestAnswer.ID                  := WSocket_ntohs(Ans^.ID);
  fDnsRequestAnswer.qr                  := (Flags and $8000) = $8000;
  fDnsRequestAnswer.opcode              := (Flags shr 11) and $000F;
  fDnsRequestAnswer.AuthoritativeAnswer := (Flags and $0400) = $0400;
  fDnsRequestAnswer.Truncation          := (Flags and $0200) = $0200;
  fDnsRequestAnswer.RecursionDesired    := (Flags and $0100) = $0100;
  fDnsRequestAnswer.RecursionAvailable  := (Flags and $0080) = $0080;
  fDnsRequestAnswer.z                   := (Flags shr 4) and $0007;
  fDnsRequestAnswer.rcode               := (Flags and $000F);
  fDnsRequestAnswer.QDCount             := WSocket_ntohs(Ans^.QDCount);
  fDnsRequestAnswer.ANCount             := WSocket_ntohs(Ans^.ANCount);
  fDnsRequestAnswer.NSCount             := WSocket_ntohs(Ans^.NSCount);
  fDnsRequestAnswer.ARCount             := WSocket_ntohs(Ans^.ARCount);

  P:=@ResponseBuf[SizeOf(TDnsRequestHeader)];
  { Should never be greater than 1 because we sent only one question }

  fQuestion.QuestionName:=NewExtractName(p);
  fQuestion.QuestionType:=WSocket_ntohs(PDWORD(p)^); inc(p,sizeof(word));
  fQuestion.QuestionClass:=WSocket_ntohs(PDWORD(p)^); inc(p,sizeof(word));

  REndPtr:=FResponseBuf^;     // !!KAP!! 2003-03-30
  inc(rendptr,FResponseLen);

  // the following is not very smart due two loops....
  // get number or RRDatas
  i:=0;
  RDataPtr:=p;
  while (integer(REndPtr)>integer(RDataPtr)) do begin
    NewExtractName(RDataPtr);
    i:=i+1;
    inc(RDataPtr,sizeof(TRRInternal)+WSocket_ntohs(pRRInternal(rdataptr)^.rdlength));
  end;
  fdatacache.count:=i;
  getmem(fdatacache.items,sizeof(fdatacache.items[0])*fdatacache.count);

  // Fetch RRDatas
  i:=0;
  RDataPtr:=p;
  while (integer(REndPtr)>integer(RDataPtr)) do begin
    fdatacache.items[i].rrpos:=rdataptr;
    NewExtractName(RDataPtr);
    fdatacache.items[i].rrtype:=WSocket_ntohs(pRRInternal(rdataptr)^.rrtype);
    i:=i+1;
    inc(RDataPtr,sizeof(TRRInternal)+WSocket_ntohs(pRRInternal(rdataptr)^.rdlength));
  end;

  TriggerRequestDone(0);
end;

{ !!KAP!! 2002-02-15}
procedure TDnsQuery.DNSSocketSessionClosed(Sender: TObject; Error: Word);
begin
  // das geht garnicht, da dann wsocketdataavailable zu spät aufgerufen wird
  // !!KAP!! 2003-03-30
  (*
  if (fUseTCP) and (error<>0) then begin
    fQueryPending:=false;
    TriggerRequestDone(Error);
    FResponseLen:=0;
  end;
  *)
end;

(************************************************************************************************)
(************************************************************************************************)
// 0 : all items, otherwise the queryitems
function TDnsQuery.GetRepsonsecount(nid : integer):integer;
var i : integer;
begin
  if (nid=0) then
    result:=fdatacache.count
   else begin
    result:=0;
    for i:=0 to fdatacache.count-1 do
      if (fdatacache.items[i].rrtype=nid) then
        inc(result);
  end;
end;

(************************************************************************************************)
(************************************************************************************************)
// 0 : all items, otherwise the queryitems
function TDnsQuery.GetResponseItem(nid : integer; nindex : integer):TRRRecord;
Type PCardinal = ^cardinal;  // D5 does not have this, so we define it anyways... 2003-02-25
var  i,
     index : integer;
     pp    : pchar;
begin
  // nada
  fillchar(result,sizeof(result),0);
  result.valid:=false;

  // Cache hit
  if (frrcache.lastid=nid) and (frrcache.lastindex=nindex) then begin
    result:=frrcache;
    exit;
  end;

  // store for caching.
  result.lastid:=nid;
  result.lastindex:=nindex;

  // Search the entry
  pp:=nil;
  index:=-1;
  for i:=0 to fdatacache.count-1 do
    if (pp=nil) and ( (nid=0) or (fdatacache.items[i].rrtype=nid) ) then begin
      if (nindex=0) then begin
        index:=i;
        pp:=fdatacache.items[i].rrpos;
       end else
        nindex:=nindex-1;
    end;

  // not found. Better raise exception?
  if (pp=nil) then exit;

  // headers
  result.name:=NewExtractName(pp);
  result.rrtype:=WSocket_ntohs(pRRInternal(pp)^.rrtype);
  result.rrclass:=WSocket_ntohs(pRRInternal(pp)^.rrclass);
  result.rrttl:=WSocket_ntohl(pRRInternal(pp)^.rrttl);
  result.rdlength:=WSocket_ntohs(pRRInternal(pp)^.rdlength);

  // end
  inc(pp,sizeof(TRRInternal));

  // Build the record
  case fdatacache.items[index].rrtype of
    DnsQueryMD,
    DnsQueryMB,
    DnsQueryMF     : result.madname:=NewExtractName(pp);
    DnsQueryMG     : result.mgmname:=NewExtractName(pp);
    DnsQueryCNAME  : result.cname:=NewExtractName(pp);
    DnsQueryMR     : result.newname:=NewExtractName(pp);
    DnsQueryNS     : result.nsdname:=NewExtractName(pp);
    DnsQueryPTR    : result.ptrname:=NewExtractName(pp);
    DnsQueryTXT    : result.txt:=NewExtractName(pp);
    DnsQueryA      : begin
                       result.adress:=PDWORD(pp)^;
                       result.ipnumber:=WSocket_inet_ntoa(tinaddr(result.adress));
                     end;
    DnsQueryMX     : begin
                       result.mx.preference:=WSocket_ntohs(PDWORD(pp)^);
                       inc(pp,2);
                       result.mx.exchange:=NewExtractName(pp);
                     end;
    DnsQueryHINFO  : move(pp^,result.hinfo,sizeof(thinfo));
    DnsQueryMINFO  : begin
                       result.minfo.rmailbx:=NewExtractName(pp);
                       result.minfo.remailbx:=NewExtractName(pp);
                     end;
    DnsQueryAAAA   : begin
                       move(pp^,result.aaaa,sizeof(TAAAA));
                       result.ip6number:=format('%p:%p',[ pointer(result.aaaa[0]),pointer(result.aaaa[2]) ]);
                     end;
    DnsQuerySOA    : begin
                       result.soa.mname:=NewExtractName(pp);
                       result.soa.rname:=NewExtractName(pp);
                       result.soa.serial:=WSocket_ntohl(pcardinal(pp)^);    inc(pp,sizeof(cardinal));
                       result.soa.refresh:=WSocket_ntohl(pcardinal(pp)^);   inc(pp,sizeof(cardinal));
                       result.soa.retry:=WSocket_ntohl(pcardinal(pp)^);     inc(pp,sizeof(cardinal));
                       result.soa.expire:=WSocket_ntohl(pcardinal(pp)^);    inc(pp,sizeof(cardinal));
                       result.soa.minimum:=WSocket_ntohl(pcardinal(pp)^);   inc(pp,sizeof(cardinal));
                     end;
    DnsQueryLOC    : begin
                       move(pp^,result.loc,sizeof(TLOCInfo));
                       result.locdecode:=Loc2Geo(result.loc);
                     end;
  end;

  // merken
  frrcache:=result;

  result.valid:=true;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{
  <0><1><129><128><0><1><0><1><0><4><0><5><7>inp
  rise<3>com<0><0><15><0><1><192><12><0>
  <15><0><1><0><1>QV<0><10><0><10><5>drui
  d<192><12><192><12><0><2><0><1><0><1>Qc<0><6><3>
  ns1<192><12><192><12><0><2><0><1><0><1>Qc<0>
  <20><3>NS1<10>SPRINTLINK
  <3>NET<0><192><12><0><2><0><1><0><1>Qc<0>
  <6><3>NS2<192>U<192><12><0><2><0><1><0><1>Q
  c<0><6><3>NS3<192>U<192>+<0><1><0><1><0>
  <1>QV<0><4><143><186><11>F<192>?<0><1><0><1><0>
  <1>Qc<0><4><207>iS<30><192>Q<0><1><0><1><0>
  <2><144>i<0><4><204>u<214><10><192>q<0><1><0><1><0>
  <2><144>i<0><4><199><2><252><10><192><131><0><1><0><1><0>
  <2><142><182><0><4><204>a<212><10>
}
{
  <0><3><129><128><0><1><0><1><0><2><0><3><4>rtf
  m<2>be<0><0><15><0><1><192><12><0><15><0><1><0>
  <1>.b<0><9><0><10><4>mail<192><12><192><12>
  <0><2><0><1><0><1>.b<0><11><2>ns<3>dn
  s<2>be<0><192><12><0><2><0><1><0><1>.b<0>
  <5><2>ns<192><12><192>'<0><1><0><1><0><1>.b
  <0><4><195><0>d<253><192>:<0><1><0><1><0><1>QY
  <0><4><134>:J!<192>Q<0><1><0><1><0><1>.b
  <0><4><195><0>d<253>
}
{
  <0><7><133><128><0><1><0><1><0><2><0><2><3>www
  <4>rtfm<2>be<0><0><1><0><1><192><12><0>
  <1><0><1><0><1>Q<128><0><4><195><0>d<253><4>rt
  fm<2>be<0><0><2><0><1><0><1>Q<128><0><5>
  <2>ns<192>-<192>-<0><2><0><1><0><1>Q<128><0>
  <9><2>ns<3>dns<192>2<192>@<0><1><0><1>
  <0><1>Q<128><0><4><195><0>d<253><192>Q<0><1><0><1>
  <0><0><26><132><0><4><134>:J!
}
(*
<0><1><129><128><0><1><0><1><0><5><0><5><9>fu-berlin
<2>de<0><0>

<29><0><1><192><12><0><29><0><1><0><0>,

<0><16><0><21><22><19><139>Av<167><130><218>L<242>
<0><152><156>\<192><12><0><2><0><1><0><0><12><176>
<0>"<4>arbi<10>informatik<13>uni-oldenburg<2>de<0>
<192><12><0><2><0><1><0><0><12><176><0><12><5>deneb<3>
dfn<192>d<192><12><0><2><0><1><0><0><12><176><0><6><3>
ns3<192><12><192><12><0><2><0><1><0><0><12><176><0><6>
<3>ns2<192><12><192><12><0><2><0><1><0><0><12><176><0>
<6><3>ns1<192><12><192>F<0><1><0><1><0><0>t<169><0><4>
<134>j<1><7><192>t<0><1><0><1><0><0>9<209><0><4><192>L
<176><9><192><140><0><1><0><1><0><0>T<19><0><4><130>
<133><1>9<192><158><0><1><0><1><0><0><28><206><0><4>
<160>-<10><12><192><176><0><1><0><1><0><0>1<198><0>
<4><160>-<8><8>
*)

{ !!KAP!! }
{raw translation of some perl-source LOC.pm from package Net::DNS::RR::LOC;

fu-berlin.de   LOC  52 27 19.591 N 13 17 40.978 E 15.00m 1000.00m 10000.00m 10.00m
}
const conv_sec = 1000.0;
      conv_min = 60.0 * conv_sec;
      conv_deg = 60.0 * conv_min;
      zh31     = 1 shl 31;

procedure SubLOCgeo(longlat : integer;
                    hemis : String;
                    var ldeg, lmin, lsec, lmsec : Extended;
                    var hemic : char);
var
    Labs : Extended;
begin
    LongLat := WSocket_ntohl(LongLat);
    // !!KAP!! 2002-08-31
    if (LongLat<0)
      then Labs    := Abs(1.0 * LongLat - zh31)
      else Labs    := Abs(1.0 * LongLat + zh31);
    //Labs    := Abs(1.0 * LongLat - zh31);
    Ldeg    := Trunc(labs / conv_deg);
    Labs    := Labs - ldeg * conv_deg;
    Lmin    := Trunc(labs / conv_min);
    Labs    := Labs - lmin * conv_min;
    Lsec    := Trunc(labs / conv_sec);
    Labs    := Labs - lsec * conv_sec;
    Lmsec   := Labs;
    Hemic   := Copy(Hemis, 1 + ord(LongLat <= zh31), 1)[1]; { yeah. }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LongLatToDMS(longlat : longint; hemis : string):string;
Var ldeg, lmin, lsec, lmsec : extended;
    hemi                    : char;
begin
  SubLOCgeo(longlat,hemis,ldeg,lmin,lsec,lmsec,hemi);
  result := Format('%d %02d %02d.%02.2d',
               [round(ldeg), round(lmin), round(lsec),
                round(lmsec)]) + ' ' + hemi;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ in cm!! }
function LocAltToAlt(Localt : LongInt) : LongInt;
begin
    Result := Round((WSocket_ntohl(localt) - 100000.0 * 100.0) / 100.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ !!KAP!! }
function Loc2Geo(loc : TLOCInfo):TLogGeo;
  { dolle umwandlung }
  procedure du(longlat : Integer;
               hemis   : String;
               var ideg, imin, isec, imsec : Integer;
               var hemic : Char);
  var
      ldeg, lmin, lsec, lmsec : extended;
  begin
      SubLOCgeo(longlat, hemis, ldeg, lmin, lsec, lmsec, hemic);
      ideg  := Round(ldeg);
      imin  := Round(lmin);
      isec  := Round(lsec);
      imsec := Round(lmsec);
  end;

begin
    Result.version  := Loc.version;
    Result.longsize := Round(Exp(Ln(10)*(loc.size and $f)));
    Result.latsize  := Round(Exp(Ln(10)*(loc.size shr 4)));

    Result.horizpre := Loc.horizpre;
    Result.vertpre  := Loc.vertpre;

    du(loc.latitude, 'NS', result.lad, result.lam,
       result.las, result.lams, result.lahem);
    du(loc.longitude, 'EW', result.lod, result.lom,
       result.los, result.loms, result.lohem);

    Result.altitude := LocAltToAlt(loc.altitude);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
