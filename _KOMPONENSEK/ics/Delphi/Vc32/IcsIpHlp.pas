{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  ICS IP protocol helper
Creation:     March 20, 2004
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
                                           francois.piette@pophost.eunet.be
Credit:       Thanks to Alfred Mirzagitov <alfred@softsci.com> who wrote an
              article in The Delphi Magazine issue 101 (January 2004) about
              using raw socket with Delphi. I used some of his ideas here.
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2005 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsIpHlp;

{$I ICSDEFS.INC}
{$IFDEF VER80}
    Bomb('Sorry, this code doesn''t work with Delphi 1');
{$ENDIF}

interface

uses
    Windows, WinSock;

const
    IcsIpHlpVersion        = 100;
    CopyRight : String     = ' IcsIpHlp (c) 2004-2005 F. Piette V1.00 ';

type
    // IP header (RFC 791)
    TIcsIpHdr = packed record
        ihl_ver  : BYTE;      // Combined field:
                              //   ihl:4 - IP header length divided by 4
                              //   version:4 - IP version
        tos      : BYTE;      // IP type-of-service field
        tot_len  : WORD;      // total length
        id       : WORD;      // unique ID
        frag_off : WORD;      // Fragment Offset + fragmentation flags (3 bits)
        ttl      : BYTE;      // time to live
        protocol : BYTE;      // protocol type
        check    : WORD;      // IP header checksum
        saddr    : DWORD;     // source IP
        daddr    : DWORD;     // destination IP
       {The options start here...}
    end;
    PIcsIpHdr = ^TIcsIpHdr;

    // TCP header (RFC 793)
    TIcsTcpHdr = packed record
        source  : WORD;       // source port
        dest    : WORD;       // destination port
        seq     : DWORD;      // sequence number
        ack_seq : DWORD;      // next sequence number
        flags   : WORD;       // Combined field:
                              //   res1:4 - reserved, must be 0
                              //   doff:4 - TCP header length divided by 4
                              //   fin:1  - FIN
                              //   syn:1  - SYN
                              //   rst:1  - Reset
                              //   psh:1  - Push
                              //   ack:1  - ACK
                              //   urg:1  - Urgent
                              //   res2:2 - reserved, must be 0
        window  : WORD;       // window size
        check   : WORD;       // checksum, computed later
        urg_ptr : WORD;       // used for async messaging?
    end;
    PIcsTcpHdr = ^TIcsTcpHdr;

    // UDP header (RFC 768)
    TIcsUdpHdr = packed record
        src_port : WORD;      // source port
        dst_port : WORD;      // destination port
        length   : WORD;      // length, including this header
        checksum : WORD;      // UDP checksum
    end;
    PIcsUdpHdr = ^TIcsUdpHdr;

const
    // ICMP types
    ICS_ICMP_ECHOREPLY                   = 0;
    ICS_ICMP_UNREACH                     = 3;
    ICS_ICMP_SOURCEQUENCH                = 4;
    ICS_ICMP_REDIRECT                    = 5;
    ICS_ICMP_ECHO                        = 8;
    ICS_ICMP_ROUTERADVERT                = 9;
    ICS_ICMP_ROUTERSOLICIT               = 10;
    ICS_ICMP_TIMXCEED                    = 11;
    ICS_ICMP_PARAMPROB                   = 12;
    ICS_ICMP_TSTAMP                      = 13;
    ICS_ICMP_TSTAMPREPLY                 = 14;
    ICS_ICMP_IREQ                        = 15;
    ICS_ICMP_IREQREPLY                   = 16;
    ICS_ICMP_MASKREQ                     = 17;
    ICS_ICMP_MASKREPLY                   = 18;

    // ICMP codes
    ICS_ICMP_UNREACH_NET                 = 0;
    ICS_ICMP_UNREACH_HOST                = 1;
    ICS_ICMP_UNREACH_PROTOCOL            = 2;
    ICS_ICMP_UNREACH_PORT                = 3;
    ICS_ICMP_UNREACH_NEEDFRAG            = 4;
    ICS_ICMP_UNREACH_SRCFAIL             = 5;
    ICS_ICMP_UNREACH_NET_UNKNOWN         = 6;
    ICS_ICMP_UNREACH_HOST_UNKNOWN        = 7;
    ICS_ICMP_UNREACH_ISOLATED            = 8;
    ICS_ICMP_UNREACH_NET_PROHIB          = 9;
    ICS_ICMP_UNREACH_HOST_PROHIB         = 10;
    ICS_ICMP_UNREACH_TOSNET              = 11;
    ICS_ICMP_UNREACH_TOSHOST             = 12;
    ICS_ICMP_UNREACH_FILTER_PROHIB       = 13;
    ICS_ICMP_UNREACH_HOST_PRECEDENCE     = 14;
    ICS_ICMP_UNREACH_PRECEDENCE_CUTOFF   = 15;
    ICS_ICMP_REDIRECT_NET                = 0;
    ICS_ICMP_REDIRECT_HOST               = 1;
    ICS_ICMP_REDIRECT_TOSNET             = 2;
    ICS_ICMP_REDIRECT_TOSHOST            = 3;
    ICS_ICMP_TIMXCEED_INTRANS            = 0;
    ICS_ICMP_TIMXCEED_REASS              = 1;
    ICS_ICMP_PARAMPROB_OPTABSENT         = 1;

type
    TIcsNetTime = LONGWORD;          // Warning: network byte order

    TIcsIcmpEcho = packed record
        id  : WORD;                  // to match requests with replies
        seq : WORD;                  // to match requests with replies
    end;
    PIcsIcmpEcho = ^TIcsIcmpEcho;

    TIcsIcmpFrag = packed record
        pad : WORD;
        mtu : WORD;
    end;
    PIcsIcmpFrag = ^TIcsIcmpFrag;

    TIcsIcmpTs = packed record
        otime : TIcsNetTime;
        rtime : TIcsNetTime;
        ttime : TIcsNetTime;
    end;
    PIcsIcmpTs = ^TIcsIcmpTs;

    // ICMP packet header
    TIcsIcmpHdr = packed record
        icmp_type : BYTE;
        icmp_code : BYTE;
        icmp_sum  : WORD;
        icmp_hun  : packed record
          case Integer of
          0: (echo    : TIcsIcmpEcho);
          1: (gateway : TInAddr);
          2: (frag    : TIcsIcmpFrag);
          end;
        icmp_dun: packed record
          case Integer of
            0: (ts   : TIcsIcmpTs);
            1: (mask : LONGWORD);
            2: (data : array [0..SizeOf(TIcsIcmpTs) - 1] of Char);
          end;
        // More data follow in the packet
    end;
    PIcsIcmpHdr = ^TIcsIcmpHdr;

implementation

end.
