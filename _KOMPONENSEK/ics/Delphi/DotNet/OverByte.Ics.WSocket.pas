{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TWSocket class encapsulate the Windows Socket paradigm
Creation:     Dec 2003 from win32 version created april 1996
Version:      6.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2005 by François PIETTE
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
Jul 18, 1996  Move all low level socket to winsock to be Delphi 2.x compatible
Sep 18, 1996  Use structured exception for handling errors
Sep 19, 1996  Check csDestroying before invoking event handler
Nov 04, 1996  Better error handling
Jan 31, 1997  Changed property assignation for Addr, Port and Proto
              Added notification handler
Feb 14, 1997  Corrected bug in property assignation for Addr, Port and Proto
Mar 26, 1997  Make UDP protocol work correctly
              Enable UDP broadcasting by using addr 255.255.255.255
Apr 1, 1997   Added class function when independent of any open socket
              Moved InitData as global
              Added ReceivedFrom function
              Added ResolveHost function
Jul 22, 1997  Adapted to Delphi 3 which has a modified winsock.accept
Aug 13, 1997  'sin' member made public
Aug 24, 1997  Create the only help
              Makes writing HSocket the same as calling Dup.
Sep 5, 1997   Version 2.01, added WinsockInfo function
Sep 21, 1997  Version 2.02, make it really thread safe
                            created global WSocketVersion
Sep 25, 1997  Version 2.04, port to C++Builder
Sep 27, 1997  Version 2.05. All class methods converted to global
              procedure or function because C++Builder do not like
              class method very much.
              Old class method              New global function
              ----------------              -------------------
              WinsockInfo                   WinsockInfo
              SocketErrorDesc               WSocketErrorDesc
              GetHostByAddr                 WSocketGetHostByAddr
              GetHostByName                 WSocketGetHostByName
              ResolveHost                   WSocketResolveHost
              HostName                      LocalHostName
Oct 02, 1997  V2.06 Added a check in destructor to avoid calling WSACleanup at
              design time which crashes the excellent Eagle Software CDK.
Oct 16, 1997  V2.07 Added PortNum property with numeric value for Port.
              Added RcvdCount property to return the number of
              characters received in the buffer but not read yet. Do not
              confuse with ReadCount which returns the number of chars
              already received.
              Added a check for FWait assignation in front of ReadLine
              Prefixed each TSocketState value by 'ws' to avoid name conflict.
              Moved FHSocket member to private section because the property
              HSocket does the right job.
              Added a check for state closed when changing Port, Proto and Addr.
Oct 22, 1997  V2.08 Added Flush method (asked by john@nexnix.co.uk) and
              FlushTimeout property (default to 60 seconds).
Oct 22, 1997  V2.09 Added SendFlags property to enable sending in or out of
              band data (normal or urgent, see RFC-1122)
Oct 28, 1997  V2.10 Added an OnLineTooLong event and code to handle the case
              where ReadLine has been called and the buffer overflowed (line
              long)
Oct 29, 1997  V2.11 Added DnsLookup functionnality (DnsLookup method, DnsResult
              property and DnsLookupDone event).
              Calling the connect method with a hostname work well except that
              it could block for a long period (ie: 2 minutes) if DNS do not
              respond. Calling the connect method with a numeric IP address will
              never block. So you can call DnsLookup to start hostname
              resolution in the background, after some time you evenutually
              receive the OnDnsLookupDone event. The copy the DnsResult property
              to the Addr property and call connect.
Oct 30, 1997  V2.12 added a check in DnsLookup to handel numeric IP which do
              not require any lookup. The numeric IP is treated immediately
              and immediately trigger the DnsLookupDone event.
              I modified the code to be compatible with Delphi 1.
Oct 31, 1997  V2.13 added CancelDnsLookup procedure.
Nov 09, 1997  V2.14 add LocalIPList function to get the list of local IP
              addresses (you have two IP addresses when connected to a LAN
              and an ISP).
Nov 11, 1997  V2.15 Made TCustomWSocket with virtual functions. This will
              allow to easily descend a new component from TCustomWSocket.
              Make ReadLine stop when the connection is broken.
Nov 12, 1997  V2.16 Corrected bug (Justin Yunke <yunke@productivity.org>)
              in LocalIPList: phe should be checked for nil.
Nov 18, 1997  Added ReceiveStr function (Suggested by FLDKNHA@danisco.com)
Nov 30, 1997  V2.18 Added a call to OnDnsLookupDone when canceling.
Dec 04, 1997  V2.19 Added LocalPort property and SessionConnected event
              for UDP socket.
              V2.20 Modified MessageLoop and ProcessMessages to process not
              only the socket messages, but all messages (necessary if the
              thread has several TWSocket for example).
Dec 09, 1997  V2.21 Corrected a minor bug in ReceiveStr. Detected by
              david@e.co.za (David Butler).
Dec 10, 1997  V2.22 Corrected a minor bug in Send which now correctly
              returns the number of bytes sent. Detected by
              james.huggins@blockbuster.com
Dec 16, 1997  V2.23 Corrected a bug which prevented the receiving of datagram
              from a UDP socket.
              Thank to Mark Melvin (melvin@misrg.ml.org) for pointing it.
Dec 20, 1997  V2.24 Added the PeekData function as suggested by Matt Rose
              mcrose@avproinc.com
Dec 26, 1997  V2.25 Added the Text property as suggested by Daniel P. Stasinski
              <dse@pacific.net>. Made GetXPort work even when listening as
              suggested by is81024@cis.nctu.edu.tw.
Jan 10, 1998  V2.26 Check for null hostname in DNSLookup
              Added DnsResultList with all IP addresses returned form DNS
Jan 13, 1998  V2.27 a Added MultiThreaaded property to tell the component that
              it is working in a thread and should take care of it (call
              internal ProcessMessages in place of Application.ProcessMessages,
              and do not use the WaitCtrl object).
Jan 15, 1998  V2.28 WMAsyncSelect revisited to work properly with NT winsock 2.
Feb 10, 1998  V2.29 Added an OnError event. If not assigned, then the component
              raise an exception when the error occurs.
Feb 14, 1998  V2.30 Published Text property
Feb 16, 1998  V2.31 Added virtual methods to trigger events
              Renamed all event handler variable to begin with FOn
Feb 26, 1998  V2.32 Added procedure PutDataInSendBuffer and PutStringInSendBuffer
              Using PutDataInSendBuffer you can place data in the send buffer
              without actualy trying to send it. This allows to place several
              (probably small) data chunk before the component attempt to send
              it. This prevent small packet to be sent. You can call
              Send(nil, 0) to force the component to begin to send data.
              If the buffer was not empty, PutDataInSendBuffer will just queue
              data to the buffer. This data will be sent in sequence.
Mar 02, 1998  V2.33 Changed the error check with WSAstartup as pointed out by
              Donald Strenczewilk (dstrenz@servtech.com)
Mar 06, 1998  V2.34 Added a runtime property to change the buffer size.
Mar 27, 1998  V2.35 Adapted for C++Builder 3
Apr 08, 1998  V2.36 Made SetDefaultValue virtual
Apr 13, 1998  V2.37 Reset FDnsLookupHandle to 0 after a failed call to
              WSACancelAsyncRequest
Apr 22, 1998  V2.38 Published AllSent property to let outside know if our
              buffer has some data unsent.
Apr 28, 1998  V2.39 Added LingerOnOff and LingerTimeout. Default values are
              wsLingerOn and timeout = 0 to behave by default as before.
              This value is setup just before Connect. Call SetLingerOption to
              set the linger option on the fly (the connection must be
              established to set the option). See winsock.closesocket on line
              help (winsock.hlp or win32.hlp) for a dsicussion of this option
              usage.
May 06, 1998  V2.40 Added a workaround for Trumpet winsock inet_addr bug.
              Thanks to Andrej Cuckov <andrej@cuckov.com> for his code.
May 18, 1998  V2.41 Jan Tomasek <xtomasej@feld.cvut.cz> found that Trumpet
              Winsock (Win 3.11) has some bugs and suggested a workaround in
              TryToSend procedure. This workaround makes TWSocket blocking in
              some cases. A new property enables the workaround. See code.
Jun 01, 1998  V2.42 In finalization section, check for not assigned IPList.
Jun 15, 1998  V2.43 Added code to finalization section to unload winsock if
              still loaded at that point (this happend if no socket where
              created but WinsockInfo called). Suggested by Daniel Fazekas
              <fdsoft@dns.gyor-ph.hu>
Jun 27, 1998  V2.44 Added checks for valid arguments in SetPort, SetProto
              and SetAddr. Deferred address resolution until Connect or Listen.
Jul 08, 1998  V2.45 Adadpted for Delphi 4
Jul 20, 1998  V2.46 Added SetWindowLong(FWindowHandle, 0, 0) in the destructor
              and a check for TWSocket class in XSocketWindowProc.
              Added virtual method RealSend.
Jul 23, 1998  V2.47 Added a TriggerSessionClosed from TryToSend in case of
              send error. This was called before, but with a nul error argument.
              Now it correctly gives the error number.
              Added a trashcan to receive data if no OnDataAvailable event
              handler is installed. Just receive the data and throw it away.
              Added reverse dns lookup asynchronous code (IP -> HostName).
              Thanks to Daniel Fazekas <fdsoft@dns.gyor-ph.hu> for his code.
Jul 30, 1998  V2.48 Changed local variable "error" by FLastError in SocketError
              to make it available from the OnError handler. Thanks to
              dana@medical-info.com for finding this bug.
              In Abort procedure, deleted all buffered data because it was send
              the next time the socket is opened !
              Added CancelDnsLookup in Abort procedure.
Aug 28, 1998  V2.49 Made InternalClose and ReceiveStr virtual
Sep 01, 1998  V2.50 Ignore CancelDnsLookup exception during destroy
Sep 29, 1998  V2.51 In InternalClose, protect AssignDefaultValue with
              try/except because SessionClosed event handler may have destroyed
              the component.
Oct 11, 1998  V2.52 Changed Shutdown(2) to Shutdown(1) in Internal Close to
              prevent data lost on send. You may have to call Shutdown(2) in
              your own code before calling Close to have the same behaviour as
              before.
              Changed argument type for ASyncReceive and passed 0 from FD_CLOSE
              message handler.
Oct 28, 1998  V2.53 Made WSocketLoadWinsock and WSocketUnloadWinsock public.
Nov 11, 1998  V2.54 Added OnDisplay event for debugging purpose
Nov 16, 1998  V2.55 Ignore WSANOTINITIALIZED error calling CloseSocket. This
              occurs when using TWSocket from a DLL and the finalization
              section is called before destroying TWSocket components (this is
              a program logic error).
              Made some properties and methods protected instead of private.
              Made some methods virtual.
              Added an Error argument to InternalClose.
              Added DoRecv virtual function.
              Added WSocketResolvePort
              Added WSocketResolveProto
              Deferred port and protocol resolution until really needed
              Transformed Listen to procedure (in case of failure Listen
              always calls SocketError which triggers an exception or the
              OnError event).
Nov 22, 1998  V3.00 Skipped from V2.55 to V3.00. Socks support is major update!
              Added SOCKS5 (RFC-1928) support for TCP connection and
              simple usercode passwword authentication.
              Consider the socks code as beta !
              New properties: SocksServer, SocksPort, SocksUsercode,
              SocksPassword, FSocksAuthentication. New events: OnSocksError,
              OnSocksConnected, OnSocksAuthState.
              I used WinGate 2.1d to test my code. Unfortunately WinGate do
              not correctly handle user authentication, so the code here is
              just untested...
Dec 05, 1998  V3.10 Removed ReadLine feature using TWait component.
              Added new TCustomLineWSocket and TCustomSyncWSocket.
              Those modifications implies that the ReadLine functionnality is
              slightly changed. Notably, the end of line marker is now
              configurable and remains in the received line unless a timeout
              occurs or the buffer is too small.
Dec 10, 1998  V3.11 Added missing code to resolve port in the Listen method.
Dec 12, 1998  V3.12 Added write method for LocalPort property. Thanks to
              Jan Tomasek <xtomasej@feld.cvut.cz> for his code.
              Added background exception handling.
              Fixed a bug in TCustomLineWSocket.TriggerDataAvailable which was
              not calling the inherited function when it actually should.
              Added a check on multithreaded in WaitForClose to call the
              correct ProcessMessages procedure.
              Added SOCKS4 support (only tcp connect is supported).
Dec 28, 1998  V3.13 Changed WSocketResolveHost to check for invalid numeric
              IP addresses whitout trying to use them as hostnames.
Dec 30, 1998  V3.14 Changed SetPort to SetRemotePort to solve the SetPort
              syndrome with BCB. Also chnaged GetPort to be consistant.
Jan 12, 1999  V3.15 Introduced DoRecvFrom virtual function. This correct a bug
              introduced in V3.14 related to UDP and RecvFrom.
Jan 23, 1999  V3.16 Changed FRcvdFlag computation in DoRecv and DoRecvFrom
              because it caused problems with HTTP component and large blocks.
              Removed modification by Jan Tomasek in TriggerDataAvailable
Jan 30, 1999  V3.17 Added WSocketResolveIp function.
              Checked for tcp protocol before setting linger off in abort.
              Moved a lot of variables from private to protected sections.
              Removed check for Assigned(FOnDataSent) in WMASyncSelect.
Feb 03, 1999  V3.18 Removed useless units in the uses clause.
Feb 14, 1999  V4.00 Jump to next major version number because lots of
              fundamental changes have been done. See below.

              Use runtime dynamic link with winsock. All winsock functions
              used by TWSocket are linked at runtime instead of loadtime. This
              allows programs to run without winsock installed, provided program
              doesn't try to use TWSocket or winsock function without first
              checking for winsock installation.
              Removed WSocketLoadWinsock and all use to DllStarted because it
              is no longer necessary because winsock is automatically loaded
              and initialized with the first call to a winsock function.

              Added MessagePump to centralize call to the message pump.
              It is a virtual procedure so that you can override it to
              cutomize your message pump. Also changed slightly ProcessMessages
              to closely match what is done in the forms unit.

              Removed old stuff related to WaitCtrl (was already excluded from
              compilation using a conditional directive).

              Added NOFORMS conditional compilation to exclude the Forms unit
              from wsocket. This will reduce exe or dll size by 100 to 150KB.
              To use this feature, you have to add NOFORMS in your project
              options in the "defines" edit box in the "directory/conditional"
              tab. Then you must add a message pump to your application and
              call it from TWSocket.OnMessagePump event handler. TWSocket really
              need a message pump in order to receive messages from winsock.
              Depending on how your application is built, you can use either
              TWSocket.MessageLoop or TWSocket.ProcessMessages to quickly build
              a working message pump. Or you may build your own custom message
              pump taylored to your needs. Your message pump must set
              TWSocket.Terminated property to TRUE when your application
              terminates or you may experience long delays when closing your
              application.
              You may use NOFORMS setting even if you use the forms unit (GUI
              application). Simply call Application.ProcessMessages in the
              OnMessagePump event handler.
              OnMessagePump event is not visible in the object inspector. You
              must assign it at run-time before using the component and after
              having created it (in a GUI application you can do that in the
              FormCreate event, in a console application, you can do it right
              after TWSocket.Create call).
Feb 17, 1999  V4.01 Added LineEcho and LineEdit features.
Feb 27, 1999  V4.02 Added TCustomLineWSocket.GetRcvdCount to make RcvdCount
              property and ReceiveStr work in line mode.
Mar 01, 1999  V4.03 Added conditional compile for BCB4. Thanks to James
              Legg <jlegg@iname.com>.
Mar 14, 1999  V4.04 Corrected a bug: wsocket hangup when there was no
              OnDataAvailable handler and line mode was on.
Apr 21, 1999  V4.05 Added H+ (long strings) and X+ (extended syntax)
              compilation options
May 07, 1999  V4.06 Added WSAECONNABORTED to valid error codes in TryToSend.
Jul 21, 1999  V4.07 Added GetPeerPort method, PeerPort and PeerAddr propertied
              as suggested by J. Punter <JPunter@login-bv.com>.
Aug 20, 1999  V4.05 Changed conditional compilation so that default is same
              as latest compiler (currently Delphi 4, Bcb 4). Should be ok for
              Delphi 5.
              Added LocalAddr property as suggested by Rod Pickering
              <fuzzylogic123@yahoo.com>. LocalAddr default to '0.0.0.0' and is
              intended to be used by a client when connecting to a server, to
              select a local interface for multihomed computer. Note that to
              select an interface for a server, use Addr property before
              listening.
              LocalAddr has to be an IP address in dotted form. Valid values are
              '0.0.0.0' for any interface, '127.0.0.1' for localhost or any
              value returned by LocalIPList.
              Replaced loadtime import for ntohs and getpeername by runtime
              load.
              Revised check for dotted numeric IP address in WSocketResolveHost
              to allow correct handling of hostnames beginning by a digit.
              Added OnSendData event. Triggered each time data has been sent
              to winsock. Do not confuse with OnDataSent which is triggered
              when TWSocket internal buffer is emptyed. This event has been
              suggested by Paul Gertzen" <pgertzen@livetechnology.com> to
              easyly implement progress bar.
              Corrected WSocketGetHostByAddr to make it dynamically link to
              winsock.
Sep 5, 1999   V4.09 Added CloseDelayed method.
              Make sure that TriggerSessionClosed is called from WMASyncSelect
              and InternalClose, even if there is no OnSessionClosed event
              handler assigned. This is required to make derived components
              work correctly.
              Created message WM_TRIGGER_EXCEPTION to help checking background
              exception handling (OnBgException event).
              Corrected bug for Delphi 1 and ReallocMem.
Oct 02, 1999  V4.10 Added Release method.
Oct 16, 1999  V4.11 Corrected a bug in TCustomLineWSocket.DoRecv: need to move
              data in front of buffer instead of changing buffer pointer which
              will crash the whole thing at free time.
Oct 23, 1999  V4.12 Made WSocketIsDottedIP a public function
Nov 12, 1999  V4.13 removed 3 calls to TriggerSocksAuthState because it was
                    called twice. By A. Burlakov <alex@helexis.com>.
Jan 24, 1999  V4.14 Call Receive instead of DoRecv from ReceiveStr to be sure
              to set LastError correctly. Thanks to Farkas Balazs
              <megasys@www.iridium.hu>
              Suppressed FDllName and used winsocket constant directly. I had
              troubles with some DLL code and string handling at program
              termination.
Apr 09, 2000 V4.15 Added error number when resolving proto and port
Apr 29, 2000 V4.16 Added WSocketForceLoadWinsock and
             WSocketCancelForceLoadWinsock. Thanks to Steve Williams.
             Created variable FSelectEvent to store current async event mask.
             Added ComponentOptions property with currently only one options
             wsoNoReceiveLoop which disable a receive loop in AsyncReceive.
             This loop breaking was suggested by Davie <smatters@smatters.com>
             to lower resource usage with really fast LAN and large transfers.
             By default, this option is disabled so there is no change needed
             in current code.
May 20, 2000 V4.17 Made TSocket = u_int (same def as in winsock.pas)
             Moved bind after setting options.
             Thanks to Primoz Gabrijelcic <fab@siol.net>
Jul 15, 2000 V4.18 Alon Gingold <gingold@hiker.org.il> changed
             TCustomSocksWSocket calls to inherited triggers of
             TriggerSessionConnected and TriggerDataAvailable.
             Now, it calls the trigger directly. This solves the problem
             of descendent classes with overriden triggers, not being
             called when a REAL connection was established, and when real
             data starts coming in. Special care MUST be taken in such
             overridden triggers to ONLY call the inherited trigger AND
             IMMEDIATELY EXIT when FSocksState <> socksData to avoid loopback
Jul 22, 2000 V4.19 John Goodwin <john@jjgoodwin.com> found a failure in the
             logic for DnsLookup. He also implemented a workaround.
             See DnsLookup comments for explanation.
Aug 09, 2000 V4.20 Alon Gingold <gingold2@mrm-multicat.com> found a bug in
             SOCKS4 implementation where a nul byte was incorrectly added
             (it should be added only with SOCKS4A version, not straith
             SOCKS4).
Sep 17, 2000 V4.21 Eugene Mayevski <Mayevski@eldos.org> added TWndMethod for
             NOFORMS applications in other components.
Oct 15, 2000 V4.22 Added method GetXAddr which returns local IP address to
             which a socket has been bound. There was already a GetXPort.
             Thanks to Wilfried Mestdagh <wilfried_sonal@compuserve.com>
             and Steve Williams <stevewilliams@kromestudios.com>.
Nov 08, 2000 V4.23 Moved FSelectEvent from private to protected section.
Nov 11, 2000 V4.24 Added LineLimit property and OnLineLimitExceeded event.
             When using line mode, line length is checked as each data block is
             comming. If the length is greater than the limit, then the event
             is triggered. You have the opportunity to close the socket or
             change the limit to a higher value. Thus you can prevent a hacker
             from locking your system by sending unlimited line which otherwise
             would eat up all system resources.
             Changed line handling variables to LongInt
             Checked all length involved in StrPCopy calls.
Nov 26, 2000 V4.25 Do not trust GetRcvdCount. Always call Receive to check for
             incomming data (sometime NT4 will hang if we don't do that).
Jan 24, 2001 V4.26 Blaine R Southam <bsoutham@iname.com> fixed out of bound
             error in TCustomLineWSocket.TriggerDataAvailable
Feb 17, 2001 V4.27 Davie <smatters@smatters.com> fixed a bug causing byte lost
             when closing (related to wsoNoReceiveLoop option).
May 04, 2001 V4.28 Fixed W2K bug (winsock message ordering)
Jun 18, 2001 V4.29 Added AllocateHWnd and DeallocateHWnd from Forms unit to
             avoid warning from Delphi 6 in all other components.
Jul 08, 2001 V4.30 Fixed small bug related to NOFOMRS and V4.29
Jul 26, 2001 V4.31 Checked csDesigning in GetRcvdCount so that Delphi 6 does'nt
             crash when object inspector wants to display RcvdCount value.
             Added multicast capability and UDP ReuseAddr. Thanks to Mark
             G. Lewis <Lewis@erg.sri.com> for his code.
             Added TriggerSessionClosed to SocketError as suggested by Wilfried
             Mestdagh <wilfried_sonal@compuserve.com>
Jul 28, 2001 V4.32 New option wsoTcpNoDelay implemented. Code by Arnaldo Braun
             <abraun@th.com.br>
Jul 30, 2001 V4.33 Corrected at few glitches with Delphi 1
Sep 08, 2001 V4.34 Added ThreadAttach and related functions
Nov 27, 2001 V4.35 Added type definition for in_addr and Delphi 2 (Yes there are
             still some peoples who wants to use it. Don't ask me why !).
Dec 02, 2001 V4.36 david.brock2@btinternet.com found a bug in SOCKS4 where
             error check incorrectly checked "FRcvBuf[1] = #$90" instead of
             "FRcvBuf[1] <> #90". He also found a bug when receiving domain name
             where length of name was incorrectly copyed to the buffer.
Dec 23, 2001 V4.37 Removed bWrite, nMoreCnt, bMoreFlag and nMoreMax which where
             not more really used. Thanks to Al Kirk <akirk@pacific.net> for
             showing that.
Feb 24, 2002 V4.38 Wilfried Mestdagh <wilfried@mestdagh.biz> added ThreadDetach
             and a property editor for LineEnd. XSocketDeallocateHWnd made a
             function.
             I created a new unit WSocketE.pas to put Wilfried's property
             editor so that it works correctly with Delphi 6.
Apr 24, 2002 V4.39 Removed OnLineTooLong event which was not used anywhere.
             Use OnLineLimitExceeded event if you used this event.
             Thanks to Alex Kook <cookis@mail.ru> for finding this one.
Apr 27, 2002 V4.40 Added procedure WSocketUnregisterClass to be able to
             unregister hidden window. This is necessary when TWSocket is
             used within a DLL which is unloaded and reloaded by applications,
             specially when running with Windows-XP. Thanks to Jean-Michel Aliu
             <jmaliu@jmasoftware.com> who provided a test case.
Jun 02, 2002 V4.41 allow SOCK_RAW in Connect method for any protocol which is
             not TCP or UDP. Thanks to Holger Lembke <holger@hlembke.de>.
Jun 04, 2002 V4.42 Do not call Listen for SOCK_RAW.
             Thanks to Holger Lembke <holger@hlembke.de>.
Jun 08, 2002 V4.43 Add a dummy Register procedure for BCB1.
             Thanks to Marc-Alexander Prowe <listen@mohajer.de>.
Jul 07, 2002 V4.44 Added code in Connect method to check if socket still opened
             after OnChangeState event. If not, trigger an error WSAINVAL.
Sep 16, 2002 V4.45 Exposed RcvdPtr and RcvdCnt readonly properties.
Sep 17, 2002 V4.46 Used InterlockedIncrement/InterlockedDecrement to Inc/Dec
             socket count safely when TWSocket is used within a thread. This
             was proposed by Matthew Meadows <matthew.meadows@inquisite.com>
Sep 28, 2002 V4.47 Changed DnsLookup so that a hostname is checked for dotted
             IP addresse and resolve it numerically. Thanks to Bogdan Calin
             <soul4blade@yahoo.com> who found this bug. Alos loaded the result
             list with the address to be consistant with real lookup result.
Nov 17, 2002 V4.48 Roland Klabunde <roland.klabunde@gmx.net> found a bug in
             multicast code: listening on a specific interface was ignored.
             He fixed Listen and Connect.
Nov 27, 2002 V4.49 Added ListenBacklog property, default to 5.
Dec 17, 2002 V4.50 Moved code to virtual function to permit SSL implementation.
Jan 19, 2003 V5.00 First pre-release for ICS-SSL. New major version number
             V5.01 Gabi Slonto <buffne01@gmx.net> found a bug in DnsLookup
             when hostname was actulally a dotted IP address.
Mar 18, 2003 V5.02 Fixed WSocketIsDottedIP: reordering of boolean expressions
             involaving a string. Thanks to Ian Baker <ibaker@codecutters.org>
Apr 30, 2003 V5.03 Replaced all calls to setsockopt by calls to
             WSocket_setsockopt to avoid statically linked winsock DLL.
             Thanks to Piotr Dalek <enigmatical@interia.pl>.
             Also replaced inet_addr by WSocket_iniet_addr.
Aug 27, 2003 V5.04 Marco van de Voort <marcov@stack.nl> added FreePascal (FPC)
             conditional compilation. Please contact him for any FPC support
             question.
Aug 28, 2003 V5.05 Fixed a multithreading issue related to windows class
             registration. Now using a critical section around the code.
             Thanks to Bogdan Ureche <bureche@omnivex.com> for his precious help.
Aug 31, 2003 V5.06 Added warning about deprecated procedures Synchronize,
             WaitUntilReady and ReadLine. Do not use them in new applications.
Sep 03, 2003 V5.07 Bogdan Ureche <bureche@omnivex.com> added a critical section
             to avoid problem when winsock.dll is unloaded by a thread while
             another thread is still using some TWSocket.
Sep 15, 2003 V5.08 Fixed finalization section to no free critical section if
             a TWSocket is still existing. This happend for example when a
             TWSocket is on a form and Halt is called from FormCreate event.
             Changed SendStr argument to const.
Nov 09, 2003 V5.09 Added manifest constants for Shutdown
             Added TCustomLineWSocket.SendLine method.
Dec 07, 2003 V6.00 Pre-release for Delphi 8 for .NET framework


About multithreading and event-driven:
    TWSocket is a pure asynchronous component. It is non-blocking and
    event-driven. It means that when you request an operation such as connect,
    the component start the operation your requested and give control back
    immediately while performing the operation in the background automatically.
    When the operation is done, an event is triggered (such as
    OnSessionConnected if you called Connect).

    This asynchronous non-blocking behaviour is very high performance but a
    little bit difficult to start with. For example, you can't call Connect and
    immediately call SendStr the line below. If you try, you'll have an
    exception triggered saying you are not connected. Calling connect will start
    connection process but will return long before connection is established.
    Calling SendStr at the next line will not work because the socket is not
    connected yet. To make it works the right way, you have to put your SendStr
    in the OnSessionConnected event.

    The asynchronous operation allows you to do several TCP/IP I/O
    simultaneously. Just use as many component as you need. Each one will
    operate independently of the other without blocking each other ! So you
    basically don't need multi-threading with TWSocket, unless YOUR processing
    is lengthy and blocking.

    If you have to use multithreading, you have two possibilities:
    1) Create your TWSocket from your thread's Execute method
    2) Attach a TWSocket to a given thread using ThreadAttach.
    In both cases, you must set MultiThreaded property to TRUE.
    If you don't use one of those methods, you'll end up with a false
    multithreaded program: all events will be processed by the main tread !
    For both methods to work, you MUST have a message loop withing your thread.
    Delphi create a message loop automatically for the main thread (it's in
    the Forms unit), but does NOT create one in a thread ! For your convenience,
    TWSocket has his own MessageLoop procedure. You can use it from your thread.

    Sample program MtSrv uses first method while ThrdSrv uses second method.
    Sample program TcpSrv is much the same as ThrdSrv but doesn't use any
    thread. You'll see that it is able to server a lot of simultaneous clients
    as well and it is much simpler.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverByte.Ics.WSocket platform;

{$ALIGN 1}

interface

{$DEFINE NOFORMS}

uses
    System.Runtime.InteropServices,
    System.Threading,
    System.Text,
    Borland.Vcl.Windows,
    Borland.Vcl.Messages,
    Borland.Vcl.Classes,
    Borland.Vcl.WinUtils,
    Borland.Vcl.SysUtils,
    Overbyte.Ics.Component,
    OverByte.Ics.WSockBuf,
    OverByte.Ics.WinSock;

const
    WSocketVersion            = 600;
    CopyRight    : String     = ' TWSocket (c)1996-2005 Francois Piette V6.00 ';
    WM_ASYNCSELECT            = WM_USER + 1;
    WM_ASYNCGETHOSTBYNAME     = WM_USER + 2;
    WM_ASYNCGETHOSTBYADDR     = WM_USER + 3;
    WM_CLOSE_DELAYED          = WM_USER + 4;
    WM_WSOCKET_RELEASE        = WM_USER + 5;
    WM_TRIGGER_EXCEPTION      = WM_USER + 6;
    WM_TRIGGER_DATA_AVAILABLE = WM_USER + 20;
    WSA_WSOCKET_TIMEOUT       = 12001;

type
    TWndMethod           = procedure(var Message: TMessage) of object;
    ESocketException     = class(Exception);
    TSocketState         = (wsInvalidState,
                            wsOpened,     wsBound,
                            wsConnecting, wsSocksConnected, wsConnected,
                            wsAccepting,  wsListening,
                            wsClosed);
    TSocketSendFlags     = (wsSendNormal, wsSendUrgent);
    TSocketLingerOnOff   = (wsLingerOff, wsLingerOn, wsLingerNoSet);
    TWSocketSyncNextProc = procedure of object;
    TWSocketOption       = (wsoNoReceiveLoop, wsoTcpNoDelay);
    TWSocketOptions      = set of TWSocketOption;
    TDataAvailable       = procedure (Sender: TObject; ErrCode: Word) of object;
    TDataSent            = procedure (Sender: TObject; ErrCode: Word) of object;
    TSendData            = procedure (Sender: TObject; BytesSent: Integer) of object;
    TSessionClosed       = procedure (Sender: TObject; ErrCode: Word) of object;
    TSessionAvailable    = procedure (Sender: TObject; ErrCode: Word) of object;
    TSessionConnected    = procedure (Sender: TObject; ErrCode: Word) of object;
    TDnsLookupDone       = procedure (Sender: TObject; ErrCode: Word) of object;
    TChangeState         = procedure (Sender: TObject;
                                      OldState : TSocketState;
                                      NewState : TSocketState) of object;
    TDebugDisplay        = procedure (Sender: TObject;
                                      var Msg : String) of object;

    TCustomWSocket = class(TIcsComponent)
    private
        FDnsResult          : String;
        FDnsResultList      : TStrings;
        FSendFlags          : Integer;
        FLastError          : Integer;
        FDnsLookupBuffer    : TBytes;
        FDnsLookupHandle    : THandle;
        FDnsLookupCheckMsg  : Boolean;
        FDnsLookupTempMsg   : TMessage;
        FDnsLookupGCH       : GCHandle;
        FDnsLookupIntPtr    : IntPtr;
    protected
        FHSocket            : TSocket;
        FASocket            : TSocket;               { Accepted socket }
        FAddrStr            : String;
        FAddrResolved       : Boolean;
        FAddrFormat         : Integer;
        FAddrAssigned       : Boolean;
        FProto              : Integer;
        FProtoAssigned      : Boolean;
        FProtoResolved      : Boolean;
        FLocalPortResolved  : Boolean;
        FProtoStr           : String;
        FPortStr            : String;
        FPortAssigned       : Boolean;
        FPortResolved       : Boolean;
        FPortNum            : Integer;
        FLocalPortStr       : String;
        FLocalPortNum       : Integer;
        FLocalAddr          : String;     { IP address for local interface to use }
        FType               : Integer;
        FBufList            : TList;
        FBufSize            : Integer;
        FLingerOnOff        : TSocketLingerOnOff;
        FLingerTimeout      : Integer;              // In seconds, 0 = disabled
        FListenBacklog      : Integer;
        bAllSent            : Boolean;
        FReadCount          : LongInt;
        FPaused             : Boolean;
        FCloseInvoked       : Boolean;
        FFlushTimeout       : Integer;
        // More info about multicast can be found at:
        //   http://ntrg.cs.tcd.ie/undergrad/4ba2/multicast/antony/
        //    http://www.tldp.org/HOWTO/Multicast-HOWTO-6.html
        FMultiCast          : Boolean;
        // Multicast addresses consists of a range of addresses from 224.0.0.0
        // to 239.255.255.255. However, the multicast addresses from 224.0.0.0
        // to 224.0.0.255 are reserved for multicast routing information;
        // Application programs should use multicast addresses outside this
        // range.
        FMultiCastAddrStr   : String;
        FMultiCastIpTTL     : Integer;
        FReuseAddr          : Boolean;
        FComponentOptions   : TWSocketOptions;
        FState              : TSocketState;
        FRcvdFlag           : Boolean;
        FSelectEvent        : LongInt;
        FSelectMessage      : WORD;
        FRecvStrBuf         : TBytes;
        FOnSessionAvailable : TSessionAvailable;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnChangeState      : TChangeState;
        FOnDataAvailable    : TDataAvailable;
        FOnDataSent         : TDataSent;
        FOnSendData         : TSendData;
        FOnDnsLookupDone    : TDnsLookupDone;
        FOnError            : TNotifyEvent;
        FOnDisplay          : TDebugDisplay;
        procedure   WndProc(var MsgRec: TMessage); override;
        procedure   SocketError(sockfunc: String);
        procedure   WMASyncSelect(var msg: TMessage); //message WM_ASYNCSELECT;
        procedure   WMAsyncGetHostByName(var msg: TMessage); //message WM_ASYNCGETHOSTBYNAME;
        procedure   WMAsyncGetHostByAddr(var msg: TMessage); //message WM_ASYNCGETHOSTBYADDR;
        procedure   WMCloseDelayed(var msg: TMessage); //message WM_CLOSE_DELAYED;
        procedure   WMRelease(var msg: TMessage); //message WM_WSOCKET_RELEASE;
        procedure   ChangeState(NewState : TSocketState);
        procedure   TryToSend; virtual;
        procedure   ASyncReceive(ErrCode : Word; MySocketOptions : TWSocketOptions);
        procedure   AssignDefaultValue; virtual;
        procedure   InternalClose(bShut : Boolean; ErrCode : Word); virtual;
        procedure   InternalAbort(ErrCode : Word); virtual;
        //procedure   Notification(AComponent: TComponent; operation: TOperation); override;
        procedure   SetSendFlags(newValue : TSocketSendFlags);
        function    GetSendFlags : TSocketSendFlags;
        procedure   SetAddr(InAddr : String); virtual;
        function    GetAddr : String;
        procedure   SetRemotePort(sPort : String); virtual;
        function    GetRemotePort : String;
        procedure   SetLocalAddr(const sLocalAddr : String);
        procedure   SetLocalPort(const sLocalPort : String);
        procedure   SetProto(sProto : String); virtual;
        function    GetProto : String;
        function    GetRcvdCount : LongInt; virtual;
        procedure   BindSocket; virtual;
        procedure   SendText(const Str : String);
        function    RealSend(const Data : TBytes; Len : Integer) : Integer; virtual;
        procedure   RaiseExceptionFmt(const Fmt : String; args : array of const); virtual;
        procedure   RaiseException(const Msg : String); virtual;
//        procedure   HandleBackGroundException(E: Exception); virtual;
        procedure   TriggerDisplay(Msg : String);
        procedure   TriggerSendData(BytesSent : Integer); virtual;
        function    TriggerDataAvailable(ErrCode : Word) : Boolean; virtual;
        procedure   TriggerSessionAvailable(ErrCode : Word); virtual;
        procedure   TriggerSessionConnectedSpecial(ErrCode : Word); virtual;
        procedure   TriggerSessionConnected(ErrCode : Word); virtual;
        procedure   TriggerSessionClosed(ErrCode : Word); virtual;
        procedure   TriggerDataSent(ErrCode : Word); virtual;
        procedure   TriggerChangeState(OldState, NewState : TSocketState); virtual;
        procedure   TriggerDNSLookupDone(ErrCode : Word); virtual;
        procedure   TriggerError; virtual;
        function    DoRecv(out Buffer : TBytes;
                           BufferSize : Integer;
                           Flags      : Integer) : Integer; virtual;
        function    DoRecvFrom(FHSocket    : TSocket;
                               out Buffer  : TBytes;
                               BufferSize  : Integer;
                               Flags       : Integer;
                               var From    : TSockAddr;
                               var FromLen : Integer) : Integer; virtual;
        procedure   Do_FD_CONNECT(var msg: TMessage); virtual;
        procedure   Do_FD_READ(var msg: TMessage); virtual;
        procedure   Do_FD_WRITE(var msg: TMessage); virtual;
        procedure   Do_FD_ACCEPT(var msg: TMessage); virtual;
        procedure   Do_FD_CLOSE(var msg: TMessage); virtual;
        procedure   DupConnected; virtual;
        procedure   AbortComponent; override;
    public
        sin         : TSockAddr;
        constructor Create(AOwner : {$IFDEF ICS_COMPONENT}TComponent
                                    {$ELSE}TObject{$ENDIF}); override;
        destructor  Destroy; override;
        procedure   Connect; virtual;
        procedure   Close; virtual;
        procedure   CloseDelayed; virtual;
        procedure   Release; virtual;
        procedure   Abort; virtual;
        procedure   Flush; virtual;
        procedure   WaitForClose; virtual;
        procedure   Listen; virtual;
        function    Accept: TSocket; virtual;
        function    Receive(out Buffer : TBytes; BufferSize: Integer) : Integer; overload; virtual;
        function    Receive(out Buffer : TBytes; Offset, BufferSize: Integer) : Integer; overload; virtual;
        function    ReceiveStr : String; virtual;
        function    ReceiveFrom(out Buffer  : TBytes;
                                BufferSize  : Integer;
                                var From    : TSockAddr;
                                var FromLen : Integer) : Integer; virtual;
        function    PeekData(out Buffer : TBytes; BufferSize: Integer) : Integer;
        function    Send(const Data : TBytes; Len : Integer) : Integer; overload; virtual;
        function    Send(Data : Byte) : Integer; overload; virtual;
        function    SendTo(Dest       : TSockAddr;
                           DestLen    : Integer;
                           const Data : TBytes;
                           Len        : Integer) : Integer; virtual;
        function    SendStr(const Str : String) : Integer; virtual;
        procedure   DnsLookup(const HostName : String); virtual;
        procedure   ReverseDnsLookup(const HostAddr: String); virtual;
        procedure   CancelDnsLookup; virtual;
        function    GetPeerAddr: String; virtual;
        function    GetPeerPort: String; virtual;
        function    GetPeerName(var Name : TSockAddrIn;
                                NameLen  : Integer) : Integer; virtual;
        function    GetXPort: String; virtual;
        function    GetXAddr: String; virtual;
        function    TimerIsSet(var tvp : TTimeVal) : Boolean; virtual;
        procedure   TimerClear(var tvp : TTimeVal); virtual;
        function    TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean; virtual;
        function    GetSockName(out saddr    : TSockAddrIn;
                                var saddrlen : Integer) : Integer; virtual;
        procedure   SetLingerOption;
        procedure   Dup(NewHSocket : TSocket); virtual;
        procedure   Shutdown(How : Integer); virtual;
        procedure   Pause; virtual;
        procedure   Resume; virtual;
        procedure   PutDataInSendBuffer(const Data : TBytes; Len : Integer);
        procedure   PutStringInSendBuffer(const Str : String);
        procedure   DeleteBufferedData;
        procedure   ThreadAttach; override;
        procedure   ThreadDetach; override;
        property    Terminated         : Boolean        read  FTerminated
                                                        write FTerminated;
        property    OnMessagePump      : TNotifyEvent   read  FOnMessagePump
                                                        write FOnMessagePump;
    protected
        property PortNum : Integer                      read  FPortNum;
        property Handle : HWND                          read  FWindowHandle;
        property HSocket : TSocket                      read  FHSocket
                                                        write Dup;
        property Addr : String                          read  GetAddr
                                                        write SetAddr;
        property Port : String                          read  GetRemotePort
                                                        write SetRemotePort;
        property LocalPort : String                     read  FLocalPortStr
                                                        write SetLocalPort;
        property LocalAddr : String                     read  FLocalAddr
                                                        write SetLocalAddr;
        property Proto : String                         read  GetProto
                                                        write SetProto;
        property MultiCast       : Boolean              read  FMultiCast
                                                        write FMultiCast;
        property MultiCastAddrStr: String               read  FMultiCastAddrStr
                                                        write FMultiCastAddrStr;
        property MultiCastIpTTL  : Integer              read  FMultiCastIpTTL
                                                        write FMultiCastIpTTL;
        property ReuseAddr       : Boolean              read  FReuseAddr
                                                        write FReuseAddr;
        property PeerAddr : String                      read  GetPeerAddr;
        property PeerPort : String                      read  GetPeerPort;
        property DnsResult : String                     read  FDnsResult;
        property DnsResultList : TStrings               read  FDnsResultList;
        property State : TSocketState                   read  FState;
        property AllSent   : Boolean                    read  bAllSent;
        property ReadCount : LongInt                    read  FReadCount;
        property RcvdCount : LongInt                    read  GetRcvdCount;
        property LastError : Integer                    read  FLastError;
        property ComponentOptions : TWSocketOptions     read  FComponentOptions
                                                        write FComponentOptions;
        property BufSize   : Integer                    read  FBufSize
                                                        write FBufSize;
        property ListenBacklog : Integer                read  FListenBacklog
                                                        write FListenBacklog;
        property OnDataAvailable    : TDataAvailable    read  FOnDataAvailable
                                                        write FOnDataAvailable;
        property OnDataSent      : TDataSent            read  FOnDataSent
                                                        write FOnDataSent;
        property OnSendData         : TSendData         read  FOnSendData
                                                        write FOnSendData;
        property OnSessionClosed    : TSessionClosed    read  FOnSessionClosed
                                                        write FOnSessionClosed;
        property OnSessionAvailable : TSessionAvailable read  FOnSessionAvailable
                                                        write FOnSessionAvailable;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnChangeState      : TChangeState      read  FOnChangeState
                                                        write FOnChangeState;
        property OnDnsLookupDone    : TDnsLookupDone    read  FOnDnsLookupDone
                                                        write FOnDnsLookupDone;
        property OnError            : TNotifyEvent      read  FOnError
                                                        write FOnError;
        property FlushTimeout : Integer                 read  FFlushTimeOut
                                                        write FFlushTimeout;
        property SendFlags : TSocketSendFlags           read  GetSendFlags
                                                        write SetSendFlags;
        property Text: String                           read  ReceiveStr
                                                        write SendText;
        property LingerOnOff   : TSocketLingerOnOff     read  FLingerOnOff
                                                        write FLingerOnOff;
        property LingerTimeout : Integer                read  FLingerTimeout
                                                        write FLingerTimeout;
        property OnDisplay : TDebugDisplay              read  FOnDisplay
                                                        write FOnDisplay;
    end;

    TSocksState          = (socksData, socksNegociateMethods,
                            socksAuthenticate, socksConnect);
    TSocksAuthentication = (socksNoAuthentication,
                            socksAuthenticateUsercode);
    TSocksAuthState      = (socksAuthStart, socksAuthSuccess,
                            socksAuthFailure, socksAuthNotRequired);
    TSocksAuthStateEvent = procedure(Sender : TObject;
                                     AuthState : TSocksAuthState) of object;
    TSocksErrorEvent     = procedure(Sender : TObject;
                                     ErrCode : Integer;
                                     Msg : String) of Object;

    TCustomSocksWSocket = class(TCustomWSocket)
    protected
        FSocksState          : TSocksState;
        FSocksServer         : String;
        FSocksLevel          : String;
        FSocksPort           : String;
        FSocksPortAssigned   : Boolean;
        FSocksServerAssigned : Boolean;
        FSocksUsercode       : String;
        FSocksPassword       : String;
        FSocksAuthentication : TSocksAuthentication;
        FSocksAuthNumber     : char;
        FBoundAddr           : String;
        FBoundPort           : String;
        FRcvBuf              : TBytes;
        FRcvCnt              : Integer;
        FSocksRcvdCnt        : Integer;
        FSocksRcvdPtr        : Integer;
        FOnSocksError        : TSocksErrorEvent;
        FOnSocksConnected    : TSessionConnected;
        FOnSocksAuthState    : TSocksAuthStateEvent;
        procedure   AssignDefaultValue; override;
        procedure   TriggerSessionConnectedSpecial(ErrCode : Word); override;
        procedure   TriggerSocksConnected(ErrCode : Word); virtual;
        procedure   TriggerSessionClosed(ErrCode : Word); override;
        function    TriggerDataAvailable(ErrCode : Word) : Boolean; override;
        procedure   SetSocksPort(sPort : String); virtual;
        procedure   SetSocksServer(sServer : String); virtual;
        procedure   TriggerSocksError(ErrCode : Integer; Msg : String); virtual;
        procedure   TriggerSocksAuthState(AuthState : TSocksAuthState);
        function    GetRcvdCount : LongInt; override;
        procedure   SetSocksLevel(newValue : String);
        function    DoRecv(out Buffer : TBytes;
                           BufferSize : Integer;
                           Flags      : Integer) : Integer; override;
        procedure   SocksDoConnect;
        procedure   SocksDoAuthenticate;
        procedure   DataAvailableError(ErrCode : Integer; Msg : String);
    public
        procedure   Connect; override;
        procedure   Listen; override;
        protected
        property SocksServer   : String               read  FSocksServer
                                                      write SetSocksServer;
        property SocksLevel    : String               read  FSocksLevel
                                                      write SetSocksLevel;
        property SocksPort     : String               read  FSocksPort
                                                      write SetSocksPort;
        property SocksUsercode : String               read  FSocksUsercode
                                                      write FSocksUsercode;
        property SocksPassword : String               read  FSocksPassword
                                                      write FSocksPassword;
        property SocksAuthentication : TSocksAuthentication
                                                      read  FSocksAuthentication
                                                      write FSocksAuthentication;
        property OnSocksError  : TSocksErrorEvent     read  FOnSocksError
                                                      write FOnSocksError;
        property OnSocksConnected : TSessionConnected read  FOnSocksConnected
                                                      write FOnSocksConnected;
        property OnSocksAuthState : TSocksAuthStateEvent
                                                      read  FOnSocksAuthState
                                                      write FOnSocksAuthState;
    end;

    TLineLimitEvent = procedure (Sender        : TObject;
                                 RcvdLength    : LongInt;
                                 var ClearData : Boolean) of object;

    TCustomLineWSocket = class (TCustomSocksWSocket)
    protected
        FRcvdPtr             : TBytes;
        FRcvBufSize          : LongInt;
        FRcvdCnt             : LongInt;
        FLocalBuf            : TBytes;
        FLineEnd             : String;
        FLineMode            : Boolean;
        FLineLength          : Integer;    { When a line is available  }
        FLineLimit           : LongInt;    { Max line length we accept }
        FLineReceivedFlag    : Boolean;
        FLineClearData       : Boolean;
        FLineEcho            : Boolean;    { Echo received data    }
        FLineEdit            : Boolean;    { Edit received data    }
        FTimeout             : LongInt;    { Given in milliseconds }
        FTimeStop            : LongInt;    { Milliseconds          }
        FOnLineLimitExceeded : TLineLimitEvent;
        procedure   WndProc(var MsgRec: TMessage); override;
        procedure   WMTriggerDataAvailable(var msg: TMessage); message WM_TRIGGER_DATA_AVAILABLE;
        function    TriggerDataAvailable(ErrCode : Word) : Boolean; override;
        procedure   TriggerSessionClosed(ErrCode : Word); override;
        procedure   TriggerLineLimitExceeded(Cnt: Integer;
                                             var ClearData : Boolean); virtual;
        procedure   SetLineMode(newValue : Boolean); virtual;
        procedure   EditLine(var Len : Integer); virtual;
        function    GetRcvdCount : LongInt; override;
        function    DoRecv(out Buffer : TBytes;
                           BufferSize : Integer;
                           Flags      : Integer) : Integer; override;
        public
        constructor Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent
                                   {$ELSE}TObject{$ENDIF}); override;
        destructor  Destroy; override;
        property    LineLength : Integer     read  FLineLength;
        property    RcvdPtr    : TBytes      read  FRcvdPtr;
        property    RcvdCnt    : LongInt     read  FRcvdCnt;
        published
        property LineMode : Boolean          read  FLineMode
                                             write SetLineMode;
        property LineLimit : LongInt         read  FLineLimit
                                             write FLineLimit;
        property LineEnd  : String           read  FLineEnd
                                             write FLineEnd;
        property LineEcho : Boolean          read  FLineEcho
                                             write FLineEcho;
        property LineEdit : Boolean          read  FLineEdit
                                             write FLineEdit;
        property OnLineLimitExceeded : TLineLimitEvent
                                             read  FOnLineLimitExceeded
                                             write FOnLineLimitExceeded;
    end;

    TCustomSyncWSocket = class(TCustomLineWSocket)
    protected
        FLineBuffer : TBytes;
        function    Synchronize(Proc : TWSocketSyncNextProc; var DoneFlag : Boolean) : Integer; virtual;
        function    WaitUntilReady(var DoneFlag : Boolean) : Integer; virtual;
        procedure   InternalDataAvailable(Sender: TObject; ErrCode: Word);
    public
        procedure   ReadLine(Timeout : integer; var Buffer : String);
    end;

    TWSocket = class(TCustomSyncWSocket)
    public
        property PortNum;
        property Handle;
        property HSocket;
        property BufSize;
        property Text;
        property AllSent;
        property OnDisplay;
    published
        property Addr;
        property Port;
        property Proto;
        property LocalAddr;
        property LocalPort;
        property PeerPort;
        property PeerAddr;
        property DnsResult;
        property DnsResultList;
        property State;
        property ReadCount;
        property RcvdCount;
        property LastError;
        property MultiThreaded;
        property MultiCast;
        property MultiCastAddrStr;
        property MultiCastIpTTL;
        property ReuseAddr;
        property ComponentOptions;
        property ListenBacklog;
        property OnDataAvailable;
        property OnDataSent;
        property OnSendData;
        property OnSessionClosed;
        property OnSessionAvailable;
        property OnSessionConnected;
        property OnSocksConnected;
        property OnChangeState;
        { property OnLineTooLong; }
        property OnDnsLookupDone;
        property OnError;
        property OnBgException;
        property FlushTimeout;
        property SendFlags;
        property LingerOnOff;
        property LingerTimeout;
        property SocksLevel;
        property SocksServer;
        property SocksPort;
        property SocksUsercode;
        property SocksPassword;
        property SocksAuthentication;
        property OnSocksError;
        property OnSocksAuthState;
    end;

function  WinsockInfo : TWSADATA;
function  WSocketErrorDesc(ErrCode: Integer) : String;
function  WSocketGetHostByAddr(const Addr : String) : IntPtr;
function  WSocketGetHostByName(const Name : String) : IntPtr;
function  LocalHostName : String;
function  LocalIPList : TStrings;
function  WSocketResolveIp(const IpAddr : String) : String;
function  WSocketResolveHost(const InAddr : String) : TInAddr;
function  WSocketResolvePort(const Port : String; const Proto : String) : Word;
function  WSocketResolveProto(const Proto : String) : Integer;
function  WSocketIsDottedIP(const S : String) : Boolean;

function  WSocket_WSAStartup(wVersionRequired : word;
                             out WSData       : TWSAData): Integer;
function  WSocket_WSACleanup : Integer;
procedure WSocket_WSASetLastError(ErrCode: Integer);
function  WSocket_WSAGetLastError: Integer;
function  WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
function  WSocket_WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int;
                                        const name : String; buf: IntPtr;
                                        buflen: Integer): THandle;
function  WSocket_WSAAsyncGetHostByAddr(HWindow: HWND;
                                        wMsg: u_int; var addr: u_long;
                                        len, Struct: Integer;
                                        buf: IntPtr;
                                        buflen: Integer): THandle;
function  WSocket_WSAAsyncSelect(s       : TSocket;
                                 HWindow : HWND;
                                 wMsg    : u_int;
                                 lEvent  : Longint): Integer;
function  WSocket_recv(s: TSocket;
                       out Buf : TBytes; len, flags: Integer): Integer;
function  WSocket_recvfrom(s          : TSocket;
                          out Buf     : TBytes; len, flags: Integer;
                          var from    : TSockAddr;
                          var fromlen : Integer): Integer;
function  WSocket_getservbyname(const name, proto: String): IntPtr;
function  WSocket_getprotobyname(const name: String): IntPtr;
function  WSocket_gethostbyname(const name: String): IntPtr;
function  WSocket_gethostbyaddr(var addr: u_long; len, Struct: Integer): IntPtr;
function  WSocket_gethostname(out name: String): Integer;
function  WSocket_socket(af, Struct, protocol: Integer): TSocket;
function  WSocket_shutdown(s: TSocket; how: Integer): Integer;
function  WSocket_setsockopt(s: TSocket; level, optname: Integer;
                             var optval: Integer;
                             optlen: Integer): Integer; overload;
function  WSocket_setsockopt(s: TSocket; level, optname: Integer;
                             var optval: ip_mreq;
                             optlen: Integer): Integer; overload;
function  WSocket_setsockopt(s: TSocket; level, optname: Integer;
                             var optval: TInAddr;
                             optlen: Integer): Integer; overload;
function  WSocket_setsockopt(s: TSocket; level, optname: Integer;
                             var optval: TLinger;
                             optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: Integer;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: ip_mreq;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: TInAddr;
                             var optlen: Integer): Integer; overload;
function  WSocket_getsockopt(s: TSocket; level, optname: Integer;
                             var optval: TLinger;
                             var optlen: Integer): Integer; overload;
function  WSocket_sendto(s: TSocket; const Buf: TBytes;
                         len, flags: Integer;
                         var addrto: TSockAddr;
                         tolen: Integer): Integer;
function  WSocket_send(s: TSocket; const Buf: TBytes;
                       len, flags: Integer): Integer;
function  WSocket_ntohs(netshort: u_short): u_short;
function  WSocket_ntohl(netlong: u_long): u_long;
function  WSocket_listen(s: TSocket; backlog: Integer): Integer;
function  WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
function  WSocket_inet_ntoa(inaddr: TInAddr): String;
function  WSocket_inet_addr(const cp: String): u_long;
function  WSocket_htons(hostshort: u_short): u_short;
function  WSocket_htonl(hostlong: u_long): u_long;
function  WSocket_getsockname(s: TSocket; var name: TSockAddr;
                             var namelen: Integer): Integer;
function  WSocket_getpeername(s: TSocket; out name: TSockAddr;
                              var namelen: Integer): Integer;
function  WSocket_connect(s: TSocket; var name: TSockAddr;
                          namelen: Integer): Integer;
function  WSocket_closesocket(s: TSocket): Integer;
function  WSocket_bind(s: TSocket; var addr: TSockAddr;
                       namelen: Integer): Integer;
function  WSocket_accept(s: TSocket; var addr: TSockAddr; var addrlen: Integer): TSocket;
procedure WSocketForceLoadWinsock;
procedure WSocketTriggerDebugEvent(Sender : TObject; const Msg : String);

var
    WSocketDebugEvent : procedure(Sender : TObject; const Msg : String);

implementation

const
    socksNoError              = 20000;
    socksProtocolError        = 20001;
    socksVersionError         = 20002;
    socksAuthMethodError      = 20003;
    socksGeneralFailure       = 20004;
    socksConnectionNotAllowed = 20005;
    socksNetworkUnreachable   = 20006;
    socksHostUnreachable      = 20007;
    socksConnectionRefused    = 20008;
    socksTtlExpired           = 20009;
    socksUnknownCommand       = 20010;
    socksUnknownAddressType   = 20011;
    socksUnassignedError      = 20012;
    socksInternalError        = 20013;
    socksDataReceiveError     = 20014;
    socksAuthenticationFailed = 20015;
    socksRejectedOrFailed     = 20016;
    socksHostResolutionFailed = 20017;

procedure WSocketWinsockInit; forward;
procedure WSocketWinsockCleanup; forward;

var
    GInitData      : TWSADATA;
    IPList         : TStrings;
    WSocketGCount  : Integer = 0;
    GWSockCritSect : Mutex;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketTriggerDebugEvent(Sender : TObject; const Msg : String);
begin
    if Assigned(WSocketDebugEvent) then
        WSocketDebugEvent(Sender, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsDigit(Ch : Char) : Boolean;
begin
    Result := (Ch >= '0') and (Ch <= '9');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check for a valid numeric dotted IP address such as 192.161.65.25         }
{ Accept leading and trailing spaces.                                       }
function WSocketIsDottedIP(const S : String) : Boolean;
var
    I          : Integer;
    DotCount   : Integer;
    NumVal     : Integer;
begin
    Result     := FALSE;
    DotCount   := 0;
    NumVal     := 0;
    I          := 1;
    { Skip leading spaces }
    while (I <= Length(S)) and (S[I] = ' ') do
        Inc(I);
    { Can't begin with a dot }
    if (I <= Length(S)) and (S[I] = '.') then
        Exit;
    { Scan full String }
    while I <= Length(S) do begin
        if S[I] = '.' then begin
            Inc(DotCount);
            if (DotCount > 3) or (NumVal > 255) then
                Exit;
            NumVal := 0;
            { A dot must be followed by a digit }
            if (I >= Length(S)) or (not IsDigit(S[I + 1])) then
                Exit;
        end
        else if IsDigit(S[I]) then
            NumVal := NumVal * 10 + Ord(S[I]) - Ord('0')
        else begin
            { Not a digit nor a dot. Accept spaces until end of string }
            while (I <= Length(S)) and (S[I] = ' ') do
                Inc(I);
            if I <= Length(S) then
                Exit;  { Not a space, do not accept }
            break;     { Only spaces, accept        }
        end;
        Inc(I);
    end;
    { We must have exactly 3 dots }
    if (DotCount <> 3) or (NumVal > 255) then
        Exit;
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RaiseException(const Msg : String);
begin
    if Assigned(FOnError) then
        TriggerError
    else
        raise ESocketException.Create(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.RaiseExceptionFmt(const Fmt : String; args : array of const);
begin
    if Assigned(FOnError) then
        TriggerError
    else
        raise ESocketException.CreateFmt(Fmt, args);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAStartup(
    wVersionRequired : Word;
    out WSData       : TWSAData): Integer;
begin
    Result := OverByte.Ics.Winsock.WSAStartup(wVersionRequired, WSData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACleanup : Integer;
begin
    Result := OverByte.Ics.Winsock.WSACleanup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocket_WSASetLastError(ErrCode: Integer);
begin
    OverByte.Ics.Winsock.WsaSetLastError(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAGetLastError: Integer;
begin
    Result := OverByte.Ics.Winsock.WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Integer;
begin
    Result := OverByte.Ics.WinSock.WSACancelAsyncRequest(hAsyncTaskHandle);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByName(
    HWindow: HWND; wMsg: u_int;
    const name : String; buf: IntPtr;
    buflen: Integer): THandle;
begin
    Result := OverByte.Ics.Winsock.WSAAsyncGetHostByName(
                  HWindow, wMsg, name, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncGetHostByAddr(
    HWindow: HWND;
    wMsg: u_int; var addr: u_long;
    len, Struct: Integer;
    buf: IntPtr;
    buflen: Integer): THandle;
begin
    Result := OverByte.Ics.Winsock.WSAAsyncGetHostByAddr(
                   HWindow, wMsg, addr, len, struct, buf, buflen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_WSAAsyncSelect(
    S       : TSocket;
    HWindow : HWND;
    wMsg    : u_int;
    lEvent  : Longint): Integer;
begin
    Result := OverByte.Ics.Winsock.WSAAsyncSelect(S, HWindow, wMsg, lEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getservbyname(const Name, Proto: String): IntPtr;
begin
    Result := OverByte.Ics.Winsock.getservbyname(Name, Proto);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getprotobyname(const name: String): IntPtr;
begin
    Result := OverByte.Ics.Winsock.getprotobyname(name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyname(const Name: String): IntPtr;
begin
    Result := OverByte.Ics.Winsock.gethostbyname(Name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostbyaddr(
    var addr: u_long; len, Struct: Integer): IntPtr;
begin
    Result := OverByte.Ics.Winsock.gethostbyaddr(addr, len, Struct);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_gethostname(out Name: String): Integer;
var
    SB: System.Text.StringBuilder;
begin
    SB     := System.Text.StringBuilder.Create(256);
    Result := OverByte.Ics.Winsock.gethostname(SB, SB.Capacity);
    if Result <> 0 then
        Name := ''
    else
        Name := SB.ToString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_socket(af, Struct, protocol: Integer): TSocket;
begin
    Result := OverByte.Ics.Winsock.socket(af, Struct, protocol);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_shutdown(s: TSocket; how: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.shutdown(s, how);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : Integer;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByte.Ics.Winsock.setsockopt_integer(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : ip_mreq;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByte.Ics.Winsock.setsockopt_ip_mreq(
                  S, Level, OptName, OptVal, 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TInAddr;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByte.Ics.Winsock.setsockopt_tinaddr(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_setsockopt(
    s              : TSocket;
    level, optname : Integer;
    var optval     : TLinger;
    optlen         : Integer): Integer; overload;
begin
    Result := OverByte.Ics.Winsock.setsockopt_tlinger(
                  S, Level, OptName, OptVal, 4);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: Integer; var optlen: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.getsockopt_integer(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: ip_mreq; var optlen: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.getsockopt_ip_mreq(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TInAddr; var optlen: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.getsockopt_tinaddr(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockopt(
    s: TSocket; level, optname: Integer;
    var optval: TLinger; var optlen: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.getsockopt_tlinger(
                  s, level, optname, optval, optlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_sendto(
    s          : TSocket;
    const Buf  : TBytes;
    len, flags : Integer;
    var addrto : TSockAddr;
    tolen      : Integer): Integer;
begin
    Result := OverByte.Ics.WinSock.sendto(
                  s, Buf, len, flags, addrto, SizeOfTSockAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_send(
    s: TSocket;
    const Buf : TBytes;
    len, flags: Integer): Integer;
begin
    Result := OverByte.Ics.WinSock.send(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohs(netshort: u_short): u_short;
begin
    Result := OverByte.Ics.Winsock.ntohs(netshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ntohl(netlong: u_long): u_long;
begin
    Result := OverByte.Ics.Winsock.ntohl(netlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_listen(s: TSocket; backlog: Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.listen(s, backlog);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
    Result := OverByte.Ics.Winsock.ioctlsocket(s, cmd, arg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_ntoa(inaddr: TInAddr): String;
begin
    Result := OverByte.Ics.WinSock.inet_ntoa(inaddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_inet_addr(const cp: String): u_long;
begin
    Result := OverByte.Ics.WinSock.inet_addr(cp);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htons(hostshort: u_short): u_short;
begin
    Result := OverByte.Ics.Winsock.htons(hostshort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_htonl(hostlong: u_long): u_long;
begin
    Result := OverByte.Ics.Winsock.htonl(hostlong);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getsockname(
    S           : TSocket;
    var Name    : TSockAddr;
    var NameLen : Integer): Integer;
var
    APINameLen : Integer;
begin
    APINameLen := SizeOfTSockAddr;
    Result     := OverByte.Ics.Winsock.getsockname(S, Name, APINameLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_getpeername(
    S           : TSocket;
    out Name    : TSockAddr;
    var NameLen : Integer): Integer;
var
    APINameLen : Integer;
begin
    APINameLen := SizeOfTSockAddr;
    Result     := OverByte.Ics.Winsock.getpeername(S, Name, APINameLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_connect(
    S        : TSocket;
    var Name : TSockAddr;
    NameLen  : Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.connect(S, Name, NameLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_closesocket(s: TSocket): Integer;
begin
    Result := OverByte.Ics.Winsock.closesocket(s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_bind(
    S        : TSocket;
    var Addr : TSockAddr;
    NameLen  : Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.bind(S, Addr, SizeOfTSockAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_accept(
    s           : TSocket;
    var addr    : TSockAddr;
    var addrlen : Integer): TSocket;
begin
    Result := OverByte.Ics.Winsock.accept(s, addr, addrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recv(
    S          : TSocket;
    out Buf    : TBytes;
    Len, Flags : Integer): Integer;
begin
    Result := OverByte.Ics.Winsock.recv(s, Buf, len, flags);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocket_recvfrom(
    S           : TSocket;
    out Buf     : TBytes;
    Len, Flags  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer): Integer;
var
    APIFromLen : Integer;
begin
    APIFromLen := SizeOfTSockAddr;
    Result     := OverByte.Ics.Winsock.recvfrom(
                      S, Buf, Len, Flags, From, APIFromLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WinsockInfo : TWSADATA;
begin
    Result := GInitData;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AssignDefaultValue;
begin
    //FillChar(sin, 0, Sizeof(sin));
    sin.sin_family      := AF_INET;
    sin.sin_port        := 0;
    sin.sin_addr.S_addr := 0;
    FAddrFormat         := PF_INET;

    FPortAssigned      := FALSE;
    FAddrAssigned      := FALSE;
    FAddrResolved      := FALSE;
    FPortResolved      := FALSE;
    FProtoResolved     := FALSE;
    FLocalPortResolved := FALSE;

    FProtoAssigned     := TRUE;
    FProto             := IPPROTO_TCP;
    FProtoStr          := 'tcp';
    FType              := SOCK_STREAM;
    FLocalPortStr      := '0';
    FLocalAddr         := '0.0.0.0';

    FLingerOnOff       := wsLingerOn;
    FLingerTimeout     := 0;
    FHSocket           := INVALID_SOCKET;
    FSelectEvent       := 0;
    FState             := wsClosed;
    bAllSent           := TRUE;
    FPaused            := FALSE;
    FReadCount         := 0;
    FCloseInvoked      := FALSE;
    FFlushTimeout      := 60;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure handle all messages for TWSocket. All exceptions must be   }
{ handled or the application will be shutted down !                         }
{ If WndProc is overriden in descendent components, then the same exception }
{ handling *MUST* be setup because descendent component code is executed    }
{ before the base class code.                                               }
procedure TCustomWSocket.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if Msg = WM_ASYNCSELECT then
                WMASyncSelect(MsgRec)
            else if Msg = WM_ASYNCGETHOSTBYNAME then
                WMAsyncGetHostByName(MsgRec)
            else if Msg = WM_ASYNCGETHOSTBYADDR then
                WMAsyncGetHostByAddr(MsgRec)
            else if Msg = WM_CLOSE_DELAYED then
                WMCloseDelayed(MsgRec)
            else if Msg = WM_WSOCKET_RELEASE then
                WMRelease(MsgRec)
{$IFDEF NEVER}
            else if Msg = WM_TRIGGER_EXCEPTION then
                { This is useful to check for background exceptions            }
                { In your application, use following code to test your handler }
                { PostMessage(WSocket1.Handle, WM_TRIGGER_EXCEPTION, 0, 0);    }
                raise ESocketException.Create('Test exception in WSocket')
{$ENDIF}
            else
                inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadAttach;
begin
    inherited ThreadAttach;
    if FHSocket <> INVALID_SOCKET then
        WSocket_WSAASyncSelect(FHSocket, FWindowHandle,
                               WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ThreadDetach;
begin
    if FHSocket <> INVALID_SOCKET then
        WSocket_WSAASyncSelect(FHSocket, FWindowHandle, 0, 0);
    inherited ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketForceLoadWinsock;
begin
    WSocketWinsockInit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketWinsockInit;
begin
    GWSockCritSect.WaitOne;
    try
        Inc(WSocketGCount);
        if WSocketGCount = 1 then
            WSAStartup($101, GInitData);
    finally
        GWSockCritSect.ReleaseMutex;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure WSocketWinsockCleanup;
begin
    GWSockCritSect.WaitOne;
    try
        Dec(WSocketGCount);
        if WSocketGCount = 0 then
            WSACleanup;
    finally
        GWSockCritSect.ReleaseMutex;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomWSocket.Create(
    AOwner : {$IFDEF ICS_COMPONENT}TComponent
             {$ELSE}               TObject{$ENDIF});
begin
WriteLn('TCustomWSocket.Create');
    inherited Create(AOwner);
    FBufList        := TList.Create;
    FBufSize        := 1460; {1514;}             { Default buffer size }
    FDnsResultList  := TStringList.Create;
    FMultiCastIpTTL := IP_DEFAULT_MULTICAST_TTL;
    FListenBacklog  := 5;
    AssignDefaultValue;
    WSocketWinsockInit;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomWSocket.Destroy;
begin
    try
        CancelDnsLookup;             { Cancel any pending dns lookup      }
    except
        { Ignore any exception here }
    end;

    if FState <> wsClosed then       { Close the socket if not yet closed }
        Close;
    WSocketWinsockCleanup;
    DeleteBufferedData;
    if Assigned(FBufList) then begin
        FBufList.Free;
        FBufList := nil;
    end;
    if Assigned(FDnsResultList) then begin
        FDnsResultList.Free;
        FDnsResultList := nil;
    end;

    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Dup(NewHSocket : TSocket);
var
    iStatus : Integer;
begin
    if (NewHSocket = 0) or (NewHSocket = INVALID_SOCKET) then begin
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('Dup');
        Exit;
    end;

    if FState <> wsClosed then begin
        iStatus := WSocket_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if iStatus <> 0 then begin
            SocketError('Dup (closesocket)');
            Exit;
        end;

        ChangeState(wsClosed);
    end;
    FHsocket := NewHSocket;
    SetLingerOption;

    FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
    iStatus      := WSocket_WSAASyncSelect(FHSocket, Handle,
                                           WM_ASYNCSELECT, FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;
    DupConnected;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DupConnected;
begin
    ChangeState(wsConnected);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get the number of char received and waiting to be read                    }
function TCustomWSocket.GetRcvdCount : LongInt;
var
    Value : u_long;
begin
{$IFDEF ICS_COMPONENT}
    if csDesigning in ComponentState then begin
        Result := -1;
        Exit;
    end;
{$ENDIF}
    if WSocket_ioctlsocket(FHSocket, FIONREAD, Value) = SOCKET_ERROR then begin
        Result := -1;
        SocketError('ioctlSocket');
        Exit;
    end;
    Result := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ChangeState(NewState : TSocketState);
var
    OldState : TSocketState;
begin
    OldState := FState;
    FState   := NewState;
    if OldState <> NewState then
        TriggerChangeState(OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DoRecv is a simple wrapper around winsock recv function to make it        }
{ a virtual function.                                                       }
function TCustomWSocket.DoRecv(
    out Buffer : TBytes;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
begin
    Result    := WSocket_recv(FHSocket, Buffer, BufferSize, Flags);
    FRcvdFlag := (Result >= BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Receive(
    out Buffer : TBytes;
    Offset     : Integer;
    BufferSize : Integer) : Integer;
var
    LocalBuf : TBytes;
    I        : Integer;
begin
    // Allocate a local buffer with size multiple of 2K and large enough
    // for BufferSize. Using a fixed size will help memory manager to
    // minimize fragmentation
    SetLength(LocalBuf, ((BufferSize div 2048) + 1) * 2048);
    // Receive data into local buffer
    Result := Receive(LocalBuf, BufferSize);
    // Move data from local buffer to given buffer at given offset
    I := 0;
    while I < Result do begin
        Buffer[Offset + I] := LocalBuf[I];
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.Receive(
    out Buffer : TBytes;
    BufferSize : Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, 0);
    if Result < 0 then
        FLastError := WSocket_WSAGetLastError
    else
        { Here we should check for overflows ! It is well possible to }
        { receive more than 2GB during a single session.              }
        { Or we could use an Int64 variable...                        }
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The socket is non-blocking, so this routine will only receive as much     }
{ data as it is available.                                                  }
function TCustomWSocket.ReceiveStr : String;
var
    Len : Integer;
    I   : Integer;
const
    RecvBufSize = 1460;
begin
    if Length(FRecvStrBuf) < RecvBufSize then
        SetLength(FRecvStrBuf, RecvBufSize);
    Result := '';
    while TRUE do begin
        Len := Receive(FRecvStrBuf, Length(FRecvStrBuf));
        if Len <= 0 then
            break;
        for I := 0 to Len - 1 do
            Result := Result + Char(FRecvStrBuf[I]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.DoRecvFrom(
    FHSocket    : TSocket;
    out Buffer  : TBytes;
    BufferSize  : Integer;
    Flags       : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := WSocket_recvfrom(FHSocket, Buffer, BufferSize,
                               Flags, From, FromLen);
    FRcvdFlag := (Result >= BufferSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.ReceiveFrom(
    out Buffer  : TBytes;
    BufferSize  : Integer;
    var From    : TSockAddr;
    var FromLen : Integer) : Integer;
begin
    Result := DoRecvFrom(FHSocket, Buffer, BufferSize, 0, From, FromLen);
    if Result < 0 then
        FLastError := WSocket_WSAGetLastError
    else
        FReadCount := FReadCount + Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.PeekData(
    out Buffer : TBytes;
    BufferSize : Integer) : Integer;
begin
    Result := DoRecv(Buffer, BufferSize, MSG_PEEK);
    if Result < 0 then
        FLastError := WSocket_WSAGetLastError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.SendTo(
    Dest       : TSockAddr;
    DestLen    : Integer;
    const Data : TBytes;
    Len        : Integer) : Integer;
begin
    Result := WSocket_SendTo(FHSocket, Data, Len, FSendFlags,
                             Dest, DestLen);
    if Result > 0 then
        TriggerSendData(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.RealSend(const Data : TBytes; Len : Integer) : Integer;
begin
    //WSocketTriggerDebugEvent(Self, 'ReadSend ' + IntToStr(Len));
    if FType = SOCK_DGRAM then
        Result := WSocket_SendTo(FHSocket, Data, Len, FSendFlags,
                                 sin, SizeOf(sin))
    else
        Result := WSocket_Send(FHSocket, Data, Len, FSendFlags);
    if Result > 0 then
        TriggerSendData(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TryToSend;
var
    oBuffer   : TBuffer;
    Len       : Integer;
    Count     : Integer;
    Data      : TBytes;
    LastError : Integer;
    bMore     : Boolean;
begin
    if (FHSocket = INVALID_SOCKET) or                { No more socket      }
       (FBufList.Count = 0)                          { Nothing to send     }
       then
        Exit;

    bMore := TRUE;
    while bMore do begin
        oBuffer := TBuffer(FBufList.First);
        Len     := oBuffer.Peek(Data);
        if Len <= 0 then begin
            { Buffer is empty }
            if FBufList.Count <= 1 then begin
                { Every thing has been sent }
                bAllSent := TRUE;
                bMore    := FALSE;
            end
            else begin
                oBuffer.Free;
                FBufList.Delete(0);
                FBufList.Pack;
            end;
        end
        else begin
            Count := RealSend(Data, Len);

            if Count = 0 then
                bMore := FALSE  { Closed by remote }
            else if count = SOCKET_ERROR then begin
                LastError := WSocket_WSAGetLastError;
                if (LastError = WSAECONNRESET) or (LastError = WSAENOTSOCK) or
                   (LastError = WSAENOTCONN)   or (LastError = WSAEINVAL)   or
                   (LastError = WSAECONNABORTED)     { 07/05/99 }
                then begin
                    FCloseInvoked := TRUE;           { 23/07/98 }
                    Close;
                    TriggerSessionClosed(LastError); { 23/07/98 }
                end
                else if LastError <> WSAEWOULDBLOCK then begin
                    SocketError('TryToSend failed');
                    Exit;
                end;
                bMore := FALSE;
            end
            else begin
                oBuffer.Remove(Count);
                if Count < Len then begin
                    { Could not write as much as we wanted. Stop sending }
{$IFDEF VER80}
                    { A bug in some Trumpet Winsock implementation break the  }
                    { background sending. Jan Tomasek <xtomasej@feld.cvut.cz> }
                    if not TrumpetCompability then begin
                        {bWrite := FALSE;23/12/01}
                        bMore  := FALSE;
                    end;
{$ELSE}
                    {bWrite := FALSE;23/12/01}
                    bMore  := FALSE;
{$ENDIF}
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutStringInSendBuffer(const Str : String);
var
    Data : TBytes;
    I    : Integer;
begin
    SetLength(Data, Length(Str));
    for I := 1 to Length(Str) do
        Data[I - 1] := Ord(Str[I]);
    PutDataInSendBuffer(Data, Length(Str));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.PutDataInSendBuffer(const Data : TBytes; Len : Integer);
var
    oBuffer  : TBuffer;
    cWritten : Integer;
    bMore    : Boolean;
    iOffset  : Integer;
begin
    if Len <= 0 then
        exit;

    if FBufList.Count = 0 then begin
        oBuffer := TBuffer.Create(FBufSize);
        FBufList.Add(oBuffer);
    end
    else
        oBuffer := TBuffer(FBufList.Last);

    iOffset := 0;
    bMore   := TRUE;
    while bMore do begin
        cWritten := oBuffer.Write(Data, iOffset, Len);
        if cWritten >= Len then
            bMore := FALSE
        else begin
            Len  := Len - cWritten;
            Inc(iOffset, cWritten);
            if Len < 0 then
                bMore := FALSE
            else begin
                oBuffer := TBuffer.Create(FBufSize);
                FBufList.Add(oBuffer);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.Send(const Data : TBytes; Len : Integer) : Integer;
begin
    if (FState <> wsConnected) and (FState <> wsSocksConnected) then begin
        WSocket_WSASetLastError(WSAENOTCONN);
        SocketError('Send');
        Result := -1;
        Exit;
    end;

    bAllSent := FALSE;
    if Len <= 0 then
        Result := 0
    else begin
        Result   := Len;
        PutDataInSendBuffer(Data, Len);
    end;

    if bAllSent then
        Exit;

    TryToSend;

    if bAllSent then begin
        { We post a message to fire the FD_WRITE message wich in turn will }
        { fire the OnDataSent event. We cannot fire the event ourself      }
        { because the event handler will eventually call send again.       }
        { Sending the message prevent recursive call and stack overflow.   }
        { The PostMessage function posts (places) a message in a window's  }
        { message queue and then returns without waiting for the           }
        { corresponding window to process the message. The message will be }
        { seen and routed by Delphi a litle later, when we will be out of  }
        { the send function.                                               }
        PostMessage(Handle,
                    WM_ASYNCSELECT,
                    FHSocket,
                    MakeLong(FD_WRITE, 0));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Send(Data : Byte) : Integer;
var
    Buf : TBytes;
begin
    SetLength(Buf, 1);
    Buf[1] := Data;
    Result := Send(Buf, 1);
    SetLength(Buf, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return -1 if error, else return number of byte written                    }
function TCustomWSocket.SendStr(const Str : String) : Integer;
var
    Data : TBytes;
    I    : Integer;
begin
    if Length(Str) > 0 then begin
        SetLength(Data, Length(Str));
        for I := 1 to Length(Str) do
            Data[I - 1] := Ord(Str[I]);
        Result := Send(Data, Length(Str));
    end
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SendText(const Str : String);
begin
    SendStr(Str);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ASyncReceive(
    ErrCode         : Word;
    MySocketOptions : TWSocketOptions);
var
    bMore    : Boolean;
    lCount   : u_long;
    TrashCan : TBytes;
begin
    WSocketTriggerDebugEvent(Self, 'TCustomWSocket.ASyncReceive');
    bMore := TRUE;
    while bMore do begin
        FLastError := 0;

        try
            if not TriggerDataAvailable(ErrCode) then begin
                { Nothing wants to receive, we will receive and throw away  23/07/98 }
                SetLength(TrashCan, 1024);
                if DoRecv(TrashCan, Length(TrashCan), 0) = SOCKET_ERROR then begin
                    FLastError := WSocket_WSAGetLastError;
                    if FLastError = WSAEWOULDBLOCK then begin
                        FLastError := 0;
                        break;
                    end;
                end;
            end;

            { DLR Honor the socket options being passed as parameters }
            if wsoNoReceiveLoop in MySocketOptions then
                break;

            if FLastError <> 0 then begin
                bMore := FALSE;
                { -1 value is not a true error but is used to break the loop }
                if FLastError = -1 then
                    FLastError := 0;
            end
            { Check if we have something new arrived, if yes, process it }
            else if WSocket_ioctlsocket(FHSocket, FIONREAD, lCount)
                        = SOCKET_ERROR then begin
                FLastError := WSocket_WSAGetLastError;
                bMore      := FALSE;
            end
            else if lCount = 0 then
                bMore := FALSE;
        except
            bMore := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CONNECT(var msg: TMessage);
begin
    WSocketTriggerDebugEvent(Self, 'TCustomWSocket.Do_FD_CONNECT');
    if FState <> wsConnected then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(msg.LParamHi);
        if (msg.LParamHi <> 0) and (FState <> wsClosed) then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_READ(var msg: TMessage);
begin
    if FState <> wsConnected then begin
      ChangeState(wsConnected);
      TriggerSessionConnectedSpecial(msg.LParamHi);
    end;
    ASyncReceive(msg.LParamHi, FComponentOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_WRITE(var msg: TMessage);
begin
    TryToSend;
{ If you wants to test background exception, uncomment the next 2 lines. }
{   if bAllSent then                                                }
{       raise Exception.Create('Test TWSocket exception');          }
    if bAllSent then
        TriggerDataSent(msg.LParamHi);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_CLOSE(var msg: TMessage);
begin
    { In some strange situations I found that we receive a FD_CLOSE  }
    { during the connection phase, breaking the connection early !   }
    { This occurs for example after a failed FTP transfert Probably  }
    { something related to bugged winsock. Doesn't hurt with good    }
    { winsock. So let the code there !                               }
    if FState <> wsConnecting then begin
        { Check if we have something arrived, if yes, process it     }
        { DLR, since we are closing MAKE SURE WE LOOP in the receive }
        { function to get ALL remaining data                         }
        ASyncReceive(0, FComponentOptions - [wsoNoReceiveLoop]);

        if not FCloseInvoked then begin
            FCloseInvoked := TRUE;
            TriggerSessionClosed(msg.LParamHi);
        end;

        if FState <> wsClosed then
            Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Do_FD_ACCEPT(var msg: TMessage);
begin
    TriggerSessionAvailable(msg.LParamHi);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMASyncSelect(var msg: TMessage);
var
    Check  : Word;
begin
{TriggerDisplay('AsyncSelect ' + IntToStr(msg.wParam) + ', ' + IntToStr(msg.LParamLo));}
    { Verify that the socket handle is ours handle }

    if msg.wParam <> FHSocket then
        Exit;

    if FPaused then
        exit;

    Check := msg.lParamLo and FD_CONNECT;
    if Check <> 0 then begin
        FSelectMessage := FD_CONNECT;
        Do_FD_CONNECT(msg);
    end;

    Check := msg.lParamLo and FD_READ;
    if Check <> 0 then begin
        FSelectMessage := FD_READ;
        Do_FD_READ(msg);
    end;

    Check := msg.lParamLo and FD_WRITE;
    if Check <> 0 then begin
        FSelectMessage := FD_WRITE;
        Do_FD_WRITE(msg);
    end;

    Check := msg.lParamLo and FD_ACCEPT;
    if Check <> 0 then begin
        FSelectMessage := FD_ACCEPT;
        Do_FD_ACCEPT(msg);
    end;

    Check := msg.lParamLo and FD_CLOSE;
    if Check <> 0 then begin
        FSelectMessage := FD_CLOSE;
        Do_FD_CLOSE(msg);
    end;

    FSelectMessage := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure GetIPList(HostEntry : THostEnt; ToList : TStrings);
var
    AddrList   : IntPtr;
    AddrItem   : IntPtr;
    Addr       : Integer;
    I          : Integer;
begin
    ToList.Clear;
    I := 0;
    AddrList := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    while TRUE do begin
        AddrItem  := Marshal.ReadIntPtr(HostEntry.h_addr_list, I);
        if AddrItem = IntPtr.Zero then
            break;
        Addr := Marshal.ReadInt32(AddrItem);
        ToList.Add(IntToStr((Addr and $FF)) + '.' +
                   IntToStr((Addr shr 8) and $FF) + '.' +
                   IntToStr((Addr shr 16) and $FF) + '.' +
                   IntToStr((Addr shr 24) and $FF));
        Inc(I, 4);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByName(var msg: TMessage);
var
    ErrCode   : Word;
    HostEntry : THostEnt;
begin
    if FDnsLookupHandle = 0 then begin
        { We are still executing WSAAsyncGetHostByName and FDnsLookupHandle }
        { has not been assigned yet ! We should proceed later.              }
        FDnsLookupTempMsg  := msg;
        FDnsLookupCheckMsg := TRUE;
        Exit;
    end;

    if msg.wParam <> LongInt(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    ErrCode          := Msg.LParamHi;
    if ErrCode = 0 then begin
        HostEntry := THostEnt(Marshal.PtrToStructure(FDnsLookupIntPtr, TypeOf(THostEnt)));
        GetIpList(HostEntry, FDnsResultList);
        if FDnsResultList.Count > 0 then
            FDnsResult := FDnsResultList.Strings[0];
    end;
    FDnsLookupGCH.Free;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMAsyncGetHostByAddr(var msg: TMessage);
var
    HostEntry : THostEnt;
    ErrCode : Word;
begin
    if msg.wParam <> LongInt(FDnsLookupHandle) then
        Exit;
    FDnsLookupHandle := 0;
    ErrCode          := Msg.LParamHi;
    if ErrCode = 0 then begin
        HostEntry := THostEnt(Marshal.PtrToStructure(FDnsLookupIntPtr, TypeOf(THostEnt)));
        FDnsResult := Marshal.PtrToStringAnsi(HostEntry.h_name);
        FDnsResultList.Clear;
        FDnsResultList.Add(FDnsResult);
    end;
    TriggerDnsLookupDone(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetProto(sProto : String);
begin
    if FProtoAssigned and (sProto = FProtoStr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Proto if not closed');
        Exit;
    end;

    FProtoStr := Trim(sProto);
    if Length(FProtoStr) = 0 then begin
        FProtoAssigned := FALSE;
        Exit;
    end;

    FProtoResolved := FALSE;
    FProtoAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetProto : String;
begin
    Result := FProtoStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetRemotePort(sPort : String);
begin
    if FPortAssigned and (FPortStr = sPort) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Port if not closed');
        Exit;
    end;

    FPortStr := Trim(sPort);
    if Length(FPortStr) = 0 then begin
        FPortAssigned := FALSE;
        Exit;
    end;

    FPortResolved := FALSE;
    FPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetRemotePort : String;
begin
    Result := FPortStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalPort(const sLocalPort : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalPort if not closed');
        Exit;
    end;

    FLocalPortStr      := sLocalPort;
    FLocalPortResolved := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLocalAddr(const sLocalAddr : String);
begin
    if FState <> wsClosed then begin
        RaiseException('Cannot change LocalAddr if not closed');
        Exit;
    end;

    if Length(sLocalAddr) = 0 then
        FLocalAddr := '0.0.0.0'
    else
        FLocalAddr := sLocalAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXPort: String;
var
    SAddr    : TSockAddr;
    SAddrLen : Integer;
    Port     : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        SAddrLen := SizeOfTSockAddr;
        if WSocket_GetSockName(FHSocket, SAddr, SAddrLen) = 0 then begin
            port     := WSocket_ntohs(SAddr.sin_port);
            Result   := Format('%d',[Port]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetXAddr: String;
var
    SAddr    : TSockAddr;
    SAddrLen : Integer;
begin
    Result := 'error';
    if FState in [wsConnected, wsBound, wsListening] then begin
        SAddrlen := SizeOfTSockAddr;
        if WSocket_GetSockName(FHSocket, SAddr, SAddrLen) = 0 then
            Result := WSocket_inet_ntoa(SAddr.sin_addr);
     end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetAddr(InAddr : String);
begin
    if FAddrAssigned and (FAddrStr = InAddr) then
        Exit;

    if FState <> wsClosed then begin
        RaiseException('Cannot change Addr if not closed');
        Exit;
    end;

    FAddrStr := Trim(InAddr);
    if Length(FAddrStr) = 0 then begin
        FAddrAssigned := FALSE;
        Exit;
    end;

    FAddrResolved       := FALSE;
    FAddrAssigned       := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveHost(const InAddr : String) : TInAddr;
var
    Phe       : IntPtr;
    HostEntry : THostEnt;
    AddrList  : IntPtr;
    AddrItem  : IntPtr;
    IPAddr    : u_long;
begin
    if InAddr = '' then
        raise ESocketException.Create('WSocketResolveHost: ''' + InAddr +
                                      ''' Invalid Hostname.');

    if WSocketIsDottedIP(InAddr) then begin
        { Address is a dotted numeric address like 192.161.124.32 }
        IPAddr := WSocket_inet_addr(InAddr);
        if IPAddr = u_long(INADDR_NONE) then begin
            if InAddr = '255.255.255.255' then begin
                Result.s_addr := u_long(INADDR_BROADCAST);
                Exit;
            end;
            raise ESocketException.Create('WSocketResolveHost: ''' + InAddr +
                                          ''' Invalid IP address.');
        end;
        Result.s_addr := IPAddr;
        Exit;
    end;

    { Address is a hostname }
    Phe := OverByte.Ics.Winsock.GetHostByName(InAddr);
    if Phe = IntPtr.Zero then
        raise ESocketException.CreateFmt(
           'WSocketResolveHost: Cannot convert host address ''%s'', Error #%d',
           [InAddr, OverByte.Ics.Winsock.WSAGetLastError]);
    HostEntry     := THostEnt(Marshal.PtrToStructure(Phe, TypeOf(THostEnt)));
    AddrList      := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    AddrItem      := Marshal.ReadIntPtr(HostEntry.h_addr_list);
    Result.s_addr := Marshal.ReadInt32(AddrItem);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Convert port name or number to number in host order (ftp -> 21)           }
function WSocketResolvePort(const Port : String; const Proto : String) : WORD;
var
    Pse       : IntPtr;
    ServEntry : TServEnt;
begin
    if Port = '' then
        raise ESocketException.Create('WSocketResolvePort: Invalid Port.');

    if IsDigit(Port[1]) then
        Result := StrToInt(Port)
    else begin
        if Proto = '' then
            Pse := OverByte.Ics.Winsock.GetServByName(Port, '')
        else
            Pse := OverByte.Ics.Winsock.GetServByName(Port, Proto);
        if Pse = IntPtr.Zero then
            raise ESocketException.CreateFmt(
                     'WSocketResolvePort: Cannot convert port ''%s'', Error #%d',
                     [Port, OverByte.Ics.Winsock.WSAGetLastError]);
        ServEntry := TServEnt(Marshal.PtrToStructure(Pse, TypeOf(TServEnt)));
        Result := OverByte.Ics.Winsock.ntohs(ServEntry.s_port);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveProto(const Proto : String) : Integer;
var
    Ppe        : IntPtr;
    ProtoEntry : TProtoEnt;
begin
    if Proto = '' then
        raise ESocketException.Create('WSocketResolveProto: Invalid Protocol.');

    if IsDigit(Proto[1]) then
        Result := StrToInt(Proto)
    else begin
        ppe := OverByte.Ics.WinSock.getprotobyname(Proto);
        if Ppe = IntPtr.Zero then
            raise ESocketException.CreateFmt(
                      'WSocketResolveProto: Cannot convert protocol ''%s'', Error #%d',
                      [Proto, OverByte.Ics.WinSock.WSAGetLastError]);
        ProtoEntry := TProtoEnt(Marshal.PtrToStructure(Ppe, TypeOf(TProtoEnt)));
        Result     := ProtoEntry.p_proto;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetAddr : String;
begin
    Result := FAddrStr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSockName(
    out saddr    : TSockAddrIn;
    var saddrlen : Integer) : Integer;
begin
    Result := WSocket_GetSockName(FHSocket, TSockAddr(saddr), saddrlen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerAddr: string;
var
    saddr    : TSockAddrIn;
    saddrlen : Integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := WSocket_inet_ntoa(saddr.sin_addr)
        else
            SocketError('GetPeerName');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerPort: string;
var
    saddr    : TSockAddrIn;
    saddrlen : integer;
begin
    Result := 'error';
    if FState = wsConnected then begin
        saddrlen := sizeof(saddr);
        if WSocket_GetPeerName(FHSocket, TSockAddr(saddr), saddrlen) = 0 then
            Result := IntToStr(WSocket_ntohs(saddr.sin_port))
        else begin
            SocketError('GetPeerPort');
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetPeerName(var Name : TSockAddrIn; NameLen : Integer) : integer;
begin
    if FState = wsConnected then
        Result := WSocket_GetPeerName(FHSocket, TSockAddr(Name), NameLen)
    else
        Result := SOCKET_ERROR;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CancelDnsLookup;
begin
    if FDnsLookupHandle = 0 then
        Exit;
    if WSocket_WSACancelAsyncRequest(FDnsLookupHandle) <> 0 then begin
        FDnsLookupHandle := 0;
        SocketError('WSACancelAsyncRequest');
        Exit;
    end;
    FDnsLookupHandle := 0;
{$IFDEF ICS_COMPONENT}
    if not (csDestroying in ComponentState) then
{$ENDIF}
        TriggerDnsLookupDone(WSAEINTR);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DnsLookup(const HostName : String);
var
    IPAddr  : TInAddr;
begin
    if HostName = '' then begin
        RaiseException('DNS lookup: invalid host name.');
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;

    { Cancel any pending lookup }
    if FDnsLookupHandle <> 0 then begin
        WSocket_WSACancelAsyncRequest(FDnsLookupHandle);
        FDnsLookupHandle := 0;
    end;

    FDnsResult := '';
    FDnsResultList.Clear;

    if WSocketIsDottedIP(Hostname) then begin   { 28/09/2002 }
        IPAddr.S_addr := WSocket_inet_addr(HostName);
        if IPAddr.S_addr <> u_long(INADDR_NONE) then begin
            FDnsResult := WSocket_inet_ntoa(IPAddr);
            FDnsResultList.Add(FDnsResult);     { 28/09/2002 }{ 12/02/2003 }
            TriggerDnsLookupDone(0);
            Exit;
        end;
    end;

    if FWindowHandle = 0 then
        RaiseException('DnsLookup: Window not assigned');

    { John Goodwin found a case where winsock dispatch WM_ASYNCGETHOSTBYNAME }
    { message before returning from WSAAsyncGetHostByName call. Because of   }
    { that, FDnsLookupHandle is not yet assigned when execution comes in     }
    { WMAsyncGetHostByName. John use a flag to check this situation.         }
    FDnsLookupCheckMsg := FALSE;
    SetLength(FDnsLookupBuffer, MAXGETHOSTSTRUCT);
    FDnsLookupGCH    := GCHandle.Alloc(FDnsLookupBuffer, GCHandleType.Pinned);
    FDnsLookupIntPtr := FDnsLookupGCH.AddrOfPinnedObject;
    FDnsLookupHandle := WSocket_WSAAsyncGetHostByName(
                              FWindowHandle,
                              WM_ASYNCGETHOSTBYNAME,
                              HostName,
                              FDnsLookupIntPtr,
                              MAXGETHOSTSTRUCT);
    if FDnsLookupHandle = 0 then begin
        FDnsLookupGCH.Free;
        RaiseExceptionFmt(
                  '%s: can''t start DNS lookup, error #%d',
                  [HostName, WSocket_WSAGetLastError]);
        Exit;
    end;
    if FDnsLookupCheckMsg then begin
        FDnsLookupCheckMsg := FALSE;
        WMAsyncGetHostByName(FDnsLookupTempMsg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.ReverseDnsLookup(const HostAddr: String);
var
    lAddr  : u_long;
begin
    if Length(HostAddr) = 0 then begin
        RaiseException('ReverseDnsLookup: Invalid host name.');
        TriggerDnsLookupDone(WSAEINVAL);
        Exit;
    end;
    // Cancel any pending lookup
    if FDnsLookupHandle <> 0 then
        WSocket_WSACancelAsyncRequest(FDnsLookupHandle);

    FDnsResult := '';
    FDnsResultList.Clear;

    lAddr := WSocket_inet_addr(HostAddr);

    if FWindowHandle = 0 then
        RaiseException('ReverseDnsLookup: Window not assigned');

    SetLength(FDnsLookupBuffer, MAXGETHOSTSTRUCT);
    FDnsLookupGCH    := GCHandle.Alloc(FDnsLookupBuffer, GCHandleType.Pinned);
    FDnsLookupIntPtr := FDnsLookupGCH.AddrOfPinnedObject;
    FDnsLookupHandle := WSocket_WSAAsyncGetHostByAddr(
                            FWindowHandle,
                            WM_ASYNCGETHOSTBYADDR,
                            lAddr, 4, PF_INET,
                            FDnsLookupIntPtr,
                            MAXGETHOSTSTRUCT);
    if FDnsLookupHandle = 0 then begin
        FDnsLookupGCH.Free;
        RaiseExceptionFmt('%s: can''t start DNS lookup, error #%d',
                          [HostAddr, WSocket_WSAGetLastError]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.BindSocket;
var
    SockName      : TSockAddr;
    SockNamelen   : Integer;
    LocalSockName : TSockAddrIn;
    I             : Integer;
begin
    for I := Low(LocalSockName.sin_zero) to High(LocalSockName.sin_zero) do
        LocalSockName.sin_zero[0] := #0;
    LocalSockName.sin_family      := AF_INET;
    LocalSockName.sin_port        := WSocket_htons(FLocalPortNum);
    LocalSockName.sin_addr.s_addr := WSocketResolveHost(FLocalAddr).s_addr;
    SockNamelen                   := SizeOf(LocalSockName);

    if WSocket_bind(FHSocket, LocalSockName, SockNamelen) <> 0 then begin
        RaiseExceptionFmt('winsock.bind failed, error #%d',
                          [WSocket_WSAGetLastError]);
        Exit;
    end;
    SockNamelen := SizeOf(SockName);
    if WSocket_getsockname(FHSocket, SockName, SockNamelen) <> 0 then begin
        RaiseExceptionFmt('winsock.getsockname failed, error #%d',
                          [WSocket_WSAGetLastError]);
        Exit;
    end;
    FLocalPortNum := WSocket_ntohs(SockName.sin_port);
    FLocalPortStr := IntToStr(FLocalPortNum);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetLingerOption;
var
    iStatus : Integer;
    li      : TLinger;
begin
    if FLingerOnOff = wsLingerNoSet then
        Exit;                            { Option set is disabled, ignore }

    if FHSocket = INVALID_SOCKET then begin
        RaiseException('Cannot set linger option at this time');
        Exit;
    end;

    li.l_onoff  := Ord(FLingerOnOff);    { 0/1 = disable/enable linger }
    li.l_linger := FLingerTimeout;       { timeout in seconds          }
    iStatus     := WSocket_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_LINGER, li, SizeOf(li));

    if iStatus <> 0 then begin
        SocketError('setsockopt(SO_LINGER)');
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Connect;
var
    iStatus : Integer;
    optval  : Integer;
    lAddr   : TInAddr;
begin
    if (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) then begin
        RaiseException('Connect: Socket already in use');
        Exit;
    end;

    if  not FPortAssigned then begin
        RaiseException('Connect: No Port Specified');
        Exit;
    end;

    if not FAddrAssigned then begin
        RaiseException('Connect: No IP Address Specified');
        Exit;
    end;

    if not FProtoAssigned then begin
        RaiseException('Connect: No Protocol Specified');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            FProto := WSocketResolveProto(FProtoStr);
            case FProto of
            IPPROTO_UDP: FType := SOCK_DGRAM;
            IPPROTO_TCP: FType := SOCK_STREAM;
            else
                         FType := SOCK_RAW;
            end;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocketResolvePort(FPortStr, FProtoStr);
            sin.sin_port  := WSocket_htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FLocalPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FLocalPortNum      := WSocketResolvePort(FLocalPortStr, FProtoStr);
            FLocalPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocketResolveHost(FAddrStr).s_addr;
            FAddrResolved := TRUE;
        end;
    except
        on E:Exception do begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := WSocket_socket(FAddrFormat, FType, FProto);
    if FHSocket = INVALID_SOCKET then begin
        SocketError('Connect (socket)');
        Exit;
    end;
    ChangeState(wsOpened);

    if FState <> wsOpened then begin  { 07/07/02 }
        { Socket has been closed in the OnChangeState event ! }
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('Connect (Invalid operation in OnChangeState)');
        Exit;
    end;

    if FType = SOCK_DGRAM then begin
        BindSocket;
        if FMultiCast then begin
            if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then begin
                optval  := FMultiCastIpTTL; { set time-to-live for multicast }
                iStatus := WSocket_SetSockOpt(FHSocket, IPPROTO_IP,
                                              IP_MULTICAST_TTL,
                                              optval,
                                              SizeOf(optval));
                if iStatus <> 0 then begin
                        SocketError('setsockopt(IP_MULTICAST_TTL)');
                        Exit;
                end;
            end;
            if FLocalAddr <> '0.0.0.0' then begin                      { RK }
                laddr.s_addr := WSocketResolveHost(FLocalAddr).s_addr;
                iStatus      := WSocket_SetSockOpt(FHSocket, IPPROTO_IP,
                                                                IP_MULTICAST_IF,
                                                                laddr,
                                                                SizeOf(laddr));
                if iStatus <> 0 then begin
                    SocketError('setsockopt(IP_MULTICAST_IF)');
                    Exit;
                end;
            end;                                                       { /RK }
        end;

        if sin.sin_addr.S_addr = u_long(INADDR_BROADCAST) then begin
            OptVal  := 1;
            iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET, SO_BROADCAST,
                                          OptVal, SizeOf(OptVal));
            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_BROADCAST)');
                Exit;
            end;
        end;
    end
    else begin
        { Socket type is SOCK_STREAM }
        optval  := -1;
        iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_REUSEADDR, optval, SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_REUSEADDR)');
            Exit;
        end;

        if wsoTcpNoDelay in FComponentOptions then begin
            optval := -1; { true, 0=false }
            iStatus := WSocket_setsockopt(FHsocket, IPPROTO_TCP,
                                          TCP_NODELAY, optval, SizeOf(optval));
            if iStatus <> 0 then begin
                SocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)');
                Exit;
            end;
        end;

        SetLingerOption;

        optval  := -1;
        iStatus := WSocket_setsockopt(FHSocket, SOL_SOCKET,
                                      SO_KEEPALIVE, optval, SizeOf(optval));

        if iStatus <> 0 then begin
            SocketError('setsockopt(SO_KEEPALIVE)');
            Exit;
        end;

        if (FLocalPortNum <> 0) or (FLocalAddr <> '0.0.0.0') then
            BindSocket;
    end;

    FSelectEvent := FD_READ   or FD_WRITE or FD_CLOSE or
                    FD_ACCEPT or FD_CONNECT;
    iStatus       := WSocket_WSAASyncSelect(FHSocket, FWindowHandle,
                                            WM_ASYNCSELECT, FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAAsyncSelect');
        Exit;
    end;

    if FType = SOCK_DGRAM then begin
        ChangeState(wsConnected);
        TriggerSessionConnectedSpecial(0);
    end
    else begin
        iStatus := WSocket_connect(FHSocket, sin, SizeOfTSockAddr);
        if iStatus = 0 then
            ChangeState(wsConnecting)
        else begin
            iStatus := WSocket_WSAGetLastError;
            if iStatus = WSAEWOULDBLOCK then
                ChangeState(wsConnecting)
            else begin
                FLastError := iStatus;
                SocketError('Connect');
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Listen;
var
    iStatus : Integer;
    optval  : Integer;
    mreq    : ip_mreq;
    szAddr : array[0..256] of char;
begin
    if not FPortAssigned then begin
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('listen: port not assigned');
        Exit;
    end;

    if not FProtoAssigned then begin
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('listen: protocol not assigned');
        Exit;
    end;

    if not FAddrAssigned then begin
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('listen: address not assigned');
        Exit;
    end;

    try
        if not FProtoResolved then begin
            { The next line will trigger an exception in case of failure }
            FProto := WSocketResolveProto(FProtoStr);
            if FProto = IPPROTO_UDP then
                FType := SOCK_DGRAM
            else
                FType := SOCK_STREAM;
            FProtoResolved := TRUE;
        end;

        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            FPortNum      := WSocketResolvePort(FPortStr, GetProto);
            sin.sin_port  := WSocket_htons(FPortNum);
            FPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocketResolveHost(FAddrStr).s_addr;
            FAddrResolved       := TRUE;
        end;
    except
        on E:Exception do begin
            RaiseException('listen: ' + E.Message);
            Exit;
        end;
    end;

    { Remove any data from the internal output buffer }
    { (should already be empty !)                     }
    DeleteBufferedData;

    FHSocket := WSocket_socket(FAddrFormat, FType, FProto);

    if FHSocket = INVALID_SOCKET then begin
        SocketError('socket');
        exit;
    end;

    if FType = SOCK_DGRAM then begin
        if FReuseAddr then begin
        { Enable multiple tasks to listen on duplicate address and port }
            optval  := -1;
            iStatus := WSocket_SetSockOpt(FHSocket, SOL_SOCKET,
                                          SO_REUSEADDR,
                                          optval, SizeOf(optval));

            if iStatus <> 0 then begin
                SocketError('setsockopt(SO_REUSEADDR)');
                Exit;
            end;
        end;
    end;

    iStatus := WSocket_bind(FHSocket, sin, sizeof(sin));
    if iStatus = 0 then
        ChangeState(wsBound)
    else begin
        SocketError('Bind');
        Close;
        Exit;
    end;

    if FType = SOCK_DGRAM then begin
        if FMultiCast then begin
             { Use setsockopt() to join a multicast group }
             { mreq.imr_multiaddr.s_addr := WSocket_inet_addr('225.0.0.37');}
             { mreq.imr_multiaddr.s_addr := sin.sin_addr.s_addr;}
             { mreq.imr_multiaddr.s_addr := WSocket_inet_addr(FAddrStr);}
             mreq.imr_multiaddr.s_addr :=  WSocket_inet_addr(FMultiCastAddrStr);
             { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
             mreq.imr_interface.s_addr := WSocketResolveHost(FAddrStr).s_addr;
             iStatus := WSocket_SetSockOpt(FHSocket, IPPROTO_IP,
                                           IP_ADD_MEMBERSHIP,
                                           mreq, SizeOf(mreq));

             if iStatus <> 0 then begin
                SocketError('setsockopt(IP MULTICAST)');
                Exit;
             end;
        end;
    end;

    case FType of
    SOCK_RAW,
    SOCK_DGRAM :
        begin
            ChangeState(wsListening);
            ChangeState(wsConnected);
            TriggerSessionConnectedSpecial(0);
        end;
    SOCK_STREAM :
        begin
            iStatus := WSocket_listen(FHSocket, FListenBacklog);
            if iStatus = 0 then
                ChangeState(wsListening)
            else begin
                SocketError('Listen');
                Exit;
            end;
        end;
    else
        SocketError('Listen: unexpected protocol.');
        Exit;
    end;


    FSelectEvent := FD_READ   or FD_WRITE or
                    FD_ACCEPT or FD_CLOSE;
    iStatus      := WSocket_WSAASyncSelect(FHSocket, Handle,
                                           WM_ASYNCSELECT, FSelectEvent);
    if iStatus <> 0 then begin
        SocketError('WSAASyncSelect');
        exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.Accept: TSocket; 
var
   Len : Integer;
begin
    if FState <> wsListening then begin
        WSocket_WSASetLastError(WSAEINVAL);
        SocketError('not a listening socket');
        Result := INVALID_SOCKET;
        Exit;
    end;

    Len      := SizeOfTSockAddr;
    FASocket := WSocket_accept(FHSocket, sin, Len);

    if FASocket = INVALID_SOCKET then begin
        SocketError('Accept');
        Result := INVALID_SOCKET;
        Exit;
    end
    else
        Result := FASocket;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Pause;
begin
    FPaused := TRUE;
    WSocket_WSAASyncSelect(FHSocket, Handle, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Resume;
begin
    FPaused := FALSE;
    WSocket_WSAASyncSelect(FHSocket, Handle, WM_ASYNCSELECT, FSelectEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Shutdown(How : Integer);
begin
    if FHSocket <> INVALID_SOCKET then
        WSocket_shutdown(FHSocket, How);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.DeleteBufferedData;
var
    nItem : Integer;
begin
    // Delete all data buffer
    for nItem := 0 to FBufList.Count - 1 do
        TBuffer(FBufList.Items[nItem]).Free;
    FBufList.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Abort;
begin
    InternalAbort(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.AbortComponent;
begin
    Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalAbort(ErrCode : Word);
begin
    CancelDnsLookup;
    DeleteBufferedData;
    { Be sure to close as fast as possible (abortive close) }
    if (FState = wsConnected) and (FProto = IPPROTO_TCP) then begin
        FLingerOnOff := wsLingerOff;
        SetLingerOption;
    end;
    InternalClose(FALSE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Close;
begin
    InternalClose(TRUE, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.CloseDelayed;
begin
    PostMessage(Handle, WM_CLOSE_DELAYED, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Release;
begin
    PostMessage(Handle, WM_WSOCKET_RELEASE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMCloseDelayed(var msg: TMessage);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WMRelease(var msg: TMessage);
begin
    Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.Flush;
begin
    while (FHSocket <> INVALID_SOCKET) and     { No more socket   }
          (not bAllSent) do begin              { Nothing to send  }
            { Break; }
        TryToSend;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.InternalClose(bShut : Boolean; ErrCode : Word);
var
    iStatus : Integer;
begin
    if FHSocket = INVALID_SOCKET then begin
        if FState <> wsClosed then begin
            ChangeState(wsClosed);
            AssignDefaultValue;
        end;
        exit;
    end;

    if FState = wsClosed then
        Exit;

{ 11/10/98 called shutdown(1) instead of shutdonw(2). This disable only     }
{ reception. Disabling data send produced data lost is some cases. For      }
{ example when a client open the connection, send some data fast then close }
{ the connection immediately, even using the linger option.                 }
    if bShut then
        Self.ShutDown(1);

    if FHSocket <> INVALID_SOCKET then begin
        repeat
            // Close the socket
            iStatus := WSocket_closesocket(FHSocket);
            if iStatus <> 0 then begin
                FLastError := WSocket_WSAGetLastError;
                if FLastError <> WSAEWOULDBLOCK then begin
                    FHSocket := INVALID_SOCKET;
                    { Ignore the error occuring when winsock DLL not      }
                    { initialized (occurs when using TWSocket from a DLL) }
                    if FLastError = WSANOTINITIALISED then
                        break;
                    SocketError('Disconnect (closesocket)');
                    Exit;
                end;
                MessagePump;
            end;
        until iStatus = 0;
        FHSocket := INVALID_SOCKET;
    end;

    ChangeState(wsClosed);
    if
{$IFDEF ICS_COMPONENT}
       (not (csDestroying in ComponentState)) and
{$ENDIF}
       (not FCloseInvoked) {and Assigned(FOnSessionClosed)} then begin
        FCloseInvoked := TRUE;
        TriggerSessionClosed(ErrCode);
    end;
    { 29/09/98 Protect AssignDefaultValue because SessionClosed event handler }
    { may have destroyed the component.                                       }
    try
        AssignDefaultValue;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.WaitForClose;
var
    lCount    : u_long;
    Status    : Integer;
    Ch        : TBytes;
begin
    SetLength(Ch, 1);
    while (FHSocket <> INVALID_SOCKET) and (FState <> wsClosed) do begin
        MessagePump;

        if WSocket_ioctlsocket(FHSocket, FIONREAD, lCount) = SOCKET_ERROR then
            break;
        if lCount > 0 then
            TriggerDataAvailable(0);

        Status := DoRecv(Ch, 1, 0);
        if Status <= 0 then begin
            FLastError := WSocket_WSAGetLastError;
            if FLastError <> WSAEWOULDBLOCK then
                break;
        end;
        MessagePump;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Todo: create a DotNet version for THostEnt                                }
function WSocketGetHostByAddr(const Addr : String) : IntPtr;
var
    lAddr        : u_long;
    LookupIntPtr : IntPtr;
begin
    if Length(Addr) = 0 then
        raise ESocketException.Create('WSocketGetHostByAddr: Invalid address.');

    lAddr        := WSocket_inet_addr(Addr);
    LookupIntPtr := WSocket_gethostbyaddr(lAddr, 4, PF_INET);
    Result       := LookupIntPtr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketResolveIp(const IpAddr : String) : String;
var
    HostEntry    : THostEnt;
    LookupIntPtr : IntPtr;
begin
    LookupIntPtr := WSocketGetHostByAddr(IpAddr);
    if LookupIntPtr = IntPtr.Zero then
        Result := ''
    else begin
        HostEntry    := THostEnt(Marshal.PtrToStructure(LookupIntPtr,
                                                        TypeOf(THostEnt)));
        Result    := Marshal.PtrToStringAnsi(HostEntry.h_name);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketGetHostByName(const Name : String) : IntPtr;
begin
    if Length(Name) = 0 then
        raise ESocketException.Create('WSocketGetHostByName: Invalid Hostname.');

    Result := WSocket_gethostbyname(Name);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalIPList : TStrings;
var
    HostEntry    : THostEnt;
    LookupIntPtr : IntPtr;
begin
    IPList.Clear;
    Result := IPList;

    LookupIntPtr := WSocketGetHostByName(LocalHostName);
    if LookupIntPtr <> IntPtr.Zero then begin
        HostEntry := THostEnt(Marshal.PtrToStructure(LookupIntPtr,
                                                     TypeOf(THostEnt)));
        GetIpList(HostEntry, IPList);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function LocalHostName : String;
begin
    if WSocket_gethostname(Result) <> 0 then
        raise ESocketException.Create('Winsock.GetHostName failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerIsSet(var tvp : TTimeVal) : Boolean;
begin
    Result := (tvp.tv_sec <> 0) or (tvp.tv_usec <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TimerCmp(var tvp : TTimeVal; var uvp : TTimeVal; IsEqual : Boolean) : Boolean;
begin
    Result := (tvp.tv_sec = uvp.tv_sec) and (tvp.tv_usec = uvp.tv_usec);
    if not IsEqual then
        Result := not Result;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TimerClear(var tvp : TTimeVal);
begin
   tvp.tv_sec  := 0;
   tvp.tv_usec := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SetSendFlags(newValue : TSocketSendFlags);
begin
    case newValue of
    wsSendNormal: FSendFlags := 0;
    wsSendUrgent: FSendFlags := MSG_OOB;
    else
        RaiseException('Invalid SendFlags');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.GetSendFlags : TSocketSendFlags;
begin
    case FSendFlags of
    0       : Result := wsSendNormal;
    MSG_OOB : Result := wsSendUrgent;
    else
        RaiseException('Invalid internal SendFlags');
        Result := wsSendNormal;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDisplay(Msg : String);
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSendData(BytesSent : Integer);
begin
    if Assigned(FOnSendData) then
        FOnSendData(Self, BytesSent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionAvailable(ErrCode : Word);
begin
    if Assigned(FOnSessionAvailable) then
        FOnSessionAvailable(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnectedSpecial(ErrCode : Word);
begin
    TriggerSessionConnected(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionConnected(ErrCode : Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerSessionClosed(ErrCode : Word);
begin
    if Assigned(FOnSessionClosed) then
        FOnSessionClosed(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
begin
    Result := Assigned(FOnDataAvailable);
    if not Result then
        Exit;
    { Do not allow FD_READ messages, this will prevent reentering the }
    { OnDataAvailable event handler.                                  }
    FSelectEvent := FD_WRITE or FD_CLOSE or FD_CONNECT;
    WSocket_WSAASyncSelect(FHSocket, FWindowHandle,
                           WM_ASYNCSELECT, FSelectEvent);
    try
        FRcvdFlag := TRUE;
        while Result and FRcvdFlag do begin
            { Trigger user code. This will normally call DoRecv which will }
            { update FRcvdFlag.                                            }
            { If user code is wrong, we'll loop forever !                  }
            FOnDataAvailable(Self, ErrCode);
            Result := Assigned(FOnDataAvailable);
        end;
    finally
        { Allow all events now }
        FSelectEvent := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
        WSocket_WSAASyncSelect(FHSocket, FWindowHandle,
                               WM_ASYNCSELECT, FSelectEvent);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDataSent(ErrCode : Word);
begin
    if Assigned(FOnDataSent) then
        FOnDataSent(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerError;
begin
    if Assigned(FOnError) then
        FOnError(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerDNSLookupDone(ErrCode : Word);
begin
    if Assigned(FOnDNSLookupDone) then
        FOnDNSLookupDone(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.TriggerChangeState(OldState, NewState : TSocketState);
begin
    if Assigned(FOnChangeState) then
        FOnChangeState(Self, OldState, NewState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomWSocket.SocketError(sockfunc: String);
var
    ErrCode : Integer;
    Line    : String;
begin
    ErrCode := OverByte.Ics.Winsock.WSAGetLastError;
    Line    := 'Error '+ IntToStr(ErrCode) + ' in function ' + sockfunc;
    //MessageBox(0, Line, 'TCustomWSocket.SocketError', MB_OK);
    if (ErrCode = WSAECONNRESET) or
       (ErrCode = WSAENOTCONN)   then begin
        WSocket_closesocket(FHSocket);
        FHSocket := INVALID_SOCKET;
        if FState <> wsClosed then
           TriggerSessionClosed(ErrCode);
        ChangeState(wsClosed);
    end;
    FLastError := ErrCode;
    RaiseException(Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function WSocketErrorDesc(ErrCode: Integer) : String;
begin
    case ErrCode of
    0:
      WSocketErrorDesc := 'No Error';
    WSAEINTR:
      WSocketErrorDesc := 'Interrupted system call';
    WSAEBADF:
      WSocketErrorDesc := 'Bad file number';
    WSAEACCES:
      WSocketErrorDesc := 'Permission denied';
    WSAEFAULT:
      WSocketErrorDesc := 'Bad address';
    WSAEINVAL:
      WSocketErrorDesc := 'Invalid argument';
    WSAEMFILE:
      WSocketErrorDesc := 'Too many open files';
    WSAEWOULDBLOCK:
      WSocketErrorDesc := 'Operation would block';
    WSAEINPROGRESS:
      WSocketErrorDesc := 'Operation now in progress';
    WSAEALREADY:
      WSocketErrorDesc := 'Operation already in progress';
    WSAENOTSOCK:
      WSocketErrorDesc := 'Socket operation on non-socket';
    WSAEDESTADDRREQ:
      WSocketErrorDesc := 'Destination address required';
    WSAEMSGSIZE:
      WSocketErrorDesc := 'Message too long';
    WSAEPROTOTYPE:
      WSocketErrorDesc := 'Protocol wrong type for socket';
    WSAENOPROTOOPT:
      WSocketErrorDesc := 'Protocol not available';
    WSAEPROTONOSUPPORT:
      WSocketErrorDesc := 'Protocol not supported';
    WSAESOCKTNOSUPPORT:
      WSocketErrorDesc := 'Socket type not supported';
    WSAEOPNOTSUPP:
      WSocketErrorDesc := 'Operation not supported on socket';
    WSAEPFNOSUPPORT:
      WSocketErrorDesc := 'Protocol family not supported';
    WSAEAFNOSUPPORT:
      WSocketErrorDesc := 'Address family not supported by protocol family';
    WSAEADDRINUSE:
      WSocketErrorDesc := 'Address already in use';
    WSAEADDRNOTAVAIL:
      WSocketErrorDesc := 'Address not available';
    WSAENETDOWN:
      WSocketErrorDesc := 'Network is down';
    WSAENETUNREACH:
      WSocketErrorDesc := 'Network is unreachable';
    WSAENETRESET:
      WSocketErrorDesc := 'Network dropped connection on reset';
    WSAECONNABORTED:
      WSocketErrorDesc := 'Connection aborted';
    WSAECONNRESET:
      WSocketErrorDesc := 'Connection reset by peer';
    WSAENOBUFS:
      WSocketErrorDesc := 'No buffer space available';
    WSAEISCONN:
      WSocketErrorDesc := 'Socket is already connected';
    WSAENOTCONN:
      WSocketErrorDesc := 'Socket is not connected';
    WSAESHUTDOWN:
      WSocketErrorDesc := 'Can''t send after socket shutdown';
    WSAETOOMANYREFS:
      WSocketErrorDesc := 'Too many references: can''t splice';
    WSAETIMEDOUT:
      WSocketErrorDesc := 'Connection timed out';
    WSAECONNREFUSED:
      WSocketErrorDesc := 'Connection refused';
    WSAELOOP:
      WSocketErrorDesc := 'Too many levels of symbolic links';
    WSAENAMETOOLONG:
      WSocketErrorDesc := 'File name too long';
    WSAEHOSTDOWN:
      WSocketErrorDesc := 'Host is down';
    WSAEHOSTUNREACH:
      WSocketErrorDesc := 'No route to host';
    WSAENOTEMPTY:
      WSocketErrorDesc := 'Directory not empty';
    WSAEPROCLIM:
      WSocketErrorDesc := 'Too many processes';
    WSAEUSERS:
      WSocketErrorDesc := 'Too many users';
    WSAEDQUOT:
      WSocketErrorDesc := 'Disc quota exceeded';
    WSAESTALE:
      WSocketErrorDesc := 'Stale NFS file handle';
    WSAEREMOTE:
      WSocketErrorDesc := 'Too many levels of remote in path';
    WSASYSNOTREADY:
      WSocketErrorDesc := 'Network sub-system is unusable';
    WSAVERNOTSUPPORTED:
      WSocketErrorDesc := 'WinSock DLL cannot support this application';
    WSANOTINITIALISED:
      WSocketErrorDesc := 'WinSock not initialized';
    WSAHOST_NOT_FOUND:
      WSocketErrorDesc := 'Host not found';
    WSATRY_AGAIN:
      WSocketErrorDesc := 'Non-authoritative host not found';
    WSANO_RECOVERY:
      WSocketErrorDesc := 'Non-recoverable error';
    WSANO_DATA:
      WSocketErrorDesc := 'No Data';
    else
      WSocketErrorDesc := 'Not a WinSock error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

         X X        X X        X X       X      X      X X      X X X X
       X     X    X     X    X     X     X     X     X     X    X
       X          X     X    X           X   X       X          X
         X X      X     X    X           X X           X X        X X
             X    X     X    X           X   X             X          X
       X     X    X     X    X     X     X     X     X     X    X     X
         X X        X X        X X       X      X      X  X       X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.AssignDefaultValue;
begin
    inherited AssignDefaultValue;
    FSocksState          := socksData;
    FSocksServer         := '';
    FSocksPort           := '';
    FSocksLevel          := '5';
    FSocksRcvdCnt        := 0;
    FSocksPortAssigned   := FALSE;
    FSocksServerAssigned := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksLevel(newValue : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks level if not closed');
        Exit;
    end;
    if (newValue <> '4')  and (newValue <> '5') and
       (newValue <> '4A') and (newValue <> '4a') then begin
        RaiseException('Invalid socks level. Must be 4, 4A or 5.');
        Exit;
    end;
    FSocksLevel := UpperCase(newValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksPort(sPort : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks port if not closed');
        Exit;
    end;
    FSocksPort := Trim(sPort);
    if Length(FSocksPort) = 0 then begin
        FSocksPortAssigned := FALSE;
        Exit;
    end;
    FSocksPortAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SetSocksServer(sServer : String);
begin
    if State <> wsClosed then begin
        RaiseException('Can''t change socks server if not closed');
        Exit;
    end;
    FSocksServer := Trim(sServer);
    if Length(FSocksServer) = 0 then begin
        FSocksServerAssigned := FALSE;
        Exit;
    end;
    FSocksServerAssigned := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Listen;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, Listen as usual }
        inherited Listen;
        Exit;
    end;
    RaiseException('Listening is not supported thru socks server');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.Connect;
begin
    { Check if we really wants to use socks server }
    if not FSocksServerAssigned then begin
        { No socks server assigned, connect as usual }
        inherited Connect;
        Exit;
    end;

    if LowerCase(FProtoStr) <> 'tcp' then begin
        RaiseException('tcp is the only protocol supported thru socks server');
        Exit;
    end;

    try
        if not FPortResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_port  := WSocket_htons(WSocketResolvePort(FSocksPort, FProtoStr));
            FPortResolved := TRUE;
        end;

        if not FAddrResolved then begin
            { The next line will trigger an exception in case of failure }
            sin.sin_addr.s_addr := WSocketResolveHost(FSocksServer).s_addr;
            FAddrResolved       := TRUE;
        end;
        { The next line will trigger an exception in case of failure }
        FPortNum := WSocketResolvePort(FPortStr, FProtoStr);
    except
        on E:Exception do begin
            RaiseException('connect: ' + E.Message);
            Exit;
        end;
    end;

    FSocksState := socksNegociateMethods;
    FRcvCnt     := 0;
    inherited Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BufToStr(const Buf : TBytes; Cnt : Integer) : String;
var
    I : Integer;
begin
    Result := '';
    for I := 0 to Cnt - 1 do begin
        if (Buf[I] >= 32) and (Buf[I] <= 126) then
            Result := Result + Char(Buf[I])
        else
            Result := Result + '#' + Format('%2.2d', [Buf[I]]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionConnectedSpecial(ErrCode : Word);
var
    Buf : TBytes;
begin
    if FSocksState = socksNegociateMethods then begin
        {ChangeState(wsSocksConnected);}
        TriggerSocksConnected(ErrCode);
        if ErrCode <> 0 then begin
            inherited TriggerSessionConnectedSpecial(ErrCode);
            Exit;
        end;
        if FSocksLevel[1] = '4' then
            SocksDoConnect
        else begin
            if FSocksAuthentication = socksNoAuthentication then
                FSocksAuthNumber := #$00        { No authentification }
            else
                FSocksAuthNumber := #$02;       { Usercode/Password   }

            SetLength(Buf, 3);
            Buf[0] := $05;                      { Version number      }
            Buf[1] := $01;                      { Number of methods   }
            Buf[2] := Ord(FSocksAuthNumber);    { Method identifier   }
{TriggerDisplay('Send = ''' + BufToStr(Buf, 3) + '''');}
//MessageBox(0, BufToStr(Buf, 3), 'TriggerSessionConnectedSpecial', MB_OK);
            Send(Buf, 3);
        end;
    end
    else
        inherited TriggerSessionConnectedSpecial(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSessionClosed(ErrCode : Word);
begin
    if FSocksState = socksAuthenticate then
        TriggerSocksAuthState(socksAuthFailure);
    inherited TriggerSessionClosed(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksConnected(ErrCode : Word);
begin
    if Assigned(FOnSocksConnected) then
        FOnSocksConnected(Self, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksError(
    ErrCode : Integer; Msg : String);
begin
    if Assigned(FOnSocksError) then
        FOnSocksError(Self, ErrCode, Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.TriggerSocksAuthState(
    AuthState : TSocksAuthState);
begin
    if Assigned(FOnSocksAuthState) then
        FOnSocksAuthState(Self, AuthState);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoAuthenticate;
var
    Buf     : TBytes;
    I, J    : Integer;
begin
    FSocksState := socksAuthenticate;
    TriggerSocksAuthState(socksAuthStart);
    SetLength(Buf, 128);
    I      := 0;
    Buf[I] := $01; {06/03/99}           { Socks version }
    Inc(I);
    Buf[I] := Length(FSocksUsercode);
    for J := 1 to Length(FSocksUsercode) do begin
        Inc(I);
        Buf[I] := Ord(FSocksUsercode[J]);
    end;
    Inc(I);
    Buf[I] := Length(FSocksPassword);
    for J := 1 to Length(FSocksPassword) do begin
        Inc(I);
        Buf[I] := Ord(FSocksPassword[J]);
    end;
    Inc(I);
    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I) + '''');}
//MessageBox(0, BufToStr(Buf, I), 'SocksDoAuthenticate', MB_OK);
        Send(Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.SocksDoConnect;
type
    pu_long = ^u_long;
var
    Buf     : TBytes;
    I, J    : Integer;
    ErrCode : Integer;
    IP      : u_long;
begin
    FSocksState := socksConnect;
    if FSocksLevel[1] = '4' then begin
        SetLength(Buf, 128);
        Buf[0] := 4;                                 { Version number  }
        Buf[1] := 1;                                 { Connect command }
        { Todo: Check the byte order ! }
        Buf[2] := (FPortNum shr 8) and 255;          { Port high byte  }
        Buf[3] := FPortNum and 255;                  { Port low byte   }
        if FSocksLevel = '4A' then begin
            { Conventional IP saying we can't convert the destination   }
            { host's domain name to find its IP address                 }
            { The destination must then follow the user ID              }
            Buf[4] := 0;
            Buf[5] := 0;
            Buf[6] := 0;
            Buf[7] := 1;
        end
        else begin
            { With original SOCKS4, we have to supply the dest address  }
            try
                IP     := WSocketResolveHost(FAddrStr).s_addr;
                { Todo: Check the byte order ! }
                Buf[4] := (IP shr 24) and 255;
                Buf[5] := (IP shr 16) and 255;
                Buf[6] := (IP shr  8) and 255;
                Buf[7] := IP and 255;
            except
                on E:Exception do begin
                     ErrCode := socksHostResolutionFailed;
                     TriggerSocksError(ErrCode, E.ClassName + ' ' + E.Message);
                     InternalClose(TRUE, ErrCode);
                     Exit;
                end;
            end;
        end;
        I := 8;
        if Length(FSocksUsercode) > 0 then begin
            { I'm not sure it has to be like that ! Should I also use the }
            { password or not ?                                           }
            for J := 1 to Length(FSocksUsercode) do begin
                Buf[I] := Ord(FSocksUsercode[J]);
                Inc(I);
            end;
        end;
        Buf[I] := 0;
        Inc(I);
        if FSocksLevel = '4A' then begin
            { We have to supply the destination host name                 }
            for J := 1 to Length(FAddrStr) do begin
                Buf[I] := Ord(FaddrStr[J]);
                Inc(I);
            end;
            Buf[I] := 0;   { Alon Gingold }
            Inc(I);        { Alon Gingold }
        end;
        { Buf[I] := #0;      Alon Gingold }
        { Inc(I);            Alon Gingold }
    end
    else begin
        SetLength(Buf, 128);
        Buf[0] := $05;            { Socks version }
        Buf[1] := $01;            { Connect command }
        Buf[2] := $00;            { Reserved, must be $00 }
        Buf[3] := $03;            { Address type is domain name }
        Buf[4] := Length(FAddrStr);
        { Should check buffer overflow }
        I := 5;
        for J := 1 to Length(FAddrStr) do begin
            Buf[I] := Ord(FaddrStr[J]);
            Inc(I);
        end;
        { Todo: Check the byte order ! }
        Buf[I] := (FPortNum shr 8) and 255;          { Port high byte  }
        Inc(I);
        Buf[I] := FPortNum and 255;                  { Port low byte   }
        Inc(I);
    end;

    try
{TriggerDisplay('Send = ''' + BufToStr(Buf, I + 2) + '''');}
//MessageBox(0, BufToStr(Buf, I), 'SocksDoConnect', MB_OK);
        Send(Buf, I);
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSocksWSocket.DataAvailableError(
    ErrCode : Integer;
    Msg     : String);
begin
{   TriggerSocksError(ErrCode, Msg); }
{   inherited TriggerSessionConnectedSpecial(ErrCode); }
{   InternalClose(TRUE, ErrCode); }
    TriggerSocksError(ErrCode, Msg);
    FSocksState := socksData;
    {**ALON** Added, so TriggerSessionConnectedSpecial will only call inherited}
    {inherited} TriggerSessionConnectedSpecial(ErrCode);
    {**ALON** removed 'inherited' now calls top level}
    InternalClose(TRUE, ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
var
    Len     : Integer;
    I, J    : Integer;
    ErrMsg  : String;
    InAddr  : TInAddr;
    AnsLen  : Integer;
    Buf     : TBytes;
begin
    if FSocksState = socksData then begin
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    if ErrCode <> 0 then begin
        DataAvailableError(ErrCode, 'data receive error');
        Result := FALSE;
        Exit;
    end;

    if Length(FRcvBuf) <> 128 then
        SetLength(FRcvBuf, 128);
    if Length(Buf) <> 128 then
        SetLength(Buf, 128);

    if FSocksState = socksNegociateMethods then begin
        Result := TRUE;
        Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        for I := 0 to Len - 1 do begin
            FRcvBuf[FRcvCnt] := Buf[I];
            Inc(FRcvCnt);
        end;
{TriggerDisplay('socksNegociateMethods FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We should never comes here }
            DataAvailableError(socksProtocolError, 'TWSocket logic error');
            Exit;
        end
        else begin  { SOCKS5 }
            { We are waiting only two bytes }
            if FRcvCnt < 2 then
                Exit;
{            if FRcvCnt <> 2 then begin  06/03/99}
{                DataAvailableError(socksProtocolError, 'too much data availaible');}
{                Exit;                                                              }
{            end;                                                                   }
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> $05 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] = $00 then begin
                { No authentication required }
                if FSocksAuthNumber <> #$00 then
                    { We asked for authentification, so complains... }
                    TriggerSocksAuthState(socksAuthNotRequired);
            end
            else if FRcvBuf[1] = $02 then begin
                { Usercode/Password authentication required }
                SocksDoAuthenticate;
                Exit;
            end
            else begin
                DataAvailableError(socksAuthMethodError, 'authentification method not acceptable');
                Exit;
            end;
            SocksDoConnect;
        end;
    end
    else if FSocksState = socksConnect then begin
        Result := TRUE;
{TriggerDisplay('socksConnect FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FSocksLevel[1] = '4' then begin
            { We wants at most 8 characters }
            Len := Receive(Buf, 8 - FRcvCnt);
            if Len < 0 then
                Exit;
            for I := 0 to Len - 1 do begin
                FRcvBuf[FRcvCnt] := Buf[I];
                Inc(FRcvCnt);
            end;
            { We are waiting for 8 bytes }
            if FRcvCnt < 8 then
                Exit;
            FRcvCnt := 0; { Clear receive counter }
            if FRcvBuf[0] <> 0 then begin
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
            if FRcvBuf[1] <> 90 then begin  { david.brock }
                case FRcvBuf[1] of
                91: ErrCode := socksRejectedOrFailed;
                92: ErrCode := socksConnectionRefused;
                93: ErrCode := socksAuthenticationFailed;
                else
                   ErrCode := socksUnassignedError;
                end;
                case ErrCode of
                socksRejectedOrFailed :
                    ErrMsg := 'request rejected or failed';
                socksConnectionRefused :
                    ErrMsg := 'connection refused';
                socksAuthenticationFailed :
                    ErrMsg := 'authentification failed';
                else
                    ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                end;
                DataAvailableError(ErrCode, ErrMsg);
                Exit;
            end;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{           Result := inherited TriggerDataAvailable(0); }
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end
        else begin { SOCKS5 }
            Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
            if Len < 0 then
                Exit;
            for I := 0 to Len - 1 do begin
                FRcvBuf[FRcvCnt] := Buf[I];
                Inc(FRcvCnt);
            end;
            if FRcvCnt >= 1 then begin
                { First byte is version, we expect version 5 }
                if FRcvBuf[0] <> $05 then begin
                    DataAvailableError(socksVersionError, 'socks version error');
                    Exit;
                end;
            end;
            if FRcvCnt >= 2 then begin
                if FRcvBuf[1] <> $00 then begin
                    case FRcvBuf[1] of
                    1: ErrCode := socksGeneralFailure;
                    2: ErrCode := socksConnectionNotAllowed;
                    3: ErrCode := socksNetworkUnreachable;
                    4: ErrCode := socksHostUnreachable;
                    5: ErrCode := socksConnectionRefused;
                    6: ErrCode := socksTtlExpired;
                    7: ErrCode := socksUnknownCommand;
                    8: ErrCode := socksUnknownAddressType;
                    else
                       ErrCode := socksUnassignedError;
                    end;
                    case ErrCode of
                    socksGeneralFailure :
                        ErrMsg := 'general SOCKS server failure';
                    socksConnectionNotAllowed :
                        ErrMsg := 'connection not allowed by ruleset';
                    socksNetworkUnreachable :
                        ErrMsg := 'network unreachable';
                    socksHostUnreachable :
                        ErrMsg := 'host unreachable';
                    socksConnectionRefused :
                        ErrMsg := 'connection refused';
                    socksTtlExpired :
                        ErrMsg := 'time to live expired';
                    socksUnknownCommand :
                        ErrMsg := 'command not supported';
                    socksUnknownAddressType :
                        ErrMsg := 'address type not supported';
                    else
                        ErrMsg := 'unassigned error #' + IntToStr(Ord(FRcvBuf[1]));
                    end;
                    DataAvailableError(ErrCode, ErrMsg);
                    Exit;
                end;
            end;
            if FRcvCnt < 5 then
                Exit;

            { We have enough data to learn the answer length }
            if FRcvBuf[3] = $01 then
                AnsLen := 10                     { IP V4 address }
            else if FRcvBuf[3] = $03 then
                AnsLen := 7 + Ord(FRcvBuf[4])    { Domain name   }
            else
                AnsLen := 5;                     { Other unsupported }

            if FRcvCnt < AnsLen then
                Exit;

            if FRcvBuf[3] = $01 then begin
                { IP V4 address }
                { Todo: Check byte order }
                InAddr.S_addr := FRcvBuf[4] or
                                 (FRcvBuf[5] shl 8) or
                                 (FRcvBuf[6] shl 16) or
                                 (FRcvBuf[7] shl 24);
                FBoundAddr := WSocket_inet_ntoa(InAddr);
                I := 4 + 4;
            end
            else if FRcvBuf[3] = $03 then begin
                { Domain name }
                I := 5;
                SetLength(FBoundAddr, Ord(FRcvBuf[4]));
                for J := 1 to Ord(FRcvBuf[4]) do begin
                    FBoundAddr[J] := Char(FRcvBuf[I]);
                    Inc(I);
                end;
            end
            else begin
                { Unsupported address type }
                DataAvailableError(socksUnknownAddressType, 'address type not supported');
                Exit;
            end;

            FBoundPort  := Format('%d', [FRcvBuf[I] or (FRcvBuf[I + 1] shl 8)]);
            I           := I + 2;
            FSocksState := socksData;
{           inherited TriggerSessionConnectedSpecial(0); }
{ if IsConsole then WriteLn('SOCKS5 NEGOCIATION OK');}
            {inherited} TriggerSessionConnectedSpecial(0);
            {**ALON** removed 'inherited' now calls top level}
            FSocksRcvdCnt := FRcvCnt - I;
            if FSocksRcvdCnt < 0 then
                FSocksRcvdCnt := 0
            else
                FSocksRcvdPtr := I;
{           Result := inherited TriggerDataAvailable(0);}
            Result := {inherited} TriggerDataAvailable(0);
            {**ALON** removed 'inherited' now calls top level}
        end;
    end
    else if FSocksState = socksAuthenticate then begin
        Result := TRUE;
        Len := Receive(Buf, Length(Buf) - FRcvCnt - 1);
        if Len < 0 then
            Exit;
        for I := 0 to Len - 1 do begin
            FRcvBuf[FRcvCnt] := Buf[I];
            Inc(FRcvCnt);
        end;
{TriggerDisplay('socksAuthenticate FrcvBuf = ''' + BufToStr(FRcvBuf, FRcvCnt) + '''');}
        if FRcvCnt >= 1 then begin
            { First byte is version, we expect version 5 }
            if FRcvBuf[0] <> $01 then begin { 06/03/99 }
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksVersionError, 'socks version error');
                Exit;
            end;
        end;
        if FRcvCnt = 2 then begin
            { Second byte is status }
            if FRcvBuf[1] <> $00 then begin
{                TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
                DataAvailableError(socksAuthenticationFailed, 'socks authentication failed');
                Exit;
            end;
        end
        else if FRcvCnt > 2 then begin
{            TriggerSocksAuthState(socksAuthFailure); Burlakov 12/11/99 }
            DataAvailableError(socksProtocolError, 'too much data availaible');
            Exit;
        end;
        FRcvCnt := 0; { 06/03/99 }
        TriggerSocksAuthState(socksAuthSuccess);
        SocksDoConnect;
    end
    else begin
        { We should never comes here ! }
        DataAvailableError(socksInternalError, 'internal error');
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.GetRcvdCount : LongInt;
begin
    if FSocksRcvdCnt <= 0 then
        Result := inherited GetRcvdCount
    else
        Result := FSocksRcvdCnt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSocksWSocket.DoRecv(
    out Buffer : TBytes;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
var
    I : Integer;
begin
    if FSocksRcvdCnt <= 0 then begin
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;
    { We already have received data into our internal buffer }
    if FSocksRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        for I := 0 to FSocksRcvdCnt - 1 do
            Buffer[I] := FRcvBuf[FSocksRcvdPtr + I];
        Result        := FSocksRcvdCnt;
        FSocksRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    for I := 0 to BufferSize - 1 do
        Buffer[I] := FRcvBuf[FSocksRcvdPtr + I];
    Result        := BufferSize;
    FSocksRcvdPtr := FSocksRcvdPtr + BufferSize;
    FSocksRcvdCnt := FSocksRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

              X          X     X       X      X X X X
              X          X     X X     X      X
              X          X     X   X   X      X
              X          X     X     X X      X X X
              X          X     X       X      X
              X          X     X       X      X
              X X X X    X     X       X      X X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TCustomLineWSocket.Create(
    AOwner: {$IFDEF ICS_COMPONENT}TComponent
            {$ELSE}               TObject{$ENDIF});
begin
writeln('TCustomLineWSocket.Create');
    inherited Create(AOwner);
    FLineEnd   := #13#10;
    FLineMode  := FALSE;
    FLineLimit := 65536;  { Arbitrary line limit }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TCustomLineWSocket.Destroy;
begin
    if Length(FRcvdPtr) <> 0 then begin
        SetLength(FRcvdPtr, 0);
        FRcvBufSize := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WndProc(var MsgRec: TMessage);
begin
    with MsgRec do begin
        if Msg = WM_TRIGGER_DATA_AVAILABLE then begin
            { We *MUST* handle all exception to avoid application shutdown }
            try
                WMTriggerDataAvailable(MsgRec)
            except
                on E:Exception do
                    HandleBackGroundException(E);
            end;
        end
        else
            inherited WndProc(MsgRec);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.WMTriggerDataAvailable(var msg: TMessage);
begin
    while FRcvdCnt > 0 do
        TriggerDataAvailable(0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.SetLineMode(newValue : Boolean);
begin
    if FLineMode = newValue then
        Exit;
    FLineMode := newValue;
    if (FRcvdCnt > 0) or (FLineLength > 0) then
        PostMessage(Handle, WM_TRIGGER_DATA_AVAILABLE, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.GetRcvdCount : LongInt;
begin
    if not FLineMode then
        Result := inherited GetRcvdCount
    else
        Result := FLineLength;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.DoRecv(
    out Buffer : TBytes;
    BufferSize : Integer;
    Flags      : Integer) : Integer;
var
    I : Integer;
begin
    if FLineMode and (FLineLength > 0) then begin
        { We are in line mode an a line is received }
        if FLineLength <= BufferSize then begin
            { User buffer is greater than received data, copy all and clear }
            for I := 0 to FLineLength - 1 do
                Buffer[I] := FRcvdPtr[I];
            Result      := FLineLength;
            FLineLength := 0;
            Exit;
        end;
        { User buffer is smaller, copy as much as possible }
        for I := 0 to BufferSize - 1 do
            Buffer[I] := FRcvdPtr[I];
        Result   := BufferSize;
        { Move the end of line to beginning of buffer to be read the next time }
        for I := 0 to FLineLength - BufferSize - 1 do
            FRcvdPtr[I] := FRcvdPtr[BufferSize + I];
        FLineLength := FLineLength - BufferSize;
        Exit;
    end;

    if FLineMode or (FRcvdCnt <= 0) then begin
        { There is nothing in our internal buffer }
        Result := inherited DoRecv(Buffer, BufferSize, Flags);
        Exit;
    end;

    { We already have received data into our internal buffer }
    if FRcvdCnt <= BufferSize then begin
        { User buffer is greater than received data, copy all and clear }
        for I := 0 to FRcvdCnt - 1 do
            Buffer[I] := FRcvdPtr[I];
        Result   := FRcvdCnt;
        FRcvdCnt := 0;
        Exit;
    end;
    { User buffer is smaller, copy as much as possible }
    for I := 0 to BufferSize - 1 do
        Buffer[I] := FRcvdPtr[I];
    Result   := BufferSize;
    { Then move remaining data to front of buffer  16/10/99 }
    for I := 0 to FRcvdCnt - BufferSize do
        FRcvdPtr[I] := FRcvdPtr[BufferSize + I];
    FRcvdCnt := FRcvdCnt - BufferSize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Edit received data. Handle TAB and BACKSPACE characters.                  }
{ A data packet has been received into FRcvPtr buffer, starting from        }
{ FRcvdCnt offset. Packet size if passed as the Len argument.               }
procedure TCustomLineWSocket.EditLine(var Len : Integer);
var
    Buf     : TBytes;
    BufSize : LongInt;
    I       : LongInt;
    J       : LongInt;
    K       : Integer;
    Edited  : Boolean;
    NewCnt  : LongInt;
    NewSize : LongInt;
const
    BackString : String = #8 + ' ' + #8;
begin
    BufSize := 0;
    try
        Edited := FALSE;
        I      := FRcvdCnt;
        J      := FRcvdCnt;
        NewCnt := FRcvdCnt;
        { Loop to process all received char }
        while I < (FRcvdCnt + Len) do begin
            if FRcvdPtr[I] = 8 then begin   { BACKSPACE character }
                if FLineEcho and (J > 0) then
                    SendStr(BackString);
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Compute buffer size as a multiple of 256 bytes   }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    SetLength(Buf, BufSize);
                    { Copy data already processed }
                    for K := 0 to I - 1 do
                        Buf[K] := FRcvdPtr[K];
                end;
                if J > 0 then begin
                    Dec(J);
                    if J < NewCnt then
                        NewCnt := J;
                end;
                Inc(I);
            end
            else if FRcvdPtr[I] = 9 then begin  { TAB character }
                if not Edited then begin
                    { Not edited yet, so we allocate a buffer to store }
                    { edited data and we remember we edited data.      }
                    Edited := TRUE;
                    { Compute buffer size as a multiple of 256 bytes   }
                    BufSize := ((FRcvdCnt + Len + 256) shr 8) shl 8;
                    SetLength(Buf, BufSize);
                    { Copy data already processed }
                    for K := 0 to I - 1 do
                        Buf[K] := FRcvdPtr[K];
                end;
                repeat
                    if FLineEcho then
                        SendStr(' ');
                    Buf[J] := Ord(' ');
                    Inc(J);
                until (J and 7) = 0;
                Inc(I);
            end
            else begin
                if FLineEcho then
                    Send(FRcvdPtr[I]);
                if Edited then begin
                    if J >= BufSize then begin
                        { Need to allocate more buffer space }
                        NewSize := BufSize + 256;
                        SetLength(Buf, NewSize);
                        BufSize := NewSize;
                    end;
                    Buf[J] := FRcvdPtr[I];
                end;
                Inc(I);
                Inc(J);
            end;
        end;
        if Edited then begin
            if J >= FRcvBufSize then begin
                { Current buffer is too small, allocate larger }
                NewSize := J + 1;
                SetLength(FRcvdPtr, NewSize);
                FRcvBufSize := NewSize;
            end;
            { Move edited data back to original buffer }
            for K := 0 to J - 1 do
                FRcvdPtr[K] := Buf[K];
            FRcvdPtr[J] := 0;
            FRcvdCnt    := NewCnt;
            Len         := J - FRcvdCnt;
        end;
    finally
        if BufSize > 0 then
            SetLength(Buf, BufSize);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerLineLimitExceeded(
    Cnt           : Integer;
    var ClearData : Boolean);
begin
    if Assigned(FOnLineLimitExceeded) then
        FOnLineLimitExceeded(Self, Cnt, ClearData);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomLineWSocket.TriggerDataAvailable(ErrCode : Word) : Boolean;
var
    Cnt        : Integer;
    Len        : Integer;
    NewSize    : LongInt;
    SearchFrom : LongInt;
    I, K       : LongInt;
    Found      : Boolean;
begin
{  if (not FLineMode) or (Length(FLineEnd) = 0) then begin }
    if (not FLineMode) or (Length(FLineEnd) = 0) or (FSocksState <> socksData)
    {**ALON** added check so, if data is received while still handshaking }
    { with the socks server, we ask the TCustomSocksWSocket to handle it  }
    then begin
        { We are not in line mode }
        Result := inherited TriggerDataAvailable(ErrCode);
        Exit;
    end;

    { We are in line mode. We receive data ourself }

    Result := TRUE;
    Cnt    := inherited GetRcvdCount;
    { if Cnt <= 0 then }
    {    Exit;         }
    if Cnt < 0 then
        Exit;
    if Cnt = 0 then
        Cnt := 255;

    if (FRcvdCnt + Cnt + 1) > FRcvBufSize then begin
        { Current buffer is too small, allocate larger }
        NewSize := FRcvdCnt + Cnt + 1;
        SetLength(FRcvdPtr, NewSize);
        FRcvBufSize := NewSize;
    end;

    if Length(FLocalBuf) < Cnt then
        SetLength(FLocalBuf, ((Cnt + 256) shr 8) shl 8);

    Len := Receive(FLocalBuf, Cnt);
    if Len <= 0 then
        Exit;
    for I := 0 to Len - 1 do
         FRcvdPtr[FRcvdCnt + I] := FLocalBuf[I];
    FRcvdPtr[FRcvdCnt + Len] := 0;
    if FLineEdit then
        EditLine(Len)
    else if FLineEcho then
        Send(FLocalBuf, Len);
    SearchFrom := FRcvdCnt - Length(FLineEnd);
    if SearchFrom < 0 then
        SearchFrom := 0;
    FRcvdCnt := FRcvdCnt + Len;
    while FLineMode do begin
        Found := FALSE;
        I := SearchFrom;
        while I < (FRcvdCnt - Length(FLineEnd) + 1) do begin
            if FRcvdPtr[I] = Ord(FLineEnd[1]) then begin
                Found := TRUE;
                for K := 2 to Length(FLineEnd) do begin
                    Found := (FRcvdPtr[I + K - 1] = Ord(FLineEnd[K]));
                    if not Found then
                        break;
                end;
                if Found then
                    break;    { Found the end of line marker }
            end;
            Inc(I);
        end;
        if not Found then begin
            if ((FLineLimit > 0) and (FRcvdCnt > FLineLimit)) then begin
                FLineClearData := TRUE;
                TriggerLineLimitExceeded(FRcvdCnt, FLineClearData);
                if FLineClearData then begin
                    FLineLength        := 0;
                    FRcvdCnt           := 0;
                    FLineClearData     := FALSE;
                end;
            end;
            break;
        end;
        FLineLength       := I + Length(FLineEnd);
        FLineReceivedFlag := TRUE;
        { We received a complete line. We need to signal it to application }
        { The application may not have a large buffer so we may need       }
        { several events to read the entire line. In the meanwhile, the    }
        { application may turn line mode off.                              }
        while FLineMode and (FLineLength > 0) do begin
            if not inherited TriggerDataAvailable(0) then
                { There is no handler installed }
                FLineLength := 0;
        end;
        { Move remaining data in front of buffer }
        if FLineLength > 0 then begin
            { Line mode was turned off in the middle of a line read. }
            { We preserve unread line and other received data.       }
            for K := 0 to FRcvdCnt - I - 1 do
                FRcvdPtr[FLineLength + K] := FRcvdPtr[I + K];
            FRcvdCnt := FRcvdCnt - I + FLineLength;
        end
        else begin
            for K := 0 to FRcvdCnt - I - Length(FLineEnd) - 1 do
                 FRcvdPtr[K] := FRcvdPtr[I + Length(FLineEnd) + K];
            FRcvdCnt := FRcvdCnt - I - Length(FLineEnd);
        end;
        if FRcvdCnt >= 0 then
            FRcvdPtr[FRcvdCnt] := 0;
        SearchFrom := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomLineWSocket.TriggerSessionClosed(ErrCode : Word);
begin
    FLineReceivedFlag := TRUE;
    if FRcvdPtr <> nil then begin
        if FLineMode and (FRcvdCnt > 0) and (not FLineClearData) then begin
            FLineLength       := FRcvdCnt;
            while FLineMode and (FLineLength > 0) do
                inherited TriggerDataAvailable(0);
        end;
        SetLength(FRcvdPtr, FRcvBufSize);
        FRcvBufSize := 0;
        FRcvdCnt    := 0;
    end;
    inherited TriggerSessionClosed(ErrCode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

                 X X      X     X    X       X     X X X
               X     X      X   X    X X     X   X      X
               X              X X    X   X   X   X
                 X X            X    X     X X   X
                     X          X    X       X   X
               X     X    X     X    X       X   X      X
                 X X        X X      X       X     X X X

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSyncWSocket.InternalDataAvailable(
    Sender  : TObject;
    ErrCode : Word);
var
    Len : Integer;
begin
    SetLength(FLineBuffer, FLineLength);
    Len := Receive(FLineBuffer, FLineLength);
    if Len <= 0 then
        Len := 0;
    SetLength(FLineBuffer, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSyncWSocket.WaitUntilReady(var DoneFlag : Boolean) : Integer;
begin
    Result := 0;           { Suppose success }
    FTimeStop := Integer(GetTickCount) + FTimeout;
    while TRUE do begin
        if DoneFlag then begin
            Result := 0;
            break;
        end;

        if ((FTimeout > 0) and (Integer(GetTickCount) > FTimeStop)) or
{$IFNDEF NOFORMS}
           Application.Terminated or
{$ENDIF}
           FTerminated then begin
            { Application is terminated or timeout occured }
            Result := WSA_WSOCKET_TIMEOUT;
            break;
        end;
        MessagePump;
{$IFNDEF VER80}
        { Do not use 100% CPU, but slow down transfert on high speed LAN }
        Sleep(0);
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TCustomSyncWSocket.Synchronize(
    Proc : TWSocketSyncNextProc;
    var DoneFlag : Boolean) : Integer;
begin
    DoneFlag := FALSE;
    if Assigned(Proc) then
        Proc;
    Result := WaitUntilReady(DoneFlag);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCustomSyncWSocket.ReadLine(
    Timeout    : Integer;  { seconds if positive, milli-seconds if negative }
    var Buffer : String);
var
    OldDataAvailable : TDataAvailable;
    OldLineMode      : Boolean;
    Status           : Integer;
    I                : Integer;
begin
    Buffer            := '';
    if FState <> wsConnected then begin
        RaiseException('ReadLine failed: not connected');
        Exit;
    end;

    { Positive timeout means seconds. Negative means milli-seconds          }
    { Null means 60 seconds.                                                }
    if TimeOut = 0 then
        FTimeOut      := 60000
    else if TimeOut > 0 then
        FTimeOut      := Timeout * 1000
    else
        FTimeOut      := -Timeout;

    FLineReceivedFlag := FALSE;
    { Save existing OnDataAvailable handler and install our own             }
    OldDataAvailable  := FOnDataAvailable;
    FOnDataAvailable  := InternalDataAvailable;
    { Save existing line mode and turn it on }
    OldLineMode       := FLineMode;
    FLineMode         := TRUE;
    try
        Status := Synchronize(nil, FLineReceivedFlag);
        if Status = WSA_WSOCKET_TIMEOUT then begin
             { Sender didn't send line end within allowed time. Get all     }
             { data available so far.                                       }
             if FRcvdCnt > 0 then begin
                 SetLength(Buffer, FRcvdCnt);
                 for I := 0 to FRcvdCnt - 1 do
                     Buffer[I + 1] := Char(FRcvdPtr[I]);
                 FRcvdCnt := 0;
             end;
        end
        else begin
            SetLength(Buffer, Length(FLineBuffer));
            for I := 0 to Length(FLineBuffer) - 1 do
                Buffer[I + 1] := Char(FLineBuffer[I]);
        end;
        { Should I raise an exception to tell the application that          }
        { some error occured ?                                              }
    finally
        FOnDataAvailable := OldDataAvailable;
        FLineMode        := OldLineMode;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

initialization
    GWSockCritSect := Mutex.Create(FALSE);
    IPList         := TStringList.Create;

finalization
    // Cleanup winsock.
    // Will have problems if all sockets are not destroyed yet !
//    GWSockCritSect.WaitOne;
//    try
//        if WSocketGCount > 0 then
            WSACleanup;
//    finally
//        GWSockCritSect.ReleaseMutex;
//    end;

//    GWSockCritSect.Free;
//    if Assigned(IPList) then begin
//        IPList.Free;
//        IPList := nil;
//    end;

end.
