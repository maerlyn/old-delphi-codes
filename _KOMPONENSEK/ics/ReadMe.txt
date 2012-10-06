ICS - Internet Component Suite
==============================
(Aka FPIETTE's Components)


Revised: Jan 16, 2005
http://www.overbyte.be
http://www.rtfm.be/fpiette/indexuk.htm
http://users.swing.be/francois.piette/indexuk.htm

Table of content:
-----------------

- Legal issues
- Donate
- Register
- Installation
- Sample applications
- Support
- Release notes
- Midware
- Known problems
- Special thanks


Legal issues: 
-------------
              Copyright (C) 1997-2005 by François PIETTE 
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

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


Donate
------

ICS is freeware. You can use it without paying anything except the registration
postcard (see "register" below). But of course donations are welcome. You can
send cash (Euro currency or US Dollars) in an envelop to my street address or
buy a gift certificate at amazon in the UK. I will then use it to buy books.
Here is the direct URL at Amazon UK (nearest to my home, please don't use another):
http://www.amazon.co.uk/exec/obidos/gc-email-order1/ref=g_gc_email/202-6198323-6681414
For larger amount, contact me by email.


Register
--------

ICS is freeware. If you use the components, you must register by sending a 
picture postcard showing the area you lives in and some beautiful stamps for 
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me. Write on the postcard that it is your ICS registration.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, street address, EMail and website.


Installation:
-------------

The zip file has subdirectories in it. You must use the WinZip "Use folder 
names" option to restore this directory tree or you will have problems 
because the files would not be in their proper subdirectories.

This is the subdirectory layout:

.\                            Info directory
.\delphi\internet             Delphi.W32 sample applications (all Win32 Delphi versions)
.\delphi\internet\WebServData Directory for WebServ demo data files
.\delphi\dotnet               Delphi.NET components and applications 
.\cpp\internet                C++Builder sample applications
.\cpp\internet\bcb1           C++Builder version 1 projects
.\cpp\internet\bcb3           C++Builder version 3 projects
.\cpp\internet\bcb4           C++Builder version 4 projects
.\cpp\internet\bcb5           C++Builder version 5 projects
.\cpp\internet\bcb6           C++Builder version 6 projects
.\delphi\vc                   Delphi 1.x files (winsock.pas 16 bits and *.dcr)
.\delphi\vc32                 Delphi (1/2/3/4/5/6/7/2K5) and C++Builder (1/3/4/5/6) components
.\Delphi1                     Automated build for Delphi 1. Not for beginners.
.\Delphi2                     Automated build for Delphi 2. Not for beginners.
.\Delphi3                     Automated build for Delphi 3. Not for beginners.
.\bcb1                        Automated build for Bcb 1. Not for beginners.

If you are reinstalling, or installing a new version, be sure to delete all dcu,
and obj files from vc32 and internet directories. This apply to all Win32 versions.
For Delphi.NET, remove all dcpil files.

DELPHI 2005/WIN32: 
Directory VC32 contains IcsDel90.bdsproj which is a package source for
all components. Using Delphi, do a file/open project (Ctrl-F11, browse to the 
VC32 directory. Select IcsDel90.bdsproj and open it. Then in the project
manager view, right-click on IcsDel90.bpl, then click on Install button.
After a few seconds, you should have a dialog box telling you the package 
IcsDel90.bpl has been installed with a bunch of new components registered.
Then do a "Save All" (Shift-Ctrl-S) and the a "Close All".
Having installed the package, verify that the VC32 directory has been added to
the Win32 Library Path (Tools / Options / Delphi Options / Library - Win32 / 
Library Path). If not, add it manually.

Once the package is installed, you may open the sample projects. There is a
project group called Del90Sam.bdsgroup which has all sample programs. Open it
with file/open project (Ctrl-F11), browse to the Internet directory, select 
and open Del90Sam.bdsgroup. You will get some dialog box telling you that
resource files are missing (they have not been included in the zip file to 
save space) and are recreated by Delphi. It is OK. Any other error message 
is a problem you should fix. After all resource files have been recreated,
you should see in the project manager a group of projects called Del90Sam.
In this group, you'll find all sample programs.

To compile all samples at once, do Project / Build all projects. This will 
take some time to compile all sample programs.

DELPHI2005/.NET
Directory Delphi\DotNet contains IcsDotNet.bdsgroup with all projects.
Open it and build all projects.

DELPHI 7: Directory VC32 contains IcsDel70.dpk which is a package source for
all components. Using Delphi, do a file/open, select *.dpk and browse to
the VC32 directory. Select IcsDel70.dpk and open it. Then click on the 
Install button. You should see the FPiette tab on the component gallery.
Add VC32 directory path to your library path (Tools menu / Environment Options
/ Library / Library Path. Add VC32 path at the end of the existing path).

Once the package is installed, you may open the sample projects. There is a
project group called Del70Sam.bpg which has all sample programs. Open it
with file/open, browse to the Internet directory, select and open Del70Sam.bpg.
Then Project/Build all projects. You'll get all sample programs compiled.
It is likely that for each project, Delphi complains about a missing .res
file. This is not a problem, Delphi will recreate it as needed. They have not
been included to save space in the zip file.

Note 1: Delphi may run out of memory if you ask to compile all projects at 
once. If you have not enough RAM, then compile each project individually.

Note 2: Delphi 7 has new warnings which triggers a lot of messages for 100% OK
code. You can turn those warnings off in the project/ options / Compiler messages
and deselecting: "Deprecated symbol", "Platform symbol", "usafe type", "unsafe code", 
"usafe typecast". Those are intended for .NET and Linux portability. You can
safely ignore them if you run windows. For you facility, I included a utility 
SetProjectOptions (source code, you must compile it) in the internet directory.
This utility will update project options to disable the warnings.

DELPHI 6: Same installation as Delphi 7. Files are IcsDel60.dpk and Del60Sam.bpg.
(Note 2 doesn't apply to Delphi 6).

DELPHI 5: Same installation as Delphi 6. Files are IcsDel50.dpk and Del50Sam.bpg.

DELPHI 4: Same installation as Delphi 5. Files are IcsDel40.dpk and Del40Sam.bpg.

DELPHI 3: Same installation as Delphi 5. Files are IcsDel30.dpk. Del30Sam.bpg 
doesn't exists because project group was introduced with Delphi 4. Open each
project individually (*.dpr).

DELPHI 2: Directory VC32 contains all components. You must add all those
components into your library. But warning: not all files are components.
Only those with a Register procedure are components. Other are just support
files and must not be installed as components. To install a component file,
do Component/Install, then click on the Add button, then click on the Browse
button and browse to the VC32 directory. Select a component file such as
wsocket.pas and click on OK. Do it again for all component files and
finally click OK button on the Install Component dialog window. Your 
library will be rebuilt. You should see the tab FPiette added to your
component gallery. If you have an error saying "register procedure doesn't
exists", then you included a file that must not be used to install component.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.


DELPHI 1: The install procedure is the same as for Delphi 2. But you must
add the directory VC into the library path _before_ VC32 directory which
will be added automatically when you add the first component source.
VC directory in the library path is needed because Delphi 1 doesn't have 
built in winsock support. ICS contains winsock.pas in VC directory to 
add winsock support. VC directory also contains 16 bits component resource 
files (*.dcr) needed by Delphi 1. VC32 contains the 32 bits resource files.
If you use only Delphi 1, you can safely copy all files from VC to VC32
directory, but not the contrary.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

BCB6: First you need to install all components. There is a package in
Delphi\VC32 directory, called IcsBcb60.bpk. You do File/Open project and
browse to IcsBcb60.bpk. Select and open it, then Project/Build IcsBcb60.
Once the package is compiled, you can install it into your component
palette: Component/Install Packages, click the Add button and select 
Delphi\VC32\IcsBcb60.bpl (you just generated this file by compiling the 
package). Click OK button. You must now see FPiette Tab on the component 
gallery.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB6. There is also a project group
Bcb60Sam.bpg which contains all sample projects.
It is likely that for each project, Bcb complains about a missing .res
file. This is not a problem, Bcb will recreate it as needed. They have not
been included to save space in the zip file.

BCB5: follow BCB6 instruction below. Package is IcsBcb50.bpk and samples
project group is Bcb50Sam.bpg. Directory is ...\internet\bcb5.

BCB4: follow BCB6 instruction below. Package is IcsBcb40.bpk and samples
project group is Bcb40Sam.bpg. Directory is ...\internet\bcb4.

BCB 3: follow BCB6 instruction below. Package is IcsBcb30.bpk and samples
project group is Bcb30Sam.bpg. Directory is ...\internet\bcb3.

BCB1: First you need to install all components that are located in
Delphi/VC32. Components are all Object Pascal sources. You must select
*.pas when in the open dialog box (Menu/Component/Install/Add/Browse).
Components are files with a register procedure. Do not install files
with no register procedure (if you try, you'll get an error message).

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB1.


NOTES: 
- You may have an error message, using Delphi or BCB, complaining about 
Font.Charset, OldCreateOrder and other properties. Those are new properties 
in newer Delphi or BCB versions. 
You can safely ignore those errors because those properties are not 
used by the components nor sample programs. You may encounter this 
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error 
at design time, you'll have it at runtime !

- If you have Delphi or BCB complaining about a file not found, add VC32
directory to your library path.

- If you are using BCB3, 4, 5 or 6, you may encounter an error at link time 
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in BCB. To solve it, close BCB and open the package or project file with 
notepad or any text editor and remove any reference to the missing libraries. 
Then start BCB again, reopen the project and rebuilt it. With BCB5/6 you can
edit project option file (right click in project manager).

- Don't forget that the C++Builder components are located in .\delphi\vc32 
which is object pascal source code (not a problem for C++Builder, just 
indicate that the *.pas files are displayed when installing). C++Builder 
will create the *.hpp files. There are some on-line help files in the VC32
directory.

- Using BCB5 or later, you may have a problem with DsgnIntf.dcu not found.
In their readme.txt file, Borland give this info:
The compiled version of the Design Interface unit, DsgnIntf.dcu, is no 
longer included with C++Builder (or Delphi). To compile design-time 
packages using this unit, you must include the PFLAGS option -LUvcl50 
in the project options for the package's project. From the main IDE 
menu select Project|Edit Option Source to edit the project options. 
In the entry for PFLAGS add the -LUvcl50 option. For example:
<PFLAGS value="-$YD -$W -$O- -v -JPHNE -M -LUvcl50"/>
If you use BCB6, use "60" instead of "50" in the above explanations.
Runtime redistribution of DsgnIntf.dcu was in contradiction of the 
license agreement and so this file is no longer included with C++Builder

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> WSocket.pas      Winsock component - TCP, UDP, DNS,...
> WSocketS.pas     Winsock component for building servers
> WSocketE.pas     Register procedure andproperty editor for TWSocket
> HttpProt.pas     HTTP client protocol - used by the web
> HttpSrv.pas      HTTP server protocol - used to build webservers
> FtpCli.pas       FTP client protocol - file transfert
> FtpSrv.pas       FTP server protocol - file transfert
> Ping.pas         ICMP echo protocol - ping a host
> Pop3Prot.pas     POP3 client protocol - get mail from mail server
> MimeDec.pas      MIME component - decode file attach, use with POP3
> SmtpProt.pas     SMTP client protocol - send mail to server
> NntpCli.pas      NNTP client protocol - send and receive newsgroups messages
> TnCnx.pas        TELNET client protocol - terminal emulation protocol
> TnScript.pas     TELNET client protocol - with automation
> EmulVT.pas       ANSI terminal emulation in a control
> TnEmulVT.pas     TELNET and ANSI terminal emulation combined
> FingCli.pas      FINGER client protocol - Find informations about user
> Wait.pas         A kind of progress bar - now obsolete, forget it for new cod
> DnsQuery         DNS lookup component - useful for getting MX records

As a rule, the components are the files which have a Register procedure.

- The following list support and utilities units:
> IcsDES.pas       Implementation of the Data Encryption Standard (DES)
> IcsMD4.pas       Implementation of the MD4 Message-Digest Algorithm 
> IcsMD5.pas       Implementation of the MD5 Message-Digest Algorithm
> IcsSHA1.pas      Implementation of US Secure Hash Algorithm 1 (SHA1)
> IcsURL.pas       Support routines for URL handling
> MimeUtil.pas     Support routines for MIME standard
> Icmp.pas         ICMP protocol support, used by the PING component


Sample applications:
--------------------

BASFTP          Basic FTP client program
CLIDEMO         Example of client for SRVDEMO
SERVER5         Basic server GUI applications
CLIENT5         Basic client GUI applications
DYNCLI          Basic client creatin TWSocket dynamically
CONCLI1         Basic client/server console applications
CONCLI2         Basic client/server console applications with thread
CONFTP          Basic console mode FTP client
CONHTTP         Basic console mode HTTP client
CONPING         Basic console mode demo of ping component
CONPOP3         Basic console mode demo for POP3 (mail receive)
CONSMTP         Basic console mode demo for SMTP (mail send)
CONSRV1         Basic server application in console mode
DLLTST1         Test program using ICSDLL
DNSLOOK         Example of name resolution, see also NSLOOKUP sample
DYNCLI          Demo of dynamically created TWSocket components
FINGER          Example of TFingerCli component
FTPASY          Example of asychronous FTP client
FTPSERV         General purpose FTP server
FTPTHRD         Demo of multithreaded FTP client, see also FTPASY
FTPTST          Basic graphical FTP client
HTTPASP         Example of THttpCli component with cookie (POST to an ASP page)
HTTPASY         Example of THttpCli component with multiple async requests (GET)
HTTPCHK         Example of THttpCli to check for valid URL
HTTPDMO         Yet another HTTP client demo
HTTPGET         Example of THttpCli component (GET into a file)
HTTPPG          Example of THttpCli component (POST)
HTTPPOST	Example of THttpCli component (POST), work with WebServ sample
HTTPTHRD        Example of THttpCli component (multi-threaded GET)
HTTPTST	        Example of THttpCli component (GET), show many features
ICSDLL1         Example of TWSocket component within a DLL, use with TcpSrv demo
ICSDLL2         Example of HTTP client component within a DLL
ICSISAPI        Example of FTP client component within an ISAPI extension
MAILHTML        Example of HTML formatted EMail sending, including embedded images
MAILRCV         Internet EMail access using POP3 protocol
MAILSND         Example of EMail sending, including file attach
MAILSNDASYNC    Example of simultaneous EMail sending
MD5FILE         Example of MD5 unit: computer MD5 checksum for files
MD5TEST         Test program for MD5 unit
MIMEDEMO        Example of EMail decoding (attached files are extracted)
MIMETST         Example of EMail sending with attached files
MTSRV           Basic server, multi-threaded, see THRDSRV for better code
NEWSHTML        Example of NNTP component to send HTML messages
NEWSRDR         Example of TNntpCli component (Send/receive newsgroups)
NSLOOKUP        Demo for the DNS query component
PINGTST         Demo for the ping component
POP3MIME        Example of MIME decoding, for example of EMails received with POP3 component
RECV            Simple file receive (server), use with SENDER demo (client)
SENDER          Simple file send (client), use with RECV demo (server)
SHATEST         Test program for SHA unit
SOCKSTST        How to use TWSocket with SOCKS protocol (firewall traversing)
SRVDEMO         Example of server using a TTable
SRVTCP          Basic server without client forms, event-driven
SVCTCP          Same as SRVTCP but as an NT/2K/XP service
TCPSRV          Basic server without client forms, event-driven
THRDSRV         Basic multithreaded TCP server
TNCLIENT        Telnet client using a TnEmulVT
TNDEMO          Telnet client using a TMemo
TNSRV           Basic TCP server with client forms, event-driven
TWSCHAT         Chat program (both client and server in a single program)
UDPLSTN         UDP listen demo
UDPSEND         UDP send demo
WEBSERV         HTTP server demo. Show static and dynamic pages. Use template for HTML.

Note 1: Many samples are similar. When searching for something, always look at the date
        the demos where created. The most recent is always the best code !
Note 2: Not all sample have been rewritten in C++ for BCB. And those rewritten are
        frequently much simpler. So BCB user: have a look at the Delphi sample too !
Note 3: Follow "UserMade" link on ICS website to find more sample programs written by
        ICS users.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I
loaded the form, I was using another Delphi or BCB version which has new properties. 
You can safely ignore messages related to those new properties. They are not used 
in the samples. (The properties are CharSet, OldCreateOrder and others). 
You can also encounter error about duplicate resources. You can ignore them 
safely. If you have those errors, open each form in the IDE, ignore the error 
then recompile. If you don't open the form in the IDE, you'll get the errors 
at runtime and your program will abort.

If you use C++Builder 1, you may encounter problems because Borland has
two winsock include files: one .h and one .hpp. TWSocket needs the 
winsock.hpp file and it is automatically included. However, sometimes
the winsock.h file is included indirectly and you will get numerous error 
messages from the compiler. Just add a #define _WINSOCKAPI_ before 
your existing #include directives. This will prevent winsock.h from being
included. This is no a bug, it is a feature :-) Another annoying feature
is the SetPortA syndrome. Add a #define _WINSPOOL_ before any #include
directive to work around. If you have some file not found error at
compile time, be sure to verify the project options to change any directory
following your own configuration (include files and library path).
Do NOT use those #include if you use C++Builder 3: Borland has corrected
their mistake.

When installing a new version, always delete old dcu, obj, dcpil and always 
recompile everything ! 
Close everything before recompiling the library or packages.
When installing a new version, be sure to unzip it in the same directory
tree as the old one or you'll mess both versions.

The provided delphi\vc\winsock.pas is for use with Delphi 1 only.
Use Borland's file with all other Delphi versions and C++Builder.

C++Builder 1/3/4/5/6 projects are not compatibles. BCB can provide conversions.
I made separate projects in CPP\INTERNET\BCB1, CPP\INTERNET\BCB3,
CPP\INTERNET\BCB4, CPP\INTERNET\BCB5 and CPP\INTERNET\BCB6 directories. 
If you install this ICS version above a very old one, be sure to delete 
every *.mak and corresponding *.cpp files form the internet directory 
(not all *.cpp files, only those which has a corresponding *.mak file !).

It is possible to use several Delphi or BCB versions at the same time, but 
before switching from one to the other, you MUST delete all DCU, OBJ, ILS, 
ILF, TDS, ILC and ILD files.For BCB, you sometimes need to delete HPP files
as well. They will be recreated when you reinstall ICS components.


Support:
--------
There is a mailing list to discuss F. Piette's components and applications.
To subscribe surf to http://elists.org/mailman/listinfo/twsocket.
Do not use an aliased EMail address, use your real EMail address, the one 
you'll use to post messages. After asking for subscription, you'll receive a
confirmation email you must reply to it or you will _not_ be added to the
subscriber's list (this is to check for email path and also make sure 
someone doesn't subscribe you without your consent).

Once you have been registered with the mailing list processor, you can 
send messages to twsocket@elists.org. Every subscriber will receive a copy of 
your message. I will respond, but anybody is welcome to respond to each 
other's messages. So every body can share his expertize. There are many other
useful mailing lists at http://www.elists.org !

Before asking a question, browse the message archive you can download from
the support page on the website (click the "support" button from main page)
and from the mailing list website http://elists.org/mailman/listinfo/twsocket.
Google is also archiving the list with some delay.

If you found a bug, please make a short program which reproduce the problem 
attach it to a message addressed to me. If I can reproduce the problem, I 
can find a fix ! Do not send exe file but just source code and instructions.
Always use the latest version (beta if any) before reporting any bug.

You are also encouraged to use the support mailing list to ask for 
enhancements. You are welcome to post your own code.

The support mailing list has an heavy traffic: 20 to 40 messages each day. If
it is too much for you, you can select "digest" mode in which mailing list
processor will mail you only one big message per day. To select digest mode
goto http://elists.org/mailman/listinfo/twsocket.
 
You can also subscribe to another mailing list called twsocket-announce which 
will receive only very few messages when major bug fixes or updates are done. 
The subscription process is the same as for the other mailing list. 
See above procedure.


Release notes
-------------

There is no global release notes. Each component and sample has his own history.
You can find those histories in the comment in the beginning of each source file.
There are also a bunch of useful comments in the source code. You should at least
browse the source for the components you are interested in.


MidWare
-------
If you wants to build client/server applications using TCP/IP protocol, you 
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site http://www.overbyte.be.


Known problems:
--------------

Old Delphi and C++Builder have limitation on path length. If you installed 
many components in many differents subdirectories, you'll get strange errors. 
Just make the path shorter and use less different directories. You cannot have
a path greater than 255 characters (this problem is solved starting from 
Delphi 3.02 or later).

Using Delphi 3 Pro, there could a problem after inserting all the component
into your user package. Delphi 3 gives an access violation error in DCC.DLL.
This does not occur if you use Delphi C/S (That's what I do). Ben Ark
(BenArk@email.msn.com) has found the following procedure to overcome 
this [Delphi] bug:

---- copy of a message posted in twsocket@rtfm.be mailing list ----

Just thought I'd correct that previous message with some more detailed
steps...

1)	Remove the EmulVT and TnEmulVT units from your User Package.
2)	Create a new project/application.
3)	Put all of these new files in a directory seperate from your components.
4)	MOVE, do not copy, your Emulvt.pas source file into this new directory.
5)	You need to add Emulvt.pas to the new project.
6)	Build the project.
7)	This should generate an Emultvt.dcu in the directory where the project
        is.
8)	Copy ONLY the DCU file into your component directory
9)	Add EmulVT.dcu to your User Package.
10)	Make sure the EmulVT.pas file IS NOT on your search path.
11)	Now you can safely readd TnEmulVT.pas to your User Package and build
        it.

If you have any problems with these steps, let me know.

-Ben

---- End of message ----

Thank you Ben !

Eric Daniels <sparky@netgsi.com> also found the same problem with Delphi 3
standard. Here what's he tell me in a message:

---- copy of a message posted in twsocket@rtfm.be mailing list ----

This error does happen under Delphi 3 Standard which I use...  I traced it
down and found out to trigger this error all one needs to do is goto:
package options / compiler  / debugging section  and look for local symbols
if this is not checked I will get this error.  After playing around with my
compiler options I unchecked local symbols and rebuilt the package and
Access Violation in dcc.dll read of address occurred...  If anyone has this
happen while using the ics components that is how I managed to fix it.  Also
I got error about unable to find *.dcr files if you edit your package source
and specify the full path to each dcr file you will find all goes well.

---- End of message ----


Special thanks
--------------

to Bob Dolan (bobd@overware.com) who corrected my bad english.
(Ok, I introduced new errors since I updated this file... For those
who cares, I normally speak french.)


francois.piette@overbyte.be
francois.piette@rtfm.be
francois.piette@swing.be
http://www.overbyte.be
http://www.rtfm.be/fpiette/indexuk.htm
http://users.swing.be/francois.piette/indexuk.htm
