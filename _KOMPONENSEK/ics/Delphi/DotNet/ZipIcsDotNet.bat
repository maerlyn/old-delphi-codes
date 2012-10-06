@echo off
if exist IcsDotNet.zip.bak del IcsDotNet.zip.bak
if exist IcsDotNet.zip ren IcsDotNet.zip IcsDotNet.zip.bak
echo  >ZipIcsDotNet.lst ZipIcsDotNet.bat
echo >>ZipIcsDotNet.lst ReadMe.txt
echo >>ZipIcsDotNet.lst ics_logo.gif
echo >>ZipIcsDotNet.lst install.bat

echo >>ZipIcsDotNet.lst IcsDotNet.bdsgroup

echo >>ZipIcsDotNet.lst Overbyte.Ics.WinSock.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.WSocket.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.WSocket.dcr
echo >>ZipIcsDotNet.lst Overbyte.Ics.WSockBuf.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.WSocketServer.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.WSocketServer.dcr
echo >>ZipIcsDotNet.lst Overbyte.Ics.Component.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.ConApp.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.FtpClient.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.FtpClient.dcr
echo >>ZipIcsDotNet.lst Overbyte.Ics.HttpClient.pas
echo >>ZipIcsDotNet.lst Overbyte.Ics.HttpClient.dcr

echo >>ZipIcsDotNet.lst IcsSocketClientConsoleDemo.bdsproj
echo >>ZipIcsDotNet.lst IcsSocketClientConsoleDemo.dpr
echo >>ZipIcsDotNet.lst IcsSocketClientConsoleDemo.cfg

echo >>ZipIcsDotNet.lst IcsSocketServerConsoleDemo.bdsproj
echo >>ZipIcsDotNet.lst IcsSocketServerConsoleDemo.dpr
echo >>ZipIcsDotNet.lst IcsSocketServerConsoleDemo.cfg

echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpReceiver.bdsproj
echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpReceiver.dpr
echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpReceiver.cfg

echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpSender.bdsproj
echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpSender.dpr
echo >>ZipIcsDotNet.lst IcsSocketConsoleUdpSender.cfg

echo >>ZipIcsDotNet.lst IcsConsoleHttpClient.bdsproj
echo >>ZipIcsDotNet.lst IcsConsoleHttpClient.dpr
echo >>ZipIcsDotNet.lst IcsConsoleHttpClient.cfg

echo >>ZipIcsDotNet.lst IcsConsoleFtpClient.bdsproj
echo >>ZipIcsDotNet.lst IcsConsoleFtpClient.dpr
echo >>ZipIcsDotNet.lst IcsConsoleFtpClient.cfg

echo >>ZipIcsDotNet.lst FtpTst.bdsproj
echo >>ZipIcsDotNet.lst FtpTst.dpr
echo >>ZipIcsDotNet.lst FtpTst.cfg
echo >>ZipIcsDotNet.lst FtpTst.dsk
echo >>ZipIcsDotNet.lst FtpTst.res
echo >>ZipIcsDotNet.lst FtpTst1.pas
echo >>ZipIcsDotNet.lst FtpTst1.nfm
echo >>ZipIcsDotNet.lst FtpTst2.pas
echo >>ZipIcsDotNet.lst FtpTst2.nfm

:buildzip
wzzip -a -r -P IcsDotNet.zip @ZipIcsDotNet.lst

:end
if exist ZipIcsDotNet.lst del ZipIcsDotNet.lst
dir IcsDotNet.zip
pause
