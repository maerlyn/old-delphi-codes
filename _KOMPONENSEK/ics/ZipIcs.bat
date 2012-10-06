@echo off
if exist IcsOld.zip del IcsOld.zip
if exist Ics.zip    ren Ics.zip    IcsOld.zip
wzzip -P Ics.zip @ZipIcsD.lst
wzzip -P Ics.zip @ZipIcsB.lst
wzzip -P Ics.zip @ZipIcsDotNet.lst
ren Ics.zip Ics.zip
pause
