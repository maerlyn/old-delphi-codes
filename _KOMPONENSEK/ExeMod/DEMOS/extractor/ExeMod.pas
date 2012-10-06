unit ExeMod;
{
This is the first beta of the full version 1.00
That allows an ExeMod exe to work properly even
from a CD..for info on how an exe on CD is handled
please see the notes in the Interface section.

Written by G.A. Carpenter (works on D2 thru D7)
This is FreeWare for any purpose...use as you will.
Consider it public domain... feel free to make
any changes you want... if you improve it I'd love
to get a copy of the improvements :-)

gacarpenter386@yahoo.com <gwen carpenter (Gwena)>

You can use this in any program, commercial or freeware,
and you DO NOT need to mention where it came from.
(no attribution needed)

If you use NT/2000/XP please read the NT-NOTE below!

This unit enables you to make a delphi application that can
alter its own exe file on disk... this appears to happen
at runtime but actually the switch quickly takes place after
a shutdown of the running app during which a copy of the exe
deletes the old version and replaces it with a new version.

While it is also possible to alter a running exe directly by altering
the hd sectors that hold the exe's data. I abandoned that scheme
because although it worked quickly and transparently it proved to
be risky :-(

This unit lets you easily create...
-Self altering exe files
-Program Generators
-Apps that update live over the internet
-Apps that keep an internal record of data
 (no ini file or registry usage needed)
-File handling utils
-Copy protection schemes
-Self extracting archives
-Programs that alter and store data in other programs and files.
...and many other strange things ;-)


NOTE! ExeMod will not delete a demarc delimited string from an
external file if the deletion would take the file down to 0 bytes
in length! This is because some disk cleanup utils can be set to
automatically remove 0 length files. If you intend to use an external
file to hold data that will be handled by ExeMod you should begin
by placing at least a single byte of data at the beginning of the file.

------
NT-NOTE!

If you use ExeMod.pas on an NT machine then you may need to run the exe
outside the delphi ide... for some reason NT causes the debugger to
faint at the sight of my code :-)  the exe files run just fine on NT
but just not from inside the ide....

-------
Here are some simple demos for you to look over.

(Remember to put ExeMod.pas into your compiler's path or into your
project's directory... then add ExeMod to your program's uses clause)
------------------------------------------------------------------------
-Code to alter a byte in an exe and then alter the exe at runtime and
put the change onto the exe's file on disk

procedure TForm1.Button1Click(Sender: TObject);
begin
 ReadExe;  //Read exe file from disk and put into global string named Exe
 Exe[3] := 'A';  //Change byte 3 of Global string Exe
 AlterExe;   //Alter the exe file on disk then quickly restart program
end;
------------------------------------------------------------------------
-Code to add a file from disk to the end of the exe at runtime.
Assume a small txt file named C:\myfile.txt exists and contains
the text 'test'

procedure TForm1.Button1Click(Sender: TObject);
begin
 AddFile2Exe('gac1','C:\myfile.txt');
 AlterExe;
end;

The string 'gac1' serves to locate the data at the end of the exe.
I call gac1 a 'Demarc name' because when combined with a few other
characters it serves as a demarcation line in the exe file... this
gives you an easy way to search through the file and find the location
of the start of your data.

Here is what is added to the exe at runtime when you click the button

SO!#GAC1¶testEO!#GAC1

The data from the file is the 4 bytes 'test' all the rest is just delimiter
data so that ExeMod can locate the data in the exe file.
The Character '¶' in the string is to tell the program where the name
given to the data string ('GAC1') ends. it is chr(182) please do NOT
use chr(182) as a character in any demarc name or you will cause an error!

AddFile2Exe will load the exe from disk if it has not already been
loaded... it checks to see if Exe = '' and if so does a ReadExe;
-----------------------------------------------------------------------
-Code to extract an ExeMod data block from the exe at runtime and save
it to a file on disk... then remove the block from the exe and alter the
exe on disk so that it no longer contains any trace of the data block.

procedure TForm1.Button1Click(Sender: TObject);
begin
 ExtractAndStrip('gac1','myfile2.txt');
 AlterExe;
end;

This extracts the ExeMod data block gac1 to a file named 'myfile2.txt'
the file is placed into the directory where the program's exe file is
located.

This sort of code is good for making a self-extracting installation
program in delphi. All of your help files,dlls..etc can be extracted
during the first run of your program... exe size will be small because
the data will be removed from the exe after extraction. All files will
be extracted to the program's dir unless a full path name is given.
-----------------------------------------------------------------------
Code to save a copy of the running exe

procedure TForm1.Button1Click(Sender: TObject);
begin
 Exe2File('MyNewExe.exe');
 Exe := ''; //set Exe to empty string (save space if data no longer needed)
end;

Saves a copy of the running exe as myNewExe.exe in the same directory
where the running exe is. If Exe has been altered the altered version
will be saved... if Exe has not been loaded yet then ReadExe is called.
-----------------------------------------------------------------------
Code to save a modified copy of the running exe... this simple code
gives you a program generator.

procedure TForm1.Button1Click(Sender: TObject);
begin
 ReadExe;
 //add code here to modify the string named 'Exe'
 Exe2File('MyNewExe.exe');
 Exe := ''; //set Exe to empty string (save space if data no longer needed)
end;

This simple code is actually VERY powerful and VERY useful...
-----------------------------------------------------------------------

---> Much more Demo code and many more  procedures to come..soon as I
get some more free time :-)


}

interface

uses
 Windows, SysUtils, Classes, Forms, ShellAPI, Dialogs;

var Exe: string;

procedure Delay(ms: longint);
procedure ReadExe;
procedure String2File(String2BeSaved, FileName: string);
function  WinDrv: char;
function  GetDemarcCount: integer;
procedure GetDemarcName(DNumber: Integer; var DName: String);
function  File2String(FileName: string): string;
function  PeekExeByte(Byte2Get: Integer): byte;
function  PeekExeWord(Word2Get: Integer): word;
procedure PeekExeString(StartByte,Count: Integer; var ReturnedStr: String);
procedure PokeExeByte(Byte2set: Integer; ByteVal: Byte);
procedure PokeExeByteI(Byte2set: Integer; ByteVal: Byte);
procedure PokeExeString(StartByte: Integer; String2Insert: String);
procedure PokeExeStringI(StartByte: Integer; String2Insert: String);
procedure ExtractFromExe(DemarcStr: string; var ExtractedStr: string);
procedure ExtractFromFile(DemarcStr: string; DataFile: string; var ExtractedStr: string);
procedure DelFromString(DemarcStr: string; var String2Change: string);
procedure DelFromExe(DemarcStr: string);
procedure DelFromFile(DemarcStr, FileName: string);
procedure Add2File(DemarcStr, FileName, String2Add: string);
procedure ReplaceInFile(DemarcStr, FileName, ReplacementString: string);
procedure TackOnFile(DemarcStr, FileName, File2Add: string);
procedure Add2String(DemarcStr, String2Add: string; var String2Alter: string);
procedure ReplaceInString(DemarcStr, ReplacementString:
 string; var String2Alter: string);
procedure ReplaceInExe(DemarcStr, ReplacementString: string);
procedure InsOrReplaceInString(DemarcStr, ReplacementString:
 string; var String2Alter: string);
procedure InsOrReplaceInExe(DemarcStr, ReplacementString: string);
procedure AlterExe;
procedure ExtractAndStrip(DemarcStr, FileName: string);
procedure Exe2File(FileName: string);
procedure Extract2File(DemarcStr, FileName: string);
procedure AddFile2Exe(DemarcStr, FileName: string);
procedure Add2Exe(DemarcStr, String2Add: string);
procedure Stream2Exe(TempStream: TMemoryStream);
procedure String2Stream(a: String;var b: TMemoryStream);
procedure Stream2String(b: TMemoryStream;var a: String);


implementation

//==================================================
//This is just a simple procedure to implement a delay

procedure Delay(ms: longint);
var
 TheTime: LongInt;
begin
 TheTime := GetTickCount + ms;
 while GetTickCount < TheTime do
   Application.ProcessMessages;
end;
//===================================================

//===================================================
//This code returns the char of the drive where
//windows is installed

function  WinDrv: char;
var
  WinDir : String;
  n      : Integer;
begin
  SetLength(WinDir,256);
  n := GetWindowsDirectory(PChar(WinDir),256);
  SetLength(WinDir,n);
  Result := WinDir[1];
end;
//===================================================

//===================================================
//This function returns the number of data strings that
//are stored in the exe under demarc names... if demarc
//delimited data strings have been added to the global
//string Exe but not yet flushed to disk by an AlterExe
//command, they will still be included in this count!

function  GetDemarcCount: integer;
var Count,X: Integer;
begin
Count := 0;
If Exe = '' then ReadExe;
For X := 1 to Length(Exe)-10 do
  begin
    If  (Exe[X] = 'S') and (Exe[X+1] = 'O')
    and (Exe[X+2] = '!') and (Exe[X+3] = '#')
    then
    begin
      Inc(Count);
    end;
  end;
Result := Count;
end;
//===================================================

//===================================================
//You pass a number (DNumber) to this procedure and it
//returns a string (DName) that contains the DNumber'th
//demarc name from the exe file. If the DNumber'th
//demarc string does not exist then an empty string
//is returned. This procedure lets you easily look through
//the running exe or any other file to discover the demarc
//names under which data has been stored.

procedure GetDemarcName(DNumber: Integer; var DName: String);
var Count,X,Y: Integer;
begin
Count := 0;
If Exe = '' then ReadExe;
For X := 1 to Length(Exe)-10 do
  begin
    If  (Exe[X] = 'S') and (Exe[X+1] = 'O')
    and (Exe[X+2] = '!') and (Exe[X+3] = '#')
    then
    begin
      Inc(Count);
      If Count = DNumber then
      begin
        Y := X+4;
        While Exe[Y] <> chr(182) do
        begin
          DName := DName+Exe[Y];
          Inc(Y);
        end;
      end;
    end;
  end;
end;
//===================================================


//===================================================
//This code loads the Exe file from disk and into
//the global string var 'Exe'

procedure ReadExe;
var
 ExeStream: TFileStream;
begin
 ExeStream := TFileStream.Create(Application.ExeName, fmOpenRead
   or fmShareDenyNone);
 try
   SetLength(Exe, ExeStream.Size);
   ExeStream.ReadBuffer(Pointer(Exe)^, ExeStream.Size);
 finally
   ExeStream.Free;
 end;
end;
//===================================================

//===================================================
//This code can write any string to disk as a file

procedure String2File(String2BeSaved, FileName: string);
var
 MyStream: TMemoryStream;
begin
 if String2BeSaved = '' then exit;
 SetCurrentDir(ExtractFilePath(Application.ExeName));
 MyStream := TMemoryStream.Create;
 try
   MyStream.WriteBuffer(Pointer(String2BeSaved)^, Length(String2BeSaved));
   MyStream.SaveToFile(FileName);
 finally
   MyStream.Free;
 end;
end;
//===================================================

//===================================================
//This code can read any file from disk and into a string

function File2String(FileName: string): string;
var
 MyStream: TMemoryStream;
 MyString: string;
begin
 MyStream := TMemoryStream.Create;
 try
   MyStream.LoadFromFile(FileName);
   MyStream.Position := 0;
   SetLength(MyString, MyStream.Size);
   MyStream.ReadBuffer(Pointer(MyString)^, MyStream.Size);
 finally
   MyStream.Free;
 end;
 Result := MyString;
end;
//===================================================

//===================================================
//This code lets you read a byte from the running exe
//directly from the ram. Byte 1 = first byte of exe
//DO NOT attempt to read values lower than 1 or function
//will fail and produce random data. Be advised that
//bytes that are appended to the exe file cannot be read
//using this function as that data is not in ram at runtime!
//Do not attempt to read within or beyond a few hundred bytes
//of the end of your exe (minus any appended data) as data
//beyond that range cannot be retrieved with this simple
//function... there is no range checking for the upper
//bound so use caution.

function  PeekExeByte(Byte2Get: Integer): byte;
Begin
If Byte2Get < 1 then Exit;
Result := byte(pointer(Hinstance+Byte2Get-1)^);
End;
//===================================================

//===================================================
//This function is nearly identical to the one above
//so the comments for PeekExeByte apply here as well.

function  PeekExeWord(Word2Get: Integer): word;
Begin
If Word2Get < 1 then Exit;
Result := word(pointer(Hinstance+Word2Get-1)^);
End;
//===================================================

//===================================================
//This code returns a string from the exe image in RAM
//cautions are similar to PeekExeByte

procedure PeekExeString(StartByte,Count: Integer; var ReturnedStr: String);
var X: Integer;
Begin
  If StartByte < 1 then Exit;
  For X := StartByte to StartByte+Count-1 do
  begin
    ReturnedStr := ReturnedStr+(char(pointer(Hinstance+X-1)^));
  end;
End;
//===================================================

//===================================================
//This code can stuff a string into the exe starting at
//a specified point... changes are NOT immediately saved
//to disk.

procedure PokeExeString(StartByte: Integer; String2Insert: String);
var X: Integer;
Begin
  If Exe = '' then ReadExe;
  If StartByte + Length(String2Insert) > Length(Exe) then Exit;
  If StartByte < 1 then Exit;
  For X := 1 to Length(String2Insert) do
  begin
    Exe[X+StartByte-1] := String2Insert[X];
  end;
end;
//===================================================

//===================================================
//This code can stuff a string into the exe starting at
//a specified point... changes ARE immediately saved
//to disk... Take care!

procedure PokeExeStringI(StartByte: Integer; String2Insert: String);
var X: Integer;
Begin
  If Exe = '' then ReadExe;
  If StartByte + Length(String2Insert) > Length(Exe) then Exit;
  If StartByte < 1 then Exit;
  For X := 1 to Length(String2Insert) do
  begin
    Exe[X+StartByte-1] := String2Insert[X];
  end;
  AlterExe;
end;
//===================================================


//===================================================
//Pokes a byte value into a specified byte in the string
//named Exe. This string holds the image of the running
//executable. The altered version of exe is NOT immediately
//written to disk!

procedure PokeExeByte(Byte2set: Integer; ByteVal: Byte);
Begin
If Exe = '' then ReadExe;
If Byte2Set > Length(Exe) then Exit;
Exe[Byte2Set] := chr(ByteVal);
end;
//===================================================

//===================================================
//Identical to PokeExeByte procedure except the changed
//byte IS immediately written to disk! USE CARE as the
//state of the exe will be lost on restart unless you
//store all desired data inside the string named Exe.

procedure PokeExeByteI(Byte2set: Integer; ByteVal: Byte);
Begin
If Exe = '' then ReadExe;
If Byte2Set > Length(Exe) then Exit;
Exe[Byte2Set] := chr(ByteVal);
AlterExe;
end;
//===================================================

//===================================================
//This code gets data from the exe that was stored under a
//specified demarc name and places it into a string

procedure ExtractFromExe(DemarcStr: string; var ExtractedStr: string);
var
 d, e: integer;
begin
 if Length(Exe) = 0 then ReadExe;
 if Pos(uppercase('so!#' + DemarcStr + chr(182)), Exe) > 0 then
 begin
   d := Pos(uppercase('so!#' + DemarcStr + chr(182)), Exe)
     + length(uppercase('so!#' + DemarcStr + chr(182)));
   e := Pos(uppercase('eo!#' + DemarcStr), Exe);
   ExtractedStr := Copy(Exe, d, e - d);
 end;
end;
//===================================================


//===================================================
//This code gets a demarc delimited data string from
//an external file and loads it into a string

procedure ExtractFromFile(DemarcStr: string; DataFile: string; var ExtractedStr: string);
var
 d, e: integer;
 Temp: String;
begin
 Temp := File2String(DataFile);
 if Pos(uppercase('so!#' + DemarcStr + chr(182)), Temp) > 0 then
 begin
   d := Pos(uppercase('so!#' + DemarcStr + chr(182)), Temp)
     + length(uppercase('so!#' + DemarcStr + chr(182)));
   e := Pos(uppercase('eo!#' + DemarcStr), Temp);
   ExtractedStr := Copy(Temp, d, e - d);
 end;
end;
//===================================================


//===================================================
//This code deletes a demarc delimited string from any other string

procedure DelFromString(DemarcStr: string; var String2Change: string);
var
 a, b: string;
begin
 a := UpperCase('so!#' + DemarcStr + chr(182));
 b := UpperCase('eo!#' + DemarcStr);
 delete(String2Change, pos(a, String2Change), (pos(b, String2Change)
   + length(b) - pos(a, String2Change)));
end;
//===================================================

//===================================================
//This code deletes a demarc delimited string from global
//string named Exe

procedure DelFromExe(DemarcStr: string);
begin
If Exe = '' then ReadExe;
DelFromString(DemarcStr,Exe);
end;
//===================================================


//===================================================
//This code deletes a demarc delimited string from a
//file on disk

procedure DelFromFile(DemarcStr, FileName: string);
var
 MyString: string;
begin
 MyString := File2String(FileName);
 DelFromString(DemarcStr, MyString);
 String2File(MyString, FileName);
end;
//===================================================

//===================================================
//This code adds a demarc delimited string to a file

procedure Add2File(DemarcStr, FileName, String2Add: string);
var
 MyString: string;
begin
 If DemarcStr = '' then Exit;
 MyString := File2String(FileName);
 MyString := MyString + uppercase('so!#' + DemarcStr + chr(182)) + String2Add + uppercase
   ('eo!#' + DemarcStr);
 String2File(MyString, FileName);
end;
//===================================================

//===================================================
//This code adds a demarc delimited string to a file
//if the string already exists it is replaced

procedure ReplaceInFile(DemarcStr, FileName, ReplacementString: string);
begin
 If DemarcStr = '' then Exit;
 DelFromFile(DemarcStr, FileName);
 Add2File(DemarcStr, FileName, ReplacementString);
end;
//===================================================

//===================================================
//This code adds a demarc delimited file to another file

procedure TackOnFile(DemarcStr, FileName, File2Add: string);
var
 Mystring: string;
begin
 If DemarcStr = '' then Exit;
 MyString := File2String(File2add);
 Add2File(DemarcStr, FileName, MyString);
end;
//===================================================

//===================================================
//This code adds a demarc delimited string to any other
//string.

procedure Add2String(DemarcStr, String2Add: string; var String2Alter: string);
begin
 If DemarcStr = '' then Exit;
 String2Alter := String2Alter + uppercase('so!#' + DemarcStr + chr(182))
   + String2Add + uppercase('eo!#' + DemarcStr);
end;
//===================================================

//===================================================
//This code replaces the data held within a demarc delimited
//string with new data... the demarc string must exist.

procedure ReplaceInString(DemarcStr, ReplacementString: string;
 var String2Alter: string);
begin
 If DemarcStr = '' then Exit;
 if pos(uppercase('so!#' + DemarcStr + chr(182)), String2Alter) = 0 then exit;
 DelFromString(DemarcStr, String2Alter);
 Add2String(DemarcStr, ReplacementString, String2Alter);
end;
//===================================================

//===================================================
//This code replaces the data held within a demarc delimited
//string with new data... the demarc string must exist. this
//is identical to ReplaceInString except it defaults to
//the global string named Exe.

procedure ReplaceInExe(DemarcStr, ReplacementString: string);
begin
 If DemarcStr = '' then Exit;
 if pos(uppercase('so!#' + DemarcStr + chr(182)), Exe) = 0 then exit;
 DelFromString(DemarcStr, Exe);
 Add2String(DemarcStr, ReplacementString, Exe);
end;
//===================================================

//===================================================
//This code will replace the data held within a demarc delimited
//string with new data... if the demarc delimited string does not
//exist it will be created.

procedure InsOrReplaceInString(DemarcStr, ReplacementString: string;
 var String2Alter: string);
begin
 If DemarcStr = '' then Exit;
 DelFromString(DemarcStr, String2Alter);
 Add2String(DemarcStr, ReplacementString, String2Alter);
end;
//===================================================

//===================================================
//This code will replace the data held within a demarc delimited
//string with new data... if the demarc delimited string does not
//exist it will be created. This is identical with InsOrReplaceInString
//except it defaults to the global string Exe.

procedure InsOrReplaceInExe(DemarcStr, ReplacementString: string);
begin
 If DemarcStr = '' then Exit;
 If Exe = '' Then ReadExe;
 DelFromString(DemarcStr, Exe);
 Add2String(DemarcStr, ReplacementString, Exe);
end;
//===================================================

//===================================================
//This code swaps the current version of the running exe
//on disk for the version that is held in the global 
//string named 'Exe' Your application quickly shuts down and
//re-starts as the new version. ANY DATA THAT YOU WANT THE NEW
//VERSION TO BE AWARE OF MUST BE PASSED AS DATA INCLUDED
//IN 'Exe' before calling this procedure!!!

procedure AlterExe;
begin
 if (Exe) <> '' then
 begin
   String2File(Exe, 'temp0a0.exe');
   ShellExecute(0, 'open', PChar('temp0a0.exe'),
     PChar('"'+ExtractFilename(Application.ExeName)+'"'), nil, SW_SHOW);
   Application.Terminate;
 end;
end;
//===================================================

//===================================================
//This code extracts a demarc delimited string from the 
//global string 'Exe' to a file. The data is then stripped
//out of 'Exe'  .. this procedure is very useful for
//making self extracting archives that remove all the
//archived files from the exe after extraction... reducing
//hd space required... just store all your dll's, images
//etc in the exe and extract and strip them the first
//time the user runs the program :-)

procedure ExtractAndStrip(DemarcStr, FileName: string);
var
 Temp: string;
begin
 ExtractFromExe(DemarcStr, Temp);
 if Length(Temp) <> 0 then
 begin
   DelFromString(DemarcStr, Exe);
   String2File(Temp, FileName);
 end;
end;
//===================================================

//===================================================
//This code saves the global string exe to disk as a
//new exe.

procedure Exe2File(FileName: string);
begin
 if Exe = '' then ReadExe;
 String2File(Exe, FileName);
end;
//===================================================

//===================================================
//This code extracts a demarc delimited string from the
//exe and saves it as a file.

procedure Extract2File(DemarcStr, FileName: string);
var
 MyString: string;
begin
 ExtractFromExe(DemarcStr, MyString);
 if MyString <> '' then String2File(MyString, FileName);
end;
//===================================================

//===================================================
//This code adds a file to the global string exe as
//a demarc delimited string.

procedure AddFile2Exe(DemarcStr, FileName: string);
var
 MyString: string;
begin
 If DemarcStr = '' then Exit;
 MyString := File2String(FileName);
 if Exe = '' then ReadExe;
 Add2String(DemarcStr, MyString, Exe);
end;
//===================================================

//===================================================
//This code adds a demarc delimited string to the
//global string Exe.

procedure Add2Exe(DemarcStr, String2Add: string);
begin
 If DemarcStr = '' then Exit;
 if Exe = '' then readExe;
 Add2String(DemarcStr, String2Add, Exe);
end;
//===================================================

//===================================================
//This code replaces the current exe file on disk with
//one that is held in a TMemoryStream. This makes it
//easy to update an exe by downloading a new version
//over the internet and into a stream... use any of
//the free internet packs... like Fpiettes or Synapse lib

procedure Stream2Exe(TempStream: TMemoryStream);
begin
 SetCurrentDir(ExtractFilePath(Application.ExeName));
 TempStream.SaveToFile('temp0a0.exe');
 ShellExecute(0, 'open', PChar('temp0a0.exe'),
   PChar(ExtractFilename(Application.ExeName)), nil, SW_SHOW);
 Application.Terminate;
end;
//===================================================

//==============================================================================
//This code copies a string to a stream

procedure String2Stream(a: String;var b: TMemoryStream);
begin
b.Position := 0;
b.WriteBuffer(Pointer(a)^,Length(a));
b.Position := 0;
end;

//==============================================================================

//==============================================================================
//This code copies a stream to a string

procedure Stream2String(b: TMemoryStream;var a: String);
begin
b.Position := 0;
SetLength(a,b.Size);
b.ReadBuffer(Pointer(a)^,b.Size);
b.Position := 0;
end;

//==============================================================================
//This is the ExeMod.pas Initialization code...this code runs each time your
//exe is started... this code handles the actual swapping of one version of
//the exe for another. This is done by an exe file swap and re-start carried
//out by a temporary copy of the exe named temp0a0.exe. The initialization
//code can look for and delete a file named temp0a0.exe if it is found
//in the current dir. This is how temp0a0.exe is removed after an exe swap. 

initialization
 begin
   SetCurrentDir(ExtractFilePath(Application.ExeName));

//******************************************************************************
//NOTE--> WinDrv = The drive where Windows is installed!
//
//Beginning of code that lets an ExeMod exe work properly even
//if it is on a read-only CD ... this code will cause the exe
//on the CD to create a dir on WinDrv with the same name as the running
//exe. Then it moves a copy of the exe to that dir and runs that
//copy before shutting down. Thereafter any time the exe on CD is
//run it will act only as a shortcut to the version on WinDrv.
//If you do not want this functionality then just comment out this
//code or delete it from this unit. You can also disable this code
//by changing byte 3 of your exe so that it holds a zero value.
//'MZP' becomes 'MZ' followed by a zero byte.
//Programs that do not attempt to alter themselves at runtime and
//which do not attempt to save files in the current dir should
//disable this code... otherwise the app will always needlessly copy
//itself to WinDrv and run from there!

if (FileGetAttr(Application.ExeName) and faReadOnly) > 0 then
begin
  if PeekExeByte(3) = 0 then Exit;
  FileSetAttr(Application.ExeName, FileGetAttr(Application.ExeName)
        xor faReadOnly);
  if (FileGetAttr(Application.ExeName) and faReadOnly) > 0  then
    begin
    If FileExists(Windrv+':\'+ExtractFileName(Application.ExeName)+
          '\'+ExtractFileName(Application.ExeName)) then
      begin
        ShellExecute(0, 'open', PChar((WinDrv+':\'+ExtractFileName(Application.ExeName)+
              '\'+ExtractFileName(Application.ExeName))),nil, nil, SW_SHOW);
        Application.Terminate;
      end
      else
      begin
        CreateDir(WinDrv+':\'+ExtractFileName(Application.ExeName));
        Exe2File(WinDrv+':\'+ExtractFileName(Application.ExeName)+
          '\'+ExtractFileName(Application.ExeName));
        ShellExecute(0, 'open', PChar((WinDrv+':\'+ExtractFileName(Application.ExeName)+
          '\'+ExtractFileName(Application.ExeName))),nil, nil, SW_SHOW);
        Application.terminate;
      end;
    end;
end;
//This is the end of the code that allows proper operation from a CD.
//******************************************************************************

   if ParamStr(1) = 'deltemp' then
   begin
     while not (deletefile('temp0a0.exe')) do
     begin
       if (FileGetAttr('temp0a0.exe') and faReadOnly) > 0
         then FileSetAttr('temp0a0.exe', FileGetAttr('temp0a0.exe')
         xor faReadOnly);
       delay(10);
     end;
     deletefile('temp0a0.exe');
   end;
   if UpperCase(ExtractFileName(Application.ExeName)) = ('TEMP0A0.EXE') then
   begin
     ReadExe;
     while not (deletefile(ParamStr(1))) do
     begin
       if (FileGetAttr(ParamStr(1)) and faReadOnly) > 0
         then FileSetAttr(ParamStr(1), FileGetAttr(ParamStr(1))
         xor faReadOnly);
       delay(10);
     end;
     String2File(Exe, ParamStr(1));
     ShellExecute(0, 'open', PChar(ParamStr(1)),
       PChar('deltemp'), nil, SW_SHOW);
     Application.Terminate;
   end;
 end;

end.

