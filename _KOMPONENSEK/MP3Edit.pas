{
   Név:                         TMP3Edit komponens
   Verzió:                      1.0
   Dátum:                       2000.05.04.

   Leírás:                      A komponens olvassa, írja, törli az ID3 Tag-eket az MP3 fájlokból,
                                és kiolvassa az MPEG infókat úgy, mint a Winamp ID3 Tag editorjában
                                (nézd meg a demót!)
                                Az MP3Edit felismeri a Xing Variable Bitrate-et

   Szerzõ:                      Bakondy Péter (bp216@hszk.bme.hu)
   Honlap:                      http://www.hszk.bme.hu/~bp216/programok.htm
   Delphi verzió:               D5
   Tipus:                       Freeware

Properties, Methods and Events leírása:
    constructor Create(AOwner: TComponent);   - Creates an instance
    destructor Destroy; override;             - Destroys an instance
    method Save;                              - Menti az ID3 Tag-et a fájlba
    method Remove;                            - Kitörli az ID3 Tag-et a fájlból

    property Filename: TFilename;             - az MP3 fájl neve
    property Artist: String;                  - Szerzõ                     (30 Chars)
    property Title: String;                   - Cím                        (30 Chars)
    property Album: String;                   - Album                      (30 Chars)
    property Year: String;                    - Év                         ( 4 chars)
    property Comment: String;                 - Komment                    (30 Chars)
    property Genre: String;                   - Típus                      [csak olvasható]
    property GenreID: Byte;                   - Típus sorszám
    property IsTag: Boolean;                  - Van-e ID3 Tag a fájlban    [csak olvasható]
    property Error: String;                   - Error Message              [csak olvasható]

az MPEG infók [mind csak olvasható]:                                       [pl]
    property Size: String;                    - fájlméret                   (4685784 bytes)
    property MPEG: String;                    - MPEG verzió                 (MPEG 1.0)
    property Layer: String;                   - Layer verzió                (layer 3)
    property LengthSec: String;               - Hossz ms-ben                (259 sec)
    property LengthHrMinSec: String;          - Hossz óra:perc:ms-ben       (1:04:19)
    property Channel: String;                 - Csatorna                    (Joint Stereo)
    property Sample: String;                  - Mintavétel                  (44100 hz)
    property Padding: String;                 - Padding                     (Yes/No)
    property Bitrate: String;                 - Bitrate                     (192 kbit)
    property Frames: String;                  - Frame-ek száma              (12532 frames)
    property CRC: String;                     - CRC                         (Yes/No)
    property Privates: String;                - Private                     (Yes/No)
    property Copyright: String;               - Copyright                   (Yes/No)
    property Original: String;                - Original                    (Yes/No)
    property Emphasis: String;                - Hangsúly                    (None/..)
    property XingVBR: Boolean;                - Xing Variable BitRate

    property onChangeFile:TNotifyEvent;       - Végrehajtódik, ha megnyit egy fájlt
    property onChange:TNotifyEvent;           - Végrehajtódik, ha megváltozik egy ID3 Tag
    property onError:TNotifyEvent;            - Végrehajtódik, ha hibát észlel

megjegyzések
--------
  A Private Property helyett a Privates szót kellett használnom, mivel az elõbbi
  egy kulcsszó a Delphiben

  Ha többet szeretnél tudni az ID3 Tag-ekrõl, nézd meg ezeket az oldalakat:
    http://www.id3.org/
    http://www.id3lib.org/
}

unit MP3Edit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

const
  TAGLEN = 127;
  MAXGENRES = 114;
  GENRES: array [0..MAXGENRES] of String = (
  'Blues','Classic Rock','Country','Dance','Disco','Funk','Grunge','Hip-Hop','Jazz','Metal','New Age','Oldies',
  'Other','Pop','R&B','Rap','Reggae','Rock','Techno','Industrial','Alternative','Ska','Death Metal','Pranks',
  'Soundtrack','Euro-Techno','Ambient','Trip-Hop','Vocal','Jazz+Funk','Fusion','Trance','Classical','Instrumental',
  'Acid','House','Game','Sound Clip','Gospel','Noise','AlternRock','Bass','Soul','Punk','Space','Meditative',
  'Instrumental Pop','Instrumental Rock','Ethnic','Gothic','Darkwave','Techno-Industrial','Electronic','Pop-Folk',
  'Eurodance','Dream','Southern Rock','Comedy','Cult','Gangsta','Top 40','Christian Rap','Pop/Funk','Jungle',
  'Native American','Cabaret','New Wave','Psychadelic','Rave','Showtunes','Trailer','Lo-Fi','Tribal','Acid Punk',
  'Acid Jazz','Polka','Retro','Musical','Rock & Roll','Hard Rock','Folk','Folk/Rock','National Folk','Swing','Bebob',
  'Latin','Revival','Celtic','Bluegrass','Avantgarde','Gothic Rock','Progressive Rock','Psychedelic Rock','Symphonic Rock',
  'Slow Rock','Big Band','Chorus','Easy Listening','Acoustic','Humour','Speech','Chanson','Opera','Chamber Music','Sonata',
  'Symphony','Booty Bass','Primus','Porn Groove','Satire','Slow Jam','Club','Tango','Samba','Folklore'
  );

type
  TMP3Edit = class(TComponent)
  private
    vFilename: TFilename;
    vMP3Tag, vArtist, vTitle, vAlbum, vComment, vYear, vGenre, vError: String;
    vGenreID: Byte;
    vIsTag, vXingVBR: Boolean;
    vSize, vMPEG, vLayer, vCRC, vLengthSec, vLengthHrMinSec, vChannel, vSample: String;
    vPadding, vBitrate, vFrames, vPrivates, vCopyright, vOriginal, vEmphasis: String;
    vChangeFileEvent, vChangeEvent, vErrorEvent: TNotifyEvent;
    procedure SetFilename(Filename: TFilename);
    procedure SetArtist(Artist: String);
    procedure SetTitle(Title: String);
    procedure SetAlbum(Album: String);
    procedure SetYear(Year: String);
    procedure SetComment(Comment: String);
    procedure SetGenreID(ID: Byte);
    procedure Open;
  protected

  public

  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Save;
    procedure Remove;
    property Filename: TFilename read vFilename write SetFilename;
    property Artist: String read vArtist write SetArtist;
    property Title: String read vTitle write SetTitle;
    property Album: String read vAlbum write SetAlbum;
    property Year: String read vYear write SetYear;
    property Comment: String read vComment write SetComment;
    property Genre: String read vGenre;
    property GenreID: Byte read vGenreID write SetGenreID;
    property IsTag: Boolean read vIsTag;
    property Error: String read vError;
    property Size: String read vSize;
    property MPEG: String read vMPEG;
    property Layer: String read vLayer;
    property CRC: String read vCRC;
    property LengthSec: String read vLengthSec;
    property LengthHrMinsec: String read vLengthHrMinSec;
    property Channel: String read vChannel;
    property Sample: String read vSample;
    property Padding: String read vPadding;
    property Bitrate: String read vBitrate;
    property Frames: String read vFrames;
    property Privates: String read vPrivates;
    property Copyright: String read vCopyright;
    property Original: String read vOriginal;
    property Emphasis: String read vEmphasis;
    property XingVBR: Boolean read vXingVBR;
    property onChangeFile:TNotifyEvent read vChangeFileEvent write vChangeFileEvent;
    property onChange:TNotifyEvent read vChangeEvent write vChangeEvent;
    property onError:TNotifyEvent read vErrorEvent write vErrorEvent;
  end;

procedure Register;

implementation

constructor TMP3Edit.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
end;

destructor TMP3Edit.Destroy;
begin
   inherited Destroy;
end;

procedure TMP3Edit.SetFilename(Filename: TFilename);
begin
   vFilename := Filename;
   Open;
end;

procedure TMP3Edit.SetArtist(Artist: String);
begin
   vArtist := Copy(Artist,0, 30);
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.SetTitle(Title: String);
begin
   vTitle := Copy(Title, 0, 30);
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.SetAlbum(Album: String);
begin
   vAlbum := Copy(Album, 0, 30);
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.SetYear(Year: String);
begin
   vYear := Copy(Year, 0, 4);
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.SetComment(Comment: String);
begin
   vComment := Copy(Comment, 0, 30);
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.SetGenreID(ID: Byte);
begin
   vGenreID := ID;
   if Assigned(onChange) then onChange(Self);
end;

procedure TMP3Edit.Open;

  procedure MPEGInfo;
  const BitRate: array [0..1,0..2,0..15] of integer =
        (((0,32,64,96,128,160,192,224,256,288,320,352,384,416,448,0),
          (0,32,48,56,64, 80, 96, 112,128,160,192,224,256,320,384,0),
          (0,32,40,48,56, 64, 80, 96, 112,128,160,192,224,256,320,0)),
         ((0,32,48,56,64, 80, 96, 112,128,144,160,176,192,224,156,0),
          (0,8, 16,24,32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0),
          (0,8, 16,24,32, 40, 48, 56, 64, 80, 96, 112,128,144,160,0)));
        Frequencies: array [0..2,0..3] of Integer =
        ((11000, 12000,  8000, 1),
         (22050, 24000, 16000, 1),
         (44100, 48000, 32000, 1));

  var c1,c2,c3,c4 : Byte;
      Sample, Lay, Mpg, BRate, FileSiz : Integer;
      FrameSize, Padding1, Length, BRate1 : Integer;
      XingOff, XingFrames: Integer;
      FirstMpegFramePos: Integer;
      Buf: Array [0..11] of Byte;
      f1 : TStream;
  begin
    Lay:=0;
    f1 := TFileStream.Create (vFileName, fmOpenRead or fmShareDenyNone);
    try
      FirstMpegFramePos := -1;
      f1.Position := 0;
      f1.ReadBuffer(c2,1);
      f1.Position := 1;
      f1.ReadBuffer(c3,1);
      f1.Position := 2;
      f1.ReadBuffer(c4,1);
      repeat
        repeat
          c1 := c2;
          c2 := c3;
          c3 := c4;
          f1.Position := FirstMpegFramePos + 4;
          f1.ReadBuffer(c4,1);
          FirstMpegFramePos := FirstMpegFramePos + 1;
        until ((c1 = 255) and ((c2 and 224) = 224 ));
      until (((c2 and 24) <> 8) and ((c3 and 240) <> 0) and ((c3 and 240) <> 240) and
      ((c3 and 12) <> 12) and ((c4 and 3) <> 2));
      vSize := IntToStr(f1.Size) + ' bytes';
      FileSiz := f1.Size;
      finally
        f1.Free;
      end;
    if ((c2 and 16) = 0) then
    begin
      vMPEG := 'MPEG 2.5';
      Mpg := 3;
    end
    else
    begin
      if ((c2 and 8) = 0) then
      begin
        vMPEG := 'MPEG 2.0';
        Mpg := 2;
      end
      else
      begin
        vMPEG := 'MPEG 1.0';
        Mpg := 1;
      end;
    end;
    case ((c2 and 6) div 2) of
      0  : begin
             vLayer := 'layer 1';
             Lay := 1;
           end;
      1,2: begin
             vLayer := 'layer 3';
             Lay := 3;
           end;
      3  : begin
             vLayer := 'layer 2';
             Lay := 2;
           end;
    end;
    if ((c2 = 253) and ((c3 and 253) = 128)) then
    begin
      Lay := Lay - 1;
      vLayer := 'layer 2';
    end;
    if ((c2 and 1) = 0) then vCRC := 'Yes'
      else vCRC := 'No';
    BRate := (c3 and 240) div 16;
    Sample := Frequencies[3 - Mpg][(c3 and 12) div 4];
    vSample := IntToStr(Sample) + 'hz';
    Padding1 := (c3 and 2) div 2;
    if Padding1 = 1 then vPadding := 'Yes'
      else vPadding := 'No';
    if Mpg = 3 then Mpg := 2;
    BRate1 := BitRate[Mpg-1][Lay-1][BRate];
    if Lay = 1 then
    begin
      FrameSize := (12000 * BitRate[Mpg - 1][0][BRate]) div Sample;
      if Padding1 = 1 then FrameSize := FrameSize * 4;
    end
    else
    begin
      FrameSize := (144000 * BitRate[Mpg - 1][Lay - 1][BRate]) div Sample;
      if Mpg = 2 then FrameSize := FrameSize div 2;
      if Padding1 = 1 then FrameSize := FrameSize + 1;
    end;
    vFrames := IntToStr(FileSiz div (FrameSize - Padding1)) + ' frames';
    case ((c2 and 6) div 2) of
      0  : Lay := 1;
      1,2: Lay := 3;
      3  : Lay := 2;
    end;
    Length := Trunc(FileSiz / (BRate1 * 125));
    case ((c4 and 192) div 64) of
      0 : vChannel := 'Stereo';
      1 : vChannel := 'Joint Stereo';
      2 : vChannel := 'Dual Channel';
      3 : vChannel := 'Mono';
    end;
    if Mpg = 3 then Mpg := 2;
    if ((c2 = 253) and ((c3 and 253) = 128)) then Lay := Lay - 1;
    vBitrate := IntToStr(BitRate[Mpg-1][Lay-1][BRate]) + 'kbit';
    if ((c3 and 1) = 0) then vPrivates := 'No'
      else vPrivates := 'Yes';
    if ((c4 and 8) = 0) then vCopyright := 'No'
      else vCopyright := 'Yes';
    if ((c4 and 4) = 0) then vOriginal := 'No'
      else vOriginal := 'Yes';
    case (c4 and 3) of
      0 : vEmphasis := 'None';
      1 : vEmphasis := '50/15 ms';
      3 : vEmphasis := 'CCIT j.17';
    end;

    vXingVBR := False;
    if vMPEG = 'MPEG 1.0' then
      if vChannel <> 'Mono' then
        XingOff := 36
      else XingOff := 21
    else
      if vChannel <> 'Mono' then
        XingOff := 21
      else XingOff := 13;
    if FirstMpegFramePos + XingOff + 12 < FileSiz then
    begin
      try
        f1 := TFileStream.Create (vFileName, fmOpenRead or fmShareDenyNone);
        f1.Position := FirstMpegFramePos + XingOff;
        f1.ReadBuffer(Buf, 12);
      finally;
        f1.Free;
      end;
      if (Buf [0] = Ord ('X')) and (Buf [1] = Ord ('i')) and
         (Buf [2] = Ord ('n')) and (Buf [3] = Ord ('g')) then
      begin
        vXingVBR := True;
        if (Buf [7] and 1) <> 0 then
          XingFrames := (((((Buf [8] ShL 8) + Buf [9]) ShL 8) + Buf [10]) ShL 8) + Buf [11]
        else XingFrames := 0;
        if Sample > 0
          then Length := Trunc((1152 / Sample) * XingFrames)
        else Length := 0;
      end;
    end;

    vLengthSec := IntToStr(Length) + ' sec';
    if Length < 3600 then
    begin
      if ((Length mod 60) > 9) then
        vLengthHrMinSec := IntToStr(Length div 60) + ':' + IntToStr(Length mod 60)
      else
        vLengthHrMinSec := IntToStr(Length div 60) + ':0' + IntToStr(Length mod 60);
    end
    else
    begin
      if ((Length mod 3600) > 599) then
      begin
        if (((Length mod 3600) mod 60) > 9) then
          vLengthHrMinSec := IntToStr(Length div 3600) + ':' + IntToStr((Length mod 3600) div 60) + ':' + IntToStr((Length mod 3600) mod 60)
        else
          vLengthHrMinSec := IntToStr(Length div 3600) + ':' + IntToStr((Length mod 3600) div 60) + ':0' + IntToStr((Length mod 3600) mod 60);
      end
      else
      begin
        if (((Length mod 3600) mod 60) > 9) then
          vLengthHrMinSec := IntToStr(Length div 3600) + ':0' + IntToStr((Length mod 3600) div 60) + ':' + IntToStr((Length mod 3600) mod 60)
        else
          vLengthHrMinSec := IntToStr(Length div 3600) + ':0' + IntToStr((Length mod 3600) div 60) + ':0' + IntToStr((Length mod 3600) mod 60);
      end;
    end;

  end;

var id3: Array [0..TAGLEN] of Byte;

  function BufStr (APos, ALen : integer) : string;
    begin
      SetString (Result, nil, ALen);
      Move (id3[APos], Result[1], ALen);
      if Pos (#0, Result) > 0
        then Result := copy (Result, 1, Pos (#0, Result) - 1);
      Result := TrimRight (Result);
    end;

  procedure LoadFromStream (AStream : TStream; AStreamSz : integer);
  begin
    if AStreamSz >= TAGLEN then begin
      AStream.Position := AStream.Position + AStreamSz - TAGLEN - 1;
      AStream.ReadBuffer (id3, TAGLEN + 1);
    end;
  end;

  procedure LoadFromFile (const AFileName : string);
  var f : TStream;
  begin
    f := TFileStream.Create (AFileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream (f, f.Size);
    finally
      f.Free;
    end;
  end;

begin
   MPEGInfo;
   if FileExists(vFilename) then begin
      LoadFromFile(vFilename);
      vTitle := ''; vArtist := ''; vAlbum := '';
      vComment := ''; vYear := ''; vGenreID := 255;
      vGenre := '';
      vMP3Tag := BufStr(0, 3);
      if vMP3Tag = 'TAG' then begin
         vIsTag := True;
         vTitle := BufStr(3, 30);
         vArtist := BufStr(33, 30);
         vAlbum := BufStr(63, 30);
         vComment := BufStr(97, 30);
         vYear := BufStr(93, 4);
         vGenreID := Ord(id3[127]);
         if vGenreID > MAXGENRES then vGenre := ''
         else vGenre := GENRES[vGenreID];
         if Assigned(onChangeFile) then onChangeFile(Self);
      end
      else
         vIsTag := False;
   end
   else
   begin
      vIsTag := False;
      vError := 'File doesn''t exist !';
      if Assigned(onError) then onError(Self);
   end;
end;

procedure TMP3Edit.Save;

  procedure InsertToArray(Source: String; var Destination: array of char; Index: Integer);
  var i: Integer;
  begin
    for i := 0 to Length(Source) - 1 do begin
      Destination[Index + i] := Source[i + 1];
    end;
  end;

var dat: File of Char;
    id3: Array [0..TAGLEN] of Char;
begin
   if FileExists(vFilename) then
   begin
      FillChar(id3, SizeOf(id3), Ord(' '));
      InsertToArray('TAG', id3, 0);
      InsertToArray(vTitle, id3, 3);
      InsertToArray(vArtist, id3, 33);
      InsertToArray(vAlbum, id3, 63);
      InsertToArray(vComment, id3, 97);
      InsertToArray(vYear, id3, 93);
      id3[127] := Chr(vGenreID);

      AssignFile(dat, vFilename);
      Reset(dat);
      if vIsTag then
        Seek(dat, FileSize(dat) - 128)
      else
        Seek(dat, FileSize(dat));
      BlockWrite(dat, id3, 128);
      vSize := IntToStr(FileSize(dat)) + ' bytes';
      CloseFile(dat);
      vIsTag := True;
   end
   else
   begin
      vIsTag := False;
      vError := 'File doesn''t exist !';
      if Assigned(onError) then onError(Self);
   end;
end;

procedure TMP3Edit.Remove;
var dat: File of Char;
begin
  if FileExists(vFilename) and vIsTag then
  begin
    AssignFile(dat, vFileName);
    Reset(dat);
    Seek(dat, FileSize(dat) - 128);
    Truncate(dat);
    vSize := IntToStr(FileSize(dat)) + ' bytes';
    CloseFile(dat);
    vTitle := ''; vArtist := ''; vAlbum := '';
    vComment := ''; vYear := ''; vGenreID := 255;
    vGenre := '';
    vIsTag := False;
  end
  else
  begin
    vIsTag := False;
    vError := 'File doesn''t exist !';
    if Assigned(onError) then onError(Self);
  end;
end;

procedure Register;
begin
  RegisterComponents('Samples', [TMP3Edit]);
end;

end.
