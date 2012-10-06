unit MAIN3;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, fglib;

type
  TForm1 = class(TForm)
    MiniCalendar1: TMiniCalendar;
    DigitalClock1: TDigitalClock;
    AnalogicClock1: TAnalogicClock;
    CalendarPad1: TCalendarPad;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations private }
    dir : string;
  public
    { Déclarations public }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  getdir(0,dir);
  dir := dir+'\';
  CalendarPad1.directory := dir;
  CalendarPad1.SoundFileName := dir+'paper.wav';
end;

end.
