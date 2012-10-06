unit NewImageUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type
  TNewImageForm = class(TForm)
    Label1: TLabel;
    ImageWidth: TEdit;
    UpDown1: TUpDown;
    Label2: TLabel;
    ImageHeight: TEdit;
    UpDown2: TUpDown;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ColorDialog1: TColorDialog;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewImageForm: TNewImageForm;

implementation

{$R *.DFM}

procedure TNewImageForm.Button1Click(Sender: TObject);
begin
  with ColorDialog1 do
  begin
    Color := Panel1.Color;
    if Execute then Panel1.Color := Color;
  end;
end;

end.
