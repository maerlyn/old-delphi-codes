unit FVectorEditor;

interface

uses
  Forms, ComCtrls, StdCtrls, ToolWin, ExtCtrls, Buttons, Graphics,
  Controls, Classes;

type
  TVectorEditorForm = class(TForm)
    EDx: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EDy: TEdit;
    EDz: TEdit;
    BBok: TBitBtn;
    BBcancel: TBitBtn;
    IMx: TImage;
    IMy: TImage;
    IMz: TImage;
    ToolBar: TToolBar;
    TBx: TToolButton;
    TBy: TToolButton;
    TBz: TToolButton;
    TBnull: TToolButton;
    ToolButton5: TToolButton;
    procedure TBxClick(Sender: TObject);
    procedure TByClick(Sender: TObject);
    procedure TBzClick(Sender: TObject);
    procedure TBnullClick(Sender: TObject);
    procedure EDxChange(Sender: TObject);
    procedure EDyChange(Sender: TObject);
    procedure EDzChange(Sender: TObject);
  private
    { Déclarations privées }
    vx, vy, vz : Single;
    procedure TestInput(edit : TEdit; imError : TImage; var dest : Single);
  public
    { Déclarations publiques }
    function Execute(var x, y, z : Single) : Boolean;
  end;

function VectorEditorForm : TVectorEditorForm;
procedure ReleaseVectorEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.DFM}

uses SysUtils;

var
	vVectorEditorForm : TVectorEditorForm;

function VectorEditorForm : TVectorEditorForm;
begin
	if not Assigned(vVectorEditorForm) then
      vVectorEditorForm:=TVectorEditorForm.Create(nil);
	Result:=vVectorEditorForm;
end;

procedure ReleaseVectorEditorForm;
begin
	if Assigned(vVectorEditorForm) then begin
	   vVectorEditorForm.Free; vVectorEditorForm:=nil;
	end;
end;

// Execute
//
function TVectorEditorForm.Execute(var x, y, z : Single) : Boolean;
begin
   // setup dialog fields
   vx:=x;
   vy:=y;
   vz:=z;
   EDx.Text:=FloatToStr(vx);
   EDy.Text:=FloatToStr(vy);
   EDz.Text:=FloatToStr(vz);
   // show the dialog
   Result:=(ShowModal=mrOk);
   if Result then begin
      x:=vx;
      y:=vy;
      z:=vz;
   end;
end;

procedure TVectorEditorForm.TestInput(edit : TEdit; imError : TImage; var dest : Single);
begin
   if Visible then begin
      try
         dest:=StrToFloat(edit.Text);
         imError.Visible:=False;
      except
         imError.Visible:=True;
      end;
      BBOk.Enabled:=not (IMx.Visible or IMy.Visible or IMz.Visible);
   end;
end;

procedure TVectorEditorForm.TBxClick(Sender: TObject);
begin
   EDx.Text:='1'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TByClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='1'; EDz.Text:='0';
end;

procedure TVectorEditorForm.TBzClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='1';
end;

procedure TVectorEditorForm.TBnullClick(Sender: TObject);
begin
   EDx.Text:='0'; EDy.Text:='0'; EDz.Text:='0';
end;

procedure TVectorEditorForm.EDxChange(Sender: TObject);
begin
   TestInput(EDx, IMx, vx);
end;

procedure TVectorEditorForm.EDyChange(Sender: TObject);
begin
   TestInput(EDy, IMy, vy);
end;

procedure TVectorEditorForm.EDzChange(Sender: TObject);
begin
   TestInput(EDz, IMz, vz);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

finalization

   ReleaseVectorEditorForm;

end.
