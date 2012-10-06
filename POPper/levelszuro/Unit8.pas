unit Unit8;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TframeSzuro = class(TFrame)
    Label1: TLabel;
    cmbMicsoda: TComboBox;
    cmbMuvelet: TComboBox;
    Label2: TLabel;
    txtMit: TEdit;
    Label3: TLabel;
    cmbMitcsinaljon: TComboBox;
    Label4: TLabel;
    cmbMitmodositson: TComboBox;
    Label5: TLabel;
    txtMivelmodositson: TEdit;
    cmdMentes: TButton;
    cmdTorles: TButton;
    cmdHozzaadas: TButton;
    procedure cmbMitcsinaljonChange(Sender: TObject);
    procedure Initialize;
    procedure cmdHozzaadasClick(Sender: TObject);
    procedure cmdTorlesClick(Sender: TObject);
  private
    fIndex: integer;
  public
    property FilterIndex: integer read fIndex write fIndex;
  end;

implementation

{$R *.DFM}

procedure TframeSzuro.cmbMitcsinaljonChange(Sender: TObject);
begin
 if cmbMitcsinaljon.Items[cmbMitCsinaljon.ItemIndex] <> 'töröld' then
 begin
  Label4.Visible := true;
  cmbMitmodositson.Visible := true;
  Label5.Visible := true;
  txtMivelmodositson.Visible := true;
 end
 else
 begin
  Label4.Visible := false;
  cmbMitmodositson.Visible := false;
  Label5.Visible := false;
  txtMivelmodositson.Visible := false;
 end;
end;

procedure TframeSzuro.Initialize;
begin
 cmbMicsoda.ItemIndex := 0;
 cmbMuvelet.ItemIndex := 0;
 cmbMitcsinaljon.ItemIndex := 0;
 cmbMitcsinaljonChange(Self);
end;

procedure TframeSzuro.cmdHozzaadasClick(Sender: TObject);
begin
 cmbMicsoda.Enabled := true;
 cmbMuvelet.Enabled := true;
 txtMit.Enabled := true;
 cmbMitcsinaljon.Enabled := true;
 cmbMitmodositson.Enabled := true;
 txtMivelmodositson.Enabled := true;
 cmdMentes.Enabled := true;
 cmdTorles.Enabled := true;
 cmdHozzaadas.Enabled := false;
end;

procedure TframeSzuro.cmdTorlesClick(Sender: TObject);
begin
 cmbMicsoda.Enabled := false;
 cmbMuvelet.Enabled := false;
 txtMit.Enabled := false;
 cmbMitcsinaljon.Enabled := false;
 cmbMitmodositson.Enabled := false;
 txtMivelmodositson.Enabled := false;
 cmdMentes.Enabled := false;
 cmdTorles.Enabled := false;
 cmdHozzaadas.Enabled := true;
end;

end.
