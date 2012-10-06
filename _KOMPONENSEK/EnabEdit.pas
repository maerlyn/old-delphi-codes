unit EnabEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TEnabEdit = class(TEdit)
  private
    fEnabChars: set of Char;
    procedure SetEnabChars(Value: string);
    function GetEnabChars: string;
  protected
    procedure KeyPress(var Key: char);override;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property EnabledChars: string read GetEnabChars write SetEnabChars;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Md', [TEnabEdit]);
end;

{ TEnabEdit }

constructor TEnabEdit.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 fEnabChars := [];
end;

function TEnabEdit.GetEnabChars: string;
var i: integer;
begin
 Result := '';
 for i := 0 to 255 do
  if Chr(i) in fEnabChars then Result := Result + Chr(i);
end;

procedure TEnabEdit.KeyPress(var Key: char);
begin
 if (Key in fEnabChars) or (Key = #8) then
  inherited KeyPress(Key)
 else
  begin
   Key := #0;
   Beep;
  end; 
end;

procedure TEnabEdit.SetEnabChars(Value: string);
var i: integer;
begin
 fEnabChars := [];
 for i := 0 to Length(Value) do
  fEnabChars := fEnabChars + [Value[i]];
end;

end.
