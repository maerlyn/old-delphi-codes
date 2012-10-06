unit DTT_Input;

interface

type
  TMouse = record
    X, Y: Integer;
    dX, dY: Integer;
    Left, Middle, Right: Boolean;
  end;

  TKeyboard = record
    Character: Char;
    NewKey: Integer;
    Key: array [0..321] of Boolean;
  end;

  TInput = class
  public
    Mouse: TMouse;
    Keyboard: TKeyboard;
    constructor Create;
    procedure SetMouse(posX, posY: Integer);
    procedure SetButton(Button: Byte);
    procedure SetChar(Ch: Char);
    procedure SetKey(KeyPress: Word);
    procedure ResetKey(KeyPress: Word);
  end;

var
  Input: TInput;

implementation

constructor TInput.Create;
var
  i: Integer;
begin
  inherited Create;
  with Mouse do
  begin
    Left := False;
    Middle := False;
    Right := False;
  end;
  with Keyboard do
  begin
    Character := #0;
    NewKey := 0;
    for i := 0 to 321 do
      Key[i] := False;
  end;
end;

procedure TInput.SetMouse(posX, posY: Integer);
begin
  with Mouse do
  begin
    dX := posX-X;
    dY := posY-Y;
    X := posX;
    Y := posY;
  end;
end;

procedure TInput.SetButton(Button: Byte);
begin
  with Mouse do
  begin
    Left := Button = 1;
    Middle := Button = 3;
    Right := Button = 2;
  end;
end;

procedure TInput.SetChar(Ch: Char);
begin
  Keyboard.Character := Ch;
end;

procedure TInput.SetKey(KeyPress: Word);
begin
  Keyboard.Key[KeyPress] := True;
  Keyboard.NewKey := KeyPress;
end;

procedure TInput.ResetKey(KeyPress: Word);
begin
  Keyboard.Key[KeyPress] := False;
end;

initialization

  Input := TInput.Create;

finalization

  Input.Free;

end.

