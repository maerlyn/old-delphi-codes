{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1997 Master-Bank                }
{                                                       }
{*******************************************************}

unit RXColors;

{$C PRELOAD}
{$I RX.INC}

interface

uses Classes, Controls, Graphics, Forms, VCLUtils;

function RxColorToString(Color: TColor): string;
function RxStringToColor(S: string): TColor;
procedure RxGetColorValues(Proc: TGetStrProc);

procedure RegisterRxColors;

implementation

uses SysUtils, DsgnIntf;

type
  TColorEntry = record
    Value: TColor;
    Name: PChar;
  end;

const
{$IFDEF WIN32}
  clInfoBk16 = TColor($02E1FFFF);
  clNone16 = TColor($02FFFFFF);
  ColorCount = 3;
{$ELSE}
  ColorCount = 5;
{$ENDIF}
  Colors: array[0..ColorCount - 1] of TColorEntry = (
{$IFNDEF WIN32}
    (Value: clInfoBk;      Name: 'clInfoBk'),
    (Value: clNone;        Name: 'clNone'),
{$ENDIF}
    (Value: clCream;       Name: 'clCream'),
    (Value: clMoneyGreen;  Name: 'clMoneyGreen'),
    (Value: clSkyBlue;     Name: 'clSkyBlue'));

function RxColorToString(Color: TColor): string;
var
  I: Integer;
begin
  if not ColorToIdent(Color, Result) then begin
    for I := Low(Colors) to High(Colors) do
      if Colors[I].Value = Color then
      begin
        Result := StrPas(Colors[I].Name);
        Exit;
      end;
    FmtStr(Result, '$%.8x', [Color]);
  end;
end;

function RxStringToColor(S: string): TColor;
var
  I: Integer;
  Text: array[0..63] of Char;
begin
  StrPLCopy(Text, S, SizeOf(Text) - 1);
  for I := Low(Colors) to High(Colors) do
    if StrIComp(Colors[I].Name, Text) = 0 then
    begin
      Result := Colors[I].Value;
      Exit;
    end;
  Result := StringToColor(S);
end;

procedure RxGetColorValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  GetColorValues(Proc);
  for I := Low(Colors) to High(Colors) do Proc(StrPas(Colors[I].Name));
end;

{ TRxColorProperty }

type
  TRxColorProperty = class(TColorProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

function TRxColorProperty.GetValue: string;
var
  Color: TColor;
begin
  Color := TColor(GetOrdValue);
{$IFDEF WIN32}
  if Color = clNone16 then Color := clNone
  else if Color = clInfoBk16 then Color := clInfoBk;
{$ENDIF}
  Result := RxColorToString(Color);
end;

procedure TRxColorProperty.GetValues(Proc: TGetStrProc);
begin
  RxGetColorValues(Proc);
end;

procedure TRxColorProperty.SetValue(const Value: string);
begin
  SetOrdValue(RxStringToColor(Value));
end;

procedure RegisterRxColors;
begin
  RegisterPropertyEditor(TypeInfo(TColor), TPersistent, '', TRxColorProperty);
end;

end.