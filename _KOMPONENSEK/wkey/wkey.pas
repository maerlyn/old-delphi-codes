{
--------------------------------------------------------------------------------

Component Name: TWKey
Author:       Mats Asplund
Creation:     July 24, 2000
Version:      2.0
Description:  A component that stops program execution
              and waits for a single keystroke.
Credit:
EMail:        mats.asplund@telia.com
Site:         http://w1.545.telia.com/~u54503556/delphi/mdp.htm
Legal issues: Copyright (C) 2000 by Mats Asplund
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. If you decide to use this software in any of your applications.
                 Send me an EMail address and tell me about it.

Quick Reference:
              TWKey is a descendant from TComponent.

              Published properties:
                KeyToPress:      The key (a-z, AnyKey) that starts program
                                 execution.
                ShowMessage:     If true a message is shown when wait is called.
                MessagePosition: Screen-position for message.
                MessageColor:    FaceColor of the message.

              Methods:
                Wait:            Stops program execution and waits for a key to
                                 be pressed.
--------------------------------------------------------------------------------
}
unit wkey;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, FileCtrl, ComCtrls;

type
  TKeyIn = (AnyKey,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z);
  TWKey = class(TComponent)
  private
    DForm: TForm;
    DLabel: TLabel;
    FKey: TKeyIn;
    PKey: string;
    FCop: string;
    FShow: Boolean;
    FPosition: TPosition;
    FColor: TColor;
    procedure SetKey(Value: TKeyIn);
    procedure KeyPressed(Sender: TObject; var Key: char);
    procedure SetShowMessage(Value: Boolean);
    procedure SetMessagePosition(Value: TPosition);
    procedure SetMessageColor(Value: TColor);
    procedure SetCop(Value: string);
    function GetCop: string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    procedure Wait;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  published
    property KeyToPress: TKeyIn read FKey write SetKey;
    property ShowMessage: Boolean read FShow write SetShowMessage;
    property MessagePosition: TPosition read FPosition write SetMessagePosition;
    property MessageColor: TColor read FColor write SetMessageColor;
    property Copyright: string read GetCop write SetCop;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Vingelmann', [TWKey]);
end;

constructor TWKey.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DForm:= TForm.Create(Self);
  DLabel:= TLabel.Create(Self);
  DLabel.Parent:= DForm;
  DForm.Color:= clYellow;
  DForm.Position:= poScreenCenter;
  DForm.Borderstyle:= bsNone;
  DLabel.Top:= 5;
  DLabel.Left:= 5;
  DForm.OnKeyPress:= KeyPressed;
  FKey:= AnyKey;
  FShow:= true;
  FColor:= clYellow;
  FPosition:= poMainFormCenter;
  FCop:= '2000 (C), Mats Asplund / MAs Prod.';
end;

procedure TWKey.KeyPressed(Sender: TObject; var Key: char);
var OK: Boolean;
begin
  OK:= false;
  case FKey of
    AnyKey: OK:= true;
    a: if Key = 'a' then OK:= true;
    b: if Key = 'b' then OK:= true;
    c: if Key = 'c' then OK:= true;
    d: if Key = 'd' then OK:= true;
    E: if Key = 'e' then OK:= true;
    F: if Key = 'f' then OK:= true;
    g: if Key = 'g' then OK:= true;
    h: if Key = 'h' then OK:= true;
    i: if Key = 'i' then OK:= true;
    j: if Key = 'j' then OK:= true;
    k: if Key = 'k' then OK:= true;
    l: if Key = 'l' then OK:= true;
    m: if Key = 'm' then OK:= true;
    n: if Key = 'n' then OK:= true;
    o: if Key = 'o' then OK:= true;
    p: if Key = 'p' then OK:= true;
    q: if Key = 'q' then OK:= true;
    r: if Key = 'r' then OK:= true;
    S: if Key = 's' then OK:= true;
    t: if Key = 't' then OK:= true;
    u: if Key = 'u' then OK:= true;
    v: if Key = 'v' then OK:= true;
    w: if Key = 'w' then OK:= true;
    x: if Key = 'x' then OK:= true;
    y: if Key = 'y' then OK:= true;
    z: if Key = 'z' then OK:= true;
  end;
  if OK then DForm.ModalResult:= mrOK;
end;

destructor TWKey.Destroy;
begin
  DLabel.Free;
  DForm.Free;
  inherited Destroy;
end;

procedure TWKey.Wait;
begin
  if FShow then
  begin
    DForm.Width:= 125;
    DForm.Height:= 24;
    if FKey = AnyKey then DLabel.Caption:= 'Press a key to continue.' else
      DLabel.Caption:= 'Press ' + PKey + ' to continue.';
  end
  else
  begin
    DForm.Width:= 0;
    DForm.Height:= 0;
  end;
  if DForm.ShowModal = mrOK then
end;

procedure TWKey.SetKey(Value: TKeyIn);
begin
  FKey:= Value;
  case FKey of
    AnyKey: PKey:= 'a key';
    a: PKey:= '<a>';
    b: PKey:= '<b>';
    c: PKey:= '<c>';
    d: PKey:= '<d>';
    E: PKey:= '<e>';
    F: PKey:= '<f>';
    g: PKey:= '<g>';
    h: PKey:= '<h>';
    i: PKey:= '<i>';
    j: PKey:= '<j>';
    k: PKey:= '<k>';
    l: PKey:= '<l>';
    m: PKey:= '<m>';
    n: PKey:= '<n>';
    o: PKey:= '<o>';
    p: PKey:= '<p>';
    q: PKey:= '<q>';
    r: PKey:= '<r>';
    S: PKey:= '<s>';
    t: PKey:= '<t>';
    u: PKey:= '<u>';
    v: PKey:= '<v>';
    w: PKey:= '<w>';
    x: PKey:= '<x>';
    y: PKey:= '<y>';
    z: PKey:= '<z>';
  end;
end;

function TWKey.GetCop: string;
begin
  Result:= FCop;
end;

procedure TWKey.SetCop(Value: string);
begin
  FCop:= FCop;
end;

procedure TWKey.SetShowMessage(Value: Boolean);
begin
  FShow:= Value;
end;

procedure TWKey.SetMessagePosition(Value: TPosition);
begin
  FPosition:= Value;
  DForm.Position:= FPosition;
end;

procedure TWKey.SetMessageColor(Value: TColor);
begin
  FColor:= Value;
  DForm.Color:= FColor;
end;

end.

