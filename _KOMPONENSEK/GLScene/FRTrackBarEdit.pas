{: FRTrackBarEdit<p>

   Frame combining a TrackBar and an Edit.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRTrackBarEdit;

interface

uses 
  Forms, StdCtrls, ComCtrls, Classes, Controls;

type
  TRTrackBarEdit = class(TFrame)
    TrackBar: TTrackBar;
    Edit: TEdit;
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    { Déclarations privées }
    procedure SetValue(const val : Integer);
    function GetValue : Integer;
    procedure SetValueMin(const val : Integer);
    function GetValueMin : Integer;
    procedure SetValueMax(const val : Integer);
    function GetValueMax : Integer;
  public
    { Déclarations publiques }
    property Value : Integer read GetValue write SetValue;
    property ValueMin : Integer read GetValueMin write SetValueMin;
    property ValueMax : Integer read GetValueMax write SetValueMax;
  end;

implementation

{$R *.DFM}

uses SysUtils;

procedure TRTrackBarEdit.TrackBarChange(Sender: TObject);
begin
   Edit.Text:=IntToStr(TrackBar.Position);
end;

procedure TRTrackBarEdit.EditChange(Sender: TObject);
var
   i : Integer;
begin
   try
      i:=StrToInt(Edit.Text);
      TrackBar.Position:=i;
   except
      // ignore
   end;
end;

// SetValue
//
procedure TRTrackBarEdit.SetValue(const val : Integer);
begin
   TrackBar.Position:=val;
end;

// GetValue
//
function TRTrackBarEdit.GetValue : Integer;
begin
   Result:=TrackBar.Position;
end;

// SetValueMax
//
procedure TRTrackBarEdit.SetValueMax(const val : Integer);
begin
   TrackBar.Max:=val;
end;

// GetValueMax
//
function TRTrackBarEdit.GetValueMax : Integer;
begin
   Result:=TrackBar.Max;
end;

// SetValueMin
//
procedure TRTrackBarEdit.SetValueMin(const val : Integer);
begin
   TrackBar.Min:=val;
end;

// GetValueMin
//
function TRTrackBarEdit.GetValueMin : Integer;
begin
   Result:=TrackBar.Min;
end;

end.
