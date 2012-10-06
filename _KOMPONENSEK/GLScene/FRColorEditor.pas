{: FRColorEditor<p>

   RGB+Alpha color editor.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRColorEditor;

interface

uses 
  Windows, Forms, StdCtrls, ComCtrls, ExtCtrls, FRTrackBarEdit, Dialogs, Controls,
  Classes, Geometry;

type
  TRColorEditor = class(TFrame)
    Label1: TLabel;
    TBERed: TRTrackBarEdit;
    TBEGreen: TRTrackBarEdit;
    Label2: TLabel;
    TBEBlue: TRTrackBarEdit;
    Label3: TLabel;
    TBEAlpha: TRTrackBarEdit;
    Label4: TLabel;
    PAPreview: TPanel;
    ColorDialog: TColorDialog;
    procedure TBEChange(Sender: TObject);
    procedure TBERedTrackBarChange(Sender: TObject);
    procedure TBEGreenTrackBarChange(Sender: TObject);
    procedure TBEBlueTrackBarChange(Sender: TObject);
    procedure TBEAlphaTrackBarChange(Sender: TObject);
    procedure PAPreviewDblClick(Sender: TObject);
  private
    { Déclarations privées }
    FOnChange : TNotifyEvent;
    updating : Boolean;
    procedure SetColor(const val : THomogeneousFltVector);
    function GetColor : THomogeneousFltVector;
  public
    { Déclarations publiques }
    property Color : THomogeneousFltVector read GetColor write SetColor;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.DFM}

uses Graphics, GLTexture;

procedure TRColorEditor.TBEChange(Sender: TObject);
begin
   PAPreview.Color:=RGB(TBERed.Value, TBEGreen.Value, TBEBlue.Value);
   if (not updating) and Assigned(FOnChange) then FOnChange(Self);
end;

// SetColor
//
procedure TRColorEditor.SetColor(const val : THomogeneousFltVector);
begin
   updating:=True;
   try
      TBERed.Value:=Round(val[0]*255);
      TBEGreen.Value:=Round(val[1]*255);
      TBEBlue.Value:=Round(val[2]*255);
      TBEAlpha.Value:=Round(val[3]*1000);
   finally
      updating:=False;
   end;
   TBEChange(Self);
end;

// GetColor
//
function TRColorEditor.GetColor : THomogeneousFltVector;
begin
   Result:=VectorMake(TBERed.Value/255, TBEGreen.Value/255, TBEBlue.Value/255,
                      TBEAlpha.Value/1000);
end;

procedure TRColorEditor.TBERedTrackBarChange(Sender: TObject);
begin
   TBERed.TrackBarChange(Sender);
   TBEChange(Sender);
end;

procedure TRColorEditor.TBEGreenTrackBarChange(Sender: TObject);
begin
   TBEGreen.TrackBarChange(Sender);
   TBEChange(Sender);
end;

procedure TRColorEditor.TBEBlueTrackBarChange(Sender: TObject);
begin
   TBEBlue.TrackBarChange(Sender);
   TBEChange(Sender);
end;

procedure TRColorEditor.TBEAlphaTrackBarChange(Sender: TObject);
begin
   TBEAlpha.TrackBarChange(Sender);
   if (not updating) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TRColorEditor.PAPreviewDblClick(Sender: TObject);
begin
   ColorDialog.Color:=PAPreview.Color;
   if ColorDialog.Execute then
      SetColor(ConvertWinColor(ColorDialog.Color));
end;

end.
