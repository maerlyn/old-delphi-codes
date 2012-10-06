unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, Clipbrd, Unit2, ImgList, Registry;

type
  TForm1 = class(TForm)
    mnuMainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveas: TMenuItem;
    N1: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuEditCut: TMenuItem;
    mnuEditCopy: TMenuItem;
    mnuEditPaste: TMenuItem;
    N2: TMenuItem;
    mnuEditFont: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    txtSzovegmezo: TMemo;
    mnuPopupMenu1: TPopupMenu;
    mnuPopupNew: TMenuItem;
    mnuPopupOpen: TMenuItem;
    mnuPopupSave: TMenuItem;
    mnuPopupSaveas: TMenuItem;
    N3: TMenuItem;
    mnuPopupExit: TMenuItem;
    dlgOpenDialog: TOpenDialog;
    dlgSaveDialog: TSaveDialog;
    dlgFontDialog: TFontDialog;
    txtIdeiglenesSzovegmezo: TMemo;
    ImageList1: TImageList;
    mnuSearchSearch: TMenuItem;
    mnuSearchSearchagain: TMenuItem;
    dlgFindDialog: TFindDialog;
    procedure mnuFileNewClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure txtSzovegmezoChange(Sender: TObject);
    procedure mnuFileSaveasClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuEditCutClick(Sender: TObject);
    procedure mnuEditCopyClick(Sender: TObject);
    procedure mnuEditPasteClick(Sender: TObject);
    procedure mnuEditFontClick(Sender: TObject);
    procedure mnuHelpAboutClick(Sender: TObject);
    procedure mnuPopupNewClick(Sender: TObject);
    procedure mnuPopupOpenClick(Sender: TObject);
    procedure mnuPopupSaveClick(Sender: TObject);
    procedure mnuPopupSaveasClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuSearchSearchClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Modositott : Boolean;
  FileNeve : String;
  FileValtozo : TextFile;
  KeresendoSzoveg : String;
implementation

{$R *.DFM}

procedure TForm1.mnuFileNewClick(Sender: TObject);
var Valasz : integer;
begin
 if Modositott then begin
                    Valasz := Application.MessageBox('A file-t módosítottad. Mented?','Szövegszerkesztõ',MB_YESNOCANCEL+MB_DEFBUTTON3);
                    if Valasz = IDCANCEL then Exit;
                    if Valasz = IDYES then begin
                                           if FileNeve = EmptyStr then begin
                                                                       dlgSaveDialog.Execute;
                                                                       FileNeve := dlgSaveDialog.FileName;
                                                                       if FileNeve = EmptyStr then Exit;
                                                                       end;
                                           txtSzovegmezo.Lines.SaveToFile(FileNeve);
                    			   end;
 		    end;
 txtSzovegmezo.Text := EmptyStr;
 FileNeve := EmptyStr;
 Modositott := False;
 Form1.Caption := 'Névtelen - Szövegszerkesztõ';
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 txtSzovegmezo.Top := 0;
 txtSzovegmezo.Left:= 0;
 txtSzovegmezo.Width := Form1.ClientWidth;
 txtSzovegmezo.Height := Form1.ClientHeight;
end;

procedure TForm1.mnuFileOpenClick(Sender: TObject);
Var Valasz : Integer;
begin
 if Modositott then begin
                    Valasz := Application.MessageBox('A file-t módosítottad. Mented?','Szövegszerkesztõ',MB_YESNOCANCEL+MB_DEFBUTTON3);
                    if Valasz = IDCANCEL then Exit;
                    if Valasz = IDYES then begin
                        		   if FileNeve = EmptyStr then begin
                                                                       dlgSaveDialog.Execute;
                                                                       FileNeve := dlgSaveDialog.FileName;
                                                                       if FileNeve = EmptyStr then Exit;
                                                                       end;
                                           txtSzovegmezo.Lines.SaveToFile(FileNeve);
                                           Modositott := False;
                                           end;
                    end;
 txtSzovegmezo.Text := EmptyStr;
 dlgOpenDialog.Execute;
 FileNeve := dlgOpenDialog.FileName;
 if FileNeve = EmptyStr then Exit;
 txtSzovegmezo.Lines.LoadFromFile(FileNeve);
 Form1.Caption := FileNeve + ' - Szövegszerkesztõ';
end;

procedure TForm1.mnuFileSaveClick(Sender: TObject);
begin
 if FileNeve = EmptyStr then begin
                             dlgSaveDialog.Execute;
                             FileNeve := dlgSaveDialog.FileName;
                             if FileNeve = EmptyStr then Exit;
                             end;
 txtSzovegmezo.Lines.SaveToFile(FileNeve);
end;

procedure TForm1.txtSzovegmezoChange(Sender: TObject);
begin
 Modositott := True;
end;

procedure TForm1.mnuFileSaveasClick(Sender: TObject);
begin
 dlgSaveDialog.Execute;
 FileNeve := dlgSaveDialog.FileName;
 if FileNeve = EmptyStr then Exit;
 txtSzovegmezo.Lines.SaveToFile(FileNeve);
 Modositott := False;
end;

procedure TForm1.mnuEditClick(Sender: TObject);
begin
 if txtSzovegmezo.SelLength = 0 then begin
                                     mnuEditCut.Enabled := False;
                                     mnuEditCopy.Enabled := False;
                                     end
 else begin
      mnuEditCut.Enabled := True;
      mnuEditCopy.Enabled := True;
      end;
// mnuEditPaste.Enabled := SendMessage(txtSzovegmezo.Handle,em_CanPaste,0,0)<>0;
  mnuEditPaste.Enabled := ClipBoard.AsText <> '';
// else mnuEditPaste.Enabled := False;
end;

procedure TForm1.mnuEditCutClick(Sender: TObject);
begin
 txtSzovegmezo.CutToClipboard;
end;

procedure TForm1.mnuEditCopyClick(Sender: TObject);
begin
 txtSzovegmezo.CopyToClipboard;
end;

procedure TForm1.mnuEditPasteClick(Sender: TObject);
begin
 txtSzovegmezo.PasteFromClipboard;
end;

procedure TForm1.mnuEditFontClick(Sender: TObject);
begin
 dlgFontDialog.Font := txtSzovegmezo.Font;
 dlgFontDialog.Execute;
 txtSzovegmezo.Font := dlgFontDialog.Font;
end;

procedure TForm1.mnuHelpAboutClick(Sender: TObject);
begin
 frmNevjegy.Show;
end;

procedure TForm1.mnuPopupNewClick(Sender: TObject);
var Valasz : integer;
begin
 if Modositott then begin
                    Valasz := Application.MessageBox('A file-t módosítottad. Mented?','Szövegszerkesztõ',MB_YESNOCANCEL+MB_DEFBUTTON3);
                    if Valasz = IDCANCEL then Exit;
                    if Valasz = IDYES then begin
                                           if FileNeve = EmptyStr then begin
                                                                       dlgSaveDialog.Execute;
                                                                       FileNeve := dlgSaveDialog.FileName;
                                                                       if FileNeve = EmptyStr then Exit;
                                                                       end;
                                           txtSzovegmezo.Lines.SaveToFile(FileNeve);
                    			   end;
 		    end;
 txtSzovegmezo.Text := EmptyStr;
 FileNeve := EmptyStr;
 Modositott := False;
 Form1.Caption := 'Névtelen - Szövegszerkesztõ';
end;

procedure TForm1.mnuPopupOpenClick(Sender: TObject);
Var Valasz : Integer;
begin
 if Modositott then begin
                    Valasz := Application.MessageBox('A file-t módosítottad. Mented?','Szövegszerkesztõ',MB_YESNOCANCEL+MB_DEFBUTTON3);
                    if Valasz = IDCANCEL then Exit;
                    if Valasz = IDYES then begin
                        		   if FileNeve = EmptyStr then begin
                                                                       dlgSaveDialog.Execute;
                                                                       FileNeve := dlgSaveDialog.FileName;
                                                                       if FileNeve = EmptyStr then Exit;
                                                                       end;
                                           txtSzovegmezo.Lines.SaveToFile(FileNeve);
                                           Modositott := False;
                                           end;
                    end;
 txtSzovegmezo.Text := EmptyStr;
 dlgOpenDialog.Execute;
 FileNeve := dlgOpenDialog.FileName;
 if FileNeve = EmptyStr then Exit;
 txtSzovegmezo.Lines.LoadFromFile(FileNeve);
 Form1.Caption := FileNeve + ' - Szövegszerkesztõ';
end;

procedure TForm1.mnuPopupSaveClick(Sender: TObject);
begin
 if FileNeve = EmptyStr then begin
                             dlgSaveDialog.Execute;
                             FileNeve := dlgSaveDialog.FileName;
                             if FileNeve = EmptyStr then Exit;
                             end;
 txtSzovegmezo.Lines.SaveToFile(FileNeve);
end;

procedure TForm1.mnuPopupSaveasClick(Sender: TObject);
begin
 dlgSaveDialog.Execute;
 FileNeve := dlgSaveDialog.FileName;
 if FileNeve = EmptyStr then Exit;
 txtSzovegmezo.Lines.SaveToFile(FileNeve);
 Modositott := False;
end;

procedure TForm1.mnuFileExitClick(Sender: TObject);
Var Valasz : Integer;
begin
 Valasz := Application.MessageBox('Tényleg ki akarsz lépni?','Szövegszerkesztõ',MB_YESNO+MB_DEFBUTTON2);
 if Valasz = IDYES then begin
                        if Modositott then begin
                                           Valasz := Application.Messagebox('A file megváltozott. Mented?','Szövegszerkesztõ',MB_YESNOCANCEL+MB_DEFBUTTON3);
                                           case Valasz of
                                           IDYES: begin
                                                  if FileNeve = EmptyStr then begin
                                                                              dlgSaveDialog.Execute;
                                                                              if FileNeve = EmptyStr then Exit;
                                                                              FileNeve := dlgSaveDialog.FileName;
                                                                              end;
                                                  txtSzovegmezo.Lines.SaveToFile(FileNeve);
                                                  end;
                                           IDCANCEL: Exit;
                                           end;
                                           end;
                        Halt;
                        end;

 if Valasz = IDNO then Exit;
end;

procedure TForm1.mnuSearchSearchClick(Sender: TObject);
var Hely : Longint;
begin
 if dlgFindDialog.Execute then begin
                               KeresendoSzoveg := dlgFindDialog.FindText;
                               Hely := Pos(KeresendoSzoveg, txtSzovegmezo.Text);
                               if Hely = 0 then begin
                                                Application.MessageBox('Nem találtam egyezõt','Keresés',MB_OK+MB_ICONEXCLAMATION);
                                                Exit;
                                                end;
                               txtSzovegmezo.SelStart := Hely;
                               txtSzovegmezo.SelLength := Length(KeresendoSzoveg);
                               end;
end;


end.
