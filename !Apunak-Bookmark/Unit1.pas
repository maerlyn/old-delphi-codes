unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ImgList, Menus, Db, ADODB, Registry, Unit2;

type
  TfrmMainForm = class(TForm)
    TreeView1: TTreeView;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    mnuUjAlbejegyzes: TMenuItem;
    mnuBejegyzesTorlese: TMenuItem;
    mnuUjBejegyzes: TMenuItem;
    N1: TMenuItem;
    mnuModositas: TMenuItem;
    ImageList1: TImageList;
    procedure mnuUjBejegyzesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddSubnode(Node: TTreeNode; Text: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mnuModositasClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure mnuUjAlbejegyzesClick(Sender: TObject);
    procedure mnuBejegyzesTorleseClick(Sender: TObject);
    procedure TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure UpdatePictures;
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;
  Node: TTreeNode;

implementation

{$R *.DFM}

procedure TfrmMainForm.mnuUjBejegyzesClick(Sender: TObject);
begin
 TreeView1.Items.Add(nil,'új@új');
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
 OpenDialog1.Filter := 'Fileok|*.txt';
 if not OpenDialog1.Execute then Application.Terminate;
 TreeView1.LoadFromFile(OpenDialog1.FileName);
 UpdatePictures;
end;

procedure TfrmMainForm.AddSubnode(Node: TTreeNode; Text: string);
begin
 TreeView1.Items.AddChild(Node,Text);
end;

procedure TfrmMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var i: integer;
begin
 CanClose := true;
 i := Application.MessageBox('Akarod menteni a változtatásokat?','Kérdés',mb_yesNoCancel + mb_IconQuestion);
 if i = id_Cancel then
  CanClose := false;

 if i = id_Yes then
  TreeView1.SaveToFile(OpenDialog1.FileName);
end;

procedure TfrmMainForm.mnuModositasClick(Sender: TObject);
var i: integer;
begin
 if Node = nil then Abort;
 frmModositas.txtNev.Text := Copy(Node.Text,1,Pos('@',Node.Text)-1);
 frmModositas.txtCim.Text := Copy(Node.Text,Pos('@',Node.Text)+1,Length(Node.Text));
 i := frmModositas.ShowModal;
 if i = mrCancel then Exit;
 Node.Text := frmModositas.txtNev.Text + '@' + frmModositas.txtCim.Text;
end;

procedure TfrmMainForm.TreeView1DblClick(Sender: TObject);
var Pt: TPoint;
    i: integer;
begin
 GetCursorPos(Pt);
 Pt := ScreenToClient(Pt);
 Node := TreeView1.GetNodeAt(Pt.x,Pt.y);
 frmModositas.txtNev.Text := Copy(Node.Text,1,Pos('@',Node.Text) - 1);
 frmModositas.txtCim.Text := Copy(Node.Text,Pos('@',Node.Text) + 1,Length(Node.Text));
 i := frmModositas.ShowModal;
 if i = mrCancel then Exit;
 Node.Text := frmModositas.txtNev.Text + '@' + frmModositas.txtCim.Text;
end;

procedure TfrmMainForm.mnuUjAlbejegyzesClick(Sender: TObject);
begin
 if Node = nil then Exit;
 TreeView1.Items.AddChild(Node,'új@új');
end;

procedure TfrmMainForm.mnuBejegyzesTorleseClick(Sender: TObject);
begin
 if Node = nil then Abort;
 if Application.MessageBox('Tényleg törlöd?','Kérdés',mb_YesNo + mb_IconQuestion) = id_No then
  Exit;
 TreeView1.Items.Delete(Node);
end;

procedure TfrmMainForm.TreeView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
 TargetNode, SourceNode: TTreeNode;
begin
 TargetNode := TreeView1.GetNodeAt (X, Y);
 if (Source = Sender) and (TargetNode <> nil) then
 begin
  Accept := True;
  SourceNode := TreeView1.Selected;
  while (TargetNode.Parent <> nil) and (TargetNode <> SourceNode) do
   TargetNode := TargetNode.Parent;
  if TargetNode = SourceNode then
   Accept := False;
 end
 else
  Accept := False;
end;

procedure TfrmMainForm.TreeView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
 TargetNode, SourceNode: TTreeNode;
begin
 TargetNode := TreeView1.GetNodeAt (X, Y);
 if TargetNode <> nil then
 begin
  SourceNode := TreeView1.Selected;
  SourceNode.MoveTo (TargetNode, naAddChildFirst);
  TargetNode.Expand (False);
  TreeView1.Selected := TargetNode;
 end;
end;

procedure TfrmMainForm.PopupMenu1Popup(Sender: TObject);
var pt: TPoint;
begin
 GetCursorPos(pt);
 pt := ScreenToClient(pt);
 Node := TreeView1.GetNodeAt(pt.x,pt.y);
end;

procedure TfrmMainForm.UpdatePictures;
var i: integer;
begin
 for i := 0 to TreeView1.Items.Count - 1 do
  if TreeView1.Items[i].HasChildren then
   TreeView1.Items[i].ImageIndex := 0
  else
   TreeView1.Items[i].ImageIndex := 1;

  for i := 0 to TreeView1.Items.Count - 1 do
   if (TreeView1.Items[i].HasChildren) and (TreeView1.Items[i].Expanded) then
    TreeView1.Items[i].ImageIndex := 3;
end;

procedure TfrmMainForm.TreeView1Click(Sender: TObject);
begin
 UpdatePictures;
end;

procedure TfrmMainForm.TreeView1Expanded(Sender: TObject; Node: TTreeNode);
begin
 if Node.HasChildren then
  Node.ImageIndex := 3;
end;

procedure TfrmMainForm.TreeView1Collapsed(Sender: TObject;
  Node: TTreeNode);
begin
 if Node.HasChildren then
  Node.ImageIndex := 0;
end;

end.

