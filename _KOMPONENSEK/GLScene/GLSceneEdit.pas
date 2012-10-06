{: GLTexture<p>

   Handles all the color and texture stuff.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>06/08/00 - Egg - Added basic Clipboard support
      <li>14/05/00 - Egg - Added workaround for VCL DesignInfo bug (thx Nelson Chu)
      <li>28/04/00 - Egg - Fixed new objects not being immediately reco by IDE
      <li>26/04/00 - Egg - Added support for objects categories
		<li>17/04/00 - Egg - Added access to TInfoForm
		<li>16/04/00 - Egg - Fixed occasionnal crash when rebuilding GLScene dpk
									while GLSceneEdit is visible
      <li>10/04/00 - Egg - Minor Create/Release change
      <li>24/03/00 - Egg - Fixed SetScene not updating enablings
		<li>13/03/00 - Egg - Object names (ie. node text) is now properly adjusted
									when a GLScene object is renamed,
									Added Load/Save whole scene
      <li>07/02/00 - Egg - Fixed notification logic
      <li>06/02/00 - Egg - DragDrop now starts after moving the mouse a little,
                           Form is now auto-creating, fixed Notification,
                           Added actionlist and moveUp/moveDown
      <li>05/02/00 - Egg - Fixed DragDrop, added root nodes auto-expansion
   </ul></font>
}
unit GLSceneEdit;

interface

{$I DFS.INC}

uses
  Windows, Forms, ComCtrls, GLScene, Menus, ActnList, ToolWin, DsgnIntf,
  Controls, Classes, ImgList, Dialogs;

type

  TGLSceneEditorForm = class(TForm)
    Tree: TTreeView;
    PopupMenu: TPopupMenu;
    MIAddCamera: TMenuItem;
    MIAddObject: TMenuItem;
    N1: TMenuItem;
    MIDelObject: TMenuItem;
    ToolBar: TToolBar;
    ActionList: TActionList;
    ToolButton1: TToolButton;
    TBAddObjects: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PMToolBar: TPopupMenu;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ACAddCamera: TAction;
    ACAddObject: TAction;
    ImageList: TImageList;
    ACDeleteObject: TAction;
    ACMoveUp: TAction;
    ACMoveDown: TAction;
    N2: TMenuItem;
    Moveobjectup1: TMenuItem;
    Moveobjectdown1: TMenuItem;
    ACSaveScene: TAction;
    ACLoadScene: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolButton2: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ACInfo: TAction;
    ACCopy: TAction;
    ACCut: TAction;
    ACPaste: TAction;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Cut1: TMenuItem;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
	 procedure FormCreate(Sender: TObject);
    procedure TreeEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure TreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeEnter(Sender: TObject);
    procedure TreeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ACAddCameraExecute(Sender: TObject);
    procedure ACDeleteObjectExecute(Sender: TObject);
    procedure ACMoveUpExecute(Sender: TObject);
    procedure ACMoveDownExecute(Sender: TObject);
    procedure ACAddObjectExecute(Sender: TObject);
    procedure ACSaveSceneExecute(Sender: TObject);
    procedure ACLoadSceneExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACInfoExecute(Sender: TObject);
    procedure ACCopyExecute(Sender: TObject);
    procedure ACCutExecute(Sender: TObject);
    procedure ACPasteExecute(Sender: TObject);

  private
    FScene: TGLScene;
    FObjectNode, FCameraNode: TTreeNode;
    FCurrentDesigner: IFormDesigner;
    FLastMouseDownPos : TPoint;
	 procedure ReadScene;
    procedure ResetTree;
    function AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;
    procedure AddObjectClick(Sender: TObject);
	 procedure SetObjectsSubItems(parent : TMenuItem);
	 procedure OnBaseSceneObjectNameChanged(Sender : TObject);
    function IsValidClipBoardNode : Boolean;
    function IsPastePossible : Boolean;
    function CanPaste(obj, destination : TGLBaseSceneObject) : Boolean;
    function GetComponentFromClipBoard : TComponent;

  protected
	 procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure SetScene(Scene: TGLScene; Designer: IFormDesigner);

  end;

function GLSceneEditorForm : TGLSceneEditorForm;
procedure ReleaseGLSceneEditorForm;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R *.DFM}

uses GLSceneRegister, GLStrings, Info, OpenGL12, ClipBrd;

resourcestring
   cGLSceneEditor = 'GLScene Editor';

var
	vGLSceneEditorForm : TGLSceneEditorForm;

function GLSceneEditorForm : TGLSceneEditorForm;
begin
	if not Assigned(vGLSceneEditorForm) then
		vGLSceneEditorForm:=TGLSceneEditorForm.Create(nil);
	Result:=vGLSceneEditorForm;
end;

procedure ReleaseGLSceneEditorForm;
begin
	if Assigned(vGLSceneEditorForm) then begin
		vGLSceneEditorForm.Free; vGLSceneEditorForm:=nil;
   end;
end;

// FindNodeByData
//
function FindNodeByData(treeNodes : TTreeNodes; data : Pointer;
								baseNode : TTreeNode = nil) : TTreeNode;
var
	n : TTreeNode;
begin
	Result:=nil;
	if Assigned(baseNode) then begin
		n:=baseNode.getFirstChild;
		while Assigned(n) do begin
			if n.Data=data then begin
				Result:=n; Break;
			end else	if n.HasChildren then begin
				Result:=FindNodeByData(treeNodes, data, n);
				if Assigned(Result) then Break;
			end;
			n:=baseNode.GetNextChild(n);
		end;
	end else begin
		n:=treeNodes.GetFirstNode;
		while Assigned(n) do begin
			if n.Data=data then begin
				Result:=n; Break;
			end else	if n.HasChildren then begin
				Result:=FindNodeByData(treeNodes, data, n);
				if Assigned(Result) then Break;
			end;
			n:=n.getNextSibling;
		end;
	end;
end;

//----------------- TGLSceneEditorForm ---------------------------------------------------------------------------------

// SetScene
//
procedure TGLSceneEditorForm.SetScene(Scene: TGLScene; Designer: IFormDesigner);
begin
   if Assigned(FScene) then
{$ifdef DFS_DELPHI_5_UP}
		FScene.RemoveFreeNotification(Self);
{$else}
		FScene.Notification(Self, opRemove);
{$endif}
	FScene:=Scene;
   FCurrentDesigner:=Designer;
   ResetTree;
   if Assigned(FScene) then begin
      FScene.FreeNotification(Self);
      ReadScene;
      Caption:=cGLSceneEditor+' : '+FScene.Name;
   end else Caption:=cGLSceneEditor;
   TreeChange(Self, nil);
end;

// FormCreate
//
procedure TGLSceneEditorForm.FormCreate(Sender: TObject);
var
   CurrentNode: TTreeNode;
begin
	RegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
   Tree.Images:=ObjectManager.ObjectIcons;
   Tree.Indent:=ObjectManager.ObjectIcons.Width;
   with Tree.Items do begin
      // first add the scene root
      CurrentNode:=Add(nil, glsSceneRoot);
      with CurrentNode do begin
         ImageIndex:=ObjectManager.SceneRootIndex;
         SelectedIndex:=ImageIndex;
      end;
      // next the root for all cameras
      FCameraNode:=AddChild(CurrentNode, glsCameraRoot);
      with FCameraNode do begin
         ImageIndex:=ObjectManager.CameraRootIndex;
         SelectedIndex:=ObjectManager.CameraRootIndex;
      end;
      // and the root for all objects
      FObjectNode:=AddChild(CurrentNode, glsObjectRoot);
      with FObjectNode do begin
         ImageIndex:=ObjectManager.ObjectRootIndex;
         SelectedIndex:=ObjectManager.ObjectRootIndex;
      end;
   end;
   // Build SubMenus
   SetObjectsSubItems(MIAddObject);
{$ifdef DFS_DELPHI_5_UP}
	MIAddObject.SubMenuImages:=ObjectManager.ObjectIcons;
{$endif}
   SetObjectsSubItems(PMToolBar.Items);
   PMToolBar.Images:=ObjectManager.ObjectIcons;
end;

// FormDestroy
//
procedure TGLSceneEditorForm.FormDestroy(Sender: TObject);
begin
	DeRegisterGLBaseSceneObjectNameChangeEvent(OnBaseSceneObjectNameChanged);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGLSceneEditorForm.ReadScene;

var
  I: Integer;

begin
  Tree.Items.BeginUpdate;
  with FScene do
  begin
    if Assigned(Cameras) then
    begin
      FCameraNode.Data:=Cameras;
      for I:=0 to Cameras.Count - 1 do AddNodes(FCameraNode, Cameras[I]);
      FCameraNode.Expand(False);
    end;

    if Assigned(Objects) then
	 begin
      FObjectNode.Data:=Objects;
      with Objects do
        for I:=0 to Count - 1 do AddNodes(FObjectNode, Children[I]);
      FObjectNode.Expand(False);
    end;
  end;
  Tree.Items.EndUpdate;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGLSceneEditorForm.ResetTree;
begin
   // delete all subtrees (empty tree)
   Tree.Items.BeginUpdate;
   try
      FCameraNode.DeleteChildren;
      FCameraNode.Data:=nil;
      with FObjectNode do begin
         DeleteChildren;
			Data:=nil;
         Parent.Expand(True);
      end;
   finally
      Tree.Items.EndUpdate;
   end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGLSceneEditorForm.AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;

// adds the given scene object as well as its children to the tree structure and returns
// the last add node (e.g. for selection)

var
  I: Integer;
  CurrentNode: TTreeNode;

begin
  Result:=Tree.Items.AddChildObject(ANode, AObject.Name, AObject);
  Result.ImageIndex:=ObjectManager.GetImageIndex(TGLSceneObjectClass(AObject.ClassType));
  Result.SelectedIndex:=Result.ImageIndex;
  CurrentNode:=Result;
  for I:=0 to AObject.Count - 1 do Result:=AddNodes(CurrentNode, AObject[I]);
end;

procedure TGLSceneEditorForm.SetObjectsSubItems(parent : TMenuItem);
var
   objectList : TStringList;
   i, j : Integer;
   item, currentParent : TMenuItem;
   currentCategory : String;
   soc : TGLSceneObjectClass;
begin
   objectList:=TStringList.Create;
   try
      ObjectManager.GetRegisteredSceneObjects(objectList);
      for i:=0 to objectList.Count-1 do if objectList[i]<>'' then begin
         with ObjectManager do
            currentCategory:=GetCategory(TGLSceneObjectClass(objectList.Objects[i]));
         if currentCategory='' then
            currentParent:=parent
         else begin
            currentParent:=NewItem(currentCategory, 0, False, True, nil, 0, '');
            parent.Add(currentParent);
         end;
         for j:=i to objectList.Count-1 do if objectList[j]<>'' then with ObjectManager do begin
            soc:=TGLSceneObjectClass(objectList.Objects[j]);
            if currentCategory=GetCategory(soc) then begin
               item:=NewItem(objectList[j], 0, False, True, AddObjectClick, 0, '');
               item.ImageIndex:=GetImageIndex(soc);
               item.Tag:=Integer(soc);
               currentParent.Add(item);
               objectList[j]:='';
               if currentCategory='' then Break; 
            end;
         end;
      end;
	finally
      objectList.Free;
   end;
end;

procedure TGLSceneEditorForm.AddObjectClick(Sender: TObject);
var
   AParent, AObject: TGLBaseSceneObject;
   Node: TTreeNode;
begin
   if Assigned(FCurrentDesigner) then with Tree do
      if Assigned(Selected) and (Selected.Level > 0) then begin
         AParent:=TGLBaseSceneObject(Selected.Data);
         AObject:=TGLBaseSceneObject(FCurrentDesigner.CreateComponent(TGLSceneObjectClass(TMenuItem(Sender).Tag), AParent, 0, 0, 0, 0));
         TComponent(AObject).DesignInfo:=0;
         AParent.AddChild(AObject);
         Node:=AddNodes(Selected, AObject);
         Node.Selected:=True;
         FCurrentDesigner.Modified;
      end;
end;

procedure TGLSceneEditorForm.TreeDragOver(Sender, Source: TObject; X, Y: Integer;
                                          State: TDragState; var Accept: Boolean);
var
   Target : TTreeNode;
begin
   Accept:=False;
   if Source=Tree then with Tree do begin
      Target:=DropTarget;
      Accept:=Assigned(Target) and (Selected <> Target)
                and Assigned(Target.Data) and (not Target.HasAsParent(Selected));
   end;
end;

procedure TGLSceneEditorForm.TreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
   SourceNode, DestinationNode: TTreeNode;
   SourceObject, DestinationObject: TGLBaseSceneObject;
begin
   if Assigned(FCurrentDesigner) then begin
      DestinationNode:=Tree.DropTarget;
      if Assigned(DestinationNode) and (Source = Tree) then begin
			SourceNode:=TTreeView(Source).Selected;
         SourceObject:=SourceNode.Data;
         DestinationObject:=DestinationNode.Data;
         DestinationObject.Insert(0, SourceObject);
         SourceNode.MoveTo(DestinationNode, naAddChildFirst);
         FCurrentDesigner.Modified;
      end;
   end;
end;

// Notification
//
procedure TGLSceneEditorForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (FScene=AComponent) and (Operation=opRemove) then begin
      FScene:=nil;
      SetScene(nil, nil);
	end;
	inherited;
end;

// OnBaseSceneObjectNameChanged
//
procedure TGLSceneEditorForm.OnBaseSceneObjectNameChanged(Sender : TObject);
var
	n : TTreeNode;
begin
	n:=FindNodeByData(Tree.Items, Sender);
	if Assigned(n) then
		n.Text:=(Sender as TGLBaseSceneObject).Name;
end;

// TreeChange
//
procedure TGLSceneEditorForm.TreeChange(Sender: TObject; Node: TTreeNode);
var
   selNode : TTreeNode;
begin
   if Assigned(FCurrentDesigner) then begin
      selNode:=Tree.Selected;
      // select in Delphi IDE
      if Assigned(selNode) then begin
         if Assigned(selNode.Data) then
            FCurrentDesigner.SelectComponent(selNode.Data)
         else FCurrentDesigner.SelectComponent(FScene);
         // enablings
         ACAddCamera.Enabled:=(selNode=FCameraNode);
         ACAddObject.Enabled:=((selNode=FObjectNode) or selNode.HasAsParent(FObjectNode));
         ACDeleteObject.Enabled:=(selNode.Level>1);
         ACMoveUp.Enabled:=(ACAddObject.Enabled and (selNode.Index>0));
         ACMoveDown.Enabled:=(ACAddObject.Enabled and (selNode.GetNextSibling<>nil));
         ACCut.Enabled:=IsValidClipBoardNode;
         ACPaste.Enabled:=IsPastePossible;
      end else begin
         ACAddCamera.Enabled:=False;
         ACAddObject.Enabled:=False;
         ACDeleteObject.Enabled:=False;
         ACMoveUp.Enabled:=False;
         ACMoveDown.Enabled:=False;
         ACCut.Enabled:=False;
         ACPaste.Enabled:=False;
		end;
   end;
   ACCopy.Enabled:=ACCut.Enabled;
end;

// TreeEditing
//
procedure TGLSceneEditorForm.TreeEditing(Sender: TObject; Node: TTreeNode;
                                         var AllowEdit: Boolean);
begin
   AllowEdit:=(Node.Level>1);
end;

// TreeEdited
//
procedure TGLSceneEditorForm.TreeEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
   if Assigned(FCurrentDesigner) then begin
      // renaming a node means renaming a scene object
      TGLBaseSceneObject(Node.Data).Name:=S;
      FCurrentDesigner.Modified;
   end;
end;

// TreeMouseDown
//
procedure TGLSceneEditorForm.TreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   FLastMouseDownPos:=Point(X, Y);
end;

// TreeMouseMove
//
procedure TGLSceneEditorForm.TreeMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   node: TTreeNode;
begin
   if Shift=[ssLeft] then begin
      node:=Tree.Selected;
      if Assigned(node) and (node.Level>1) then
         if (Abs(FLastMouseDownPos.x-x)>4) or (Abs(FLastMouseDownPos.y-y)>4) then
            Tree.BeginDrag(False);
   end;
end;

// TreeEnter
//
procedure TGLSceneEditorForm.TreeEnter(Sender: TObject);
begin
   if Assigned(FCurrentDesigner) and Assigned(Tree.Selected) then
      FCurrentDesigner.SelectComponent(Tree.Selected.Data);
end;

// ACAddCameraExecute
//
procedure TGLSceneEditorForm.ACAddCameraExecute(Sender: TObject);
var
   AObject: TGLBaseSceneObject;
   Node: TTreeNode;
begin
   if Assigned(FCurrentDesigner) then begin
      AObject:=TGLBaseSceneObject(FCurrentDesigner.CreateComponent(TGLCamera, FScene.Cameras, 0, 0, 0, 0));
      FScene.Cameras.AddChild(AObject);
      Node:=AddNodes(FCameraNode, AObject);
      Node.Selected:=True;
      FCurrentDesigner.Modified;
   end;
end;

// ACDeleteObjectExecute
//
procedure TGLSceneEditorForm.ACDeleteObjectExecute(Sender: TObject);
var
	AObject: TGLBaseSceneObject;
   Allowed, KeepChildren: Boolean;
   ConfirmMsg: String;
   Buttons: TMsgDlgButtons;
begin
	if Assigned(Tree.Selected) and (Tree.Selected.Level > 1) then begin
      AObject:=TGLBaseSceneObject(Tree.Selected.Data);
      // ask for confirmation
      if AObject.Name <> '' then
         ConfirmMsg:='Delete ' + AObject.Name
      else ConfirmMsg:='Delete the marked object';
      Buttons:=[mbOK, mbCancel];
      // are there children to care for?
      if AObject.Count > 0 then begin
         ConfirmMsg:=ConfirmMsg + 'only or with ALL its children ?';
         Buttons:=[mbAll] + Buttons;
      end else ConfirmMsg:=ConfirmMsg + '?';
      case MessageDlg(ConfirmMsg, mtConfirmation, Buttons, 0) of
         mrAll : begin
            KeepChildren:=False;
            Allowed:=True;
			end;
         mrOK : begin
            KeepChildren:=True;
				Allowed:=True;
         end;
         mrCancel : begin
            Allowed:=False;
            KeepChildren:=True;
         end;
      else
         Allowed:=False;
         KeepChildren:=True;
      end;
      // deletion allowed?
      if allowed then begin
         AObject.Parent.Remove(AObject, KeepChildren);
         AObject.Free;
         Tree.Selected.Free;
      end
   end;
end;

// ACMoveUpExecute
//
procedure TGLSceneEditorForm.ACMoveUpExecute(Sender: TObject);
var
   node : TTreeNode;
begin
   if ACMoveUp.Enabled then begin
      node:=Tree.Selected;
      if Assigned(node) then begin
         node.MoveTo(node.GetPrevSibling, naInsert);
         with TGLBaseSceneObject(node.Data) do begin
            MoveUp;
            Update;
         end;
         TreeChange(Self, node);
      end;
   end;
end;

// ACMoveDownExecute
//
procedure TGLSceneEditorForm.ACMoveDownExecute(Sender: TObject);
var
   node : TTreeNode;
begin
   if ACMoveDown.Enabled then begin
      node:=Tree.Selected;
      if Assigned(node) then begin
         node.GetNextSibling.MoveTo(node, naInsert);
         with TGLBaseSceneObject(node.Data) do begin
				MoveDown;
            Update;
         end;
			TreeChange(Self, node);
		end;
	end;
end;

// ACAddObjectExecute
//
procedure TGLSceneEditorForm.ACAddObjectExecute(Sender: TObject);
begin
	TBAddObjects.CheckMenuDropdown;
end;

// ACSaveSceneExecute
//
procedure TGLSceneEditorForm.ACSaveSceneExecute(Sender: TObject);
begin
	if SaveDialog.Execute then
		FScene.SaveToFile(SaveDialog.FileName);
end;

// ACLoadSceneExecute
//
procedure TGLSceneEditorForm.ACLoadSceneExecute(Sender: TObject);
begin
	if OpenDialog.Execute then begin
		FScene.LoadFromFile(OpenDialog.FileName);
		ResetTree;
		ReadScene;
	end;
end;

// ACInfoExecute
//
procedure TGLSceneEditorForm.ACInfoExecute(Sender: TObject);
var
	AScene: TGLSceneViewer;
begin
	AScene:=TGLSceneViewer.Create(Self);
	AScene.Name:='GLSceneEditor';
	AScene.Width:=0;
	AScene.Height:=0;
	AScene.Parent:=Self;
	try
		AScene.ShowInfo;
	finally
		AScene.Free;
	end;
end;

// IsValidClipBoardNode
//
function TGLSceneEditorForm.IsValidClipBoardNode : Boolean;
var
   selNode : TTreeNode;
begin
   selNode:=Tree.Selected;
   Result:=((selNode<>nil) and (selNode.Parent<>nil)
            and (selNode.Parent.Parent<>nil));
end;

// IsPastePossible
//
function TGLSceneEditorForm.IsPastePossible : Boolean;
var
   selNode : TTreeNode;
	anObject, destination : TGLBaseSceneObject;
begin
   selNode:=Tree.Selected;
   if (selNode<>nil) and (selNode.Parent<>nil) and ClipBoard.HasFormat(CF_COMPONENT) then begin
      anObject:=TGLBaseSceneObject(GetComponentFromClipBoard);
      destination:=TGLBaseSceneObject(selNode.Data);
      Result:=CanPaste(anObject, destination);
      anObject.Free;
   end else Result:=False;
end;

// CanPaste
//
function TGLSceneEditorForm.CanPaste(obj, destination : TGLBaseSceneObject) : Boolean;
begin
   Result:=((obj is TGLCamera) or (destination<>FScene.Cameras))
           and (obj is TGLBaseSceneObject);
end;

// GetComponentFromClipBoard
//
function TGLSceneEditorForm.GetComponentFromClipBoard : TComponent;
var
  Data: THandle;
  DataPtr: Pointer;
  MemStream: TMemoryStream;
  Reader: TReader;
begin
   // a bug in the VCL prevents use of the standard function...
   // here is a simplified fixed one
   Result := nil;
   with ClipBoard do begin
      Open;
      try
         Data := GetClipboardData(CF_COMPONENT);
         if Data = 0 then Exit;
         DataPtr := GlobalLock(Data);
         if DataPtr = nil then Exit;
         try
            MemStream:=TMemoryStream.Create;
            try
               MemStream.WriteBuffer(DataPtr^, GlobalSize(Data));
               MemStream.Position:=0;
               Reader:=TReader.Create(MemStream, 256);
               try
                  Result:=Reader.ReadRootComponent(nil);
               finally
                  Reader.Free;
               end;
            finally
               MemStream.Free;
            end;
         finally
            GlobalUnlock(Data);
         end;
      finally
        Close;
      end;
   end;
end;

// ACCopyExecute
//
procedure TGLSceneEditorForm.ACCopyExecute(Sender: TObject);
begin
   if IsValidClipBoardNode then
      ClipBoard.SetComponent(TGLBaseSceneObject(Tree.Selected.Data));
end;

// ACCutExecute
//
procedure TGLSceneEditorForm.ACCutExecute(Sender: TObject);
var
	AObject : TGLBaseSceneObject;
   selNode : TTreeNode;
begin
   selNode:=Tree.Selected;
   if IsValidClipBoardNode then begin
      AObject:=TGLBaseSceneObject(selNode.Data);
      ClipBoard.SetComponent(AObject);
      AObject.Parent.Remove(AObject, False);
      AObject.Free;
      Tree.Selected.Free;
   end;
end;

// ACPasteExecute
//
procedure TGLSceneEditorForm.ACPasteExecute(Sender: TObject);
var
   selNode : TTreeNode;
	anObject, destination : TGLBaseSceneObject;
begin
   selNode:=Tree.Selected;
   if (selNode<>nil) and (selNode.Parent<>nil) then begin
      anObject:=TGLBaseSceneObject(GetComponentFromClipBoard);
      destination:=TGLBaseSceneObject(selNode.Data);
      if CanPaste(anObject, destination) then begin
         destination.AddChild(anObject);
         AddNodes(selNode, anObject);
         selNode.Expand(False);
      end else anObject.Free;
   end;
end;

initialization

finalization

   ReleaseGLSceneEditorForm;

end.
