{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Original File: lazarus\components\ideintf\listviewpropedit.pp
 This is modified version for TSGJListView from SGJ Controls
 Customized and added support for Group support


 *****************************************************************************
 Property editor for TListView objects

 Author: Olivier Guilbaud  (golivier@free.fr)
         Tomas Gregorovic
 
 History
   01/28/2003 OG - Create
   18/02/2003 OG - First release
   19/02/2003 OG - Add ObjInspStrConsts unit
   24/02/2003 OG - Replace TListBox with TTreeView
                   Include suItems property
   22/01/2006 TG - Dialog converted to lfm.
                   
   ToDo :
     Select the first item on show editor ... do not work :o(
}
unit SGJListViewPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, SGJ.ListView,
  // LCL
  Forms, Controls, ComCtrls, StdCtrls, Dialogs, Buttons, ButtonPanel,
  // IdeIntf
  PropEdits, ComponentEditors, ObjInspStrConsts, IDEWindowIntf;

type
  { TSGJListViewItemsEditorForm }

  TSGJListViewItemsEditorForm = class(TForm)
    BtnNewItem: TButton;
    BtnNewSubItem: TButton;
    BtnDelete: TButton;
    ButtonPanel: TButtonPanel;
    cbGroups: TComboBox;
    edtText: TEdit;
    edtIndexImg: TEdit;
    edtIndexState: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabelGroupID: TLabel;
    LabelCaption: TLabel;
    LabelImageIndex: TLabel;
    LabelStateIndex: TLabel;
    TreeView1: TTreeView;
    procedure BtnNewItemClick(Sender: TObject);
    procedure cbGroupsSelect(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelStateIndexClick(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edtIndexStateEditingDone(Sender: TObject);
  private
    FListView: TSGJListView;
    FModified: Boolean;
  public
    procedure LoadFromList(AListView: TSGJListView);
    procedure SaveToList;
  end;

  { TSGJListViewComponentEditor }

  TSGJListViewComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {TSGJListViewItemsPropertyEditor
   Property editor for the Items properties of TSGJListView object.
   Brings up the dialog for editing items}
  TSGJListViewItemsPropertyEditor = Class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  
implementation

{$R *.lfm}

function EditListView(AListView: TSGJListView): Boolean;
var
  ListViewEditorDlg: TSGJListViewItemsEditorForm;
begin
  if AListView.OwnerData then
  begin
    MessageDlg(sccsLvEdtMsgOwnerData, mtError, [mbOK], 0);
    Result := false;
    exit;
  end;

  ListViewEditorDlg := TSGJListViewItemsEditorForm.Create(Application);
  try
    ListViewEditorDlg.LoadFromList(AListView);

    if ListViewEditorDlg.ShowModal = mrOk then
      ListViewEditorDlg.SaveToList;
      
    Result := ListViewEditorDlg.FModified;
  finally
    ListViewEditorDlg.Free;
  end;
end;

{ TSGJListViewItemsEditorForm }

procedure TSGJListViewItemsEditorForm.FormCreate(Sender: TObject);
begin
  Caption  := sccsLvEdtCaption;

  GroupBox1.Caption := sccsLvEdtGrpLCaption;
  GroupBox2.Caption := sccsLvEdtGrpRCaption;

  BtnNewItem.Caption := sccsLvEdtNewItem;
  BtnNewSubItem.Caption := sccsLvEdtNewSubItem;
  BtnDelete.Caption := sccsLvEdtDelete;

  ButtonPanel.HelpButton.Caption := oisHelp;
  ButtonPanel.OKButton.Caption := oisOK;
  ButtonPanel.CancelButton.Caption := oisCancel;
  ButtonPanel.CloseButton.Caption := sccsLvEdtApply;
  ButtonPanel.CloseButton.Kind := bkCustom;
  ButtonPanel.CloseButton.Glyph := nil;
  ButtonPanel.CloseButton.ModalResult := mrNone;

  LabelCaption.Caption := sccsLvEdtLabelCaption;
  LabelImageIndex.Caption := sccsLvEdtLabelImageIndex;
  LabelStateIndex.Caption := sccsLvEdtLabelStateIndex;

  IDEDialogLayoutList.ApplyLayout(Self);

end;

procedure TSGJListViewItemsEditorForm.FormShow(Sender: TObject);
begin
  if TreeView1.Items.Count>0 then
     TreeView1.Selected := TreeView1.Items[0];
end;

procedure TSGJListViewItemsEditorForm.LabelStateIndexClick(Sender: TObject);
begin

end;

procedure TSGJListViewItemsEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TSGJListViewItemsEditorForm.BtnNewItemClick(Sender: TObject);
var
  S: String;
begin
  S := sccsLvEdtItem + IntToStr(TreeView1.Items.Count);
  if (Sender as TComponent).Tag = 1 then
    TreeView1.Selected := TreeView1.Items.Add(nil, S)
  else
  begin
    if (TreeView1.Selected=nil) or (TreeView1.Selected.Level = 0) then
      TreeView1.Selected := TreeView1.Items.AddChild(TreeView1.Selected, S)
    else
      TreeView1.Selected := TreeView1.Items.Add(TreeView1.Selected, S);
  end;

  GroupBox2.Enabled := TreeView1.Items.Count > 0;

  edtText.SetFocus;
  edtText.SelectAll;
end;

procedure TSGJListViewItemsEditorForm.cbGroupsSelect(Sender: TObject);
begin
   edtIndexStateEditingDone(cbGroups);
end;

procedure TSGJListViewItemsEditorForm.Edit1Change(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
    TreeView1.Selected.Text := edtText.Text;
end;

procedure TSGJListViewItemsEditorForm.TreeView1SelectionChanged(
  Sender: TObject);
var
  I:Integer;
begin
  if Assigned(TreeView1.Selected) then
  begin
    edtText.Text := TreeView1.Selected.Text;
    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
    //edtGroup.Text := IntToStr(TreeView1.Selected.OverlayIndex);

    for I:=0 to cbGroups.Items.Count - 1 do
    if Pos(IntToStr(TreeView1.Selected.OverlayIndex)+' - ', cbGroups.Items[i]) > 0 then
       cbGroups.ItemIndex:=I;
  end;
end;

procedure TSGJListViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  SaveToList;
end;

procedure TSGJListViewItemsEditorForm.btnDeleteClick(Sender: TObject);
var
  TempNode: TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
  begin
    TempNode := TreeView1.Selected.GetNextSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.GetPrevSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.Parent;

    TreeView1.Items.Delete(TreeView1.Selected);

    if TempNode <> nil then
      TreeView1.Selected := TempNode;

    GroupBox2.Enabled := TreeView1.Items.Count > 0;
    TreeView1.SetFocus;
  end;
end;

procedure TSGJListViewItemsEditorForm.edtIndexStateEditingDone(
  Sender: TObject);
var
  parts: TStringArray;
  gStr:string;
  I:Integer;
begin
  if Assigned(TreeView1.Selected) then
  begin
    gStr:=cbGroups.Text;
    parts:=gStr.Split([' - ']);
    //TreeView1.Selected.OverlayIndex:=StrToIntDef(edtGroup.Text, -1);
    TreeView1.Selected.OverlayIndex:= StrToIntDef(parts[0],-1);
    TreeView1.Selected.ImageIndex := StrToIntDef(edtIndexImg.Text, -1);
    TreeView1.Selected.StateIndex := StrToIntDef(edtIndexState.Text, -1);
    TreeView1.Selected.SelectedIndex := TreeView1.Selected.ImageIndex;

    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
    //edtGroup.Text := IntToStr(TreeView1.Selected.OverlayIndex);
    for I:=0 to cbGroups.Items.Count - 1 do
    if Pos(IntToStr(TreeView1.Selected.OverlayIndex)+' - ', cbGroups.Items[i]) > 0 then
       cbGroups.ItemIndex:=I;
  end;
end;

procedure TSGJListViewItemsEditorForm.LoadFromList(AListView: TSGJListView);
var
  I, J: Integer;
  Node: TTreeNode;
begin
  FListView := AListView;
  if Assigned(AListView) then
  begin
    TreeView1.Images := AListView.SmallImages;
    TreeView1.StateImages := AListView.StateImages;
    
    TreeView1.Items.BeginUpdate;
    try
      TreeView1.Items.Clear;

      cbGroups.Items.add('-1 - (None)');
      for I := 0 to AListView.Groups.Count - 1 do
      cbGroups.Items.add(IntToStr(AListView.Groups.Items[I].GroupID)+
                                  ' - '+
                                  AListView.Groups.Items[I].Header);
      
      for I := 0 to AListView.Items.Count - 1 do
      begin
        Node := TreeView1.Items.Add(nil, AListView.Items[I].Caption);
        with Node do
        begin
          ImageIndex := AListView.Items[I].ImageIndex;
          StateIndex := AListView.Items[I].StateIndex;
          SelectedIndex := ImageIndex;
          OverlayIndex := TSGJListItem(AListView.Items[I]).Group;
        end;

        //SubItems
        for J := 0 to AListView.Items[I].SubItems.Count - 1 do
        begin
          with TreeView1.Items.AddChild(Node, AListView.Items[I].SubItems[J]) do
          begin
            ImageIndex := AListView.Items[I].SubItemImages[J];
            SelectedIndex := ImageIndex;
          end;
        end;
      end;
    finally
      TreeView1.Items.EndUpdate;
    end;
  end;

  GroupBox2.Enabled := TreeView1.Items.Count > 0;
end;

procedure TSGJListViewItemsEditorForm.SaveToList;
var
  I, J: Integer;
  Node: TTreeNode;
  Item: TSGJListItem;//TListItem;
begin
  if Assigned(FListView) then
  begin
    FListView.BeginUpdate;
    try
      FListView.Items.Clear;

      //Recreate new items or modify
      for I := 0 to TreeView1.Items.Count - 1 do
      begin
        Node := TreeView1.Items[I];
        if Node.Level = 0 then
        begin
          Item := TSGJListItem(FListView.Items.Add);
          Item.Caption := Node.Text;
          Item.ImageIndex := Node.ImageIndex;
          Item.StateIndex := Node.StateIndex;
          Item.Group:=Node.OverlayIndex;
          //SubItems
          for J := 0 to Node.Count - 1 do
          begin
            Item.SubItems.Add(Node.Items[J].Text);
            Item.SubItemImages[J] := Node.Items[J].ImageIndex;
          end;
        end;
      end;
    finally
      FListView.EndUpdate;
    end;
    FListView.UpdateGroups();
    FModified := True;
  end;
end;

{ TSGJListViewItemsPropertyEditor }

procedure TSGJListViewItemsPropertyEditor.Edit;
begin
  if EditListView(GetComponent(0) as TSGJListView) then Modified;
end;

function TSGJListViewItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paRevertable];
end;

{ TSGJListViewComponentEditor }

procedure TSGJListViewComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  AListView: TSGJListView;
begin
  AListView := GetComponent as TSGJListView;
  case Index of
    0: 
    begin
      GetHook(Hook);
      if EditListView(AListView) then
        if Assigned(Hook) then Hook.Modified(Self);
    end;
    1:
    begin
      GetHook(Hook);
      EditCollection(AListView, AListView.Columns, 'Columns');
      if Assigned(Hook) then Hook.Modified(Self);
    end;
    2:
    begin
      GetHook(Hook);
      EditCollection(AListView, AListView.Groups, 'Groups');
      if Assigned(Hook) then Hook.Modified(Self);
    end;
  end;
end;

function TSGJListViewComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := sccsLvEdt;
    1: Result := sccsLvColEdt;
    2: Result :='Edit Groups ...';
    else
      Result := '';
  end;
end;

function TSGJListViewComponentEditor.GetVerbCount: Integer;
begin
  Result := 3;
end;

initialization
  //Register TSGJListViewItemsPropertyEditor
  RegisterPropertyEditor(ClassTypeInfo(TSGJListItem), TSGJListView, 'Items',
    TSGJListViewItemsPropertyEditor);

  //Register a component editor for TSGJListView
  RegisterComponentEditor(TSGJListView, TSGJListViewComponentEditor);
  
end.
