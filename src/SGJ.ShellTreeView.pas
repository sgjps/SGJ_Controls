{
====================================================================
home page : https://www.hiperapps.com
email     : sgj@sgjps.com

Control name: TSGJShellTreeView

date      : 2025/12/11
version   : 2.0

This file is part of SGJ Controls for Lazarus

====================================================================

Set icon on Unix/linux example:

var
  i:integer;
  Node:string;
begin

  for i:=0 to SGJShellTreeView1.Items.Count-1 do
  begin
    if SGJShellTreeView1.Items[i].Text=RS_SGJShellTreeView_ThisPC then
    begin
    SGJShellTreeView1.Items[i].imageindex:=0;
    SGJShellTreeView1.Items[i].selectedindex:=0;
    end;
    if SGJShellTreeView1.Items[i].Text=RS_SGJShellTreeView_Desktop then
    begin
        SGJShellTreeView1.Items[i].imageindex:=0;
        SGJShellTreeView1.Items[i].selectedindex:=0;
    end;
    if SGJShellTreeView1.Items[i].Text=RS_SGJShellTreeView_Download then
    begin
        SGJShellTreeView1.Items[i].imageindex:=0;
        SGJShellTreeView1.Items[i].selectedindex:=0;
    end;
    if SGJShellTreeView1.Items[i].Text=RS_SGJShellTreeView_Pictures then
    begin
        SGJShellTreeView1.Items[i].imageindex:=0;
        SGJShellTreeView1.Items[i].selectedindex:=0;
    end;
    if SGJShellTreeView1.Items[i].Text=RS_SGJShellTreeView_Documents then
    begin
        SGJShellTreeView1.Items[i].imageindex:=0;
        SGJShellTreeView1.Items[i].selectedindex:=0;
    end;
    if SGJShellTreeView1.Items[i].Text='/' then
    begin
        SGJShellTreeView1.Items[i].imageindex:=1;
        SGJShellTreeView1.Items[i].selectedindex:=1;
    end;
    SGJShellTreeView1.FolderIcon:=1;
    SGJShellTreeView1.FolderSelectedIcon:=1;
  end;




}





unit SGJ.ShellTreeView;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}
interface

uses
  {$IFDEF MSWINDOWS}
  Windows, ShellApi,windirs,
  {$ENDIF}
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, SysUtils,ComCtrls,Controls,Graphics, Generics.Collections,dialogs, ShellCtrls;

type
  TSGJTreeShellNode = class(TTreeNode)
  private
    fAPath: String;
  public
    property APath: String read fAPath write fAPath;
  end;


type
  TSGJShellTreeView = class(TTreeView)
  private
    fIconsList: TImageList;
    FPath: string;
    fShellListView:TCustomShellListView;
    {$IFDEF WINDOWS}
    function GetVolumeName(const ADriveLetter: string): string;
    function VolumeID(Drive: string): string;
    procedure CreateOsDirsNode(ADir: string; AFolderID: TGUID);
    {$ENDIF}
    procedure CreateOsDirsNode(ADir: string; AFolderID: string);
    procedure LoadIcons(APath: widestring;AImgList: TImageList);
    procedure LoadDrivesToTree(TreeView: TTreeView);
    procedure LoadFoldersToNode(TreeView: TTreeView; Node: TTreeNode);
    function  CheckIfSubDirsExists(ADir: string):boolean;
    procedure OpenNodeByPath(TreeView: TTreeView; const Path: string; Separator: Char = '\');
    procedure SetPath(AValue: string);
  protected
    procedure DoCreateNodeClass(var NewNodeClass: TTreeNodeClass); override;
    procedure GetSelectedIndex(Node: TTreeNode);override;
    procedure Expand(Node: TTreeNode);override;
    //procedure Click; override;
    procedure Change(Node: TTreeNode);override;
  public
    FolderIcon: integer;
    FolderSelectedIcon:Integer;
        Constructor Create(AOwner : TComponent); override;
        destructor Destroy; override;
  published
  property Path: string read FPath write SetPath;
  property ShellListView: TCustomShellListView read fShellListView write fShellListView;
  { TCustomTreeView properties }
  property Align;
  property Anchors;
  property AutoExpand;
  property BorderSpacing;
  //property BiDiMode;
  property BackgroundColor;
  property BorderStyle;
  property BorderWidth;
  property Color;
  property Constraints;
  property Enabled;
  property ExpandSignType;
  property Font;
  property HideSelection;
  property HotTrack;
  property Images;
  property Indent;
  property MultiSelectStyle;
  //property ParentBiDiMode;
  property ParentColor default False;
  property ParentFont;
  property ParentShowHint;
  property PopupMenu;
  property ReadOnly default True;
  property RightClickSelect;
  property RowSelect;
  property ScrollBars;
  property SelectionColor;
  property ShowButtons;
  property ShowHint;
  property ShowLines;
  property ShowRoot;
  property StateImages;
  property TabOrder;
  property TabStop default True;
  property Tag;
  property ToolTips;
  property Visible;
  property Options;
  property TreeLineColor;
  property TreeLinePenStyle;
  property ExpandSignColor;

  property OnAdvancedCustomDraw;
  property OnAdvancedCustomDrawItem;
  property OnChange;
  property OnChanging;
  property OnClick;
  property OnCollapsed;
  property OnCollapsing;
  property OnCustomDraw;
  property OnCustomDrawItem;
  property OnDblClick;
  property OnEdited;
  property OnEditing;
  property OnEnter;
  property OnExit;
  property OnExpanded;
  property OnExpanding;
  property OnGetImageIndex;
  property OnGetSelectedIndex;
  property OnHasChildren;
  property OnKeyDown;
  property OnKeyPress;
  property OnKeyUp;
  property OnMouseDown;
  property OnMouseEnter;
  property OnMouseLeave;
  property OnMouseMove;
  property OnMouseUp;
  property OnMouseWheel;
  property OnMouseWheelDown;
  property OnMouseWheelUp;
  property OnMouseWheelHorz;
  property OnMouseWheelLeft;
  property OnMouseWheelRight;
  property OnSelectionChanged;
  property OnShowHint;
  property OnUTF8KeyPress;
  end;

  procedure Register;

  resourcestring
    RS_SGJShellTreeView_ThisPC = 'This PC';
    RS_SGJShellTreeView_Desktop = 'Desktop';
    RS_SGJShellTreeView_Download = 'Download';
    RS_SGJShellTreeView_Pictures = 'Pictures';
    RS_SGJShellTreeView_Documents = 'Documents';
    RS_SGJShellTreeView_OneDrive = 'OneDrive';
    RS_SGJShellTreeView_Music    = 'Music';
    RS_SGJShellTreeView_Videos   = 'Videos';
    RS_SGJShellTreeView_LocalDisk = 'Local Disk';
    RS_SGJShellTreeView_CD = 'CD-Rom';
    RS_SGJShellTreeView_Remote = 'Remote';
    RS_SGJShellTreeView_Ramdisk = 'Ramdisk';
    RS_SGJShellTreeView_Removable = 'Removable';
implementation


procedure Register;
begin
  RegisterComponents('SGJ', [TSGJShellTreeView]);
end;
{
procedure TSGJShellTreeView.Click;
begin
  inherited;
  If ShellListView<>nil
  then
  if Path<>'' then
  ShellListView.Root:= Path;
end; }
procedure TSGJShellTreeView.Change(Node: TTreeNode);
begin
  inherited;
  If ShellListView<>nil
  then
  if Path<>'' then
  ShellListView.Root:= Path;
end;
procedure TSGJShellTreeView.Expand(Node: TTreeNode);
begin
    if Node.Count = 0 then
    LoadFoldersToNode(self, Node);
    inherited;
end;
procedure TSGJShellTreeView.GetSelectedIndex(Node: TTreeNode);
begin
    Fpath:=TSGJTreeShellNode(Node).APath;
    inherited;
end;
procedure TSGJShellTreeView.SetPath(AValue: string);
begin
  {$IFDEF UNIX}
   AValue:='/'+AValue.Replace('/','\');
  {$ENDIF}
   OpenNodeByPath(self,AValue,'\');
end;



procedure TSGJShellTreeView.OpenNodeByPath(TreeView: TTreeView; const Path: string; Separator: Char = '\');
var
  Node: TTreeNode;
  PathParts: specialize  TArray<string>;
  Part: string;
  Drive: Char;
  APath: string;
begin
  {$IFDEF MSWINDOWS}
  if Path.Contains(':\') then begin
  Drive:=(path[1]);
  APath:=Path.Replace(Drive+':',VolumeID((Drive)+':'));
  end;
  APath:=RS_SGJShellTreeView_ThisPC+'\'+APath;
  {$ENDIF}
  {$IFDEF Linux}
  APath:=RS_SGJShellTreeView_ThisPC+'\'+Path;
  {$ENDIF}
  PathParts := APath.Split([Separator]);
  Node := TreeView.Items.GetFirstNode;

  for Part in PathParts do
  begin
    while Assigned(Node) and (Node.Text <> Part) do
      Node := Node.GetNextSibling;

    if not Assigned(Node) then
      Exit; // Path not found

    Node.Expand(False); // Expand the current node
    Node := Node.GetFirstChild; // Move to the next level
  end;

  if Assigned(Node) then
    TreeView.Selected := Node.Parent; // Select the final node

end;

procedure TSGJShellTreeView.DoCreateNodeClass(var NewNodeClass: TTreeNodeClass);
begin
  NewNodeClass := TSGJTreeShellNode;
  inherited DoCreateNodeClass(NewNodeClass);
end;
procedure TSGJShellTreeView.LoadIcons(APath: widestring;AImgList: TImageList);
var
    mIcon: TIcon;
 {$IFDEF MSWINDOWS}
  FileInfo: SHFILEINFOw;
  {$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  SHGetFileInfoW(PWideChar(APath), 0, FileInfo, SizeOf(FileInfo), SHGFI_ICON or SHGFI_DISPLAYNAME or SHGFI_SHELLICONSIZE or SHGFI_TYPENAME or SHGFI_ICON or SHGFI_LargeICON or SHGFI_SYSICONINDEX);
     if FileInfo.hIcon <> 0 then
       begin
             mIcon := TIcon.Create;
            mIcon.Handle := FileInfo.hIcon;
            fIconsList.AddIcon(micon);

            mIcon.free;
       end;
  {$ENDIF}
end;
{$IFDEF MSWINDOWS}
function TSGJShellTreeView.GetVolumeName(const ADriveLetter: string): string;
var
 dummy: DWORD;
 buffer: array [0 .. MAX_PATH] of Char;
 oldmode: LongInt;
begin

 oldmode := SetErrorMode(SEM_FAILCRITICALERRORS);
 try
   GetVolumeInformation(pchar(ADriveLetter + '\'), buffer, sizeof(buffer), nil, dummy, dummy, nil, 0);
   Result := StrPas(buffer);
 finally
   SetErrorMode(oldmode);
 end;
end;

function TSGJShellTreeView.VolumeID(Drive: string): string;
var
 OldErrorMode: Integer;
 NotUsed,VolFlags:longword;
 Buf: array [0 .. MAX_PATH] of Char;
 drvtype:integer;
begin
 OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
 try
   if GetVolumeInformation(PChar(Drive+'\'), Buf, sizeof(Buf),
     nil, NotUsed, VolFlags, nil, 0) then
     SetString(Result, Buf, StrLen(Buf))
   else
     Result := '';

    if result<>'' then result:=result + ' ('+Drive+')'
    else  begin
       drvtype := GetDriveType(PChar(Drive+'\'));
       if drvtype <> 0 then case drvtype of
             DRIVE_FIXED: result:=RS_SGJShellTreeView_LocalDisk+' ('+Drive+')';
             DRIVE_CDROM: result:=RS_SGJShellTreeView_CD+' ('+Drive+')';
             DRIVE_REMOVABLE:  result:=RS_SGJShellTreeView_Remote+' ('+Drive+')';
             DRIVE_REMOTE:    result:=RS_SGJShellTreeView_Ramdisk+' ('+Drive+')';
             DRIVE_RAMDISK:   result:=RS_SGJShellTreeView_Removable+' ('+Drive+')';

     // result:=GetVolumeName(drive);
     end;
    end;
 finally
   SetErrorMode(OldErrorMode);
 end;
end;
{$ENDIF}
procedure  TSGJShellTreeView.LoadDrivesToTree(TreeView: TTreeView);
var
  Drive: WChar;
  RootNode,DNode: TTreeNode;
  ADrive: widestring;
begin
  TreeView.Items.BeginUpdate;
  try
    RootNode := TreeView.Items.Add(nil, RS_SGJShellTreeView_ThisPC);
    RootNode.Data := Pointer(1);
    RootNode.HasChildren := True;
    {$IFDEF MSWINDOWS}
    RootNode.ImageIndex:=0;
    RootNode.SelectedIndex:=0;
    {$ENDIF}
    {$IFDEF UNIX}
    DNode := TreeView.Items.AddChild(RootNode,'/');
    DNode.HasChildren := CheckIfSubDirsExists('/');
    TSGJTreeShellNode(DNode).APath:='/';
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    for Drive := 'A' to 'Z' do
    begin
      if DirectoryExists(Drive + ':\') then
      begin
        ADrive:=Drive + ':\';
        DNode := TreeView.Items.AddChild(RootNode, VolumeID(Drive+':'));
        DNode.HasChildren := CheckIfSubDirsExists(Drive + ':\');
        TSGJTreeShellNode(DNode).APath:=ADrive;
        LoadIcons(ADrive,fIconsList);
        dnode.ImageIndex:=fIconsList.Count-1;
        dnode.SelectedIndex:=fIconsList.Count-1;

      end;
    end;
    {$ENDIF}
  finally
    TreeView.Items.EndUpdate;
  end;
end;

function TSGJShellTreeView.CheckIfSubDirsExists(ADir: string):boolean;
var
  SearchRec: TSearchRec;
begin
  result:=false;
   if FindFirst((ADir)+'*', faDirectory, SearchRec) = 0 then
   begin
     try
       repeat
         if (SearchRec.Attr and faDirectory <> 0) and
            (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
           if SearchRec.Name<>'' then
           begin
             result:=true;
             exit;
           end;
       until FindNext(SearchRec) <> 0;
     finally
       FindClose(SearchRec);
     end;
   end;
end;

procedure  TSGJShellTreeView.LoadFoldersToNode(TreeView: TTreeView; Node: TTreeNode);
var
  SearchRec: TSearchRec;
  FolderPath: string;
  ChildNode: TTreeNode;
  APath: string;
begin

  FolderPath := IncludeTrailingPathDelimiter(TSGJTreeShellNode(Node).APath);

  if FindFirst(FolderPath+ '*', faDirectory, SearchRec) = 0 then
  begin
    try
      repeat
        if (SearchRec.Attr and faDirectory <> 0) and
           (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          ChildNode := TreeView.Items.AddChild(Node, SearchRec.Name);
          ChildNode.HasChildren :=CheckIfSubDirsExists(IncludeTrailingPathDelimiter(FolderPath + SearchRec.Name));
          TSGJTreeShellNode(ChildNode).APath:=FolderPath+IncludeTrailingPathDelimiter(SearchRec.Name);
          APath:= FolderPath+SearchRec.Name;
          {$IFDEF MSWINDOWS}
          LoadIcons(unicodestring(APath),fIconsList);
                      ChildNode.ImageIndex:=fIconsList.Count-1;
                      ChildNode.SelectedIndex:=fIconsList.Count-1;
          {$ENDIF}
          {$IFDEF UNIX}
          ChildNode.ImageIndex:=FolderIcon;
          ChildNode.SelectedIndex:=FolderSelectedIcon;
          {$ENDIF}

        end;
      until FindNext(SearchRec) <> 0;
    finally
      FindClose(SearchRec);
    end;
  end;
end;
{$IFDEF MSWINDOWS}
procedure TSGJShellTreeView.CreateOsDirsNode(ADir: string; AFolderID: TGUID);
var
  RootNode: TTreeNode;
begin
  RootNode := self.Items.Add(nil, ADir);
  TSGJTreeShellNode(RootNode).APath:=IncludeTrailingPathDelimiter(GetWindowsSpecialDir(AFolderID,false));
  LoadIcons(IncludeTrailingPathDelimiter(GetWindowsSpecialDirUnicode(AFolderID,false)),fIconsList);
  RootNode.ImageIndex:=fIconsList.Count-1;
  RootNode.SelectedIndex:=fIconsList.Count-1;

end;
{$ENDIF}
procedure TSGJShellTreeView.CreateOsDirsNode(ADir: string; AFolderID: string);
var
  SL: TStringList;
  RootNode: TTreeNode;
  HPath: string;
  IIndex:integer;
begin
  SL := TStringList.Create;
  try
    // Check if file exists before loading
    if FileExists(GetUserDir+'.config/user-dirs.dirs') then
    begin
      try
        SL.LoadFromFile(GetUserDir+'.config/user-dirs.dirs');
        if SL.Values[AFolderID]<>'' then
        begin
           HPath:=SL.Values[AFolderID].Replace('$HOME/',GetUserDir);
           HPath:=HPath.Replace('"','');
          RootNode := self.Items.Add(nil, ADir);
          TSGJTreeShellNode(RootNode).APath:=HPath;
        end;

      finally
      end;
    end;

  finally
     SL.Free;
  end;

end;

Constructor TSGJShellTreeView.Create(AOwner : TComponent);
begin
  Inherited;
  if not ( csDesigning in ComponentState) then
  begin
  self.Options:=[tvoAutoExpand,tvoShowButtons,tvoThemedDraw, tvoShowRoot, tvoKeepCollapsedNodes];
  ReadOnly:=true;
  FolderIcon:=-1;
  FolderSelectedIcon:=-1;
  {$IFDEF MSWINDOWS}
  fIconsList := TImageList.Create(self);
  fIconsList.Width:=24;
  fIconsList.Height:=24;
  self.Images:=fIconsList;
  self.ImagesWidth:=24;
  CreateOsDirsNode(RS_SGJShellTreeView_Desktop, FOLDERID_Desktop);
  CreateOsDirsNode(RS_SGJShellTreeView_Documents, FOLDERID_Documents);
  CreateOsDirsNode(RS_SGJShellTreeView_Download, FOLDERID_Downloads);
  CreateOsDirsNode(RS_SGJShellTreeView_Pictures, FOLDERID_Pictures);
  CreateOsDirsNode(RS_SGJShellTreeView_OneDrive, FOLDERID_OneDrive);
  CreateOsDirsNode(RS_SGJShellTreeView_Music, FOLDERID_Music);
  CreateOsDirsNode(RS_SGJShellTreeView_Videos, FOLDERID_Videos);
  {$ENDIF}
  {$IFDEF Linux}
  CreateOsDirsNode(RS_SGJShellTreeView_Desktop, 'XDG_DESKTOP_DIR');
  CreateOsDirsNode(RS_SGJShellTreeView_Documents, 'XDG_DOCUMENTS_DIR');
  CreateOsDirsNode(RS_SGJShellTreeView_Download, 'XDG_DOWNLOAD_DIR');
  CreateOsDirsNode(RS_SGJShellTreeView_Pictures, 'XDG_PICTURES_DIR');
  CreateOsDirsNode(RS_SGJShellTreeView_Music, 'XDG_MUSIC_DIR');
  CreateOsDirsNode(RS_SGJShellTreeView_Videos, 'XDG_VIDEOS_DIR'); 
  // XDG_TEMPLATES_DIR, XDG_PUBLICSHARE_DIR,
  {$ENDIF}

  LoadDrivesToTree(self);

  end;
end;

destructor  TSGJShellTreeView.Destroy;
begin
    {$IFDEF MSWINDOWS}
  fIconsList.Free;
    {$ENDIF}
  inherited;
end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.ShellTreeView.lrs}
{$ENDIF}
end.


