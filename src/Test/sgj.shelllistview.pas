unit SGJ.ShellListView;

{$mode ObjFPC}{$H+}

interface

uses
  LCLType,
  {$IFDEF Windows}
  Windows, ShellApi, Commctrl, uxTheme,
  {$ENDIF}
  Classes, SysUtils, ComCtrls, ImgList, Controls, FileUtil, Graphics,
  SGJ.ShellTreeView, LCLStrConsts;

type
  TObjectType = (otFolders, otNonFolders, otHidden);
  TObjectTypes = set of TObjectType;

  TSGJShlLVOptType = (optDarkMode, optThemed);
  TSGJShlLVOptTypes = set of TSGJShlLVOptType;


  TSGJShellListItem = class(TListItem)
  private
    FPath: String; // Custom property
  public
    property Path: String read FPath write FPath;
  end;

  TSGJShellListView = class(TCustomListView)
  private
    fPath: string;
    fSmallIconList: TImageList;
    fLargeIconList: TImageList;
    //fDarkMode: boolean;
    FObjectTypes: TObjectTypes;
    FOptions : TSGJShlLVOptTypes;
    FListItems: TListItems;
    procedure FileFoundEvent(FileIterator: TFileIterator);
    procedure DirectoryFoundEvent(FileIterator: TFileIterator);
    procedure SetPath(AValue: string);
 //   procedure SetDarkMode(AValue: Boolean);
    procedure LoadShellIcons();
    procedure LoadItems(APath: string);
    procedure SetObjectTypes(const Value: TObjectTypes);
    procedure SetOptions(const Value: TSGJShlLVOptTypes);

    function GetVolumeName(const ADriveLetter: string): string;
    function VolumeIDorType(Drive: string;ATypeOnly:boolean): string;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; ADarkMode:Boolean);
    property Path: string read FPath write SetPath;
    procedure ThisPC();
  protected
    procedure Loaded; override;
    destructor  Destroy;
    procedure DblClick; override;
    procedure DoCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
  published
    //property EnableDarkMode: boolean read fDarkMode write SetDarkMode;
    property Options: TSGJShlLVOptTypes read FOptions write SetOptions;
    property Align;
    property AllocBy;
    property Anchors;
    property AutoSort;
    property AutoSortIndicator;
    //    property AutoWidthLastColumn: Boolean read FAutoWidthLastColumn write SetAutoWidthLastColumn default False; // resize last column to fit width of TListView
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property Columns;
    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property GridLines;
    property HideSelection;
    property IconOptions;
    // ItemIndex shouldn't be published, see bug 16367
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes default [otNonFolders];
    property Items;
    property LargeImages;
    property LargeImagesWidth;
    property MultiSelect;
    property OwnerData;
    property OwnerDraw;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
    property SmallImages;
    property SmallImagesWidth;
    property SortColumn;
    property SortDirection;
    property SortType;
    property StateImages;
    property StateImagesWidth;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle;

    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
  //  property OnCreateItemClass;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnItemChecked;
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
    property OnResize;
    property OnSelectItem;
    property OnShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


const
  DarkColor2= $00383838;
  DarkColorFont= clWhite;

const
  SHIL_LARGE = $00;
  //The image size is normally 32x32 pixels. However, if the Use large icons option is selected from the Effects section of the Appearance tab in Display Properties, the image is 48x48 pixels.
  SHIL_SMALL = $01;
  //These images are the Shell standard small icon size of 16x16, but the size can be customized by the user.
  SHIL_EXTRALARGE = $02;
  //These images are the Shell standard extra-large icon size. This is typically 48x48, but the size can be customized by the user.
  SHIL_SYSSMALL = $03;
  //These images are the size specified by GetSystemMetrics called with SM_CXSMICON and GetSystemMetrics called with SM_CYSMICON.
  SHIL_JUMBO = $04;  //Windows Vista and later. The image is normally 256x256 pixels.
  IID_IImageList: TGUID = '{46EB5926-582E-4017-9FDF-E8998DAA0950}';
  ID_SUB_LISTVIEW    = 6;

resourcestring
  RS_SGJShellTreeView_LocalDisk = 'Local Disk';
  RS_SGJShellTreeView_CD = 'CD-Rom';
  RS_SGJShellTreeView_Remote = 'Remote';
  RS_SGJShellTreeView_Ramdisk = 'Ramdisk';
  RS_SGJShellTreeView_Removable = 'Removable';

procedure Register;
implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJShellListView]);
end;

function ListViewWindowProcSubclassed(Window: HWND; Msg: UINT; wParam: Windows.WPARAM; lParam: Windows.LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var NMHdr: LCLType.PNMHDR; NMCustomDraw: PNMCustomDraw;
begin

    If Msg = WM_NOTIFY then begin
       NMHdr := LCLType.PNMHDR(LParam);
       if NMHdr^.code = LCLType.NM_CUSTOMDRAW then begin
          NMCustomDraw:= PNMCustomDraw(LParam);
          case NMCustomDraw^.dwDrawStage of
            CDDS_PREPAINT:
             begin
               Result := CDRF_NOTIFYITEMDRAW;
               exit;
             end;
            CDDS_ITEMPREPAINT:
             begin
               SetTextColor(NMCustomDraw^.hdc , RGBToColor(255, 255, 255));
               Result := CDRF_NEWFONT;
               exit;
             end;
          end;
        end;
    end;
    Result := DefSubclassProc(Window, Msg, WParam, LParam);
end;
{
procedure TSGJShellListView.SetDarkMode(AValue: Boolean);
begin
  if fDarkMode<>AValue then
     fDarkMode:=AValue;
end;
}
procedure TSGJShellListView.LoadShellIcons();
var
  mIcon: TIcon;
  FileInfo: SHFILEINFOw;
  // fpath:unicodestring;
  aImgList: HIMAGELIST;
  i: integer;
  filePath: unicodestring;
begin
 fSmallIconList.clear;
 fLargeIconList.Clear;
 for i := 0 to self.items.Count - 1 do
    begin


      filePath := TSGJShellListItem(self.Items[i]).Path;//fPath + self.items[i].Caption;
      // showmessage(filepath);
      FillChar(FileInfo, SizeOf(FileInfo), 0);
      SHGetFileInfoW(pwidechar(filePath), 0, FileInfo, SizeOf(FileInfo),
        SHGFI_ICON or SHGFI_DISPLAYNAME or SHGFI_SHELLICONSIZE or
        SHGFI_TYPENAME or SHGFI_ICON or SHGFI_LargeICON or SHGFI_SYSICONINDEX);

      mIcon := TIcon.Create;
      //SHGetImageList(SHIL_JUMBO, IID_IImageList, @aImgList);
      SHGetImageList(SHIL_SMALL, IID_IImageList, @aImgList);
      mIcon.Handle := ImageList_GetIcon(aImgList, FileInfo.iIcon, ILD_NORMAL);
      //mIcon.Handle := FileInfo.hIcon;
      fSmallIconList.AddIcon(mIcon);

      SHGetImageList(SHIL_LARGE, IID_IImageList, @aImgList);
      mIcon.Handle := ImageList_GetIcon(aImgList, FileInfo.iIcon, ILD_NORMAL);
      fLargeIconList.AddIcon(mIcon);
      self.items[i].ImageIndex := i;
      mIcon.Free;


    end;
end;

procedure TSGJShellListView.LoadItems(APath: string);
var
  fi: TFileSearcher;
begin
  self.beginupdate;
  self.Items.Clear;

  if otFolders in ObjectTypes then begin
  fi := TFileSearcher.Create;
  if otHidden in ObjectTypes then
  fi.DirectoryAttribute:=faDirectory or faHidden
  else
  fi.DirectoryAttribute:=faDirectory  and not faHidden;
  fi.OnDirectoryFound := @DirectoryFoundEvent;
  try
    fi.Search(APath, '*.*', False);
  finally
    fi.Free;
  end;

  end;

  if otNonFolders in ObjectTypes then begin
  fi := TFileSearcher.Create;
  if otHidden in ObjectTypes then
  fi.FileAttribute :=faAnyFile or faHidden
  else
  fi.FileAttribute :=faAnyFile and not faHidden;
  fi.OnFileFound := @FileFoundEvent;
  try
    fi.Search(APath, '*.*', False);
  finally
    fi.Free;
  end;

  end;

  LoadShellIcons();
  self.endupdate;
end;

constructor TSGJShellListView.Create(AOwner: TComponent);
var
  AColumn: TListColumn;
begin
  inherited Create(AOwner);

 AColumn := self.Columns.Add;
 AColumn.Caption := sShellCtrlsName; // Set the column title
 AColumn.Width := 100;

 AColumn := self.Columns.Add;
 AColumn.Caption := sShellCtrlsType; // Set the column title
 AColumn.Width := 100;

 AColumn := self.Columns.Add;
 AColumn.Caption := sShellCtrlsSize; // Set the column title
 AColumn.Width := 100;

 AColumn := self.Columns.Add;
 AColumn.Caption := ''; // Set the column title
 AColumn.Width := 100;

 fSmallIconList := TImageList.Create(self);
 fLargeIconList := TImageList.Create(self);
 fSmallIconList.Width:=16;
 fSmallIconList.Height:=16;
 fLargeIconList.Width:=32;
 fLargeIconList.Height:=32;

 if not Assigned(SmallImages) then
 SmallImages:=fSmallIconList;
 if not Assigned(LargeImages) then
 LargeImages:=fLargeIconList;

 ViewStyle:=vsReport;

 FObjectTypes:= [otFolders, otNonFolders];

 FOptions := [optThemed];

 onCreateItemClass:=@DoCreateItemClass;
end;

destructor  TSGJShellListView.Destroy;
begin
  fSmallIconList.Free;
  fLargeIconList.Free;
  inherited;
end;

constructor TSGJShellListView.Create(AOwner: TComponent; ADarkMode:Boolean);
var
  AColumn: TListColumn;
begin
  inherited Create(AOwner);
  //fDarkMode:=ADarkMode;
end;

procedure TSGJShellListView.FileFoundEvent(FileIterator: TFileIterator);
var
  ListItem: TSGJShellListItem;
  Size: Double;
begin
  ListItem := TSGJShellListItem(Items.Add);
  ListItem.Caption := ExtractFileName(FileIterator.FileName);
  ListItem.SubItems.Add(ExtractFileExt(FileIterator.FileName));
  ListItem.Path:=FileIterator.FileName;
  Size:= FileUtil.FileSize(FileIterator.FileName);
 // ListItem.SubItems.Add(FormatFloat('0.00 B', Size))
 if Size<1024 then
  ListItem.SubItems.Add(FormatFloat('0 B', Size))
  else
  if Size<1024*1024 then begin
     Size:=Size / 1024;
     ListItem.SubItems.Add(FormatFloat('0 KB', Size));
  end else
  if Size<1024*1024*1024 then begin
     Size:=Size / (1024*1024);
     ListItem.SubItems.Add(FormatFloat('0.00 MB', Size));
  end else
  begin
    Size:=Size / (1024*1024*1024);
    ListItem.SubItems.Add(FormatFloat('0.00 GB', Size));
  end;

end;

procedure TSGJShellListView.DirectoryFoundEvent(FileIterator: TFileIterator);
var
  ListItem: TSGJShellListItem;
begin
  ListItem := TSGJShellListItem(Items.Add);
  ListItem.Caption := ExtractFileName(FileIterator.FileName);
  ListItem.SubItems.Add('');
  ListItem.SubItems.Add('Dir');
  ListItem.Path:=FileIterator.FileName;
end;

procedure TSGJShellListView.SetPath(AValue: string);
begin
  if fPath<>AValue then
  if DirectoryExists(AValue) then
  begin
  fPath := AValue;
  LoadItems(fpath);
  end;
end;

procedure TSGJShellListView.Loaded;
var
 h:THandle;
begin


 if optDarkMode in Options then
 begin
 SetWindowTheme(self.handle,'DarkMode_Explorer',nil);
 h:=ListView_GetHeader(self.handle);
 SetWindowTheme(h, 'DarkMode_ItemsView', NIL);
 SetWindowSubclass(self.Handle, @ListViewWindowProcSubclassed, ID_SUB_LISTVIEW, 0);
 self.Color:=DarkColor2;
 self.Font.Color:=DarkColorFont;
 end;

 if optThemed in Options then
 begin
 SetWindowTheme(self.handle,'Explorer',nil);
 end;

 inherited Loaded;
end;

procedure TSGJShellListView.SetObjectTypes(const Value: TObjectTypes);
begin
   if FObjectTypes = Value then Exit;
  FObjectTypes := Value;
end;

procedure TSGJShellListView.SetOptions(const Value: TSGJShlLVOptTypes);
begin
   if FOptions = Value then Exit;
  FOptions := Value;
end;

function TSGJShellListView.GetVolumeName(const ADriveLetter: string): string;
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

function TSGJShellListView.VolumeIDorType(Drive: string;ATypeOnly:boolean): string;
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

    if (result<>'') and (ATypeOnly=false) then result:=result + ' ('+Drive+')'
    else  begin
       drvtype := GetDriveType(PChar(Drive+'\'));
       if drvtype <> 0 then case drvtype of
             DRIVE_FIXED: result:=RS_SGJShellTreeView_LocalDisk;//+' ('+Drive+')';
             DRIVE_CDROM: result:=RS_SGJShellTreeView_CD;//+' ('+Drive+')';
             DRIVE_REMOVABLE:  result:=RS_SGJShellTreeView_Remote;//+' ('+Drive+')';
             DRIVE_REMOTE:    result:=RS_SGJShellTreeView_Ramdisk;//+' ('+Drive+')';
             DRIVE_RAMDISK:   result:=RS_SGJShellTreeView_Removable;//+' ('+Drive+')';

     // result:=GetVolumeName(drive);
     end;
       if  ATypeOnly<>true then result:=result+' ('+Drive+')';
    end;
 finally
   SetErrorMode(OldErrorMode);
 end;
end;
procedure TSGJShellListView.ThisPC();
var
 Drive: WChar;
 ListItem: TSGJShellListItem;
begin
 Items.Clear;
 for Drive := 'A' to 'Z' do
 begin
   if DirectoryExists(Drive + ':\') then
   begin
      ListItem := TSGJShellListItem(Items.Add);
      ListItem.Caption := VolumeIDorType(Drive+':',false);
      ListItem.SubItems.Add(VolumeIDorType(Drive+':',true));
      ListItem.SubItems.Add('');
      ListItem.Path:=Drive + ':\';
   end;
 end;
 LoadShellIcons();
end;

procedure TSGJShellListView.DoCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
 inherited;
 ItemClass:=TSGJShellListItem;
end;

procedure TSGJShellListView.DblClick;
begin
 inherited;
  if SelCount=1 then
  if DirectoryExists(TSGJShellListItem(Selected).Path) then
     Path:=TSGJShellListItem(Selected).Path;
end;

end.
