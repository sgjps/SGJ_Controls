{
*****************************************************************************
This file is part os SGJ Controls

  Author: Grzegorz Skulimowski
  Web:    www.hiperapps.com
  Web:    www.sgjps.com
  Mail:   sgj@sgjps.com

  History
    Created: 2025/12/25



*****************************************************************************
}

unit SGJ.ListView;

{$mode objfpc}{$H+}

interface

uses
  {$IfDef MSWindows}
  Windows, uxtheme, CommCtrl,
  {$EndIf}
  {$IFDEF FPC}
  LCLType,LResources,
  {$EndIf}
  Classes, ComCtrls, SysUtils, Graphics, Dialogs, ImgList;

type
  //Copy from ComCtrls->listitems.inc
  TItemHeader = record
    Size, Count: integer;
    Items: record
      end;
  end;

  ////Copy from ComCtrls->listitems.inc | Extended on GroupID
  TLazItemInfo = record
    ImageIndex: integer;
    StateIndex: integer;
    OverlayIndex: integer;
    SubItemCount: integer;
    GroupID: integer;
  end;

  //Custom View Style
  TSGJViewStyle = (lvsIcon, lvsSmallIcon, lvsList, lvsReport, lvsTile);
  TStyleName = (snDefault, snLight, snDark);

  //Tile Options, Delphi Compatible, but without TileColumns- TTileColumns
  //Tile Columns is calculated from SubLineCount

  //Tile Size Type
  TTileSizeType = (tstAutoSize, tstFixedHeight, tstFixedSize, tstFixedWidth);

  //Tile Label Margins
type
  TTileLabelMargins = class(TPersistent)
  private
    fBottom: integer;
    fLeft: integer;
    fRight: integer;
    fTop: integer;
  published
    property Bottom: integer read fBottom write fbottom;
    property Left: integer read fLeft write fLeft;
    property Right: integer read fRight write fRight;
    property Top: integer read fTop write fTop;
  end;

  TTileOptions = class(TPersistent)
  private
    fWidth: integer;
    fHeight: integer;
    fSizeType: TTileSizeType;
    fLineCount: integer;
    fLabelMargins: TTileLabelMargins;
  published
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property SizeType: TTileSizeType read fSizeType write fSizeType;
    property SubLineCount: integer read fLineCount write fLineCount;
    property LabelMargins: TTileLabelMargins read fLabelMargins write fLabelMargins;
  end;


  //ListView Groups
  //Settings is compatible with Delphi
  //https://learn.microsoft.com/en-us/windows/win32/api/commctrl/ns-commctrl-lvgroup

  //List Group State
  TListGroupState = (
    lgsNormal,
    lgsHidden,
    lgsCollapsed,
    lgsNoHeader,
    lgsCollapsible
    );

  TListGroupStateSet = set of TListGroupState;

  //List Group
  TListGroup = class(TCollectionItem)
  private
    FHeader: TTranslateString;
    FFooter: TTranslateString;
    FGroupID: integer;
    FState: TListGroupStateSet;
    FHeaderAlign: TAlignment;
    FFooterAlign: TAlignment;
    FSubtitle: TTranslateString;
    FTitleImage: TImageIndex;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(ASource: TPersistent); override;
    procedure SetHeader(AValue: TTranslateString);
    procedure SetFooter(AValue: TTranslateString);
    procedure SetSubtitle(AValue: TTranslateString);
    procedure SetGroupID(AValue: integer);
    procedure SetHeaderAlign(AValue: TAlignment);
    procedure SetFooterAlign(AValue: TAlignment);
    procedure SetState(AValue: TListGroupStateSet);
    procedure SetImage(AValue: TImageIndex);
  published
    property HeaderAlign: TAlignment read FHeaderAlign write SetHeaderAlign;
    property FooterAlign: TAlignment read FFooterAlign write SetFooterAlign;
    property Header: TTranslateString read FHeader write SetHeader;
    property Footer: TTranslateString read FFooter write SetFooter;
    property GroupID: integer read FGroupID write FGroupID;
    property Subtitle: TTranslateString read FSubtitle write SetSubtitle;
    property State: TListGroupStateSet read FState write SetState;
    property TitleImage: TImageIndex read FTitleImage write SetImage;
  end;

  TListGroups = class(TCollection)
  private
    fOwner: TCustomListView;
    fNextGroupId: integer;
    function GetItem(const AIndex: integer): TListGroup;
    procedure SetItem(const AIndex: integer; const AValue: TListGroup);
  public
    property Items[const AIndex: integer]: TListGroup read GetItem write SetItem;
    procedure UpdateGroups();
    procedure SetNextGroup();
    constructor Create(AOwner: TCustomListView);
    function Add: TListGroup;
    procedure Assign(ASource: TPersistent); override;
  published
    property Owner: TCustomListView read fOwner write fOwner;
    property NextGroupID: integer read fNextGroupId write fNextGroupId;
  end;


  //TListItem subclassed and extended, Group added
  TSGJListItem = class(ComCtrls.TListItem)
  private
    FGroupID: integer;
  protected
    function IsEqual(const AItem: TSGJListItem): boolean;
  public
    property Group: integer read FGroupID write FGroupID;

  end;



  //TListItems subclassed
  TSGJListItems = class(TListItems)
  private
    procedure ReadLazData(Stream: TStream);
    procedure WriteLazData(Stream: TStream);
  protected
    procedure SetItem(Index: integer; Value: TSGJListItem);
    function GetItem(Index: integer): TSGJListItem;
    procedure DefineProperties(Filer: TFiler); override;
  public
    function Add: TSGJListItem;
    property Item[const Index: integer]: TSGJListItem read GetItem write SetItem;
  end;



  //TSGJListView
  TSGJListView = class(TCustomListView)
  private
    fGroups: TListGroups;
    fGrupView: boolean;
    fViewStyle: TSGJViewStyle;
    fTileOpt: TTileOptions;
    fTileColumns: array of uint;
    fStyleName: TStyleName;
    fOldColor: TColor;
    fOldFontColor: TColor;
    FGroupHeaderImages: TCustomImageList;
    procedure SetViewStyleEx(AValue: TSGJViewStyle);
    procedure SetTileView();
    procedure EnableTileView();
    procedure SetTileSubcaption(AItem: integer);
    procedure SetTileOpt(Value: TTileOptions);
    procedure SetGroupView(AValue: boolean);
    procedure SetGroups(AValue: TListGroups);
    function GetMyItems: TSGJListItems;
    procedure SetMyItems(AValue: TSGJListItems);
    procedure SetGroupHeaderImages(Value: TCustomImageList);
    procedure DrawDarkGroupHeader();
    procedure SetStyle(AValue: TStyleName);
    procedure doAdvancedCustomDraw(Sender: TCustomListView;
      const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateGroups();
  protected
    procedure DoCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure Loaded; override;
    procedure InsertItem(AItem: TListItem); override;
    function CreateListItems: TListItems; override;
    function CreateListItem: TListItem; override;
    function CustomDraw(const ARect: TRect; AStage: TCustomDrawStage): boolean; override;
  published
    //Style
    property StyleName: TStylename read fStyleName write SetStyle;
    //GroupView
    property GroupView: boolean read fGrupView write SetGroupView;
    //GroupHeaderImages
    property GroupHeaderImages: TCustomImageList
      read fGroupHeaderImages write SetGroupHeaderImages;
    //Groups
    property Groups: TListGroups read fGroups write SetGroups;
    //TileOptions
    property TileOptions: TTileOptions read fTileOpt write SetTileOpt;
    //Items
    property Items: TSGJListItems read GetMyItems write SetMyItems;
    property Align;
    property AllocBy;
    property Anchors;
    property AutoSort;
    property AutoSortIndicator;
    property AutoWidthLastColumn; // resize last column to fit width of TListView
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
    //property ViewStyle;
    property ViewStyleEx: TSGJViewStyle read fViewStyle write SetViewStyleEx;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
    property OnCreateItemClass;
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

{$IFDEF MSWINDOWS}
const
  LVS_TILEVIEW = $0004;
  LVGS_COLLAPSIBLE = $00000008;
  LVGS_NOHEADER = $00000004;
  ID_SUB_LISTVIEW = 100;
  LVM_GETGROUPRECT = LVM_FIRST + 98;
  LVM_GETGROUPSTATE  = LVM_FIRST + 92;
  LVGGR_GROUP = 0; // Entire group
  LVGGR_HEADER = 1; // Header only
  LVGGR_LABEL = 2; // Label only
  LVGGR_SUBSETLINK = 3; // Subset link only

{$ENDIF}
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJListView]);
end;
{$IFDEF MSWindows}

function IsGroupCollapsed(hwnd: HWND; GroupID: Integer): Boolean;
var
  State: UINT;
begin
  Result := False;
  // Send message to get the group state
  State := SendMessage(hwnd, LVM_GETGROUPSTATE, GroupID, LVGS_COLLAPSED);

  // If LVGS_COLLAPSED bit is set, the group is collapsed
  Result := (State and LVGS_COLLAPSED) <> 0;
end;

function ListView_GetGroupRect(hwnd: HWND; iGroupId, iType: integer;
  var prc: TRect): integer;
begin
  prc.Top := iType;
  Result := SendMessage(hwnd, LVM_GETGROUPRECT, WPARAM(iGroupId), LPARAM(@prc));
end;

function ListViewWindowProcSubclassed(Window: HWND; Msg: UINT;
  _wParam: Windows.WPARAM; _lParam: Windows.LPARAM; uISubClass: UINT_PTR;
  dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  NMHdr: LCLType.PNMHDR;
  NMCustomDraw: PNMLVCUSTOMDRAW;
  DC: HDC;
  R: TRect;
  OldFont: HFONT;
  Font: TFont;
begin
  if Msg = WM_NOTIFY then
  begin
    NMHdr := LCLType.PNMHDR(_LParam);
    if NMHdr^.code = NM_CUSTOMDRAW then
    begin
      NMCustomDraw := PNMLVCUSTOMDRAW(_LParam);
      case NMCustomDraw^.nmcd.dwDrawStage of
        CDDS_PREPAINT:
        begin
          Result := CDRF_NOTIFYITEMDRAW;
          exit;
        end;
        CDDS_ITEMPREPAINT:
        begin
          SetTextColor(NMCustomDraw^.nmcd.hdc, RGBToColor(255, 255, 255));
          Result := CDRF_NEWFONT;
          exit;
        end;
      end;
    end;
  end;
  Result := DefSubclassProc(Window, Msg, _WParam, _LParam);
end;
{$ENDIF}
{$region TListGroup}
procedure TListGroup.SetHeader(AValue: TTranslateString);
begin
  if FHeader <> AValue then
  begin
    FHeader := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;

end;

procedure TListGroup.SetImage(AValue: TImageIndex);
begin
  if FTitleImage <> AValue then
  begin
    FTitleImage := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.SetFooter(AValue: TTranslateString);
begin
  if FFooter <> AValue then
  begin
    FFooter := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.SetSubtitle(AValue: TTranslateString);
begin
  if FSubTitle <> AValue then
    FSubTitle := AValue;
end;

procedure TListGroup.SetGroupID(AValue: integer);
begin
  if FGroupId <> AValue then
  begin
    FGroupId := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.SetHeaderAlign(AValue: TAlignment);
begin
  if FHeaderAlign <> AValue then
  begin
    FHeaderAlign := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.SetFooterAlign(AValue: TAlignment);
begin
  if FFooterAlign <> AValue then
  begin
    FFooterAlign := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.SetState(AValue: TListGroupStateSet);
begin
  if FState <> AValue then
  begin
    FState := AValue;
    TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
  end;
end;

procedure TListGroup.Assign(ASource: TPersistent);
var
  GR: TListGroup;
begin
  if ASource is TListGroup then
  begin
    GR := TListGroup(ASource);
    FGroupID := GR.GroupID;
    FHeader := GR.Header;
    FSubtitle := GR.Subtitle;
    FFooter := GR.Footer;
    FFooterAlign := GR.FooterAlign;
    FHeaderAlign := GR.HeaderAlign;
  end
  else
    inherited Assign(ASource);
end;

constructor TListGroup.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  fGroupID := TListGroups(Collection).NextGroupID;
  fTitleImage := -1;
  TListGroups(Collection).SetNextGroup;
  TSGJListView(TListGroups(Collection).Owner).UpdateGroups();
end;
{$EndRegion}

{$Region TListGroups}
procedure TListGroups.SetNextGroup();
begin
  Inc(fNextGroupId);
end;

constructor TListGroups.Create(AOwner: TCustomListView);
begin
  FOwner := AOwner;
  fNextGroupId := 0;
  inherited Create(TListGroup);
end;

procedure TListGroups.UpdateGroups();
begin
  TSGJListView(Owner).UpdateGroups();
end;

function TListGroups.Add: TListGroup;
begin

  Result := TListGroup(inherited Add);

end;

procedure TListGroups.Assign(ASource: TPersistent);
var
  gr: TListGroup;
  I: integer;
begin
  if (ASource = nil) or (ASource = Self) then exit;
  if ASource is TCollection then
  begin
    for I := 0 to TCollection(ASource).Count - 1 do
    begin
      gr := Add;
      gr.Assign(TCollection(ASource).Items[I]);
    end;
  end
  else
    inherited Assign(ASource);
end;

procedure TListGroups.SetItem(const AIndex: integer; const AValue: TListGroup);
begin
  inherited SetItem(AIndex, AValue);
end;

function TListGroups.GetItem(const AIndex: integer): TListGroup;
begin
  Result := TListGroup(inherited GetItem(AIndex));
end;
{$EndRegion}
procedure TSGJListView.SetTileSubcaption(AItem: integer);
{$IFDEF MSWindows}
var
  TileInfo: TLVTILEINFO;
  i, j, k: integer;
{$ENDIF}
begin
  {$IFDEF MSWindows}
  //Create Columns
  SetLength(fTileColumns, TileOptions.SubLineCount + 1);
  for i := 0 to TileOptions.SubLineCount do
    fTileColumns[i] := i;
  //Set Tile Info for all items

  k := Items.Count - 1;
  j := 0;
  if AItem <> -1 then
  begin
    k := AItem;
    j := AItem;
  end;
  for i := j to k do
  begin
    FillChar(TileInfo, SizeOf(TileInfo), 0);
    TileInfo.cbSize := SizeOf(TileInfo);
    TileInfo.iItem := i;
    TileInfo.cColumns := TileOptions.SubLineCount;
    TileInfo.puColumns := @fTileColumns[1];

    SendMessage(Handle, LVM_SETTILEINFO, 0, LPARAM(@TileInfo));
  end;
  {$ENDIF}

end;

procedure TSGJListView.EnableTileView();
{$IFDEF MSWindows}
var
  lvSize: TLVTILEVIEWINFO;
{$ENDIF}
begin
  {$IFDEF MSWindows}

  // Change style to tile view
  SetWindowLong(self.Handle, GWL_STYLE,
    GetWindowLong(self.Handle, GWL_STYLE) or LVS_TILEVIEW);

  // Fill tile view info
  //FillChar(lvSize, SizeOf(lvSize), 0);
  lvSize.cbSize := SizeOf(lvSize);
  lvSize.dwMask := LVTVIM_TILESIZE or LVTVIM_COLUMNS or LVTVIM_LABELMARGIN;
  lvSize.cLines := TileOptions.SubLineCount;
  case TileOptions.SizeType of
    tstAutoSize: lvSize.dwFlags := LVTVIF_AUTOSIZE;
    tstFixedSize: lvSize.dwFlags := LVTVIF_FIXEDSIZE;
    tstFixedHeight: lvSize.dwFlags := LVTVIF_FIXEDHEIGHT;
    tstFixedWidth: lvSize.dwFlags := LVTVIF_FIXEDWIDTH;
  end;
  //Set Tile Size
  lvSize.sizeTile.cx := TileOptions.Width;
  lvSize.sizeTile.cy := TileOptions.Height;


  lvSize.rcLabelMargin := Rect(TileOptions.LabelMargins.Left,
    TileOptions.LabelMargins.Top, TileOptions.LabelMargins.Right,
    TileOptions.LabelMargins.Bottom);
  // Apply tile size
  SendMessage(self.Handle, LVM_SETTILEVIEWINFO, 0, LPARAM(@lvSize));
  {$ENDIF}
end;

procedure TSGJListView.SetTileView();
begin
  EnableTileView();



  {$IFDEF MSWindows}
  SendMessage(self.Handle, LVM_SETVIEW, LV_VIEW_TILE, 0);
  {$ENDIF}
  SetTileSubcaption(-1);
end;

constructor TSGJListView.Create(AOwner: TComponent);
var
  AColumn: TListColumn;
begin
  inherited Create(AOwner);
  onCreateItemClass := @DoCreateItemClass;
  onAdvancedCustomDraw := @doAdvancedCustomDraw;

  fTileOpt := TTileOptions.Create;
  ftileOpt.fLabelMargins := TTileLabelMargins.Create;
  fGroups := TListGroups.Create(self);
  {$IFDEF MSWindows}
  if fGrupView then
  begin
    SetGroupView(fGrupView);
    UpdateGroups();
  end;
  if HandleAllocated then
    if fGrupView then
      SendMessage(Handle, LVM_ENABLEGROUPVIEW, 1, 0);
  {$ENDIF}

end;

procedure TSGJListView.doAdvancedCustomDraw(Sender: TCustomListView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: boolean);
begin

end;

destructor TSGJListView.Destroy;
begin
  ftileOpt.fLabelMargins.Free;
  fTileOpt.Free;
  fGroups.Free;
  inherited;
end;

procedure TSGJListView.Loaded;
{$IFDEF MSWindows}
var
  h: THandle;
  i: integer;
{$ENDIF}
begin
  inherited Loaded;

  {$IFDEF MSWindows}
  fOldColor:=Color;
  if Font.Color=clDefault then
  fOldFontColor:=GetDefaultColor(dctFont)
  else
  fOldFontColor:=Font.Color;
  SetStyle(StyleName);

  if fGrupView then
  begin
    UpdateGroups();
    SendMessage(Handle, LVM_ENABLEGROUPVIEW, 1, 0);
  end;
  {$ENDIF}


  if FViewStyle = LVSTile then SetTileView;
end;

procedure TSGJListView.SetTileOpt(Value: TTileOptions);
begin
  FTileOpt.Assign(Value);
end;

procedure TSGJListView.InsertItem(AItem: TListItem);
begin
  inherited InsertItem(AItem);
  if HandleAllocated then
  begin
    SetTileSubcaption(Items.Count - 1);
    UpdateGroups;
  end;
end;

procedure TSGJListView.SetViewStyleEx(AValue: TSGJViewStyle);
begin
  FViewStyle := AValue;
  case AValue of
    lvsReport: begin
      ViewStyle := vsList;
      ViewStyle := vsReport;
    end;
    lvsIcon: ViewStyle := vsIcon;
    lvsSmallIcon: ViewStyle := vsSmallIcon;
    lvsList: ViewStyle := vsList;
    lvsTile: begin
      ViewStyle := vsReport;
      if HandleAllocated then SetTileView;
    end;
  end;

end;

procedure TSGJListView.SetGroupView(AValue: boolean);
begin
  fGrupView := AValue;
  {$IfDef MSWindows}
  if AValue = True then
  begin
    SendMessage(self.Handle, LVM_ENABLEGROUPVIEW, 1, 0);
    UpdateGroups()
  end
  else
    SendMessage(self.Handle, LVM_ENABLEGROUPVIEW, 0, 0);
  {$EndIf}
end;

procedure TSGJListView.SetGroups(AValue: TListGroups);
begin
  if AValue = FGroups then exit;
  FGroups.Assign(AValue);
end;

procedure TSGJListView.UpdateGroups();
{$IfDef MSWindows}
var
  LvGroup: TLVGROUP;
  i: integer;
  LVITEMW: TLVITEMW;
{$EndIf}
begin
  {$IfDef MSWindows}
  if HandleAllocated then
    ListView_RemoveAllGroups(Handle);
  if HandleAllocated then
    SendMessage(self.Handle, LVM_ENABLEGROUPVIEW, 0, 0);



  //if Groups.Count > 0 then
  for i := Groups.Count - 1 downto 0 do
  begin
    //ListView_RemoveGroup(Handle, Groups.Items[i].GroupID);

    FillChar(LvGroup, SizeOf(TLVGROUP), 0);
    with LvGroup do
    begin
      cbSize := SizeOf(TLVGROUP);
      mask := LVGF_ALIGN or LVGF_GROUPID or LVGF_STATE or LVGF_HEADER;

      if Groups.Items[i].Footer <> '' then
        mask := mask or LVGF_FOOTER;

      if Groups.Items[i].Subtitle <> '' then
        mask := mask or LVGF_SUBTITLE;

      if Groups.Items[i].fTitleImage > -1 then
      begin
        mask := mask or LVGF_TITLEIMAGE;
        iTitleImage := Groups.Items[i].TitleImage;
      end;

      pszHeader := pwidechar(WideString(Groups.Items[i].Header));
      cchHeader := Length(LvGroup.pszHeader);

      pszFooter := pwidechar(WideString(Groups.Items[i].Footer));
      cchFooter := Length(LvGroup.pszFooter);

      pszSubtitle := pwidechar(WideString(Groups.Items[i].Subtitle));
      cchSubtitle := Length(LvGroup.pszSubtitle);

      iGroupId := Groups.Items[i].GroupID;


      if lgsNormal in Groups.Items[i].State then
        state := state or LVGS_NORMAL;

      if lgsCollapsible in Groups.Items[i].State then
        state := state or LVGS_COLLAPSIBLE;

      if lgsCollapsed in Groups.Items[i].State then
        state := state or LVGS_COLLAPSED;

      if lgsNoHeader in Groups.Items[i].State then
        state := state or LVGS_NOHEADER;

      if lgsHidden in Groups.Items[i].State then
        state := state or LVGS_HIDDEN;

      case Groups.Items[i].HeaderAlign of
        taLeftJustify: uAlign := LVGA_HEADER_LEFT;
        taCenter: uAlign := LVGA_HEADER_CENTER;
        taRightJustify: uAlign := LVGA_HEADER_RIGHT;
      end;
      case Groups.Items[i].FooterAlign of
        taLeftJustify: uAlign := uAlign or LVGA_FOOTER_LEFT;
        taCenter: uAlign := uAlign or LVGA_FOOTER_CENTER;
        taRightJustify: uAlign := uAlign or LVGA_FOOTER_RIGHT;
      end;

      if (Groups.Items[i].HeaderAlign = taCenter) and
        (Groups.Items[i].TitleImage <> -1) then
      begin
        mask := mask or LVGF_DESCRIPTIONTOP or LVGF_DESCRIPTIONBOTTOM;
        pszHeader := pwidechar('');
        pszSubtitle := pwidechar('');
        pszDescriptionTop := pwidechar(WideString(Groups.Items[i].Header));
        cchDescriptionTop := Length(LvGroup.pszDescriptionTop);
        pszDescriptionBottom := pwidechar(WideString(Groups.Items[i].Subtitle));
        cchDescriptionBottom := Length(LvGroup.pszDescriptionBottom);
      end;

    end;
    SendMessage(Handle, LVM_INSERTGROUP, 0, longint(@LvGroup));

  end;


  for I := 0 to Items.Count - 1 do
  begin
    with LvItemW do
    begin
      FillChar(LvItemW, SizeOf(TLvItemW), 0);
      mask := LVIF_GROUPID;
      iItem := I;
      iGroupId := Items.Item[I].Group;
    end;
    SendMessage(Handle, LVM_SETITEM, 0, longint(@LvItemW));

  end;

  if HandleAllocated then
    if fGrupView then
      SendMessage(Handle, LVM_ENABLEGROUPVIEW, 1, 0);
  {$EndIf}

end;


procedure TSGJListView.DoCreateItemClass(Sender: TCustomListView;
  var ItemClass: TListItemClass);
begin
  inherited;
  ItemClass := TSGJListItem;
end;

function TSGJListView.CreateListItem: TListItem;
begin
  Result := TSGJListItem.Create(Items);
end;

function TSGJListView.CreateListItems: TListItems;
begin
  Result := TSGJListItems.Create(self);

end;

procedure TSGJListView.SetGroupHeaderImages(Value: TCustomImageList);
begin
  if fGroupHeaderImages <> Value then
  begin
    if Value <> nil then
    begin
      fGroupHeaderImages := Value;
      {$IfDef MSWindows}
      ListView_SetImageList(self.Handle, GroupHeaderImages.Handle, LVSIL_GROUPHEADER);
      {$EndIf}
    end
    else
    begin
      fGroupHeaderImages := nil;
      {$IfDef MSWindows}
      ListView_SetImageList(self.Handle, 0, LVSIL_GROUPHEADER);
      {$EndIf}
    end;
  end;

end;

function TSGJListView.GetMyItems: TSGJListItems;
begin
  Result := TSGJListItems(inherited Items);
end;

procedure TSGJListView.SetMyItems(AValue: TSGJListItems);
begin
  Items := AValue;
end;

function TSGJListItems.Add: TSGJListItem;
begin
  Result := TSGJListItem(inherited Add);
end;

function TSGJListItem.IsEqual(const AItem: TSGJListItem): boolean;
begin
  Result := (Caption = AItem.Caption) and (Data = AItem.Data);
end;

procedure TSGJListItems.SetItem(Index: integer; Value: TSGJListItem);
begin
  inherited SetItem(Index, Value);
end;

function TSGJListItems.GetItem(Index: integer): TSGJListItem;
begin
  Result := TSGJListItem(inherited GetItem(Index));
end;

{$Region Stream TSGJListView to LFM with Items.Group}
//Based on code from comctrls - listitems.inc

procedure TSGJListItems.DefineProperties(Filer: TFiler);

  function WriteItems: boolean;
  var
    I: integer;
    Items: TSGJListItems;
  begin
    Items := TSGJListItems(Filer.Ancestor);
    if not Assigned(Items) then
      Result := Count > 0
    else if (Items.Count <> Count) then
      Result := True
    else
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := not TSGJListItem(Item[I]).IsEqual(TSGJListItem(Items[I]));
        if Result then Break;
      end;
    end;
  end;

begin
  Filer.DefineBinaryProperty('LazData', @ReadLazData, @WriteLazData, WriteItems);
end;

procedure TSGJListItems.ReadLazData(Stream: TStream);
var
  I, J: integer;
  ItemInfo: TLazItemInfo;
  ListItem: TListItem;
  Size, ItemCount, SubCount: integer;
  StartPos: int64;

  {$ifdef FPC_BIG_ENDIAN}

    { This is a local redefinition of TStream.ReadAnsiString(), which ensures
      that the string length is read in little-endian order to conform to the
      convention of the remaining resources.
    }
    function stream_readAnsiStringLE(): AnsiString;

    Var
      TheSize : Longint;
      P : PByte ;
    begin
      Stream.ReadBuffer (TheSize,SizeOf(TheSize));
      TheSize := LEtoN(TheSize);
      SetLength(Result,TheSize);
      // Illegal typecast if no AnsiStrings defined.
      if TheSize>0 then
       begin
         Stream.ReadBuffer (Pointer(Result)^,TheSize);
         P:=Pointer(Result)+TheSize;
         p^:=0;
       end
    end { stream_readAnsiStringLE } ;

  {$endif FPC_BIG_ENDIAN}
begin
  Clear;
  StartPos := Stream.Position;
  Size := LEtoN(integer(Stream.ReadDWord));
  ItemCount := LEtoN(integer(Stream.ReadDWord));
  Owner.BeginUpdate;
  try
    for I := 0 to ItemCount - 1 do
    begin
      Stream.ReadBuffer(ItemInfo, SizeOf(ItemInfo));
      ListItem := Add;
      {$ifndef FPC_BIG_ENDIAN}
      ListItem.Caption := Stream.ReadAnsiString();
      {$else}
        ListItem.Caption := stream_readAnsiStringLE;
      {$endif FPC_BIG_ENDIAN}
      TSGJListItem(ListItem).Group := LEtoN(ItemInfo.GroupID);
      ListItem.ImageIndex := LEtoN(ItemInfo.ImageIndex);
      // ListItem.StateIndex := LEtoN(ItemInfo.StateIndex);
      // ListItem.OverlayIndex := LEtoN(ItemInfo.OverlayIndex);

      SubCount := LEtoN(ItemInfo.SubItemCount);
      for J := 0 to SubCount - 1 do
      begin
        {$ifndef FPC_BIG_ENDIAN}
        ListItem.SubItems.Add(Stream.ReadAnsiString);
        {$else}
          ListItem.SubItems.Add(stream_readAnsiStringLE())
        {$endif FPC_BIG_ENDIAN}
      end;

    end;

    //read subitem images
    if Stream.Position < StartPos + Size then
    begin
      for I := 0 to Count - 1 do
      begin
        ListItem := TListItem(Item[I]);
        if ListItem.SubItems = nil then Continue;

        for J := 0 to ListItem.SubItems.Count - 1 do
          ListItem.SubItemImages[J] := LEtoN(integer(Stream.ReadDWord));
      end;
    end;
  finally
    Owner.EndUpdate;
  end;
end;

procedure TSGJListItems.WriteLazData(Stream: TStream);
var
  I, J, Size, L: integer;
  ItemHeader: TItemHeader;
  ItemInfo: TLazItemInfo;
  ListItem: TListItem;

  {$ifdef FPC_BIG_ENDIAN}

    { This is a local redefinition of TStream.WriteAnsiString(), which ensures
      that the string length is written in little-endian order to conform to the
      convention of the remaining resources.
    }
    procedure stream_writeAnsiStringLE(const S: AnsiString);

    Var L : Longint;

    begin
      L:=NtoLE(Length(S));
      Stream.WriteBuffer (L,SizeOf(L));
      Stream.WriteBuffer (Pointer(S)^,L)
    end { stream_writeAnsiStringLE } ;

  {$endif FPC_BIG_ENDIAN}
begin
  Size := SizeOf(ItemHeader);
  for I := 0 to Count - 1 do
  begin
    L := Length(Item[I].Caption) + 4;
    for J := 0 to Item[I].SubItems.Count - 1 do
    begin
      Inc(L, Length(Item[I].SubItems[J]) + 4);
      Inc(L, SizeOf(DWORD));
    end;
    Inc(Size, SizeOf(TLazItemInfo) + L);
  end;

  ItemHeader.Size := NtoLE(Size);
  ItemHeader.Count := NtoLE(Count);
  Stream.WriteBuffer(ItemHeader, SizeOf(ItemHeader));

  for I := 0 to Count - 1 do
  begin
    ListItem := TSGJListItem(Item[I]);
    ItemInfo.GroupID := NtoLE(TSGJListItem(ListItem).Group);
    ItemInfo.ImageIndex := NtoLE(ListItem.ImageIndex);
    ItemInfo.StateIndex := NtoLE(integer(-1)) {StateIndex};
    ItemInfo.OverlayIndex := NtoLE(integer(-1)) {OverlayIndex};
    // don't acces SubItems directly, they will be created
    if ListItem.SubItems = nil then ItemInfo.SubItemCount := 0
    else
      ItemInfo.SubItemCount := NtoLE(ListItem.SubItems.Count);

    Stream.WriteBuffer(ItemInfo, SizeOf(ItemInfo));

    // Write the strings
    {$ifndef FPC_BIG_ENDIAN}
    Stream.WriteAnsiString(ListItem.Caption);
    {$else}
      stream_writeAnsiStringLE(ListItem.Caption);
    {$endif FPC_BIG_ENDIAN}

    for J := 0 to ItemInfo.SubItemCount - 1 do
    begin
      {$ifndef FPC_BIG_ENDIAN}
      Stream.WriteAnsiString(ListItem.SubItems[J]);
      {$else}
        stream_writeAnsiStringLE(ListItem.SubItems[J])
      {$endif FPC_BIG_ENDIAN}
    end;
  end;

  //write SubItem images.
  for I := 0 to Count - 1 do
  begin
    ListItem := TListItem(Item[I]);
    // do not force subitem creation
    if ListItem.SubItems = nil then Continue;
    for J := 0 to ListItem.SubItems.Count - 1 do
    begin
      Stream.WriteDWord(DWord(ListItem.SubItemImages[J]));
    end;
  end;
end;
{$EndRegion}

procedure TSGJListView.DrawDarkGroupHeader();
var
  R, LR: trect;
  i: integer;
  ImgWidth, ImgHeight: integer;
  LMargin, RMargin, TMargn: integer;
  collapse: integer;
begin
  {$IFDEF MSWINDOWS}
  canvas.font.Color := clSilver;
  Canvas.Brush.Color := Color;
  for i := 0 to Groups.Count - 1 do
  begin
    ImgWidth := 0;
    ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_LABEL, R);
    LMargin := R.Left;
    TMargn := R.Top;
    if (GroupHeaderImages <> nil) and (Groups.Items[i].TitleImage <> -1) then
    begin
      ImgWidth := GroupHeaderImages.Width;
      ImgHeight := GroupHeaderImages.Height;
    end;

    if Groups.Items[i].HeaderAlign = taLeftJustify then
    begin

      canvas.TextOut(R.Left + ImgWidth, R.Top, Groups.Items[i].Header);
      if Groups.Items[i].Subtitle <> '' then
        canvas.TextOut(R.Left + ImgWidth, R.Top + Canvas.TextHeight('T'),
          Groups.Items[i].Subtitle);

    end;
    if (Groups.Items[i].HeaderAlign = taCenter) then
    begin
      ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_HEADER, R);
      if lgsCollapsible in Groups.Items[i].State then
        collapse := ScaleX(16, 96)
      else
        collapse := 0;


      if Groups.Items[i].TitleImage = -1 then
      begin
        canvas.TextOut((R.Width - collapse) div 2 -
          (Canvas.TextWidth(Groups.Items[i].Header) div 2), TMargn, Groups.Items[i].Header);
        if Groups.Items[i].Subtitle <> '' then
          canvas.TextOut((R.Width - collapse) div 2 -
            (Canvas.TextWidth(Groups.Items[i].Subtitle) div 2), TMargn + Canvas.TextHeight(
            'T'), Groups.Items[i].Subtitle);
      end
      else
      begin
        //ImgWidth div  2+(R.Width - collapse)- LMargin -((R.Width - collapse) div 4) - (Canvas.TextWidth(Groups.Items[i].Header) div 2)
        canvas.TextOut(10 + (R.Width - collapse) -
          ((R.Width - collapse - ImgWidth) div 4) -
          (Canvas.TextWidth(Groups.Items[i].Header) div 2), TMargn, Groups.Items[i].Header);
        if Groups.Items[i].Subtitle <> '' then
          canvas.TextOut(10 + (R.Width - collapse) -
            ((R.Width - collapse - ImgWidth) div 4) -
            (Canvas.TextWidth(Groups.Items[i].Subtitle) div 2), TMargn + ImgHeight div
            2, Groups.Items[i].Subtitle);
      end;

    end;


    if (Groups.Items[i].HeaderAlign = taRightJustify) then
    begin
      ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_HEADER, R);
      if lgsCollapsible in Groups.Items[i].State then
        collapse := ScaleX(16, 96)
      else
        collapse := 0;
      canvas.TextOut(R.Width - LMargin - (Canvas.TextWidth(Groups.Items[i].Header)) -
        collapse, TMargn, Groups.Items[i].Header);
      if Groups.Items[i].Subtitle <> '' then
        canvas.TextOut(R.Width - LMargin -
          (Canvas.TextWidth(Groups.Items[i].Subtitle)) - collapse, TMargn +
          Canvas.TextHeight('T'), Groups.Items[i].Subtitle);
    end;

    if Groups.Items[i].Footer <> '' then
    begin
      if (Groups.Items[i].FooterAlign = taLeftJustify) then
      begin
        if IsGroupCollapsed(self.Handle,Groups.Items[i].GroupID) then
        begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_HEADER, R);
          canvas.TextOut(12, R.Top + R.Height, Groups.Items[i].Footer);
        end else begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_GROUP, R);
          canvas.TextOut(12, R.Top + R.Height - Canvas.TextHeight('T'), Groups.Items[i].Footer);
        end;
      end
      else
      if (Groups.Items[i].FooterAlign = taCenter) then
      begin
        if IsGroupCollapsed(self.Handle,Groups.Items[i].GroupID) then
        begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_HEADER, R);
          canvas.TextOut(6+R.Width div 2 -
          (Canvas.TextWidth(Groups.Items[i].Footer) div 2), R.Top + R.Height, Groups.Items[i].Footer);
        end else begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_GROUP, R);
          canvas.TextOut(6+R.Width div 2 -
          (Canvas.TextWidth(Groups.Items[i].Footer) div 2), R.Top + R.Height - Canvas.TextHeight('T'), Groups.Items[i].Footer);
        end;
      end
      else
      if (Groups.Items[i].FooterAlign = taRightJustify) then
      begin
        if IsGroupCollapsed(self.Handle,Groups.Items[i].GroupID) then
        begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_HEADER, R);
          canvas.TextOut(R.Width -
          (Canvas.TextWidth(Groups.Items[i].Footer))-2, R.Top + R.Height, Groups.Items[i].Footer);
        end else begin
          ListView_GetGroupRect(handle, Groups.Items[i].GroupID, LVGGR_GROUP, R);
          canvas.TextOut(R.Width -
          (Canvas.TextWidth(Groups.Items[i].Footer))-2, R.Top + R.Height - Canvas.TextHeight('T'), Groups.Items[i].Footer);
        end;
      end;

    end;
  end;
  {$ENDIF}
end;

function TSGJListView.CustomDraw(const ARect: TRect; AStage: TCustomDrawStage): boolean;
var
  R: trect;
begin
  Result := (inherited CustomDraw(ARect, AStage));
  if (AStage = cdPostPaint) and (StyleName = snDark) and (GroupView) then
  begin
    DrawDarkGroupHeader();
  end;

end;

procedure TSGJListView.SetStyle(AValue: TStyleName);
var
  h: THandle;
begin
  fStyleName := AValue;
  {$IFDEF MSWINDOWS}
  if fStyleName = snDefault then
  begin
    RemoveWindowSubclass(Handle, @ListViewWindowProcSubclassed, ID_SUB_LISTVIEW);
    SetWindowTheme(handle, '', nil);
    h := ListView_GetHeader(handle);
    SetWindowTheme(h, '', nil);
    Color:=fOldColor;
    Font.Color:=fOldFontColor;
  end;
  if fStyleName = snDark then
  begin
    SetWindowTheme(self.handle, 'DarkMode_Explorer', nil);
    h := ListView_GetHeader(handle);
    SetWindowTheme(h, 'DarkMode_ItemsView', nil);
    SetWindowSubclass(self.Handle, @ListViewWindowProcSubclassed, ID_SUB_LISTVIEW, 0);
    Color:=$002a2a2a;
    Font.Color:=clWhite;
  end;

  if fStyleName = snLight then
  begin
    SetWindowTheme(handle, 'Explorer', nil);
    h := ListView_GetHeader(handle);
    SetWindowTheme(h, 'Explorer', nil);
    RemoveWindowSubclass(Handle, @ListViewWindowProcSubclassed, ID_SUB_LISTVIEW);
    Color:=clWhite;
    Font.Color:=clBlack;
  end;
  {$ENDIF}
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.ListView.lrs}
{$ENDIF}

end.
