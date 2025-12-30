{
*****************************************************************************
This file is part of SGJ Controls

  Author: Grzegorz Skulimowski
  Web:    www.hiperapps.com
  Web:    www.sgjps.com
  Mail:   sgj@sgjps.com

  History
    Created: 2025/12/30



*****************************************************************************
}

unit SGJ.Win32popupMenu;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, uxTheme,
  {$EndIf}
  {$IFDEF FPC}
  LResources,LCLProc, LMessages,
  {$ENDIF}
  SysUtils, Menus, Graphics, Forms, Controls, Dialogs,
  types, Classes;


  {$IFDEF MSWINDOWS}
type
  TSGJNativeWin32Menu = class;
  {$EndIf}

type
  TSGJWin32Menu = class(TComponent)
  private
    fEnabled: boolean;
    fOwner: TForm;
    fCustomColors: boolean;
    fColorMenu: TColor;
    fColorText: TColor;
    fColorBG: TColor;
    fColorBGHot: TColor;
    fColorBGSelected: TColor;
    fColorBorder: TColor;
    fColorTextDisabled: TColor;
    Hooks: TList;
    fFormMainMenu: TMainMenu;
    {$IFDEF MSWINDOWS}
    Win32Menu: TSGJNativeWin32Menu;
    WinPopupMenu: TSGJNativeWin32Menu;
    procedure HookAllControls(Parent: TWinControl; AHooks: TList);
    {$EndIF}
    procedure SetEnabled(AValue: boolean);
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MainMenuCustomColors: boolean read fCustomColors write fCustomColors;
    property MainMenuColorMenu: TColor read fColorMenu write fColorMenu;
    property MainMenuColorText: TColor read fColorText write fColorText;
    property MainMenuColorBG: TColor read fColorBG write fColorBG;
    property MainMenuColorBGHot: TColor read fColorBGHot write fColorBGHot;
    property MainMenuColorBGSelected: TColor read fColorBGSelected
      write fColorBGSelected;
    property MainMenuColorBorder: TColor read fColorBorder write fColorBorder;
    property MainMenuColorTextDisabled: TColor
      read fColorTextDisabled write fColorTextDisabled;
    property Enabled: boolean read fEnabled write SetEnabled;
  end;

  {$IFDEF MSWINDOWS}
type
  PUAHMENU = ^UAHMENU;

  tagUAHMENU = record
    _hMenu: HMENU;
    _hdc: HDC;
    uFlags: UINT;
  end;
  UAHMENU = tagUAHMENU;

type
  UAHMENUITEMMETRICS = record
    case integer of
      0: (
        rgsizeBar: array[0..1] of record
          cx: DWORD;
          cy: DWORD;
          end);
      1: (
        rgsizePopup: array[0..3] of record
          cx: DWORD;
          cy: DWORD;
          end);
  end;

type
  UAHMENUPOPUPMETRICS = record
    rgcx: array[0..3] of cardinal;
    fUpdateMaxWidths: cardinal;
  end;

  tagUAHMENUITEM = record
    iPosition: integer;
    umim: UAHMENUITEMMETRICS;
    umpm: UAHMENUPOPUPMETRICS;
  end;
  UAHMENUITEM = tagUAHMENUITEM;

  UAHDRAWMENUITEM = record
    dis: DRAWITEMSTRUCT;
    um: UAHMENU;
    umi: UAHMENUITEM;
  end;
  PUAHDRAWMENUITEM = ^UAHDRAWMENUITEM;

const
  WM_UAHDRAWMENU = $0091;
  WM_UAHDRAWMENUITEM = $0092;
  WM_UAHMEASUREMENUITEM = $0093;
  OBJID_MENU = $FFFFFFFD;

type
  TSGJNativeWin32Menu = class
  private
    fPopupMenu: TMenu;
    fHandle: THandle;
    fForm: TForm;
  public
    CustomColors: boolean;
    ColorMenu: TColor;
    ColorText: TColor;
    ColorBG: TColor;
    ColorBGHot: TColor;
    ColorBGSelected: TColor;
    ColorBorder: TColor;
    ColorTextDisabled: TColor;
    constructor Create(AForm: TForm);
    destructor Destroy; override;
    procedure CreateMenu(APopup: TMenu);
  end;

type
  TWndProcHook = class
  private
    FControl: TWinControl;
    FOldProc: TWndMethod;
    FWin32Menu: TSGJWin32Menu;
    procedure NewWndProc(var Msg: TLMessage);
  public
    constructor Create(AControl: TWinControl; AW32Menu: TSGJWin32Menu);
  end;

  var
  PrevWndProc: Windows.WNDProc;
  {$EndIf}



procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJWin32Menu]);
end;

{$IFDEF MSWINDOWS}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT; stdcall;
var
  i: integer;

  R: TRect;
  rcWindow: TRect;
  _Brush: HBRUSH;
  mbi: MENUBARINFO;
  pUDM: PUAHMENU;

  pUDMI: PUAHDRAWMENUITEM;
  g_brItemBackground, g_brItemBackgroundHot, g_brItemBackgroundSelected,
  g_brItemBorder: HBRUSH;
  pbrBackground, pbrBorder: ^HBRUSH;
  menuString: array[0..255 - 1] of widechar;
  mii: MENUITEMINFOW;
  dwFlags: DWORD;
  iTextStateID, iBackgroundStateID: integer;
  opts: TDTTOPTS;
  menuTheme: htheme;
  W32Menu: TSGJWin32Menu;

  procedure ExecuteSubItems(AItem: TMenuItem);
  var
    i: integer;
  begin
    for i := 0 to AItem.Count - 1 do
      if LOWORD(wParam) = AItem.Items[i].Command then
        AItem.Items[i].Click
      else
      if AItem.Count > 0 then
        ExecuteSubItems(AItem.Items[i]);
  end;

begin
  W32Menu := TSGJWin32Menu(GetWindowLongPtr(AhWnd, GWLP_USERDATA));
  if uMsg = WM_CONTEXTMENU then
     if W32Menu.Enabled then
     W32Menu.WinPopupMenu.CreateMenu(TForm(W32Menu.fOwner).PopupMenu);
  if uMsg = WM_COMMAND then     ;
  begin
    if (lParam = 0) and (HIWORD(wParam) = 0) then
    begin
      if W32Menu.Enabled then
      if W32Menu.Win32Menu <> nil then
        if W32Menu.Win32Menu.fPopupMenu is TMainMenu then
          for i := 0 to W32Menu.Win32Menu.fPopupMenu.Items.Count - 1 do
            if LOWORD(wParam) = W32Menu.Win32Menu.fPopupMenu.Items[i].Command then
              W32Menu.Win32Menu.fPopupMenu.Items[i].Click
            else
            if W32Menu.Win32Menu.fPopupMenu.Items[i].Count > 0 then
              ExecuteSubItems(W32Menu.Win32Menu.fPopupMenu.Items[i]);

      if W32Menu.WinPopupMenu <> nil then
        if W32Menu.WinPopupMenu.fPopupMenu is TPopupMenu then
          for i := 0 to W32Menu.WinPopupMenu.fPopupMenu.Items.Count - 1 do
            if LOWORD(wParam) = W32Menu.WinPopupMenu.fPopupMenu.Items[i].Command then
              W32Menu.WinPopupMenu.fPopupMenu.Items[i].Click
            else
            if W32Menu.WinPopupMenu.fPopupMenu.Items[i].Count > 0 then
              ExecuteSubItems(W32Menu.WinPopupMenu.fPopupMenu.Items[i]);
    end;
  end;
  if W32Menu.Win32Menu.CustomColors then
  begin
    if uMsg = WM_UAHDRAWMENU then
    begin
      pUDM := PUAHMENU(lParam);

      FillChar(R, SizeOf(R), 0);
      mbi.cbSize := SizeOf(mbi);
      GetMenuBarInfo(ahWnd, -3, 0, @mbi);

      GetWindowRect(ahWnd, rcWindow);

      R := mbi.rcBar;
      Types.OffsetRect(R, -rcWindow.left, -rcWindow.top);

      _Brush := CreateSolidBrush(ColorToRGB(W32Menu.Win32Menu.ColorMenu));
      FillRect(pUDM^._hdc, R, _Brush);


      Exit;
    end;

    if uMsg = WM_UAHDRAWMENUITEM then
    begin
      pUDMI := PUAHDRAWMENUITEM(lParam);


      g_brItemBackground := CreateSolidBrush(ColorToRGB(W32Menu.Win32Menu.ColorBG));
      g_brItemBackgroundHot :=
        CreateSolidBrush(ColorToRGB(W32Menu.Win32Menu.ColorBGHot));
      g_brItemBackgroundSelected :=
        CreateSolidBrush(ColorToRGB(W32Menu.Win32Menu.ColorBGSelected));
      g_brItemBorder := CreateSolidBrush(ColorToRGB(W32Menu.Win32Menu.ColorBorder));

      pbrBackground := @g_brItemBackground;
      pbrBorder := @g_brItemBackground;

      ZeroMemory(@menuString, SizeOf(menuString));
      ZeroMemory(@mii, SizeOf(mii));
      mii.cbSize := SizeOf(mii);
      mii.fMask := MIIM_STRING;
      mii.dwTypeData := @menuString;
      mii.cch := Length(menuString) - 1;

      GetMenuItemInfoW(pUDMI^.um._hMenu, pUDMI^.umi.iPosition, True, @mii);

      dwFlags := DT_CENTER or DT_SINGLELINE or DT_VCENTER;

      iTextStateID := 0;
      iBackgroundStateID := 0;

      if ((pUDMI^.dis.itemState and ODS_GRAYED) <> 0) or
        ((pUDMI^.dis.itemState and ODS_DISABLED) <> 0) then
      begin
        iTextStateID := MBI_DISABLED;
        iBackgroundStateID := MBI_DISABLED;
      end
      else
      begin

        if ((pUDMI^.dis.itemState and ODS_INACTIVE) <> 0) or
          ((pUDMI^.dis.itemState and ODS_DEFAULT) <> 0) then
        begin
          iTextStateID := MBI_NORMAL;
          iBackgroundStateID := MBI_NORMAL;
        end;
        if (pUDMI^.dis.itemState and ODS_HOTLIGHT) <> 0 then
        begin
          iTextStateID := MBI_HOT;
          iBackgroundStateID := MBI_HOT;

          pbrBackground := @g_brItemBackgroundHot;
          pbrBorder := @g_brItemBorder;
        end;
        if (pUDMI^.dis.itemState and ODS_SELECTED) <> 0 then
        begin
          iTextStateID := MBI_PUSHED;
          iBackgroundStateID := MBI_PUSHED;

          pbrBackground := @g_brItemBackgroundSelected;
          pbrBorder := @g_brItemBorder;
        end;

        if (pUDMI^.dis.itemState and ODS_NOACCEL) <> 0 then
          dwFlags := dwFlags or DT_HIDEPREFIX;

      end;


      menuTheme := OpenThemeData(ahWnd, 'Menu');

      ZeroMemory(@opts, SizeOf(opts));
      opts.dwSize := SizeOf(opts);
      opts.dwFlags := DTT_TEXTCOLOR;
      opts.crText := ColorToRGB(W32Menu.Win32Menu.ColorText);

      if iTextStateID <> MBI_DISABLED then
        opts.crText := ColorToRGB(W32Menu.Win32Menu.ColorText)
      else
        opts.crText := ColorToRGB(W32Menu.Win32Menu.ColorTextDisabled);

      FillRect(pUDMI^.um._hdc, pUDMI^.dis.rcItem, pbrBackground^);
      FrameRect(pUDMI^.um._hdc, pUDMI^.dis.rcItem, pbrBorder^);
      DrawThemeTextEx(menuTheme, pUDMI^.um._hdc, MENU_BARITEM, MBI_NORMAL,
        pwidechar(menuString), mii.cch, dwFlags, @pUDMI^.dis.rcItem, @opts);
      CloseThemeData(menuTheme);
      exit;
    end;

  end;

  Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;


constructor TWndProcHook.Create(AControl: TWinControl; AW32Menu: TSGJWin32Menu);
begin
  inherited Create;
  FControl := AControl;

  FOldProc := AControl.WindowProc;
  AControl.WindowProc := @NewWndProc;
  FWin32Menu := AW32Menu;
end;


procedure TSGJWin32Menu.HookAllControls(Parent: TWinControl; AHooks: TList);
var
  i: integer;
begin
  for i := 0 to Parent.ControlCount - 1 do
  begin
    if Parent.Controls[i] is TWinControl then
    begin
      AHooks.Add(TWndProcHook.Create(TWinControl(Parent.Controls[i]), self));
      HookAllControls(TWinControl(Parent.Controls[i]), AHooks);
    end;
  end;
end;

procedure TWndProcHook.NewWndProc(var Msg: TLMessage);
begin
  if FWin32Menu.fEnabled then
    if Msg.msg = WM_CONTEXTMENU then
    begin
      if (FControl is TWinControl) then
        if TWinControl(FControl).PopupMenu <> nil then
        begin
          FWin32Menu.WinPopupMenu.CreateMenu(TWinControl(FControl).PopupMenu);
          exit;
        end;
    end;

  if Assigned(FOldProc) then FOldProc(Msg);
end;
{$EndIf}

constructor TSGJWin32Menu.Create(AOwner: TComponent);
begin
  inherited;
  {$IfDef MSWindows}
  fOwner := TForm(AOwner);
  MainMenuColorMenu := $002A2A2A;
  MainMenuColorText := clWhite;
  MainMenuColorBG := $002A2A2A;
  MainMenuColorBGHot := $004D4D4D;
  MainMenuColorBGSelected := $004D4D4D;
  MainMenuColorBorder := $004D4D4D;
  MainMenuColorTextDisabled := ClGray;
  {$ENDIF}
end;

destructor TSGJWin32Menu.Destroy;
var
  i: integer;
begin
  {$IfDef MSWindows}
  if not (csDesigning in ComponentState) then
  begin
    SetWindowLongPtr(fOwner.Handle, GWLP_WNDPROC, PtrUInt(PrevWndProc));
    for i := 0 to Hooks.Count - 1 do
      TObject(Hooks[i]).Free;
    Hooks.Free;
    Win32Menu.Free;
    WinPopupMenu.Free;
  end;
  {$ENDIF}

  inherited Destroy;
end;

procedure TSGJWin32Menu.Loaded;
begin
  inherited Loaded;
  {$IfDef MSWindows}
  if csDesigning in ComponentState then Exit;
  begin
    SetWindowLongPtr(fOwner.Handle, GWLP_USERDATA, PtrInt(Self));
    Win32Menu := TSGJNativeWin32Menu.Create(fOwner);
    WinPopupMenu := TSGJNativeWin32Menu.Create(fOwner);
    if fOwner.Menu <> nil then
      Win32Menu.CreateMenu(fOwner.Menu);

    Hooks := TList.Create;
    HookAllControls(fOwner, Hooks);
    PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(fOwner.Handle,
      GWL_WNDPROC, PtrUInt(@WndCallback)));

    Win32Menu.CustomColors := MainMenuCustomColors;
    Win32Menu.ColorMenu := MainMenuColorMenu;
    Win32Menu.ColorText := MainMenuColorText;
    Win32Menu.ColorBG := MainMenuColorBG;
    Win32Menu.ColorBGHot := MainMenuColorBGHot;
    Win32Menu.ColorBGSelected := MainMenuColorBGSelected;
    Win32Menu.ColorBorder := MainMenuColorBorder;
    Win32Menu.ColorTextDisabled := MainMenuColorTextDisabled;
    if not fEnabled then SetEnabled(false);
   end;
  {$ENDIF}
end;

procedure TSGJWin32Menu.SetEnabled(AValue: boolean);
var
  i: integer;
begin
  fEnabled := AValue;

  {$IFDEF MSWINDOWS}
  if fEnabled = false then
      if fOwner.Menu <> nil then
      begin
        fFormMainMenu := fOwner.Menu;
        fOwner.Menu := nil;
        fOwner.Menu := fFormMainMenu;
      end;

   if fEnabled then
      if fOwner.Menu <> nil then
        Win32Menu.CreateMenu(fOwner.Menu);
   {$ENDIF}
end;

{$IFDEF MSWINDOWS}
destructor TSGJNativeWin32Menu.Destroy;
begin
  inherited;
end;

constructor TSGJNativeWin32Menu.Create(AForm: TForm);
begin
  fForm := AForm;
  fHandle := AForm.Handle;
end;

function CreatePopup(AMenuItem: TMenuItem; APopup: TMenu): HMenu;
var
  popupMenu: HMenu;
  i: integer;
  Bmp: TBitmap;
  ShortCut: string;
begin
  popupMenu := Windows.CreatePopupMenu;
  for i := 0 to AMenuItem.Count - 1 do
  begin
    if AMenuItem.Items[i].ShortCut <> 0 then
      Shortcut := #9 + ShortCutToText(AMenuItem.Items[i].ShortCut)
    else
      Shortcut := '';
    if AMenuItem.Items[i].Caption = '-' then
      AppendMenu(popupMenu, MF_SEPARATOR, 0, nil)
    else
    if AMenuItem.Items[i].Enabled = False then
      AppendMenuW(popupMenu, MF_STRING or MF_Disabled, AMenuItem.Items[i].Command,
        pwidechar(WideString(AMenuItem.Items[i].Caption)))
    else
    if AMenuItem.Items[i].Count > 0 then
      AppendMenuW(popupMenu, MF_POPUP,
        CreatePopup(AMenuItem.Items[i], APopup),
        pwidechar(WideString(AMenuItem.Items[i].Caption))
        )
    else
    if AMenuItem.Items[i].Checked = True then
      AppendMenuW(popupMenu, MF_STRING or MF_Checked, AMenuItem.Items[i].Command,
        pwidechar(WideString(AMenuItem.Items[i].Caption) + Shortcut))
    else
      AppendMenuW(popupMenu, MF_STRING, AMenuItem.Items[i].Command,
        pwidechar(WideString(AMenuItem.Items[i].Caption) + Shortcut));

    if (AMenuItem.Items[i].ImageIndex <> -1) and (Assigned(APopup.Images)) then
    begin
      Bmp := TBitmap.Create;
      bmp.SetSize(ScaleX(16, 96), ScaleX(16, 96));
      bmp.PixelFormat := pf32bit;

      if AMenuItem.Items[i].Enabled = False then
        APopup.Images.DrawForPPI(bmp.Canvas, 0, 0,
          AMenuItem.Items[i].ImageIndex, 16, Forms.Screen.PixelsPerInch, 1, False);
      if (AMenuItem.Items[i].Enabled) and (AMenuItem.Items[i].Caption <> '-') then
        APopup.Images.DrawForPPI(bmp.Canvas, 0, 0,
          AMenuItem.Items[i].ImageIndex, 16, Forms.Screen.PixelsPerInch, 1, True);

      SetMenuItemBitmaps(popupMenu, i, MF_BYPOSITION,
        bmp.ReleaseHandle, bmp.handle);
      bmp.Free;
    end;

  end;

  Result := popupMenu;
end;

procedure TSGJNativeWin32Menu.CreateMenu(APopup: TMenu);
var
  pt: TPoint;
  //MII: TMenuItemInfo;
  Bmp: TBitmap;
  i: integer;
  popupMenu: HMenu;
  Shortcut: string;
begin
  fPopupMenu := APopup;
  if APopup is TMainMenu then
    popupMenu := Windows.CreateMenu()
  else
    popupMenu := Windows.CreatePopupMenu;

  if popupMenu = 0 then
    Exit;

  for i := 0 to fPopupMenu.Items.Count - 1 do
  begin
    if fPopupMenu.Items[i].ShortCut <> 0 then
      Shortcut := #9 + ShortCutToText(fPopupMenu.Items[i].ShortCut)
    else
      Shortcut := '';

    if fPopupMenu.Items[i].Caption = '-' then
      AppendMenu(popupMenu, MF_SEPARATOR, 0, nil)
    else
    if fPopupMenu.Items[i].Enabled = False then
      AppendMenuW(popupMenu, MF_STRING or MF_Disabled, fPopupMenu.Items[i].Command,
        pwidechar(WideString(fPopupMenu.Items[i].Caption)))
    else
    if fPopupMenu.Items[i].Count > 0 then
      AppendMenuW(popupMenu, MF_POPUP,
        CreatePopup(fPopupMenu.Items[i], fPopupMenu),
        pwidechar(WideString(fPopupMenu.Items[i].Caption))
        )
    else
    if fPopupMenu.Items[i].Checked = True then
      AppendMenuW(popupMenu, MF_STRING or MF_Checked, fPopupMenu.Items[i].Command,
        pwidechar(WideString(fPopupMenu.Items[i].Caption) + Shortcut))
    else
      AppendMenuW(popupMenu, MF_STRING, fPopupMenu.Items[i].Command,
        pwidechar(WideString(fPopupMenu.Items[i].Caption) + Shortcut));

    if (fPopupMenu.Items[i].ImageIndex <> -1) and (Assigned(fPopupMenu.Images)) then
    begin
      Bmp := TBitmap.Create;
      bmp.SetSize(ScaleX(16, 96), ScaleX(16, 96));
      bmp.PixelFormat := pf32bit;

      if fPopupMenu.Items[i].Enabled = False then
        fPopupMenu.Images.DrawForPPI(bmp.Canvas, 0, 0,
          fPopupMenu.Items[i].ImageIndex, 16,
          Forms.Screen.PixelsPerInch, 1, False);
      if (fPopupMenu.Items[i].Enabled) and (fPopupMenu.Items[i].Caption <> '-') then
        fPopupMenu.Images.DrawForPPI(bmp.Canvas, 0, 0,
          fPopupMenu.Items[i].ImageIndex, 16, Forms.Screen.PixelsPerInch, 1, True);

      SetMenuItemBitmaps(popupMenu, i, MF_BYPOSITION,
        bmp.ReleaseHandle, bmp.handle);
      bmp.Free;
    end;
  end;

  if fPopupMenu is TMainMenu then
    SetMenu(fHandle, popupMenu)
  else
  begin
    GetCursorPos(pt);
    TrackPopupMenu(
      popupMenu,
      TPM_LEFTALIGN or TPM_TOPALIGN,
      pt.x, pt.y,
      0,
      fHandle,
      nil
      );
    DestroyMenu(popupMenu);

  end;

  { if APopup is TPopupMenu then
      TPopupMenu(APopup).PopUp;}

end;
{$ENDIF}
{$IFDEF FPC}
initialization
  {$I resources/SGJ.Win32Menu.lrs}
{$ENDIF}
end.
