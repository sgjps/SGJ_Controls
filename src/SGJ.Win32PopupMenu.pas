unit SGJ.Win32popupMenu;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, uxTheme,
  {$EndIf}
  SysUtils, Menus, Graphics, Forms, Controls, Dialogs, LCLProc,
  types;

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
  {$EndIf}
const
  WM_UAHDRAWMENU = $0091;
  WM_UAHDRAWMENUITEM = $0092;
  WM_UAHMEASUREMENUITEM = $0093;
  OBJID_MENU = DWORD($FFFFFFFD);

type
  TSGJWin32Menu = class
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
    constructor Create(AForm: TForm;AEnableWndCall: boolean);
    destructor Destroy; override;
    procedure CreateMenu(APopup: TMenu);
  end;

var
  {$IFDEF MSWINDOWS}
  PrevWndProc: Windows.WNDProc;
  {$EndIf}
  Win32Menu: TSGJWin32Menu;
  WinPopupMenu:TSGJWin32Menu;

implementation
{$IFDEF MSWINDOWS}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam;
  lParam: LParam): LRESULT; stdcall;
var
  MenuItem: TMenuItem;
  i: integer;

  DC: HDC;
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

  procedure ExecuteSubItems(AItem: TMenuItem);
  var
    i:integer;
  begin
    for i := 0 to AItem.Count - 1 do
    if LOWORD(wParam) = AItem.Items[i].Command then
            AItem.Items[i].Click
    else
     if AItem.Count>0 then
       ExecuteSubItems(AItem.Items[i]) ;
  end;
begin
  if uMsg = WM_COMMAND then
  begin
    if (lParam = 0) and (HIWORD(wParam) = 0) then
      if Win32Menu.fPopupMenu is TMenu then
        for i := 0 to Win32Menu.fPopupMenu.Items.Count - 1 do
          if LOWORD(wParam) = Win32Menu.fPopupMenu.Items[i].Command then
            Win32Menu.fPopupMenu.Items[i].Click
          else
          if Win32Menu.fPopupMenu.Items[i].count>0 then
             ExecuteSubItems(Win32Menu.fPopupMenu.Items[i]);
    if WinPopupMenu.fPopupMenu is TMenu then
      for i := 0 to WinPopupMenu.fPopupMenu.Items.Count - 1 do
        if LOWORD(wParam) = WinPopupMenu.fPopupMenu.Items[i].Command then
          WinPopupMenu.fPopupMenu.Items[i].Click
        else
        if WinPopupMenu.fPopupMenu.Items[i].count>0 then
           ExecuteSubItems(WinPopupMenu.fPopupMenu.Items[i]);
  end;
  if Win32Menu.CustomColors then
  begin
    if uMsg = WM_UAHDRAWMENU then
    begin
      pUDM := PUAHMENU(lParam);

      FillChar(R, SizeOf(R), 0);
      mbi.cbSize := SizeOf(mbi);
      GetMenuBarInfo(ahWnd, OBJID_MENU, 0, @mbi);

      GetWindowRect(ahWnd, rcWindow);

      R := mbi.rcBar;
      Types.OffsetRect(R, -rcWindow.left, -rcWindow.top);

      _Brush := CreateSolidBrush(ColorToRGB(Win32Menu.ColorMenu));
      FillRect(pUDM^._hdc, R, _Brush);


      Exit;
    end;

    if uMsg = WM_UAHDRAWMENUITEM then
    begin
      pUDMI := PUAHDRAWMENUITEM(lParam);


      g_brItemBackground := CreateSolidBrush(ColorToRGB(Win32Menu.ColorBG));
      g_brItemBackgroundHot := CreateSolidBrush(ColorToRGB(Win32Menu.ColorBGHot));
      g_brItemBackgroundSelected :=
        CreateSolidBrush(ColorToRGB(Win32Menu.ColorBGSelected));
      g_brItemBorder := CreateSolidBrush(ColorToRGB(Win32Menu.ColorBorder));

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
      opts.crText := ColorToRGB(Win32Menu.ColorText);

      if iTextStateID <> MBI_DISABLED then
        opts.crText := ColorToRGB(Win32Menu.ColorText)
      else
        opts.crText := ColorToRGB(Win32Menu.ColorTextDisabled);

      FillRect(pUDMI^.um._hdc, pUDMI^.dis.rcItem, pbrBackground^);
      FrameRect(pUDMI^.um._hdc, pUDMI^.dis.rcItem, pbrBorder^);
      //DrawThemeText(menuTheme, pUDMI^.um._hdc, MENU_BARITEM, MBI_NORMAL, PWideChar(menuString), mii.cch, dwFlags, 0,pUDMI^.dis.rcItem);
      DrawThemeTextEx(menuTheme, pUDMI^.um._hdc, MENU_BARITEM, MBI_NORMAL,
        pwidechar(menuString), mii.cch, dwFlags, @pUDMI^.dis.rcItem, @opts);
      CloseThemeData(menuTheme);
      exit;
    end;

  end;

  Result := CallWindowProc(PrevWndProc, Ahwnd, uMsg, WParam, LParam);
end;
{$EndIf}
destructor TSGJWin32Menu.Destroy;
begin
  {$IFDEF MSWINDOWS}
  SetWindowLongPtr(fhandle, GWLP_WNDPROC, PtrUInt(PrevWndProc));
  {$EndIf}
  inherited;
end;

constructor TSGJWin32Menu.Create(AForm: TForm;AEnableWndCall: boolean);
begin
  fForm := AForm;
  fHandle := AForm.Handle;
  {$IFDEF MSWINDOWS}
  if AEnableWndCall then
  PrevWndProc := Windows.WNDPROC(SetWindowLongPtr(fHandle, GWL_WNDPROC,
    PtrUInt(@WndCallback)));
  {$EndIf}
  CustomColors := False;
  ColorMenu := $002A2A2A;
  ColorText := clWhite;
  ColorBG := $002A2A2A;
  ColorBGHot := $004D4D4D;
  ColorBGSelected := $004D4D4D;
  ColorBorder := $004D4D4D;
  ColorTextDisabled := ClGray;
end;
{$IFDEF MSWINDOWS}
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
{$EndIf}
procedure TSGJWin32Menu.CreateMenu(APopup: TMenu);
var
  pt: TPoint;
  //MII: TMenuItemInfo;
  Bmp: TBitmap;
  i: integer;
  {$IFDEF MSWINDOWS}
  popupMenu: HMenu;
  {$EndIf}
  Shortcut: string;
begin
  fPopupMenu := APopup;
  {$IFDEF MSWINDOWS}
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
  {$ELSE}
   if APopup is TPopupMenu then
      TPopupMenu(APopup).PopUp;
  {$ENDIF}
end;

end.
