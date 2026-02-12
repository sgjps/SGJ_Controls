unit SGJ.Win32Extension;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows, DWMApi, uxtheme,
  {$ENDIF}
  Forms, Controls, Classes, SysUtils, Dialogs, Graphics, LResources;

type

  TSGJWin32Ex = class;

  TSGJRoundedCorners = (rcCornerDefault, rcCornerOff, rcCornerOn, rcCornerSmall);

  TSGJWin11 = class(TPersistent)
  private
    fRoundedCorners: TSGJRoundedCorners;
    fFormHandle: THandle;
    fTitleBar_Color: TColor;
    fTitleBar_FontColor: TColor;
    fFormBorder: TColor;
    fOwner:TSGJWin32Ex;
    procedure SetRoundedCorners(AValue: TSGJRoundedCorners);
    procedure SetWin11Corners(AValue: TSGJRoundedCorners);
    procedure Set_DWMTitle(AValue: TColor);
    procedure Set_DWMFont(AValue: TColor);
    procedure Set_DWMBorder(AValue: TColor);
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
  published
    property RoundedCorners: TSGJRoundedCorners
      read fRoundedCorners write SetRoundedCorners;
    property BorderColor: TColor read fFormBorder write Set_DWMBorder;
    property CaptionFontColor: TColor read fTitleBar_FontColor write Set_DWMFont;
    property CaptionColor: TColor read fTitleBar_Color write Set_DWMTitle;

  end;

  TSGJGlassFrame = class(TPersistent)
  private
    fEnabled: boolean;
    fLeft: integer;
    fTop: integer;
    fBottom: integer;
    fRight: integer;
    fFormHandle: THandle;
    fSheetOfGlass: boolean;
    fOwner:TSGJWin32Ex;
    procedure SetEnabled(AValue: boolean);
    procedure SetSheetOfGlass(AValue: boolean);
    procedure EnableGlass(AValue: boolean);
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
  published
    property Left: integer read fLeft write fLeft;
    property Right: integer read fRight write fRight;
    property Top: integer read fTop write fTop;
    property Bottom: integer read fBottom write fBottom;
    property Enabled: boolean read fEnabled write SetEnabled;
    property SheetOfGlass: boolean read fSheetOfGlass write SetSheetOfGlass;
  end;

  TSGJWinVista = class(TPersistent)
  private
    fGlassFrame: TSGJGlassFrame;
    fFormHandle: THandle;
    fDisableDWM: boolean;
    fOwner:TSGJWin32Ex;
    procedure SetDisableDWM(AVAlue: boolean);
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
    destructor Destroy; override;
  published
    property DisableDWMForWindow: boolean read fDisableDWM write SetDisableDWM;
    property GlassFrame: TSGJGlassFrame read fGlassFrame write fGlassFrame;
  end;

  TSGJDarkMode = class(TPersistent)
  private
    fDarkTitleBar: boolean;
    fFormHandle: THandle;
    fOwner:TSGJWin32Ex;
    procedure SetDarkTitleBar(AValue: boolean);
    function SetDarkModeForTitleBar(hWnd: HWND; DarkMode: Bool): integer;
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
  published
    property TitleBar: boolean read fDarkTitleBar write SetDarkTitleBar;
  end;

  TSGJWin10 = class(TPersistent)
  private
    fDarkMode: TSGJDarkMode;
    fFormHandle: THandle;
    fOwner:TSGJWin32Ex;
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
    destructor Destroy; override;
  published
    property DarkMode: TSGJDarkMode read fDarkMode write fDarkMode;
  end;

  TSGJWin2k = class(TPersistent)
  private
    fTransparentColor: boolean;
    fTransparentColorValue: TColor;
    fFormHandle: THandle;
    fOwner:TSGJWin32Ex;
    procedure SetColor(AValue: TColor);
    procedure SetTransparent(AValue: boolean);
  public
    constructor Create(AHandle: THandle; AOwner: TSGJWin32Ex);
  published
    property TransparentColor: boolean read fTransparentColor write SetTransparent;
    property TransparentColorValue: TColor read fTransparentColorValue write SetColor;
  end;

  TSGJWin32Ex = class(TComponent)
  private
    fWin11Opt: TSGJWin11;
    fWin10opt: TSGJWin10;
    fWinVistaOpt: TSGJWinVista;
    fWin2k: TSGJWin2k;
    fOwnerHandle: THandle;
    function IsRuntime: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
  published
    property Windows11: TSGJWin11 read fWin11Opt write fWin11Opt;
    property Windows10: TSGJWin10 read fWin10opt write fWin10opt;
    property WindowsVista: TSGJWinVista read fWinVistaOpt write fWinVistaOpt;
    property Win2k: TSGJWin2k read fWin2k write fWin2k;
  end;



type
  // Enum for DWMNCRENDERINGPOLICY
  TDWMNCRENDERINGPOLICY = (
    DWMNCRP_USEWINDOWSTYLE, // Default behavior
    DWMNCRP_DISABLED,       // Non-client rendering disabled
    DWMNCRP_ENABLED         // Non-client rendering enabled
    );

const
  //Win10 17763+
  DWMWA_USE_IMMERSIVE_DARK_MODE_OLD = 19;
  //Win10 18985+, Win11
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;

  DWMWA_WINDOW_CORNER_PREFERENCE = 33;
  DWMWA_BORDER_COLOR = 34; //Border color
  DWMWA_CAPTION_COLOR = 35; // Title bar background color
  DWMWA_TEXT_COLOR = 36; // Title bar text color


  DWMWCP_DEFAULT = 0; // Let the system decide whether or not to round window corners
  DWMWCP_DONOTROUND = 1; // Never round window corners
  DWMWCP_ROUND = 2; // Round the corners if appropriate
  DWMWCP_ROUNDSMALL = 3; // Round the corners if appropriate, with a small radius


type
  TPreferredAppMode = (
    pamDefault,
    pamAllowDark,
    pamForceDark,
    pamForceLight,
    pamMax
    );

const
  uxthemelib = 'uxtheme.dll';

var
  UxThemeLibrary: THandle;
  ReferenceCount: integer;

{$IFDEF MSWINDOWS}
var
  SetPreferredAppMode: function(appMode: TPreferredAppMode): TPreferredAppMode; stdcall;
  AllowDarkModeForWindow: function(hWnd: HWND; allow: bool): bool; stdcall;
{$ENDIF}
function InitUxThemeLibrary: boolean;
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJWin32Ex]);
end;

procedure FreeUxThemeLibrary;
begin

  if ReferenceCount > 0 then
    Dec(ReferenceCount);

  if (UxThemeLibrary <> 0) and (ReferenceCount = 0) then
  begin
    FreeLibrary(UxThemeLibrary);
    UxThemeLibrary := 0;
    {$IFDEF MSWINDOWS}
    SetPreferredAppMode := nil;
    AllowDarkModeForWindow := nil;
    {$ENDIF}
  end;
end;

function InitUxThemeLibrary: boolean;
begin
  Inc(ReferenceCount);
  if UxThemeLibrary = 0 then
  begin
    UxThemeLibrary := LoadLibrary(uxthemelib);
    if UxThemeLibrary > 0 then
    begin
      {$IFDEF MSWINDOWS}
      Pointer(AllowDarkModeForWindow) :=
        GetProcAddress(UxThemeLibrary, MAKEINTRESOURCEA(133)); //b 17763
      Pointer(SetPreferredAppMode) :=
        GetProcAddress(UxThemeLibrary, MAKEINTRESOURCEA(135)); //b 18334
      {$ENDIF}
    end;
  end;
  Result := UxThemeLibrary > 0;
end;

constructor TSGJWin11.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  inherited Create;
  fFormHandle := AHandle;
  fOwner:=AOwner;
  fTitleBar_Color := clNone;
  fTitleBar_FontColor := clNone;
  fFormBorder := clNone;
end;

procedure TSGJWin11.SetWin11Corners(AValue: TSGJRoundedCorners);
var
  DWM_WINDOW_CORNER_PREFERENCE: DWORD;
begin
  {$IFDEF MSWINDOWS}
  if (Win32BuildNumber >= 22000) then
  begin
    case AValue of
      rcCornerOff: DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
      rcCornerOn: DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
      rcCornerSmall: DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
      else
        DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
    end;
    dwmapi.DwmSetWindowAttribute(fFormHandle, DWMWA_WINDOW_CORNER_PREFERENCE,
      @DWM_WINDOW_CORNER_PREFERENCE, SizeOf(DWM_WINDOW_CORNER_PREFERENCE));
  end;
  {$ENDIF}
end;

procedure TSGJWin11.SetRoundedCorners(AValue: TSGJRoundedCorners);
begin
  fRoundedCorners := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  SetWin11Corners(AValue);
end;


constructor TSGJDarkMode.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  fFormHandle := AHandle;
  fOwner:=AOwner;
end;
{$IFDEF MSWINDOWS}
function TSGJDarkMode.SetDarkModeForTitleBar(hWnd: HWND; DarkMode: Bool): integer;
var
  Policy: TDWMNCRENDERINGPOLICY;
begin
  Result := 0;

  if DarkMode then
    Policy := DWMNCRP_ENABLED
  else
    Policy := DWMNCRP_USEWINDOWSTYLE;
  DwmSetWindowAttribute(hWnd, DWMWA_NCRENDERING_POLICY, @Policy, SizeOf(Policy));
  if (Win32BuildNumber >= 17763) and (Win32BuildNumber < 18985) then
    if (DwmSetWindowAttribute(hWnd, DWMWA_USE_IMMERSIVE_DARK_MODE_OLD,
      @DarkMode, SizeOf(DarkMode)) <> S_OK) then
      Result := 1;
  if Win32BuildNumber >= 18985 then
    if (DwmSetWindowAttribute(hWnd, DWMWA_USE_IMMERSIVE_DARK_MODE,
      @DarkMode, SizeOf(DarkMode)) <> S_OK) then
      Result := 1;
end;
{$ENDIF}
procedure TSGJDarkMode.SetDarkTitleBar(AValue: boolean);
begin
  fDarkTitleBar := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  {$IFDEF MSWINDOWS}
  if fDarkTitleBar then
    SetDarkModeForTitleBar(fFormHandle, True)
  else
    SetDarkModeForTitleBar(fFormHandle, False);
  {$ENDIF}
end;

procedure TSGJWin11.Set_DWMBorder(AValue: TColor);
{$IFDEF MSWINDOWS}
var
  ColorRef: TCOLORREF;
{$ENDIF}
begin
  if fFormBorder <> AValue then
    fFormBorder := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  {$IFDEF MSWINDOWS}
  ColorRef := ColorToRGB(AValue);
  if AValue <> clNone then
    DwmSetWindowAttribute(fFormHandle, DWMWA_BORDER_COLOR, @ColorRef, SizeOf(ColorRef));
  {$ENDIF}
end;

procedure TSGJWin11.Set_DWMTitle(AValue: TColor);
{$IFDEF MSWINDOWS}
var
  ColorRef: TCOLORREF;
{$ENDIF}
begin
  if fTitleBar_Color <> AValue then
    fTitleBar_Color := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  {$IFDEF MSWINDOWS}
  ColorRef := ColorToRGB(AValue);
  if AValue <> clNone then
    DwmSetWindowAttribute(fFormHandle, DWMWA_CAPTION_COLOR, @ColorRef, SizeOf(ColorRef));
  {$ENDIF}
end;

procedure TSGJWin11.Set_DWMFont(AValue: TColor);
{$IFDEF MSWINDOWS}
var
  ColorRef: TCOLORREF;
{$ENDIF}
begin
  if fTitleBar_FontColor <> AValue then
    fTitleBar_FontColor := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  {$IFDEF MSWINDOWS}
  ColorRef := ColorToRGB(AValue);
  if AValue <> clNone then
    DwmSetWindowAttribute(fFormHandle, DWMWA_TEXT_COLOR, @ColorRef, SizeOf(ColorRef));
  {$ENDIF}
end;



constructor TSGJWin10.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  inherited Create;
  fFormHandle := AHandle;
  fDarkMode := TSGJDarkMode.Create(AHandle, AOwner);
  fOwner:=AOwner;
end;

destructor TSGJWin10.Destroy;
begin
  fDarkMode.Free;
  inherited Destroy;
end;


constructor TSGJGlassFrame.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  fFormHandle := AHandle;
  fOwner:=AOwner;
end;

procedure TSGJGlassFrame.SetEnabled(AValue: boolean);
begin
  fEnabled := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  if AValue = True then
    EnableGlass(True)
  else
    EnableGlass(False);
end;

procedure TSGJGlassFrame.SetSheetOfGlass(AValue: boolean);
begin
  fSheetOfGlass := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  if fEnabled then
    EnableGlass(True);
end;

procedure TSGJGlassFrame.EnableGlass(AValue: boolean);
{$IFDEF MSWINDOWS}
var
  mgn: TMargins;
  cEnable: BOOL;
{$ENDIF}
begin
  {$IFDEF MSWINDOWS}
  if Win32MajorVersion >= 6 then
  begin
    DwmIsCompositionEnabled(cEnable);
    if cEnable then
    begin
      ZeroMemory(@mgn, SizeOf(mgn));
      if AValue = True then
      begin
        mgn.cxLeftWidth := fLeft;
        mgn.cxRightWidth := fRight;
        mgn.cyTopHeight := fTop;
        mgn.cyBottomHeight := fBottom;
      end
      else
      begin
        mgn.cxLeftWidth := 0;
        mgn.cxRightWidth := 0;
        mgn.cyTopHeight := 0;
        mgn.cyBottomHeight := 0;
      end;

      if fSheetOfGlass then
      begin
        mgn.cxLeftWidth := -1;
        mgn.cxRightWidth := -1;
        mgn.cyTopHeight := -1;
        mgn.cyBottomHeight := -1;
      end;
      { Extend Form }
      DwmExtendFrameIntoClientArea(fFormHandle, @mgn);
    end;

  end;
  {$ENDIF}
end;

constructor TSGJWinVista.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  inherited Create;
  fFormHandle := AHandle;
  fGlassFrame := TSGJGlassFrame.Create(fFormHandle, AOwner);
  fOwner:=AOwner;
end;

destructor TSGJWinVista.Destroy;
begin
  fGlassFrame.Free;
  inherited Destroy;
end;

procedure TSGJWinVista.SetDisableDWM(AValue: boolean);
var
  Policy: TDWMNCRENDERINGPOLICY;
begin
  if fDisableDWM <> AValue then
    fDisableDWM := AValue;
  if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
  {$IFDEF MSWINDOWS}
  if fDisableDWM then
    Policy := DWMNCRP_DISABLED
  else
    Policy := DWMNCRP_ENABLED;
  DwmSetWindowAttribute(fFormHandle, DWMWA_NCRENDERING_POLICY, @Policy, SizeOf(Policy));
  {$ENDIF}
end;

constructor TSGJWin2k.Create(AHandle: THandle; AOwner: TSGJWin32Ex);
begin
  inherited Create;
  fFormHandle := AHandle;
  fOwner:=AOwner;
end;

procedure TSGJWin2k.SetColor(AValue: TColor);
begin
  if fTransparentColorValue <> AValue then
  begin
    fTransparentColorValue := AValue;
    if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
    {$IFDEF MSWINDOWS}
    if fTransparentColor then
    begin
      SetWindowLong(fFormHandle, GWL_EXSTYLE,
        GetWindowLong(fFormHandle, GWL_EXSTYLE) or WS_EX_LAYERED);

      SetLayeredWindowAttributes(fFormHandle, fTransparentColorValue,
        0, LWA_COLORKEY);
    end;
    {$ENDIF}
  end;
end;

procedure TSGJWin2k.SetTransparent(AValue: boolean);
begin
  if fTransparentColor <> AValue then
  begin
    fTransparentColor := AValue;
    if Assigned(fOwner) and not fOwner.IsRuntime then Exit;
    {$IFDEF MSWINDOWS}
    if fTransparentColor then
    begin
      SetWindowLong(fFormHandle, GWL_EXSTYLE,
        GetWindowLong(fFormHandle, GWL_EXSTYLE) or WS_EX_LAYERED);

      SetLayeredWindowAttributes(fFormHandle, fTransparentColorValue,
        0, LWA_COLORKEY);
    end
    else
    begin
      // Remove layered style
      SetWindowLong(fFormHandle, GWL_EXSTYLE,
        GetWindowLong(fFormHandle, GWL_EXSTYLE) and not WS_EX_LAYERED);
    end;
    {$ENDIF}
  end;
end;


constructor TSGJWin32Ex.Create(AOwner: TComponent);
var
  F: TCustomForm;
begin
  inherited Create(AOwner);
  if AOwner is TControl then
    F := GetParentForm(TControl(AOwner))
  else if AOwner is TCustomForm then
    F := TCustomForm(AOwner)
  else
    F := nil;

  if Assigned(F) then
    fOwnerHandle := F.Handle
  else
    fOwnerHandle := 0;

  fWin11Opt := TSGJWin11.Create(fOwnerHandle, self);
  fWin10opt := TSGJWin10.Create(fOwnerHandle, self);
  fWinVistaOpt := TSGJWinVista.Create(fOwnerHandle, self);
  fWin2k := TSGJWin2k.Create(fOwnerHandle, self);
end;


procedure TSGJWin32Ex.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then Exit;
  {$IFDEF MSWINDOWS}
  if fWin2k.fTransparentColor = True then
  begin
    SetWindowLong(fOwnerHandle, GWL_EXSTYLE,
      GetWindowLong(fOwnerHandle, GWL_EXSTYLE) or WS_EX_LAYERED);

    SetLayeredWindowAttributes(fOwnerHandle, fWin2k.fTransparentColorValue,
      0, LWA_COLORKEY);
  end;
  {$ENDIF}
end;

destructor TSGJWin32Ex.Destroy;
begin
  fWin11Opt.Free;
  fWin10Opt.Free;
  fWinVistaOpt.Free;
  fWin2k.Free;
  inherited Destroy;
end;

function TSGJWin32Ex.IsRuntime: Boolean;
begin
  Result := not (csDesigning in ComponentState);
end;


initialization
  ReferenceCount := 0;
  {$IFDEF FPC}
    {$I resources/SGJ.Win32Ex.lrs}
  {$ENDIF}

finalization
  while ReferenceCount > 0 do
    FreeUxThemeLibrary;

end.
