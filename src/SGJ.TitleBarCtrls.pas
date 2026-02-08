unit SGJ.TitleBarCtrls;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef MSWindows}
  Windows, dwmApi, UxTheme,
  {$EndIF}
  Forms, Themes, Classes, SysUtils, Controls, Buttons, Graphics, bgrabitmap,
  bgrabitmaptypes, LCLType, Dialogs, LResources,BGRACanvas2D;

type
  {$ifdef MSWindows}
  TForm = class(Forms.TForm)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure AdjustClientRect(var aRect: TRect); override;
    procedure Paint; override;
  end;
   {$EndIF}

  TSystemTitlebarButton = class(TCustomSpeedButton)
  private
    fhot: boolean;
    fdown: boolean;
    procedure PaintBtnSymbol;
  protected
    constructor Create(AOwner: TComponent); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Paint; override;
    procedure Click; override;
  end;


  TCaptionAlign = (caLeft, caCenter, caNone);

  TSGJCustomTitleBarPanel = class(TCustomControl)
  private
    FMouseDownPt: TPoint;
    fDoubleClick: boolean;
    fCaptionAlignement: TCaptionAlign;
    fMinButton: TSystemTitlebarButton;
    fMaxRestoreButton: TSystemTitlebarButton;
    fCloseButton: TSystemTitlebarButton;
    fEnabled: boolean;
    fShowIcon:boolean;
    fLastLeftButton: integer;
    procedure SetTransparentColor;
    procedure EnableGlass(ALeft, ARight, ATop, ABottom: integer);
  protected
    procedure DblClick; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Loaded; override;
    procedure Paint; override;
    procedure AdjustClientRect(var aRect: TRect); override;
  public
    fVistaPNG: TPortableNetworkGraphic;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CaptionAlignment: TCaptionAlign
      read fCaptionAlignement write fCaptionAlignement;
    property Enabled: boolean read fEnabled write fEnabled;
    property ShowIcon: boolean read fShowIcon write fShowIcon;
  end;

  TSGJTitleBarPanel = class(TSGJCustomTitleBarPanel)
  published
    property CaptionAlignment;
    property ShowIcon;
  end;

type
  // Enum for DWMNCRENDERINGPOLICY
  TDWMNCRENDERINGPOLICY = (
    DWMNCRP_USEWINDOWSTYLE, // Default behavior
    DWMNCRP_DISABLED,       // Non-client rendering disabled
    DWMNCRP_ENABLED         // Non-client rendering enabled
    );

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJTitleBarPanel]);
end;


{$ifdef MSWindows}
procedure TForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  BorderStyle := bsNone;
  if Win32MajorVersion = 10 then
  begin
    Params.ExStyle := Params.ExStyle or WS_EX_STATICEDGE;
    Params.Style := Params.Style;
  end
  else
  begin
    Params.ExStyle := Params.ExStyle;// or WS_EX_STATICEDGE;
    Params.Style := Params.Style and not Ws_border;
  end;
  self.BorderIcons := [];
end;

procedure TForm.Paint;
var
  Details: TThemedElementDetails;
begin
  inherited;

  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion=6) then
  begin
    Canvas.Brush.Color := clblack;
    Canvas.FillRect(0, 0, Width, height);
  end;
  if not DWMApi.DwmCompositionEnabled then
  begin
    Canvas.Brush.Color := clFuchsia;
    Canvas.FillRect(0, 0, Width, 5);

    Details := ThemeServices.GetElementDetails(twFrameLeftActive);
    ThemeServices.DrawElement(Canvas.Handle, Details, Rect(0, 7, 7, Height));

    Details := ThemeServices.GetElementDetails(twFrameRightActive);
    ThemeServices.DrawElement(Canvas.Handle, Details,
      Rect(Width - 7, 7, Width, Height));

    Details := ThemeServices.GetElementDetails(twCaptionActive);
    ThemeServices.DrawElement(Canvas.Handle, Details, Rect(0, 0, Width, 30));

    Details := ThemeServices.GetElementDetails(twFrameBottomActive);
    ThemeServices.DrawElement(Canvas.Handle,
      Details, Rect(0, Height - 7, Width, Height));
  end;
end;

procedure TForm.WMNCHitTest(var Message: TWMNCHitTest);
const
  EDGEDETECT = 10;
var
  deltaRect: TRect;
begin
  inherited;
  if BorderStyle = bsNone then
    with Message, deltaRect do
    begin
      Left := XPos - BoundsRect.Left;
      Right := BoundsRect.Right - XPos;
      Top := YPos - BoundsRect.Top;
      Bottom := BoundsRect.Bottom - YPos;
      if (Top < EDGEDETECT) and (Left < EDGEDETECT) then
        Result := HTTOPLEFT
      else if (Top < EDGEDETECT) and (Right < EDGEDETECT) then
        Result := HTTOPRIGHT
      else if (Bottom < EDGEDETECT) and (Left < EDGEDETECT) then
        Result := HTBOTTOMLEFT
      else if (Bottom < EDGEDETECT) and (Right < EDGEDETECT) then
        Result := HTBOTTOMRIGHT
      else if (Top < EDGEDETECT) then
        Result := HTTOP
      else if (Left < EDGEDETECT) then
        Result := HTLEFT
      else if (Bottom < EDGEDETECT) then
        Result := HTBOTTOM
      else if (Right < EDGEDETECT) then
        Result := HTRIGHT;
    end;
end;

procedure TForm.AdjustClientRect(var aRect: TRect);
begin
  inherited AdjustClientRect(ARect);
  if not DWMApi.DwmCompositionEnabled then
    ARect := Rect(ARect.left + 7, ARect.Top,
      ARect.Right - 7, ARect.Bottom - 7);

  if (Win32MajorVersion = 6) and (DWMApi.DwmCompositionEnabled) then
  begin
    if WindowState=wsNormal then
    begin
    ARect := Rect(ARect.left + 7,
      ARect.Top, ARect.Right - 7,
      ARect.Bottom - 7);
    end;
    if WindowState=wsMaximized then
    begin
    ARect := Rect(ARect.left,
      ARect.Top, ARect.Right,
      ARect.Bottom);
    end;
  end;
end;
{$ENDIF}
constructor TSystemTitlebarButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifdef MSWindows}
  Flat := True;
  if (Win32MajorVersion = 6) and (DWMApi.DwmCompositionEnabled) and
  (Win32MinorVersion in [0,1]) then
  Constraints.MaxHeight:=20;
  {$ENDIF}
end;

procedure TSystemTitlebarButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  b: TBGRABitmap;
begin
  {$ifdef MSWindows}
  fdown := True;
  if (Win32MajorVersion = 10) then
  begin

    if Tag = 3 then
      self.canvas.Brush.color := RGBtoColor(151, 23, 34)
    else
      self.canvas.Brush.color := RGBtoColor(43, 41, 44);
    self.canvas.fillrect(self.ClientRect);

    PaintBtnSymbol;
  end;
  if (Win32MajorVersion = 6) and (Win32MinorVersion in [2, 3]) then
  begin
    b := TBGRABitmap.Create;
    b.SetSize(Width, Height);
    if Tag = 3 then
      b.FillRect(ClientRect, RGB(153, 61, 61))
    else
      b.FillRect(ClientRect, RGB(61, 96, 153));
    b.Draw(Canvas, 0, 0, True);
    PaintBtnSymbol;
  end;

  if (Win32MajorVersion = 6) and (Win32MInorVersion in [0, 1]) then
    PaintBtnSymbol;
  {$ENDIF}
end;

procedure TSystemTitlebarButton.MouseMove(Shift: TShiftState; X, Y: integer);
var
  b: TBGRABitmap;
begin
  {$ifdef MSWindows}
  fhot := True;
  if (Win32MajorVersion = 10) then
  begin
    if Tag = 3 then
      self.canvas.Brush.color := RGBtoColor(232, 17, 35)
    else
      self.canvas.Brush.color := RGBtoColor(55, 55, 55);
    self.canvas.fillrect(self.ClientRect);
    PaintBtnSymbol;
  end;

  if (Win32MajorVersion = 6) and (Win32MInorVersion in [2, 3]) then
  begin
    b := TBGRABitmap.Create;
    b.SetSize(Width, Height);
    if Tag = 3 then
      b.FillRect(ClientRect, RGB(224, 67, 67))
    else
      b.FillRect(ClientRect, RGB(54, 101, 179));
    b.Draw(Canvas, 0, 0, True);
    PaintBtnSymbol;
  end;

  if (Win32MajorVersion = 6) and (Win32MInorVersion in [0, 1]) then
    PaintBtnSymbol;
  {$ENDIF}
end;

procedure TSystemTitlebarButton.PaintBtnSymbol;
var
  AStyle: TTextStyle;
  BtnSym: string;
  b,g: TBGRABitmap;
  Details: TThemedElementDetails;
  aDPI: integer;
begin
  {$ifdef MSWindows}
  if Win32MajorVersion = 10 then
  begin
    AStyle := Canvas.TextStyle;
    AStyle.Alignment := taCenter;
    AStyle.Layout := tlCenter;
    AStyle.ShowPrefix := True;
    Canvas.Font.Color := clWhite;
    //Canvas.Font.Size:=8;
    if Win32BuildNumber >= 2200 then
      Canvas.Font.Name := 'Segoe Fluent Icons'
    else
      Canvas.Font.Name := 'Segoe MDL2 Assets';
    case Tag of
      1: BtnSym := widechar($E921);
      2: begin
        if GetParentForm(self).WindowState = wsMaximized then
          BtnSym := widechar($E923)
        else
          BtnSym := widechar($E922);
      end;
      3: BtnSym := widechar($E8BB);
    end;
    Canvas.TextRect(ClientRect, 0, 0, BtnSym, AStyle);
  end;

  if Screen.PixelsPerInch >= 192 then
     ADpi:=192 else
  if Screen.PixelsPerInch >= 144 then
     ADpi:=144 else
  if Screen.PixelsPerInch >= 120 then
     ADpi:=120 else
  ADpi:=96;

  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
  begin
    g:=TBGRABitmap.Create();

    if Win32MinorVersion in [2, 3] then
    begin
     if TAG =1 then
     if not fdown and not fhot then
      g.LoadFromResource('Min_8_Hot_'+IntToStr(ADpi))
      else
      g.LoadFromResource('Min_8_'+IntToStr(ADpi));
     if tag=2 then
     if not fdown and not fhot then
     begin
        if GetParentForm(self).WindowState=wsmaximized
          then
         g.LoadFromResource('Restore_8_Hot_'+IntToStr(ADpi))
         else
         g.LoadFromResource('Max_8_Hot_'+IntToStr(ADpi));
      end
      else
         begin
           if GetParentForm(self).WindowState=wsmaximized
            then
            g.LoadFromResource('Restore_8_'+IntToStr(ADpi))
          else
         g.LoadFromResource('Max_8_'+IntToStr(ADpi))
         end;

      if TAG = 3 then begin
        if not fdown and not fhot then
        begin
          b := TBGRABitmap.Create(Width, Height);
          b.FillRect(ClientRect, RGB(199, 80, 80));
          b.draw(canvas, 0, 0, True);
          b.Free;
        end;
        g.LoadFromResource('Close_8_'+IntToStr(ADpi));
      end;
      g.Draw(canvas, width div 2 - g.Width div 2, height div 2 - g.Height div 2, false);
    end;
    if Win32MinorVersion in [0, 1] then
    begin
      b := TBGRABitmap.Create(Width, Height);

      case Tag of
        1: begin
          if fhot or fdown then
          b.fillrect(clientrect,RGB(50,50,50),dmdrawwithtransparency,2000);
          b.DrawVertLine(0, 1, Height - 3, RGB(98, 98, 98));
          b.SetPixel(1, Height - 2, RGB(98, 98, 98));
          b.DrawHorizLine(2, Height - 1, Width, RGB(98, 98, 98));
          g.LoadFromResource('Min_Vista_'+IntToStr(ADpi));
        end;
        2: begin
          if fhot or fdown then
          b.fillrect(clientrect,RGB(50,50,50),dmdrawwithtransparency,2000);

          b.DrawVertLine(0, 1, Height, RGB(98, 98, 98));
          b.DrawHorizLine(0, Height - 1, Width, RGB(98, 98, 98));
          if GetParentForm(self).WindowState=wsmaximized
          then
          g.LoadFromResource('Restore_Vista_'+IntToStr(ADpi))
          else
          g.LoadFromResource('Max_Vista_'+IntToStr(ADpi));
        end;
        3: begin
          // b.FillRect(1,1,Width-1,height div 2+1,RGB(222,112,93),dmDrawWithTransparency,59000);
          // b.FillRect(1,height div 2 +1,Width-1,height -1,RGB(210,34,2),dmDrawWithTransparency,59000);
          if GetParentForm(self).Active then
          begin
            if fhot or fdown then
            begin
              b.canvas.GradientFill(Rect(1, 2, Width - 1, Height div 2 + 1), RGB(
                222, 112, 93), RGB(202, 92, 77), gdvertical);
              b.canvas.GradientFill(Rect(1, Height div 2 + 1, Width - 1, Height - 1),
                RGB(198, 34, 2), RGB(214, 137, 47), gdvertical);
            end
            else
            begin
            b.canvas.GradientFill(Rect(1, 2, Width - 1, Height div 2 + 1), RGB(
              200, 133, 121), RGB(187, 92, 77), gdvertical);
            b.canvas.GradientFill(Rect(1, Height div 2 + 1, Width - 1, Height - 1),
              RGB(210, 34, 2), RGB(224, 137, 47), gdvertical);
            end;
          end;
          b.DrawVertLine(0, 1, Height, RGB(98, 98, 98));
          b.DrawHorizLine(0, Height - 1, Width - 3, RGB(98, 98, 98));
          b.SetPixel(Width - 2, Height - 2, RGB(98, 98, 98));
          b.DrawVertLine(Width - 1, 1, Height - 3, RGB(98, 98, 98));
          g.LoadFromResource('Close_Vista_'+IntToStr(ADpi));
        end;
      end;
      b.Draw(canvas, 0, 0, True);
      g.Draw(canvas, width div 2 - g.Width div 2, height div 2 - g.Height div 2, false);
      b.Free;

    end;
    g.free;
  end;

  if not DWMApi.DwmCompositionEnabled then
  begin
    case Tag of
      1: begin
        if fhot then
          Details := ThemeServices.GetElementDetails(twMinButtonHot)
        else if fdown then
          Details := ThemeServices.GetElementDetails(twMinButtonPushed)
        else
          Details := ThemeServices.GetElementDetails(twMinButtonNormal);
      end;
      2: begin
        if fhot then
          Details := ThemeServices.GetElementDetails(twMaxButtonHot)
        else if fdown then
          Details := ThemeServices.GetElementDetails(twMaxButtonPushed)
        else
          Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
      end;
      3: begin
        if fhot then
          Details := ThemeServices.GetElementDetails(twCloseButtonHot)
        else if fdown then
          Details := ThemeServices.GetElementDetails(twCloseButtonPushed)
        else
          Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
      end;
    end;
    ThemeServices.DrawElement(Canvas.Handle, Details, ClientRect);

  end;
  fhot := False;
  fdown := False;
  {$ENDIF}
end;

procedure TSystemTitlebarButton.Paint;
begin
  inherited;
  PaintBtnSymbol;
end;

procedure TSystemTitlebarButton.Click;
begin
  case Tag of
    1: Application.Minimize;
    2: TSGJCustomTitleBarPanel(Parent).DblClick;
    3: GetParentForm(self).Close;
  end;
end;

constructor TSGJCustomTitleBarPanel.Create(AOwner: TComponent);
var
  bWidth: integer;
begin
  inherited Create(AOwner);
  {$ifdef MSWindows}
  Align := alTop;
  Height := 45;
  Constraints.MaxHeight:=45;

  fLastLeftButton:=0;

  if not DWMApi.DwmCompositionEnabled then
    bWidth := 35 //non DWM
  else
  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    bwidth := 50
  else
    bwidth := 45; // win 11

  fMaxRestoreButton := TSystemTitlebarButton.Create(self);
  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    fMaxRestoreButton.Width := 30
  else
    fMaxRestoreButton.Width := bWidth;
  fMaxRestoreButton.Parent := self;
  fMaxRestoreButton.align := alright;
  fMaxRestoreButton.Tag := 1;

  fMinButton := TSystemTitlebarButton.Create(self);
  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    fMinButton.Width := 30
  else
    fMinButton.Width := bWidth;
  fMinButton.Parent := self;
  fMinButton.align := alright;
  fMinButton.Tag := 2;

  fCloseButton := TSystemTitlebarButton.Create(self);
  fCloseButton.Width := bWidth;
  fCloseButton.align := alright;
  fCloseButton.Parent := self;
  fCloseButton.Tag := 3;

  ControlStyle := ControlStyle + [csAcceptsControls];
  {$ENDIF}
end;

destructor TSGJCustomTitleBarPanel.Destroy;
begin
  {$ifdef MSWindows}
  fMinButton.Free;
  fMaxRestoreButton.Free;
  fCloseButton.Free;
  fVistaPNG.Free;
  {$ENDIF}
  inherited;
end;

procedure TSGJCustomTitleBarPanel.Loaded;
var
  i: integer;
  ChildControl: TControl;
  L: integer;
  DWM_WINDOW_CORNER_PREFERENCE: DWORD;
  Policy: TDWMNCRENDERINGPOLICY;
const
  DWMWCP_ROUND = 2;
  DWMWA_WINDOW_CORNER_PREFERENCE = 33;
begin
  inherited;

  {$ifdef MSWindows}
  if not DWMApi.DwmCompositionEnabled then
  begin
    SetTransparentColor;
    ChildSizing.HorizontalSpacing := 3;
  end;



  if DWMApi.DwmCompositionEnabled then
  begin
    Policy := DWMNCRP_ENABLED;
    DwmSetWindowAttribute(GetParentForm(self).handle, DWMWA_NCRENDERING_POLICY, @Policy, SizeOf(Policy));
    if Win32MajorVersion = 6 then
      EnableGlass(7, 7, ScaleX(30,96), 7);
  end;



  for i := 0 to ControlCount - 1 do
  begin
    ChildControl := Controls[I];
    if (ChildControl is TSystemTitlebarButton) then
      if ChildControl.align = alRight then
        ChildControl.Left := Width;
    if (ChildControl is TWinControl) then
    begin
       if TWinControl(ChildControl).Align=alleft then
          if (TWinControl(ChildControl).Left+TWinControl(ChildControl).width)>fLastLeftButton then
          fLastLeftButton:=(TWinControl(ChildControl).Left+TWinControl(ChildControl).width);
    end;
  end;





    {$IFDEF MSWINDOWS}
    if (Win32BuildNumber >= 22000) then
    begin
      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
      dwmapi.DwmSetWindowAttribute(GetParentForm(self).handle, DWMWA_WINDOW_CORNER_PREFERENCE,
        @DWM_WINDOW_CORNER_PREFERENCE, SizeOf(DWM_WINDOW_CORNER_PREFERENCE));
    end;
    {$ENDIF}

    {$ENDIF}
end;

procedure TSGJCustomTitleBarPanel.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if fDoubleClick = False then
    if (ssLeft in Shift) then
    begin
      GetParentForm(self).Left := GetParentForm(self).Left + (X - FMouseDownPt.X);
      GetParentForm(self).Top := GetParentForm(self).Top + (Y - FMouseDownPt.Y);
    end;
  fDoubleClick := False;
end;

procedure TSGJCustomTitleBarPanel.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FMouseDownPt := Point(X, Y);
end;

procedure TSGJCustomTitleBarPanel.DblClick;
begin
  fDoubleClick := True;
  if GetParentForm(self).WindowState = wsMaximized then
  begin
    {$ifdef MSWindows}
    if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    EnableGlass(7,7,Height,7);
    {$ENDIF}
    GetParentForm(self).WindowState := wsNormal
  end
  else
  begin
    with Screen.MonitorFromWindow(self.Handle).WorkAreaRect do
    begin
      GetParentForm(self).Constraints.MaxHeight := Bottom;
      GetParentForm(self).Constraints.MaxWidth := Right;
    end;
    {$ifdef MSWindows}
    if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    EnableGlass(0,0,Height,0);
    {$ENDIF}
    GetParentForm(self).WindowState := wsMaximized;
  end;
  inherited DblClick;
end;

procedure TSGJCustomTitleBarPanel.AdjustClientRect(var aRect: TRect);
var
  IconLeft:integer;
begin
  inherited AdjustClientRect(ARect);
  if fShowIcon then
     IconLeft:=20 else
     IconLeft:=0;
  {$ifdef MSWindows}
  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 10) then
  ARect := Rect(ARect.left+ScaleX(IconLeft,96),
    ARect.Top, ARect.Right,
    ARect.Bottom);

  if not DWMApi.DwmCompositionEnabled then
    ARect := Rect(ARect.left+ScaleX(IconLeft,96), ARect.Top + 12,
      ARect.Right, ARect.Bottom - 6);

  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion = 6) then
    ARect := Rect(ARect.left+ScaleX(IconLeft,96),
      ARect.Top, ARect.Right,
      ARect.Bottom);
  {$ENDIF}
end;

procedure TSGJCustomTitleBarPanel.Paint;
var
  b: tbgrabitmap;
  icon: TIcon;
  FontData: TFontData;
  Details: TThemedElementDetails;
  textSize: TSize;
begin
  inherited;
  {$ifdef MSWindows}
  if (DWMApi.DwmCompositionEnabled) and (Win32MajorVersion=6) then
  begin
    Canvas.Brush.Color := clblack;
    Canvas.FillRect(0, 0, Width, height);
  end;

  if not DWMApi.DwmCompositionEnabled then
  begin
    Details := ThemeServices.GetElementDetails(twCaptionActive);
    ThemeServices.DrawElement(Canvas.Handle, Details,
      Rect(-7, 0, Width + 7, Height));
  end;
  {$ENDIF}

  if fShowIcon then
  begin
  b := tbgrabitmap.Create;
  b.setsize(canvas.Width, canvas.Height);
  icon := TIcon.Create;
  icon.Assign(Application.Icon);
  b.Assign(icon);
  b.draw(canvas, 3, Height div 2 - icon.Height div 2, False);
  icon.Free;
  b.Free;
  end;


  b := tbgrabitmap.Create();
  textSize:= b.TextSize(GetParentForm(self).Caption);
  b.SetSize(ScaleX(textSize.cx,96), ScaleX(textSize.cy,96));
  b.FontAntialias := True;
  FontData := GetFontData(Canvas.Font.Handle);
  b.FontHeight := Abs(FontData.Height);



  b.TextOut(0, 0, GetParentForm(self).Caption, ColorToBGRA(ColorToRGB(GetParentForm(self).font.Color)));

  if fCaptionAlignement=caLeft then
  begin
  if fShowIcon then
  b.draw(canvas, ScaleX(FLastLeftButton +20,96), Height div 2 - b.FontHeight div 2, False)
  else
  b.draw(canvas, ScaleX(FLastLeftButton,96), Height div 2 - b.FontHeight div 2, False);
  end;
  if fCaptionAlignement=caCenter then
  b.draw(canvas, width div 2 - b.Width div 2, Height div 2 - b.FontHeight div 2, False);
  b.Free;
end;

procedure TSGJCustomTitleBarPanel.SetTransparentColor;
begin
  {$ifdef MSWindows}
  SetWindowLong(GetParentForm(self).Handle, GWL_EXSTYLE,
    GetWindowLong(GetParentForm(self).Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

  SetLayeredWindowAttributes(GetParentForm(self).Handle, clFuchsia,
    0, LWA_COLORKEY);
  {$ENDIF}
end;

procedure TSGJCustomTitleBarPanel.EnableGlass(ALeft, ARight, ATop, ABottom: integer);
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
      mgn.cxLeftWidth := ALeft;
      mgn.cxRightWidth := ARight;
      mgn.cyTopHeight := ATop;
      mgn.cyBottomHeight := ABottom;
      { Extend Form }
      DwmExtendFrameIntoClientArea(GetParentForm(self).Handle, @mgn);
    end;

  end;
  {$ENDIF}
end;
initialization
{$IFDEF FPC}
  {$I resources/SGJ.TitleBarCtrls.lrs}
{$ENDIF}
end.
