{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }

{ date      : 2024/12/25                                             }
{             SGJ Controls                                           }
{ version   : 1.2                                                    }
{ TSGJTitlePanel v 1.0                                               }
{ This file is part of SGJ Controls for Delphi and Lazarus           }

{********************************************************************}

unit SGJ.TitlePanel;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows, dwmapi,registry, uxtheme,themes,
  {$ENDIF}
  ColorSpeedButton, ImgList, bgrabitmap,
  LCLType, LResources, Forms, Controls, Graphics, ExtCtrls, Classes, SysUtils;

type
  TSGJTitlePanel = class(TPanel)
  private
    btn_Close: TColorSpeedButton;
    btn_Minimize: TColorSpeedButton;
    btn_Restore: TColorSpeedButton;
    FMouseDownPt: TPoint;
    DoubleClick: boolean;
    AColor: TColor;
    fImages: TCustomImageList;
    fImageIndex_Minimize: integer;
    fImageIndex_Maximize: integer;
    fImageIndex_Restore: integer;
    fImageIndex_Close: integer;
    fUseSystemColors: boolean;
    fTitleOnCenter: boolean;
    fShowTitle:boolean;
    fShowIcon: boolean;
    procedure btn_click(Sender: TObject);
     {$IFDEF Windows}
    procedure btn_OnPaint(Sender: TObject);

    procedure btn_OnMouseMove(Sender: TObject;

  Shift: TShiftState; X, Y: Integer);
    procedure btn_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
      {$endif}
    procedure SetUseSystemColors(AChecked: boolean);
    procedure SetTitleOnCenter(AChecked: boolean);
    procedure SetShowIcon(AChecked: boolean);
    procedure SetShowTitle(AChecked: boolean);
  public
    constructor Create(aOwner: TComponent); override;
    procedure Paint; override;
  protected

    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    procedure DblClick; override;

  published
    property Color;
    property Images: TCustomImageList read fImages write fImages;
    property ImageIndex_Minimize: integer read fImageIndex_Minimize
      write fImageIndex_Minimize;
    property ImageIndex_Maximize: integer read fImageIndex_Maximize
      write fImageIndex_Maximize;
    property ImageIndex_Close: integer read fImageIndex_Close write fImageIndex_Close;
    property ImageIndex_Restore: integer read fImageIndex_Restore
      write fImageIndex_Restore;
    property UseSystemColors: boolean read fUseSystemColors write SetUseSystemColors;
    property TitleOnCenter: boolean read fTitleOnCenter write SetTitleOnCenter;
    property ShowIcon: boolean read fShowIcon write SetShowIcon;
    property ShowTitle: boolean read fShowTitle write SetShowTitle;
  end;

type
  TRoundedWindowCornerType = (RoundedCornerDefault, RoundedCornerOff,
    RoundedCornerOn, RoundedCornerSmall);

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJTitlePanel]);
end;

procedure TSGJTitlePanel.SetShowTitle(AChecked: boolean);
begin
  if fShowTitle<> AChecked then
  begin
    fShowTitle := AChecked;
    Paint;
  end;
end;

procedure TSGJTitlePanel.SetShowIcon(AChecked: boolean);
begin
  if fShowIcon <> AChecked then
  begin
    fShowIcon := AChecked;
    Paint;
  end;
end;

procedure TSGJTitlePanel.SetTitleOnCenter(AChecked: boolean);
begin
  if fTitleOnCenter <> AChecked then
  begin
    fTitleOnCenter := AChecked;
    Paint;
  end;
end;

procedure TSGJTitlePanel.SetUseSystemColors(AChecked: boolean);
begin
  if fUseSystemColors <> AChecked then
  begin
    fUseSystemColors := AChecked;
    Paint;
  end;
end;

procedure TSGJTitlePanel.Btn_click(Sender: TObject);
begin
  case (Sender as TColorSpeedButton).Tag of
    0: GetParentForm(Self).Close;
    1: Application.Minimize;
    2: DblClick;
  end;
end;

{$IFDEF Windows}
procedure TSGJTitlePanel.btn_OnPaint(Sender: TObject);
var
   Details: TThemedElementDetails;
begin
  if (fUseSystemColors) and  (Win32MajorVersion<6)  then
       if ThemeServices.ThemesEnabled then
      begin
    Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
    ThemeServices.DrawElement(btn_Close.Canvas.Handle, Details, btn_Close.ClientRect);

        if GetParentForm(Self).WindowState=wsMaximized then
        begin
          Details := ThemeServices.GetElementDetails(twRestoreButtonNormal);
  ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);
        end;
        if GetParentForm(Self).WindowState=wsNormal then
        begin
            Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
    ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);

        end;


    Details := ThemeServices.GetElementDetails(twMinButtonNormal);
    ThemeServices.DrawElement(btn_Minimize.Canvas.Handle, Details, btn_Minimize.ClientRect);

   end;
end;
procedure TSGJTitlePanel.btn_OnMouseMove(Sender: TObject;
Shift: TShiftState; X, Y: Integer);
var
   Details: TThemedElementDetails;
begin
  if (fUseSystemColors) and  (Win32MajorVersion<6)  then
       if ThemeServices.ThemesEnabled then
  begin
      case (Sender as TColorSpeedButton).Tag of
        0: begin
           Details := ThemeServices.GetElementDetails(twCloseButtonHot);
           ThemeServices.DrawElement(btn_Close.Canvas.Handle, Details, btn_Close.ClientRect);
           end;
        2: begin
           if GetParentForm(Self).WindowState=wsMaximized then begin
           Details := ThemeServices.GetElementDetails(twRestoreButtonHot);
           ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);
           end;
           if GetParentForm(Self).WindowState=wsNormal then begin
           Details := ThemeServices.GetElementDetails(twMaxButtonHot);
           ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);
           end
           end;
        1: begin
           Details := ThemeServices.GetElementDetails(twMinButtonHot);
           ThemeServices.DrawElement(btn_Minimize.Canvas.Handle, Details, btn_Minimize.ClientRect);
           end;
      end;
  end;
end;
procedure TSGJTitlePanel.btn_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   Details: TThemedElementDetails;
begin
  if (fUseSystemColors) and  (Win32MajorVersion<6)
     then
       if ThemeServices.ThemesEnabled then
  begin
      case (Sender as TColorSpeedButton).Tag of
        0: begin
           Details := ThemeServices.GetElementDetails(twCloseButtonPushed);
           ThemeServices.DrawElement(btn_Close.Canvas.Handle, Details, btn_Close.ClientRect);
           end;
        2: begin
           if GetParentForm(Self).WindowState=wsMaximized then begin
           Details := ThemeServices.GetElementDetails(twRestoreButtonPushed);
           ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);
           end;
           if GetParentForm(Self).WindowState=wsNormal then begin
           Details := ThemeServices.GetElementDetails(twMaxButtonPushed);
           ThemeServices.DrawElement(btn_Restore.Canvas.Handle, Details, btn_Restore.ClientRect);
           end
           end;
        1: begin
           Details := ThemeServices.GetElementDetails(twMinButtonPushed);
           ThemeServices.DrawElement(btn_Minimize.Canvas.Handle, Details, btn_Minimize.ClientRect);
           end;
      end;
  end;
end;
{$endif}

procedure TSGJTitlePanel.DblClick();
begin
  DoubleClick := True;
  if GetParentForm(Self).WindowState = wsMaximized then
    GetParentForm(Self).WindowState := wsNormal
  else
  begin
    with Screen.MonitorFromWindow(GetParentForm(Self).Handle).WorkAreaRect do
    begin
      GetParentForm(Self).Constraints.MaxHeight := Bottom;
      GetParentForm(Self).Constraints.MaxWidth := Right;
    end;
    GetParentForm(Self).WindowState := wsMaximized;
  end;
end;

procedure TSGJTitlePanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseDownPt := Point(X, Y);
end;

procedure TSGJTitlePanel.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  if not DoubleClick then
    if (ssLeft in Shift) then
    begin
      GetParentForm(Self).Left := GetParentForm(Self).Left + (X - FMouseDownPt.X);
      GetParentForm(Self).Top := GetParentForm(Self).Top + (Y - FMouseDownPt.Y);
    end;
  DoubleClick := False;
end;

procedure TSGJTitlePanel.Paint();
{$IFDEF Windows}
const
 DWMWA_BORDER_COLOR =34 ;
DWMWA_CAPTION_COLOR =35;
var
Registry: TRegistry;
IconHandle: HICON;
HI: THandle;
IconIndex: word;
SColor, GColor: array of string;
GradColor:TCOLOR;
PixelColor : TColor;
R: TRect;
Details: TThemedElementDetails;
{$ENDIF}
begin
  inherited Paint;

  {$IFDEF Windows}
   if (fUseSystemColors) and  (Win32MajorVersion>=6) then
   begin

   //win 11 titlebar color
   Registry := TRegistry.Create(KEY_READ);
     try
       Registry.RootKey := HKEY_CURRENT_USER;
       Registry.OpenKey('\Software\Microsoft\Windows\DWM',false);
       if Registry.ValueExists('ColorizationColor')then
       AColor := Registry.ReadInteger('ColorizationColor')else
       AColor:=clBlack;
       AColor:=RGB(GetBValue(AColor),GetGValue(AColor),GetRValue(AColor));
       self.Color:=AColor
     finally
       Registry.Free;
     end;
   end
   else
   if ((fUseSystemColors) and  (Win32MajorVersion<=6) and not DwmCompositionEnabled)
    then
       if ThemeServices.ThemesEnabled then
      begin
    Details := ThemeServices.GetElementDetails(twCaptionActive);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, self.ClientRect);


    btn_Close.BorderSpacing.Right:=5;
    btn_Close.BorderSpacing.top:=3;
    btn_Close.BorderSpacing.Bottom:=3;
    btn_Restore.BorderSpacing.top:=3;
    btn_Restore.BorderSpacing.Bottom:=3;
    btn_Minimize.BorderSpacing.top:=3;
    btn_Minimize.BorderSpacing.Bottom:=3;
      end else begin
     Registry := TRegistry.Create(KEY_READ);
       try
         Registry.RootKey := HKEY_CURRENT_USER;
         Registry.OpenKey('\Control Panel\Colors',false);
         SColor := Registry.ReadString('ActiveTitle').Split(' ');
         GColor := Registry.ReadString('GradientActiveTitle').Split(' ');
         AColor:=RGB(SColor[0].ToInteger,SColor[1].ToInteger,SColor[2].ToInteger);
         GradColor:=RGB(GColor[0].ToInteger,GColor[1].ToInteger,GColor[2].ToInteger);
         self.Color:=AColor;
         R:=Rect(0, 0, self.Width, self.Height);
         Canvas.GradientFill(R,AColor,GradColor,gdHorizontal);

       finally
         Registry.Free;
       end;
   end
   else
     AColor:=self.Color;




     // Win 11 Fix title bar top pixels
     if not ( csDesigning in ComponentState) then
     begin
          if Win32MajorVersion>=10 then begin
          dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_CAPTION_COLOR, @AColor,SizeOf(AColor));
          dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_BORDER_COLOR, @AColor,SizeOf(AColor));
          end;
     end;

        if (GetParentForm(Self).BorderStyle=bsNone) or (csDesigning in ComponentState) then
    begin

    if fShowIcon then
    begin
    HI := ExtractIcon(hInstance, PChar(Application.ExeName), 0);
    DrawIconEx(Canvas.Handle,3, (self.Height div 2) - 12, HI,
      24 ,24, 0, 0, DI_NORMAL or DI_COMPAT);
    end;


    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := clWhite;
    Canvas.Font.Size:=Font.Size;
    Canvas.Font.Style:=[fsBold];
    if fShowTitle then
       if fTitleOnCenter then
          Canvas.TextOut(self.Width div 2 - Canvas.TextWidth(GetParentForm(Self).Caption) div 2 ,(self.Height div 2)- (Canvas.TextHeight(GetParentForm(Self).Caption)div 2),GetParentForm(Self).Caption)
       else
       if fShowIcon then
          Canvas.TextOut(36,(self.Height div 2)- (Canvas.TextHeight(GetParentForm(Self).Caption)div 2),GetParentForm(Self).Caption)
       else
          Canvas.TextOut(5,(self.Height div 2)- (Canvas.TextHeight(GetParentForm(Self).Caption)div 2),GetParentForm(Self).Caption);


    PixelColor := Canvas.Pixels[btn_Minimize.Left,0];

    btn_Close.StateNormal.Color:=PixelColor;//AColor;
    btn_Close.StateNormal.BorderWidth:=0;
    btn_Close.StateHover.Color:=clMaroon;
    btn_Close.StateHover.BorderWidth:=0;
    if (fImages<>nil) then
    btn_Close.Images:=fImages;
    btn_Close.ImageIndex:=fImageIndex_Close;


    btn_Minimize.StateNormal.Color:=PixelColor;//AColor;
    btn_Minimize.StateNormal.BorderWidth:=0;
    btn_Minimize.StateHover.Color:=clSilver;
    btn_Minimize.StateHover.BorderWidth:=0;
    if (fImages<>nil) then
    btn_Minimize.Images:=fImages;
    btn_Minimize.ImageIndex:=fImageIndex_Minimize;

    btn_Restore.StateNormal.Color:=PixelColor;//AColor;
    btn_Restore.StateNormal.BorderWidth:=0;
    btn_Restore.StateHover.Color:=clSilver;
    btn_Restore.StateHover.BorderWidth:=0;
    if (fImages<>nil) then
    btn_Restore.Images:=fImages;
    if GetParentForm(Self).WindowState=wsMaximized then
    btn_Restore.ImageIndex:=fImageIndex_Restore;
    if GetParentForm(Self).WindowState=wsNormal then
    btn_Restore.ImageIndex:=fImageIndex_Maximize;



    end
        else
        begin
          btn_Restore.Visible:=false ;
          btn_Minimize.Visible:=false;
          btn_Close.Visible:=false;
        end;
  {$ENDIF}
end;

constructor TSGJTitlePanel.Create(AOwner: TComponent);
  {$IFDEF Windows}
const
DWMWA_WINDOW_CORNER_PREFERENCE = 33  ;
DWMWCP_DEFAULT    = 0; // Let the system decide whether or not to round window corners
DWMWCP_DONOTROUND = 1; // Never round window corners
DWMWCP_ROUND      = 2; // Round the corners if appropriate
DWMWCP_ROUNDSMALL = 3; // Round the corners if appropriate, with a small radius
DWMWA_USE_IMMERSIVE_DARK_MODE = 20;


var
   CornerType: TRoundedWindowCornerType ;
   DWM_WINDOW_CORNER_PREFERENCE: DWORD;
    USE_DARK_MODE:Windows.Bool = false;
  {$EndIf}
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  parent := TWinControl(AOwner);
  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
  {$EndIf}

  self.BorderStyle := bsNone;
  self.BevelInner := bvNone;
  self.BevelOuter := bvNone;
  self.Height := 34;
  self.Align := alTop;
  self.fUseSystemColors := True;

  {$IFDEF Windows}
  if not ( csDesigning in ComponentState) then
  begin
    if Win32MajorVersion>=10 then begin
  //Win 11 runded cornels
    CornerType:=RoundedCornerOn;
        case CornerType of
          RoundedCornerOff:     DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
          RoundedCornerOn:      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
          RoundedCornerSmall:   DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
        else
          DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
        end;
   dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_WINDOW_CORNER_PREFERENCE, @DWM_WINDOW_CORNER_PREFERENCE,SizeOf(DWM_WINDOW_CORNER_PREFERENCE));
   // dark mode
   //dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_USE_IMMERSIVE_DARK_MODE, @USE_DARK_MODE,SizeOf(USE_DARK_MODE));
    end;


    SetWindowLong(GetParentForm(Self).Handle,GWL_Style,GetWindowLong(self.Parent.Handle, GWL_Style)  or ws_thickframe);
   end;
  {$ENDIF}




    btn_Minimize := TColorSpeedButton.Create(self);
    btn_Minimize.Parent := self;
    btn_Minimize.Width := 55;
    btn_Minimize.Height := 32;
    btn_Minimize.Align := alRight;
    btn_Minimize.OnClick := @btn_Click;
       {$IFDEF Windows}
    btn_Minimize.OnPaint:=@btn_OnPaint;

    btn_Minimize.OnMouseMove:=@btn_OnMouseMove;
    btn_Minimize.OnMouseDown:=@btn_MouseDown;
    {$endif}
    btn_Minimize.ImageIndex := -1;
    btn_Minimize.Tag:=1;

    btn_Restore := TColorSpeedButton.Create(self);
    btn_Restore.Parent := self;
    btn_Restore.Width := 55;
    btn_Restore.Height := 32;
    btn_Restore.Align := alRight;
    btn_Restore.OnClick := @Btn_Click;
      {$IFDEF Windows}
    btn_Restore.OnPaint:=@btn_OnPaint;

    btn_Restore.OnMouseMove:=@btn_OnMouseMove;
    btn_Restore.OnMouseDown:=@btn_MouseDown;
      {$endif}
    btn_Restore.ImageIndex := -1;
    btn_Restore.Tag:=2;

    btn_Close := TColorSpeedButton.Create(self);
    btn_Close.Parent := self;
    btn_Close.Width := 55;
    btn_Close.Height := 32;
    btn_Close.Align := alRight;
    btn_Close.OnClick := @btn_Click;
      {$IFDEF Windows}
    btn_Close.OnPaint:=@btn_OnPaint;

    btn_Close.OnMouseMove:=@btn_OnMouseMove;
    btn_Close.OnMouseDown:=@btn_MouseDown;
        {$endif}
    btn_Close.ImageIndex := -1;
    btn_Close.Tag:=0;


end;
{$IFDEF FPC}
initialization
  {$I SGJ.TitlePanel.lrs}
{$ENDIF}

end.
