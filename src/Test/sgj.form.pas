unit SGJ.Form;

{$mode objfpc}{$H+}

interface

uses
      {$endif}
  Windows, Registry,dwmapi,uxtheme,    {$endif}
  Controls, Classes, SysUtils, ExtCtrls, Graphics, Themes,  Forms,ImgList;

type
  TSGJForm = class(TPanel)
  private
    fShowTitle: boolean;
    fShowIcon: boolean;
    fUseSystemColors: boolean;
    fTitleOnCenter: boolean;
    fImages: TCustomImageList;
    fImageIndex_Minimize: integer;
    fImageIndex_Maximize: integer;
    fImageIndex_Restore: integer;
    fImageIndex_Close: integer;
    imgCaption, imgTop, imgLeft, ImgBottom, ImgBottomRight, imgRight,
    imgClose, imgMaximize, imgMinimize: TImage;
    DoubleClick:boolean;
    FMouseDownPt: TPoint;
    ButtonHeight:integer;
    ButtonSeparator:Integer;
    ButtonCloseRightMargin:Integer;
    ButtonTop:integer;
    procedure SetUseSystemColors(AChecked: boolean);
    procedure SetTitleOnCenter(AChecked: boolean);
    procedure SetShowIcon(AChecked: boolean);
    procedure SetShowTitle(AChecked: boolean);
    procedure CaptionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
    procedure btn_OnMouseMove(Sender: TObject;
Shift: TShiftState; X, Y: Integer);
    procedure btn_OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure btn_OnMouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure btn_OnMouseLeave(Sender: TObject);
    procedure frame_MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
    procedure btn_click(Sender: TObject);
    procedure DblClick(Sender: TObject);
    procedure CaptionMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
    Function GetBtnRect(ATag: integer):TRect;

  public

    constructor Create(aOwner: TComponent); override;
    procedure Paint; override;
  protected

  published
    property UseSystemColors: boolean read fUseSystemColors write SetUseSystemColors;
    property TitleOnCenter: boolean read fTitleOnCenter write SetTitleOnCenter;
    property ShowIcon: boolean read fShowIcon write SetShowIcon;
    property ShowTitle: boolean read fShowTitle write SetShowTitle;
    property Images: TCustomImageList read fImages write fImages;
    property ImageIndex_Minimize: integer read fImageIndex_Minimize
      write fImageIndex_Minimize;
    property ImageIndex_Maximize: integer read fImageIndex_Maximize
      write fImageIndex_Maximize;
    property ImageIndex_Close: integer read fImageIndex_Close write fImageIndex_Close;
    property ImageIndex_Restore: integer read fImageIndex_Restore
      write fImageIndex_Restore;
  end;

type
  TRoundedWindowCornerType = (RoundedCornerDefault, RoundedCornerOff,
    RoundedCornerOn, RoundedCornerSmall);

var
  SGJForm:TSGJForm;
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJForm]);
end;

procedure TSGJForm.SetShowTitle(AChecked: boolean);
begin
  if fShowTitle <> AChecked then
  begin
    fShowTitle := AChecked;
    Paint;
  end;
end;

procedure TSGJForm.SetShowIcon(AChecked: boolean);
begin
  if fShowIcon <> AChecked then
  begin
    fShowIcon := AChecked;
    Paint;
  end;
end;

procedure TSGJForm.SetTitleOnCenter(AChecked: boolean);
begin
  if fTitleOnCenter <> AChecked then
  begin
    fTitleOnCenter := AChecked;
    Paint;
  end;
end;

procedure TSGJForm.SetUseSystemColors(AChecked: boolean);
begin
  if fUseSystemColors <> AChecked then
  begin
    fUseSystemColors := AChecked;
    Paint;
  end;
end;
procedure TSGJForm.Btn_click(Sender: TObject);
begin
  case (Sender as TImage).Tag of
    0: GetParentForm(Self).Close;
    1: DblClick(Sender);
    2: Application.Minimize;

  end;
end;

procedure TSGJForm.DblClick(Sender: TObject);
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

procedure TSGJForm.CaptionMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not DoubleClick then
    if (ssLeft in Shift) then
    begin
      GetParentForm(Self).Left := GetParentForm(Self).Left + (X - FMouseDownPt.X);
      GetParentForm(Self).Top := GetParentForm(Self).Top + (Y - FMouseDownPt.Y);
    end;
  DoubleClick := False;
end;
procedure TSGJForm.CaptionMouseDown(Sender: TObject;Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  FMouseDownPt := Point(X, Y);
end;
constructor TSGJForm.Create(AOwner: TComponent);
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
  self.Align := alClient;





  imgCaption := TImage.Create(self);
  imgCaption.Parent := self;
  imgCaption.Height := Scale96ToForm(32);
  imgCaption.Align := alTop;
  imgCaption.OnMouseMove:=@CaptionMouseMove;
  imgCaption.OnMouseDown:=@CaptionMouseDown;
  imgCaption.OnDblClick:=@DblClick;


  //imgCaption.Anchors:=[AkTop,AkLeft,AkRight];
  imgTop := TImage.Create(self);
  imgTop.Parent := self;
  imgTop.Height := 2;
  imgTop.Width := Width;
  imgTop.Left := 0;
  imgTop.Top := 0;
  imgTop.Tag:=12;
  imgTop.Cursor:=crSizeNS;
  imgTop.OnMouseDown:=@frame_MouseDown;


  imgLeft := TImage.Create(self);
  imgLeft.Parent := self;
  imgLeft.Tag:=10; 
 imgLeft.Anchors := [AkTop, AkBottom, AkLeft];
  imgLeft.OnMouseDown:=@frame_MouseDown;
  imgLeft.Cursor:=crSizeWE;

  imgRight := TImage.Create(self);
  imgRight.Parent := self;
  imgRight.Tag:=11;
//imgRight.Anchors := [AkTop, AkBottom, AkRight];
  imgRight.OnMouseDown:=@frame_MouseDown;
  imgRight.Cursor:=crSizeWE;



  ImgBottom := TImage.Create(self);
  ImgBottom.Parent := self;
  ImgBottom.Tag:=15;
  ImgBottom.Anchors := [AkBottom, AKLeft, AkRight];
  ImgBottom.OnMouseDown:=@frame_MouseDown;
  ImgBottom.Cursor:=crSizeNS;

  if  not ( csDesigning in ComponentState) then begin

  ImgBottomRight:= TImage.Create(self);
  ImgBottomRight.Parent := self;
//  ImgBottomRight.Left := self.Width - 5;
//  ImgBottomRight.Top := self.Height-5;
  ImgBottomRight.Height:=5;
  ImgBottomRight.Width := 5;
  ImgBottomRight.Tag:=17;
  ImgBottomRight.OnMouseDown:=@frame_MouseDown;
  ImgBottomRight.Cursor:=crSizeNW;

   ImgClose := TImage.Create(self);
ImgClose.Parent := self;
Imgclose.Top:=4;
ImgClose.Width:=Scale96ToForm(24);
ImgClose.Height:=Scale96ToForm(24);
ImgClose.Anchors := [AkTop, AkRight];
ImgClose.Tag:=0;
ImgClose.onMouseMove:=@btn_OnMouseMove;
ImgClose.onMouseDown:=@btn_OnMouseDown;
ImgClose.OnMouseUp:=@btn_onMouseUp;
ImgClose.OnMouseLeave:=@btn_onMouseLeave;
ImgClose.OnClick := @btn_Click;

imgMaximize := TImage.Create(self);
imgMaximize.Parent := self;
//imgMaximize.Left := width-60;
imgMaximize.Top:=4;
imgMaximize.Width:=Scale96ToForm(24);
imgMaximize.Height:=Scale96ToForm(24);
imgMaximize.Anchors := [AkTop, AkRight];
imgMaximize.Tag:=1;
imgMaximize.onMouseMove:=@btn_OnMouseMove;
imgMaximize.onMouseDown:=@btn_OnMouseDown;
imgMaximize.OnMouseUp:=@btn_onMouseUp;
imgMaximize.OnMouseLeave:=@btn_onMouseLeave;
imgMaximize.OnClick := @btn_Click;

 imgMinimize := TImage.Create(self);
 imgMinimize.Parent := self;
// imgMinimize.Left := width-90;
 imgMinimize.Top:=4;
 imgMinimize.Width:=Scale96ToForm(24);
 imgMinimize.Height:=Scale96ToForm(24);
 imgMinimize.Anchors := [AkTop, AkRight];
imgMinimize.Tag:=2;
imgMinimize.onMouseMove:=@btn_OnMouseMove;
imgMinimize.onMouseDown:=@btn_OnMouseDown;
imgMinimize.OnMouseUp:=@btn_onMouseUp;
imgMinimize.OnMouseLeave:=@btn_onMouseLeave;
imgMinimize.OnClick := @btn_Click;

 ButtonHeight:=20;
 ButtonSeparator:=4;
 ButtonTop:=4;
 ButtonCloseRightMargin:=5;

  end;


end;

procedure TSGJForm.frame_MouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
   ReleaseCapture();
  SendMessage(GetParentForm(Self).Handle, WM_NCLBUTTONDOWN, TWinControl(Sender).Tag, 0);
end;

Function TSGJForm.GetBtnRect(ATag: integer):TRect;
begin
  case ATag of
  0: result:= Classes.Rect(Width-ButtonCloseRightMargin-ImgClose.Width, ButtonTop , Width-ButtonCloseRightMargin, ButtonTop+ButtonHeight);
  1: result:= Classes.Rect(Width-ButtonCloseRightMargin-ImgMaximize.Width-ButtonSeparator-ImgClose.Width, ButtonTop , Width-ButtonCloseRightMargin-ImgClose.Width-ButtonSeparator, ButtonTop+ButtonHeight);
  2: result:= Classes.Rect(Width-ButtonCloseRightMargin-(2*ButtonSeparator)-imgMinimize.Width-ImgMaximize.Width-ImgClose.Width, ButtonTop , Width-(2*ButtonSeparator)-ImgMaximize.Width- ImgClose.Width- ButtonCloseRightMargin, ButtonTop+ButtonHeight);
  end;
end;
procedure TSGJForm.btn_OnMouseLeave(Sender: TObject);
var
   Details: TThemedElementDetails;
begin
 case (Sender as TImage).Tag of
   0: begin
 // R :=
  Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
  ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(0));
   end;
   1: begin
      if GetParentForm(Self).WindowState=wsMaximized then
      begin
      Details := ThemeServices.GetElementDetails(twRestoreButtonNormal);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;
      if GetParentForm(Self).WindowState=wsNormal then
      begin
      Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;
       end;
      2: begin
     Details := ThemeServices.GetElementDetails(twMinButtonNormal);
     ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));
      end;
   end;


end;
procedure TSGJForm.btn_OnMouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   Details: TThemedElementDetails;
begin
 case (Sender as TImage).Tag of
   0: begin
      Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
  ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(0));
   end;
   1: begin
      if GetParentForm(Self).WindowState=wsMaximized then
      begin
      //R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
      Details := ThemeServices.GetElementDetails(twRestoreButtonNormal);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;
      if GetParentForm(Self).WindowState=wsNormal then
      begin
      Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;

   end;
       2: begin
        Details := ThemeServices.GetElementDetails(twMinButtonNormal);
     ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));
      end;
 end;
end;

procedure TSGJForm.btn_OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   Details: TThemedElementDetails;
   R: TRect;
begin
 case (Sender as TImage).Tag of
   0: begin
 // R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)+ImgClose.Width, ButtonTop+ButtonHeight);
  Details := ThemeServices.GetElementDetails(twCloseButtonPushed);
  ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(0));
   end;
   1: begin
      if GetParentForm(Self).WindowState=wsMaximized then
      begin
      //R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
      Details := ThemeServices.GetElementDetails(twRestoreButtonPushed);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;
      if GetParentForm(Self).WindowState=wsNormal then
      begin
      //R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
      Details := ThemeServices.GetElementDetails(twMaxButtonPushed);
      ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
      end;

   end;
   2: begin
  //R := Classes.Rect(Width-(3*ButtonSeparator)-Scale96ToForm(72), ButtonTop , Width-(3*ButtonSeparator)-Scale96ToForm(48), ButtonTop+ButtonHeight);
  Details := ThemeServices.GetElementDetails(twMinButtonPushed);
  ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));
   end;
 end;
end;

procedure TSGJForm.btn_OnMouseMove(Sender: TObject;
Shift: TShiftState; X, Y: Integer);
var
   Details: TThemedElementDetails;
  // R: TRect;
begin
 begin
      case (Sender as TImage).Tag of
        0: begin
         //  R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)+ImgClose.Width, ButtonTop+ButtonHeight);
           Details := ThemeServices.GetElementDetails(twCloseButtonHot);
           ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(0));
           end;
        1: begin
           if GetParentForm(Self).WindowState=wsMaximized then
           begin
         //  R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-4-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
           Details := ThemeServices.GetElementDetails(twRestoreButtonHot);
           ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
           end;
           if GetParentForm(Self).WindowState=wsNormal then
           begin
        //   R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
           Details := ThemeServices.GetElementDetails(twMaxButtonHot);
           ThemeServices.DrawElement(Canvas.Handle, Details,GetBtnRect(1));
           end;
           end;
         2: begin
  //R := Classes.Rect(Width-ButtonSeparator-ButtonSeparator-ButtonSeparator-Scale96ToForm(72), ButtonTop , Width-ButtonSeparator-ButtonSeparator-ButtonSeparator-Scale96ToForm(48), ButtonTop+ButtonHeight);
  Details := ThemeServices.GetElementDetails(twMinButtonHot);
  ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));
   end;
      end;

 end;
end;


procedure TSGJForm.Paint();
const
DWMWA_WINDOW_CORNER_PREFERENCE = 33  ;
DWMWCP_DEFAULT    = 0; // Let the system decide whether or not to round window corners
DWMWCP_DONOTROUND = 1; // Never round window corners
DWMWCP_ROUND      = 2; // Round the corners if appropriate
DWMWCP_ROUNDSMALL = 3; // Round the corners if appropriate, with a small radius
DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
DWMWA_BORDER_COLOR =34 ;
DWMWA_CAPTION_COLOR =35;
var
   CornerType: TRoundedWindowCornerType ;
   DWM_WINDOW_CORNER_PREFERENCE: DWORD;

   USE_DARK_MODE:Windows.Bool = false;

  Details: TThemedElementDetails;
  R: TRect;
  HI: THandle;
  SColor, GColor: array of string;
  AColor,GradColor:TCOLOR;
  Registry: TRegistry;
  MARGINS:TMargins;
   pBlurBehind : DWM_BLURBEHIND;
   Region1:hrgn;
   opaque:Windows.Bool;
begin
   //  if ((Win32MajorVersion>-6) and (ThemeServices.ThemesEnabled)) and not (dwmapi.DwmCompositionEnabled) then
   //    ButtonHeight:=16;


  if  not ( csDesigning in ComponentState) then begin
  if UseSystemColors then
  begin
  if (Win32MajorVersion>=6) and (dwmapi.DwmCompositionEnabled) then begin
       ButtonSeparator:=0;
       ButtonTop:=0;
       ButtonCloseRightMargin:=0;
       ButtonHeight:=Scale96ToForm(30);
       ImgClose.Width:=Scale96ToForm(45);
       ImgMaximize.Width:=Scale96ToForm(45);
       ImgMinimize.Width:=Scale96ToForm(45);

     {Registry := TRegistry.Create(KEY_READ);
     try
       Registry.RootKey := HKEY_CURRENT_USER;
       Registry.OpenKey('\Software\Microsoft\Windows\DWM',false);
       if Registry.ValueExists('ColorizationColor')then
       AColor := Registry.ReadInteger('ColorizationColor')else
       AColor:=clBlack;
       AColor:=RGB(GetBValue(AColor),GetGValue(AColor),GetRValue(AColor));
       //self.Color:=AColor;
     finally
       Registry.Free;
     end; }

      dwmapi.DwmGetColorizationColor(AColor,opaque);
      AColor:=RGB(GetBValue(AColor),GetGValue(AColor),GetRValue(AColor));
      self.Color:=AColor;

     MARGINS.cxLeftWidth:=5;
     Margins.cxRightWidth:=5;
     Margins.cyBottomHeight:=5;
     Margins.cyTopHeight:=Scale96ToForm(32);
    // dwmapi.DwmExtendFrameIntoClientArea(GetParentForm(Self).Handle,@MARGINS);;

     pBlurBehind.dwFlags:=DWM_BB_ENABLE or DWM_BB_BLURREGION;
    pBlurBehind.fEnable:=true;

    pBlurBehind.fTransitionOnMaximized:=false;

    if ((Win32MajorVersion=6) and (Win32MinorVersion>1)) or (Win32MajorVersion=10) then begin
    //SetWindowLong(GetParentForm(Self).Handle, GWL_EXSTYLE, GetWindowLong(GetParentForm(Self).Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    //SetLayeredWindowAttributes(GetParentForm(Self).Handle, clFuchsia, 0, LWA_COLORKEY);

      if Win32BuildNumber>=22000 then begin
       CornerType:=RoundedCornerOn;
           case CornerType of
             RoundedCornerOff:     DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
             RoundedCornerOn:      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
             RoundedCornerSmall:   DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
           else
             DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
           end;
           dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_WINDOW_CORNER_PREFERENCE, @DWM_WINDOW_CORNER_PREFERENCE,SizeOf(DWM_WINDOW_CORNER_PREFERENCE));
        end;


         // dark mode
   //dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_USE_IMMERSIVE_DARK_MODE, @USE_DARK_MODE,SizeOf(USE_DARK_MODE));
    end
    else begin  //Vista/7
       ButtonSeparator:=0;
       ButtonTop:=0;
       ButtonHeight:=Scale96ToForm(20);
       ImgClose.Width:=Scale96ToForm(45);

       region1 := CreateRoundRectRgn(0, 0, GetParentForm(Self).width, GetParentForm(Self).height, 15, 15);
       SetWindowRgn(GetParentForm(Self).Handle, region1, true);
     // dwmapi.DwmExtendFrameIntoClientArea(GetParentForm(Self).Handle,@MARGINS);;
     // dwmapi.DwmEnableBlurBehindWindow(GetParentForm(Self).Handle,@pBlurBehind) ;
     end;



  end
  else
  if ThemeServices.ThemesEnabled then
  begin
    if Win32MajorVersion>=6 then
       begin
       ButtonHeight:=Scale96ToForm(16);
       ButtonTop:=Scale96ToForm(8);
       end else
       begin
          ButtonHeight:=Scale96ToForm(24);
          ButtonTop:=Scale96ToForm(4);
          ButtonSeparator:=2
       end;
    R := Classes.Rect(0, 0, Width, Scale96ToForm(32));
    Details := ThemeServices.GetElementDetails(twCaptionActive);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, R);

    R := Classes.Rect(0, Scale96ToForm(32), Scale96ToForm(5), Height);
    Details := ThemeServices.GetElementDetails(twFrameLeftActive);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, R);

    R := Classes.Rect(Width - Scale96ToForm(5), Scale96ToForm(32), Width, Height);
    Details := ThemeServices.GetElementDetails(twFrameRightActive);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, R);

    R := Classes.Rect(0, Height - Scale96ToForm(5), Width, Height);
    Details := ThemeServices.GetElementDetails(twFrameBottomActive);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, R);

   // R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)+ImgClose.Width, ButtonTop+ButtonHeight);
    Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
    ThemeServices.DrawElement(self.Canvas.Handle, Details, GetBtnRect(0));

    //R :=Classes.Rect(Width-(3*ButtonSeparator)-Scale96ToForm(72), ButtonTop , Width-(3*ButtonSeparator)-Scale96ToForm(48), ButtonTop+ButtonHeight);
         Details := ThemeServices.GetElementDetails(twMinButtonNormal);
         ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));

    if GetParentForm(Self).WindowState=wsMaximized then
         begin
         //R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
         Details := ThemeServices.GetElementDetails(twRestoreButtonNormal);
         ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
         end;
         if GetParentForm(Self).WindowState=wsNormal then
         begin
         //R := Classes.Rect(Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24), ButtonTop , Width-ButtonSeparator-Scale96ToForm(24)-ButtonSeparator-Scale96ToForm(24)+ImgMaximize.Width, ButtonTop+ButtonHeight);
         Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
         ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
         end;

  end
  else
  begin
     Registry := TRegistry.Create(KEY_READ);
       try
         Registry.RootKey := HKEY_CURRENT_USER;
         Registry.OpenKey('\Control Panel\Colors',false);
         SColor := Registry.ReadString('ActiveTitle').Split(' ');
         GColor := Registry.ReadString('GradientActiveTitle').Split(' ');
         AColor:=RGB(SColor[0].ToInteger,SColor[1].ToInteger,SColor[2].ToInteger);
         GradColor:=RGB(GColor[0].ToInteger,GColor[1].ToInteger,GColor[2].ToInteger);
         R:=Rect(0, 0, self.Width, Scale96ToForm(25));
         Canvas.GradientFill(R,AColor,GradColor,gdHorizontal);

         SColor := Registry.ReadString('ActiveBorder').Split(' ');
         AColor:=RGB(SColor[0].ToInteger,SColor[1].ToInteger,SColor[2].ToInteger);
         Canvas.Pen.Style:=psSolid;
         Canvas.Pen.Width  :=9;
         Canvas.Pen.Color:=AColor;
         Canvas.Brush.Style:=bsClear;
         Canvas.Rectangle(0, 0, width, height);

         ButtonTop:=Scale96ToForm(8);
         ButtonHeight:=Scale96ToForm(20);

         Details := ThemeServices.GetElementDetails(twCloseButtonNormal);
         ThemeServices.DrawElement(self.Canvas.Handle, Details, GetBtnRect(0));

          Details := ThemeServices.GetElementDetails(twMinButtonNormal);
              ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(2));

         if GetParentForm(Self).WindowState=wsMaximized then
              begin
              Details := ThemeServices.GetElementDetails(twRestoreButtonNormal);
              ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
              end;
              if GetParentForm(Self).WindowState=wsNormal then
              begin
              Details := ThemeServices.GetElementDetails(twMaxButtonNormal);
              ThemeServices.DrawElement(Canvas.Handle, Details, GetBtnRect(1));
              end;
       finally
         Registry.Free;
       end;
  end;

  end
  else
  begin
      self.color:=clred;

      if (Win32MajorVersion=10) and (Win32BuildNumber>=22000) then begin
       CornerType:=RoundedCornerOn;
           case CornerType of
             RoundedCornerOff:     DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DONOTROUND;
             RoundedCornerOn:      DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUND;
             RoundedCornerSmall:   DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_ROUNDSMALL;
           else
             DWM_WINDOW_CORNER_PREFERENCE := DWMWCP_DEFAULT;
           end;
           dwmapi.DwmSetWindowAttribute(GetParentForm(Self).Handle,DWMWA_WINDOW_CORNER_PREFERENCE, @DWM_WINDOW_CORNER_PREFERENCE,SizeOf(DWM_WINDOW_CORNER_PREFERENCE));


        end
       else
       begin
       region1 := CreateRoundRectRgn(0, 0, GetParentForm(Self).width, GetParentForm(Self).height, 15, 15);
       SetWindowRgn(GetParentForm(Self).Handle, region1, true);
       end;
  end;


 

  if fShowIcon then
  begin
    HI := ExtractIcon(hInstance, PChar(Application.ExeName), 0);
    DrawIconEx(Canvas.Handle, Scale96ToForm(6),
      Scale96ToForm(6), HI,
      24, 24, 0, 0, DI_NORMAL or DI_COMPAT);
  end;


  Canvas.Brush.Style := bsClear;
  Canvas.Font.Color := clWhite;
  Canvas.Font.Size := Font.Size;
  Canvas.Font.Style := [fsBold];
  if fShowTitle then
    if fTitleOnCenter then
      Canvas.TextOut(self.Width div 2 -
        Canvas.TextWidth(GetParentForm(Self).Caption) div
        2, Scale96ToForm(16) -
        (Canvas.TextHeight(GetParentForm(Self).Caption) div 2), GetParentForm(Self).Caption)
    else
    if fShowIcon then
      Canvas.TextOut(36, Scale96ToForm(16) -
        (Canvas.TextHeight(GetParentForm(Self).Caption) div 2), GetParentForm(Self).Caption)
    else
      Canvas.TextOut(5, Scale96ToForm(16) -
        (Canvas.TextHeight(GetParentForm(Self).Caption) div 2), GetParentForm(Self).Caption);






  imgClose.BoundsRect:=GetBtnRect(0);
  imgMaximize.BoundsRect:=GetBtnRect(1);
  imgMinimize.BoundsRect:=GetBtnRect(2);

    imgTop.Height := 2;
   imgTop.Width := self.Width;
   imgTop.Left := 0;
   imgTop.Top := 0;

   ImgBottomRight.Left := self.Width - 5;
   ImgBottomRight.Top := self.Height-5;
   ImgBottomRight.Height:=5;
   ImgBottomRight.Width := 5;
  end;

  imgCaption.Width := Width;
  imgCaption.Left := 0;
  imgCaption.Top := 0;
  imgCaption.Align := alTop;

    imgLeft.Left := 0;
  imgLeft.Top := 32;
  imgLeft.Width := 5;
  imgLeft.Height := Height;

  imgRight.Left := Width - 5;
  imgRight.Top:= 32;
  imgRight.Height := Height ;
  imgRight.Width:=5;

  ImgBottom.Left := 5;
  ImgBottom.Top := self.Height - 5;
  ImgBottom.Width := self.Width-10;
  ImgBottom.Height := 5;
 // ImgBottom.Align := alBottom;
end;

end.
