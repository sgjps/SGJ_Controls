{********************************************************************
 home page : https://www.hiperapps.com
 email     : sgj@sgjps.com

 Control name: TSGJButton

 date      : 2025/12/11
 version   : 2.0

 This file is part of SGJ Controls for Lazarus

********************************************************************}

unit SGJ.Button;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LCLType,LResources, LCLIntf,
  {$ELSE}
  Windows,
  {$ENDIF}
  Themes,
  bgrabitmap, BGRABitmapTypes,
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms, Messages, Types, ImgList;

type
  TSGJTextAlignment = (taCenter, taLeftCenter);
  TSGJImagePosition = (ipCenter, ipLeftCenter, ipTopCenter, ipBottomCenter);

  TCustomSGJButton = class;

type
  TButtonArrow = (baNone, baRight, baDown, baUp);
  TButtonVisualState = (bsNormal, bsHover, bsPressed);

type
  TSGJBtnState = class(TPersistent)
  private
    fOwner: TCustomSGJButton;
    fColor: TColor;
    fBorderColor: TColor;
    fFont: TFont;
    fFontDescription: TFont;
    fImages: TCustomImageList;
    fImageIndex: integer;
    fRoundedCorners: boolean;
    fTextAlignment: TSGJTextAlignment;
    fImagePosition: TSGJImagePosition;
    procedure SetCaptionFont(AValue: TFont);
    procedure SetSubCaptionFont(AValue: TFont);
    procedure SetBorderColor(AColor: TColor);
    procedure SetColor(AColor: TColor);
    procedure SetRoundedBorder(AChecked: boolean);
    procedure SetTextAlign(AValue: TSGJTextAlignment);
    procedure SetImagePosition(AValue: TSGJImagePosition);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AControl: TControl); virtual;
    destructor Destroy; override;
  published
    property Color: TColor read fColor write SetColor;
    property ColorBorder: TColor read fBorderColor write SetBorderColor;
    property Font: TFont read fFont write SetCaptionFont;
    property FontSubCaption: TFont read fFontDescription write SetSubCaptionFont;
    property RoundedCorners: boolean read fRoundedCorners write SetRoundedBorder;
    property Images: TCustomImageList read fImages write fImages;
    property ImageIndex: integer read fImageIndex write fImageIndex;
    property TextAlignment: TSGJTextAlignment read fTextAlignment write SetTextAlign;
    property ImagePosition: TSGJImagePosition read fImagePosition write SetImagePosition;
  end;


type
  TCustomSGJButton = class(TCustomControl)
  private
    fSubCaption: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF};
    fGetFocus: boolean;
    fNormal: TSGJBtnState;
    fHover: TSGJBtnState;
    fClicked: TSGJBtnState;
    fDisabled: TSGJBtnState;
    fMouseMove: TButtonVisualState;
    fthemed: boolean;
    fbuttonArrow: TButtonArrow;
    ButtonState: TSGJBtnState;
    FIBackground: TBGRABitmap;
    FLastState: TButtonVisualState;
    FLastSize: TSize;
    procedure SetSubcaption(AValue: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
    procedure PaintButton();
    procedure SetThemed(AValue: boolean);
    procedure DrawButtonArrow(ACanvas: TCanvas);
    procedure DrawThemedBackground();
    procedure DrawBackground();
    procedure InvalidateBackground;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Themed: boolean read fthemed write SetThemed;
    property ButtonArrow: TButtonArrow read fButtonArrow write fButtonArrow;
    property CaptionLine2: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read fSubCaption write SetSubcaption;
    property ButtonNormal: TSGJBtnState read fNormal write fnormal;
    property ButtonHover: TSGJBtnState read fHover write fHover;
    property ButtonClicked: TSGJBtnState read fClicked write fClicked;
    property ButtonDisabled: TSGJBtnState read fDisabled write fDisabled;
  protected
    procedure AdjustSize; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  end;

type
  TSGJButton = class(TCustomSGJButton)
  published
    property ButtonArrow;
    property ButtonNormal;
    property ButtonHover;
    property ButtonClicked;
    property ButtonDisabled;
    property Caption;
    property CaptionLine2;
    property Themed;
    //property Font;
    property OnClick;
    //property Color;
    property Anchors;
    property Align;
    property Visible;
    property Enabled;
    property AutoSize;
    property BidiMode;
    //  property BorderSpacing;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentBidiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    //    property State;
    property TabOrder;
    property TabStop default True;
    //     property OnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJButton]);
end;

constructor TSGJBtnState.Create(AControl: TControl);
begin
  fFont := TFont.Create();
  fFontDescription := TFont.Create();
  inherited Create;
  FOwner := TCustomSGJButton(AControl);
end;

destructor TSGJBtnState.Destroy;
begin
  FreeAndNil(fFont);
  FreeAndNil(fFontDescription);
  inherited;
end;

procedure TSGJBtnState.SetCaptionFont(AValue: TFont);
begin
  if FFont.IsEqual(AValue) then exit;
  FFont.Assign(AValue);
end;

procedure TSGJBtnState.SetSubCaptionFont(AValue: TFont);
begin
  if fFontDescription.IsEqual(AValue) then exit;
  fFontDescription.Assign(AValue);
end;

procedure TSGJBtnState.SetColor(AColor: TColor);
begin
  if fColor <> AColor then
  begin
    fColor := AColor;
    if Assigned(FOwner) then FOwner.InvalidateBackground;
  end;
end;

procedure TSGJBtnState.SetBorderColor(AColor: TColor);
begin
  if fBorderColor <> AColor then
  begin
    fBorderColor := AColor;
    if Assigned(FOwner) then FOwner.InvalidateBackground;
  end;
end;

procedure TSGJBtnState.SetRoundedBorder(AChecked: boolean);
begin
  if fRoundedCorners <> AChecked then
  begin
    fRoundedCorners := AChecked;
    if Assigned(FOwner) then FOwner.InvalidateBackground;
  end;
end;

procedure TSGJBtnState.SetTextAlign(AValue: TSGJTextAlignment);
begin
  if fTextAlignment <> AValue then
    fTextAlignment := AValue;
end;

procedure TSGJBtnState.SetImagePosition(AValue: TSGJImagePosition);
begin
  if fImagePosition <> AValue then
    fImagePosition := AValue;
end;

procedure TSGJBtnState.Assign(Source: TPersistent);
begin
  if Source is TSGJBtnState then
  begin
    fColor := TSGJBtnState(Source).Color;
    fBorderColor := TSGJBtnState(Source).ColorBorder;
    fFont.Assign(TSGJBtnState(Source).Font);
    fFontDescription.Assign(TSGJBtnState(Source).FontSubCaption);
    fImages := TSGJBtnState(Source).Images;
    fImageIndex := TSGJBtnState(Source).ImageIndex;
    fTextAlignment := TSGJBtnState(Source).TextAlignment;
    fRoundedCorners := TSGJBtnState(Source).RoundedCorners;
    fImagePosition := TSGJBtnState(Source).ImagePosition;
  end
  else
    inherited;
end;

constructor TCustomSGJButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  TabStop := True;
  fNormal := TSGJBtnState.Create(self);
  fHover := TSGJBtnState.Create(self);
  fClicked := TSGJBtnState.Create(self);
  fDisabled := TSGJBtnState.Create(self);
  Height := 32;
  ParentBackground := True;

  fNormal.ImageIndex := -1;
  fHover.ImageIndex := -1;
  fClicked.ImageIndex := -1;
  fDisabled.ImageIndex := -1;
  fNormal.fColor := clSilver;
  fHover.fColor := clGray;
  fClicked.fColor := clMedGray;
  fDisabled.fColor := clGray;
  fDisabled.fFont.Color := clMedGray;
  ControlStyle := ControlStyle + [csAcceptsControls];
end;

destructor TCustomSGJButton.Destroy;
begin
  FreeAndNil(fNormal);
  FreeAndNil(fHover);
  FreeAndNil(fClicked);
  FreeAndNil(fDisabled);
  FreeAndNil(FIBackground);
  inherited Destroy;
end;

procedure TCustomSGJButton.SetSubCaption(AValue:
  {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
begin
  if fSubCaption <> AValue then
  begin
    fSubCaption := AValue;
    Invalidate;
  end;
end;

procedure TCustomSGJButton.SetThemed(AValue: boolean);
begin
  if fThemed <> AValue then
  begin
    fThemed := AValue;
    Invalidate;
  end;
end;

procedure TCustomSGJButton.DoEnter();
begin
  inherited DoEnter;
  fGetFocus := True;
  Invalidate;
end;

procedure TCustomSGJButton.DoExit();
begin
  inherited DoExit;
  fGetFocus := False;
  Invalidate;
end;

procedure TCustomSGJButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  fMouseMove := bsHover;
  if Cursor <> crHandPoint then Cursor := crHandPoint;
  invalidate;
end;

procedure TCustomSGJButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  fMouseMove := bsNormal;
  if Cursor <> crDefault then Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  fMouseMove := bsPressed;
  if Cursor <> crDefault then Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  fMouseMove := bsHover;
  if Cursor <> crDefault then Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_SPACE then
    fMouseMove := bsNormal;
  if Cursor <> crDefault then Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_SPACE then
  begin
    fMouseMove := bsPressed;
    Click;
  end;
  if Cursor <> crDefault then Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.Paint();
begin
  inherited Paint;
  if HandleAllocated then
    PaintButton();
end;

procedure TCustomSGJButton.DrawThemedBackground();
var
  Details: TThemedElementDetails;
begin
  case fMouseMove of
    bsNormal: Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
    bsHover: Details := ThemeServices.GetElementDetails(tbPushButtonHot);
    bsPressed: Details := ThemeServices.GetElementDetails(tbPushButtonPressed);
  end;
  if not self.Enabled then
    Details := ThemeServices.GetElementDetails(tbPushButtonDisabled);
  ThemeServices.DrawElement(self.canvas.Handle, Details, self.ClientRect);
end;

procedure TCustomSGJButton.InvalidateBackground;
begin
  FreeAndNil(FIBackground);
  Invalidate;
end;

procedure TCustomSGJButton.DrawBackground();
var
  BtnColor: TColor;
begin
  if (FIBackground = nil) or (FLastState <> fMouseMove) or
    (FLastSize.cx <> Width) or (FLastSize.cy <> Height) then
  begin
    BtnColor := ButtonState.Color;
    FreeAndNil(FIBackground);
    FIBackground := TBGRABitmap.Create(Width, Height,
      ColorToBGRA(ColorToRGB(Parent.Brush.Color)));

    if ButtonState.RoundedCorners then
      FIBackground.FillRoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10,
        ColorToBGRA(ColorToRGB(BtnColor)))
    else
      FIBackground.FillRect(0, 0, Width, Height, ColorToBGRA(ColorToRGB(BtnColor)));

    if ButtonState.ColorBorder <> clNone then
    begin
      if not ButtonState.RoundedCorners then
        FIBackground.RectangleAntialias(0, 0, Width - 1, Height - 1,
          ColorToBGRA(ColorToRGB(ButtonState.ColorBorder)), 1)
      else
        FIBackground.RoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10,
          ColorToBGRA(ColorToRGB(ButtonState.ColorBorder)), 1);
    end;

    if fGetFocus = True then
    begin
      FIBackground.JoinStyle := pjsBevel;
      FIBackground.PenStyle := psDot;
      FIBackground.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToRGB(clwhite), 1);
    end;

    FLastState := fMouseMove;
    FLastSize.cx := Width;
    FLastSize.cy := Height;
  end;
  FIBackground.Draw(Canvas, 0, 0, True);
end;

procedure TCustomSGJButton.PaintButton();
var
  SubFont: TFont;
  SubMargin, ImgWidth, ImgHeight: integer;
  CaptionHeight: integer;
  R, R2: TRect;
begin
  case fMouseMove of
    bsNormal: ButtonState := ButtonNormal;
    bsHover: ButtonState := ButtonHover;
    bsPressed: ButtonState := ButtonClicked;
  end;
  if not self.Enabled then ButtonState := ButtonDisabled;

  Canvas.Font := ButtonState.Font;
  Canvas.Font.Size := ScaleX(ButtonState.Font.Size, 96);
  Canvas.Brush.Color := ButtonState.Color;

  SubFont := ButtonState.FontSubCaption;
  CaptionHeight := Canvas.TextHeight('Ag');
  SetBkMode(Canvas.Handle, TRANSPARENT);
  {$REGION DrawButton}
  if fThemed then
    DrawThemedBackground()
  else
    DrawBackground();
  {$EndREGION DrawButton}
  SubMargin := round(8 * (Forms.Screen.PixelsPerInch / 96));
  {$REGION Images Support}
  if (ButtonState.Images <> nil) then
  begin
    if (ButtonState.ImageIndex <> -1) and (ButtonState.ImageIndex <
      ButtonState.Images.Count) then
    begin
      ImgWidth := ButtonState.Images.Width * Screen.PixelsPerInch div 96;
      ImgHeight := ButtonState.Images.Height * Screen.PixelsPerInch div 96;

      case ButtonState.ImagePosition of
        ipLeftCenter: ButtonState.Images.DrawForPPI(Canvas,
            SubMargin, (Height div 2) - (ImgHeight div 2), ButtonState.ImageIndex,
            ButtonState.Images.Width,
            Screen.PixelsPerInch, 1.0, True);
        ipCenter: ButtonState.Images.DrawForPPI(Canvas, (Width div 2) -
            (ImgWidth div 2),
            (Height div 2) - (ImgHeight div 2), ButtonState.ImageIndex,
            ButtonState.Images.Width,
            Screen.PixelsPerInch, 1.0, True);
        ipTopCenter: ButtonState.Images.DrawForPPI(Canvas,
            (Width div 2) - (ImgWidth div 2),
            SubMargin, ButtonState.ImageIndex, ButtonState.Images.Width,
            Screen.PixelsPerInch, 1.0, True);
        ipBottomCenter: ButtonState.Images.DrawForPPI(Canvas,
            (Width div 2) - (ImgWidth div 2),
            Height - ImgHeight - SubMargin, ButtonState.ImageIndex,
            ButtonState.Images.Width,
            Screen.PixelsPerInch, 1.0, True);
      end;

      if ButtonState.ImagePosition = ipLeftCenter then
      begin
        //Caption with Images
        if fSubCaption = '' then
        begin
          R := Rect(ImgWidth + (2 * SubMargin), 0, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_VCENTER or DT_SINGLELINE);
        end
        else
        begin
          R := Rect(ImgWidth + (2 * SubMargin), (Height div 2) -
            (Canvas.TextHeight('Ag')) - 1, Canvas.TextWidth(Caption) +
            ImgWidth + (2 * SubMargin), self.Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_SINGLELINE);

          Canvas.Font := ButtonState.FontSubCaption;
          Canvas.Font.Size := ScaleX(ButtonState.FontSubCaption.Size, 96);
          if Canvas.TextWidth(fSubCaption) <
            (Width - (ImgWidth + (3 * SubMargin))) then
          begin
            R := RECT(ImgWidth + (2 * SubMargin), (Height div 2) +
              1, Canvas.TextWidth(fSubCaption) + ImgWidth + (2 * SubMargin),
              self.Height);
            DrawText(Canvas.Handle, PChar(fSubCaption), Length(fSubCaption), R,
              0);
          end
          else
          begin
            R := RECT(ImgWidth + (2 * SubMargin), (Height div 2) +
              1, Canvas.TextWidth(fSubCaption) + ImgWidth + (2 * SubMargin),
              self.Height);
            DrawText(Canvas.Handle, PChar('...'), Length('...'), R,
              DT_SINGLELINE);
          end;
        end;
      end
      else
      //Image Center Top
      if ButtonState.ImagePosition = ipTopCenter then
      begin
        if fSubCaption = '' then
        begin
          R := Rect(0, (ImgHeight + SubMargin), Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_CENTER or DT_SINGLELINE);
        end
        else
        begin
          R := Rect(0, (ImgHeight + SubMargin), self.Width, self.Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_CENTER or DT_SINGLELINE);

          Canvas.Font := ButtonState.FontSubCaption;
          Canvas.Font.Size := ScaleX(ButtonState.FontSubCaption.Size, 96);
          if Canvas.TextWidth(fSubCaption) < (Width - SubMargin) then
          begin
            R := RECT(0, (ImgHeight + (2 * SubMargin) + CaptionHeight),
              self.Width, self.Height);
            DrawText(Canvas.Handle, PChar(fSubCaption), Length(fSubCaption), R,
              DT_CENTER or DT_SINGLELINE);
          end
          else
          begin
            R := RECT(0, (ImgHeight + (2 * SubMargin) + CaptionHeight),
              self.Width, self.Height);
            DrawText(Canvas.Handle, PChar('...'), Length('...'), R,
              DT_CENTER or DT_SINGLELINE);
          end;
        end;
      end
      else
      //Image Center Bottom
      if ButtonState.ImagePosition = ipBottomCenter then
      begin
        if fSubCaption = '' then
        begin
          R := Rect(0, SubMargin, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_CENTER or DT_SINGLELINE);
        end
        else
        begin
          R := Rect(0, SubMargin, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R,
            DT_CENTER or DT_SINGLELINE);

          Canvas.Font := ButtonState.FontSubCaption;
          Canvas.Font.Size := ScaleX(ButtonState.FontSubCaption.Size, 96);
          if Canvas.TextWidth(fSubCaption) < (Width - SubMargin) then
          begin
            R := RECT(0, ((2 * SubMargin) + CaptionHeight), Width, Height);
            DrawText(Canvas.Handle, PChar(fSubCaption), Length(fSubCaption), R,
              DT_CENTER or DT_SINGLELINE);
          end
          else
          begin
            R := RECT(0, ((2 * SubMargin) + CaptionHeight), Width, Height);
            DrawText(Canvas.Handle, PChar('...'), Length('...'), R,
              DT_CENTER or DT_SINGLELINE);
          end;
        end;
      end;
    end;
  end
  {$EndREGION Images Support}
  {$REGION Caption Without Images}
  {TextAlignment support only buttons without images}
  else
  begin
    if fSubCaption = '' then
    begin
      case ButtonState.TextAlignment of
        taCenter: begin
          R := Rect(0, 0, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
            R, DT_VCENTER or DT_CENTER or DT_SINGLELINE);
        end;
        taLeftCenter: begin
          R := Rect(SubMargin, 0, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
            R, DT_VCENTER or DT_SINGLELINE);
        end;
      end;
    end
    else
    begin
      case ButtonState.TextAlignment of
        taCenter: begin
          R := Rect(0, (Height div 2) - (Canvas.TextHeight('Ag')) - 1, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption),
            R, DT_CENTER or DT_SINGLELINE);
          Canvas.Font := ButtonState.FontSubCaption;
          Canvas.Font.Size := ScaleX(ButtonState.FontSubCaption.Size, 96);
          R := Rect(0, (Height div 2) + 1, self.Width, self.Height);
          DrawText(Canvas.Handle, PChar(fSubCaption), Length(fSubCaption),
            R, DT_CENTER or DT_SINGLELINE);
        end;
        taLeftCenter: begin
          R := Rect(SubMargin, (Height div 2) - (Canvas.TextHeight('Ag')) -
            1, Width, Height);
          DrawText(Canvas.Handle, PChar(Caption), Length(Caption), R, DT_SINGLELINE);
          Canvas.Font := ButtonState.FontSubCaption;
          Canvas.Font.Size := ScaleX(ButtonState.FontSubCaption.Size, 96);
          R := Rect(SubMargin, (Height div 2) + 1, Width, Height);
          DrawText(Canvas.Handle, PChar(fSubCaption), Length(fSubCaption),
            R, DT_SINGLELINE);
        end;
      end;
    end;
  end;
  {$EndREGION Caption Without Images}

  if fButtonArrow <> baNone then
    DrawButtonArrow(Canvas);
end;

procedure TCustomSGJButton.DrawButtonArrow(ACanvas: TCanvas);
var
  OldFont: TFont;
  AStyle: TTextStyle;
  BtnSym: string;
  R: TRect;
begin
  // zapamiętujemy czcionkę
  OldFont := TFont.Create;
  OldFont.Assign(ACanvas.Font);

  // ustawienia stylu tekstu
  AStyle := ACanvas.TextStyle;
  AStyle.Alignment := taRightJustify;
  AStyle.Layout := tlCenter;
  AStyle.ShowPrefix := True;

  // wybór symbolu
  {$ifdef msWindows}
  if Win32MajorVersion = 10 then
  begin
    if Win32BuildNumber >= 2200 then
      ACanvas.Font.Name := 'Segoe Fluent Icons'
    else
      ACanvas.Font.Name := 'Segoe MDL2 Assets';

    case fButtonArrow of
      baRight: BtnSym := widechar($E970);
      baDown: BtnSym := widechar($E96E);
      baUp: BtnSym := widechar($E96D);
    end;
  end
  else
  {$endif}
  begin
    case fButtonArrow of
      baRight: BtnSym := '>';
      baDown: BtnSym := '˅';
      baUp: BtnSym := '˄';
    end;
  end;

  ACanvas.Font.Size := ScaleX(8, 96);
  ACanvas.Font.Color := ButtonState.Font.Color;

  R := ClientRect;
  R.Right := R.Right - 5;

  ACanvas.TextRect(R, 0, 0, BtnSym, AStyle);

  ACanvas.Font.Assign(OldFont);
  OldFont.Free;
end;


procedure TCustomSGJButton.AdjustSize;
var
  TextWidth, TextHeight: integer;
  iWidth: integer = 0;
  IHeight: integer = 0;
begin
  if not HandleAllocated then Exit;
  if (fNormal.fImages <> nil) and (fNormal.fImageIndex >= 0) and
    (fNormal.fImageIndex < fNormal.fImages.Count) then
  begin
    iWidth := fNormal.fImages.Width * Screen.PixelsPerInch div 96;
    iHeight := fNormal.fImages.Height * Screen.PixelsPerInch div 96;
  end;
  Canvas.Font.Assign(fNormal.Font);
  TextWidth := Canvas.TextWidth(Caption);
  TextHeight := Canvas.TextHeight(Caption);
  if fSubCaption <> '' then
  begin
    if Canvas.TextWidth(fSubCaption) > TextWidth then
      TextWidth := Canvas.TextWidth(fSubCaption);
    TextHeight := TextHeight + Canvas.TextHeight(fSubCaption);
  end;
  if TextHeight < iHeight then TextHeight := iHeight;
  // Add padding for button borders
  if Autosize then
    SetBounds(Left, Top, TextWidth + 20 + iWidth, TextHeight + 10);

end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.Button.lrs}
{$ENDIF}

end.
