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
  LCLType,LResources,
  {$ELSE}
  WINDOWS,
  {$ENDIF}
  Themes,
   bgrabitmap, BGRABitmapTypes,
   Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms, Messages, Types, ImgList;

type
  TSGJTextAlignment = (taCenter, taLeftCenter);
  TSGJImagePosition = (ipCenter, ipLeftCenter, ipTopCenter, ipBottomCenter);

type
  TSGJBtnState = class(TPersistent)
  private
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
    fMouseMove: byte;
    fthemed: boolean;
    procedure SetSubcaption(AValue: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
    procedure PaintButton();
    procedure SetThemed(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Themed: boolean read fthemed write SetThemed;
    property CaptionLine2: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read fSubCaption write SetSubcaption;
    property ButtonNormal: TSGJBtnState read fNormal write fnormal;
    property ButtonHover: TSGJBtnState read fHover write fHover;
    property ButtonClicked: TSGJBtnState read fClicked write fClicked;
    property ButtonDisabled: TSGJBtnState read fDisabled write fDisabled;
  protected
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
  end;
end;

procedure TSGJBtnState.SetBorderColor(AColor: TColor);
begin
  if fBorderColor <> AColor then
  begin
    fBorderColor := AColor;
  end;
end;

procedure TSGJBtnState.SetRoundedBorder(AChecked: boolean);
begin
  if fRoundedCorners <> AChecked then
  begin
    fRoundedCorners := AChecked;
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
    fFont := TSGJBtnState(Source).Font;
    fFontDescription := TSGJBtnState(Source).FontSubCaption;
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
  parent := TWinControl(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  TabStop := True;
  fNormal := TSGJBtnState.Create(self);
  fHover := TSGJBtnState.Create(self);
  fClicked := TSGJBtnState.Create(self);
  fDisabled := TSGJBtnState.Create(self);
  BorderStyle := bsNone;
  Height := 32;
  ParentBackground := False;
  color := Parent.Brush.Color;

  fNormal.ImageIndex:=-1;
  fHover.ImageIndex:=-1;
  fClicked.ImageIndex:=-1;
  fDisabled.ImageIndex:=-1;
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
  inherited Destroy;
end;

procedure TCustomSGJButton.SetSubCaption(AValue: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
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
var
  image: TBGRABitmap;
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
  fMouseMove := 1;
  Cursor := crHandPoint;
  invalidate;
end;

procedure TCustomSGJButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  fMouseMove := 0;
  Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  fMouseMove := 2;
  Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  fMouseMove := 1;
  Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.KeyUp(var Key: word; Shift: TShiftState);
begin
  inherited;
  if Key = 32 then
    fMouseMove := 0;
  Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  inherited;
  if Key = 32 then
    fMouseMove := 2;
  Cursor := crDefault;
  invalidate;
end;

procedure TCustomSGJButton.Paint();
begin
  inherited;
  if HandleAllocated then
    PaintButton();
end;

procedure TCustomSGJButton.PaintButton();
var
  image: TBGRABitmap;
  BtnColor: TColor;
  SubFont: TFont;
  Rounded: boolean;
  BorderColor: TColor;
  ButtonState: TSGJBtnState;
  Details: TThemedElementDetails;
  ImgRes, SubMargin,
  ImgWidth,ImgHeight:integer;
  CaptionHeight:integer;
begin
  case fMouseMove of
    0: ButtonState := ButtonNormal;
    1: ButtonState := ButtonHover;
    2: ButtonState := ButtonClicked;
  end;
  if not self.Enabled then ButtonState := ButtonDisabled;

  Canvas.Font := ButtonState.Font;
  Canvas.Brush.Color := ButtonState.Color;
  BtnColor := ButtonState.Color;
  SubFont := ButtonState.FontSubCaption;
  CaptionHeight:=Canvas.TextHeight(Caption);
  {$Region 'Themed Button'}
  if fThemed then
  begin
    case fMouseMove of
      0: Details := ThemeServices.GetElementDetails(tbPushButtonNormal);
      1: Details := ThemeServices.GetElementDetails(tbPushButtonHot);
      2: Details := ThemeServices.GetElementDetails(tbPushButtonPressed);
    end;
    if not self.Enabled then
      Details := ThemeServices.GetElementDetails(tbPushButtonDisabled);
    ThemeServices.DrawElement(self.canvas.Handle, Details, self.ClientRect);
  end
  {$EndRegion Themed Button}
  {$REGION DrawButton}
  else
  begin
    image := TBGRABitmap.Create(Width, Height,
      ColorToBGRA(ColorToRGB(Parent.Brush.Color)));

    if ButtonState.RoundedCorners then
      image.FillRoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10,
        ColorToBGRA(ColorToRGB(BtnColor)))
    else
      image.FillRect(0, 0, Width, Height, ColorToBGRA(ColorToRGB(BtnColor)));

    if ButtonState.ColorBorder <> clNone then
    begin
      if not ButtonState.RoundedCorners then
        image.RectangleAntialias(0, 0, Width - 1, Height - 1,
          ColorToBGRA(ColorToRGB(ButtonState.ColorBorder)), 1)
      else
        image.RoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10,
          ColorToBGRA(ColorToRGB(ButtonState.ColorBorder)), 1);
    end;

    if fGetFocus = True then
    begin
      image.JoinStyle := pjsBevel;
      image.PenStyle := psDot;
      image.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToRGB(clwhite), 1);
    end;

    image.Draw(Canvas, 0, 0, True);
    image.Free;
  end;
  {$EndREGION DrawButton}
  SubMargin := round(8 * (Forms.Screen.PixelsPerInch / 96));
  {$REGION Images Support}
  if (ButtonState.Images <> nil) then
  begin
    if (ButtonState.ImageIndex <> -1) and (ButtonState.ImageIndex <
      ButtonState.Images.Count) then
    begin
      ImgRes := round(ButtonState.Images.Width * (Forms.Screen.PixelsPerInch / 96));
      ImgWidth:= ButtonState.Images.Resolution[ImgRes].Width;
      ImgHeight:=ButtonState.Images.Resolution[ImgRes].Height;

      if ButtonState.ImagePosition = ipLeftCenter then
      begin
        ButtonState.Images.Resolution[ImgRes].Draw(Canvas,
          SubMargin, (Height div 2) -
          (ImgHeight div 2),
          ButtonState.ImageIndex, True);
        //Caption with Images
        if fthemed then
        begin
          if fSubCaption = '' then
          ThemeServices.DrawText(self.canvas, Details, Caption,
                        Rect(ImgWidth + (2*SubMargin),0,Width,Height),
                        DT_VCENTER or DT_SINGLELINE, 0)
          else
          begin
            ThemeServices.DrawText(self.canvas, Details, Caption,
              Rect(ImgWidth + (2*SubMargin),
              (Height div 2) - (Canvas.TextHeight(Caption)) - 1,
              Canvas.TextWidth(Caption) + ImgWidth + (2*SubMargin),
              self.Height), 0, 0);

            Canvas.Font := ButtonState.FontSubCaption;
            if Canvas.TextWidth(fSubCaption) < (Width -
              (ImgWidth + (3*SubMargin))) then
              ThemeServices.DrawText(self.canvas, Details, fSubCaption,
                RECT(ImgWidth + (2*SubMargin),
                (Height div 2) + 1, Canvas.TextWidth(fSubCaption) +
                ImgWidth + (2*SubMargin),
                self.Height), 0, 0)
            else
              ThemeServices.DrawText(self.canvas, Details, '...',
                RECT(ImgWidth + (2*SubMargin),
                (Height div 2) + 1, Canvas.TextWidth(fSubCaption) +
                ImgWidth + (2*SubMargin),
                self.Height), 0, 0);

          end;
        end
        else
        if fSubCaption = '' then
          Canvas.TextOut(ImgWidth +(2*SubMargin),
          (Height div 2) - (Canvas.TextHeight(Caption) div 2), Caption)
        else
        begin
          Canvas.TextOut(ImgWidth +
            (2*SubMargin), (Height div 2) - (Canvas.TextHeight(Caption)) - 1, Caption);

          Canvas.Font := ButtonState.FontSubCaption;
          if Canvas.TextWidth(fSubCaption) < (Width -(ImgWidth + (3*SubMargin))) then
            Canvas.TextOut(ImgWidth +(2*SubMargin), (Height div 2) + 1, fSubCaption)
          else
            Canvas.TextOut(ImgWidth +
              (2*SubMargin), (Height div 2) + 1, '...');
        end;

      end;
      if ButtonState.ImagePosition = ipCenter then
      begin
        ButtonState.Images.Resolution[ImgRes].Draw(Canvas,
          (Width div 2) - (ImgWidth div 2),
          (Height div 2) - (ImgHeight div 2),
          ButtonState.ImageIndex, True);
      end;
      //Image Center Top
      if ButtonState.ImagePosition = ipTopCenter then
      begin
        ButtonState.Images.Resolution[ImgRes].Draw(Canvas,
          (Width div 2) - (ImgWidth div 2),
          SubMargin,
          ButtonState.ImageIndex, True);
        if fthemed then
                begin
                  if fSubCaption = '' then
                  ThemeServices.DrawText(self.canvas, Details, Caption,
                      Rect(0,(ImgHeight + SubMargin),Width,Height),
                      DT_CENTER or DT_SINGLELINE, 0)
                  else
                  begin
                    ThemeServices.DrawText(self.canvas, Details, Caption,
                      Rect(0,
                      (ImgHeight + SubMargin),
                      self.width,
                      self.Height), DT_CENTER or DT_SINGLELINE, 0);

                    Canvas.Font := ButtonState.FontSubCaption;
                    if Canvas.TextWidth(fSubCaption) < (Width - SubMargin) then
                      ThemeServices.DrawText(self.canvas, Details, fSubCaption,
                        RECT(0,
                        (ImgHeight + (2*SubMargin)+CaptionHeight),
                        self.Width,
                        self.Height), DT_CENTER or DT_SINGLELINE, 0)
                    else
                      ThemeServices.DrawText(self.canvas, Details, '...',
                        RECT(0,
                        (ImgHeight + (2*SubMargin)+CaptionHeight),

                        self.Width,
                        self.Height), DT_CENTER or DT_SINGLELINE, 0);

                  end;
                end
                else
                if fSubCaption = '' then
                  Canvas.TextOut((Width div 2)-(Canvas.TextWidth(Caption) div 2),
                  (ImgHeight + SubMargin), Caption)
                else
                begin
                  Canvas.TextOut((Width div 2)-(Canvas.TextWidth(Caption) div 2),
                  (ImgHeight + SubMargin), Caption);

                  Canvas.Font := ButtonState.FontSubCaption;
                  if Canvas.TextWidth(fSubCaption) < (Width -(SubMargin)) then
                    Canvas.TextOut((Width div 2)-(Canvas.TextWidth(fSubCaption) div 2),
                    (ImgHeight + (2*SubMargin)+CaptionHeight), fSubCaption)
                  else
                    Canvas.TextOut((Width div 2)-(Canvas.TextWidth(fSubCaption) div 2),
                      (ImgHeight + (2*SubMargin)+CaptionHeight), '...');
                end;


      end;
      //Image Center Bottom
      if ButtonState.ImagePosition = ipBottomCenter then
      begin
        ButtonState.Images.Resolution[ImgRes].Draw(Canvas,
          (Width div 2) - (ImgWidth div 2),
          Height - ImgHeight - SubMargin,
          ButtonState.ImageIndex, True);
        if fthemed then
                begin
                  if fSubCaption = '' then
                    ThemeServices.DrawText(self.canvas, Details, Caption,
                      Rect(0,SubMargin,Width,Height),
                      DT_CENTER or DT_SINGLELINE, 0)
                  else
                  begin
                    ThemeServices.DrawText(self.canvas, Details, Caption,
                      Rect(0,SubMargin,width,Height),
                      DT_CENTER or DT_SINGLELINE, 0);

                    Canvas.Font := ButtonState.FontSubCaption;
                    if Canvas.TextWidth(fSubCaption) < (Width - SubMargin) then
                      ThemeServices.DrawText(self.canvas, Details, fSubCaption,
                        RECT(0,((2*SubMargin)+CaptionHeight),Width,Height),
                        DT_CENTER or DT_SINGLELINE, 0)
                    else
                      ThemeServices.DrawText(self.canvas, Details, '...',
                        RECT(0,((2*SubMargin)+CaptionHeight),Width,Height),
                        DT_CENTER or DT_SINGLELINE, 0);
                  end;
                end
                else
                if fSubCaption = '' then
                  Canvas.TextOut((Width div 2)-(Canvas.TextWidth(Caption) div 2),
                  (SubMargin), Caption)
                else
                begin
                  Canvas.TextOut((Width div 2)-(Canvas.TextWidth(Caption) div 2),
                  SubMargin, Caption);

                  Canvas.Font := ButtonState.FontSubCaption;
                  if Canvas.TextWidth(fSubCaption) < (Width -(SubMargin)) then
                    Canvas.TextOut((Width div 2)-(Canvas.TextWidth(fSubCaption) div 2),
                    ((2*SubMargin)+CaptionHeight), fSubCaption)
                  else
                    Canvas.TextOut((Width div 2)-(Canvas.TextWidth(fSubCaption) div 2),
                      ((2*SubMargin)+CaptionHeight), '...');
                end;
      end;

    end;
  end
  {$EndREGION Images Support}
  {$REGION Caption Without Images}
  {TextAlignment support only buttons without images and without subcaptions}
  else
  if fthemed then
  begin
    if fSubCaption = '' then
    begin
      case ButtonState.TextAlignment of
        taCenter: ThemeServices.DrawText(self.canvas, Details, Caption,
            Rect(0,0,Width, Height),
            DT_VCENTER or DT_CENTER or DT_SINGLELINE, 0);

        taLeftCenter: ThemeServices.DrawText(self.canvas,
                Details, Caption, Rect(SubMargin, 0, Width, Height),
                DT_VCENTER or DT_SINGLELINE, 0)
      end;
    end
    else
    begin
      case ButtonState.TextAlignment of
        taCenter: begin
          ThemeServices.DrawText(self.canvas, Details, Caption,
            Rect(0,(Height div 2) - (Canvas.TextHeight(Caption)) -1,width, Height),
                           DT_CENTER or DT_SINGLELINE, 0);
          Canvas.Font := ButtonState.FontSubCaption;
          ThemeServices.DrawText(self.canvas, Details, fSubCaption,
            Rect(0, (Height div 2) + 1,
            self.width, self.Height), DT_CENTER or DT_SINGLELINE, 0);
                   end;
        taLeftCenter: begin
              ThemeServices.DrawText(self.canvas, Details, Caption,
              Rect(SubMargin, (Height div 2) - (Canvas.TextHeight(Caption)) -
        1, Width, Height), 0, 0);
      Canvas.Font := ButtonState.FontSubCaption;
      ThemeServices.DrawText(self.canvas, Details, fSubCaption,
        Rect(SubMargin, (Height div 2) + 1, Width, Height), 0, 0);
                  end;
        end;
    end;
  end
  else
  if fSubCaption = '' then
  begin
    case ButtonState.TextAlignment of
      taCenter: Canvas.TextOut(Width div 2 - Canvas.TextWidth(Caption) div
          2, (Height div 2) - (Canvas.TextHeight(Caption) div 2), Caption);
      taLeftCenter: Canvas.TextOut(SubMargin, (Height div 2) -
          (Canvas.TextHeight(Caption) div 2), Caption);
    end;
  end
  else
  begin
    case ButtonState.TextAlignment of
         taLeftCenter: begin
           Canvas.TextOut(SubMargin,
                         (Height div 2) - (Canvas.TextHeight(Caption)) - 1, Caption);
           Canvas.Font := ButtonState.FontSubCaption;
           Canvas.TextOut(SubMargin, (Height div 2) + 1, fSubCaption);
                   end;
         taCenter: begin
                       Canvas.TextOut(Width div 2 - Canvas.TextWidth(Caption) div 2,
                       (Height div 2) - (Canvas.TextHeight(Caption)) - 1, Caption);
                       Canvas.Font := ButtonState.FontSubCaption;
                       Canvas.TextOut(Width div 2 - Canvas.TextWidth(fSubCaption) div 2,
                       (Height div 2) + 1, fSubCaption);
                       end;
    end;
  end;
  {$EndREGION Caption Without Images}
end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.Button.lrs}
{$ENDIF}

end.
