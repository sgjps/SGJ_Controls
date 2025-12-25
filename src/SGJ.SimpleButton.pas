{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }

{ date      : 2025/08/27                                             }

{ version   : 1.5                                                   }

{ This file is part of SGJ Controls for Lazarus                      }

{********************************************************************}
unit SGJ.SimpleButton;

{$IfDef FPC}
{$mode ObjFPC}{$H+}
{$EndIf}

interface

uses
{$IfDef FPC}
  LCLType, LResources,
{$ELSE}
 BGRAGraphics,
{$EndIf}
  bgrabitmap, BGRABitmapTypes,
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms, Messages, Types, ImgList;

type
  TCustomSimpleSGJButton = class(TCustomControl)
  private
    fNormalColor: TColor;
    fHoverColor: TColor;
    fTitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF};
    fImages: TCustomImageList;
    fImageIndex: integer;
    fDesc: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF};
    fShowDescription: boolean;
    fDescriptionFont: TFont;
    fShowBorder: boolean;
    fRoundedCorners: boolean;
    fBorderColor: TColor;
    fCenterTitle: boolean;
    fShowArrow: boolean;
    fGetFocus: boolean;
    procedure SetTitle(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
    procedure SetDescription(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
    procedure SetColor1(AColor: TColor);
    procedure SetBorderColor(AColor: TColor);
    procedure SetShowDescription(AChecked: boolean);
    procedure SetBorder(AChecked: boolean);
    procedure SetRoundedBorder(AChecked: boolean);
    procedure SetTitleOnCenter(AChecked: boolean);
    procedure SetArrow(AChecked: boolean);
    procedure PaintButtonBGRA(AMouseMove: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Description: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read fDesc write SetDescription;
    property ColorNormal: TColor read fNormalColor write SetColor1;
    property ColorHover: TColor read fHoverColor write fHoverColor;
    property Caption: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read FTitle write SetTitle;
    property Images: TCustomImageList read fImages write fImages;
    property ImageIndex: integer read fImageIndex write fImageIndex;
    property ShowDescription: boolean read fShowDescription write SetShowDescription;
    property FontDescription: TFont read fDescriptionFont write fDescriptionFont;
    property ShowBorder: boolean read fShowBorder write SetBorder;
    property BorderColor: TColor read fBorderColor write SetBorderColor;
    property RoundedCorners: boolean read fRoundedCorners write SetRoundedBorder;
    property TitleOnCenter: boolean read fCenterTitle write SetTitleOnCenter;
    property ShowArrow: boolean read fShowArrow write SetArrow;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure Paint; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  end;

type
  TSGJSimpleButton = class(TCustomSimpleSGJButton)
  published
    property Description;
    property ColorNormal;
    property ColorHover;
    property Caption;
    property Images;
    property ImageIndex;
    property ShowDescription;
    property FontDescription;
    property ShowBorder;
    property BorderColor;
    property RoundedCorners;
    property TitleOnCenter;
    property ShowArrow;

    property Font;
    property OnClick;
    //property Color;
    property Anchors;
    property Align;
    property Visible;
    property Enabled;
    property AutoSize;
    property BidiMode;
 //   property BorderSpacing;
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
  RegisterComponents('SGJ', [TSGJSimpleButton]);
end;

procedure TCustomSimpleSGJButton.SetArrow(AChecked: boolean);
begin
  if fShowArrow <> AChecked then
  begin
    fShowArrow := AChecked;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetTitleOnCenter(AChecked: boolean);
begin
  if fCenterTitle <> AChecked then
  begin
    fCenterTitle := AChecked;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetBorder(AChecked: boolean);
begin
  if fShowBorder <> AChecked then
  begin
    fShowBorder := AChecked;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetRoundedBorder(AChecked: boolean);
begin
  if fRoundedCorners <> AChecked then
  begin
    fRoundedCorners := AChecked;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetShowDescription(AChecked: boolean);
begin
  if fShowDescription <> AChecked then
  begin
    fShowDescription := AChecked;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetBorderColor(AColor: TColor);
begin
  if fBorderColor <> AColor then
  begin
    fBorderColor := AColor;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.DoEnter();
var
  image: TBGRABitmap;
begin
  inherited DoEnter;
  fGetFocus := True;
  paint;
end;

procedure TCustomSimpleSGJButton.DoExit();
begin
  inherited DoExit;
  fGetFocus := False;
  paint;
end;

procedure TCustomSimpleSGJButton.SetColor1(AColor: TColor);
begin
  if fNormalColor <> AColor then
  begin
    fNormalColor := AColor;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetTitle(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
begin
  if fTitle <> ATitle then
  begin
    fTitle := ATitle;
    Paint;
  end;
end;

procedure TCustomSimpleSGJButton.SetDescription(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
begin
  if fDesc <> ATitle then
  begin
    fDesc := ATitle;
    Paint;
  end;
end;

constructor TCustomSimpleSGJButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  parent := TWinControl(AOwner);

  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  BorderStyle := bsNone;
  {$ENDIF}
  TabStop := True;
  fDescriptionFont := TFont.Create();
  Height := 32;
  ParentBackground := False;
  ImageIndex := -1;
  color := Parent.Brush.Color;
  fNormalColor := clBtnface;
  fHoverColor := clSilver;
  color := fNormalColor;

  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TCustomSimpleSGJButton.PaintButtonBGRA(AMouseMove: boolean);
var
  image: TBGRABitmap;
  AColor: TColor;
begin
  Canvas.Font.Color := Font.Color;
  Canvas.Font.Size := Font.Size;
  Canvas.Font.Style := Font.Style;

  if not AMouseMove then
  begin
    AColor := fNormalColor;
    Canvas.Brush.Color := fNormalColor;
  end
  else
  begin
    AColor := fHoverColor;
    Canvas.Brush.Color := fHoverColor;
  end;

  image := TBGRABitmap.Create(Width, Height,
    ColorToBGRA(ColorToRGB(Parent.Brush.Color)));
  if fRoundedCorners then
    image.FillRoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10, ColorToBGRA(
      ColorToRGB(AColor)))
  else
    image.FillRect(0, 0, Width, Height, ColorToBGRA(ColorToRGB(AColor)));

  if fShowBorder then
  begin
    if not fRoundedCorners then
      image.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToBGRA(
        ColorToRGB(fBorderColor)), 1)
    else
      image.RoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10, ColorToBGRA(
        ColorToRGB(fBorderColor)), 1);
  end;

  if fGetFocus = True then
  begin
    image.JoinStyle := pjsBevel;
    image.PenStyle := psDot;
    image.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToRGB(clwhite), 1);
  end;

  image.Draw(Canvas, 0, 0, True);
  image.Free;

  if fShowArrow then
  begin
    Canvas.TextOut(Width - 10 - (Canvas.TextWidth('>')), (Height div 2) -
      (Canvas.TextHeight('>') div 2), '>');
  end;


  if (fImages <> nil) then
  begin
    if (fImageIndex <> -1) and (fImageIndex < fImages.Count) then
    begin
      fImages.Resolution[round(fImages.Width *
        (Forms.Screen.PixelsPerInch / 96))].Draw(Canvas, round(8 *
        (Forms.Screen.PixelsPerInch / 96)), (Height div 2) -
        (fImages.Resolution[round(fImages.Width * (Forms.Screen.PixelsPerInch / 96))].Height div 2)
        , fImageIndex, True);
      if not fShowDescription then
        Canvas.TextOut(fImages.Resolution[round(fImages.Width *
          (Forms.Screen.PixelsPerInch / 96))].Width + 24, (Height div 2) -
          (Canvas.TextHeight(fTitle) div 2), fTitle)
      else
      begin
        Canvas.TextOut(fImages.Resolution[round(fImages.Width *
          (Forms.Screen.PixelsPerInch / 96))].Width + 24, (Height div 2) -
          (Canvas.TextHeight(fTitle)) - 1, fTitle);
        Canvas.Font.Color := fDescriptionFont.Color;
        Canvas.Font.Size := fDescriptionFont.Size;
        Canvas.Font.Style := fDescriptionFont.Style;
        if Canvas.TextWidth(fDesc) <
          (Width - (fImages.Resolution[round(fImages.Width * (Forms.Screen.PixelsPerInch / 96))].Width + 32)) then
          Canvas.TextOut(fImages.Resolution[round(fImages.Width *
            (Forms.Screen.PixelsPerInch / 96))].Width + 24, (Height div 2) + 1, fDesc)
        else
          Canvas.TextOut(fImages.Resolution[round(fImages.Width *
            (Forms.Screen.PixelsPerInch / 96))].Width + 24, (Height div 2) + 1, '...');
      end;
    end;
  end
  else
  if not fShowDescription then
  begin
    if not fCenterTitle then
      Canvas.TextOut(8, (Height div 2) - (Canvas.TextHeight(fTitle) div 2), fTitle)
    else
      Canvas.TextOut(Width div 2 - Canvas.TextWidth(fTitle) div
        2, (Height div 2) - (Canvas.TextHeight(fTitle) div 2), fTitle);
  end
  else
  begin
    Canvas.TextOut(8, (Height div 2) - (Canvas.TextHeight(fTitle)) - 1, fTitle);
    Canvas.Font.Color := fDescriptionFont.Color;
    Canvas.Font.Size := fDescriptionFont.Size;
    Canvas.Font.Style := fDescriptionFont.Style;
    Canvas.TextOut(8, (Height div 2) + 1, fDesc);
  end;
end;

procedure TCustomSimpleSGJButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  PaintButtonBGRA(True);
  Cursor := crHandPoint;
end;

procedure TCustomSimpleSGJButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  PaintButtonBGRA(False);
  Cursor := crDefault;
end;

procedure TCustomSimpleSGJButton.Paint();
begin
  inherited;
  if HandleAllocated then
    PaintButtonBGRA(False);
end;

destructor TCustomSimpleSGJButton.Destroy;
begin
  FreeAndNil(fDescriptionFont);
  inherited Destroy;
end;


{$IFDEF FPC}
initialization
  {$I resources/SGJ.SimpleButton.lrs}
{$ENDIF}

end.
