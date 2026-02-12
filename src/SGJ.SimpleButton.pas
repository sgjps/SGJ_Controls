{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }

{ date      : 2025/08/27                                             }

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
    procedure SetHoverColor(AColor: TColor);
    procedure SetBorderColor(AColor: TColor);
    procedure SetShowDescription(AChecked: boolean);
    procedure SetBorder(AChecked: boolean);
    procedure SetRoundedBorder(AChecked: boolean);
    procedure SetTitleOnCenter(AChecked: boolean);
    procedure SetArrow(AChecked: boolean);
    procedure PaintButtonBGRA(AMouseMove: boolean);
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Description: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read fDesc write SetDescription;
    property ColorNormal: TColor read fNormalColor write SetColor1;
    property ColorHover: TColor read fHoverColor write SetHoverColor;
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
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetTitleOnCenter(AChecked: boolean);
begin
  if fCenterTitle <> AChecked then
  begin
    fCenterTitle := AChecked;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetBorder(AChecked: boolean);
begin
  if fShowBorder <> AChecked then
  begin
    fShowBorder := AChecked;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetRoundedBorder(AChecked: boolean);
begin
  if fRoundedCorners <> AChecked then
  begin
    fRoundedCorners := AChecked;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetShowDescription(AChecked: boolean);
begin
  if fShowDescription <> AChecked then
  begin
    fShowDescription := AChecked;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetHoverColor(AColor: TColor);
begin
  if fHoverColor <> AColor then
  begin
    fHoverColor := AColor;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetBorderColor(AColor: TColor);
begin
  if fBorderColor <> AColor then
  begin
    fBorderColor := AColor;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.DoEnter();
begin
  inherited DoEnter;
  fGetFocus := True;
  Invalidate;
end;

procedure TCustomSimpleSGJButton.DoExit();
begin
  inherited DoExit;
  fGetFocus := False;
  Invalidate;
end;

procedure TCustomSimpleSGJButton.SetColor1(AColor: TColor);
begin
  if fNormalColor <> AColor then
  begin
    fNormalColor := AColor;
    Invalidate;
  end;
end;

procedure TCustomSimpleSGJButton.SetTitle(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
begin
  if fTitle <> ATitle then
  begin
    fTitle := ATitle;
    if AutoSize then
    if HandleAllocated then
      AdjustSize; // Recalculate size when caption changes
    Invalidate; // Redraw
  end;
end;

procedure TCustomSimpleSGJButton.SetDescription(ATitle: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
begin
  if fDesc <> ATitle then
  begin
    fDesc := ATitle;
    Invalidate;
  end;
end;

constructor TCustomSimpleSGJButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

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

  fNormalColor := clBtnface;
  fHoverColor := clSilver;
  color := fNormalColor;

  ControlStyle := ControlStyle + [csAcceptsControls];
end;

procedure TCustomSimpleSGJButton.PaintButtonBGRA(AMouseMove: boolean);
var
  image: TBGRABitmap;
  AColor: TColor;
  IWidth, IHeight: integer;
begin
  Canvas.Font.Assign(Self.Font);

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
    image.FillRect(0, 0, Width, Height, ColorToBGRA(AColor));

  if fShowBorder then
  begin
    if not fRoundedCorners then
      image.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToBGRA(
        fBorderColor), 1)
    else
      image.RoundRectAntialias(0, 0, Width - 1, Height - 1, 10, 10, ColorToBGRA(
        fBorderColor), 1);
  end;

  if fGetFocus = True then
  begin
    image.JoinStyle := pjsBevel;
    image.PenStyle := psDot;
    image.RectangleAntialias(0, 0, Width - 1, Height - 1, ColorToBGRA(clwhite), 1);
  end;

  image.Draw(Canvas, 0, 0, True);
  image.Free;

  if fShowArrow then
  begin
    Canvas.TextOut(Width - 10 - (Canvas.TextWidth('>')), (Height div 2) -
      (Canvas.TextHeight('>') div 2), '>');
  end;

  if (fImages <> nil) and (fImageIndex >= 0) and (fImageIndex < fImages.Count) then
  begin
    iWidth:=fImages.Width * Screen.PixelsPerInch div 96;
    iHeight:=fImages.Height * Screen.PixelsPerInch div 96;
    fImages.DrawForPPI(Canvas,8,(Height div 2) - (iHeight div 2),fImageIndex,fimages.Width,
    Screen.PixelsPerInch,1.0,true);

    // Tekst obok obrazka
    if not fShowDescription then
    begin
      Canvas.TextOut(
        16+iWidth,
        (Height div 2) - (Canvas.TextHeight(fTitle) div 2),
        fTitle
      );
    end
    else
    begin
      Canvas.TextOut(
        16+iWidth,
        (Height div 2) - Canvas.TextHeight(fTitle),
        fTitle
      );

      Canvas.Font.Assign(fDescriptionFont);

      if Canvas.TextWidth(fDesc) < (Width - (IWidth  + 24)) then
        Canvas.TextOut(16+iWidth, (Height div 2) + 2, fDesc)
      else
        Canvas.TextOut(16+iWidth, (Height div 2) + 2, '...');
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
  Canvas.Font.Assign(fDescriptionFont);
  Canvas.TextOut(8, (Height div 2) + 1, fDesc);
end;
end;

procedure TCustomSimpleSGJButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  PaintButtonBGRA(True);
  if Cursor <> crHandPoint then Cursor := crHandPoint;
end;

procedure TCustomSimpleSGJButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  PaintButtonBGRA(False);
  if Cursor <> crDefault then Cursor := crDefault;
end;

procedure TCustomSimpleSGJButton.Paint();
begin
  inherited Paint;
  if HandleAllocated then
    PaintButtonBGRA(False);
end;

destructor TCustomSimpleSGJButton.Destroy;
begin
  FreeAndNil(fDescriptionFont);
  inherited Destroy;
end;

procedure TCustomSimpleSGJButton.AdjustSize;
var
  TextWidth, TextHeight: Integer;
  iWidth: integer = 0;
  IHeight: integer = 0;
begin
  if not HandleAllocated then Exit;
  if (fImages <> nil) and (fImageIndex >= 0) and (fImageIndex < fImages.Count) then
  begin
    iWidth:=fImages.Width * Screen.PixelsPerInch div 96;
    iHeight:=fImages.Height * Screen.PixelsPerInch div 96;
  end;
  Canvas.Font.Assign(Self.Font);
  TextWidth := Canvas.TextWidth(fTitle);
  TextHeight := Canvas.TextHeight(fTitle);
  if fShowDescription then
  begin
  if Canvas.TextWidth(fDesc)>TextWidth then
  TextWidth:=Canvas.TextWidth(fDesc);
  TextHeight:=TextHeight+Canvas.TextHeight(fDesc);
  end;
  if TextHeight<iHeight then TextHeight:=iHeight;
  // Add padding for button borders
  if Autosize then
  SetBounds(Left, Top, TextWidth + 20+iWidth, TextHeight + 10);
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.SimpleButton.lrs}
{$ENDIF}

end.
