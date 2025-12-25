//********************************************************************
// home page : https://www.sgjps.com
// home page : https://www.hiperapps.com
// email     : sgj@sgjps.com
//
// date      : 2025/12.03
// version   : 1.0
//
//
//
// This file is part of SGJ Controls
//
//********************************************************************
unit SGJ.CheckBox;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}
interface

uses
  {$IFDEF FPC}
    LResources,
  {$ENDIF}
  Classes, Messages, Controls, Graphics, bgrabitmap, BGRABitmapTypes;

type
  TSGJCheckboxStyle = (Checkbox, ToogleButton, RadioButton, ToogleRadioButton);

type
  TSGJToogleStyle = (Default, Style1);

type
  TSGJCBVisual = class(TPersistent)
  private
    fColorInner: TColor;
    fColorOuter: TColor;
    fColorDisabled: TColor;
    fButtonStyle: TSGJCheckboxStyle;
    fToogleStyle:TSGJToogleStyle;
    FOnChange: TNotifyEvent;
    function GetBtnStyle: TSGJCheckboxStyle;
    procedure SetBtnStyle(Value: TSGJCheckboxStyle);
    function GetToogleStyle: TSGJToogleStyle;
    procedure SetToogleStyle(Value: TSGJToogleStyle);
    procedure SetColorInner(AValue: TColor);
    procedure SetColorOuter(AValue: TColor);
    procedure SetColorDisabled(AValue: TColor);
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property ColorInner: TColor
      read fColorInner write SetColorInner default clWhite;
    property ColorOuter: TColor
      read fColorOuter write SetColorOuter default clBlack;
    property ColorDisabled: TColor read fColorDisabled
      write SetColorDisabled default clGray;
    property CheckBoxStyle: TSGJCheckboxStyle
      read GetBtnStyle write SetBtnStyle;
    property ToogleStyle: TSGJToogleStyle
      read GetToogleStyle write SetToogleStyle;
  end;

type
  TSGJCheckbox = class(TCustomControl)
  private
    fChecked: boolean;
    fGetFocus: boolean;
    fVisualOpt: TSGJCBVisual;
    procedure SetVisualOptions(Value: TSGJCBVisual);
    function GetChecked: boolean;
    procedure SetChecked(Value: boolean);
    procedure PaintCheckControl();
    procedure UncheckOthers;
    procedure OnColorChange(Sender: TObject);
  protected
    property ParentColor default False;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure Click; override;
  published
    property VisualOptions: TSGJCBVisual read fVisualOpt write SetVisualOptions;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property Checked: boolean read GetChecked write SetChecked;
    //  property Color;
    property Constraints;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentBidiMode;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
  //  property State;
    property TabOrder;
    property TabStop default True;
    property Visible;
  //  property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
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
  RegisterComponents('SGJ', [TSGJCheckBox]);
end;

procedure TSGJCBVisual.Assign(Source: TPersistent);
begin
  if Source is TSGJCBVisual then
  begin
    FColorInner := TSGJCBVisual(Source).ColorInner;
    FColorOuter := TSGJCBVisual(Source).ColorOuter;
    FColorDisabled := TSGJCBVisual(Source).ColorDisabled;
  end
  else
    inherited;
end;

procedure TSGJCheckbox.SetVisualOptions(Value: TSGJCBVisual);
begin
  FVisualOpt.Assign(Value);
end;

procedure TSGJCBVisual.SetColorInner(AValue: TColor);
begin
  if fColorInner <> AValue then
    fColorInner := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGJCBVisual.SetColorOuter(AValue: TColor);
begin
  if fColorOuter <> AValue then
    fColorOuter := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSGJCBVisual.SetColorDisabled(AValue: TColor);
begin
  if fColorDisabled <> AValue then
    fColorDisabled := AValue;

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TSGJCheckbox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FVisualOpt := TSGJCBVisual.Create;
  FVisualOpt.OnChange := @OnColorChange;
  parent := TWinControl(TheOwner);
  TabStop := True;
  Height := ScaleX(16, 96);
  Width := 150;
  Constraints.MaxHeight := ScaleX(16, 96);
  Constraints.MinHeight := ScaleX(16, 96);
  FVisualOpt.ColorInner := clWhite;
  FVisualOpt.ColorOuter := clBlack;
  FVisualOpt.ColorDisabled := clGray;
  ParentBackground := True;
  ParentColor := True;
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TSGJCheckbox.Destroy;
begin
  FVisualOpt.Free;
  inherited Destroy;
end;

procedure TSGJCheckbox.KeyDown(var Key: word; Shift: TShiftState);
begin
  if Enabled then
  begin
    if Key = 32 then
      if Checked = True then Checked := False
      else
        Checked := True;
    Invalidate;
  end;
  inherited;
end;

procedure TSGJCheckbox.Click;
begin
  if Enabled then
  begin
    if Checked = True then Checked := False
    else
      Checked := True;
    Invalidate;
  end;
  inherited;
end;

procedure TSGJCheckbox.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  Cursor := crHandPoint;
end;

procedure TSGJCheckbox.MouseLeave(var Msg: TMessage);
begin
  inherited;
  Cursor := crDefault;
end;

procedure TSGJCheckbox.DoEnter();
var
  image: TBGRABitmap;
begin
  inherited DoEnter;
  fGetFocus := True;
  Invalidate;
end;

procedure TSGJCheckbox.DoExit();
begin
  inherited DoExit;
  fGetFocus := False;
  Invalidate;
end;

function TSGJCheckbox.GetChecked: boolean;
begin
  Result := fChecked;
end;

procedure TSGJCheckbox.SetChecked(Value: boolean);
begin
  if FChecked = Value then
    Exit;
  FChecked := Value;
  if (FVisualOpt.fButtonStyle = ToogleRadioButton) or (FVisualOpt.fButtonStyle = RadioButton)
  then
  if FChecked then
    UncheckOthers;
  Invalidate;
end;

function TSGJCBVisual.GetBtnStyle: TSGJCheckboxStyle;
begin
  Result := fButtonStyle;
end;

procedure TSGJCBVisual.SetBtnStyle(Value: TSGJCheckboxStyle);
begin
  if fButtonStyle <> Value then
    fButtonStyle := Value;
end;

function TSGJCBVisual.GetToogleStyle: TSGJToogleStyle;
begin
  Result := fToogleStyle;
end;

procedure TSGJCBVisual.SetToogleStyle(Value: TSGJToogleStyle);
begin
  if fToogleStyle <> Value then
    fToogleStyle := Value;
end;

procedure TSGJCheckbox.UncheckOthers;
var
  i: integer;
  control: TWinControl;
begin
  if Parent is TWinControl then
  begin
    control := TWinControl(Parent);
    for i := 0 to control.ControlCount - 1 do
      if (control.Controls[i] <> Self) and (control.Controls[i] is
        TSGJCheckbox) then
        if (TSGJCheckbox(control.Controls[i]).VisualOptions.CheckBoxStyle = ToogleRadioButton)
        or   (TSGJCheckbox(control.Controls[i]).VisualOptions.CheckBoxStyle = RadioButton)
        then
        TSGJCheckbox(control.Controls[i]).Checked := False;
  end;
end;
procedure TSGJCheckbox.PaintCheckControl();
var
  image: TBGRABitmap;
  c: TBGRAPixel;
  c_innerColor, c_OuterColor: TColor;
begin
  if Enabled = False then
    c_innerColor := FVisualOpt.fColorDisabled
  else
    c_innerColor := FVisualOpt.ColorInner;

  c_OuterColor:= FVisualOpt.ColorOuter;

  image := TBGRABitmap.Create(Width, Height,
    ColorToBGRA(ColorToRGB(Parent.Brush.Color)));

  image.FontAntialias := True;
  image.FontStyle := Font.Style;
  image.FontHeight := abs(GetFontData(Font.Reference.Handle).Height);
  c := ColorToRGB(FVisualOpt.ColorOuter);

  //Checkbox
  if FVisualOpt.fButtonStyle = Checkbox then
  begin
    image.JoinStyle := pjsRound;
    image.FillroundRectAntialias(height div 5, height div 5, Height - (height div 5), Height - (height div 5), 4, 4, ColorToRGB(c_innerColor));
    image.RoundRectAntialias(height div 5, height div 5, Height - (height div 5), Height - (height div 5), 4, 4, c, 1);

    if Checked then
    begin
      image.JoinStyle := pjsBevel;
      image.LineCap := pecSquare;
      image.PenStyle := psSolid;

      image.DrawPolyLineAntialias([PointF(Height div 3, Height div 2),
        PointF(Height div 4 + Height div 5, Height - Height div 3), PointF(Height - Height div 3, Height div 5 + height div 7)], c, 1);
      image.Draw(Canvas, 0, 0, True);
    end;
    image.TextOut(Height, Height div 2 - (image.FontHeight div 2),
      Caption, Font.Color);
  end;
  //Radio
  if FVisualOpt.fButtonStyle = RadioButton then
  begin

    image.FillEllipseAntialias(Height div 2, Height div 2, Height div
        3, Height div 3, ColorToBGRA(ColorToRGB(c_innerColor)));

    image.EllipseAntialias(Height div 2, Height div 2, Height div
        3, Height div 3, ColorToBGRA(ColorToRGB(c_OuterColor)),1);
    if Checked then
    begin
      image.FillEllipseAntialias(Height div 2, Height div 2, Height div
          5, Height div 5, ColorToBGRA(ColorToRGB(c_OuterColor)));
    end;
    image.TextOut(Height, Height div 2 - (image.FontHeight div 2),
      Caption, Font.Color);
  end;

  if (FVisualOpt.fButtonStyle = ToogleButton) or (FVisualOpt.fButtonStyle = ToogleRadioButton)then
  begin
    //ToogleButton

    if FVisualOpt.fToogleStyle = Default then
    begin
    image.FillRoundRectAntialias(2, 2, Height * 2-(Height div 4), Height - 2, Height div
       2, Height div 2, c, [rrDefault]);
       //checked
       if Checked then
         image.FillEllipseAntialias(Height + (Height div 3),
           Height div 2, Height div 3, Height div 3, ColorToBGRA(ColorToRGB(c_innerColor)));
       //unchecked
       if not Checked then
         image.FillEllipseAntialias(Height div 2, Height div 2, Height div
           3, Height div 3, ColorToBGRA(ColorToRGB(c_innerColor)));
    end;
    if FVisualOpt.fToogleStyle = Style1 then
    begin
    image.RoundRectAntialias(2, 2, Height * 2-(Height div 4), Height - 2, Height div
       2, Height div 2,ColorToBGRA(ColorToRGB(c_OuterColor)),1,[rrDefault]);
    //checked
    if Checked then
      image.FillEllipseAntialias(Height + (Height div 3),
        Height div 2, Height div 3, Height div 3, ColorToBGRA(ColorToRGB(c_innerColor)));
    //unchecked
    if not Checked then
      image.FillEllipseAntialias(Height div 2, Height div 2, Height div
        3, Height div 3, ColorToBGRA(ColorToRGB(c_innerColor)));
 end;

    image.TextOut(Height * 2-(Height div 5)+2, Height div 2 - (image.FontHeight div 2),
      Caption, Font.Color);
  end;

  if fGetFocus = True then
  begin
    image.JoinStyle := pjsBevel;
    image.PenStyle := psDot;
    image.RectangleAntialias(1, 1, Width - 1, Height - 1,
      ColorToRGB(FVisualOpt.ColorOuter), 1);
  end;

  image.Draw(Canvas, 0, 0, True);
  image.Free;
end;

procedure TSGJCheckbox.OnColorChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TSGJCheckbox.Paint();
begin
  inherited;
  if HandleAllocated then
    PaintCheckControl();
end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.CheckBox.lrs}
{$ENDIF}
end.
