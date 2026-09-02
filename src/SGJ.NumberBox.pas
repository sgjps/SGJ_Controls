unit SGJ.NumberBox;

{$mode objfpc}{$H+}

interface

uses
  LResources, Classes, SysUtils, Controls, Forms, StdCtrls, Graphics, LCLType, Types, LCLIntf,
  Buttons, bcbutton, bctypes, bgrabitmap, bgrabitmaptypes, math;

type
  TSGJBorderStyle = (ebsNone, ebsNormal, ebsRoundedCorner);
  TSGJAlignment = (taLeftJustify, taCenter, taRightJustify);

  TSGJNumberBox = class(TCustomControl)
  private
    FMin: integer;
    FMax: integer;
    FEdit: TEdit;
    FBMin: TBCButton;
    FBMax: TBCButton;
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderStyle: TSGJBorderStyle;
    FAlignment: TSGJAlignment;

    function GetText: integer;
    procedure SetText(const AValue: integer);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(AValue: Boolean);

    procedure EditChange(Sender: TObject);

    function GetPasswordChar: Char;
    procedure SetPasswordChar(AValue: Char);

    function GetNumbersOnly: Boolean;
    procedure SetNumbersOnly(AValue: Boolean);

    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderStyle(AValue: TSGJBorderStyle);

    procedure SetAlignment(AValue: TSGJAlignment);
    procedure ApplyAlignment;
    procedure SetMin(AValue: integer);
    procedure SetMax(AValue: integer);
    procedure AfterRenderMin(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
    procedure AfterRenderMax(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
    procedure ClickMin(Sender: TObject);
    procedure ClickMax(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure SetColor(AValue: TColor); override;
    procedure FontChanged(Sender: TObject); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion,AYProportion: Double);override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Value: integer read GetText write SetText;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Alignment: TSGJAlignment read FAlignment write SetAlignment default taLeftJustify;

    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderStyle: TSGJBorderStyle read FBorderStyle write SetBorderStyle default ebsNormal;

    property Align;
    property Anchors;
    property Enabled;
    property Font;
    property Color;
    property Visible;
    property AutoSize;
    property OnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJNumberBox]);
end;

procedure DrawArrowUp(C: TCanvas; X, Y, Size: integer);
var
  W, H: integer;
begin
  Size := ScaleX(Size, 96);

  W := Size;

  H := Size div 2;

  C.Pen.Width := Max(1, ScaleX(2, 96));

  C.MoveTo(X - W, Y + H);
  C.LineTo(X,     Y - H);

  C.MoveTo(X + W, Y + H);
  C.LineTo(X,     Y - H);
end;


procedure DrawArrowDown(C: TCanvas; X, Y, Size: integer);
var
  W, H: integer;
begin
  Size := ScaleX(Size, 96);

  W := Size;
  H := Size div 2;

  C.Pen.Width := Max(1, ScaleX(2, 96));

  C.MoveTo(X - W, Y - H);
  C.LineTo(X,     Y + H);

  C.MoveTo(X + W, Y - H);
  C.LineTo(X,     Y + H);
end;



function AutoAdjustColor(Color: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
  L: Integer;
begin
  Color := ColorToRGB(Color);

  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);

  L := (R * 30 + G * 59 + B * 11) div 100;

  if L < 40 then
    Amount := Abs(Amount);

  if L > 220 then
    Amount := -Abs(Amount);

  R := R + (Amount * R div 100);
  G := G + (Amount * G div 100);
  B := B + (Amount * B div 100);

  if R > 255 then R := 255 else if R < 0 then R := 0;
  if G > 255 then G := 255 else if G < 0 then G := 0;
  if B > 255 then B := 255 else if B < 0 then B := 0;

  Result := RGB(R, G, B);
end;


procedure TSGJNumberBox.DoAutoAdjustLayout(
  const AMode: TLayoutAdjustmentPolicy; const AXProportion,
  AYProportion: Double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
      Width := Round(Width * AXProportion);
      Height := Round(Height * AYProportion);
  end;
end;

constructor TSGJNumberBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 140;
  Height := 23;

  FBorderColor := clBlack;
  FBorderStyle := ebsNormal;
  FAlignment := taLeftJustify;

  AutoSize := True;

  FEdit := TEdit.Create(Self);
  FEdit.Parent := Self;
  FEdit.Align := alClient;
  FEdit.BorderStyle := bsNone;
  FEdit.BorderSpacing.Around := 4;
  FEdit.NumbersOnly := true;
  FEdit.OnKeyDown := @EditKeyDown;
  FEdit.OnChange := @EditChange;

  Alignment:=taRightJustify;

  FBMin := TBCButton.Create(Self);
  FBMin.Parent := Self;
  FBMin.Align := alRight;
  FBMin.BorderSpacing.Top:=3;
  FBMin.BorderSpacing.Bottom:=3;
  FBMin.BorderSpacing.Right:=3;
  FBMin.Width:=40;
  FBMin.Rounding.RoundX:=5;
  FBMin.Rounding.Roundy:=5;
  FBMin.StateNormal.Background.style:=bbsColor;
  FBMin.StateNormal.Background.Color:=Color;
  FBMin.StateClicked.Background.style:=bbsColor;
  FBMin.StateHover.Background.style:=bbsColor;
  FBMin.OnAfterRenderBCButton:=@AfterRenderMin;
  FBMin.OnClick:=@ClickMin;

  FBMax := TBCButton.Create(Self);
  FBMax.Parent := Self;
  FBMax.Align := alRight;
  FBMax.BorderSpacing.Top:=3;
  FBMax.BorderSpacing.Bottom:=3;
  FBMax.BorderSpacing.Right:=3;
  FBMax.Width:=40;
  FBMax.Rounding.RoundX:=5;
  FBMax.Rounding.Roundy:=5;
  FBMax.StateNormal.Background.style:=bbsColor;
  FBMax.StateNormal.Background.Color:=Color;
  FBMax.StateClicked.Background.style:=bbsColor;
  FBMax.StateHover.Background.style:=bbsColor;
  FBMax.OnAfterRenderBCButton:=@AfterRenderMax;
  FBMax.OnClick:=@ClickMax;
end;

procedure TSGJNumberBox.ClickMin(Sender: TObject);
begin
  if Value > FMin then
    Value := Value - 1;
end;

procedure TSGJNumberBox.ClickMax(Sender: TObject);
begin
  if Value < FMax then
    Value := Value + 1;
end;

procedure TSGJNumberBox.AfterRenderMin(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  Bmp: TBitmap;
  Temp: TBGRABitmap;
  X, Y: Integer;
begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(ARect.Width, ARect.Height);
  Bmp.Transparent := True;
  Bmp.TransparentColor := clFuchsia;
  Bmp.Canvas.Brush.Color := clFuchsia;
  Bmp.Canvas.FillRect(Rect(0,0,ARect.Width,ARect.Height));

  X := ARect.Width div 2;
  Y := ARect.Height div 2;
  DrawArrowDown(Bmp.Canvas, X, Y, 6);

  Temp := TBGRABitmap.Create(Bmp);
  ABGRA.PutImage(0, 0, Temp, dmDrawWithTransparency, 255);
  Temp.Free;

  Bmp.Free;
end;


procedure TSGJNumberBox.AfterRenderMax(Sender: TObject;
  const ABGRA: TBGRABitmap; AState: TBCButtonState; ARect: TRect);
var
  Bmp: TBitmap;
  Temp: TBGRABitmap;
  X, Y: Integer;
begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(ARect.Width, ARect.Height);
  Bmp.Transparent := True;
  Bmp.TransparentColor := clFuchsia;
  Bmp.Canvas.Brush.Color := clFuchsia;
  Bmp.Canvas.FillRect(Rect(0,0,ARect.Width,ARect.Height));

  X := ARect.Width div 2;
  Y := ARect.Height div 2;
  DrawArrowUp(Bmp.Canvas, X, Y, 6);

  Temp := TBGRABitmap.Create(Bmp);
  ABGRA.PutImage(0, 0, Temp, dmDrawWithTransparency, 255);
  Temp.Free;

  Bmp.Free;
end;

destructor TSGJNumberBox.Destroy;
begin
  FBMin.Free;
  FBMax.Free;
  FEdit.Free;
  inherited Destroy;
end;

procedure TSGJNumberBox.CreateWnd;
begin
  inherited CreateWnd;
  FEdit.Color := Color;
  ApplyAlignment;
end;

procedure TSGJNumberBox.SetMin(AValue: integer);
begin
  if FMin<>AValue then
  FMin:=AValue;
end;

procedure TSGJNumberBox.SetMax(AValue: integer);
begin
  if FMax<>AValue then
  FMax:=AValue;
end;

procedure TSGJNumberBox.Loaded;
begin
  inherited Loaded;
  if Color = clDefault then
      Color := clWhite;

  FEdit.Color := Color;
  ApplyAlignment;
  FBMin.Width:=FBMin.Height;
  FBMax.Width:=FBMax.Height;
  FBMin.StateNormal.Background.Color:=Color;
  FBMin.StateHover.Background.Color:=AutoAdjustColor(Color,-20);
  FBMin.StateClicked.Background.Color:=AutoAdjustColor(Color,-40);

  FBMax.StateNormal.Background.Color:=Color;
  FBMax.StateHover.Background.Color:=AutoAdjustColor(Color,-20);
  FBMax.StateClicked.Background.Color:=AutoAdjustColor(Color,-40);

end;

procedure TSGJNumberBox.Resize;
begin
  inherited Resize;
end;

procedure TSGJNumberBox.SetColor(AValue: TColor);
begin
  if AValue = Color then Exit;
  inherited SetColor(AValue);
  FEdit.Color := AValue;
  Invalidate;
end;

procedure TSGJNumberBox.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FEdit.Font.Assign(Font);
  InvalidatePreferredSize;
end;

procedure TSGJNumberBox.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  H: Integer;
begin
  Canvas.Font.Assign(Font);
  H := Canvas.TextHeight('Ag');

  PreferredHeight := H + ScaleX(8,96);
end;

procedure TSGJNumberBox.EditChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSGJNumberBox.GetText: integer;
begin
  if TryStrToInt(FEdit.Text, Result) then
    Exit;

  Result := FMin;
end;

procedure TSGJNumberBox.SetText(const AValue: integer);
var
  V: Integer;
begin
  V := AValue;

  if V < FMin then V := FMin;
  if V > FMax then V := FMax;

  FEdit.Text := IntToStr(V);
end;

function TSGJNumberBox.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TSGJNumberBox.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

procedure TSGJNumberBox.Paint;
var
  R, Inner: TRect;
begin
  inherited Paint;

  if Parent <> nil then
    Canvas.Brush.Color := Parent.Color
  else
    Canvas.Brush.Color := clBtnFace;
  Canvas.FillRect(ClientRect);

  R := ClientRect;

  Canvas.Pen.Color := FBorderColor;
  Canvas.Brush.Style := bsClear;

  case FBorderStyle of
    ebsNone: ;
    ebsNormal:
      Canvas.Rectangle(R);
    ebsRoundedCorner:
      Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, ScaleX(8,96), ScaleX(8,96));
  end;

  Inner := R;

  if FBorderStyle = ebsNormal then
    InflateRect(Inner, -1, -1);

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;

  if FBorderStyle = ebsRoundedCorner then
    Canvas.RoundRect(Inner.Left, Inner.Top, Inner.Right, Inner.Bottom, 8, 8)
  else
    Canvas.FillRect(Inner);


end;

function TSGJNumberBox.GetPasswordChar: Char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TSGJNumberBox.SetPasswordChar(AValue: Char);
begin
  FEdit.PasswordChar := AValue;
end;

function TSGJNumberBox.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

procedure TSGJNumberBox.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TSGJNumberBox.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TSGJNumberBox.SetBorderStyle(AValue: TSGJBorderStyle);
begin
  if FBorderStyle = AValue then Exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TSGJNumberBox.SetAlignment(AValue: TSGJAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  ApplyAlignment;
end;

procedure TSGJNumberBox.ApplyAlignment;
var
  Style: LongInt;
begin
  if not FEdit.HandleAllocated then Exit;

  Style := GetWindowLong(FEdit.Handle, GWL_STYLE);
  Style := Style and not (ES_CENTER or ES_RIGHT);

  case FAlignment of
    taCenter:       Style := Style or ES_CENTER;
    taRightJustify: Style := Style or ES_RIGHT;
  end;

  SetWindowLong(FEdit.Handle, GWL_STYLE, Style);
  Invalidate;
end;

procedure TSGJNumberBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case Key of
    VK_UP:
      if Value < FMax then
        Value := Value + 1;

    VK_DOWN:
      if Value > FMin then
        Value := Value - 1;
  end;
end;

procedure TSGJNumberBox.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key, Shift);
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.NumberBox.lrs}
{$ENDIF}
end.

