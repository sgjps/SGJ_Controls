unit SGJ.Edit;

{$mode objfpc}{$H+}

interface

uses
  LResources, Classes, SysUtils, Controls, Forms, StdCtrls, Graphics, LCLType, Types, LCLIntf;

type
  TSGJBorderStyle = (ebsNone, ebsNormal, ebsRoundedCorner);
  TSGJAlignment = (taLeftJustify, taCenter, taRightJustify);

  TSGJEdit = class(TCustomControl)
  private
    FEdit: TEdit;
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderStyle: TSGJBorderStyle;
    FAlignment: TSGJAlignment;

    function GetText: string;
    procedure SetText(const AValue: string);

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

  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure SetColor(AValue: TColor); override;
    procedure FontChanged(Sender: TObject); override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion,AYProportion: Double);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Text: string read GetText write SetText;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly default False;

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
  RegisterComponents('SGJ', [TSGJEdit]);
end;

procedure TSGJEdit.DoAutoAdjustLayout(
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

constructor TSGJEdit.Create(AOwner: TComponent);
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

  FEdit.OnChange := @EditChange;
end;

destructor TSGJEdit.Destroy;
begin
  FEdit.Free;
  inherited Destroy;
end;

procedure TSGJEdit.CreateWnd;
begin
  inherited CreateWnd;
  FEdit.Color := Color;
  ApplyAlignment;
end;

procedure TSGJEdit.Loaded;
begin
  inherited Loaded;
  if Color = clDefault then
      Color := clWhite;

  FEdit.Color := Color;
  ApplyAlignment;
end;

procedure TSGJEdit.Resize;
begin
  inherited Resize;
end;

procedure TSGJEdit.SetColor(AValue: TColor);
begin
  if AValue = Color then Exit;
  inherited SetColor(AValue);
  FEdit.Color := AValue;
  Invalidate;
end;

procedure TSGJEdit.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FEdit.Font.Assign(Font);
  InvalidatePreferredSize;
end;

procedure TSGJEdit.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
var
  H: Integer;
begin
  Canvas.Font.Assign(Font);
  H := Canvas.TextHeight('Ag');

  PreferredHeight := H + ScaleX(8,96);
end;

procedure TSGJEdit.EditChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSGJEdit.GetText: string;
begin
  Result := FEdit.Text;
end;

procedure TSGJEdit.SetText(const AValue: string);
begin
  FEdit.Text := AValue;
end;

function TSGJEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

procedure TSGJEdit.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;

procedure TSGJEdit.Paint;
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

function TSGJEdit.GetPasswordChar: Char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TSGJEdit.SetPasswordChar(AValue: Char);
begin
  FEdit.PasswordChar := AValue;
end;

function TSGJEdit.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

procedure TSGJEdit.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TSGJEdit.SetBorderColor(AValue: TColor);
begin
  if FBorderColor = AValue then Exit;
  FBorderColor := AValue;
  Invalidate;
end;

procedure TSGJEdit.SetBorderStyle(AValue: TSGJBorderStyle);
begin
  if FBorderStyle = AValue then Exit;
  FBorderStyle := AValue;
  Invalidate;
end;

procedure TSGJEdit.SetAlignment(AValue: TSGJAlignment);
begin
  if FAlignment = AValue then Exit;
  FAlignment := AValue;
  ApplyAlignment;
end;

procedure TSGJEdit.ApplyAlignment;
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

{$IFDEF FPC}
initialization
  {$I resources/SGJ.Edit.lrs}
{$ENDIF}

end.

