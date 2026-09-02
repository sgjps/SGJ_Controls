{
This file is part of SGJ Controls for Delphi and Lazarus
home page : https://www.hiperapps.com
email     : sgj@sgjps.com

date      : 2026/02/16
}
unit SGJ.ExpandPanel;

{$mode ObjFPC}{$H+}

interface

uses
  LResources, Classes, SysUtils, Controls, Graphics, LCLType,
  SGJ.Button, ExtCtrls, StdCtrls;

type
  THeaderSGJButton = class(TCustomSGJButton)
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
  published
    property ButtonArrow;
    property ButtonNormal;
    property ButtonHover;
    property ButtonClicked;
    property ButtonDisabled;
    property Caption;
    property CaptionLine2;
    property Hint;
    property ShowHint;
  end;

type
  TSGJEPClientArea = class(TPersistent)
  private
    fBackground: TColor;
    fBorder: TColor;
    frounderdCorners: boolean;
    procedure SetBackground(AValue: TColor);
  public
    constructor Create(AControl: TControl); virtual;
  published
    property Background: TColor read fBackground write SetBackground;
    property BorderColor: TColor read fBorder write fBorder;
    property RoundedCorners: boolean read frounderdCorners write frounderdCorners;
  end;

type
  TSGJExpandPanel = class(TCustomControl)
  private
    fHeaderButton: THeaderSGJButton;
    fClientAreaSettings: TSGJEPClientArea;
    fHeight: integer;
    fCollapsed: boolean;
    fAnimTimer: TTimer;
    fAnimTarget: integer;
    fAnimStep: integer;
    fExpandedHeight: integer;
    fIsAnimating: boolean;
    procedure AnimTimerTick(Sender: TObject);
    procedure StartAnimation(TargetHeight: integer);
    procedure SetCollapsed(AValue: boolean);
    procedure HeaderClick(Sender: TObject);
    procedure SetExpandedHeight(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: double); override;


  published
    property HeaderButton: THeaderSGJButton read fHeaderButton;
    property ClientArea: TSGJEPClientArea read fClientAreaSettings
      write fClientAreaSettings;
    property Collapsed: boolean read fCollapsed write SetCollapsed;
    property ExpandedSize: integer read fExpandedHeight write SetExpandedHeight;
    // property BorderStyle;
    property Font;
    property OnClick;
    property Color;
    property Anchors;
    property Align;
    property Visible;
    property Enabled;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property ChildSizing;
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
  RegisterComponents('SGJ', [TSGJExpandPanel]);
end;

procedure TSGJEPClientArea.SetBackground(AValue: TColor);
begin
  if fBackground <> AValue then
    fBackground := AValue;
end;

constructor TSGJEPClientArea.Create(AControl: TControl);
begin
  inherited Create;
end;

procedure THeaderSGJButton.KeyDown(var Key: word; Shift: TShiftState);
begin
  if Key = VK_SPACE then
  begin
    TSGJExpandPanel(Parent).Collapsed := not TSGJExpandPanel(Parent).Collapsed;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

constructor TSGJExpandPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  BorderStyle := bsNone;
  Height := 100;
  Width := 100;
  ParentBackground := True;
  color := clDefault;
  ChildSizing.VerticalSpacing := 3;

  ControlStyle := ControlStyle + [csAcceptsControls];

  fHeaderButton := THeaderSGJButton.Create(self);
  fHeaderButton.Parent := self;
  fHeaderButton.SetSubComponent(True);
  fHeaderButton.Align := alTop;
  fHeaderButton.ControlStyle :=
    fHeaderButton.ControlStyle - [csNoDesignSelectable] + [csAcceptsControls];
  fHeaderButton.OnClick := @HeaderClick;

  fClientAreaSettings := TSGJEPClientArea.Create(self);
  fClientAreaSettings.Background := clDefault;
  fClientAreaSettings.BorderColor := clDefault;


  fHeaderButton.ButtonArrow := baDown;

  fAnimTimer := TTimer.Create(Self);
  fAnimTimer.Enabled := False;
  fAnimTimer.Interval := 10;
  fAnimTimer.OnTimer := @AnimTimerTick;

end;

destructor TSGJExpandPanel.Destroy;
begin
  fHeaderButton.Free;
  fClientAreaSettings.Free;
  inherited;
end;

procedure TSGJExpandPanel.HeaderClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;

procedure TSGJExpandPanel.Loaded;
begin
  inherited Loaded;

  if csDesigning in ComponentState then
    Exit;

end;


procedure TSGJExpandPanel.Paint;
begin
  inherited;
  Canvas.Pen.Color := ClientArea.BorderColor;
  Canvas.Brush.Color := ClientArea.Background;
  if ClientArea.RoundedCorners then
    Canvas.RoundRect(0, fHeaderButton.Height + 3, Width, Height, 10, 10)
  else
    Canvas.Rectangle(0, fHeaderButton.Height + 3, Width, Height);

end;

procedure TSGJExpandPanel.SetCollapsed(AValue: boolean);
begin
  if fCollapsed = AValue then Exit;
  fCollapsed := AValue;


  if HandleAllocated then
  begin
    if fCollapsed then
    begin
      if not (csDesigning in ComponentState) then
        StartAnimation(HeaderButton.Height)
      else
        Height := HeaderButton.Height;
      fHeaderButton.ButtonArrow := baDown;
    end
    else
    begin
      if not (csDesigning in ComponentState) then
        StartAnimation(fExpandedHeight)
      else
        Height := fExpandedHeight;
      fHeaderButton.ButtonArrow := baUp;
    end;

  end;
end;


procedure TSGJExpandPanel.AnimTimerTick(Sender: TObject);
var
  NewHeight: integer;
begin
  NewHeight := Height + fAnimStep;

  if ((fAnimStep > 0) and (NewHeight >= fAnimTarget)) or
    ((fAnimStep < 0) and (NewHeight <= fAnimTarget)) then
  begin
    Height := fAnimTarget;
    fAnimTimer.Enabled := False;
    Exit;
  end;

  Height := NewHeight;
end;

procedure TSGJExpandPanel.StartAnimation(TargetHeight: integer);
begin
  fAnimTarget := TargetHeight;

  if TargetHeight > Height then
    fAnimStep := 50
  else
    fAnimStep := -50;

  fAnimTimer.Enabled := True;
end;

procedure TSGJExpandPanel.Resize;
begin
  inherited Resize;
  if not fCollapsed then
    fHeight := Height;
end;

procedure TSGJExpandPanel.SetExpandedHeight(AValue: integer);
begin
  if (fExpandedHeight = AValue) then
    exit;

  fExpandedHeight := AValue;
end;

procedure TSGJExpandPanel.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if not Collapsed and (ComponentState * [csLoading] = []) then
    FExpandedHeight := Height;
end;

procedure TSGJExpandPanel.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: double);
begin
  inherited DoAutoAdjustLayout(AMode, AXProportion, AYProportion);

  if AMode in [lapAutoAdjustForDPI, lapAutoAdjustWithoutHorizontalScrolling] then
  begin
    fExpandedHeight := Round(fExpandedHeight * AYProportion);
  end;
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.ExpandPanel.lrs}
{$ENDIF}

end.
