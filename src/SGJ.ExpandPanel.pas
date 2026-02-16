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
    procedure AnimTimerTick(Sender: TObject);
    procedure StartAnimation(TargetHeight: integer);

    procedure SetCollapsed(AValue: boolean);
    procedure HeaderClick(Sender: TObject);
    procedure HeaderKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Resize; override;
  published
    property HeaderButton: THeaderSGJButton read fHeaderButton;
    property ClientArea: TSGJEPClientArea read fClientAreaSettings
      write fClientAreaSettings;
    property Collapsed: boolean read fCollapsed write SetCollapsed;
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

constructor TSGJExpandPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Set default width and height
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
  fHeaderButton.OnKeyDown := @HeaderKeyDown;

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

procedure TSGJExpandPanel.HeaderKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) then
  begin
    if Height > fHeaderButton.Height then
    begin
      Height := fHeaderButton.Height;
      fHeaderButton.ButtonArrow := baDown;
    end
    else
    begin
      Height := fHeight;
      fHeaderButton.ButtonArrow := baUp;
    end;
  end;
end;

procedure TSGJExpandPanel.HeaderClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;

procedure TSGJExpandPanel.Loaded;
begin
  inherited;
  fHeight := Height;

  if csDesigning in ComponentState then
    Exit;

  if fCollapsed then
    Height := fHeaderButton.Height
  else
    Height := fHeight;
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

  // DESIGN TIME: do NOT collapse visually
  if csDesigning in ComponentState then
    Exit;

  // RUNTIME: collapse/expand with animation
  if fCollapsed then
  begin
    StartAnimation(fHeaderButton.Height);
    fHeaderButton.ButtonArrow := baDown;
  end
  else
  begin
    StartAnimation(fHeight);
    fHeaderButton.ButtonArrow := baUp;
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
    fAnimStep := 20
  else
    fAnimStep := -20;

  fAnimTimer.Enabled := True;
end;

procedure TSGJExpandPanel.Resize;
begin
  inherited Resize;
  if not fCollapsed then
    fHeight := Height;
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.ExpandPanel.lrs}
{$ENDIF}

end.
