unit SGJ.ExpandPanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  SGJ.Button, ExtCtrls;

type
  THeaderSGJButton = class(TCustomSGJButton)
  published
    property Caption;
    property ColorHover;
    property ColorNormal;
    property Description;
    property Font;
    property FontDescription;
    property ImageIndex;
    property Images;
    property RoundedCorners;
    property ShowArrow;
    property ShowBorder;
    property ShowDescription;
    property Hint;
    property ShowHint;
    property TitleOnCenter;
  end;


type
  TSGJExpandPanel = class(TCustomControl)
  private
    fHeaderButton: THeaderSGJButton;
    fClientArea: TCustomControl;
    fHeight: integer;
    procedure HeaderClick(Sender: TObject);
    procedure SetHeight(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    //  destructor Destroy; override;
  protected
    procedure Loaded; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  published
    property HeaderButton: THeaderSGJButton read fHeaderButton;
    property ExpandedHeight: integer read fHeight write SetHeight;
    property BorderStyle;
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

constructor TSGJExpandPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  parent := TWinControl(AOwner);

  // Set default width and height
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);

  BorderStyle := bsNone;
  Height := 100;
  Width := 100;
  ParentBackground := False;
  color := Parent.Brush.Color;

  ControlStyle := ControlStyle + [csAcceptsControls];

  fHeaderButton := THeaderSGJButton.Create(self);
  fHeaderButton.Parent := self;
  fHeaderButton.SetSubComponent(True);
  fHeaderButton.Align := alTop;
  fHeaderButton.ColorNormal := clBlack;
  fHeaderButton.ColorHover := clSilver;
  fHeaderButton.ControlStyle :=
  fHeaderButton.ControlStyle - [csNoDesignSelectable];
  fHeaderButton.OnClick := @HeaderClick;

  fClientArea := TCustomControl.Create(self);
  fClientArea.parent := self;
  fClientArea.align := alClient;
  fClientArea.ControlStyle := ControlStyle;

  fHeight := 200;
end;

procedure TSGJExpandPanel.SetHeight(AValue: integer);
begin
  fHeight := Scale96ToForm(AValue);
end;

procedure TSGJExpandPanel.HeaderClick(Sender: TObject);
begin
  if Height > fHeaderButton.Height then
    Height := fHeaderButton.Height
  else
    Height := fHeight;
end;

procedure TSGJExpandPanel.Loaded;
var
  i: integer;
begin
  inherited;
  for i := ControlCount - 1 downto 0 do
    if (Controls[i] <> fHeaderButton) and (Controls[i] <> fClientArea) then
      if Controls[i].Top < fHeaderButton.Height then
      Controls[i].Parent := fHeaderButton
      else
      Controls[i].Parent := fClientArea;

  if fClientArea.ControlCount>0 then
  for i:=fClientArea.ControlCount -1 to 0 do
      fClientArea.Controls[i].Top:=fClientArea.Controls[i].Top-fHeaderButton.Height;

  HeaderClick(self);
end;

procedure TSGJExpandPanel.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: integer;
begin
  inherited;
  for i := 0 to fHeaderButton.ControlCount - 1 do
    Proc(fHeaderButton.Controls[i]);
  for i := 0 to fClientArea.ControlCount - 1 do
    Proc(fClientArea.Controls[i]);
end;


end.
