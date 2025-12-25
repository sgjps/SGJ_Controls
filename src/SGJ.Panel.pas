{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }
{                                                                    }
{ date      : 2023/08/15                                             }
{                                                                    }
{ version   : 1.2 (2025/12/06)                                       }
{                                                                    }
{ This file is part of SGJ Controls for Delphi and Lazarus           }
{                                                                    }
{********************************************************************}
unit SGJ.Panel;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
  LResources,LCLType,
  {$ELSE}
  Windows,Vcl.Direct2D,D2D1,
  {$ENDIF}
  Forms, Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
{$IFnDef FPC}
 TTranslateString = string;
{$ENDIF}
  TSGJPnPos = (poDefault, PoCenter,poBottom);

  TSGJPanel = class(TCustomPanel)
  private
          fTitle : TTranslateString;
          fBorderColor: TColor;
	  fShowBorder: boolean;
          fCaptionPos: TSGJPnPos;
          procedure SetCaption(ACaption: TTranslateString);
          procedure SetBorder(ABorder: boolean);
          procedure SetBorderColor(AColor: TColor);
          procedure SetCaptionPos(AValue: TSGJPnPos);
  protected
  public
          constructor Create(AOwner: TComponent); override;
          procedure AdjustClientRect(var aRect: TRect); override;
          procedure Paint; override;
  published
          property Caption: TTranslateString read FTitle write SetCaption;
          property CaptionPosition: TSGJPnPos read fCaptionPos write SetCaptionPos;
          property BorderColor: TColor read fBorderColor write SetBorderColor;
	  property BorderEnabled: boolean read fShowBorder write SetBorder;
          property Align;
          property Alignment;
          property Anchors;
          property AutoSize;
         // property BorderSpacing;
         // property BevelColor;
         // property BevelInner;
         // property BevelOuter;
         // property BevelWidth;
          property BidiMode;
         // property BorderWidth;
         // property BorderStyle;
         // property Caption;
          property ClientHeight;
          property ClientWidth;
          property Color;
          property Constraints;
          property DockSite;
          property DoubleBuffered;
          property DragCursor;
          property DragKind;
          property DragMode;
          property Enabled;
          property Font;
          property FullRepaint;
          property ParentBackground;
          property ParentBidiMode;
          property ParentColor;
          property ParentDoubleBuffered;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          //property ShowAccelChar;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property UseDockManager default True;
          property VerticalAlignment;
          property Visible;
          //property Wordwrap;

          //property OnChangeBounds;
          property OnClick;
          property OnContextPopup;
          property OnDockDrop;
          property OnDockOver;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDock;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnGetSiteInfo;
          //property OnGetDockCaption;
          property OnMouseDown;
          property OnMouseEnter;
          property OnMouseLeave;
          property OnMouseMove;
          property OnMouseUp;
          property OnMouseWheel;
          property OnMouseWheelDown;
          property OnMouseWheelUp;

          property OnResize;
          property OnStartDock;
          property OnStartDrag;
          property OnUnDock;
          {$IFDEF FPC}
          property ChildSizing;
          property OnMouseWheelHorz;
          property OnMouseWheelLeft;
          property OnMouseWheelRight;
          property OnPaint;
          {$ENDIF}
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ',[TSGJPanel]);
end;

procedure TSGJPanel.SetCaptionPos(AValue: TSGJPnPos);
begin
    if fCaptionPos<>AValue then
    begin
       fCaptionPos:=AValue;
       Paint();
    end;
end;

procedure TSGJPanel.SetBorderColor(AColor: TColor);
begin
  if fBorderColor<>AColor then
  begin
     fBorderColor:=AColor;
     Paint();
  end;
end;

procedure TSGJPanel.SetCaption(ACaption: TTranslateString);
begin
  if fTitle<>ACaption then
  begin
     fTitle:=ACaption;
     Paint();
  end;
end;

procedure TSGJPanel.SetBorder(ABorder: boolean);
begin
  if fShowBorder<>ABorder then
  begin
     fShowBorder:=ABorder;
     Paint();
  end;
end;

constructor TSGJPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  parent:=TWinControl(AOwner);
  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
  {$EndIf}

  self.BorderStyle:=bsNone;
  self.BevelInner:=bvNone;
  self.BevelOuter:=bvNone;
  self.Height:=28;
  self.Width:=60;
  self.ParentBackground:=true;
  self.BorderColor:=$00B16300;

end;

procedure TSGJPanel.Paint();
begin
   inherited Paint;
   if HandleAllocated then
    with Canvas do begin
      Brush.Style := bsClear;
      Pen.Color:=self.BorderColor;
      Pen.Width:=2;
      Canvas.Font.Color := self.Font.Color;
      Canvas.font.Style:=self.Font.Style;
      Canvas.Font.Size:=self.Font.Size;
      Brush.Style := bsSolid;
      Canvas.Brush.Color:=self.Color;


      if fShowBorder then
	 if fTitle='' then
            Canvas.RoundRect(2,2,Self.width-2,Self.height-2,8,8)
	  else
          if CaptionPosition <>poBottom then
            Canvas.RoundRect(2,(TextHeight(fTitle)div 2),Self.Width-2,Self.Height-2,8,8)
          else
            Canvas.RoundRect(2,2,Self.width-2,Self.height-TextHeight('T') div 2,8,8);

       Case CaptionPosition of
          poDefault: Canvas.TextOut(15,0, Self.fTitle);
          poCenter: Canvas.TextOut(Self.Width div 2 - TextWidth(fTitle) div 2 ,0, fTitle);
          poBottom: Canvas.TextOut(Self.Width div 2 - TextWidth(fTitle) div 2,self.Height-TextHeight('T')-2, Self.fTitle);
       end;
     end;
end;

procedure TSGJPanel.AdjustClientRect(var aRect: TRect);
begin
    inherited AdjustClientRect(ARect);
    if caption='' then
    ARect:= Rect(
            ARect.left+5,
            ARect.Top+6,
            ARect.Right-5,
            ARect.Bottom-6
            )
    else
    if CaptionPosition<>poBottom then
    ARect:= Rect(
            ARect.left+5,
            ARect.Top+self.Canvas.TextHeight('T')+5,
            ARect.Right-5,
            ARect.Bottom-5
            )
    else
    ARect:= Rect(
            ARect.left+5,
            ARect.Top+5,
            ARect.Right-5,
            ARect.Bottom-5-self.Canvas.TextHeight('T')
            )
end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.Panel.lrs}
 {$ENDIF}
end.
