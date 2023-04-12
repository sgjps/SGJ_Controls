{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }
{                                                                    }
{ date      : 2023/04/05                                             }
{                                                                    }
{ version   : 1.0                                                    }
{                                                                    }
{ This file is part of SGJ Controls for Delphi and Lazarus           }
{                                                                    }
{ Compilation Configuration:                                         }
{                                                                    }
{ $DEFINE SGJCTRL_W_Canvas  - With Canvas (Default) [Delphi/FPC]     }
{ $DEFINE SGJCTRL_W_GDIPlUS - With GDIPLUS (Windows Only)[Delphi/FPC]}
{ $DEFINE SGJCTRL_W_BGRA    - With BGRABitmap [FPC]                  }
{ $DEFINE SGJCTRL_W_D2D     - With Direct2D [Delphi, Windows 7+]     }
{                                                                    }
{ Define only one from this settings. Only this one is used          }
{ Exception:                                                         }
{ $DEFINE SGJCTRL_W_D2D     - Use Direct2D (if D2D is unsupported)   }
{                             then Canvas is Used                                       }
{ $DEFINE SGJCTRL_W_D2D     - If D2D is unsupported                  }
{ $DEFINE SGJCTRL_W_GDIPlUS - then switch to GDIPLUS                 }
{********************************************************************}
unit SGJ.ToogleButton;
{$IFDEF FPC}
  {$DEFINE SGJCTRL_W_BGRA}
//{$DEFINE SGJCTRL_W_Canvas}
//{$DEFINE SGJCTRL_W_GDIPlUS}
{$ELSE}
//{$DEFINE SGJCTRL_W_Canvas}
{$IF CompilerVersion >= 21}
	{$DEFINE SGJCTRL_W_D2D}
{$IFEND}
  {$DEFINE SGJCTRL_W_GDIPlUS}
{$ENDIF}
interface

uses
{$IFDEF MSWINDOWS}
  Windows,
   {$IFDEF SGJCTRL_W_GDIPlUS}
  Winapi.GDIPAPI, Winapi.GDIPOBJ, Winapi.GDIPUTIL,
   {$ENDIF}
   {$IFDEF SGJCTRL_W_D2D}
    Vcl.Direct2D, D2D1,
   {$ENDIF}
{$ENDIF}

{$IFDEF SGJCTRL_W_BGRA}
  bgrabitmap, BGRABitmapTypes,
{$ENDIF}
  Forms, Messages, Classes, Graphics, ExtCtrls, Controls,StdCtrls;

type
  TSGJToogleButton = class(TCustomPanel)
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
    procedure Paint; override;
    procedure Click; override;
  private
    fTitle: string;
    fChecked: boolean;
    fTextBefore: boolean;
    fButtonColor: TColor;
    FButtonCheckedColor: TColor;
    FButtonUnCheckedColor: TColor;
    procedure SetChecked(AChecked: boolean);
    procedure SetCaption(ACaption: String);
    procedure SetButtonColor(AColor:TColor);
    procedure SetCheckedColor(AColor:TColor);
    procedure SetUnCheckedColor(AColor:TColor);
    procedure SetTextBefore(AChecked: boolean);
    procedure Paint_Canvas();
    {$IFDEF MSWINDOWS}
    {$IFDEF SGJCTRL_W_GDIPlUS}
    procedure Paint_GDIPLUS();
    {$ENDIF}
    {$IFDEF SGJCTRL_W_D2D}
    procedure Paint_D2D();
    {$ENDIF}
    {$ENDIF}
    {$IFDEF SGJCTRL_W_BGRA}
    procedure Paint_BGRABitmap();
    {$ENDIF}
  published
    property Font;
    property Color;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property Caption: string read FTitle write SetCaption;
    property Checked: boolean read fChecked write SetChecked;
    property TextBeforeButton: boolean read fTextBefore write SetTextBefore;
    property ButtonColor: TColor read fButtonColor write SetButtonColor;
    property ButtonCheckedColor: TColor read FButtonCheckedColor
      write SetCheckedColor;
    property ButtonUnCheckedColor: TColor read FButtonUnCheckedColor
      write SetUnCheckedColor;
    property OnClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJToogleButton]);
end;

procedure TSGJToogleButton.SetTextBefore(AChecked: boolean);
begin
  if fTextBefore<>AChecked then
     begin
        fTextBefore:=AChecked;
        Paint;
     end;
end;

procedure TSGJToogleButton.SetCheckedColor(AColor:TColor);
begin
  if FButtonCheckedColor<>AColor then
     begin
        FButtonCheckedColor:=AColor;
        Paint;
     end;
end;

procedure TSGJToogleButton.SetUnCheckedColor(AColor:TColor);
begin
  if FButtonUnCheckedColor<>AColor then
     begin
        FButtonUnCheckedColor:=AColor;
        Paint;
     end;
end;

procedure TSGJToogleButton.SetButtonColor(AColor:TColor);
begin
  if fButtonColor<>AColor then
     begin
        fButtonColor:=AColor;
        Paint;
     end;
end;

procedure TSGJToogleButton.SetCaption(ACaption: String);
begin
  if fTitle<>ACaption then
     begin
        fTitle:=ACaption;
        Paint;
     end;
end;
procedure TSGJToogleButton.SetChecked(AChecked: boolean);
begin
  if fChecked<>AChecked then
  if AChecked = True then
    fChecked:=true
  else
    fChecked:=false;
  Paint;
end;

function GetCircleRect(X, Y: integer; Radius: integer): TRect;
begin
  Result := Rect(X - Radius, Y - Radius, X + Radius, Y + Radius);
end;

constructor TSGJToogleButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  parent:=TWinControl(AOwner);
{$IFDEF FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
{$ENDIF}
  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 30;
  Width := 60;
  Caption := ' ';
  ParentBackground := True;
  ButtonColor := $00E17D04;
  FButtonCheckedColor := $00FFFFFF;
  ButtonUnCheckedColor := $00000000;
end;

procedure TSGJToogleButton.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited;
  Cursor := crHandPoint;
end;

procedure TSGJToogleButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  Cursor := crDefault;
end;

procedure TSGJToogleButton.Click();
begin
  inherited;
  if fChecked = False then
    fChecked := True
  else
    fChecked := False;

  Paint;
end;


{$Region 'Canvas'}
procedure TSGJToogleButton.Paint_Canvas();
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    if Enabled then
    begin
      Brush.Color := ButtonColor;
      Pen.Color := ButtonColor;
    end
    else
    begin
      Brush.Color := clgray;
      Pen.Color := clgray;
    end;

    if fTextBefore then
    begin
      rectangle((Width - Height) - (Height div 2), 0, (Width - Height) +
        (Height div 2), Height);
      Ellipse(GetCircleRect(Width + Height div 2 - (Height * 2), Height div
        2, Height div 2));
      Ellipse(GetCircleRect(Width - (Height div 2), Height div 2, Height div 2));

      if Checked = False then
      begin
        Pen.Color := ButtonUnCheckedColor;
        Brush.Color := ButtonUnCheckedColor;
        Ellipse(GetCircleRect((Width - Height) - Height div 2, Height div
          2, Height div 3));
      end
      else
      begin
        Pen.Color := ButtonCheckedColor;
        Brush.Color := ButtonCheckedColor;
        Ellipse(GetCircleRect(Width - (Height div 2), Height div 2, Height div 3));
      end;

      Brush.Style := bsClear;
      Canvas.Font.Color := Font.Color;
      Canvas.font.Style := Font.Style;
      Canvas.Font.Size := Font.Size;

      Canvas.TextOut(5, Height div 2 - (Canvas.TextHeight(self.fTitle) div 2), self.fTitle);
    end
    else
    begin
      rectangle(Height div 2, 0, Height + (Height div 2), Height);
      Ellipse(GetCircleRect(Height div 2, Height div 2, Height div 2));
      Ellipse(GetCircleRect(Height + (Height div 2), Height div 2, Height div 2));

      if Checked = False then
      begin
        Pen.Color := ButtonUnCheckedColor;
        Brush.Color := ButtonUnCheckedColor;
        Ellipse(GetCircleRect(Height div 2, Height div 2, Height div 3));
      end
      else
      begin
        Pen.Color := ButtonCheckedColor;
        Brush.Color := ButtonCheckedColor;
        Ellipse(GetCircleRect(Height * 2 - (Height div 2), Height div 2, Height div 3));
      end;

      Brush.Style := bsClear;
      Canvas.Font.Color := Font.Color;
      Canvas.font.Style := Font.Style;
      Canvas.Font.Size := Font.Size;

      Canvas.TextOut(Height * 2 + 5, Height div 2 -
        (Canvas.TextHeight(self.fTitle) div 2), self.fTitle);
    end;

  end;
end;
{$ENDREGION}

{$Region 'D2D'}
{$IFDEF MSWINDOWS}
{$IFDEF SGJCTRL_W_D2D}
procedure TSGJToogleButton.Paint_D2D();
 var
   LCanvas: TDirect2DCanvas;
begin
  if TDirect2DCanvas.Supported then
  begin
   LCanvas := TDirect2DCanvas.Create(Canvas, ClientRect);
   LCanvas.BeginDraw;

   try
    LCanvas.Brush.Style := bsSolid;
    if Enabled then begin
      LCanvas.Brush.Color := ButtonColor;
      LCanvas.Pen.Color := ButtonColor;
    end
    else begin
      LCanvas.Brush.Color := clgray;
      LCanvas.Pen.Color := clgray;
    end;

    if fTextBefore then
    begin
      //LCanvas.Ellipse(width - 60 + 2, 2, width - 60 + 28, 28);
      //LCanvas.rectangle(width - 60 + 12, 2, width - 60 + 42, 28);
      //LCanvas.Ellipse(width - 60 + 30, 2, width - 60 + 56, 28);
         LCanvas.rectangle((width - height)- (height div 2) , 0,(width - height)+ (height div 2), height);
        LCanvas.Ellipse(GetCircleRect(width + height div 2 - (height*2), height div 2, height div 2));
          LCanvas.Ellipse(GetCircleRect(width -(height div 2) , height div 2, height div 2));
      if Checked = false then
      begin
        LCanvas.Pen.Color := ButtonUnCheckedColor;
        LCanvas.Brush.Color := ButtonUnCheckedColor;
        //LCanvas.Ellipse(width - 60 + 7, 7, width - 60 + 24, 24);
        LCanvas.Ellipse(GetCircleRect((width - height)-height div 2, height div 2, height div 3));
      end else
      begin
        LCanvas.Pen.Color := ButtonCheckedColor;
        LCanvas.Brush.Color := ButtonCheckedColor;
        //LCanvas.Ellipse(width - 60 + 36, 7, width - 60 + 53, 24);
        LCanvas.Ellipse(GetCircleRect(width - (height div 2), height div 2, height div 3));
      end;

      LCanvas.Brush.Style := bsClear;
      LCanvas.Font.Color := Font.Color;
      LCanvas.font.Style := Font.Style;
      LCanvas.Font.Size := Font.Size;

      LCanvas.TextOut(5, Height div 2 - (Canvas.TextHeight(Title) div 2), Title);
    end
    else
    begin
      //LCanvas.Ellipse(2, 2, 26, 28);
      //LCanvas.rectangle(12, 2, 42, 28);
      //LCanvas.Ellipse(30, 2, 56, 28);
        LCanvas.Rectangle(height div 2, 0, height+(height div 2), height);
        LCanvas.Ellipse(GetCircleRect(height div 2, height div 2, height div 2));
        LCanvas.Ellipse(GetCircleRect(height +(height div 2) , height div 2, height div 2));

      if Checked = false then
      begin
        LCanvas.Pen.Color := ButtonUnCheckedColor;
        LCanvas.Brush.Color := ButtonUnCheckedColor;
        //LCanvas.Ellipse(7, 7, 24, 24);
         LCanvas.Ellipse(GetCircleRect(height div 2, height div 2, height div 3));
      end else
      begin
        LCanvas.Pen.Color := ButtonCheckedColor;
        LCanvas.Brush.Color := ButtonCheckedColor;
        //LCanvas.Ellipse(36, 7, 53, 24);
        LCanvas.Ellipse(GetCircleRect(height*2-(height div 2),height div 2, height div 3));
      end;
       LCanvas.Brush.Style := bsClear;
      LCanvas.Font.Color := Font.Color;
      LCanvas.font.Style := Font.Style;
      LCanvas.Font.Size := Font.Size;

      LCanvas.TextOut(height *2 +5, Height div 2 - (Canvas.TextHeight(Title) div 2), Title);
    end;



   finally
     LCanvas.EndDraw;
     LCanvas.Free;
   end;
  end
  else
  {$IFDEF SGJCTRL_W_GDIPLUS}
    Paint_GDIPLUS();
  {$ELSE}
    Paint_Canvas();
  {$ENDIF}
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'GDIPLUS"}
{$IFDEF SGJCTRL_W_GDIPLUS}
procedure TSGJToogleButton.Paint_GDIPLUS();
var
  Graphics: TGPGraphics;
  SolidBrush: TGPSolidBrush;
  //  fontFamily: TGPFontFamily;
  //  gdifont: TGPFont;
  //  pointF: TGPPointF;
begin
  inherited;
  Graphics := TGPGraphics.Create(Canvas.Handle);
  Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if not fTextBefore then
  begin
    if Enabled then
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(fButtonColor),
        GetGValue(fButtonColor), GetBValue(fButtonColor)));
    end
    else
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(clGray),
        GetGValue(clGray), GetBValue(clGray)));
    end;
    //graphics.FillEllipse(SolidBrush, 1, 1, 28, 26);
    //graphics.FillRectangle(SolidBrush, 16, 1, 28, 26);
    //graphics.FillEllipse(SolidBrush, 30, 1, 28, 26);
    Graphics.FillEllipse(SolidBrush, 0, 0, Height, Height);
    Graphics.FillRectangle(SolidBrush, Height div 2, 0, Height, Height);
    Graphics.FillEllipse(SolidBrush, Height, 0, Height, Height);

    if Checked = False then
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255,
        GetRValue(FButtonUnCheckedColor), GetGValue(FButtonUnCheckedColor),
        GetBValue(FButtonUnCheckedColor)));
      //graphics.FillEllipse(SolidBrush, 7, 5, 18, 18);
      Graphics.FillEllipse(SolidBrush, Height div 6, Height div 6,
        Height / 1.5, Height / 1.5);
    end
    else
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255,
        GetRValue(FButtonCheckedColor), GetGValue(FButtonCheckedColor),
        GetBValue(FButtonCheckedColor)));
      //graphics.FillEllipse(SolidBrush, 34, 3, 21, 21);
      Graphics.FillEllipse(SolidBrush, Height + (Height div 6), Height div
        6, Height / 1.5, Height / 1.5);
    end;

    {fontFamily := TGPFontFamily.Create(Font.Name);
    gdifont := TGPFont.Create(fontFamily, Font.Size, FontStyleRegular, UnitPixel);
    pointF := MakePoint(Height*2+5, Height / 2 - Font.Size /2 -3);
    solidBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(Font.Color),GetGValue(Font.Color), GetBValue(Font.Color)));
    graphics.DrawString(Title, -1, gdifont, pointF, solidBrush);}
    Canvas.Font.Size := Font.Size;
    Canvas.TextOut(Height * 2 + 5, Height div 2 - (Canvas.TextHeight(self.fTitle) div 2), self.fTitle);
  end
  else
  begin
    if Enabled then
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(fButtonColor),
        GetGValue(fButtonColor), GetBValue(fButtonColor)));
    end
    else
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255, GetRValue(clGray),
        GetGValue(clGray), GetBValue(clGray)));
    end;
    //graphics.FillEllipse(SolidBrush, width - 60 + 1, 1, 28, 26);
    //graphics.FillRectangle(SolidBrush, width - 60 + 16, 1, 28, 26);
    //graphics.FillEllipse(SolidBrush, width - 60 + 30, 1, 28, 26);
    Graphics.FillEllipse(SolidBrush, Width - Height, 0, Height, Height);
    Graphics.FillRectangle(SolidBrush, Width - Height - Height div 2, 0, Height, Height);
    Graphics.FillEllipse(SolidBrush, Width - Height * 2, 0, Height, Height);

    if Checked = False then
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255,
        GetRValue(FButtonUnCheckedColor), GetGValue(FButtonUnCheckedColor),
        GetBValue(FButtonUnCheckedColor)));

      //graphics.FillEllipse(SolidBrush, width - 60 + 7, 5, 18, 18);
      Graphics.FillEllipse(SolidBrush, Width - Height - Height div 2 -
        Height div 3, Height div 6, Height / 1.5, Height / 1.5);
    end
    else
    begin
      SolidBrush := TGPSolidBrush.Create(MakeColor(255,
        GetRValue(FButtonCheckedColor), GetGValue(FButtonCheckedColor),
        GetBValue(FButtonCheckedColor)));

      //graphics.FillEllipse(SolidBrush, width - 60 + 34, 3, 21, 21);
      Graphics.FillEllipse(SolidBrush, Width - Height + (Height div 6), Height div
        6, Height / 1.5, Height / 1.5);
    end;
    {pointF := MakePoint(5.0, Height / 2 - Font.Size / 2-3);
    fontFamily := TGPFontFamily.Create(Font.Name);
    gdifont := TGPFont.Create(fontFamily, Font.Size, FontStyleRegular, UnitPixel);
    graphics.DrawString(Title, -1, gdifont, pointF, solidBrush); }
    Canvas.Font.Size := Font.Size;
    Canvas.TextOut(5, Height div 2 - (Canvas.TextHeight(self.fTitle) div 2), self.fTitle);
  end;

  Graphics.Free;
  //fontFamily.Free;
  //gdifont.Free;
  solidBrush.Free;
end;

{$ENDIF}
{$ENDIF}
{$ENDREGION}

{$REGION 'BGRA'}
{$IFDEF SGJCTRL_W_BGRA}
procedure TSGJToogleButton.Paint_BGRABitmap();
var
  image: TBGRABitmap;
  CTRLColor: TColor;
begin
  image := TBGRABitmap.Create(width, height, ColorToBGRA(ColorToRGB(color)));

  if Enabled then
    CTRLColor := fButtonColor
  else
    CTRLColor := clGray;

  image.FontHeight := Font.Size;
  image.FontAntialias := true;
  image.FontStyle := Font.Style;
  if fTextBefore then
  begin
    Image.FillRect((width - height)- (height div 2) , 0,(width - height)+ (height div 2), height, ColorToBGRA(ColorToRGB(CTRLColor)));
    image.FillEllipseAntialias(width + height div 2 - (height*2), height div 2, height div 2, height div 2, ColorToBGRA(ColorToRGB(CTRLColor)));
    image.FillEllipseAntialias(width -(height div 2) , height div 2, height div 2, height div 2, ColorToBGRA(ColorToRGB(CTRLColor)));


    if Checked then
       image.FillEllipseAntialias(width - (height div 2), height div 2, height div 3 , height div 3, ColorToBGRA(ColorToRGB(FButtonCheckedColor)))
    else
        image.FillEllipseAntialias((width - height)-height div 2, height div 2, height div 3, height div 3, ColorToBGRA(ColorToRGB(FButtonUnCheckedColor)));

    image.TextOut(5, Height div 2  - (image.FontHeight div 2), fTitle, Font.Color);
  end
  else
  begin
    Image.FillRect(height div 2, 0, height+(height div 2), height, ColorToBGRA(ColorToRGB(CTRLColor)));
    image.FillEllipseAntialias(height div 2, height div 2, height div 2, height div 2, ColorToBGRA(ColorToRGB(CTRLColor)));
    image.FillEllipseAntialias(height +(height div 2) , height div 2, height div 2, height div 2, ColorToBGRA(ColorToRGB(CTRLColor)));


    if Checked then
       image.FillEllipseAntialias(height*2-(height div 2), height div 2, height div 3 , height div 3, ColorToBGRA(ColorToRGB(FButtonCheckedColor)))
    else
       image.FillEllipseAntialias(height div 2, height div 2, height div 3, height div 3, ColorToBGRA(ColorToRGB(FButtonUnCheckedColor)));


    image.TextOut(height *2 +5, Height div 2 - (image.FontHeight div 2), fTitle, Font.Color);
  end;

  image.Draw(Canvas, 0, 0, True);
  image.free;
end;
{$ENDIF}
{$ENDREGION}

procedure TSGJToogleButton.Paint();
begin
  inherited;
  {$IFDEF SGJCTRL_W_Canvas}
      Paint_Canvas();
  {$ENDIF}

  {$IFDEF SGJCTRL_W_BGRA}
      Paint_BGRABitmap();
  {$ENDIF}

  {$IFDEF MSWINDOWS}
     {$IFDEF SGJCTRL_W_GDIPLUS}
         {$IFNDEF SGJCTRL_W_D2D}
             Paint_GDIPLUS();
         {$ENDIF}
      {$ENDIF}
      {$IFDEF SGJCTRL_W_D2D}
          Paint_D2D;
       {$ENDIF}
  {$ENDIF}
end;

end.
