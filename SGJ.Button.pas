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
{********************************************************************}
unit SGJ.Button;

{$IfDef FPC}
{$mode ObjFPC}{$H+}
{$EndIf}

interface

uses
{$IFDEF DCC}
{$IF CompilerVersion >= 21} //Delphi 2010
  D2D1, Vcl.Direct2D, 
{$IFEND}
{$ENDIF}
{$IFDEF FPC}
  bgrabitmap, BGRABitmapTypes,
{$ENDIF}
  Classes, SysUtils, Controls, ExtCtrls, Graphics, Forms, Messages, Types, ImgList;


  TSGJButton = class(TCustomPanel)
    public
	    fNormalColor: TColor;
        fHoverColor:TColor;
        fTitle : string;
        fImages: TCustomImageList;
        fImageIndex: Integer;
        fDesc: string;
        fShowDescription: boolean;
        fDescriptionFont: TFont;
        fShowBorder: boolean;
        fRoundedCorners: boolean;
        fBorderColor: TColor;
        fCenterTitle: boolean;
        procedure SetTitle(ATitle: String);
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
      procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
      procedure Paint; override;
    private
        FSettings: TSGJButtonSettings;
        procedure PaintButton(AMouseMove: Boolean);
        {$IFDEF FPC}
        procedure PaintButtonBGRA(AMouseMove: Boolean);
        {$ENDIF}
    published
        property Description: String read fDesc write fDesc;
        property ColorNormal: TColor read fNormalColor write fNormalColor;
        property ColorHover: TColor read fHoverColor write fHoverColor;
        property Title: String read FTitle write SetTitle;
        property Images: TCustomImageList read fImages write fImages;
        property ImageIndex: Integer read fImageIndex write fImageIndex default -1;
        property ShowDescription: boolean read fShowDescription write fShowDescription;
        property FontDescription: TFont read fDescriptionFont write fDescriptionFont;
        property ShowBorder: boolean read fShowBorder write fShowBorder;
        property BorderColor: TColor read fBorderColor write fBorderColor;
        property RoundedCorners: Boolean read fRoundedCorners write fRoundedCorners;
        property TitleOnCenter: Boolean read fCenterTitle write fCenterTitle;
        property Font;
        property OnClick;
        property Color;
		property Align;
end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('SGJ', [TSGJButton]);
end;

procedure TSGJButton.SetTitle(ATitle: String);
begin
   fTitle:=ATitle;
   Paint;
end;


constructor TSGJButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
  {$EndIf}

  BorderStyle:=bsNone;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  Height:=32;
  ParentBackground:=false;

  color:=fNormalColor;

end;

procedure TSGJButton.PaintButton(AMouseMove: Boolean);
{$IFDEF DCC}
{$IF CompilerVersion >= 21}
 var
   LCanvas: TDirect2DCanvas;
{$IFEND}
{$ENDIF}
begin
{$IFDEF DCC}     //Direct2D Draw
{$IF CompilerVersion >= 21}
  if TDirect2DCanvas.Supported then
  begin
   LCanvas := TDirect2DCanvas.Create(Canvas, ClientRect);
   LCanvas.BeginDraw;

   try
    LCanvas.Font.Color := Font.Color;
    LCanvas.Font.Size:=Font.Size;
    LCanvas.Font.Style:=Font.Style;

    if not AMouseMove then begin
      LCanvas.Brush.Color:=fNormalColor;
      LCanvas.Pen.Color:=fNormalColor;
    end
    else
    begin
      LCanvas.Brush.Color:=fHoverColor;
      LCanvas.Pen.Color:=fHoverColor;
    end;

   if fRoundedCorners then
    LCanvas.roundrect(0,0,width,height,10,10)
   else
    LCanvas.Rectangle(0,0,width,height);

       if fShowBorder then
       begin
        LCanvas.Pen.Color:=fBorderColor;
        if not fRoundedCorners then
          LCanvas.Rectangle(0,0,width,height)
          else
          LCanvas.roundrect(0,0,width,height,10,10)
       end;

   if (fImages<>nil)
       then
       begin
        if (fImageIndex <> -1) and (fImageIndex < fImages.Count)
          then
            begin
            //fImages.Draw(Canvas,8,(height  div 2) - (fImages.Height div 2), fImageIndex,true);
            if not fShowDescription then
            LCanvas.TextOut(fImages.Width+16,(Height div 2) - (LCanvas.TextHeight(fTitle) div 2), fTitle)
            else
              begin
                  LCanvas.TextOut(fImages.Width+16,(Height div 2)- (LCanvas.TextHeight(fTitle))-1, fTitle) ;
                  LCanvas.Font.Color := fDescriptionFont.Color;
                  LCanvas.Font.Size:=fDescriptionFont.Size;
                  LCanvas.Font.Style:= fDescriptionFont.Style;
                  if LCanvas.TextWidth(fDesc)<(width - (fImages.Width+32)) then
                    LCanvas.TextOut(fImages.Width+16,(Height div 2) +1, fDesc)
                  else
                    LCanvas.TextOut(fImages.Width+16,(Height div 2) +1, '...')
              end;
            end;
       end
       else
       if not fShowDescription then begin
        if not fCenterTitle then
           LCanvas.TextOut(8,(Height div 2)- (LCanvas.TextHeight(fTitle) div 2), fTitle)
        else
           LCanvas.TextOut(width div 2 - LCanvas.TextWidth(fTitle) div 2,(Height div 2)- (LCanvas.TextHeight(fTitle) div 2), fTitle)
       end
       else
       begin
        LCanvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fTitle))-1, fTitle) ;
        LCanvas.Font.Color := fDescriptionFont.Color;
        LCanvas.Font.Size:=fDescriptionFont.Size;
        LCanvas.Font.Style:= fDescriptionFont.Style;
        LCanvas.TextOut(8,(Height div 2) +1, fDesc)
       end;

     finally
     LCanvas.EndDraw;
     LCanvas.Free;
     if (fImages<>nil) then
        if (fImageIndex <> -1) and (fImageIndex < fImages.Count) then
            fImages.Draw(Canvas,8,(height  div 2) - (fImages.Height div 2), fImageIndex,true);
   end;
  end
  else
{$IFEND}
{$ENDIF} //Canvas Draw
   begin
   Canvas.Font.Color := Font.Color;
   Canvas.Font.Size:=Font.Size;
   Canvas.Font.Style:=Font.Style;

   if not AMouseMove then begin
    Canvas.Brush.Color:=fNormalColor;
    Canvas.Pen.Color:=fNormalColor;
   end
   else
   begin
    Canvas.Brush.Color:=fHoverColor;
    Canvas.Pen.Color:=fHoverColor;
   end;

   if fRoundedCorners then
    Canvas.RoundRect(0,0,width,height,10,10)
   else
    Canvas.Rectangle(0,0,width,height);

       if fShowBorder then
       begin
        Canvas.Pen.Color:=fBorderColor;
        if not fRoundedCorners then
          Canvas.Rectangle(0,0,width,height)
        else
        canvas.roundrect(0,0,width,height,10,10)
       end;

   if (fImages<>nil)
       then
       begin
        if (fImageIndex <> -1) and (fImageIndex < fImages.Count)
          then
            begin
            fImages.Draw(Canvas,8,(height  div 2) -(fImages.Height div 2) ,fImageIndex,true);
            if not fShowDescription then
            Canvas.TextOut(fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
            else
              begin
                 Canvas.TextOut(fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fTitle))-1, fTitle) ;
                  Canvas.Font.Color := fDescriptionFont.Color;
                  Canvas.Font.Size:=fDescriptionFont.Size;
                  Canvas.Font.Style:= fDescriptionFont.Style;
                 if Canvas.TextWidth(fDesc)<(width - (fImages.Width+32)) then
                 Canvas.TextOut(fImages.Width+16,(Height div 2) +1, fDesc)
                 else
                 Canvas.TextOut(fImages.Width+16,(Height div 2) +1, '...')
              end;
            end;
       end
       else
       if not fShowDescription then begin
        if not fCenterTitle then
           Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
        else
           Canvas.TextOut(width div 2 - Canvas.TextWidth(fTitle) div 2,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
       end
       else
       begin
       Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fTitle))-1, fTitle) ;
       Canvas.Font.Color := fDescriptionFont.Color;
       Canvas.Font.Size:=fDescriptionFont.Size;
       Canvas.Font.Style:= fDescriptionFont.Style;
       Canvas.TextOut(8,(Height div 2) +1, fDesc)
       end;
   end;
end;

{$IFDEF FPC}
procedure TSGJButton.PaintButtonBGRA(AMouseMove: Boolean);
 var
   image: TBGRABitmap;
   AColor:TColor;
begin
  Canvas.Font.Color := Font.Color;
    Canvas.Font.Size:=Font.Size;
    Canvas.Font.Style:=Font.Style;

    if not AMouseMove then begin
    AColor:=fNormalColor;
    Canvas.Brush.Color:=fNormalColor;
    end
    else
    begin
     AColor:=fHoverColor;
     Canvas.Brush.Color:=fHoverColor;
    end;

    image := TBGRABitmap.Create(width, height, ColorToBGRA(ColorToRGB(self.Color)));
    if fRoundedCorners then
    image.FillRoundRectAntialias(0,0,width,height,10,10,ColorToBGRA(ColorToRGB(AColor)))
    else
     image.FillRect(0,0,width,height,ColorToBGRA(ColorToRGB(AColor)));

        if fShowBorder then
        begin
         if not fRoundedCorners then
         image.RectangleAntialias(0,0,width-1,height-1,ColorToBGRA(ColorToRGB(fBorderColor)),1)
         else
         image.RoundRectAntialias(0,0,width-1,height-1,10,10,ColorToBGRA(ColorToRGB(fBorderColor)),1);
        end;

    image.Draw(Canvas, 0, 0, True);
    image.free;

    if (fImages<>nil)
        then
        begin
         if (fImageIndex <> -1) and (fImageIndex < fImages.Count)
           then
             begin
             fImages.Draw(Canvas,8,(height  div 2) -(fImages.Height div 2) ,fImageIndex,true);
             if not fShowDescription then
             Canvas.TextOut(fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
             else
               begin
                  Canvas.TextOut(fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fTitle))-1, fTitle) ;
                   Canvas.Font.Color := fDescriptionFont.Color;
                   Canvas.Font.Size:=fDescriptionFont.Size;
                   Canvas.Font.Style:= fDescriptionFont.Style;
                  if Canvas.TextWidth(fDesc)<(width - (fImages.Width+32)) then
                  Canvas.TextOut(fImages.Width+16,(Height div 2) +1, fDesc)
                  else
                  Canvas.TextOut(fImages.Width+16,(Height div 2) +1, '...')
               end;
             end;
        end
        else
        if not fShowDescription then begin
         if not fCenterTitle then
            Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
         else
            Canvas.TextOut(width div 2 - Canvas.TextWidth(fTitle) div 2,(Height div 2)- (Canvas.TextHeight(fTitle) div 2), fTitle)
        end
        else
        begin
        Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fTitle))-1, fTitle) ;
        Canvas.Font.Color := fDescriptionFont.Color;
        Canvas.Font.Size:=fDescriptionFont.Size;
        Canvas.Font.Style:= fDescriptionFont.Style;
        Canvas.TextOut(8,(Height div 2) +1, fDesc)
        end;
end;
{$ENDIF}
procedure TSGJButton.MouseMove(Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  {$IFDEF FPC}
   PaintButtonBGRA(true);
  {$ELSE}
  PaintButton(true);
  {$ENDIF}
  Cursor:=crHandPoint;
end;

procedure TSGJButton.MouseLeave(var Msg: TMessage);
begin
  inherited;
  {$IFDEF FPC}
   PaintButtonBGRA(false);
  {$ELSE}
  PaintButton(false);
  {$ENDIF}
  Cursor:=crDefault;
end;

procedure TSGJButton.Paint();
begin
  inherited;
  {$IFDEF FPC}
   PaintButtonBGRA(false);
  {$ELSE}
  PaintButton(false);
  {$ENDIF}
end;

destructor TSGJButton.Destroy;
begin
  FreeAndNil(fDescriptionFont);
  inherited Destroy;
end;



end.

