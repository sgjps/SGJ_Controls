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


 type
  TSGJButton = class;
  TSGJButtonSettings = class;

  TSGJButtonSettings  = class(TPersistent)
    private
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
      public
        constructor Create(AControl: TSGJButton);
     published
        //property Canvas;
        property Description: String read fDesc write fDesc;
        property ColorNormal: TColor read fNormalColor write fNormalColor;
        property ColorHover: TColor read fHoverColor write fHoverColor;
        property Title: String read FTitle write ftitle;
        property Images: TCustomImageList read fImages write fImages;
        property ImageIndex: Integer read fImageIndex write fImageIndex default -1;
        property ShowDescription: boolean read fShowDescription write fShowDescription;
        property FontDescription: TFont read fDescriptionFont write fDescriptionFont;
        property ShowBorder: boolean read fShowBorder write fShowBorder;
        property BorderColor: TColor read fBorderColor write fBorderColor;
        property RoundedCorners: Boolean read fRoundedCorners write fRoundedCorners;
        property TitleOnCenter: Boolean read fCenterTitle write fCenterTitle;
  end;
  TSGJButton = class(TCustomPanel)
    public
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
        property ButtonSettings:TSGJButtonSettings read FSettings write FSettings;
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

constructor TSGJButtonSettings.Create(AControl: TSGJButton);
begin
  inherited Create;
  fDescriptionFont := TFont.Create;
  fNormalColor:=clBtnFace;
  fHoverColor:= clSilver;
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

  fSettings:=TSGJButtonSettings.Create(self);
  color:=fSettings.fNormalColor;

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
      LCanvas.Brush.Color:=fSettings.fNormalColor;
      LCanvas.Pen.Color:=fSettings.fNormalColor;
    end
    else
    begin
      LCanvas.Brush.Color:=fSettings.fHoverColor;
      LCanvas.Pen.Color:=fSettings.fHoverColor;
    end;

   if fSettings.fRoundedCorners then
    LCanvas.roundrect(0,0,width,height,10,10)
   else
    LCanvas.Rectangle(0,0,width,height);

       if fSettings.fShowBorder then
       begin
        LCanvas.Pen.Color:=fSettings.fBorderColor;
        if not fSettings.fRoundedCorners then
          LCanvas.Rectangle(0,0,width,height)
          else
          LCanvas.roundrect(0,0,width,height,10,10)
       end;

   if (fSettings.fImages<>nil)
       then
       begin
        if (fSettings.fImageIndex <> -1) and (fSettings.fImageIndex < fSettings.fImages.Count)
          then
            begin
            //fSettings.fImages.Draw(Canvas,8,(height  div 2) - (fSettings.fImages.Height div 2), fSettings.fImageIndex,true);
            if not fSettings.fShowDescription then
            LCanvas.TextOut(fSettings.fImages.Width+16,(Height div 2) - (LCanvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
            else
              begin
                  LCanvas.TextOut(fSettings.fImages.Width+16,(Height div 2)- (LCanvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
                  LCanvas.Font.Color := fSettings.fDescriptionFont.Color;
                  LCanvas.Font.Size:=fSettings.fDescriptionFont.Size;
                  LCanvas.Font.Style:= fSettings.fDescriptionFont.Style;
                  if LCanvas.TextWidth(fSettings.fDesc)<(width - (fSettings.fImages.Width+32)) then
                    LCanvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, fSettings.fDesc)
                  else
                    LCanvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, '...')
              end;
            end;
       end
       else
       if not fSettings.fShowDescription then begin
        if not fSettings.fCenterTitle then
           LCanvas.TextOut(8,(Height div 2)- (LCanvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
        else
           LCanvas.TextOut(width div 2 - LCanvas.TextWidth(fSettings.fTitle) div 2,(Height div 2)- (LCanvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
       end
       else
       begin
        LCanvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
        LCanvas.Font.Color := fSettings.fDescriptionFont.Color;
        LCanvas.Font.Size:=fSettings.fDescriptionFont.Size;
        LCanvas.Font.Style:= fSettings.fDescriptionFont.Style;
        LCanvas.TextOut(8,(Height div 2) +1, fSettings.fDesc)
       end;

     finally
     LCanvas.EndDraw;
     LCanvas.Free;
     if (fSettings.fImages<>nil) then
        if (fSettings.fImageIndex <> -1) and (fSettings.fImageIndex < fSettings.fImages.Count) then
            fSettings.fImages.Draw(Canvas,8,(height  div 2) - (fSettings.fImages.Height div 2), fSettings.fImageIndex,true);
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
    Canvas.Brush.Color:=fSettings.fNormalColor;
    Canvas.Pen.Color:=fSettings.fNormalColor;
   end
   else
   begin
    Canvas.Brush.Color:=fSettings.fHoverColor;
    Canvas.Pen.Color:=fSettings.fHoverColor;
   end;

   if fSettings.fRoundedCorners then
    Canvas.RoundRect(0,0,width,height,10,10)
   else
    Canvas.Rectangle(0,0,width,height);

       if fSettings.fShowBorder then
       begin
        Canvas.Pen.Color:=fSettings.fBorderColor;
        if not fSettings.fRoundedCorners then
          Canvas.Rectangle(0,0,width,height)
        else
        canvas.roundrect(0,0,width,height,10,10)
       end;

   if (fSettings.fImages<>nil)
       then
       begin
        if (fSettings.fImageIndex <> -1) and (fSettings.fImageIndex < fSettings.fImages.Count)
          then
            begin
            fSettings.fImages.Draw(Canvas,8,(height  div 2) -(fSettings.fImages.Height div 2) ,fSettings.fImageIndex,true);
            if not fSettings.fShowDescription then
            Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
            else
              begin
                 Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
                  Canvas.Font.Color := fSettings.fDescriptionFont.Color;
                  Canvas.Font.Size:=fSettings.fDescriptionFont.Size;
                  Canvas.Font.Style:= fSettings.fDescriptionFont.Style;
                 if Canvas.TextWidth(fSettings.fDesc)<(width - (fSettings.fImages.Width+32)) then
                 Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, fSettings.fDesc)
                 else
                 Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, '...')
              end;
            end;
       end
       else
       if not fSettings.fShowDescription then begin
        if not fSettings.fCenterTitle then
           Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
        else
           Canvas.TextOut(width div 2 - Canvas.TextWidth(fSettings.fTitle) div 2,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
       end
       else
       begin
       Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
       Canvas.Font.Color := fSettings.fDescriptionFont.Color;
       Canvas.Font.Size:=fSettings.fDescriptionFont.Size;
       Canvas.Font.Style:= fSettings.fDescriptionFont.Style;
       Canvas.TextOut(8,(Height div 2) +1, fSettings.fDesc)
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
    AColor:=fSettings.fNormalColor;
    Canvas.Brush.Color:=fSettings.fNormalColor;
    end
    else
    begin
     AColor:=fSettings.fHoverColor;
     Canvas.Brush.Color:=fSettings.fHoverColor;
    end;

    image := TBGRABitmap.Create(width, height, ColorToBGRA(ColorToRGB(self.Color)));
    if fSettings.fRoundedCorners then
    image.FillRoundRectAntialias(0,0,width,height,10,10,ColorToBGRA(ColorToRGB(AColor)))
    else
     image.FillRect(0,0,width,height,ColorToBGRA(ColorToRGB(AColor)));

        if fSettings.fShowBorder then
        begin
         if not fSettings.fRoundedCorners then
         image.RectangleAntialias(0,0,width-1,height-1,ColorToBGRA(ColorToRGB(fSettings.fBorderColor)),1)
         else
         image.RoundRectAntialias(0,0,width-1,height-1,10,10,ColorToBGRA(ColorToRGB(fSettings.fBorderColor)),1);
        end;

    image.Draw(Canvas, 0, 0, True);
    image.free;

    if (fSettings.fImages<>nil)
        then
        begin
         if (fSettings.fImageIndex <> -1) and (fSettings.fImageIndex < fSettings.fImages.Count)
           then
             begin
             fSettings.fImages.Draw(Canvas,8,(height  div 2) -(fSettings.fImages.Height div 2) ,fSettings.fImageIndex,true);
             if not fSettings.fShowDescription then
             Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
             else
               begin
                  Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
                   Canvas.Font.Color := fSettings.fDescriptionFont.Color;
                   Canvas.Font.Size:=fSettings.fDescriptionFont.Size;
                   Canvas.Font.Style:= fSettings.fDescriptionFont.Style;
                  if Canvas.TextWidth(fSettings.fDesc)<(width - (fSettings.fImages.Width+32)) then
                  Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, fSettings.fDesc)
                  else
                  Canvas.TextOut(fSettings.fImages.Width+16,(Height div 2) +1, '...')
               end;
             end;
        end
        else
        if not fSettings.fShowDescription then begin
         if not fSettings.fCenterTitle then
            Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
         else
            Canvas.TextOut(width div 2 - Canvas.TextWidth(fSettings.fTitle) div 2,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle) div 2), fSettings.fTitle)
        end
        else
        begin
        Canvas.TextOut(8,(Height div 2)- (Canvas.TextHeight(fSettings.fTitle))-1, fSettings.fTitle) ;
        Canvas.Font.Color := fSettings.fDescriptionFont.Color;
        Canvas.Font.Size:=fSettings.fDescriptionFont.Size;
        Canvas.Font.Style:= fSettings.fDescriptionFont.Style;
        Canvas.TextOut(8,(Height div 2) +1, fSettings.fDesc)
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
  FreeAndNil(fSettings.fDescriptionFont);
  inherited Destroy;
end;



end.

