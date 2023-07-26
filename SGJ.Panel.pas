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
unit SGJ.Panel;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
{$IFDEF FPC}
  LResources, LCLType, 
{$ELSE}
  Windows,
{$ENDIF}
  Forms, Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
  TSGJPanel = class(TPanel)
  private
          fTitle : string;
          fBorderColor: TColor;
		      fShowBorder: boolean;
          procedure SetCaption(ACaption: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
          procedure SetBorder(ABorder: boolean);
          procedure SetBorderColor(AColor: TColor);
  protected
  public
          constructor Create(AOwner: TComponent); override;
          procedure AdjustClientRect(var aRect: TRect); override;
          procedure Paint; override;
  published
          property Caption: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF} read FTitle write SetCaption;
          property BorderColor: TColor read fBorderColor write SetBorderColor;
		      property ShowBorder: boolean read fShowBorder write SetBorder;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ',[TSGJPanel]);
end;

procedure TSGJPanel.SetBorderColor(AColor: TColor);
begin
  if fBorderColor<>AColor then
  begin
     fBorderColor:=AColor;
     Paint();
  end;
end;

procedure TSGJPanel.SetCaption(ACaption: {$IFDEF FPC}TTranslateString{$ELSE}string{$ENDIF});
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
   inherited;
   with Canvas do begin
      Brush.Style := bsClear;
      //Canvas.Brush.Color:=$00B16300;
      Pen.Color:=self.BorderColor;
      Canvas.Font.Color := self.Font.Color;
      Canvas.font.Style:=self.Font.Style;
      Canvas.Font.Size:=self.Font.Size;
      Canvas.TextOut(15,0, Self.fTitle);
      Brush.Style := bsSolid;
	  
	  if self.ShowBorder then
		begin
			if Self.fTitle<>'' then
			begin
      //left
      Canvas.MoveTo(1,(TextHeight(Self.fTitle)div 2)+1);
      Canvas.LineTo(1,self.height-1);

      //Top Left
      Canvas.MoveTo(2, (TextHeight(Self.fTitle)div 2));
      Canvas.LineTo(12,(TextHeight(Self.fTitle)div 2));

      //Top Right
      Canvas.MoveTo(TextWidth(Self.fTitle)+18,(TextHeight(Self.fTitle)div 2));//+1);
      Canvas.LineTo(self.width-3,(TextHeight(Self.fTitle)div 2));//+1);
	  
      //right
      Canvas.MoveTo(self.width-3,(TextHeight(Self.fTitle)div 2)+1);
      Canvas.LineTo(Self.width-3,self.height-1);

      //bottom
      Canvas.MoveTo(2, self.height-1);
      Canvas.LineTo(Self.width-3,self.height-1);
	  end
	  else begin
		Canvas.Brush.Color:=self.Color;
		Canvas.RoundRect(2,2,Self.width-2,Self.height-2,4,4) ;
    end;
	  end;
   end;
end;

procedure TSGJPanel.AdjustClientRect(var aRect: TRect);
begin
    inherited AdjustClientRect(ARect);
    ARect:= Rect(
            ARect.left+5,
            ARect.Top+20,
            ARect.Right-5,
            ARect.Bottom-5
            );
end;

end.
