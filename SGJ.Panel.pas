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
  LResources,
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
  protected
  public
          constructor Create(AOwner: TComponent); override;
          procedure AdjustClientRect(var aRect: TRect); override;
          procedure Paint; override;
  published
          property Title: String read FTitle write ftitle;
          property BorderColor: TColor read fBorderColor write fBorderColor;
		      property ShowBorder: boolean read fShowBorder write fShowBorder;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ',[TSGJPanel]);
end;


constructor TSGJPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
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
      Canvas.TextOut(15,0, Self.Title);
      Brush.Style := bsSolid;
	  
	  if self.ShowBorder then
	  begin
	  if Self.Title<>'' then
	  begin
      //left
      Canvas.MoveTo(1,(TextHeight(Self.Title)div 2)+1);
      Canvas.LineTo(1,self.height-1);

      //Top Left
      Canvas.MoveTo(2, (TextHeight(Self.Title)div 2));
      Canvas.LineTo(12,(TextHeight(Self.Title)div 2));

      //Top Right
      Canvas.MoveTo(TextWidth(Self.Title)+18,(TextHeight(Self.Title)div 2));//+1);
      Canvas.LineTo(self.width-3,(TextHeight(Self.Title)div 2));//+1);
	  
      //right
      Canvas.MoveTo(self.width-3,(TextHeight(Self.Title)div 2)+1);
      Canvas.LineTo(Self.width-3,self.height-1);

      //bottom
      Canvas.MoveTo(2, self.height-1);
      Canvas.LineTo(Self.width-3,self.height-1);
	  end
	  else begin
    Canvas.Brush.Color:=self.Color;
    Canvas.RoundRect(1,1,width-2,height-2,4,7) ;
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
