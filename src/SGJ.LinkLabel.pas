{********************************************************************}
{ home page : https://www.sgjps.com                                  }
{ email     : sgj@sgjps.com                                          }
{                                                                    }
{ date      : 2025/08/27                                             }
{                                                                    }
{ version   : 2.0                                                   }
{                                                                    }
{ This file is part of SGJ Controls for Delphi and Lazarus           }
{                                                                    }
{********************************************************************}
unit SGJ.LinkLabel;

{$IFDEF FPC}
{$mode ObjFPC}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF FPC}
   lclintf, LResources,
  {$ELSE}
   Windows, ShellApi,
  {$ENDIF}
   Graphics, StdCtrls, Classes, Messages, Forms, Controls;

type
  TSGJLinkLabel = class(TLabel)
  private
      fURL : string;
      fHoverColor:TColor;
      fNormalColor: TColor;
  protected

  public
      constructor Create(AOwner: TComponent); override;
      procedure MouseMove(Shift:TShiftState; X,Y:Integer); override;
      procedure MouseLeave(var Msg: TMessage); message CM_MouseLeave;
      procedure Click; override;
  published
          property URL: String read fURL write fURL;
          property ColorHover: TColor read fHoverColor write fHoverColor;
          property ColorNormal: TColor read fNormalColor write fNormalColor;
          property OnClick;
          property Anchors;
  end;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ',[TSGJLinkLabel]);
end;


  constructor TSGJLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
  {$EndIf}
end;

procedure TSGJLinkLabel.MouseMove(Shift:TShiftState; X,Y:Integer);
begin
  inherited;
  Self.Font.Color:=ColorHover;
  self.Cursor:=crHandPoint;
end;

procedure TSGJLinkLabel.MouseLeave(var Msg: TMessage);
begin
  inherited;
  Self.Font.Color:=ColorNormal;
  self.Cursor:=crDefault;
end;

procedure TSGJLinkLabel.Click;
begin
  inherited Click;
  {$IFDEF FPC}
    OpenURL(self.URL);
  {$ELSE}
    ShellExecute(0, nil, PChar(self.URL), nil, nil,SW_SHOWNORMAL);
  {$ENDIF}

end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.LinkLabel.lrs}
{$ENDIF}
end.
