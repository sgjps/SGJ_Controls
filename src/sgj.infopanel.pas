unit SGJ.InfoPanel;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC}
  LResources,LCLType, LCLIntf,  Dialogs, DialogRes,
  {$ELSE}
  Windows,
  {$ENDIF}
  Forms, Classes, SysUtils, Controls, Graphics, ExtCtrls;

type
  {$IFnDef FPC}
  TTranslateString = string;
  {$ENDIF}
  TSGJIPMsgLength = (mlLong, mlShort);
  TSGJIPSeverity = (ivCustom, ivConfirmation, ivWarning, ivError,
    ivInformation, ivShield);
  TSGJIPActionStyle = (asButton, asHyperlink);

  TSGJInfoPanel = class(TCustomPanel)
  private
    fBorderColor: TColor;
    fInnerColor: TColor;
    fTitle: TTranslateString;
    fShortMsg: TTranslateString;
    fLongMsg: TTranslateString;
    fMessageLength: TSGJIPMsgLength;
    fSeverity: TSGJIPSeverity;
    fCloseRect: TRect;        // Prostokąt przeznaczony na przycisk X
    fCloseHovered: boolean;   // Czy myszka znajduje się nad przyciskiem X
    fClosePressed: boolean;
    fOnCloseClick: TNotifyEvent; // Zdarzenie kliknięcia w X
    fActionText: TTranslateString;
    fActionStyle: TSGJIPActionStyle;
    fShowAction: boolean;
    // Obszar roboczy i stany myszy dla akcji
    fActionRect: TRect;
    fActionHovered: boolean;
    fActionPressed: boolean;

    fOnActionClick: TNotifyEvent;
    procedure SetBorderColor(AColor: TColor);
    procedure SetInnerColor(AColor: TColor);
    procedure SetTitle(ATitle: TTranslateString);
    procedure SetShortMsg(AMsg: TTranslateString);
    procedure SetLongMsg(AMsg: TTranslateString);
    procedure SetMessageLength(AValue: TSGJIPMsgLength);
    procedure SetSeverity(AValue: TSGJIPSeverity);
    procedure SetActionText(const AValue: TTranslateString);
    procedure SetActionStyle(AValue: TSGJIPActionStyle);
    procedure SetShowAction(AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithStandardChildren: boolean); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  published
    property BorderColor: TColor
      read fBorderColor write SetBorderColor default $00B16300;
    property InnerColor: TColor
      read fInnerColor write SetInnerColor default clNone;
    property Title: TTranslateString read FTitle write SetTitle;
    property ShortMsg: TTranslateString read fShortMsg write SetShortMsg;
    property LongMsg: TTranslateString read fLongMsg write SetLongMsg;
    property MessageLength: TSGJIPMsgLength
      read fMessageLength write SetMessageLength default mlShort;
    property Severity: TSGJIPSeverity
      read fSeverity write SetSeverity default ivInformation;
    property ActionText: TTranslateString read fActionText write SetActionText;
    property ActionStyle: TSGJIPActionStyle
      read fActionStyle write SetActionStyle default asHyperlink;
    property OnActionClick: TNotifyEvent read fOnActionClick write fOnActionClick;
    property ShowAction: boolean read fShowAction write SetShowAction default False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    // property BevelColor;
    // property BevelInner;
    // property BevelOuter;
    // property BevelWidth;
    property BidiMode;
    // property BorderWidth;
    // property BorderStyle;
    property Caption;
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

    property ChildSizing;
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    property OnPaint;
    property OnCloseClick: TNotifyEvent read fOnCloseClick write fOnCloseClick;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJInfoPanel]);
end;

procedure TSGJInfoPanel.SetTitle(ATitle: TTranslateString);
begin
  if fTitle <> ATitle then
  begin
    fTitle := ATitle;
    Invalidate();
  end;
end;

procedure TSGJInfoPanel.SetShortMsg(AMsg: TTranslateString);
begin
  if fShortMsg <> AMsg then
  begin
    fShortMsg := AMsg;
    Invalidate();
  end;
end;

procedure TSGJInfoPanel.SetLongMsg(Amsg: TTranslateString);
begin
  if fLongMsg <> AMsg then
  begin
    fLongMsg := AMsg;
    Invalidate();
  end;
end;

procedure TSGJInfoPanel.SetBorderColor(AColor: TColor);
begin
  if fBorderColor <> AColor then
  begin
    fBorderColor := AColor;
    Invalidate();
  end;
end;

procedure TSGJInfoPanel.SetInnerColor(AColor: TColor);
begin
  if fInnerColor <> AColor then
  begin
    fInnerColor := AColor;
    Invalidate();
  end;
end;

constructor TSGJInfoPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle;

  {$IfDef FPC}
  // Set default width and height
  with GetControlClassDefaultSize do
      SetInitialBounds(0, 0, CX, CY);
  {$EndIf}

  BorderStyle := bsNone;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  Height := 28;
  Width := 60;
  ParentBackground := True;
  BorderColor := $00B16300;
  InnerColor := clNone;
  fMessageLength := mlShort;
  fSeverity := ivInformation;
  fCloseHovered := False;
  fCloseRect := Rect(0, 0, 0, 0);
  fActionStyle := asHyperlink;
  fActionHovered := False;
  fActionPressed := False;
  fActionRect := Rect(0, 0, 0, 0);
  fShowAction := False;
end;

procedure TSGJInfoPanel.SetMessageLength(AValue: TSGJIPMsgLength);
begin
  if fMessageLength <> AValue then
  begin
    fMessageLength := AValue;
    Invalidate(); // Wymusza ponowne przerysowanie komponentu po zmianie
  end;
end;

procedure TSGJInfoPanel.SetSeverity(AValue: TSGJIPSeverity);
begin
  if fSeverity <> AValue then
  begin
    fSeverity := AValue;
    Invalidate(); // Wymusza odświeżenie graficzne komponentu po zmianie ikony/stylu
  end;
end;
{
procedure TSGJInfoPanel.Paint();
var
  IconIndex: Integer;
begin
   inherited Paint;
   if HandleAllocated then
    with Canvas do begin
      Brush.Style := bsClear;
      Pen.Color:=self.BorderColor;
      Pen.Width:=1;
      Canvas.Font.Color := self.Font.Color;
      Canvas.font.Style:=self.Font.Style;
      Canvas.Font.Size:=self.Font.Size;
      Brush.Style := bsSolid;
      Canvas.Brush.Color:=self.Color;

      if InnerColor<>clNone  then
      Canvas.Brush.Color:=self.InnerColor;
      Canvas.RoundRect(2,2,Self.width-2,Self.height-2,8,8);

      {
      IndexConfirm := DialogGlyphs.DialogIcon[idDialogConfirm]; // mtConfirmation
IndexWarning := DialogGlyphs.DialogIcon[idDialogWarning]; // mtWarning
IndexError   := DialogGlyphs.DialogIcon[idDialogError];   // mtError
IndexInfo    := DialogGlyphs.DialogIcon[idDialogInfo];    // mtInformation
IndexShield  := DialogGlyphs.DialogIcon[idDialogShield];  /
}
      IconIndex := DialogGlyphs.DialogIcon[idDialogShield];
      DialogGlyphs.Draw(Canvas, 20, 20, IconIndex);


     end;
end;
}
{
procedure TSGJInfoPanel.Paint();
var
  IconIndex: Integer;
  PadX, PadY, IconSize, IconSpace: Integer;
  R: TRect;
  TxtStyle: TTextStyle;

  // Zmienne pomocnicze na wyliczone kolory Fluent
  FinalBorder, FinalInner: TColor;
  DrawIcon: Boolean;
  IsDarkMode: Boolean;
  CloseSize: Integer;
  W: Integer; // Zmienna do wyliczania symetrii linii krzyżyka
begin
  inherited Paint;
  if not HandleAllocated then Exit;

  // 1. AUTOMATYCZNE AKCENTY KOLORYSTYCZNE WINUI 3 LUB TRYB CUSTOM

  IsDarkMode := ColorToRGB(Self.Color) < $7F7F7F;
  DrawIcon := True;

  if not IsDarkMode then
    begin
      // ==========================================
      // PALETA DLA TRYBU JASNEGO (LIGHT MODE)
      // ==========================================
      case fSeverity of
        ivInformation: begin FinalBorder := $00F38B00; FinalInner := $00F9F3EB; IconIndex := DialogGlyphs.DialogIcon[idDialogInfo]; end;
        ivConfirmation: begin FinalBorder := $00198610; FinalInner := $00F3FBF2; IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm]; end;
        ivWarning:     begin FinalBorder := $000080B8; FinalInner := $00EFF9FF; IconIndex := DialogGlyphs.DialogIcon[idDialogWarning]; end;
        ivError:       begin FinalBorder := $00243DE4; FinalInner := $00F2F3FF; IconIndex := DialogGlyphs.DialogIcon[idDialogError]; end;
        ivShield:      begin FinalBorder := $008A8A8A; FinalInner := $00F3F3F3; IconIndex := DialogGlyphs.DialogIcon[idDialogShield]; end;
        ivCustom:      begin FinalBorder := $008A8A8A; FinalInner := $00F9F9F9; DrawIcon := False; IconIndex := 0; end;
      end;
    end
    else
    begin
      // ==========================================
      // PALETA DLA TRYBU CIEMNEGO (DARK MODE)
      // ==========================================
      case fSeverity of
        ivInformation: begin FinalBorder := $00FF9D3B; FinalInner := $002C221A; IconIndex := DialogGlyphs.DialogIcon[idDialogInfo]; end;
        ivConfirmation: begin FinalBorder := $0030D143; FinalInner := $001A2B1E; IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm]; end;
        ivWarning:     begin FinalBorder := $003BD2FF; FinalInner := $001A2733; IconIndex := DialogGlyphs.DialogIcon[idDialogWarning]; end;
        ivError:       begin FinalBorder := $004756FF; FinalInner := $00201A2D; IconIndex := DialogGlyphs.DialogIcon[idDialogError]; end;
        ivShield:      begin FinalBorder := $009E9E9E; FinalInner := $002D2D2D; IconIndex := DialogGlyphs.DialogIcon[idDialogShield]; end;
        ivCustom:      begin FinalBorder := $00606060; FinalInner := $00202020; DrawIcon := False; IconIndex := 0; end;
      end;
    end;

    if (Self.BorderColor <> clNone) then FinalBorder := Self.BorderColor;
    if (Self.InnerColor <> clNone) then FinalInner := Self.InnerColor;

    with Canvas do begin
    // 2. Definiowanie marginesów i odstępów bazujących na DPI (względem bazy 96)
    PadX := ScaleX(16, 96);
    PadY := ScaleY(12, 96);
    IconSpace := ScaleX(12, 96);

    // 3. RYSOWANIE TŁA I RAMKI
    if IsDarkMode then Pen.Color := $00404040 else Pen.Color := $00E6E6E6;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := FinalInner;
    Brush.Style := bsSolid;

    RoundRect(2, 2, Self.Width - 2, Self.Height - 2, 8, 8);

    // 4. RYSOWANIE SKALOWANEJ IKONY
    IconSize := 0;
    if DrawIcon then begin
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
        IconSize := ScaleX(16, 96)
      else
        IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96];

      if Assigned(DialogGlyphs) and (not (csDesigning in ComponentState)) then
        DialogGlyphs.Draw(Canvas, PadX, PadY, IconIndex)
      else begin
        Pen.Color := FinalBorder;
        Brush.Style := bsClear;
        Rectangle(PadX, PadY, PadX + IconSize, PadY + IconSize);
      end;
    end;

    // 4a. RYSOWANIE PRZYCISKU ZAMKNIĘCIA "X" (Jako pełnoprawny przycisk WinUI 3)
    CloseSize := ScaleX(24, 96);
    fCloseRect := Rect(Self.Width - PadX - CloseSize, PadY - ScaleY(4, 96), Self.Width - PadX, PadY - ScaleY(4, 96) + CloseSize);

    if fClosePressed then
    begin
      if IsDarkMode then Brush.Color := $00454545 else Brush.Color := $00E1E1E1;
      Brush.Style := bsSolid; Pen.Style := psClear;
      Canvas.RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end
    else if fCloseHovered then
    begin
      if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA;
      Brush.Style := bsSolid; Pen.Style := psClear;
      Canvas.RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end;

    // --- POPRAWIONE RYSOWANIE INTEGRALNEGO SYMBOLU X ZA POMOCĄ LINII ---
    if IsDarkMode then Pen.Color := clWhite else Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsClear;

    W := ScaleX(8, 96);
    IconSize := fCloseRect.Left + (CloseSize - W) div 2;
    IconSpace := fCloseRect.Top + (CloseSize - W) div 2;

    // Linia 1: \
    MoveTo(IconSize, IconSpace);
    LineTo(IconSize + W + 1, IconSpace + W + 1);

    // Linia 2: /
    MoveTo(IconSize + W, IconSpace);
    LineTo(IconSize - 1, IconSpace + W + 1);

    // 4b. Styl czcionki i AUTOMATYCZNY DOBÓR KOLORU TEKSTU dla WinUI 3
    Brush.Style := bsClear;
    Font.Size := Self.Font.Size;

    if (Self.Font.Color = clDefault) or (Self.Font.Color = clWindowText) then
    begin
      if IsDarkMode then Font.Color := clWhite else Font.Color := clBlack;
    end
    else
      Font.Color := Self.Font.Color;

    TxtStyle := TextStyle;
    TxtStyle.SingleLine := False;
    TxtStyle.Wordbreak := True;
    TxtStyle.Clipping := True;

    // 6. DYNAMICZNE WYLICZENIE OBSZARU DLA TEKSTU
    if DrawIcon then
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then IconSize := ScaleX(16, 96)
      else IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96]
    else IconSize := 0;

    if DrawIcon then R.Left := PadX + IconSize + IconSpace else R.Left := PadX;

    R.Top := PadY;
    R.Right := fCloseRect.Left - ScaleX(12, 96);
    R.Bottom := Self.Height - PadY;

    // 7. RYSOWANIE TEKSTÓW W ZALEŻNOŚCI OD TRYBU MessageLength
    case fMessageLength of

      mlShort:
      begin
        if (fTitle <> '') or (fShortMsg <> '') then begin
          if (fTitle <> '') and (fShortMsg <> '') then begin
            Font.Style := Self.Font.Style + [fsBold];
            Canvas.TextRect(R, R.Left, R.Top, fTitle, TxtStyle);

            R.Left := R.Left + Canvas.TextWidth(fTitle) + ScaleX(6, 96);

            Font.Style := Self.Font.Style - [fsBold];
            Canvas.TextRect(R, R.Left, R.Top, fShortMsg, TxtStyle);
          end
          else if fTitle <> '' then begin
            Font.Style := Self.Font.Style + [fsBold];
            Canvas.TextRect(R, R.Left, R.Top, fTitle, TxtStyle);
          end
          else begin
            Font.Style := Self.Font.Style - [fsBold];
            Canvas.TextRect(R, R.Left, R.Top, fShortMsg, TxtStyle);
          end;
        end;
      end;

      mlLong:
      begin
        if fTitle <> '' then begin
          Font.Style := Self.Font.Style + [fsBold];
          Canvas.TextRect(R, R.Left, R.Top, fTitle, TxtStyle);
          R.Top := R.Top + Canvas.TextHeight(fTitle) + ScaleY(4, 96);
        end;

        Font.Style := Self.Font.Style - [fsBold];
        if fLongMsg <> '' then begin
          Canvas.TextRect(R, R.Left, R.Top, fLongMsg, TxtStyle);
        end;
      end;

    end;

    Font.Style := Self.Font.Style - [fsBold];
  end;
end;

}

procedure TSGJInfoPanel.Paint();
var
  IconIndex: integer;
  PadX, PadY, IconSize, IconSpace: integer;
  R: TRect;
  TxtStyle: TTextStyle;

  // Zmienne pomocnicze na wyliczone kolory Fluent
  FinalBorder, FinalInner: TColor;
  DrawIcon: boolean;
  IsDarkMode: boolean;
  CloseSize: integer;
  W: integer; // Zmienna do wyliczania symetrii linii krzyżyka
  CalcRect: TRect;
  // Nowe zmienne lokalne do bezpiecznego pozycjonowania przycisku akcji
  CurrentLeft: integer;
  BaseFontColor: TColor;
  IconTop, CloseTop: integer; // Zmienne do dynamicznego pozycjonowania pionowego
begin
  inherited Paint;
  if not HandleAllocated then Exit;

  // 1. AUTOMATYCZNE AKCENTY KOLORYSTYCZNE WINUI 3 LUB TRYB CUSTOM
  IsDarkMode := ColorToRGB(Self.Color) < $7F7F7F;
  DrawIcon := True;

  if not IsDarkMode then
  begin
    // ==========================================
    // PALETA DLA TRYBU JASNEGO (LIGHT MODE)
    // ==========================================
    case fSeverity of
      ivInformation: begin
        FinalBorder := $00F38B00;
        FinalInner := $00F9F3EB;
        IconIndex := DialogGlyphs.DialogIcon[idDialogInfo];
      end;
      ivConfirmation: begin
        FinalBorder := $00198610;
        FinalInner := $00F3FBF2;
        IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm];
      end;
      ivWarning: begin
        FinalBorder := $000080B8;
        FinalInner := $00EFF9FF;
        IconIndex := DialogGlyphs.DialogIcon[idDialogWarning];
      end;
      ivError: begin
        FinalBorder := $00243DE4;
        FinalInner := $00F2F3FF;
        IconIndex := DialogGlyphs.DialogIcon[idDialogError];
      end;
      ivShield: begin
        FinalBorder := $008A8A8A;
        FinalInner := $00F3F3F3;
        IconIndex := DialogGlyphs.DialogIcon[idDialogShield];
      end;
      ivCustom: begin
        FinalBorder := $008A8A8A;
        FinalInner := $00F9F9F9;
        DrawIcon := False;
        IconIndex := 0;
      end;
    end;
  end
  else
  begin
    // ==========================================
    // PALETA DLA TRYBU CIEMNEGO (DARK MODE)
    // ==========================================
    case fSeverity of
      ivInformation: begin
        FinalBorder := $00FF9D3B;
        FinalInner := $002C221A;
        IconIndex := DialogGlyphs.DialogIcon[idDialogInfo];
      end;
      ivConfirmation: begin
        FinalBorder := $0030D143;
        FinalInner := $001A2B1E;
        IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm];
      end;
      ivWarning: begin
        FinalBorder := $003BD2FF;
        FinalInner := $001A2733;
        IconIndex := DialogGlyphs.DialogIcon[idDialogWarning];
      end;
      ivError: begin
        FinalBorder := $004756FF;
        FinalInner := $00201A2D;
        IconIndex := DialogGlyphs.DialogIcon[idDialogError];
      end;
      ivShield: begin
        FinalBorder := $009E9E9E;
        FinalInner := $002D2D2D;
        IconIndex := DialogGlyphs.DialogIcon[idDialogShield];
      end;
      ivCustom: begin
        FinalBorder := $00606060;
        FinalInner := $00202020;
        DrawIcon := False;
        IconIndex := 0;
      end;
    end;
  end;

  if (Self.BorderColor <> clNone) then FinalBorder := Self.BorderColor;
  if (Self.InnerColor <> clNone) then FinalInner := Self.InnerColor;

  with Canvas do
  begin
    // 2. Definiowanie marginesów i odstępów bazujących na DPI (względem bazy 96)
    PadX := ScaleX(16, 96);
    PadY := ScaleY(12, 96);
    IconSpace := ScaleX(12, 96);
    CloseSize := ScaleX(32, 96); // Zwiększony hitbox do 32px zgodnie z WinUI

    // 3. RYSOWANIE TŁA I RAMKI
    if IsDarkMode then Pen.Color := $00404040
    else
      Pen.Color := $00E6E6E6;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := FinalInner;
    Brush.Style := bsSolid;

    RoundRect(2, 2, Self.Width - 2, Self.Height - 2, 8, 8);

    // 4. RYSOWANIE SKALOWANEJ IKONY
    IconSize := 0;
    if DrawIcon then
    begin
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
        IconSize := ScaleX(16, 96)
      else
        IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96];

      // Wyliczanie pozycji pionowej ikony w zależności od trybu
      if fMessageLength = mlShort then
        IconTop := (Self.Height - IconSize) div
          2 // Centrowanie pionowe dla trybu krótkiego
      else
        IconTop := PadY; // Klasyczna pozycja od góry dla trybu długiego

      if Assigned(DialogGlyphs) and (not (csDesigning in ComponentState)) then
        DialogGlyphs.Draw(Canvas, PadX, IconTop, IconIndex)
      else
      begin
        Pen.Color := FinalBorder;
        Brush.Style := bsClear;
        Rectangle(PadX, IconTop, PadX + IconSize, IconTop + IconSize);
      end;
    end;

    // 4a. OBLICZENIE I RYSOWANIE PRZYCISKU ZAMKNIĘCIA "X" (Z warunkowym pozycjonowaniem)
    if fMessageLength = mlShort then
      CloseTop := (Self.Height - CloseSize) div 2 // Centrowanie tylko dla mlShort
    else
      CloseTop := PadY - ScaleY(4, 96);
    // Wyczyszczona pozycja na górze dla mlLong (uwzględnia ujemny margin dla wyrównania z tekstem)

    fCloseRect := Rect(Self.Width - PadX - CloseSize, CloseTop,
      Self.Width - PadX, CloseTop + CloseSize);

    if fClosePressed then
    begin
      if IsDarkMode then Brush.Color := $00454545
      else
        Brush.Color := $00E1E1E1;
      Brush.Style := bsSolid;
      Pen.Style := psClear;
      RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end
    else if fCloseHovered then
    begin
      if IsDarkMode then Brush.Color := $00383838
      else
        Brush.Color := $00EAEAEA;
      Brush.Style := bsSolid;
      Pen.Style := psClear;
      RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end;

    // Symbol X narysowany liniami (Rozmiar niezmieniony: 8px)
    if IsDarkMode then Pen.Color := clWhite
    else
      Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsClear;

    W := ScaleX(8, 96);
    IconSize := fCloseRect.Left + (fCloseRect.Width - W) div 2;
    IconSpace := fCloseRect.Top + (fCloseRect.Height - W) div 2;

    MoveTo(IconSize, IconSpace);
    LineTo(IconSize + W + 1, IconSpace + W + 1);
    MoveTo(IconSize + W, IconSpace);
    LineTo(IconSize - 1, IconSpace + W + 1);

    // 4b. USTAWIENIE BAZOWEGO STYLU CZCIONKI DLA TEKSTÓW KOMUNIKATU
    Brush.Style := bsClear;
    Font.Size := Self.Font.Size;

    if (Self.Font.Color = clDefault) or (Self.Font.Color = clWindowText) then
    begin
      if IsDarkMode then BaseFontColor := clWhite
      else
        BaseFontColor := clBlack;
    end
    else
      BaseFontColor := Self.Font.Color;

    Font.Color := BaseFontColor;

    // Ponowne wyliczenie IconSize dla zachowania marginesu tekstowego
    if DrawIcon then
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
        IconSize := ScaleX(16, 96)
      else
        IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96]
    else
      IconSize := 0;

    if DrawIcon then CurrentLeft := PadX + IconSize + ScaleX(12, 96)
    else
      CurrentLeft := PadX;

    TxtStyle := TextStyle;
    TxtStyle.SingleLine := False;
    TxtStyle.Wordbreak := True;
    TxtStyle.Clipping := True;

    // 7. RYSOWANIE TEKSTÓW ORAZ LINKU AKCJI (ActionText)
    case fMessageLength of

      mlShort:
      begin
        if (fTitle <> '') or (fShortMsg <> '') then
        begin

          // Tytuł (Wycentrowany pionowo względem wysokości całego paska)
          if fTitle <> '' then
          begin
            Font.Style := Self.Font.Style + [fsBold];
            Font.Color := BaseFontColor;
            R := Rect(CurrentLeft, (Self.Height - Canvas.TextHeight(fTitle)) div
              2, fCloseRect.Left - ScaleX(12, 96), Self.Height);
            TextRect(R, R.Left, R.Top, fTitle, TxtStyle);
            CurrentLeft := CurrentLeft + Canvas.TextWidth(fTitle) + ScaleX(8, 96);
          end;

          // Krótka wiadomość (Wycentrowana pionowo względem wysokości całego paska)
          if fShortMsg <> '' then
          begin
            Font.Style := Self.Font.Style - [fsBold];
            Font.Color := BaseFontColor;
            R := Rect(CurrentLeft, (Self.Height - Canvas.TextHeight(fShortMsg)) div
              2, fCloseRect.Left - ScaleX(12, 96), Self.Height);
            TextRect(R, R.Left, R.Top, fShortMsg, TxtStyle);
            CurrentLeft := CurrentLeft + Canvas.TextWidth(fShortMsg) + ScaleX(16, 96);
          end;

          // PRZYCISK/LINK AKCJI W TRYBIE SHORT (Idealnie wycentrowany pionowo)
          if (fActionText <> '') and fShowAction then
          begin
            Font.Style := Self.Font.Style;

            if fActionStyle = asButton then
            begin
              // Przycisk ma 32px wysokości i jest idealnie zbalansowany w pionie
              fActionRect := Rect(CurrentLeft,
                (Self.Height - ScaleY(32, 96)) div 2, CurrentLeft +
                Canvas.TextWidth(fActionText) + ScaleX(24, 96),
                ((Self.Height - ScaleY(32, 96)) div 2) + ScaleY(32, 96));

              if fActionPressed then
              begin
                if IsDarkMode then Brush.Color := $00454545
                else
                  Brush.Color := $00E1E1E1;
              end
              else if fActionHovered then
              begin
                if IsDarkMode then Brush.Color := $00383838
                else
                  Brush.Color := $00EAEAEA;
              end
              else
              begin
                if IsDarkMode then Brush.Color := $002D2D2D
                else
                  Brush.Color := $00F3F3F3;
              end;

              Pen.Style := psSolid;
              if IsDarkMode then Pen.Color := $00555555
              else
                Pen.Color := $00D1D1D1;
              RoundRect(fActionRect, ScaleX(4, 96), ScaleX(4, 96));

              Brush.Style := bsClear;
              Font.Color := BaseFontColor;
              TextOut(fActionRect.Left + ScaleX(12, 96), fActionRect.Top +
                (fActionRect.Height - Canvas.TextHeight(fActionText)) div 2, fActionText);
            end
            else
            begin
              // Hyperlink w trybie Short wycentrowany w osi tekstu komórki
              fActionRect := Rect(CurrentLeft,
                (Self.Height - Canvas.TextHeight(fActionText)) div 2, CurrentLeft +
                Canvas.TextWidth(fActionText), ((Self.Height - Canvas.TextHeight(fActionText)) div 2) +
                Canvas.TextHeight(fActionText));
              if fActionHovered then
              begin
                if IsDarkMode then Brush.Color := $00383838
                else
                  Brush.Color := $00EAEAEA;
                Brush.Style := bsSolid;
                Pen.Style := psClear;
                RoundRect(Rect(fActionRect.Left - ScaleX(6, 96),
                  fActionRect.Top - ScaleY(4, 96), fActionRect.Right + ScaleX(6, 96),
                  fActionRect.Bottom + ScaleY(4, 96)), ScaleX(4, 96), ScaleX(4, 96));
              end;
              Brush.Style := bsClear;
              if IsDarkMode then Font.Color := $00FF9D3B
              else
                Font.Color := $00CC4400;
              TextOut(fActionRect.Left, fActionRect.Top, fActionText);
            end;
          end
          else
            fActionRect := Rect(0, 0, 0, 0);
        end;
      end;
      mlLong:
      begin
        // Przygotowanie podstawowego prostokąta dla tekstów w dół
        R.Left := CurrentLeft;
        R.Top := PadY;
        R.Right := fCloseRect.Left - ScaleX(12, 96);
        R.Bottom := Self.Height - PadY;
        // A: Tytuł
        if fTitle <> '' then
        begin
          Font.Color := BaseFontColor;
          Font.Style := Self.Font.Style + [fsBold];
          TextRect(R, R.Left, R.Top, fTitle, TxtStyle);
          CalcRect := Rect(0, 0, R.Right - R.Left, 0);
          LCLIntf.DrawText(Canvas.Handle, PChar(fTitle), Length(fTitle), CalcRect,
            DT_WORDBREAK or DT_CALCRECT);
          R.Top := R.Top + (CalcRect.Bottom - CalcRect.Top) + ScaleY(4, 96);
        end;
        // B: Długa wiadomość
        Font.Style := Self.Font.Style - [fsBold];
        if fLongMsg <> '' then
        begin
          if (Self.Font.Color = clDefault) or (Self.Font.Color = clWindowText) then
            if IsDarkMode then Font.Color := $00D4D4D4
            else
              Font.Color := $00333333
          else
            Font.Color := BaseFontColor;
          TextRect(R, R.Left, R.Top, fLongMsg, TxtStyle);
          CalcRect := Rect(0, 0, R.Right - R.Left, 0);
          LCLIntf.DrawText(Canvas.Handle, PChar(fLongMsg), Length(fLongMsg),
            CalcRect, DT_WORDBREAK or DT_CALCRECT);
          R.Top := R.Top + (CalcRect.Bottom - CalcRect.Top) + ScaleY(8, 96);
        end;
        // C: Przycisk/Hyperlink Akcji pod spodem
        if (fActionText <> '') and fShowAction then
        begin
          Font.Style := Self.Font.Style;
          if fActionStyle = asButton then
          begin
            fActionRect := Rect(R.Left, R.Top, R.Left + Canvas.TextWidth(fActionText) +
              ScaleX(24, 96), R.Top + ScaleY(32, 96));
            if fActionPressed then
            begin
              if IsDarkMode then Brush.Color := $00454545
              else
                Brush.Color := $00E1E1E1;
            end
            else if fActionHovered then
            begin
              if IsDarkMode then Brush.Color := $00383838
              else
                Brush.Color := $00EAEAEA;
            end
            else
            begin
              if IsDarkMode then Brush.Color := $002D2D2D
              else
                Brush.Color := $00F3F3F3;
            end;
            Pen.Style := psSolid;
            if IsDarkMode then Pen.Color := $00555555
            else
              Pen.Color := $00D1D1D1;
            RoundRect(fActionRect, ScaleX(4, 96), ScaleX(4, 96));
            Brush.Style := bsClear;
            if IsDarkMode then Font.Color := clWhite
            else
              Font.Color := clBlack;
            TextOut(fActionRect.Left + ScaleX(12, 96), fActionRect.Top +
              (fActionRect.Height - Canvas.TextHeight(fActionText)) div 2, fActionText);
          end
          else
          begin
            fActionRect := Rect(R.Left, R.Top + ScaleY(4, 96), R.Left +
              Canvas.TextWidth(fActionText), R.Top + ScaleY(4, 96) + Canvas.TextHeight(fActionText));
            if fActionHovered then
            begin
              if IsDarkMode then Brush.Color := $00383838
              else
                Brush.Color := $00EAEAEA;
              Brush.Style := bsSolid;
              Pen.Style := psClear;
              RoundRect(Rect(fActionRect.Left - ScaleX(6, 96), fActionRect.Top -
                ScaleY(4, 96), fActionRect.Right + ScaleX(6, 96), fActionRect.Bottom + ScaleY(4, 96)),
                ScaleX(4, 96), ScaleX(4, 96));
            end;
            Brush.Style := bsClear;
            if IsDarkMode then Font.Color := $00FF9D3B
            else
              Font.Color := $00CC4400;
            TextOut(fActionRect.Left, fActionRect.Top, fActionText);
          end;
        end
        else
          fActionRect := Rect(0, 0, 0, 0);
      end;
    end;
    // Resetowanie stylów czcionki na koniec procedury Paint
    Font.Style := Self.Font.Style;
    Font.Color := Self.Font.Color;
  end;
end;
{
procedure TSGJInfoPanel.Paint();
var
  IconIndex: Integer;
  PadX, PadY, IconSize, IconSpace: Integer;
  R: TRect;
  TxtStyle: TTextStyle;

  // Zmienne pomocnicze na wyliczone kolory Fluent
  FinalBorder, FinalInner: TColor;
  DrawIcon: Boolean;
  IsDarkMode: Boolean;
  CloseSize: Integer;
  W: Integer; // Zmienna do wyliczania symetrii linii krzyżyka
  CalcRect:TRect;
  // Nowe zmienne lokalne do bezpiecznego pozycjonowania przycisku akcji
  CurrentLeft: Integer;
  BaseFontColor: TColor;
begin
  inherited Paint;
  if not HandleAllocated then Exit;

  // 1. AUTOMATYCZNE AKCENTY KOLORYSTYCZNE WINUI 3 LUB TRYB CUSTOM

  IsDarkMode := ColorToRGB(Self.Color) < $7F7F7F;
  DrawIcon := True;

  if not IsDarkMode then
    begin
      // ==========================================
      // PALETA DLA TRYBU JASNEGO (LIGHT MODE)
      // ==========================================
      case fSeverity of
        ivInformation: begin FinalBorder := $00F38B00; FinalInner := $00F9F3EB; IconIndex := DialogGlyphs.DialogIcon[idDialogInfo]; end;
        ivConfirmation: begin FinalBorder := $00198610; FinalInner := $00F3FBF2; IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm]; end;
        ivWarning:     begin FinalBorder := $000080B8; FinalInner := $00EFF9FF; IconIndex := DialogGlyphs.DialogIcon[idDialogWarning]; end;
        ivError:       begin FinalBorder := $00243DE4; FinalInner := $00F2F3FF; IconIndex := DialogGlyphs.DialogIcon[idDialogError]; end;
        ivShield:      begin FinalBorder := $008A8A8A; FinalInner := $00F3F3F3; IconIndex := DialogGlyphs.DialogIcon[idDialogShield]; end;
        ivCustom:      begin FinalBorder := $008A8A8A; FinalInner := $00F9F9F9; DrawIcon := False; IconIndex := 0; end;
      end;
    end
    else
    begin
      // ==========================================
      // PALETA DLA TRYBU CIEMNEGO (DARK MODE)
      // ==========================================
      case fSeverity of
        ivInformation: begin FinalBorder := $00FF9D3B; FinalInner := $002C221A; IconIndex := DialogGlyphs.DialogIcon[idDialogInfo]; end;
        ivConfirmation: begin FinalBorder := $0030D143; FinalInner := $001A2B1E; IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm]; end;
        ivWarning:     begin FinalBorder := $003BD2FF; FinalInner := $001A2733; IconIndex := DialogGlyphs.DialogIcon[idDialogWarning]; end;
        ivError:       begin FinalBorder := $004756FF; FinalInner := $00201A2D; IconIndex := DialogGlyphs.DialogIcon[idDialogError]; end;
        ivShield:      begin FinalBorder := $009E9E9E; FinalInner := $002D2D2D; IconIndex := DialogGlyphs.DialogIcon[idDialogShield]; end;
        ivCustom:      begin FinalBorder := $00606060; FinalInner := $00202020; DrawIcon := False; IconIndex := 0; end;
      end;
    end;

    if (Self.BorderColor <> clNone) then FinalBorder := Self.BorderColor;
    if (Self.InnerColor <> clNone) then FinalInner := Self.InnerColor;

    with Canvas do begin
    // 2. Definiowanie marginesów i odstępów bazujących na DPI (względem bazy 96)
    PadX := ScaleX(16, 96);
    PadY := ScaleY(12, 96);
    IconSpace := ScaleX(12, 96);

    // 3. RYSOWANIE TŁA I RAMKI
    if IsDarkMode then Pen.Color := $00404040 else Pen.Color := $00E6E6E6;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Color := FinalInner;
    Brush.Style := bsSolid;

    RoundRect(2, 2, Self.Width - 2, Self.Height - 2, 8, 8);

    // 4. RYSOWANIE SKALOWANEJ IKONY
    IconSize := 0;
    if DrawIcon then begin
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
        IconSize := ScaleX(16, 96)
      else
        IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96];

      if Assigned(DialogGlyphs) and (not (csDesigning in ComponentState)) then
        DialogGlyphs.Draw(Canvas, PadX, PadY, IconIndex)
      else begin
        Pen.Color := FinalBorder;
        Brush.Style := bsClear;
        Rectangle(PadX, PadY, PadX + IconSize, PadY + IconSize);
      end;
    end;

    // 4a. RYSOWANIE PRZYCISKU ZAMKNIĘCIA "X"
    CloseSize := ScaleX(24, 96);
    fCloseRect := Rect(Self.Width - PadX - CloseSize, PadY - ScaleY(4, 96), Self.Width - PadX, PadY - ScaleY(4, 96) + CloseSize);

    if fClosePressed then
    begin
      if IsDarkMode then Brush.Color := $00454545 else Brush.Color := $00E1E1E1;
      Brush.Style := bsSolid; Pen.Style := psClear;
      Canvas.RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end
    else if fCloseHovered then
    begin
      if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA;
      Brush.Style := bsSolid; Pen.Style := psClear;
      Canvas.RoundRect(fCloseRect, ScaleX(4, 96), ScaleX(4, 96));
    end;

    // Symbol X narysowany liniami
    if IsDarkMode then Pen.Color := clWhite else Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Style := psSolid;
    Brush.Style := bsClear;

    W := ScaleX(8, 96);
    IconSize := fCloseRect.Left + (CloseSize - W) div 2;
    IconSpace := fCloseRect.Top + (CloseSize - W) div 2;

    MoveTo(IconSize, IconSpace); LineTo(IconSize + W + 1, IconSpace + W + 1);
    MoveTo(IconSize + W, IconSpace); LineTo(IconSize - 1, IconSpace + W + 1);

    // 4b. USTAWIENIE BAZOWEGO STYLU CZCIONKI DLA TEKSTÓW KOMUNIKATU
    Brush.Style := bsClear;
    Font.Size := Self.Font.Size;

    if (Self.Font.Color = clDefault) or (Self.Font.Color = clWindowText) then
    begin
      if IsDarkMode then BaseFontColor := clWhite else BaseFontColor := clBlack;
    end
    else
      BaseFontColor := Self.Font.Color;

    Font.Color := BaseFontColor;

    // Ponowne wyliczenie IconSize dla zachowania marginesu tekstowego
    if DrawIcon then
      if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then IconSize := ScaleX(16, 96)
      else IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96]
    else IconSize := 0;

    if DrawIcon then CurrentLeft := PadX + IconSize + IconSpace else CurrentLeft := PadX;

    TxtStyle := TextStyle;
    TxtStyle.SingleLine := False;
    TxtStyle.Wordbreak := True;
    TxtStyle.Clipping := True;

    // 7. RYSOWANIE TEKSTÓW ORAZ LINKU AKCJI (ActionText)
    case fMessageLength of

      mlShort:
      begin
        if (fTitle <> '') or (fShortMsg <> '') then begin

          // Tytuł
          if fTitle <> '' then begin
            Font.Style := Self.Font.Style + [fsBold];
            Font.Color := BaseFontColor;
            R := Rect(CurrentLeft, PadY, fCloseRect.Left - ScaleX(12, 96), Self.Height - PadY);
            Canvas.TextRect(R, R.Left, R.Top, fTitle, TxtStyle);
            CurrentLeft := CurrentLeft + Canvas.TextWidth(fTitle) + ScaleX(6, 96);
          end;

          // Krótka wiadomość
          if fShortMsg <> '' then begin
            Font.Style := Self.Font.Style - [fsBold];
            Font.Color := BaseFontColor;
            R := Rect(CurrentLeft, PadY, fCloseRect.Left - ScaleX(12, 96), Self.Height - PadY);
            Canvas.TextRect(R, R.Left, R.Top, fShortMsg, TxtStyle);
            CurrentLeft := CurrentLeft + Canvas.TextWidth(fShortMsg) + ScaleX(12, 96);
          end;

          // --- NOWOŚĆ: KROK PO KROKU - PRZYCISK/LINK AKCJI W TRYBIE SHORT ---
          if (fActionText <> '') and fShowAction then
          begin
            Font.Style := Self.Font.Style;

            if fActionStyle = asButton then begin
              // Rysowanie obudowy przycisku
              fActionRect := Rect(CurrentLeft, PadY - ScaleY(4, 96), CurrentLeft + Canvas.TextWidth(fActionText) + ScaleX(16, 96), PadY - ScaleY(4, 96) + ScaleY(24, 96));

              if fActionPressed then begin if IsDarkMode then Brush.Color := $00454545 else Brush.Color := $00E1E1E1; end
              else if fActionHovered then begin if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA; end
              else begin if IsDarkMode then Brush.Color := $002D2D2D else Brush.Color := $00F3F3F3; end;

              Pen.Style := psSolid;
              if IsDarkMode then Pen.Color := $00555555 else Pen.Color := $00D1D1D1;
              Canvas.RoundRect(fActionRect, ScaleX(4, 96), ScaleX(4, 96));

              // Rysowanie napisu wewnątrz przycisku
              Brush.Style := bsClear;
              Font.Color := BaseFontColor;
              Canvas.TextOut(fActionRect.Left + ScaleX(8, 96), fActionRect.Top + (fActionRect.Height - Canvas.TextHeight(fActionText)) div 2, fActionText);
            end
            else begin
              // Rysowanie tła w trybie Hyperlink przy najechaniu (Hover)
              fActionRect := Rect(CurrentLeft, PadY, CurrentLeft + Canvas.TextWidth(fActionText), PadY + Canvas.TextHeight(fActionText));
              if fActionHovered then begin
                if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA;
                Brush.Style := bsSolid; Pen.Style := psClear;
                Canvas.RoundRect(Rect(fActionRect.Left - 4, fActionRect.Top - 2, fActionRect.Right + 4, fActionRect.Bottom + 2), ScaleX(4, 96), ScaleX(4, 96));
              end;

              // Rysowanie tekstu z kolorem akcentu WinUI 3
              Brush.Style := bsClear;
              if IsDarkMode then Font.Color := $00FF9D3B else Font.Color := $00CC4400;
              Canvas.TextOut(fActionRect.Left, fActionRect.Top, fActionText);
            end;
          end else fActionRect := Rect(0,0,0,0);
        end;
      end;

      mlLong:
      begin
        // Przygotowanie podstawowego prostokąta dla tekstów w dół
        R.Left := CurrentLeft;
        R.Top := PadY;
        R.Right := fCloseRect.Left - ScaleX(12, 96);
        R.Bottom := Self.Height - PadY;

        // A: Tytuł
        if fTitle <> '' then begin
          Font.Color := BaseFontColor;
          Font.Style := Self.Font.Style + [fsBold];
          Canvas.TextRect(R, R.Left, R.Top, fTitle, TxtStyle);

          // Mierzymy ile dokładnie miejsca zajął tytuł w tym obszarze
          CalcRect := Rect(0, 0, R.Right - R.Left, 0);
          LCLIntf.DrawText(Canvas.Handle, PChar(fTitle), Length(fTitle), CalcRect, DT_WORDBREAK or DT_CALCRECT);
          R.Top := R.Top + (CalcRect.Bottom - CalcRect.Top) + ScaleY(4, 96);
        end;

        // B: Długa wiadomość
        Font.Style := Self.Font.Style - [fsBold];
        if fLongMsg <> '' then begin
          if (Self.Font.Color = clDefault) or (Self.Font.Color = clWindowText) then
            if IsDarkMode then Font.Color := $00D4D4D4 else Font.Color := $00333333
          else Font.Color := BaseFontColor;

          Canvas.TextRect(R, R.Left, R.Top, fLongMsg, TxtStyle);

          // DYNAMICZNY POMIAR: Sprawdzamy rzeczywistą wysokość zajętą przez złamany tekst
          CalcRect := Rect(0, 0, R.Right - R.Left, 0);
          LCLIntf.DrawText(Canvas.Handle, PChar(fLongMsg), Length(fLongMsg), CalcRect, DT_WORDBREAK or DT_CALCRECT);

          // Przesuwamy wskaźnik R.Top dokładnie pod OSTATNIĄ linijkę tekstu + margines 8px
          R.Top := R.Top + (CalcRect.Bottom - CalcRect.Top) + ScaleY(8, 96);
        end;

        // C: Przycisk/Hyperlink Akcji pod spodem
                if (fActionText <> '') and fShowAction then
                begin
                  Font.Style := Self.Font.Style;
                  if fActionStyle = asButton then begin
                    // WinUI Standard: Wysokość przycisku InfoBar to zwykle 32px, padding boczny 12px
                    fActionRect := Rect(R.Left, R.Top, R.Left + Canvas.TextWidth(fActionText) + ScaleX(24, 96), R.Top + ScaleY(32, 96));

                    if fActionPressed then begin if IsDarkMode then Brush.Color := $00454545 else Brush.Color := $00E1E1E1; end
                    else if fActionHovered then begin if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA; end
                    else begin if IsDarkMode then Brush.Color := $002D2D2D else Brush.Color := $00F3F3F3; end;

                    Pen.Style := psSolid;
                    if IsDarkMode then Pen.Color := $00555555 else Pen.Color := $00D1D1D1;
                    Canvas.RoundRect(fActionRect, ScaleX(4, 96), ScaleX(4, 96));

                    Brush.Style := bsClear;
                    if IsDarkMode then Font.Color := clWhite else Font.Color := clBlack;

                    // Poprawione centrowanie tekstu w pionie i poziomie (uwzględnia dynamiczną wysokość fActionRect)
                    Canvas.TextOut(fActionRect.Left + ScaleX(12, 96), fActionRect.Top + (fActionRect.Height - Canvas.TextHeight(fActionText)) div 2, fActionText);
                  end
                  else begin
                    // Dla stylu Hyperlink: dopasowanie tła (hover) z zachowaniem czytelnych marginesów WinUI
                    fActionRect := Rect(R.Left, R.Top, R.Left + Canvas.TextWidth(fActionText), R.Top + Canvas.TextHeight(fActionText));
                    if fActionHovered then begin
                      if IsDarkMode then Brush.Color := $00383838 else Brush.Color := $00EAEAEA;
                      Brush.Style := bsSolid; Pen.Style := psClear;
                      // Przeskalowano stałe paddingi (4px i 2px) funkcją ScaleX/ScaleY, aby dobrze wyglądały w High DPI
                      Canvas.RoundRect(Rect(fActionRect.Left - ScaleX(6, 96), fActionRect.Top - ScaleY(4, 96), fActionRect.Right + ScaleX(6, 96), fActionRect.Bottom + ScaleY(4, 96)), ScaleX(4, 96), ScaleX(4, 96));
                    end;
                    Brush.Style := bsClear;
                    if IsDarkMode then Font.Color := $00FF9D3B else Font.Color := $00CC4400;
                    Canvas.TextOut(fActionRect.Left, fActionRect.Top, fActionText);
                  end;
                end else fActionRect := Rect(0,0,0,0);
              end;


          end;
          // Resetowanie stylów czcionki na koniec procedury Paint
          Font.Style := Self.Font.Style;
          Font.Color := Self.Font.Color;
          end;
          end;



procedure TSGJInfoPanel.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                               WithStandardChildren: Boolean);
var
  IconIndex: Integer;
  PadX, PadY, IconSize, IconSpace: Integer;
  W, H: Integer;
  DrawIcon: Boolean;
  CloseSize: Integer;
begin
  // 1. Definiujemy bazowe, czyste miary (bez nakładania podwójnego skalowania LCL)
  PadX := ScaleX(16, 96);
  PadY := ScaleY(12, 96);
  IconSpace := ScaleX(12, 96);
  CloseSize := ScaleX(16, 96);

  DrawIcon := True;
  IconIndex := 0;

  case fSeverity of
    ivInformation:  IconIndex := DialogGlyphs.DialogIcon[idDialogInfo];
    ivConfirmation: IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm];
    ivWarning:      IconIndex := DialogGlyphs.DialogIcon[idDialogWarning];
    ivError:        IconIndex := DialogGlyphs.DialogIcon[idDialogError];
    ivShield:       IconIndex := DialogGlyphs.DialogIcon[idDialogShield];
    ivCustom:       DrawIcon := False;
  end;

  // Ustalamy precyzyjny rozmiar ikony
  IconSize := 0;
  if DrawIcon then
  begin
    if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
      IconSize := ScaleX(16, 96)
    else
      IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96];
  end;

  // Pobranie aktualnego kontekstu czcionki komponentu
  Canvas.Font.Assign(Self.Font);
  if Canvas.Font.Reference.Handle = 0 then ;

  case fMessageLength of
    mlShort:
    begin
      // Dokładny pomiar szerokości samego tekstu
      Canvas.Font.Style := Self.Font.Style + [fsBold];
      W := Canvas.TextWidth(fTitle);

      Canvas.Font.Style := Self.Font.Style - [fsBold];
      if (fTitle <> '') and (fShortMsg <> '') then
        W := W + ScaleX(6, 96) + Canvas.TextWidth(fShortMsg)
      else if fShortMsg <> '' then
        W := Canvas.TextWidth(fShortMsg);

      // PRECYZYJNA POPRAWKA SZEROKOŚCI:
      // Sumujemy tylko niezbędne elementy układu w jednej linii bez sztucznych zapasów:
      // [Lewy margines] + [Ikona + Odstęp] + [Szerokość Tekstów] + [Odstęp] + [Przycisk X] + [Prawy margines]
      if DrawIcon then
        PreferredWidth := PadX + IconSize + IconSpace + W + ScaleX(12, 96) + CloseSize + PadX
      else
        PreferredWidth := PadX + W + ScaleX(12, 96) + CloseSize + PadX;

      // Precyzyjna wysokość
      H := Canvas.TextHeight('Wg');
      if IconSize > H then H := IconSize;
      if CloseSize > H then H := CloseSize;
      PreferredHeight := PadY + H + PadY;
    end;

    mlLong:
    begin
      // W trybie długim zachowujemy szerokość zadeklarowaną, dopasowujemy tylko wysokość w dół
      PreferredWidth := Self.Width;

      H := 0;
      Canvas.Font.Style := Self.Font.Style + [fsBold];
      if fTitle <> '' then H := H + Canvas.TextHeight(fTitle) + ScaleY(4, 96);

      Canvas.Font.Style := Self.Font.Style - [fsBold];
      if fLongMsg <> '' then H := H + Canvas.TextHeight(fLongMsg);

      if IconSize > H then H := IconSize;
      if CloseSize > H then H := CloseSize;
      PreferredHeight := PadY + H + PadY + ScaleY(4, 96);
    end;
  end;
end;
 }
procedure TSGJInfoPanel.CalculatePreferredSize(
  var PreferredWidth, PreferredHeight: integer;
  WithStandardChildren: boolean);
var
  IconIndex: integer;
  PadX, PadY, IconSize, IconSpace: integer;
  W, H, ActionW, TextAvailableWidth: integer;
  DrawIcon: boolean;
  CloseSize: integer;
  CalcRect: TRect;
begin
  // 1. Marginesy bazowe i nowy fizyczny gabaryt przycisku X (zgodnie z Paint)
  PadX := ScaleX(16, 96);
  PadY := ScaleY(12, 96);
  IconSpace := ScaleX(12, 96);
  CloseSize := ScaleX(32, 96); // Korekta z 16px na 32px (WinUI Hitbox)

  DrawIcon := True;
  IconIndex := 0;

  case fSeverity of
    ivInformation: IconIndex := DialogGlyphs.DialogIcon[idDialogInfo];
    ivConfirmation: IconIndex := DialogGlyphs.DialogIcon[idDialogConfirm];
    ivWarning: IconIndex := DialogGlyphs.DialogIcon[idDialogWarning];
    ivError: IconIndex := DialogGlyphs.DialogIcon[idDialogError];
    ivShield: IconIndex := DialogGlyphs.DialogIcon[idDialogShield];
    ivCustom: DrawIcon := False;
  end;

  IconSize := 0;
  if DrawIcon then
  begin
    if (csDesigning in ComponentState) or (not Assigned(DialogGlyphs)) then
      IconSize := ScaleX(16, 96)
    else
      IconSize := DialogGlyphs.WidthForPPI[IconIndex, 96];
  end;

  Canvas.Font.Assign(Self.Font);
  if Canvas.Font.Reference.Handle = 0 then;

  // Wyliczenie realnej szerokości przycisku akcji z uwzględnieniem paddingu 12px z każdej strony (razem 24px)
  ActionW := 0;
  if (fActionText <> '') and fShowAction then
  begin
    ActionW := Canvas.TextWidth(fActionText);
    if fActionStyle = asButton then
      ActionW := ActionW + ScaleX(24, 96)
    // Korekta: dopasowano do ScaleX(24, 96) z procedury Paint
    else
      ActionW := ActionW; // Dla hyperlinka to czysty TextWidth
  end;

  case fMessageLength of
    // ==========================================
    // TRYB SHORT (Poziomy)
    // ==========================================
    mlShort:
    begin
      Canvas.Font.Style := Self.Font.Style + [fsBold];
      W := Canvas.TextWidth(fTitle);

      Canvas.Font.Style := Self.Font.Style - [fsBold];
      if (fTitle <> '') and (fShortMsg <> '') then
        W := W + ScaleX(8, 96) + Canvas.TextWidth(fShortMsg)
      // Zsynchronizowano odstęp z Paint (8px)
      else if fShortMsg <> '' then
        W := Canvas.TextWidth(fShortMsg);

      if ActionW > 0 then
        W := W + ScaleX(16, 96) + ActionW;
      // Zsynchronizowano odstęp przed akcją z Paint (16px)

      if DrawIcon then
        PreferredWidth := PadX + IconSize + ScaleX(12, 96) + W +
          ScaleX(12, 96) + CloseSize + PadX
      else
        PreferredWidth := PadX + W + ScaleX(12, 96) + CloseSize + PadX;

      // Wyliczanie wysokości elementu centralnego
      H := Canvas.TextHeight('Wg');
      if (fActionText <> '') and fShowAction then
      begin
        if fActionStyle = asButton then
        begin
          if ScaleY(32, 96) > H then H := ScaleY(32, 96);
          // Korekta: min. wysokość przycisku to teraz 32px
        end
        else
        begin
          if Canvas.TextHeight(fActionText) > H then H := Canvas.TextHeight(fActionText);
        end;
      end;

      if IconSize > H then H := IconSize;
      if CloseSize > H then H := CloseSize;
      // Zabezpieczenie: przycisk X ma teraz 32px wysokości

      PreferredHeight := PadY + H + PadY;
    end;

    // ==========================================
    // TRYB LONG (Pionowy)
    // ==========================================
    mlLong:
    begin
      if Self.Width > ScaleX(50, 96) then
        PreferredWidth := Self.Width
      else
        PreferredWidth := ScaleX(300, 96);

      H := 0;

      // Wyliczamy DOKŁADNĄ szerokość dostępną wyłącznie dla tekstu
      if DrawIcon then
        TextAvailableWidth := PreferredWidth - (PadX + IconSize + ScaleX(12, 96)) -
          (ScaleX(12, 96) + CloseSize + PadX)
      else
        TextAvailableWidth := PreferredWidth - PadX -
          (ScaleX(12, 96) + CloseSize + PadX);

      if TextAvailableWidth < ScaleX(50, 96) then TextAvailableWidth := ScaleX(200, 96);

      // A: Dokładne mierzenie wysokości Tytułu
      if fTitle <> '' then
      begin
        Canvas.Font.Style := Self.Font.Style + [fsBold];
        CalcRect := Rect(0, 0, TextAvailableWidth, 0);
        LCLIntf.DrawText(Canvas.Handle, PChar(fTitle), Length(fTitle),
          CalcRect, LCLType.DT_WORDBREAK or LCLType.DT_CALCRECT);
        H := H + (CalcRect.Bottom - CalcRect.Top) + ScaleY(4, 96);
      end;

      // B: Dokładne mierzenie wysokości długiej wiadomości po zawinięciu (Wordbreak)
      if fLongMsg <> '' then
      begin
        Canvas.Font.Style := Self.Font.Style - [fsBold];
        CalcRect := Rect(0, 0, TextAvailableWidth, 0);
        LCLIntf.DrawText(Canvas.Handle, PChar(fLongMsg), Length(fLongMsg),
          CalcRect, LCLType.DT_WORDBREAK or LCLType.DT_CALCRECT);
        H := H + (CalcRect.Bottom - CalcRect.Top) + ScaleY(8, 96);
      end;

      // C: Przestrzeń na dolny przycisk / link akcji (Dopasowano do wymiarów z Paint)
      if (fActionText <> '') and fShowAction then
      begin
        if fActionStyle = asButton then
          H := H + ScaleY(32, 96) // Korekta z 24px na 32px wysokości przycisku
        else
          H := H + ScaleY(4, 96) + Canvas.TextHeight(fActionText);
        // Dodano margines górny hyperlinka
      end;

      // Wyrównanie gabarytów pionowych
      if IconSize > H then H := IconSize;
      if CloseSize > H then H := CloseSize;

      PreferredHeight := PadY + H + PadY;
    end;
  end;
  Canvas.Font.Style := Self.Font.Style;
end;


procedure TSGJInfoPanel.MouseMove(Shift: TShiftState; X, Y: integer);
var
  WasCloseHovered, WasActionHovered: boolean;
  Pt: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  Pt := Point(X, Y);

  WasCloseHovered := fCloseHovered;
  WasActionHovered := fActionHovered;

  // Przycisk zamknij sprawdzamy zawsze
  fCloseHovered := fCloseRect.Contains(Pt);

  // Przycisk akcji sprawdzamy TYLKO wtedy, gdy jest włączony (fShowAction = True)
  if fShowAction then
    fActionHovered := fActionRect.Contains(Pt)
  else
    fActionHovered := False;

  // Zmiana kursora na łapkę, jeśli najechaliśmy na jakikolwiek aktywny przycisk
  if fCloseHovered or fActionHovered then
    Cursor := crHandPoint
  else
    Cursor := crDefault;

  if not fActionHovered then
    fActionPressed := False;

  if (WasCloseHovered <> fCloseHovered) or (WasActionHovered <> fActionHovered) then
    Invalidate;
end;


procedure TSGJInfoPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  Pt: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  Pt := Point(X, Y);

  if Button = mbLeft then
  begin
    if fCloseHovered then
    begin
      fClosePressed := True;
      Invalidate;
    end;
    if fActionHovered and fShowAction then
    begin
      fActionPressed := True;
      Invalidate;
    end;
  end;
end;

procedure TSGJInfoPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  Pt: TPoint;
begin
  inherited MouseUp(Button, Shift, X, Y);
  Pt := Point(X, Y);

  if Button = mbLeft then
  begin
    if fClosePressed then
    begin
      fClosePressed := False;
      Invalidate;
      if fCloseRect.Contains(Pt) and Assigned(fOnCloseClick) then
        fOnCloseClick(Self);
    end;

    if fActionPressed and fShowAction then
    begin
      fActionPressed := False;
      Invalidate;
      if fActionRect.Contains(Pt) and Assigned(fOnActionClick) then
        fOnActionClick(Self);
    end;

  end;
end;

procedure TSGJInfoPanel.MouseLeave;
begin
  inherited MouseLeave;
  if fCloseHovered or fClosePressed or fActionHovered or fActionPressed then
  begin
    fCloseHovered := False;
    fClosePressed := False;
    fActionHovered := False;
    fActionPressed := False;
    Cursor := crDefault;
    Invalidate;
  end;
end;


procedure TSGJInfoPanel.SetActionText(const AValue: TTranslateString);
begin
  if fActionText <> AValue then
  begin
    fActionText := AValue;
    Invalidate;
  end;
end;

procedure TSGJInfoPanel.SetActionStyle(AValue: TSGJIPActionStyle);
begin
  if fActionStyle <> AValue then
  begin
    fActionStyle := AValue;
    Invalidate;
  end;
end;

procedure TSGJInfoPanel.SetShowAction(AValue: boolean);
begin
  if fShowAction <> AValue then
  begin
    fShowAction := AValue;
    Invalidate(); // Wymusza odświeżenie graficzne po włączeniu/wyłączeniu
  end;
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.InfoPanel.lrs}
{$ENDIF}

end.
