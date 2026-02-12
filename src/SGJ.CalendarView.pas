{********************************************************************
 home page : https://www.hiperapps.com
 email     : sgj@sgjps.com
 github    : https://github.com/sgjps/SGJ_Controls

 Control name: TSGJCalendarView

 date      : 2026/02/08

 This file is part of SGJ Controls for Lazarus

********************************************************************}
unit SGJ.CalendarView;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF LCLGTK2} Gtk2, Gtk2Def, {$ENDIF}
  LCLType, LCLIntf, LResources, Classes, Buttons, SysUtils, Grids, Controls,
  DateUtils, StdCtrls, ExtCtrls, Dialogs,
  Graphics, SGJ.SimpleButton;

type
  TSGJCalendarViewStyle = (cvsDays, cvsMonths, cvsYears);
  TSGJCalendarFirstDayOfWeek = (dwSunday, dwMonday);

  TSGJCalendarV = class(TCustomStringGrid)
  private
    DTa, DTb: TDateTime;
    fViewStyle: TSGJCalendarViewStyle;
    fDayNow: integer;
    fDaySelected: integer;
    fComparedDate: boolean;
    fComparedSDate: boolean;
    procedure Init;
  protected
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: integer; ARect: TRect; AState: TGridDrawState);
      override;
    function SelectCell(ACol, ARow: integer): boolean; override;
    procedure Click; override;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    Year, Month, Day: word;
    NextMonth,PreviousMonth:word;
    HeaderYear: word;
    HeaderMonth: word;
    FDrawFocus: boolean;
    DaysNotInMonth: TColor;
    TodayColor: TColor;
    DayFromDate: TColor;
    DateTime: TDateTime;
    FS: TFormatSettings;
    ShowPrevAndNextMonthName: boolean;
    FirstDayOfWeek: TSGJCalendarFirstDayOfWeek;
    constructor Create(Aowner: TComponent); override;
    procedure LoadMonthCalendar(AYear, AMonth: word);
    procedure LoadYearCalendar();
    procedure LoadDecadeCalendar(AYear: word);
    procedure Resize; override;
    procedure SetHeaderDate(AYear, AMonth: Word);
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property FocusColor;
    property ViewStyle: TSGJCalendarViewStyle read fViewStyle write fViewStyle;
  end;

  TSGJCalendarView = class;

  TCalendarButton = class(TCustomControl)
  private
    isHovered:boolean;
    fCalendarView:TSGJCalendarView;
    constructor Create(AOwner: TSGJCalendarView);
  protected
     procedure AdjustSize; override;
     procedure DoEnter; override;
     procedure DoExit; override;
     procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
     procedure MouseLeave(); override;
     procedure Paint; override;

  end;

  TSGJCalendarView = class(TCustomControl)
  private
    fHeader: TCustomControl;
    fCalendar: TSGJCalendarV;
    fBtnLeft: TCalendarButton;
    fUp: TCalendarButton;
    fShowGrid: boolean;
    fGridLineColor: TColor;
    fFocusColor: TColor;
    fCalendarColor: TColor;
    fCalendarOtherdays: TColor;
    fCurrentDay: TColor;
    fSelectedDay: TColor;
    fFontColor: TColor;
    fFontButtonHoverColor: TColor;
    fButtonsHoverColor: TColor;
    fDown: TCalendarButton;
    fDate: TDateTime;
    fFirstDayOfWeek: TSGJCalendarFirstDayOfWeek;
    FS: TFormatSettings;
    fShowMonthNames: boolean;
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure BtnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure PaintBtnText(Sender: TObject);
    procedure SetDate(AValue: TDateTime);
    procedure Init(ADate: TDateTime);
    procedure SetGrid(AValue:Boolean);
    procedure SetFocusColor(AValue:TColor);
    procedure SetTodayColor(AValue:TColor);
    procedure SetOtherDaysColor(AValue:TColor);
    procedure SetFontColor(AValue:TColor);
    procedure SetFirstDay(AValue:TSGJCalendarFirstDayOfWeek);
    procedure SetVisibleMonthsPreNext(AValue: boolean);
  public
    constructor Create(Aowner: TComponent); override;
    procedure SetHeader(AValue: string);
    procedure Loaded; override;
  protected
    procedure SetColor(AValue: TColor); override;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Date: TDateTime read fDate write SetDate;
    property ShowGrid: boolean read fShowGrid write SetGrid;
    property GridLineColor: TColor read fGridLineColor write fGridLineColor;
    property FocusColor: TColor read fFocusColor write fFocusColor;
    property TodayColor: TColor read fCurrentDay write SetTodayColor;
    property DayFromDate: TColor read fSelectedDay write fSelectedDay;
    property DaysNotInMonth: TColor read fCalendarOtherdays write SetOtherDaysColor;
    property FontColor: TColor read fFontColor write SetFontColor;
    property FontColorButtonHover: TColor read fFontButtonHoverColor
      write fFontButtonHoverColor;
    property ShowPrevAndNextMonthName: boolean read fShowMonthNames write SetVisibleMonthsPreNext;
    property ButtonHoverColor: TColor read fButtonsHoverColor write fButtonsHoverColor;
    property FirstDayOfWeek: TSGJCalendarFirstDayOfWeek
      read fFirstDayOfWeek write SetFirstDay;
    property TabStop;
    property TabOrder;
  end;



procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJCalendarView]);
end;

procedure DrawArrowUp(C: TCanvas; X, Y, Size: integer);
begin
  C.Pen.Width := 2;
  // Left stroke
  C.MoveTo(X - Size, Y + Size);
  C.LineTo(X, Y - Size);

  // Right stroke
  C.MoveTo(X + Size, Y + Size);
  C.LineTo(X, Y - Size);
end;


procedure DrawArrowDown(C: TCanvas; X, Y, Size: integer);
begin
  C.Pen.Width := 2;
  // Left stroke
  C.MoveTo(X - Size, Y - Size);
  C.LineTo(X, Y + Size);

  // Right stroke
  C.MoveTo(X + Size, Y - Size);
  C.LineTo(X, Y + Size);
end;

constructor TCalendarButton.Create(AOwner: TSGJCalendarView);
begin
  inherited Create(AOwner);
  FCalendarView := AOwner;
end;

procedure TCalendarButton.AdjustSize;
var
  TextWidth, TextHeight: Integer;
begin
  if HandleAllocated then
  begin
  Canvas.Font := Self.Font;
  TextWidth := Canvas.TextWidth(Caption);
  TextHeight := Canvas.TextHeight(Caption);

  // Add padding for button borders
  SetBounds(Left, Top, TextWidth + 20, TextHeight + 10);
  end;
end;

procedure TCalendarButton.DoEnter;
begin
 Canvas.DrawFocusRect(ClientRect);
end;

procedure TCalendarButton.DoExit;
begin
  Invalidate;
end;

procedure TCalendarButton.paint;
begin
     inherited paint;
  if isHovered then
  begin
   Canvas.Brush.Color:=FCalendarView.ButtonHoverColor;
   Canvas.Font.Color := FCalendarView.FontColorButtonHover;
   Canvas.Brush.Color := FCalendarView.ButtonHoverColor;
   Canvas.Pen.Color := FCalendarView.FontColorButtonHover;
   Canvas.Pen.Style:=psClear;
   {$IFDEF LCLGTK2}
   Canvas.FillRect(clientrect);
   {$ELSE}
   Canvas.RoundRect(ClientRect,10,10);
   {$ENDIF}
  end
  else
  begin
  Canvas.Brush.Color:=FCalendarView.Color;
  Canvas.Font.Color := FCalendarView.FontColor;
  Canvas.Pen.Color := FCalendarView.FontColor;
  Canvas.FillRect(clientrect);
  end;
  FCalendarView.PaintBtnText(self);
end;
procedure TCalendarButton.MouseLeave;
begin
   isHovered:=false;
   invalidate;
end;
procedure TCalendarButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
   isHovered:=true;
   invalidate;
end;

procedure TSGJCalendarView.SetHeader(AValue: string);
begin
  fbtnLeft.Caption := AValue;
end;

procedure TSGJCalendarView.Init(ADate: TDateTime);
begin
  DecodeDate(ADate, fCalendar.HeaderYear, fCalendar.HeaderMonth, fCalendar.Day);
  fBtnLeft.Caption := FormatDateTime('mmmm yyyy', ADate);
  fCalendar.SetHeaderDate(fCalendar.HeaderYear, fCalendar.HeaderMonth);
end;

procedure TSGJCalendarView.SetFirstDay(AValue:TSGJCalendarFirstDayOfWeek);
begin
  if fFirstDayOfWeek <> AValue then
     fFirstDayOfWeek:=AValue;

  if HandleAllocated then begin
     fCalendar.FirstDayOfWeek:=AValue;
     fCalendar.LoadMonthCalendar(fCalendar.Year, fCalendar.Month);
  end;
end;

procedure TSGJCalendarView.SetGrid(AValue: boolean);
begin
  if fShowGrid <> AValue then
     fShowGrid:=AValue;

  if HandleAllocated then
  if not fShowGrid then fCalendar.GridLineStyle := psClear
  else
    fCalendar.GridLineStyle := psSolid;
end;

procedure TSGJCalendarView.SetVisibleMonthsPreNext(AValue: boolean);
begin
  if fShowMonthNames <> AValue then
     fShowMonthNames:=AValue;

  if HandleAllocated then
  fCalendar.ShowPrevAndNextMonthName := AValue;
  fCalendar.Invalidate;
end;

procedure TSGJCalendarView.SetFocusColor(AValue: TColor);
begin
  if fFocusColor <> AValue then
     fFocusColor:=AValue;

  if HandleAllocated then
    fCalendar.FocusColor := fFocusColor;
end;

procedure TSGJCalendarView.SetColor(AValue: TColor);
begin
  inherited SetColor(AValue);

  if HandleAllocated then
    fCalendar.Color := Color;
end;

procedure TSGJCalendarView.SetFontColor(AValue: TColor);
begin
  if fFontColor <> AValue then
     fFontColor:=AValue;

  Font.Color:=AValue;
end;

procedure TSGJCalendarView.SetTodayColor(AValue: TColor);
begin
  if fCurrentDay <> AValue then
     fCurrentDay:=AValue;

  if HandleAllocated then
  begin
    fCalendar.TodayColor := fCurrentDay;
    fCalendar.invalidate;
  end;
end;

procedure TSGJCalendarView.SetOtherDaysColor(AValue: TColor);
begin
  if fCalendarOtherdays <> AValue then
     fCalendarOtherdays:=AValue;

  if HandleAllocated then
  begin
    fCalendar.DaysNotInMonth := fCalendarOtherdays;
    fCalendar.invalidate;
  end;
end;

procedure TSGJCalendarView.SetDate(AValue: TDateTime);
begin
  if fDate <> AValue then
  begin
    fDate := AValue;
    if HandleAllocated then
    begin
      DecodeDate(fDate, fCalendar.Year, fCalendar.Month, fCalendar.Day);
      fCalendar.DateTime := fdate;
      fCalendar.fDaySelected:=DayOf(fdate);
      if fCalendar.fDaySelected < 1 then fCalendar.fDaySelected := 1;
      init(fDate);

      fCalendar.LoadMonthCalendar(fCalendar.Year, fCalendar.Month);
    end;
  end;
end;

constructor TSGJCalendarView.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  Width:=300;
  Height:=380;

  FS := DefaultFormatSettings;
  FS.DateSeparator := '/';
  FS.ShortDateFormat := 'yyyy/mm/dd';

  DayFromDate:= clBtnFace;
  DaysNotInMonth := clGray;
  FocusColor := clHighlight;
  GridLIneColor := clSilver;
  TodayColor := clHighlight;
  ButtonHoverColor := clNone;
  FontColorButtonHover:=clHighlight;
  Color:=clwindow;

  Date := DateOf(Now);

  //Header
  fHeader := TCustomControl.Create(self);
  fHeader.Parent := self;
  FHeader.Align := alTop;
  FHeader.AutoSize := True;


  //Internal grid - Calendar
  fCalendar := TSGJCalendarV.Create(self);
  fCalendar.ScrollBars := ssNone;
  fCalendar.Parent := self;
  fCalendar.Align := alClient;
  fCalendar.DateTime := date;
  fCalendar.FirstDayOfWeek := FirstDayOfWeek;
  fCalendar.FS := FS;
  fCalendar.fDaySelected:=DayOf(Date);
  fCalendar.TabStop:=true;
  FHeader.TabOrder:=1;

  //Button Header Left - Month/Year
  fBtnLeft := TCalendarButton.Create(self);
  fBtnLeft.Parent := fHeader;
  fBtnLeft.Align := alLeft;
  fBtnLeft.Caption := '';
  fBtnLeft.Constraints.MaxHeight := 45;
  fBtnLeft.BorderSpacing.Around := 5;
  fBtnLeft.Autosize := True;
  fBtnLeft.OnMouseDown := @BtnMouseDown;
  fBtnLeft.OnKeyDown:=@BtnKeyDown;
  fBtnLeft.TabStop:=true;
  FHeader.TabOrder:=0;

  //Button UP
  fUp := TCalendarButton.Create(self);
  fUp.Parent := fHeader;
  fUp.Align := alRight;
  fUp.Caption := '˄';
  fUp.Constraints.MaxHeight := 45;
  fUp.Constraints.MinWidth := 24;
  fUp.AutoSize := True;
  fUp.BorderSpacing.Around := 5;
  fUp.Tag := 1;
  fUp.OnMouseDown := @BtnMouseDown;
  fUp.OnKeyDown:=@BtnKeyDown;
  fUp.TabStop:=true;
  FHeader.TabOrder:=1;

  //Button Down
  fDown := TCalendarButton.Create(self);
  fDown.Parent := fHeader;
  fDown.Align := alRight;
  fDown.Caption := '˅';
  fDown.Constraints.MaxHeight := 45;
  fDown.Constraints.MinWidth := 24;
  fDown.AutoSize := True;
  fDown.BorderSpacing.Around := 5;
  fDown.OnMouseDown := @BtnMouseDown;
  fDown.Tag := 2;
  fDown.OnKeyDown:=@BtnKeyDown;
  fDown.TabStop:=true;
  FHeader.TabOrder:=2;

  Init(Now);
end;

procedure TSGJCalendarView.Loaded;
{$IFDEF LCLGTK2}
var
  Widget: Pointer;
{$ENDIF}
begin
  inherited Loaded;
  Init(Now);
  fCalendar.Loaded;
  fCalendar.FocusColor := FocusColor;
  fCalendar.GridLineColor := GridLineColor;
  if not fShowGrid then fCalendar.GridLineStyle := psClear
  else
    fCalendar.GridLineStyle := psSolid;
  fCalendar.DaysNotInMonth := DaysNotInMonth;
  fCalendar.TodayColor := TodayColor;
  fCalendar.DayFromDate := DayFromDate;
  fCalendar.Color := Color;
  fCalendar.Font.Color := FontColor;
  fCalendar.FirstDayOfWeek := FirstDayOfWeek;
  fCalendar.ShowPrevAndNextMonthName:=ShowPrevAndNextMonthName;
  fCalendar.DateTime:=Date;
  fCalendar.fDaySelected:=DayOf(Date);

  fDown.Font.Color := FontColor;
  fDown.Font.Size := 14;
  fUp.Font.Color := FontColor;
  fUp.Font.Size := 14;
  fBtnLeft.Font.Color := FontColor;
  fBtnLeft.Font.Size := 14;

  {$IFDEF LCLGTK2}
  //Fix for GTK2 - Hide Scroll
  Widget := Pointer(fCalendar.Handle);
  gtk_scrolled_window_set_policy(PGtkScrolledWindow(Widget),
                                   GTK_POLICY_NEVER, GTK_POLICY_NEVER);
  {$ENDIF}
end;

procedure TSGJCalendarView.PaintBtnText(Sender: TObject);
var
  AStyle: TTextStyle;
begin

  AStyle := TCalendarButton(Sender).Canvas.TextStyle;
  AStyle.Alignment := TAlignment.taCenter;
  AStyle.Layout := tlCenter;

  if TCalendarButton(Sender).Tag = 0 then
  begin
    AStyle.Alignment := TAlignment.taLeftJustify;
    TCalendarButton(Sender).Canvas.TextRect(TCalendarButton(Sender).ClientRect,
      4, 0, TCalendarButton(Sender).Caption, AStyle);
  end;
  TCalendarButton(Sender).Canvas.Pen.Style:=psSolid;
  //On Windows 8+ button arrows is painted from fonts
  if TCalendarButton(Sender).Tag > 0 then
  begin
    {$ifdef msWindows}
    if Win32MajorVersion = 10 then
    begin
      if Win32BuildNumber >= 2200 then
        TCalendarButton(Sender).Canvas.Font.Name := 'Segoe Fluent Icons'
      else
        TCalendarButton(Sender).Canvas.Font.Name := 'Segoe MDL2 Assets';

      fUp.Caption := widechar($E010);
      fDown.Caption := widechar($E011);
    end
    else
    if (Win32MajorVersion = 6) and (Win32MinorVersion in [2, 3]) then
    begin
      TCalendarButton(Sender).Canvas.Font.Name := 'Segoe UI Symbol';
      fUp.Caption := widechar($E098);
      fDown.Caption := widechar($E099);
    end;

    if (Win32MajorVersion = 10) or ((Win32MajorVersion = 6) and
      (Win32MinorVersion in [2, 3])) then
    begin
      case TCalendarButton(Sender).Tag of
        1: TCalendarButton(Sender).Canvas.TextRect(TCalendarButton(Sender).ClientRect,
            0, 0, fUp.Caption, AStyle);
        2: TCalendarButton(Sender).Canvas.TextRect(TCalendarButton(Sender).ClientRect,
            0, 0, fDown.Caption, AStyle);
      end;
    end
    else
    {$Endif}//Draw Arrow
      case TCalendarButton(Sender).Tag of
        1: DrawArrowUp(TCalendarButton(Sender).Canvas, TCalendarButton(Sender).Width div
            2, TSpeedButton(Sender).Height div 2, ScaleX(6, 96));
        2: DrawArrowDown(TCalendarButton(Sender).Canvas, TCalendarButton(Sender).Width div
            2, TCalendarButton(Sender).Height div 2, ScaleX(6, 96));
      end;
  end;
end;

procedure TSGJCalendarView.BtnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
begin
  if Key = VK_SPACE then
  BtnMouseDown(TCalendarButton(Sender),mbLeft,[ssLeft],0,0);
end;

procedure TSGJCalendarView.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  s, sd: string;

  procedure SetDays;
  begin
    fCalendar.SetHeaderDate(fCalendar.Year, fCalendar.Month);
  end;

begin
  case TCalendarButton(Sender).Tag of
    0: case fCalendar.fViewStyle of
        cvsMonths: begin
          fCalendar.fViewStyle := cvsYears;
          fCalendar.LoadDecadeCalendar(fCalendar.Year);
          fCalendar.SetHeaderDate(fCalendar.HeaderYear,fCalendar.HeaderMonth);
        end;
        cvsDays: begin
          fCalendar.fViewStyle := cvsMonths;
          fCalendar.LoadYearCalendar();
          fCalendar.SetHeaderDate(fCalendar.HeaderYear,fCalendar.HeaderMonth);
        end;
      end;
    1: case fCalendar.fViewStyle of
        cvsYears: begin
          fCalendar.Year := fCalendar.Year + 4;
          fCalendar.LoadDecadeCalendar(fCalendar.Year);
        end;
        cvsMonths: Dec(fCalendar.Year);
        cvsDays: begin
          if fCalendar.Month = 1 then
          begin
            fCalendar.Month := 12;
            Dec(fCalendar.Year);
          end
          else
            Dec(fCalendar.Month);
          SetDays;
          fCalendar.LoadMonthCalendar(fCalendar.Year, fCalendar.Month);
        end;
      end;
    2: case fCalendar.fViewStyle of
        cvsYears: begin
          fCalendar.Year := fCalendar.Year - 4;
          fCalendar.LoadDecadeCalendar(fCalendar.Year);
        end;
        cvsMonths: Inc(fCalendar.Year);
        cvsDays: begin
          if fCalendar.Month = 12 then
          begin
            fCalendar.Month := 1;
            Inc(fCalendar.Year);
          end
          else
            Inc(fCalendar.Month);
          SetDays;
          fCalendar.LoadMonthCalendar(fCalendar.Year, fCalendar.Month);
        end;
      end;
  end;

  case fCalendar.fViewStyle of
    cvsYears: s := fCalendar.Cells[0, 0] + ' - ' + fCalendar.Cells[3, 3];
    cvsMonths: DateTimeToString(s, 'yyyy', EncodeDate(fCalendar.Year, 1, 1));
    cvsDays: DateTimeToString(s, 'mmmm yyyy',EncodeDate(fCalendar.Year, fCalendar.Month, 1));
  end;
  fBtnLeft.Caption := s;
  fCalendar.FDrawFocus := False;
  fCalendar.Resize;
  fBtnLeft.Invalidate;
end;

//Calendar control - Custom StringGrid
procedure TSGJCalendarV.Init;
begin
  DecodeDate(Date, Year, Month, Day);
  fDaySelected := Day;
  if fDaySelected < 1 then fDaySelected := 1;
  SetHeaderDate(Year, Month);
  LoadMonthCalendar(Year, Month);
end;

constructor TSGJCalendarV.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FixedCols := 0;
  FixedRows := 0;
  BorderStyle := bsNone;
  FocusRectVisible := False;
  GridLineStyle := psClear;
  Options := Options - [goRangeSelect, goRowSelect, goDrawFocusSelected];

  fViewStyle := cvsDays;
  Init;
end;

procedure TSGJCalendarV.Loaded;
begin
  inherited Loaded;
  Init;
end;

procedure TSGJCalendarV.SetHeaderDate(AYear, AMonth: Word);
begin
  HeaderYear := AYear;
  HeaderMonth := AMonth;
end;

function TSGJCalendarV.SelectCell(ACol, ARow: integer): boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  FDrawFocus := True;
end;
//DblClick -> SetDate
procedure TSGJCalendarV.DblClick;
begin
  if fViewStyle = cvsDays then
    TSGJCalendarView(Parent).Date :=
      EncodeDate(Year, Month, StrToInt(Cells[col, row]));
end;

procedure TSGJCalendarV.Click;
var
  i: integer;
  AYear: integer;
begin
  case fViewStyle of
    cvsDays: if (row > 0) then
      begin
        if (row = 1) and (StrToInt(Cells[col, row]) > 20) then
          if Month > 1 then
            Month := Month - 1
          else
          begin
            Year := Year - 1;
            Month := 12;
          end;
        if (row > 4) and (StrToInt(Cells[col, row]) < 20) then
          if Month < 12 then
            Month := Month + 1
          else
          begin
            Year := Year + 1;
            Month := 1;
          end;
        //set date moved to DblClick
      end;

    cvsMonths: begin
      i := Row * 4 + (Col + 1);
      if i < 13 then
        AYear := HeaderYear
      else
      begin
        AYear := HeaderYear + 1;
        i := i - 12;
        HeaderYear := (AYear);
      end;

      TSGJCalendarView(Parent).SetHeader(FormatSettings.LongMonthNames[i] +
        ' ' + InttoStr(HeaderYear));

      SetHeaderDate(AYear, i);
      LoadMonthCalendar(AYear, i);
      Year := AYear;
      Month := i;
      fViewStyle := cvsDays;
      col := 0;
      row := 0;
    end;

    cvsYears: begin
      TSGJCalendarView(Parent).SetHeader(Cells[col, row]);
      HeaderYear := StrToInt((Cells[col, row]));
      LoadYearCalendar();
      fViewStyle := cvsMonths;
    end;
  end;
  Resize;
end;

procedure TSGJCalendarV.Resize;
var
  W, H: integer;
  C: integer = 4;
begin
  inherited Resize;
  //Resize column to fit  grid size
  if fViewStyle = cvsDays then C := 7;

  DefaultColWidth := (Width div C);
  DefaultRowHeight := Height div C;

  w := Width - (DefaultColWidth * C);
  h := Height - (defaultRowHeight * C);
  if rowcount > 1 then
    RowHeights[0] := DefaultRowHeight + h;

  if colcount > 1 then
    ColWidths[0] := DefaultColWidth + w;
end;

procedure TSGJCalendarV.LoadDecadeCalendar(AYear: word);
var
  i, j: integer;
  StartYear: integer;
begin
  beginupdate;
  ColCount := 4;
  RowCount := 4;
  StartYear := AYear - 8;

  for i := 0 to 3 do
    for j := 0 to 3 do
    begin
      Cells[j, i] := IntToStr(StartYear);
      Inc(StartYear);
    end;
  endupdate;
end;

procedure TSGJCalendarV.LoadYearCalendar();
var
  i: integer;
begin
  beginupdate;
  ColCount := 4;
  RowCount := 4;
  for i := 1 to 16 do
    Cells[(i - 1) mod 4, (i - 1) div 4] :=
      FormatSettings.ShortMonthNames[((i - 1) mod 12) + 1];
  endupdate;
end;

procedure TSGJCalendarV.LoadMonthCalendar(AYear, AMonth: Word);
var
  FirstDay: TDateTime;
  StartOffset: Integer;
  TotalDays, PrevMonthDays: Integer;
  PrevMonthDate: TDateTime;
  DayCounter: Integer;
  r, c: Integer;
begin
  //Set params
  DTa := EncodeDate(HeaderYear, HeaderMonth, DayOf(Now));

  if fDaySelected < 1 then fDaySelected:=1;
  DTb := EncodeDate(HeaderYear, HeaderMonth, fDaySelected);

  fDayNow := DayOf(Now);
  fComparedDate := SameDate(DTa, Date);

  fComparedSDate := SameDate(DTb, DateTime);
  if AMonth=12 then NextMonth:=1 else NextMonth:=AMonth+1;
  if AMonth=1 then PreviousMonth:=12 else PreviousMonth:=AMonth-1;
  //
  BeginUpdate;

  ColCount := 7;
  RowCount := 7;

  // Clear grid (rows 1..6)
  for r := 1 to 6 do
    for c := 0 to 6 do
      Cells[c, r] := '';

  // Header row (weekday names)
  for c := 0 to 6 do
  begin
    if FirstDayOfWeek = dwMonday then
    begin
      Cells[c, 0] := FormatSettings.ShortDayNames[((c + 1) mod 7) + 1];
    end
    else
    begin
      Cells[c, 0] := FormatSettings.ShortDayNames[c + 1];
    end;
  end;

  // First day of the month
  FirstDay := EncodeDate(AYear, AMonth, 1);

  // Compute offset (0..6)
  if FirstDayOfWeek = dwMonday then
    StartOffset := DayOfTheWeek(FirstDay) - 1        // Monday=0
  else
    StartOffset := DayOfTheWeek(FirstDay) mod 7;     // Sunday=0

  TotalDays := DaysInAMonth(AYear, AMonth);

  // Previous month
  PrevMonthDate := IncMonth(FirstDay, -1);
  PrevMonthDays := DaysInAMonth(YearOf(PrevMonthDate), MonthOf(PrevMonthDate));

  // Fill previous month trailing days
  if StartOffset > 0 then
    for c := 0 to StartOffset - 1 do
      Cells[c, 1] := IntToStr(PrevMonthDays - StartOffset + c + 1);

  // Fill current month
  DayCounter := 1;
  r := 1;
  c := StartOffset;

  while DayCounter <= TotalDays do
  begin
    Cells[c, r] := IntToStr(DayCounter);

    Inc(DayCounter);
    Inc(c);

    if c > 6 then
    begin
      c := 0;
      Inc(r);
    end;
  end;

  // Fill next month
  DayCounter := 1;

  for r := r to 6 do
    for c := 0 to 6 do
      if Cells[c, r] = '' then
      begin
        Cells[c, r] := IntToStr(DayCounter);
        Inc(DayCounter);
      end;

  EndUpdate;
end;

procedure TSGJCalendarV.DrawCell(aCol, aRow: integer; aRect: TRect;
  aState: TGridDrawState);
var
  S: string;
  CellValue: integer;
begin
  S := Cells[ACol, ARow];
  //Default
  Canvas.Brush.Color := Color;
  if fViewStyle = cvsDays then
  begin
    //Days not in month
    CellValue := StrToIntDef(S, -1);
    if (aRow = 1) and (CellValue > 20) or (aRow >= 5) and (CellValue < 20) then
    Canvas.Brush.Color := DaysNotInMonth
    else
    begin
      //Selected Day from Date
      if (CellValue = fDaySelected) and (fComparedSDate) then
        Canvas.Brush.Color := DayFromDate;
      //Today
      if (CellValue = fDayNow) and (fComparedDate) then
        Canvas.Brush.Color := TodayColor;
    end;
  end;
  //Selected
  if (gdSelected in aState) and FDrawFocus then
    case fViewStyle of
      cvsDays: if (aRow <> 0) then Canvas.Brush.Color := FocusColor;
      cvsYears, cvsMonths: Canvas.Brush.Color := FocusColor;
    end;
  Canvas.FillRect(aRect);

  //Draw Grid
  if GridLineStyle = psSolid then
    if ((aRow > 0) and (fViewStyle = cvsDays)) or
      (fViewStyle in [cvsMonths, cvsYears]) then
    begin
      Canvas.Pen.Color := GridLineColor;
      Canvas.Rectangle(ARect);
    end;
  //Set Font
  if (arow > 0) or (fViewStyle <> cvsDays) then
    Canvas.Font.Size := 14
  else
    Canvas.Font.Size := 0; //default
  //Draw Text
  DrawText(Canvas.Handle, PChar(S), Length(S), aRect, DT_CENTER or
    DT_VCENTER or DT_SINGLELINE);

  if ShowPrevAndNextMonthName and (fViewStyle = cvsDays) then
  begin
  Canvas.Font.Size:=0;
  if (aRow = 1) and (CellValue > 20) and (ACol = 0) then
      Canvas.TextOut(aRect.Left+2,aRect.top+2,FormatSettings.ShortMonthNames[PreviousMonth]);
  if (aRow >=5) and (CellValue < 20) and (CellValue =1) then
      Canvas.TextOut(aRect.Left+2,aRect.top+2,FormatSettings.ShortMonthNames[NextMonth]);
  end;
end;

procedure TSGJCalendarV.DoEnter;
begin
 Canvas.DrawFocusRect(ClientRect);
end;

procedure TSGJCalendarV.DoExit;
begin
  Invalidate;
end;

{$IFDEF FPC}
initialization
  {$I resources/SGJ.CalendarView.lrs}
{$ENDIF}

end.
