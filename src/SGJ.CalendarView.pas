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
  LCLType, LCLIntf, LResources, Classes, Buttons, SysUtils, Grids, Controls,
  DateUtils, StdCtrls, ExtCtrls, Dialogs,
  Graphics;

type
  TSGJCalendarViewStyle = (cvsDays, cvsMonths, cvsYears);
  TSGJCalendarFirstDayOfWeek = (dwSunday, dwMonday);

  TSGJCalendarV = class(TCustomStringGrid)
  private
    DTa, DTb: TDateTime;
    fViewStyle: TSGJCalendarViewStyle;
    fDayNow: integer;
    fDaySelected: integer;
    fComparedDate:boolean;
    fComparedSDate:boolean;
    procedure Init;
  protected
    procedure Loaded; override;
    procedure DrawCell(ACol, ARow: integer; ARect: TRect; AState: TGridDrawState);
      override;
    function SelectCell(ACol, ARow: integer): boolean; override;
    procedure Click; override;
    procedure DblClick; override;
  public
    Year, Month, Day: word;
    HeaderDate: string;
    FDrawFocus: boolean;
    DaysNotInMonth: TColor;
    TodayColor: TColor;
    DayFromDate:TColor;
    DateTime: TDateTime;
    FS: TFormatSettings;
    FirstDayOfWeek: TSGJCalendarFirstDayOfWeek;
    constructor Create(Aowner: TComponent); override;
    procedure LoadMonthCalendar(AYear, AMonth: word);
    procedure LoadYearCalendar();
    procedure LoadDecadeCalendar(AYear: word);
    procedure Resize; override;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property FocusColor;
    property ViewStyle: TSGJCalendarViewStyle read fViewStyle write fViewStyle;
  end;

  TSGJCalendarView = class(TCustomControl)
  private
    fHeader: TCustomControl;
    fCalendar: TSGJCalendarV;
    fBtnLeft: TSpeedButton;
    fUp: TSpeedButton;
    fShowGrid: boolean;
    fGridLineColor: TColor;
    fFocusColor: TColor;
    fCalendarColor: TColor;
    fCalendarOtherdays: TColor;
    fCurrentDay: TColor;
    fSelectedDay:TColor;
    fFontColor: TColor;
    fFontButtonHoverColor: TColor;
    fButtonsHoverColor: TColor;
    fDown: TSpeedButton;
    fDate: TDateTime;
    fFirstDayOfWeek: TSGJCalendarFirstDayOfWeek;
    FS: TFormatSettings;
    procedure PaintBtn(Sender: TObject);
    procedure BtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BtnMouseLeave(Sender: TObject);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBtnText(Sender: TObject);
    procedure SetDate(AValue: TDateTime);
    procedure Init(ADate:TDateTime);
  public
    constructor Create(Aowner: TComponent); override;
    procedure SetHeader(AValue: string);
    procedure Loaded; override;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property Color;
    property Date: TDateTime read fDate write SetDate;
    property ShowGrid: boolean read fShowGrid write fShowGrid;
    property GridLineColor: TColor read fGridLineColor write fGridLineColor;
    property FocusColor: TColor read fFocusColor write fFocusColor;
    property TodayColor: TColor read fCurrentDay write fCurrentDay;
    property DayFromDate: TColor read fSelectedDay write fSelectedDay;
    property DaysNotInMonth: TColor read fCalendarOtherdays write fCalendarOtherdays;
    property FontColor: TColor read fFontColor write fFontColor;
    property FontColorButtonHover: TColor read fFontButtonHoverColor
      write fFontButtonHoverColor;
    property ButtonHoverColor: TColor read fButtonsHoverColor write fButtonsHoverColor;
    property FirstDayOfWeek: TSGJCalendarFirstDayOfWeek
      read fFirstDayOfWeek write fFirstDayOfWeek;
  end;



procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJCalendarView]);
end;

procedure TSGJCalendarView.SetHeader(AValue: string);
begin
  fbtnLeft.Caption := AValue;
end;

procedure TSGJCalendarView.Init(ADate:TDateTime);
var
  s: string;
begin
  DateTimeToString(s, 'mmmm yyyy', ADate);
  fBtnLeft.Caption := s;

  DateTimeToString(s, 'yyyy/mm', ADate, FS);
  fCalendar.HeaderDate := s;
end;

procedure TSGJCalendarView.SetDate(AValue: TDateTime);
begin
  if fDate <> AValue then
  begin
    fDate := AValue;
    if HandleAllocated then
    begin
      DecodeDate(fDate, fCalendar.Year, fCalendar.Month, fCalendar.Day);
      fCalendar.DateTime:=fdate;

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

  Width := 300;
  Height := 400;

  Font.Size := 14;

  FS := DefaultFormatSettings;
  FS.DateSeparator := '/';
  FS.ShortDateFormat := 'yyyy/mm/dd';

  DaysNotInMonth := clGray;
  FocusColor := clHighlight;
  GridLIneColor := clSilver;
  TodayColor := clHighlight;
  ButtonHoverColor := clNone;

  Date := DateOf(Now);

  fHeader := TCustomControl.Create(self);
  fHeader.Parent := self;
  FHeader.Align := alTop;
  FHeader.AutoSize := True;


  fCalendar := TSGJCalendarV.Create(self);
  fCalendar.Parent := self;
  fCalendar.Align := alClient;
  fCalendar.DateTime := date;
  fCalendar.FirstDayOfWeek := FirstDayOfWeek;
  fCalendar.FS := FS;

  fBtnLeft := TSpeedButton.Create(fHeader);
  fBtnLeft.Parent := fHeader;
  fBtnLeft.Align := alLeft;
  fBtnLeft.Caption := '';
  fBtnLeft.Constraints.MaxHeight := ScaleX(45, 96);
  fBtnLeft.BorderSpacing.Around := 5;
  fBtnLeft.Autosize := True;
  fBtnLeft.OnPaint := @PaintBtn;
  fBtnLeft.Flat := True;
  fBtnLeft.OnMouseMove := @BtnMouseMove;
  fBtnLeft.OnMouseLeave := @BtnMouseLeave;
  fBtnLeft.OnMouseDown := @BtnMouseDown;

  fUp := TSpeedButton.Create(fHeader);
  fUp.Parent := fHeader;
  fUp.Align := alRight;
  fUp.Caption := '˄';
  fUp.Constraints.MaxHeight := ScaleX(45, 96);
  fUp.AutoSize := True;
  fUp.BorderSpacing.Around := 5;
  fUp.Flat := True;
  fUp.Tag := 1;
  fUp.OnPaint := @PaintBtn;
  fUp.OnMouseMove := @BtnMouseMove;
  fUp.OnMouseLeave := @BtnMouseLeave;
  fUp.OnMouseDown := @BtnMouseDown;

  fDown := TSpeedButton.Create(fHeader);
  fDown.Parent := fHeader;
  fDown.Align := alRight;
  fDown.Caption := '˅';
  fDown.Constraints.MaxHeight := ScaleX(45, 96);
  fDown.AutoSize := True;
  fDown.BorderSpacing.Around := 5;
  fDown.Flat := True;
  fDown.OnPaint := @PaintBtn;
  fDown.OnMouseMove := @BtnMouseMove;
  fDown.OnMouseLeave := @BtnMouseLeave;
  fDown.OnMouseDown := @BtnMouseDown;
  fDown.Tag := 2;

  Init(Now);
end;

procedure TSGJCalendarView.Loaded;

begin
  inherited Loaded;
  Init(Now);

  fCalendar.FocusColor := FocusColor;
  fCalendar.GridLineColor := GridLineColor;
  if not ShowGrid then fCalendar.GridLineStyle := psClear
  else
    fCalendar.GridLineStyle := psSolid;
  fCalendar.DaysNotInMonth := DaysNotInMonth;
  fCalendar.TodayColor := TodayColor;
  fCalendar.DayFromDate:=DayFromDate;
  fCalendar.Color := Color;
  fCalendar.Font.Color := FontColor;
  fCalendar.FirstDayOfWeek := FirstDayOfWeek;

  fCalendar.Loaded;

  fDown.Font.Color := FontColor;
  fUp.Font.Color := FontColor;
  fBtnLeft.Font.Color := FontColor;
end;

procedure TSGJCalendarView.PaintBtnText(Sender: TObject);
var
  AStyle: TTextStyle;
begin

  AStyle := TSpeedButton(Sender).Canvas.TextStyle;
  AStyle.Alignment := TAlignment.taCenter;
  AStyle.Layout := tlCenter;

  if TSpeedButton(Sender).Tag = 0 then
  begin
    AStyle.Alignment := TAlignment.taLeftJustify;
    TSpeedButton(Sender).Canvas.TextRect(TSpeedButton(Sender).ClientRect,
      0, 0, TSpeedButton(Sender).Caption, AStyle);
  end;

  if TSpeedButton(Sender).Tag > 0 then
  begin
    {$ifdef msWindows}
    if Win32MajorVersion = 10 then
    begin
      if Win32BuildNumber >= 2200 then
        TSpeedButton(Sender).Canvas.Font.Name := 'Segoe Fluent Icons'
      else
        TSpeedButton(Sender).Canvas.Font.Name := 'Segoe MDL2 Assets';

      fUp.Caption := widechar($E010);
      fDown.Caption := widechar($E011);
    end
    else
    if (Win32MajorVersion = 6) and (Win32MinorVersion in [2, 3]) then
    begin
      TSpeedButton(Sender).Canvas.Font.Name := 'Segoe UI Symbol';
      fUp.Caption := widechar($E098);
      fDown.Caption := widechar($E099);
    end;
    {$Endif}
    case TSpeedButton(Sender).Tag of
      1: TSpeedButton(Sender).Canvas.TextRect(TSpeedButton(Sender).ClientRect,
          0, 0, fUp.Caption, AStyle);
      2: TSpeedButton(Sender).Canvas.TextRect(TSpeedButton(Sender).ClientRect,
          0, 0, fDown.Caption, AStyle);
    end;
  end;
end;

procedure TSGJCalendarView.PaintBtn(Sender: TObject);
begin
  TSpeedButton(Sender).Canvas.Brush.Color := Color;
  TSpeedButton(Sender).Canvas.FillRect(TSpeedButton(Sender).ClientRect);

  PaintBtnText(Sender);
end;

procedure TSGJCalendarView.BtnMouseLeave(Sender: TObject);
begin
  TSpeedButton(Sender).Canvas.Font.Color := FontColor;
end;

procedure TSGJCalendarView.BtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  s, sd: string;
  procedure SetDays;
  begin
    DateTimeToString(s, 'mmmm yyyy',
      EncodeDate(fCalendar.Year, fCalendar.Month, 1));
    DateTimeToString(sd, 'yyyy/mm',
      EncodeDate(fCalendar.Year, fCalendar.Month, 1), FS);
    fCalendar.HeaderDate := sd;
    fCalendar.DTa :=
      StrToDateTime(fCalendar.HeaderDate + '/' + IntToStr(DayOf(Now)), FS);
  end;
begin

  case TSpeedButton(Sender).Tag of
    0: case fCalendar.fViewStyle of
        cvsMonths: begin
          fCalendar.fViewStyle := cvsYears;
          fCalendar.LoadDecadeCalendar(fCalendar.Year);
          fCalendar.HeaderDate := TSpeedButton(Sender).Caption;
        end;
        cvsDays: begin
          fCalendar.fViewStyle := cvsMonths;
          fCalendar.LoadYearCalendar();
          fCalendar.HeaderDate := RightStr(TSpeedButton(Sender).Caption, 4);
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
  end;
  fBtnLeft.Caption := s;
  fCalendar.FDrawFocus := False;
end;

procedure TSGJCalendarView.BtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
begin
  TSpeedButton(Sender).Canvas.Font.Color := FontColorButtonHover;
  TSpeedButton(Sender).Canvas.Brush.Color := ButtonHoverColor;
  TSpeedButton(Sender).Canvas.FillRect(TSpeedButton(Sender).ClientRect);
  PaintBtnText(Sender);
end;

procedure TSGJCalendarV.Init;
begin
  DecodeDate(Date, Year, Month, Day);
  LoadMonthCalendar(Year, Month);
end;

constructor TSGJCalendarV.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FixedCols := 0;
  FixedRows := 0;
  ScrollBars := ssNone;
  BorderStyle := bsNone;
  FocusRectVisible := False;
  Options := Options - [goRangeSelect, goRowSelect, goDrawFocusSelected];
  fViewStyle := cvsDays;
  Init;
end;

procedure TSGJCalendarV.Loaded;
begin
  inherited Loaded;
  Init;
end;

function TSGJCalendarV.SelectCell(ACol, ARow: integer): boolean;
begin
  Result := inherited SelectCell(ACol, ARow);
  FDrawFocus := True;
end;

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
        AYear := StrToInt(HeaderDate)
      else
      begin
        AYear := StrToInt(HeaderDate) + 1;
        i := i - 12;
        HeaderDate := IntToStr(AYear);
      end;

      TSGJCalendarView(Parent).SetHeader(FormatSettings.LongMonthNames[i] +
        ' ' + HeaderDate);

      HeaderDate := HeaderDate + '/' + IntToStr(i);
      LoadMonthCalendar(AYear, i);
      Year := AYear;
      Month := i;
      fViewStyle := cvsDays;
      col := 0;
      row := 0;
    end;

    cvsYears: begin
      TSGJCalendarView(Parent).SetHeader(Cells[col, row]);
      HeaderDate := (Cells[col, row]);
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

procedure TSGJCalendarV.LoadMonthCalendar(AYear, AMonth: word);
var
  FirstDay, PrevMonthDate, NextMonthDate: TDateTime;
  StartDayOfWeek, StartDay: integer;
  TotalDays, PrevMonthDays: integer;
  DayCounter: integer;
  aRow, aCol: integer;
  i: integer;
begin
  if HeaderDate = '' then DTA := now
  else
   DTa := StrToDateTime(HeaderDate + '/' + IntToStr(DayOf(date)), FS);
  fDayNow := DayOf(Now);
  fComparedDate:=SameDate(DTa, Date);
  if HeaderDate = '' then DTb := DateTime
  else
  DTb := StrToDateTime(HeaderDate + '/' + IntToStr(DayOf(DateTime)), FS);
  fDaySelected:= DayOf(DateTime);
  fComparedSDate:=SameDate(DTb, DateTime);

//  beginupdate;
  // Configure grid
  ColCount := 7;   // Header - Week Days
  RowCount := 7;   // 6 Weeks + Header


  // Clear grid
  for aRow := 1 to RowCount - 1 do
    for aCol := 0 to ColCount - 1 do
      Cells[aCol, aRow] := '';

  // Headers
  for i := 0 to 6 do
  begin
    if FirstDayOfWeek = dwSunday then
      Cells[i, 0] := FormatSettings.ShortDayNames[i + 1];
    if FirstDayOfWeek = dwMonday then
    begin
      Cells[i, 0] := FormatSettings.ShortDayNames[i + 2];
      if i = 6 then Cells[i, 0] := FormatSettings.ShortDayNames[1];
    end;
  end;

  // First day of this month
  FirstDay := EncodeDate(AYear, AMonth, 1);

  // Monday=1..Sunday=7 → convert to 0..6
  if FirstDayOfWeek = dwSunday then StartDay := 7;
  if FirstDayOfWeek = dwMonday then StartDay := 1;
  //StartDay:=1;
  StartDayOfWeek := DayOfTheWeek(FirstDay) - StartDay;
  if StartDayOfWeek < 0 then
    StartDayOfWeek := 0;

  TotalDays := DaysInAMonth(AYear, AMonth);

  // Previous month info
  PrevMonthDate := IncMonth(FirstDay, -1);
  PrevMonthDays := DaysInAMonth(YearOf(PrevMonthDate), MonthOf(PrevMonthDate));

  // Fill previous month's trailing days
  aRow := 1;
  aCol := 0;

  if StartDayOfWeek > 0 then
  begin
    // Start from the last needed day of previous month
    aCol := 0;
    while aCol < StartDayOfWeek do
    begin
      Cells[aCol, aRow] :=
        IntToStr(PrevMonthDays - (StartDayOfWeek - aCol) + 1);
      Inc(aCol);
    end;
  end;

  // Fill current month
  DayCounter := 1;
  aRow := 1;
  aCol := StartDayOfWeek;

  while DayCounter <= TotalDays do
  begin
    Cells[aCol, aRow] := IntToStr(DayCounter);

    Inc(DayCounter);
    Inc(aCol);

    if aCol > 6 then
    begin
      aCol := 0;
      Inc(aRow);
    end;
  end;

  // Fill next month
  NextMonthDate := IncMonth(FirstDay, 1);
  DayCounter := 1;

  while (aRow < RowCount) do
  begin
    while (aCol < 7) do
    begin
      if Cells[aCol, aRow] = '' then
        Cells[aCol, aRow] := IntToStr(DayCounter)
      else
      begin
        Inc(aCol);
        Continue;
      end;

      Inc(DayCounter);
      Inc(aCol);
    end;

    aCol := 0;
    Inc(aRow);
  end;
//  endupdate;
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
  //Selected
  if (gdSelected in aState) and FDrawFocus
  then
    case fViewStyle of
        cvsDays: if (aRow <> 0) then Canvas.Brush.Color := FocusColor;
        cvsYears,cvsMonths: Canvas.Brush.Color := FocusColor;
    end;

  if fViewStyle = cvsDays then
  begin
      //Days not in mounth
      CellValue := StrToIntDef(S, -1);
      if (aRow = 1) and (CellValue > 20) or
         (aRow >= 5) and (CellValue < 20) then
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
    Canvas.Font.Size := ScaleX(14, 96)
  else
    Canvas.Font.Size := 0;
  //Draw Text
  DrawText(Canvas.Handle, PChar(S), Length(S), aRect, DT_CENTER or
    DT_VCENTER or DT_SINGLELINE);
end;
{$IFDEF FPC}
initialization
  {$I resources/SGJ.CalendarView.lrs}
{$ENDIF}
end.
