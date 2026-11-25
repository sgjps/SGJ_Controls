unit SGJ.PersonPicture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls, BGRABitmap, BGRABitmapTypes,
  BGRAGraphics, Math;

type
  TSGJPictureDisplayMode = (pdmPicture, pdmInitials);

  TSGJPersonPicture = class(TCustomImage)
  private
    fDisplayMode: TSGJPictureDisplayMode;
    fInitials: string;
    fInitialsBackground: TColor;
    fInitialsFontColor: TColor;
    fBorderColor: TColor;
    fBorderWidth: integer;
    fImageFileName: TFileName;
    fDisplayName: string;
    fRuntimeBitmap: TBGRABitmap;
    procedure SetDisplayMode(AValue: TSGJPictureDisplayMode);
    procedure SetInitials(AValue: string);
    procedure SetInitialsBackground(AValue: TColor);
    procedure SetInitialsFontColor(AValue: TColor);
    procedure SetBorderColor(AValue: TColor);
    procedure SetBorderWidth(AValue: integer; AUpdate: boolean = True);
    procedure SetImageFileName(AValue: TFileName);
    procedure SetDisplayName(AValue: string);
    function ExtractInitialsFromDisplayName(const AName: string): string;
    procedure RedrawControl;
    procedure UpdateFromColorProperty;
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadPictureFromFile(const FileName: string);
    procedure LoadPictureFromBitmap(ABitmap: TBitmap);
    procedure LoadPictureFromImage(ASourceImage: TImage);
    procedure ClearPicture;
  published
    property DisplayName: string read fDisplayName write SetDisplayName;
    property ImageFileName: TFileName read fImageFileName write SetImageFileName;
    property DisplayMode: TSGJPictureDisplayMode
      read fDisplayMode write SetDisplayMode default pdmInitials;
    property Initials: string read fInitials write SetInitials;
    property InitialsBackground: TColor read fInitialsBackground
      write SetInitialsBackground default $00D8A200;
    property InitialsFontColor: TColor read fInitialsFontColor
      write SetInitialsFontColor default clDefault;

    property BorderColor: TColor read fBorderColor write SetBorderColor default
      $00272826;
    property BorderWidth: integer read fBorderWidth write SetBorderWidth default 3;

    property Align;
    property Anchors;
    property BorderSpacing;
    property Color default clBtnFace;
    property Constraints;
    property Enabled;
    property Font;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SGJ', [TSGJPersonPicture]);
end;

{ TSGJPersonPicture }

constructor TSGJPersonPicture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Stretch := False;
  Proportional := False;
  Center := False;

  Width := 64;
  Height := 64;
  Color := clBtnFace;

  fDisplayMode := pdmInitials;
  fInitials := 'AA';
  fDisplayName := '';
  fInitialsBackground := $00D8A200;
  fInitialsFontColor := clDefault;
  fBorderColor := $00272826;
  fBorderWidth := 3;
  fImageFileName := '';
  fRuntimeBitmap := nil;
end;

destructor TSGJPersonPicture.Destroy;
begin
  if Assigned(fRuntimeBitmap) then fRuntimeBitmap.Free;
  inherited Destroy;
end;

procedure TSGJPersonPicture.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if Assigned(AParent) then UpdateFromColorProperty;
end;

procedure TSGJPersonPicture.Resize;
begin
  inherited Resize;
  RedrawControl;
end;

procedure TSGJPersonPicture.UpdateFromColorProperty;
begin
  RedrawControl;
end;

procedure TSGJPersonPicture.SetImageFileName(AValue: TFileName);
begin
  if fImageFileName = AValue then Exit;
  fImageFileName := AValue;
  if Assigned(fRuntimeBitmap) then
  begin
    fRuntimeBitmap.Free;
    fRuntimeBitmap := nil;
  end;
  RedrawControl;
end;

procedure TSGJPersonPicture.LoadPictureFromFile(const FileName: string);
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    fImageFileName := FileName;
    if Assigned(fRuntimeBitmap) then
    begin
      fRuntimeBitmap.Free;
      fRuntimeBitmap := nil;
    end;
    fDisplayMode := pdmPicture;
    RedrawControl;
  end;
end;

procedure TSGJPersonPicture.LoadPictureFromBitmap(ABitmap: TBitmap);
begin
  if not Assigned(ABitmap) or ABitmap.Empty then Exit;

  fImageFileName := '';
  if Assigned(fRuntimeBitmap) then fRuntimeBitmap.Free;

  fRuntimeBitmap := TBGRABitmap.Create(ABitmap);
  fDisplayMode := pdmPicture;
  RedrawControl;
end;

procedure TSGJPersonPicture.LoadPictureFromImage(ASourceImage: TImage);
var
  TmpBmp: TBitmap;
begin
  if not Assigned(ASourceImage) or not Assigned(ASourceImage.Picture) then Exit;

  fImageFileName := '';
  if Assigned(fRuntimeBitmap) then fRuntimeBitmap.Free;
  fRuntimeBitmap := nil;

  if Assigned(ASourceImage.Picture.Bitmap) and not ASourceImage.Picture.Bitmap.Empty then
  begin
    fRuntimeBitmap := TBGRABitmap.Create(ASourceImage.Picture.Bitmap);
  end
  else if Assigned(ASourceImage.Picture.Graphic) and not
    ASourceImage.Picture.Graphic.Empty then
  begin
    TmpBmp := TBitmap.Create;
    try
      TmpBmp.Assign(ASourceImage.Picture.Graphic);
      fRuntimeBitmap := TBGRABitmap.Create(TmpBmp);
    finally
      TmpBmp.Free;
    end;
  end;

  if Assigned(fRuntimeBitmap) then
  begin
    fDisplayMode := pdmPicture;
    RedrawControl;
  end;
end;

function TSGJPersonPicture.ExtractInitialsFromDisplayName(const AName: string): string;
var
  Words: TStringList;
  CleanName: string;
  Idx: integer;
begin
  Result := '';
  CleanName := Trim(AName);
  if CleanName = '' then Exit;

  Words := TStringList.Create;
  try
    Words.Delimiter := ' ';
    Words.StrictDelimiter := True;
    Words.DelimitedText := CleanName;

    Idx := Words.Count - 1;
    while Idx >= 0 do
    begin
      if Trim(Words[Idx]) = '' then Words.Delete(Idx);
      Dec(Idx);
    end;

    if Words.Count = 0 then Exit;

    if Words.Count = 1 then
    begin
      Result := UpperCase(Copy(Words[0], 1, 1));
    end
    else
    begin
      Result := UpperCase(Copy(Words[0], 1, 1) + Copy(Words[Words.Count - 1], 1, 1));
    end;
  finally
    Words.Free;
  end;
end;

procedure TSGJPersonPicture.SetDisplayName(AValue: string);
begin
  if fDisplayName = AValue then Exit;
  fDisplayName := AValue;

  if Trim(fDisplayName) <> '' then
  begin
    fInitials := ExtractInitialsFromDisplayName(fDisplayName);
  end;

  RedrawControl;
end;

procedure TSGJPersonPicture.ClearPicture;
begin
  fImageFileName := '';
  if Assigned(fRuntimeBitmap) then
  begin
    fRuntimeBitmap.Free;
    fRuntimeBitmap := nil;
  end;
  fDisplayMode := pdmInitials;
  RedrawControl;
end;

procedure TSGJPersonPicture.RedrawControl;
var
  src, output: TBGRABitmap;
  targetW, targetH: integer;
  x, y: integer;
  dx, dy, d, R, CX, CY: single;
  srcX, srcY: integer;
  alpha: single;
  c: TBGRAPixel;
  TextSize: TSize;
  FinalFontColor: TColor;
  Luminance: double;
  RGBColor: longint;
  Padding: single;
  BackgroundPixel: TBGRAPixel;
  HasValidFile: boolean;
  ShouldDrawPicture: boolean;
begin
  targetW := Width;
  targetH := Height;
  if (targetW = 0) or (targetH = 0) then Exit;

  CX := targetW / 2;
  CY := targetH / 2;

  Padding := (fBorderWidth / 2) + 0.5;
  R := (Min(targetW, targetH) / 2) - Padding;

  BackgroundPixel := ColorToBGRA(ColorToRGB(Self.Color));
  HasValidFile := (fImageFileName <> '') and FileExists(fImageFileName);

  ShouldDrawPicture := (fDisplayMode = pdmPicture) and
    (HasValidFile or Assigned(fRuntimeBitmap));

  if ShouldDrawPicture then
  begin
    if HasValidFile then
      src := TBGRABitmap.Create(fImageFileName)
    else
      src := TBGRABitmap.Create(fRuntimeBitmap);

    output := TBGRABitmap.Create(targetW, targetH, BackgroundPixel);
    try
      for y := 0 to targetH - 1 do
        for x := 0 to targetW - 1 do
        begin
          dx := x + 0.5 - CX;
          dy := y + 0.5 - CY;
          d := Sqrt(dx * dx + dy * dy);

          if d >= R then Continue;

          if d <= R - 1.0 then alpha := 1
          else
            alpha := (R - d);

          if alpha < 0 then alpha := 0;
          if alpha > 1 then alpha := 1;

          srcX := Round((dx / R + 1) * 0.5 * (src.Width - 1));
          srcY := Round((dy / R + 1) * 0.5 * (src.Height - 1));

          if (srcX >= 0) and (srcX < src.Width) and (srcY >= 0) and
            (srcY < src.Height) then
          begin
            c := src.GetPixel(srcX, srcY);
            c.alpha := Round(alpha * 255);
            output.SetPixel(x, y, c);
          end;
        end;

      output.EllipseAntialias(CX, CY, R, R, ColorToBGRA(ColorToRGB(fBorderColor)),
        fBorderWidth);

      Self.Picture.Bitmap.SetSize(targetW, targetH);
      output.Draw(Self.Picture.Bitmap.Canvas, 0, 0, True);
    finally
      src.Free;
      output.Free;
    end;
  end
  else
  begin
    output := TBGRABitmap.Create(targetW, targetH, BackgroundPixel);
    try
      output.FillEllipseAntialias(CX, CY, R, R, ColorToBGRA(fInitialsBackground));

      if fInitials <> '' then
      begin
        if fInitialsFontColor = clDefault then
        begin
          RGBColor := ColorToRGB(fInitialsBackground);
          Luminance := (0.299 * Red(RGBColor) + 0.587 * Green(RGBColor) +
            0.114 * Blue(RGBColor)) / 255;
          if Luminance > 0.5 then FinalFontColor := clBlack
          else
            FinalFontColor := clWhite;
        end
        else
          FinalFontColor := fInitialsFontColor;

        output.FontName := Font.Name;
        output.FontStyle := Font.Style;
        output.FontHeight := Round(Min(targetW, targetH) * 0.4);
        TextSize := output.TextSize(fInitials);
        output.TextOut(Round(CX - (TextSize.cx / 2)), Round(CY - (TextSize.cy / 2)),
          fInitials, ColorToBGRA(FinalFontColor));
      end;
      output.EllipseAntialias(CX, CY, R, R, ColorToBGRA(ColorToRGB(fBorderColor)),
        fBorderWidth);
      Self.Picture.Bitmap.SetSize(targetW, targetH);
      output.Draw(Self.Picture.Bitmap.Canvas, 0, 0, True);
    finally
      output.Free;
    end;
  end;
end;

procedure TSGJPersonPicture.SetDisplayMode(AValue: TSGJPictureDisplayMode);
begin
  if fDisplayMode = AValue then Exit;
  fDisplayMode := AValue;
  RedrawControl;
end;

procedure TSGJPersonPicture.SetInitials(AValue: string);
begin
  if fInitials = AValue then Exit;
  fInitials := UpperCase(Copy(AValue, 1, 3));
  RedrawControl;
end;

procedure TSGJPersonPicture.SetInitialsBackground(AValue: TColor);
begin
  if fInitialsBackground = AValue then Exit;
  fInitialsBackground := AValue;
  RedrawControl;
end;

procedure TSGJPersonPicture.SetInitialsFontColor(AValue: TColor);
begin
  if fInitialsFontColor = AValue then Exit;
  fInitialsFontColor := AValue;
  RedrawControl;
end;

procedure TSGJPersonPicture.SetBorderColor(AValue: TColor);
begin
  if fBorderColor = AValue then Exit;
  fBorderColor := AValue;
  RedrawControl;
end;

procedure TSGJPersonPicture.SetBorderWidth(AValue: integer; AUpdate: boolean);
begin
  if fBorderWidth = AValue then Exit;
  fBorderWidth := AValue;
  if AUpdate then RedrawControl;
end;

end.
