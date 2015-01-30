unit SynVCLStyleHooks;

//This code is based on the scrollbar hook from the Virtual Treeview Project.
//https://github.com/Virtual-TreeView/Virtual-TreeView

interface

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,

  System.Classes,
  Vcl.Themes,
  Vcl.Controls;

const
  CM_UPDATE_VCLSTYLE_SCROLLBARS = CM_BASE + 2050;

type
  // XE2+ VCL Style
  TSynEditVclStyleScrollBarsHook = class(TMouseTrackControlStyleHook)
  strict private type
  {$REGION 'TVclStyleScrollBarWindow'}
      TVclStyleScrollBarWindow = class(TWinControl)strict private FScrollBarWindowOwner: TSynEditVclStyleScrollBarsHook;
    FScrollBarVertical: Boolean;
    FScrollBarVisible: Boolean;
    FScrollBarEnabled: Boolean;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
  strict protected
    procedure CreateParams(var Params: TCreateParams);
    override;
  public
    constructor Create(AOwner: TComponent);
    override;
    property ScrollBarWindowOwner: TSynEditVclStyleScrollBarsHook read FScrollBarWindowOwner write FScrollBarWindowOwner;
    property ScrollBarVertical: Boolean read FScrollBarVertical write FScrollBarVertical;
    property ScrollBarVisible: Boolean read FScrollBarVisible write FScrollBarVisible;
    property ScrollBarEnabled: Boolean read FScrollBarEnabled write FScrollBarEnabled;
    end;
  {$ENDREGION}
  private
    FHorzScrollBarDownButtonRect: TRect;
    FHorzScrollBarDownButtonState: TThemedScrollBar;
    FHorzScrollBarRect: TRect;
    FHorzScrollBarSliderState: TThemedScrollBar;
    FHorzScrollBarSliderTrackRect: TRect;
    FHorzScrollBarUpButtonRect: TRect;
    FHorzScrollBarUpButtonState: TThemedScrollBar;
    FHorzScrollBarWindow: TVclStyleScrollBarWindow;
    FLeftMouseButtonDown: Boolean;
    FPrevScrollPos: Integer;
    FScrollPos: Single;
    FVertScrollBarDownButtonRect: TRect;
    FVertScrollBarDownButtonState: TThemedScrollBar;
    FVertScrollBarRect: TRect;
    FVertScrollBarSliderState: TThemedScrollBar;
    FVertScrollBarSliderTrackRect: TRect;
    FVertScrollBarUpButtonRect: TRect;
    FVertScrollBarUpButtonState: TThemedScrollBar;
    FVertScrollBarWindow: TVclStyleScrollBarWindow;

    procedure CMUpdateVclStyleScrollbars(var Message: TMessage); message CM_UPDATE_VCLSTYLE_SCROLLBARS;
    procedure WMKeyDown(var Msg: TMessage); message WM_KEYDOWN;
    procedure WMKeyUp(var Msg: TMessage); message WM_KEYUP;
    procedure WMLButtonDown(var Msg: TWMMouse);  message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMNCLButtonDown(var Msg: TWMMouse); message WM_NCLBUTTONDOWN;
    procedure WMNCMouseMove(var Msg: TWMMouse); message WM_NCMOUSEMOVE;
    procedure WMNCLButtonUp(var Msg: TWMMouse); message WM_NCLBUTTONUP;
    procedure WMNCPaint(var Msg: TMessage); message WM_NCPAINT;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
    procedure WMHScroll(var Msg: TMessage); message WM_HSCROLL;
    procedure WMCaptureChanged(var Msg: TMessage); message WM_CAPTURECHANGED;
    procedure WMNCLButtonDblClk(var Msg: TWMMouse); message WM_NCLBUTTONDBLCLK;
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
    procedure WMMove(var Msg: TMessage); message WM_MOVE;
    procedure WMPosChanged(var Msg: TMessage); message WM_WINDOWPOSCHANGED;
  protected
    procedure CalcScrollBarsRect; virtual;
    procedure DrawHorzScrollBar(DC: HDC); virtual;
    procedure DrawVertScrollBar(DC: HDC); virtual;
    function GetHorzScrollBarSliderRect: TRect;
    function GetVertScrollBarSliderRect: TRect;
    procedure MouseLeave; override;
    procedure PaintScrollBars; virtual;
    procedure UpdateScrollBarWindow;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;


implementation

uses
  System.SysUtils,
  System.Math,
  System.Types,
  Vcl.Graphics,
  SynEdit;

type
  TSynEditCracker = class(TSynEdit)
  end;


// XE2+ VCL Style
{ TVclStyleScrollBarsHook }

procedure TSynEditVclStyleScrollBarsHook.CalcScrollBarsRect;
var
  P: TPoint;
  BorderValue: TSize;
  BarInfo: TScrollBarInfo;
  I: Integer;

  procedure CalcVerticalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    FVertScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FVertScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FVertScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FVertScrollBarRect.BottomRight := P;
      OffsetRect(FVertScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CYVTHUMB);
      // Down Button
      FVertScrollBarDownButtonRect := FVertScrollBarRect;
      FVertScrollBarDownButtonRect.Top :=
        FVertScrollBarDownButtonRect.Bottom - I;

      // UP Button
      FVertScrollBarUpButtonRect := FVertScrollBarRect;
      FVertScrollBarUpButtonRect.Bottom := FVertScrollBarUpButtonRect.Top + I;

      FVertScrollBarSliderTrackRect := FVertScrollBarRect;
      Inc(FVertScrollBarSliderTrackRect.Top, I);
      Dec(FVertScrollBarSliderTrackRect.Bottom, I);
    end;
  end;

  procedure CalcHorizontalRects;
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    FHorzScrollBarWindow.Visible :=
      not(STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0);
    FHorzScrollBarWindow.Enabled :=
      not(STATE_SYSTEM_UNAVAILABLE and BarInfo.rgstate[0] <> 0);
    if FHorzScrollBarWindow.Visible then
    begin
      // ScrollBar Rect
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      FHorzScrollBarRect.BottomRight := P;
      OffsetRect(FHorzScrollBarRect, BorderValue.cx, BorderValue.cy);

      I := GetSystemMetrics(SM_CXHTHUMB);
      // Down Button
      FHorzScrollBarDownButtonRect := FHorzScrollBarRect;
      FHorzScrollBarDownButtonRect.Left :=
        FHorzScrollBarDownButtonRect.Right - I;

      // UP Button
      FHorzScrollBarUpButtonRect := FHorzScrollBarRect;
      FHorzScrollBarUpButtonRect.Right := FHorzScrollBarUpButtonRect.Left + I;

      FHorzScrollBarSliderTrackRect := FHorzScrollBarRect;
      Inc(FHorzScrollBarSliderTrackRect.Left, I);
      Dec(FHorzScrollBarSliderTrackRect.Right, I);
    end;
  end;

begin
  BorderValue.cx := 0;
  BorderValue.cy := 0;
  if HasBorder then
    if HasClientEdge then
    begin
      BorderValue.cx := GetSystemMetrics(SM_CXEDGE);
      BorderValue.cy := GetSystemMetrics(SM_CYEDGE);
    end;
  CalcVerticalRects;
  CalcHorizontalRects;

end;

constructor TSynEditVclStyleScrollBarsHook.Create(AControl: TWinControl);
begin
  inherited;
  FVertScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FVertScrollBarWindow.ScrollBarWindowOwner := Self;
  FVertScrollBarWindow.ScrollBarVertical := True;

  FHorzScrollBarWindow := TVclStyleScrollBarWindow.CreateParented
    (GetParent(Control.Handle));
  FHorzScrollBarWindow.ScrollBarWindowOwner := Self;

  FVertScrollBarSliderState := tsThumbBtnVertNormal;
  FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
  FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
  FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
  FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
  FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
end;

destructor TSynEditVclStyleScrollBarsHook.Destroy;
begin
  FVertScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FVertScrollBarWindow);
  FHorzScrollBarWindow.ScrollBarWindowOwner := nil;
  FreeAndNil(FHorzScrollBarWindow);
  inherited;
end;

procedure TSynEditVclStyleScrollBarsHook.DrawHorzScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FHorzScrollBarWindow.Visible and StyleServices.Available and (seBorder in TSynEdit(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := FHorzScrollBarRect.Width;
      B.Height := FHorzScrollBarRect.Height;
      MoveWindowOrg(B.Canvas.Handle, -FHorzScrollBarRect.Left,
        -FHorzScrollBarRect.Top);
      R := FHorzScrollBarRect;
      R.Left := FHorzScrollBarUpButtonRect.Right;
      R.Right := FHorzScrollBarDownButtonRect.Left;

      Details := StyleServices.GetElementDetails(tsUpperTrackHorzNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        GetHorzScrollBarSliderRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FHorzScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FHorzScrollBarUpButtonRect);

      if FHorzScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (FHorzScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FHorzScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FHorzScrollBarRect.Left,
        FHorzScrollBarRect.Top);
      with FHorzScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height, B.Canvas.Handle, 0, 0,
          SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

procedure TSynEditVclStyleScrollBarsHook.DrawVertScrollBar(DC: HDC);
var
  B: TBitmap;
  Details: TThemedElementDetails;
  R: TRect;
begin
  if ((Handle = 0) or (DC = 0)) then
    Exit;
  if FVertScrollBarWindow.Visible and StyleServices.Available and
    (seBorder in TSynEdit(Control).StyleElements) then
  begin
    B := TBitmap.Create;
    try
      B.Width := FVertScrollBarRect.Width;
      B.Height := FVertScrollBarWindow.Height;
      MoveWindowOrg(B.Canvas.Handle, -FVertScrollBarRect.Left,
        -FVertScrollBarRect.Top);
      R := FVertScrollBarRect;
      R.Bottom := B.Height + FVertScrollBarRect.Top;
      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);
      R.Top := FVertScrollBarUpButtonRect.Bottom;
      R.Bottom := FVertScrollBarDownButtonRect.Top;

      Details := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
      StyleServices.DrawElement(B.Canvas.Handle, Details, R);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarSliderState);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        GetVertScrollBarSliderRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails(FVertScrollBarUpButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnUpDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FVertScrollBarUpButtonRect);

      if FVertScrollBarWindow.Enabled then
        Details := StyleServices.GetElementDetails
          (FVertScrollBarDownButtonState)
      else
        Details := StyleServices.GetElementDetails(tsArrowBtnDownDisabled);
      StyleServices.DrawElement(B.Canvas.Handle, Details,
        FVertScrollBarDownButtonRect);

      MoveWindowOrg(B.Canvas.Handle, FVertScrollBarRect.Left,
        FVertScrollBarRect.Top);
      with FVertScrollBarRect do
        BitBlt(DC, Left, Top, B.Width, B.Height - TSynEditCracker(Control).BorderWidth, B.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      B.Free;
    end;
  end;
end;

function TSynEditVclStyleScrollBarsHook.GetHorzScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Left := BarInfo.xyThumbTop;
    Result.Right := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TSynEditVclStyleScrollBarsHook.GetVertScrollBarSliderRect: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    BarInfo.cbSize := SizeOf(BarInfo);
    GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), BarInfo);
    P := BarInfo.rcScrollBar.TopLeft;
    ScreenToClient(Handle, P);
    Result.TopLeft := P;
    P := BarInfo.rcScrollBar.BottomRight;
    ScreenToClient(Handle, P);
    Result.BottomRight := P;
    Result.Top := BarInfo.xyThumbTop;
    Result.Bottom := BarInfo.xyThumbBottom;
    if HasBorder then
      if HasClientEdge then
        OffsetRect(Result, 2, 2)
      else
        OffsetRect(Result, 1, 1);
  end
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TSynEditVclStyleScrollBarsHook.MouseLeave;
begin
  inherited;
  if FVertScrollBarSliderState = tsThumbBtnVertHot then
    FVertScrollBarSliderState := tsThumbBtnVertNormal;

  if FHorzScrollBarSliderState = tsThumbBtnHorzHot then
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;

  if FVertScrollBarUpButtonState = tsArrowBtnUpHot then
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

  if FVertScrollBarDownButtonState = tsArrowBtnDownHot then
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

  if FHorzScrollBarUpButtonState = tsArrowBtnLeftHot then
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

  if FHorzScrollBarDownButtonState = tsArrowBtnRightHot then
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

  PaintScrollBars;
end;

procedure TSynEditVclStyleScrollBarsHook.PaintScrollBars;
begin
  FVertScrollBarWindow.Repaint;
  FHorzScrollBarWindow.Repaint;
end;


procedure TSynEditVclStyleScrollBarsHook.UpdateScrollBarWindow;
var
  R: TRect;
  Owner: TSynEdit;
  HeaderHeight: Integer;
  BorderWidth: Integer;
begin
  Owner := TSynEdit(Control);
  HeaderHeight := 0;
  BorderWidth := 0;
  // VertScrollBarWindow

  if FVertScrollBarWindow.Visible and (seBorder in TSynEdit(Control).StyleElements)
  then
  begin
    R := FVertScrollBarRect;
    if Control.BiDiMode = bdRightToLeft then
    begin
      OffsetRect(R, -R.Left, 0);
      if HasBorder then
        OffsetRect(R, GetSystemMetrics(SM_CXEDGE), 0);
    end;
    if HasBorder then
      BorderWidth := GetSystemMetrics(SM_CYEDGE) * 2;
    ShowWindow(FVertScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FVertScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TSynEditCracker(Control).BorderWidth, Control.Top + R.Top + HeaderHeight
      + TSynEditCracker(Control).BorderWidth, R.Right - R.Left,
      Control.Height - HeaderHeight - BorderWidth - TSynEditCracker(Control).BorderWidth, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FVertScrollBarWindow.Handle, SW_HIDE);

  // HorzScrollBarWindow
  if FHorzScrollBarWindow.Visible and (seBorder in TSynEdit(Control).StyleElements)
  then
  begin
    R := FHorzScrollBarRect;
    if Control.BiDiMode = bdRightToLeft then
      OffsetRect(R, FVertScrollBarRect.Width, 0);
    ShowWindow(FHorzScrollBarWindow.Handle, SW_SHOW);
    SetWindowPos(FHorzScrollBarWindow.Handle, HWND_TOP, Control.Left + R.Left +
      TSynEditCracker(Control).BorderWidth, Control.Top + R.Top +
      TSynEditCracker(Control).BorderWidth + HeaderHeight, R.Right - R.Left,
      R.Bottom - R.Top, SWP_SHOWWINDOW);
  end
  else
    ShowWindow(FHorzScrollBarWindow.Handle, SW_HIDE);
end;

procedure TSynEditVclStyleScrollBarsHook.WMCaptureChanged(var Msg: TMessage);
begin
  if FVertScrollBarWindow.Visible and FVertScrollBarWindow.Enabled then
  begin
    if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      PaintScrollBars;
    end;

    if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      PaintScrollBars;
    end;
  end;

  if FHorzScrollBarWindow.Visible and FHorzScrollBarWindow.Enabled then
  begin
    if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      PaintScrollBars;
    end;

    if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      PaintScrollBars;
    end;
  end;

  CallDefaultProc(TMessage(Msg));
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMHScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.CMUpdateVclStyleScrollbars(var Message: TMessage);
begin
  CalcScrollBarsRect;
  PaintScrollBars;
end;

procedure TSynEditVclStyleScrollBarsHook.WMKeyDown(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMKeyUp(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMLButtonDown(var Msg: TWMMouse);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);
  if FVertScrollBarWindow.Visible then
  begin
    if FVertScrollBarSliderState = tsThumbBtnVertPressed then
    begin
      PostMessage(Handle, WM_VSCROLL,
        Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
      FLeftMouseButtonDown := False;
      FVertScrollBarSliderState := tsThumbBtnVertNormal;
      PaintScrollBars;
      Handled := True;
      ReleaseCapture;
      Exit;
    end;

    if FVertScrollBarUpButtonState = tsArrowBtnUpPressed then
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;

    if FVertScrollBarDownButtonState = tsArrowBtnDownPressed then
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
  end;

  if FHorzScrollBarWindow.Visible then
  begin
    if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
    begin
      PostMessage(Handle, WM_HSCROLL,
        Integer(SmallPoint(SB_ENDSCROLL, 0)), 0);
      FLeftMouseButtonDown := False;
      FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
      PaintScrollBars;
      Handled := True;
      ReleaseCapture;
      Exit;
    end;

    if FHorzScrollBarUpButtonState = tsArrowBtnLeftPressed then
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;

    if FHorzScrollBarDownButtonState = tsArrowBtnRightPressed then
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
  end;
  PaintScrollBars;
  FLeftMouseButtonDown := False;
end;

procedure TSynEditVclStyleScrollBarsHook.WMMouseMove(var Msg: TWMMouse);
var
  SF: TScrollInfo;
begin
  inherited;
  if FVertScrollBarSliderState = tsThumbBtnVertPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_VERT, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.Y - FPrevScrollPos) /
      FVertScrollBarSliderTrackRect.Height);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.Y;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_VERT, SF, False);
    PostMessage(Handle, WM_VSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Min(Round(FScrollPos), High(SmallInt)))), 0);
    // Min() prevents range check error

    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
  begin
    SF.fMask := SIF_ALL;
    SF.cbSize := SizeOf(SF);
    GetScrollInfo(Handle, SB_HORZ, SF);
    if SF.nPos <> Round(FScrollPos) then
      FScrollPos := SF.nPos;

    FScrollPos := FScrollPos + (SF.nMax - SF.nMin) *
      ((Mouse.CursorPos.X - FPrevScrollPos) /
      FHorzScrollBarSliderTrackRect.Width);
    if FScrollPos < SF.nMin then
      FScrollPos := SF.nMin;
    if FScrollPos > SF.nMax then
      FScrollPos := SF.nMax;
    if SF.nPage <> 0 then
      if Round(FScrollPos) > SF.nMax - Integer(SF.nPage) + 1 then
        FScrollPos := SF.nMax - Integer(SF.nPage) + 1;
    FPrevScrollPos := Mouse.CursorPos.X;
    SF.nPos := Round(FScrollPos);

    SetScrollInfo(Handle, SB_HORZ, SF, False);
    PostMessage(Handle, WM_HSCROLL, Integer(SmallPoint(SB_THUMBPOSITION,
      Round(FScrollPos))), 0);

    PaintScrollBars;
    Handled := True;
    Exit;
  end;

  if FHorzScrollBarSliderState = tsThumbBtnHorzHot then
  begin
    FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarSliderState = tsThumbBtnVertHot then
  begin
    FVertScrollBarSliderState := tsThumbBtnVertNormal;
    PaintScrollBars;
  end
  else if FHorzScrollBarUpButtonState = tsArrowBtnLeftHot then
  begin
    FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
    PaintScrollBars;
  end
  else if FHorzScrollBarDownButtonState = tsArrowBtnRightHot then
  begin
    FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarUpButtonState = tsArrowBtnUpHot then
  begin
    FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
    PaintScrollBars;
  end
  else if FVertScrollBarDownButtonState = tsArrowBtnDownHot then
  begin
    FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
    PaintScrollBars;
  end;

  CallDefaultProc(TMessage(Msg));
  if FLeftMouseButtonDown then
    PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMMouseWheel(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMNCLButtonDblClk(var Msg: TWMMouse);
begin
  WMNCLButtonDown(Msg);
end;

procedure TSynEditVclStyleScrollBarsHook.WMNCLButtonDown(var Msg: TWMMouse);
var
  P: TPoint;
  SF: TScrollInfo;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if FVertScrollBarWindow.Visible then
  begin
    if PtInRect(GetVertScrollBarSliderRect, P) then
    begin
      FLeftMouseButtonDown := True;
      SF.fMask := SIF_ALL;
      SF.cbSize := SizeOf(SF);
      GetScrollInfo(Handle, SB_VERT, SF);
      // FListPos := SF.nPos;
      FScrollPos := SF.nPos;
      FPrevScrollPos := Mouse.CursorPos.Y;
      FVertScrollBarSliderState := tsThumbBtnVertPressed;
      PaintScrollBars;
      SetCapture(Handle);
      Handled := True;
      Exit;
    end;

    if FVertScrollBarWindow.Enabled then
    begin
      if PtInRect(FVertScrollBarDownButtonRect, P) then
        FVertScrollBarDownButtonState := tsArrowBtnDownPressed;
      if PtInRect(FVertScrollBarUpButtonRect, P) then
        FVertScrollBarUpButtonState := tsArrowBtnUpPressed;
    end;
  end;

  if FHorzScrollBarWindow.Visible then
  begin
    if PtInRect(GetHorzScrollBarSliderRect, P) then
    begin
      FLeftMouseButtonDown := True;
      SF.fMask := SIF_ALL;
      SF.cbSize := SizeOf(SF);
      GetScrollInfo(Handle, SB_HORZ, SF);
      // FListPos := SF.nPos;
      FScrollPos := SF.nPos;
      FPrevScrollPos := Mouse.CursorPos.X;
      FHorzScrollBarSliderState := tsThumbBtnHorzPressed;
      PaintScrollBars;
      SetCapture(Handle);
      Handled := True;
      Exit;
    end;

    if FHorzScrollBarWindow.Enabled then
    begin
      if PtInRect(FHorzScrollBarDownButtonRect, P) then
        FHorzScrollBarDownButtonState := tsArrowBtnRightPressed;
      if PtInRect(FHorzScrollBarUpButtonRect, P) then
        FHorzScrollBarUpButtonState := tsArrowBtnLeftPressed;
    end;
  end;
  FLeftMouseButtonDown := True;
  PaintScrollBars;
end;

procedure TSynEditVclStyleScrollBarsHook.WMNCLButtonUp(var Msg: TWMMouse);
var
  P: TPoint;
  B: Boolean;
begin
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  if FVertScrollBarWindow.Visible then
    if FVertScrollBarWindow.Enabled then
    begin
      if FVertScrollBarSliderState = tsThumbBtnVertPressed then
      begin
        FLeftMouseButtonDown := False;
        FVertScrollBarSliderState := tsThumbBtnVertNormal;
        PaintScrollBars;
        Handled := True;
        Exit;
      end;

      if PtInRect(FVertScrollBarDownButtonRect, P) then
        FVertScrollBarDownButtonState := tsArrowBtnDownHot
      else
        FVertScrollBarDownButtonState := tsArrowBtnDownNormal;

      if PtInRect(FVertScrollBarUpButtonRect, P) then
        FVertScrollBarUpButtonState := tsArrowBtnUpHot
      else
        FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
    end;

  if FHorzScrollBarWindow.Visible then
    if FHorzScrollBarWindow.Enabled then
    begin
      if FHorzScrollBarSliderState = tsThumbBtnHorzPressed then
      begin
        FLeftMouseButtonDown := False;
        FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
        PaintScrollBars;
        Handled := True;
        Exit;
      end;

      if PtInRect(FHorzScrollBarDownButtonRect, P) then
        FHorzScrollBarDownButtonState := tsArrowBtnRightHot
      else
        FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;

      if PtInRect(FHorzScrollBarUpButtonRect, P) then
        FHorzScrollBarUpButtonState := tsArrowBtnLeftHot
      else
        FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
    end;
  CallDefaultProc(TMessage(Msg));

  if (FHorzScrollBarWindow.Visible) or (FVertScrollBarWindow.Visible)
  then
    PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMNCMouseMove(var Msg: TWMMouse);
var
  P: TPoint;
  MustUpdateScroll: Boolean;
  B: Boolean;
begin
  inherited;
  P := Point(Msg.XPos, Msg.YPos);
  ScreenToClient(Handle, P);

  if HasBorder then
    if HasClientEdge then
    begin
      P.X := P.X + 2;
      P.Y := P.Y + 2;
    end
    else
    begin
      P.X := P.X + 1;
      P.Y := P.Y + 1;
    end;

  MustUpdateScroll := False;
  if FVertScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetVertScrollBarSliderRect, P);
    if B and (FVertScrollBarSliderState = tsThumbBtnVertNormal) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarSliderState = tsThumbBtnVertHot) then
    begin
      FVertScrollBarSliderState := tsThumbBtnVertNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FVertScrollBarDownButtonRect, P);
    if B and (FVertScrollBarDownButtonState = tsArrowBtnDownNormal) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarDownButtonState = tsArrowBtnDownHot) then
    begin
      FVertScrollBarDownButtonState := tsArrowBtnDownNormal;
      MustUpdateScroll := True;
    end;
    B := PtInRect(FVertScrollBarUpButtonRect, P);
    if B and (FVertScrollBarUpButtonState = tsArrowBtnUpNormal) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpHot;
      MustUpdateScroll := True;
    end
    else if not B and (FVertScrollBarUpButtonState = tsArrowBtnUpHot) then
    begin
      FVertScrollBarUpButtonState := tsArrowBtnUpNormal;
      MustUpdateScroll := True;
    end;
  end;

  if FHorzScrollBarWindow.Enabled then
  begin
    B := PtInRect(GetHorzScrollBarSliderRect, P);
    if B and (FHorzScrollBarSliderState = tsThumbBtnHorzNormal) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarSliderState = tsThumbBtnHorzHot) then
    begin
      FHorzScrollBarSliderState := tsThumbBtnHorzNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarDownButtonRect, P);
    if B and (FHorzScrollBarDownButtonState = tsArrowBtnRightNormal) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarDownButtonState = tsArrowBtnRightHot) then
    begin
      FHorzScrollBarDownButtonState := tsArrowBtnRightNormal;
      MustUpdateScroll := True;
    end;

    B := PtInRect(FHorzScrollBarUpButtonRect, P);
    if B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftNormal) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftHot;
      MustUpdateScroll := True;
    end
    else if not B and (FHorzScrollBarUpButtonState = tsArrowBtnLeftHot) then
    begin
      FHorzScrollBarUpButtonState := tsArrowBtnLeftNormal;
      MustUpdateScroll := True;
    end;
  end;

  if MustUpdateScroll then
    PaintScrollBars;
end;

procedure TSynEditVclStyleScrollBarsHook.WMNCPaint(var Msg: TMessage);
begin
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
end;

procedure TSynEditVclStyleScrollBarsHook.WMSize(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMMove(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  CalcScrollBarsRect;
  UpdateScrollBarWindow;
  PaintScrollBars;
  Handled := True;
end;

procedure TSynEditVclStyleScrollBarsHook.WMPosChanged(var Msg: TMessage);
begin
  WMMove(Msg);
end;

procedure TSynEditVclStyleScrollBarsHook.WMVScroll(var Msg: TMessage);
begin
  CallDefaultProc(TMessage(Msg));
  PaintScrollBars;
  Handled := True;
end;

{ TVclStyleScrollBarsHook.TVclStyleScrollBarWindow }

constructor TSynEditVclStyleScrollBarsHook.TVclStyleScrollBarWindow.Create
  (AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOverrideStylePaint];
  FScrollBarWindowOwner := nil;
  FScrollBarVertical := False;
  FScrollBarVisible := False;
  FScrollBarEnabled := False;
end;

procedure TSynEditVclStyleScrollBarsHook.TVclStyleScrollBarWindow.CreateParams
  (var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TSynEditVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMEraseBkgnd
  (var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TSynEditVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMNCHitTest
  (var Msg: TWMNCHitTest);
begin
  Msg.Result := HTTRANSPARENT;
end;

procedure TSynEditVclStyleScrollBarsHook.TVclStyleScrollBarWindow.WMPaint
  (var Msg: TWMPaint);
var
  PS: TPaintStruct;
  DC: HDC;
begin
  BeginPaint(Handle, PS);
  try
    if FScrollBarWindowOwner <> nil then
    begin
      DC := GetWindowDC(Handle);
      try
        if FScrollBarVertical then
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FVertScrollBarRect.Left,
            -FScrollBarWindowOwner.FVertScrollBarRect.Top);
          FScrollBarWindowOwner.DrawVertScrollBar(DC);
        end
        else
        begin
          MoveWindowOrg(DC, -FScrollBarWindowOwner.FHorzScrollBarRect.Left,
            -FScrollBarWindowOwner.FHorzScrollBarRect.Top);
          FScrollBarWindowOwner.DrawHorzScrollBar(DC);
        end;
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

end.
