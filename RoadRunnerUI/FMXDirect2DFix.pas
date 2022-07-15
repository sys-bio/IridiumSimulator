unit FMXDirect2DFix;

interface

// Use this code only if:
// - Windows
// - XE4 or 5 (known versions this fix works for, and which require this fix)

{$ifdef MSWINDOWS}
  // System.RTLVersion: 25 for XE4; 26 for XE5
  {$if (RTLVersion >= 25.0) and (RTLVersion <= 26.0)}
    {$DEFINE APPLY_FMX_D2D_FIX}
  {$ifend}
{$endif}

{$ifdef APPLY_FMX_D2D_FIX}
uses
  FMX.Types,
  Winapi.D3D10_1,
  Winapi.D3D10,
  WinAPI.Windows,
  System.Math;
{$endif}

  procedure TryUseDirect2D;

implementation

{$ifdef APPLY_FMX_D2D_FIX}

  // The following FPU state methods from FMX.Context.DX10.pas

  {$IFDEF CPUX64}
    var
      PrevFPUState: TArithmeticExceptionMask;
  {$ENDIF}

  procedure SaveClearFPUState;
  begin
    {$IFDEF CPUX64}
      PrevFPUState:= System.Math.GetExceptionMask;
      System.Math.SetExceptionMask(exAllArithmeticExceptions);
    {$ENDIF}
  end;

  procedure RestoreFPUState;
  begin
    {$IFDEF CPUX64}
      System.Math.SetExceptionMask(PrevFPUState);
    {$ENDIF}
  end;

  // Fix a FireMonkey bug where if there's no Direct3D-10 hardware, Direct2D is not used
  // (GDI+ is used by default.) Direct2D can still be used in this case, through WARP, a
  // high-performance software rasterizer which has better performance than GDI+.
  // Note it's probably also possible to use DX9 hardware acceleration but that requires
  // editing the FMX source; WARP is still a lot better than the default, GDI+, though.
  // See http://itinerantdeveloper.blogspot.ro/2014/01/firemonkey-canvas-classes-and-bugfix-to.html
  procedure TryUseDirect2D;
  var
    DX10Library : THandle;
    TestDevice : ID3D10Device1;
  begin
    DX10Library := LoadLibrary(Winapi.D3D10_1.D3D10_1_dll);
    if DX10Library = 0 then Exit;

    try
      SaveClearFPUState;
      try
        if GetProcAddress(DX10Library, 'D3D10CreateDevice1') = nil then Exit;

        // If there's no hardware D3D10 support, but there /is/ WARP (software support)
        // force that to be used. Don't bother checking DX9 support, just go for WARP.
        if not Succeeded(D3D10CreateDevice1(nil, D3D10_DRIVER_TYPE_HARDWARE, 0, D3D10_CREATE_DEVICE_BGRA_SUPPORT, D3D10_FEATURE_LEVEL_10_1, D3D10_1_SDK_VERSION, TestDevice)) and
          Succeeded(D3D10CreateDevice1(nil, D3D10_DRIVER_TYPE_WARP, 0, D3D10_CREATE_DEVICE_BGRA_SUPPORT, D3D10_FEATURE_LEVEL_10_1, D3D10_1_SDK_VERSION, TestDevice))
          then begin
            // Ok to force software (WARP) since unable to create hardware
            FMX.Types.GlobalUseDX10Software := true;
          end;
      finally
        TestDevice := nil;
        RestoreFPUState;
      end;
    finally
      FreeLibrary(DX10Library);
    end;
  end;

{$else}

  // A dummy implementation for non-Windows, non-XE4 or 5.
  procedure TryUseDirect2D;
  begin
  end;

{$endif}

end.
