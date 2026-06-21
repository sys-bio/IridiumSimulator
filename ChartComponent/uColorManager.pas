unit uColorManager;

interface

Uses System.UITypes, System.UIConsts, Rtti;

type
  TColorManager = class
  private
    class var FColorIndex: Integer;
    class var FPalette: array of TAlphaColor;
    class function GetNextColor: TAlphaColor; static;
    class procedure InitializePalette; // Sets up the colors once
  public
    class procedure ResetCycle;
    // A class property does not require an object instance
    class property NextColor: TAlphaColor read GetNextColor;
  end;

implementation


class procedure TColorManager.ResetCycle;
begin
  FColorIndex := 0;
end;

class procedure TColorManager.InitializePalette;
begin
  if Length(FPalette) = 0 then
  begin
    FColorIndex := 0;

    FPalette := [$FFE6194B, $FF3CB44B, $FF4363D8, $FFFFE119, $FF4363D8, $FFF58231,
            $FF911EB4, $FF46F0F0, $FFF032E6, $FFBCF60C,
            $FF008080, $FFE6BEFF, $FF9A6324, $FFFFFAC8, $FF800000, $FFAAFFC3,
            $FF808000, $FFFFD8B1, $FF000075, $FF808080, $FFFFFFFF, $FF000000];

//    FPalette := [
//      $FFE6194B, // 1. Red
//      $FF3CB44B, // 2. Green
//      $FF4363D8, // 4. Blue
//      $FFF58231, // 5. Orange
//      $FF911EB4, // 6. Purple
//      $FFF032E6, // 8. Magenta
//      $FF800000, // 15. Maroon
//      $FFB8860B, // 3. Gold/Mustard
//      $FFBFEF45, // 9. Lime
//      $FFFABED4, // 10. Pink
//      $FF42D4F4, // 7. Cyan
//      $FF469990, // 11. Teal
//      $FFDCBEFF, // 12. Lavender
//      $FF9A6324, // 13. Brown
//      $FFFFFAC8, // 14. Beige
//      $FFAAFFC3, // 16. Mint
//      $FF808000, // 17. Olive
//      $FFFFD8B1, // 18. Apricot
//      $FF000075, // 19. Navy
//      $FFA9A9A9  // 20. Grey
//    ];
  end;
end;


class function TColorManager.GetNextColor: TAlphaColor;
begin
  InitializePalette; // Ensure colors exist

  Result := FPalette[FColorIndex];
  FColorIndex := (FColorIndex + 1) mod Length(FPalette);
end;

end.



