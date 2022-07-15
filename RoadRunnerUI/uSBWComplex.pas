unit uSBWComplex;

// Complex arithmetic routines

interface

uses SysUtils, Math;

type
   TSBWComplex = record r, i : double; end;

function complex (r, i : double) : TSBWComplex;

function geTSBWComplexRealPart (const z : TSBWComplex) : double;
function geTSBWComplexImagPart (const z : TSBWComplex) : double;

function complexAdd (a, b : TSBWComplex) : TSBWComplex;
function complexSub (a, b : TSBWComplex) : TSBWComplex;
function complexMult (a, b : TSBWComplex) : TSBWComplex;
function complexDiv (a, b : TSBWComplex) : TSBWComplex;
function complexNeg (a : TSBWComplex) : TSBWComplex;
function complexPow (a, b : TSBWComplex) : TSBWComplex;
function complexSqrt (const z : TSBWComplex) : TSBWComplex;
function complexSqr  (const z : TSBWComplex) : TSBWComplex;
function complexAbs  (const z : TSBWComplex) : double;
function complexPhase(const z : TSBWComplex) : double;

function complexSin (const z : TSBWComplex) : TSBWComplex;
function complexCos (const z : TSBWComplex) : TSBWComplex;
function complexTan (const z : TSBWComplex) : TSBWComplex;
function complexExp (const z : TSBWComplex) : TSBWComplex;
function complexLn  (const z : TSBWComplex) : TSBWComplex;
function complexLog10 (const z : TSBWComplex) : TSBWComplex;
function complexLog2 (const z : TSBWComplex) : TSBWComplex;

function complexLessThan (a, b : TSBWComplex) : boolean;
function complexGreaterThan (a, b : TSBWComplex) : boolean;
function complexLessThanOrEqual (a, b : TSBWComplex) : boolean;
function complexGreaterThanOrEqual (a, b : TSBWComplex) : boolean;

function complexNotEquals (a, b : TSBWComplex) : boolean;
function complexEquals (a, b : TSBWComplex) : boolean;

implementation


function complex (r, i : double) : TSBWComplex;
begin
  result.r := r; result.i := i;
end;


function geTSBWComplexRealPart (const z : TSBWComplex) : double;
begin
  result := z.r;
end;


function geTSBWComplexImagPart (const z : TSBWComplex) : double;
begin
  result := z.i;
end;


function complexAdd (a, b : TSBWComplex) : TSBWComplex;
begin
  result.r := a.r + b.r;
  result.i := a.i + b.i;
end;


function complexSub (a, b : TSBWComplex) : TSBWComplex;
begin
  result.r := a.r - b.r;
  result.i := a.i - b.i;
end;


function complexMult (a, b : TSBWComplex) : TSBWComplex;
begin
  result.r := (a.r * b.r) - (a.i * b.i);
  result.i := (a.r * b.i) + (a.i * b.r);
end;


function complexDiv (a, b : TSBWComplex) : TSBWComplex;
var abs_breal, abs_bimag : double; ratio, denom : double;
begin
  // Taken from Python code, 2.1
  // This algorithm is better, and is pretty obvious:  first divide the
  // numerators and denominator by whichever of {b.real, b.imag} has
  // larger magnitude.  The earliest reference I found was to CACM
  // Algorithm 116 (Complex Division, Robert L. Smith, Stanford
  // University).  As usual, though, we're still ignoring all IEEE
  // endcases.

  if b.r < 0 then abs_breal := -b.r else abs_breal := b.r;
  if b.i < 0 then abs_bimag := -b.i else abs_bimag := b.i;

  if (abs_breal >= abs_bimag) then
     begin
     // divide tops and bottom by b.real
     if (abs_breal = 0.0) then
        raise Exception.Create ('Floating point divide by zero in complex divide')  // Doesn't return if successful
     else
        begin
  	ratio := b.i / b.r;
  	denom := b.r + b.i * ratio;
  	result.r := (a.r + a.i * ratio) / denom;
  	result.i := (a.i - a.r * ratio) / denom;
  	end
     end
  else
     begin
     // divide tops and bottom by b.imag
     ratio := b.r / b.i;
     denom := b.r * ratio + b.i;
     //assert(b.imag != 0.0);
     result.r := (a.r * ratio + a.i) / denom;
     result.i := (a.i * ratio - a.r) / denom;
     end;
end;


function complexNeg (a : TSBWComplex) : TSBWComplex;
begin
  result.r := -a.r;
  result.i := -a.i;
end;


function complexPow(a, b : TSBWComplex) : TSBWComplex;
var vabs, len, at, phase : double;
begin
  if (b.r = 0.0) and (b.i = 0.0) then
     begin
     result.r := 1.0;
     result.i := 0.0;
     end
  else if (a.r = 0.0) and (a.i = 0.0) then
          begin
          if (b.i <> 0.0) or (b.r < 0.0) then
             raise Exception.Create ('Floating point range error in complex power')  // Doesn't return if successful
          end
  else  begin
        vabs := hypot(a.r, a.i);
        len := power(vabs, b.r);
        at := arctan2(a.i, a.r);
        phase := at*b.r;
	if (b.i <> 0.0) then
           begin
	   len := len / exp(at*b.i);
	   phase := phase + b.i*ln(vabs);
	   end;
	result.r := len*cos(phase);
	result.i := len*sin(phase);
	end;
end;


function complexSqr (const z : TSBWComplex) : TSBWComplex;
begin
  result.r := (z.r * z.r) - (z.i * z.i);
  result.i := (z.r * z.i) + (z.i * z.r);
end;


function complexSqrt (const z : TSBWComplex) : TSBWComplex;
var r : TSBWComplex; mag : double;
begin
  //
  // Complex square root.
  //
  // This function was adapted from the f2c "z_sqrt" function.
  // Copyright 1990 by AT&T Bell Laboratories and Bellcore.
  //

  mag := complexAbs (z);
  if mag = 0.0 then
     begin
     r.r := 0.0;
     r.i := 0.0;
     end
  else if (z.r > 0.0) then
     begin
     r.r := sqrt (0.5 * (mag + z.r));
     r.i := z.i / r.r / 2.0;
     end
  else
    begin
      r.i := sqrt (0.5 * (mag - z.r));
      if (z.i < 0.0) then
	r.i := -r.i;
      r.r := z.i / r.i / 2.0;
    end;
  result := r;
end;


function complexPhase (const z : TSBWComplex) : double;
begin
  if (z.r = 0.0) and (z.i = 0.0) then
     result := 0.0
  else
     result := ArcTan2 (z.i, z.r);

end;


function complexAbs (const z : TSBWComplex) : double;
var r, i, temp : double;
begin
  //
  // Complex magnitude.
  //
  // This function was adapted from the f2c "z_abs" function.
  // Copyright 1990 by AT&T Bell Laboratories and Bellcore.
  //

  r := z.r;
  i := z.i;

  if (r < 0) then r := -r;
  if (i < 0) then i := -i;
  if (i > r) then
     begin
     temp := r;
     r := i;
     i := temp;
     end;

  if ((r + i) = r) then
     begin
     result := r;
     exit;
     end;

  temp := i / r;
  temp := r * sqrt (1.0 + temp * temp);

  result := temp;
end;


function complexSin (const z : TSBWComplex) : TSBWComplex;
begin
  result.r := sin (z.r) * cosh (z.i);
  result.i := cos (z.r) * sinh (z.i);
end;

function complexCos (const z : TSBWComplex) : TSBWComplex;
begin
  result.r := cos (z.r) * cosh (z.i);
  result.i := -sin (z.r) * sinh (z.i);
end;

function complexTan (const z : TSBWComplex) : TSBWComplex;
begin
  result.r := 1.0 / (cos (2 * z.r) + cosh (2 * z.i));
  result.i := result.r;
  result.r := result.r * sin (2 * z.r);
  result.i := result.i * sinh (2 * z.i);
end;

function complexExp (const z : TSBWComplex) : TSBWComplex;
var r : double;
begin
  r := exp(z.r);
  result.r := cos(z.i)*r;
  result.i := sin(z.i)*r;
end;


function complexATan (z : TSBWComplex) : TSBWComplex;
begin
  // atan(z) = i*log((i+z)/(i-z))/2
  //         = i*log(-((x^2+y^2-1)+i*(2*x))/(x^2+y^2-2*y+1))/2

  result.r := -1.0 / (z.r * z.r + z.i * z.i - 2 * z.i + 1.0);
  result.i := result.r;

  result.r := result.r * (z.r * z.r + z.i * z.i - 1.0);
  result.i := result.i * (2 * z.r);

  z := complexLn (result);

  result.r := -0.5 * z.i;
  result.i := 0.5 * z.r;
end;


function complexLn (const z : TSBWComplex) : TSBWComplex;
begin
  result.r := Ln (sqrt (z.r*z.r + z.i*z.i));
  result.i := ArcTan2 (z.i, z.r);
end;


// Compute Log to the base n
function complexLog_n (const z : TSBWComplex; n : double) : TSBWComplex;
begin
  result := complexDiv (complexLn(z), complex (Ln(n), 0.0));
end;

// Uses: log(x)/log(10)
function complexLog10 (const z : TSBWComplex) : TSBWComplex;
begin
  result := complexLog_n (z, 10.0);
end;

// Uses: log(x)/log(a);
function complexLog2 (const z : TSBWComplex) : TSBWComplex;
begin
  result := complexLog_n (z, 2.0);
end;


// -----------------------------------------------------------------


function complexLessThan (a, b : TSBWComplex) : boolean;
begin
  result := ((a.r*a.r+a.i*a.i) < (b.r*b.r+b.i*b.i));
end;


function complexGreaterThan (a, b : TSBWComplex) : boolean;
begin
  result := ((a.r*a.r+a.i*a.i) > (b.r*b.r+b.i*b.i));
end;


function complexLessThanOrEqual (a, b : TSBWComplex) : boolean;
begin
  result := ((a.r*a.r+a.i*a.i) <= (b.r*b.r+b.i*b.i));
end;


function complexGreaterThanOrEqual (a, b : TSBWComplex) : boolean;
begin
  result := ((a.r*a.r+a.i*a.i) >= (b.r*b.r+b.i*b.i));
end;


function complexNotEquals (a, b : TSBWComplex) : boolean;
begin
  result := (a.r <> b.r) and (a.i <> b.i);
end;


function complexEquals (a, b : TSBWComplex) : boolean;
begin
  result := (a.r = b.r) and (a.i = b.i);
end;


end.
