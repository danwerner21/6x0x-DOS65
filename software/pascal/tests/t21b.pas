{ T21B: array var declared, but only scalar output - no subscript }
PROGRAM T21B;
VAR
  I : INTEGER;
  A : ARRAY [1..5] OF INTEGER;
BEGIN
  I := 99;
  WRITELN(I);
END.
