{ T21.PAS - Phase 10b: ARRAY type support }
PROGRAM T21;
VAR
  A : ARRAY [1..5] OF INTEGER;
  I : INTEGER;
BEGIN
  A[1] := 10;
  A[2] := 20;
  A[3] := 30;
  I := 2;
  WRITELN(A[1]);
  WRITELN(A[2]);
  WRITELN(A[I]);
END.
