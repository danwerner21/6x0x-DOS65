PROGRAM T32B;
{ Mixed: function with VAR-by-ref arg taken from a local of caller. }

PROCEDURE INC2(VAR N: INTEGER);
BEGIN
  N := N + 2
END;

PROCEDURE OUTER;
VAR
  X: INTEGER;
BEGIN
  X := 10;
  INC2(X);
  WRITELN(X)
END;

BEGIN
  OUTER
END.
