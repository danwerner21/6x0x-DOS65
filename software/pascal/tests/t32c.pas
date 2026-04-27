PROGRAM T32C;
{ Function with local vars beyond its params, called with a local-var arg
  from another function's frame.  Exercises non-zero lsize_extra plus
  caller-MP arg evaluation. }

FUNCTION SCALE(A: INTEGER): INTEGER;
VAR
  T1, T2: INTEGER;
BEGIN
  T1 := A * 3;
  T2 := A + 1;
  SCALE := T1 + T2
END;

FUNCTION OUTER(X: INTEGER): INTEGER;
BEGIN
  OUTER := SCALE(X) + X
END;

BEGIN
  WRITELN(OUTER(5))
END.
