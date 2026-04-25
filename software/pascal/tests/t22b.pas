PROGRAM T22B;
TYPE
  TPOINT = RECORD
    X : INTEGER;
    Y : INTEGER
  END;
VAR
  A, B : TPOINT;
  I : INTEGER;
BEGIN
  A.X := 1;
  A.Y := 2;
  B.X := 100;
  B.Y := 200;
  WRITELN(A.X);
  WRITELN(A.Y);
  WRITELN(B.X);
  WRITELN(B.Y);
  I := A.X + B.Y;
  WRITELN(I)
END.
