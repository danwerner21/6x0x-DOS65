PROGRAM T22A;
TYPE
  TVEC = RECORD
    X, Y, Z : INTEGER
  END;
VAR
  V : TVEC;
BEGIN
  V.X := 1;
  V.Y := 22;
  V.Z := 333;
  WRITELN(V.X);
  WRITELN(V.Y);
  WRITELN(V.Z)
END.
