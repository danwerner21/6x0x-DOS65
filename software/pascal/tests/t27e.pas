PROGRAM T27E;
TYPE
  TPOINT = RECORD X, Y: INTEGER END;
  TLINE = RECORD A, B: TPOINT END;
  TBOX  = RECORD
            HEAD: TLINE;
            TAIL: RECORD U, V: INTEGER END
          END;
VAR
  L: TLINE;
BEGIN
  L.A.X := 1;
  L.A.Y := 2;
  L.B.X := 30;
  L.B.Y := 40;
  WRITELN(L.A.X);
  WRITELN(L.A.Y);
  WRITELN(L.B.X);
  WRITELN(L.B.Y);
  WRITELN(L.A.X + L.B.Y)
END.
