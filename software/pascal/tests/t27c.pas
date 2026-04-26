PROGRAM T27C;
TYPE
  TPOINT = RECORD X, Y: INTEGER END;
  TLINE = RECORD A, B: TPOINT END;
  TBOX  = RECORD
            HEAD: TLINE;
            TAIL: RECORD U, V: INTEGER END
          END;
VAR
  L: TLINE;
  K: TBOX;
BEGIN
  L.A.X := 1;
  L.A.Y := 2;
  L.B.X := 30;
  L.B.Y := 40;
  WRITELN(L.A.X);
  WRITELN(L.A.Y);
  WRITELN(L.B.X);
  WRITELN(L.B.Y);
  WRITELN(L.A.X + L.B.Y);

  K.HEAD.A.X := 100;
  K.HEAD.A.Y := 200;
  K.HEAD.B.X := 300;
  K.HEAD.B.Y := 400;
  K.TAIL.U   := 555;
  K.TAIL.V   := 666;
  WRITELN(K.HEAD.A.X);
  WRITELN(K.HEAD.B.Y);
  WRITELN(K.TAIL.U);
  WRITELN(K.TAIL.V);
  WRITELN(K.HEAD.A.X + K.TAIL.V)
END.
