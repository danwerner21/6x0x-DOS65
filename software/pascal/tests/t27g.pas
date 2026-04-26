PROGRAM T27G;
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
  L.B.X := 30;
  K.HEAD.A.X := 100
END.
