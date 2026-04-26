PROGRAM T27A;
VAR
  P: RECORD
       X, Y: INTEGER
     END;
  Q: RECORD
       NAME: INTEGER;
       AGE: INTEGER
     END;
BEGIN
  P.X := 11;
  P.Y := 22;
  Q.NAME := 99;
  Q.AGE := 100;
  WRITELN(P.X);
  WRITELN(P.Y);
  WRITELN(Q.NAME);
  WRITELN(Q.AGE);
  WRITELN(P.X + P.Y + Q.NAME + Q.AGE)
END.
