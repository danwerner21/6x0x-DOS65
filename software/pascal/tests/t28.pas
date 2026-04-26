PROGRAM T28;
TYPE
  TPOINT = RECORD X, Y: INTEGER END;
  TBOX = RECORD P, Q: TPOINT END;
VAR
  B: TBOX;
BEGIN
  WITH B.P DO
  BEGIN
    X := 10;
    Y := 20
  END;
  WRITELN(B.P.X);
  WRITELN(B.P.Y);

  WITH B DO
  BEGIN
    WITH Q DO
    BEGIN
      Y := 30;
      X := P.X + Y
    END
  END;
  WRITELN(B.Q.X);
  WRITELN(B.Q.Y)
END.
