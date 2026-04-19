PROGRAM T08;
VAR
  I : INTEGER;
  J : INTEGER;
BEGIN
  I := 1;
  WHILE I <= 3 DO
  BEGIN
    J := 1;
    WHILE J <= 3 DO
    BEGIN
      IF I = J THEN
        WRITELN(I * 10 + J + 100)
      ELSE
        WRITELN(I * 10 + J);
      J := J + 1
    END;
    I := I + 1
  END
END.
