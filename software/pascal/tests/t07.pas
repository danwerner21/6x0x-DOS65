PROGRAM T07;
VAR
  I : INTEGER;
BEGIN
  I := 1;
  WHILE I <= 5 DO
  BEGIN
    IF I = 3 THEN
      WRITELN(33)
    ELSE
      WRITELN(I);
    I := I + 1
  END
END.
