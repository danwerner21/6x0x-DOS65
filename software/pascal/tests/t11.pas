PROGRAM T11;
VAR
  I : INTEGER;
  S : INTEGER;
BEGIN
  S := 0;
  FOR I := 1 TO 10 DO
    S := S + I;
  WRITELN(S)
END.
