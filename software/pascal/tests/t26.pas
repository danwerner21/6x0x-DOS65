PROGRAM T26;
VAR
  P, Q: ^INTEGER;
  I: INTEGER;
BEGIN
  NEW(P);
  P^ := 42;
  WRITELN('P^=', P^);

  NEW(Q);
  Q^ := P^ + 100;
  WRITELN('Q^=', Q^);

  P^ := Q^ - 3;
  WRITELN('P^=', P^, ' Q^=', Q^);

  FOR I := 1 TO 5 DO BEGIN
    NEW(P);
    P^ := I * I
  END;
  WRITELN('LAST P^=', P^);

  DISPOSE(P);
  DISPOSE(Q);
  WRITELN('DONE')
END.
