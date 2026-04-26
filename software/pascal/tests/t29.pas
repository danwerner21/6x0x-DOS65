PROGRAM T29;
TYPE
  TNIB = SET OF 0..15;
VAR
  A, B, C: TNIB;
BEGIN
  A := [1, 3, 5];
  B := [3..6];

  C := A + B;
  WRITELN(1 IN C);
  WRITELN(2 IN C);
  WRITELN(6 IN C);

  C := A * B;
  WRITELN(3 IN C);
  WRITELN(1 IN C);

  C := B - [4, 6];
  WRITELN(3 IN C);
  WRITELN(4 IN C);
  WRITELN(C = [3, 5]);

  C := [];
  WRITELN(1 IN C)
END.
