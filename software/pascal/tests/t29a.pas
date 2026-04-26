PROGRAM T29A;
TYPE
  TMID = SET OF 3..7;
VAR
  S: TMID;
BEGIN
  S := [3, 5, 7];
  WRITELN(3 IN S);
  WRITELN(2 IN S);

  S := S + [4..6];
  WRITELN(6 IN S);
  WRITELN(S = [3..7]);

  S := S - [4..6];
  WRITELN(S = [3, 7])
END.
