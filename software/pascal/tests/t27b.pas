PROGRAM T27B;
TYPE
  TPOINT = RECORD X, Y: INTEGER END;

PROCEDURE SHOW;
VAR
  L: TPOINT;
BEGIN
  L.X := 7;
  L.Y := 8;
  WRITELN('LX=', L.X);
  WRITELN('LY=', L.Y);
  WRITELN('SUM=', L.X + L.Y)
END;

VAR
  G: TPOINT;
BEGIN
  G.X := 100;
  G.Y := 200;
  SHOW;
  WRITELN('GX=', G.X);
  WRITELN('GY=', G.Y)
END.
