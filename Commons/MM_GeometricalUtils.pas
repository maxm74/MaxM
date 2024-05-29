unit MM_GeometricalUtils;

{$mode objfpc}{$H+}

interface
uses Types;

type
    TRectArray = array of TRect;

implementation

function Rect_Contains(ARect, BRect: TRect): Boolean;
begin
     Result :=(BRect.Left >= ARect.Left) and (BRect.Top >= ARect.Top) and
              (BRect.Right <= ARect.Right) and (BRect.Bottom <= ARect.Bottom);
end;

function Rect_Intersects(ARect, BRect: TRect): Boolean;
begin
     Result :=not((BRect.Right <= ARect.Left) or (BRect.Bottom <= ARect.Top) or
                  (BRect.Left >= ARect.Right) or (BRect.Top >= ARect.Bottom));
end;

function Rect_Complement(var lRects: TRectArray; ARect, BRect: TRect): Integer;
var
   top, bottom, left, right: TRect;

begin
(*
  Result :=0;

  if not(Rect_Intersects(ARect, BRect)) or Rect_Contains(BRect, ARect)
  then Exit;

  if (BRect.Top - ARect.Top) > 0 then top :=Rect(ARect.Left, ARect.Top, ARect.Right, BRect.Top);
  if (BRect.Bottom - ARect.Bottom) > 0 bottom:=Rect(ARect.Left, BRect.Top, ARect.Right, BRect.Top);
*)
end;



end.

