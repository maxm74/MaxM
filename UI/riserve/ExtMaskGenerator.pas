unit extMaskgenerator;

interface
uses
  Windows, SysUtils, Classes, Graphics, Forms;

  procedure ExtGenerateMask (BitMap: TBitmap; transparentcolor: TColor; FName: String);

implementation

// This is called when the User clicks the OK Button
procedure ExtGenerateMask (BitMap: TBitmap; transparentcolor: TColor; FName: String);
var
  x,y	      : integer;
  rgn1, rgn2  : hrgn;
  startx,endx : integer;
  size	      : integer;
  rgndata     : pRGNData;
  writer      : TFileStream;

begin
  // for every line do...
  rgn1 := 0;
  //rgn2 := 0;  //wasn't used
  for y := 0 to BitMap.Height-1 do
  begin
    // don`t look as if we were locked up
    Application.ProcessMessages;
    x:=0;
    //endx:=x; //wasn' used
    repeat
      // look for the beginning of a stretch of non-transparent pixels
      while (bitmap.canvas.pixels[x,y] = transparentcolor) and (x = BitMap.width) do
      inc(x);
      startx:=x;
      // look for the end of a stretch of non-transparent pixels
      inc(x);
      while (bitmap.canvas.pixels[x,y]<>transparentcolor) and (x<=BitMap.width) do
      inc(x);
      endx:=x;
      // do we have some pixels?
      if startx<>BitMap.Width then
      begin
        if endx= BitMap.Width then dec(endx);
        // do we have a region already?
        if rgn1 = 0 then
        begin
          // Create a region to start with
          rgn1 := createrectrgn(startx+1,y,endx,y+1);
        end else
        begin
          // Add to the existing region
          rgn2 := createrectrgn(startx+1,y,endx,y+1);
          if rgn2<>0 then combinergn(rgn1,rgn1,rgn2,RGN_OR);
          deleteobject(rgn2);
        end;
      end;
    until x>=BitMap.width-1;
  end;

  if (rgn1<>0) then
  begin
    writer := TFileStream.Create (FName, fmCreate or fmShareDenyWrite);
    // get the region data`s size
    size:=getregiondata (rgn1, 0, nil);
    getmem (rgndata, size);
    // get the data itself
    getregiondata(rgn1, size, rgndata);
    // write it
    writer.write (size, sizeof(size));
    writer.write (rgndata^, size);
    freemem(rgndata, size);
    writer.Free;
  end;
end;

end.
