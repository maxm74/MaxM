unit UPlasmaForm;
//version 1.1

{==============================================================================
v1.0      :  01/12/98 - first version

v1.1      :  01/04/99 - LoadMaskFromResource method added
                        (sugestion by Konstantin Leonidov)
                      - Set the form Scaled property to false
                      - The mask is no more cleared when the bitmap changes
==============================================================================}
// 2003 Massimo Magnano
//         Porting for Delphi 6,7 (Separated Packages RunTime and DesigneTime)
//         Fixed some bugs (VCL Picture.Onchange overridded but never call old)
//         Correct the MouseDown, MouseMove, MouseUp Seq.
//         Added CreateMaskFromColor
//         To Do :
//  ----->     Non calcola bene le regione in Win98
//             Reorganization of Mask Property Editor
//             Test for JPeg Images compatibility


interface
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, Buttons, StdCtrls, UPlasmaRegion;

type

  	TPlasmaForm = class(TImage)
	              private
                    FMoveable : Boolean;
                    Moving :Boolean;
                    HotSpotX,
                    HotSpotY,
                    rLeft,
                    rTop      : Integer;
              			FRegion   : TPlasmaRegion;
                    ImagePictureChanged :TNotifyEvent;

                    procedure   RefreshForm;
                    procedure   PictureUpdate(Sender:TObject);
                protected
              			procedure MouseDown(Button: TMouseButton; Shift: TShiftState;X,Y:Integer);override;
              		  procedure MouseMove(Shift: TShiftState;X, Y: Integer);override;
                    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;X,Y:Integer);override;
              			procedure   SetRegion(Value:TPlasmaRegion);
              			function    GetRegion:TPlasmaRegion;
              			procedure   SetParent(Value:TWinControl);override;
              	public
              			constructor Create(Aowner:TComponent); override;
              			destructor	 Destroy; override;
                    procedure   Loaded;override;
              			procedure   LoadMaskFromFile(Value:String);
                    procedure   LoadMaskFromResource(Instance:THandle;ResName:string);
                    procedure   CreateMaskFromColor(_TransparentColor: TColor);
              	published
              			property    Mask:TPlasmaRegion read GetRegion write SetRegion;
                    property    Moveable:Boolean   read FMoveable write FMoveable;
                end;

implementation
uses ImgUtils;

Type
  PDWordArray = ^TDWordArray;
  TDWordArray = array[0..0] of DWord;


{==============================================================================}
constructor TPlasmaForm.Create(Aowner:TComponent);
begin
     inherited Create(AOwner);
     Align:=alClient;
     AutoSize:=True;
     FRegion:=TPlasmaRegion.Create(Self);
     ImagePictureChanged :=Picture.OnChange;
     Picture.OnChange:=PictureUpdate;
     FMoveable:=True;
     Moving :=False;
end;

destructor TPlasmaForm.Destroy;
begin
FRegion.Free;
inherited Destroy;
end;

procedure TPlasmaForm.Loaded;
begin
inherited Loaded;
RefreshForm;
end;
{==============================================================================}
procedure TPlasmaForm.RefreshForm;
begin
     If (not (csDesigning in ComponentState)) then
     begin
          //SetWindowRgn(Parent.Handle, 0, true);
    			SetWindowRgn(Parent.Handle,FRegion.Region,true);
      end;
end;
{==============================================================================}
procedure TPlasmaForm.PictureUpdate(Sender:TObject);
begin
If Not Picture.Bitmap.Empty then
   begin
   Parent.ClientWidth:=Picture.Width;
   Parent.ClientHeight:=Picture.Height;
   end;
   ImagePictureChanged(Sender);
end;

procedure TPlasmaForm.SetParent(Value:TWinControl);
begin
inherited SetParent(Value);
if Value<>nil then
   if (Value is TForm) then
      begin
      TForm(Value).BorderStyle:=bsNone;
      TForm(Value).Scaled:=false;
      end
     else
   			raise Exception.Create('Please Drop on a Form')
end;
{==============================================================================}
procedure TPlasmaForm.SetRegion(Value:TPlasmaRegion);
begin
if Value<>nil then FRegion:=Value;
RefreshForm;
end;

function TPlasmaForm.GetRegion:TPlasmaRegion;
begin
Result:=FRegion;
end;
{===============================================================================}
procedure TPlasmaForm.MouseDown(Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
begin
   inherited MouseDown(Button,Shift,X,Y);
   if FMoveable and (Button = mbLeft) then
   begin
      Screen.Cursor := crSizeAll;
      Moving := True;
      HotSpotX  := X;
      HotSpotY  := Y;
      rLeft := Left;
      rTop := Top;
      SetCapture(Parent.Handle);
   end;
end;

procedure TPlasmaForm.MouseMove(Shift: TShiftState;X,Y:Integer);
begin
   inherited MouseMove(Shift,x,y);
   if Moving then
   begin
       rLeft := rLeft+(X-HotSpotX);
       rTop  := rTop +(Y-HotSpotY);
       SetWindowPos(Parent.Handle, 0, rLeft, rTop, 0, 0,
                    SWP_NOSIZE+SWP_NOZORDER+SWP_SHOWWINDOW);
   end;
end;

procedure TPlasmaForm.MouseUp(Button: TMouseButton; Shift: TShiftState;X,Y:Integer);
begin
   inherited MouseUp(Button, Shift, X, Y);  
   if Moving then
   begin
      ReleaseCapture;
      Moving := False;
      Screen.Cursor := crDefault;
   end;
end;

{==============================================================================}
procedure TPlasmaForm.LoadMaskFromFile(Value:String);
var Reader:TFileStream;
    Data  :PRgnData;
begin
try
  Reader:=TFileStream.Create(Value,fmOpenRead);
  GetMem(Data,Reader.Size);
  Reader.Read(Data^,Reader.Size);
	FRegion.Region:=ExtCreateRegion(nil,Reader.Size,Data^);
  FreeMem(Data,Reader.Size);
  Reader.free;
  RefreshForm;
except
   raise Exception.Create('Error Loading Mask');
  	end;
end;


//to use number-> resname = #number like #128;
procedure TPlasmaForm.LoadMaskFromResource(Instance:THandle;ResName:string);
var Reader:TResourceStream;
    Data  :PRgnData;
begin
try
  Reader:=TResourceStream.Create(Instance, ResName, RT_RCDATA);
	GetMem(Data,Reader.Size);
	Reader.Read(Data^,Reader.Size);
	FRegion.Region:=ExtCreateRegion(nil,Reader.Size,Data^);
  FreeMem(Data,Reader.Size);
  Reader.free;
  RefreshForm;
except
   raise Exception.Create('Error Loading Mask form resource');
  	end;
end;

procedure TPlasmaForm.CreateMaskFromColor(_TransparentColor: TColor);
var
   RStart    :Integer;
   REnd      :Integer;
   x,y       :Integer;
//   P         :PDWordArray;
   tmpBitmap :TBitmap;
   tmpBitmapCanvas :TCanvas;
   currentColor :TColor;

    procedure PaintRegion(x1,x2,_y:Integer;Color1,Color2:TColor);
    var
       k:Integer;
    begin
        for k:=x1 to x2 do
          if (Odd(k div 7)) xor (Odd(_y div 7))
          then Picture.Bitmap.Canvas.Pixels[k,_y]:=Color1
          else Picture.Bitmap.Canvas.Pixels[k,_y]:=Color2;
        Application.ProcessMessages;
    end;

    procedure AddRegion(x1,x2,_y:Integer);
    var
       Aux:HRgn;

    begin
         If FRegion.Region=0
         then FRegion.Region:=CreateRectRgn(x1,_y,x2+1,_y+1)
         else begin
                   Aux:=CreateRectRgn(x1,y,x2+1,_y+1);
                   CombineRgn(FRegion.Region,FRegion.Region,Aux,RGN_OR);
                   DeleteObject(Aux);
               end;
     end;

begin
     FRegion.Free;
     FRegion :=TPlasmaRegion.Create(Self);
     FRegion.Region :=0;
     Application.ProcessMessages;

     tmpBitmap :=MakeMask(Picture.Bitmap, _TransparentColor);
     if (tmpBitmap=Nil) then Exit;

     tmpBitmapCanvas :=tmpBitmap.Canvas; //Opt.
     for y := 0 to tmpBitmap.Height -1 do
     begin
          RStart:=-1;
          //REnd:=-1;
          for x := 0 to tmpBitmap.Width -1 do
          begin
               currentColor :=tmpBitmapCanvas.Pixels[x, y];
               If ((currentColor=clBlack) or
                   (x=tmpBitmap.Width-1)) and
                   (RStart=-1) then
               begin
                    RStart:=x;
                end;

               If ((currentColor<>clBlack) or
                   (x=tmpBitmap.Width-1)) and
                   (RStart<>-1) then
               begin
                    REnd:=x-1;
                    AddRegion(RStart,REnd,y);
                    RStart:=-1;
                end;
           end;
    end;
    tmpBitmap.Free;

(*   Faster but the conversion to 32bit not work correctly in Win98
     Picture.Bitmap.PixelFormat :=pf32bit;
     for y := 0 to Picture.Height -1 do
     begin
          P := Picture.Bitmap.ScanLine[y];
          RStart:=-1;
          REnd:=-1;
          for x := 0 to Picture.Width -1 do
          begin
               If ((P[x]<>_TransparentColor) or
                   (x=Picture.Width-1)) and
                   (RStart=-1) then
               begin
                    RStart:=x;
                end;

               If ((P[x]=_TransparentColor) or
                   (x=Picture.Width-1)) and
                   (RStart<>-1) then
               begin
                    REnd:=x-1;
                    AddRegion(RStart,REnd,y);
                    RStart:=-1;
                end;
           end;
    end;
*)
   RefreshForm;
end;

end.
