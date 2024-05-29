unit UPlasmaRegion;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls , Buttons, StdCtrls;

type
    TPlasmaRegion  = class(TPersistent)
                     private
                      FRegion    :Hrgn;
               		     FOwner     :TImage;
                      procedure   SetRegion(Value:HRgn);
                     protected
                      procedure   ReadMask(Reader: TStream);
                      procedure   WriteMask(Writer: TStream);
                      procedure   DefineProperties(Filer: TFiler);override;
                     public
                      procedure   SetRegionByData(Value:PRgnData;Size:Integer);
                      constructor Create(AOwner:TImage);
                      destructor  Destroy;override;
                      property    Owner:TImage read FOwner;
                      property    Region:HRgn read FRegion write SetRegion;
                     end;


implementation

{===========================TPlasmaRegion======================================}
constructor TPlasmaRegion.Create(AOwner:TImage);
begin
inherited Create;
FOwner:=AOwner;
end;

destructor TPlasmaRegion.Destroy;
begin
If FRegion<>0 then DeleteObject(FRegion);
inherited destroy;
end;

procedure TPlasmaRegion.SetRegionByData(Value:PRgnData;Size:Integer);
begin
If FRegion<>0 then DeleteObject(FRegion);
If Value=nil then
   FRegion:=0
  else
   FRegion:=ExtCreateRegion(nil,Size,Value^);
end;

procedure TPlasmaRegion.SetRegion(Value:HRgn);
var p:PRgnData;
    s:Integer;
begin
If FRegion<>0 then DeleteObject(FRegion);
If Value=0 then
   FRegion:=0
  else
   begin
   s:=GetRegionData(Value,0,nil);
   GetMem(p,s);
   GetRegionData(Value,s,p);
   FRegion:=ExtCreateRegion(nil,s,p^);
   FreeMem(p,s);
   end;
end;

procedure TPlasmaRegion.ReadMask(Reader: TStream);
var Data:PRgnData;
    Size:Integer;
begin
Reader.Read(Size,Sizeof(Size));
if Size>0 then
  	begin
 		GetMem(Data,Size);
  	Reader.Read(Data^,Size);
 		SetRegionByData(Data,Size);
   FreeMem(Data,Size);
   end
  else
   Region:=0;
end;

procedure TPlasmaRegion.WriteMask(Writer: TStream);
var Size		 : Integer;
  	 Data	  : PRgnData;
begin
Size:=GetRegionData(Region,0,nil);
Writer.Write(Size,SizeOf(Size));
if (Size>0) then
  	begin
  	GetMem(Data,Size);
  	GetRegionData(Region,Size,Data);
  	Writer.Write(Data^,Size);
  	FreeMem(Data,Size);
  	end;
end;

procedure TPlasmaRegion.DefineProperties(Filer: TFiler);
begin
inherited DefineProperties(Filer);
Filer.DefineBinaryProperty('Data',ReadMask,WriteMask,true);
end;


end.
