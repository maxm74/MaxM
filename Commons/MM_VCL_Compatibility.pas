//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2013                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_VCL_Compatibility.pas
//
//  Description : Implementation of Some functions not in FCL.
//
//******************************************************************************

unit MM_VCL_Compatibility;
{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils;

function StripHotKey(AHotKey :String):String;

implementation

function StripHotKey(AHotKey :String):String;
Var
   i :Integer;

begin
     Result :=AHotKey;
     repeat
           i :=Pos('&', Result);
           if (i > 0)
           then Delete(Result, i, 1);
     until (i = 0);
end;

end.

