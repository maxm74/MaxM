//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2013                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_DelphiCompatibility.pas
//
//  Description : Implementation of Some functions not in FCL.
//
//******************************************************************************

unit MM_DelphiCompatibility;
{$mode objfpc}
{$H+}

interface

{$ifndef fpc}
type
{$ifdef CPUX64}
  PtrInt = Int64;
  PtrUInt = UInt64;
{$else}
  PtrInt = longint;
  PtrUInt = Longword;
{$endif}
const
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
  DirectorySeparator = '\';
{$endif}

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

