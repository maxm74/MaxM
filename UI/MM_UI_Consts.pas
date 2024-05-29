unit MM_UI_Consts;

{$mode objfpc}{$H+}

interface

uses
  LMessages;

const
     LMUI_Message = LM_USER + 6000;

     mtd_MM_UI_HighlighterCanProcess  = 'MM_UI_HighlighterCanProcess'; //function (isActive :Boolean): Boolean;
     mtd_MM_UI_HighlighterGetStdColor = 'MM_UI_HighlighterGetStdColor'; //function : TColor;


function FindMethod(AObject :TObject; AName :shortstring):TMethod;

implementation

function FindMethod(AObject :TObject; AName :shortstring):TMethod;
var
   mtdCode :Pointer;

begin
     Result.Code :=AObject.MethodAddress(AName);
     if (Result.Code <> Nil)
     then Result.Data :=Pointer(AObject)
     else Result.Data :=Nil;
end;

end.

