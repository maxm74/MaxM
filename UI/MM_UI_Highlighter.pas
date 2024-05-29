unit MM_UI_Highlighter;
{$mode objfpc}
{$H+}

interface
uses
  SysUtils, Classes, ExtCtrls, Controls, Graphics, Forms, MM_UI_Consts;

type
  TMM_UI_HighlighterCanProcess = function (isActive :Boolean): Boolean of object;
  TMM_UI_HighlighterGetStdColor = function : TColor of object;

  TMM_UI_HighlighterEvent = procedure (Sender :TObject;
                                   CurrentActive, OldActive :TWinControl;
                                   var CanProcessCurrent, CanProcessOld :Boolean) of object;

  { TMM_UI_Highlighter }

  TMM_UI_Highlighter = class(TTimer)
  protected
    CurrentActive,
    OldActive      :TWinControl;
    OldActiveColor :TColor;
    rOnHighlight   :TMM_UI_HighlighterEvent;
    rHighlightColor :TColor;

    function GetRealMessageHandler(Recevier :TWinControl): TWinControl; virtual;
    procedure TimerEvent(Sender: TObject);

    property OnTimer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Interval default 150;
    property HighlightColor :TColor read rHighlightColor write rHighlightColor
                                    default $CDFFFF; //Light Yellow.

    property OnHighlight :TMM_UI_HighlighterEvent read rOnHighlight write rOnHighlight;
  end;

procedure Register;

implementation

type
    TWinCTRLAccess = Class(TWinControl)
    end;

procedure Register;
begin
     RegisterComponents('MaxM_UI', [TMM_UI_Highlighter]);
end;


function TMM_UI_Highlighter.GetRealMessageHandler(Recevier: TWinControl): TWinControl;
begin
     if Recevier.ClassNameIs('TEbEdit')
     then Result := Recevier.Parent
     else Result := Recevier;
end;

procedure TMM_UI_Highlighter.TimerEvent(Sender: TObject);
var
   CanProcessCurrent, CanProcessOld :Boolean;
   mtd_CanProcess  :TMM_UI_HighlighterCanProcess;
   mtd_GetStdColor :TMM_UI_HighlighterGetStdColor;
   curColor :TColor;

begin
     CurrentActive :=TForm(Owner).ActiveControl;
     if (CurrentActive <> OldActive)
     then begin
               CanProcessCurrent :=True;
               CanProcessOld :=True;
               if Assigned(rOnHighlight)
               then rOnHighlight(Self, CurrentActive, OldActive, CanProcessCurrent, CanProcessOld);

               if (OldActive <> Nil)
               then begin
                         mtd_CanProcess :=TMM_UI_HighlighterCanProcess(FindMethod(GetRealMessageHandler(OldActive), mtd_MM_UI_HighlighterCanProcess));
                         if Assigned(mtd_CanProcess)
                         then CanProcessOld :=mtd_CanProcess(False);

                         //CanProcessOld :=Boolean(GetRealMessageHandler(OldActive).Perform(LMUI_HighlighterCanProcess, 0, 0));

                         //Restore Default color
                         if CanProcessOld
                         then begin
                                   mtd_GetStdColor :=TMM_UI_HighlighterGetStdColor(FindMethod(GetRealMessageHandler(OldActive), mtd_MM_UI_HighlighterGetStdColor));
                                   if Assigned(mtd_GetStdColor)
                                   then curColor :=mtd_GetStdColor()
                                   else curColor :=OldActiveColor;

                                   TWinCTRLAccess(OldActive).Color :=curColor;
                              end;
               end;

               if (CurrentActive <> Nil)
               then begin
                         mtd_CanProcess :=TMM_UI_HighlighterCanProcess(FindMethod(GetRealMessageHandler(CurrentActive), mtd_MM_UI_HighlighterCanProcess));
                         if Assigned(mtd_CanProcess)
                         then CanProcessCurrent :=mtd_CanProcess(True);

                         //CanProcessCurrent :=Boolean(GetRealMessageHandler(CurrentActive).Perform(LMUI_HighlighterCanProcess, 1, 0));

                         OldActive :=CurrentActive;

                         if CanProcessCurrent
                         then begin
                                   //Color to Yellow
                                   mtd_GetStdColor :=TMM_UI_HighlighterGetStdColor(FindMethod(GetRealMessageHandler(CurrentActive), mtd_MM_UI_HighlighterGetStdColor));
                                   if Assigned(mtd_GetStdColor)
                                   then OldActiveColor :=mtd_GetStdColor()
                                   else OldActiveColor :=TWinCTRLAccess(CurrentActive).Color;

                                   TWinCTRLAccess(CurrentActive).Color :=rHighlightColor;
                              end;
                    end;
          end;
end;

constructor TMM_UI_Highlighter.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     OldActive :=Nil;
     OldActiveColor :={$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
     Interval :=150;
     rHighlightColor :=$A6FFFF;
     OnTimer :=@TimerEvent;
end;

end.

