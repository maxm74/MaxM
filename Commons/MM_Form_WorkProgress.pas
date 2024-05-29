//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2015                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_Form_WorkProgress.pas
//
//  Description : Global On Top Progress Form. (Wait Please :-o )
//
//******************************************************************************

unit MM_Form_WorkProgress;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, AsyncProcess;

{ TWorkProgress }

type
  TWorkProgress = class(TForm)
    lbCaption: TLabel;
    lbTitle: TLabel;
    Panel1: TPanel;
    pBar: TProgressBar;
  private
    { private declarations }
  public
    { public declarations }
  end;

  {$ifdef USE_Threads}
  { TWorkProgressThread }

  TWorkProgressThread = class(TThread)
  public
     constructor Create;
     destructor Destroy; override;
     procedure Execute; override;
  end;
  {$endif}

var
  WorkProgress: TWorkProgress =nil;

procedure WorkProgressShow(ACaption :String; ATitle: String=''; BarMarquee :Boolean=True; BarSmooth :Boolean=True);
procedure WorkProgressClose;

implementation

{$ifdef USE_Threads}
var
   WorkProgressThread :TWorkProgressThread = nil;
{$endif}

procedure WorkProgressShow(ACaption: String; ATitle: String=''; BarMarquee: Boolean=True; BarSmooth: Boolean=True);
begin
  if (WorkProgress = nil)
  then WorkProgress :=TWorkProgress.Create(Application);

  if (ATitle <> '')
  then WorkProgress.lbTitle.Caption := ATitle;
  WorkProgress.lbCaption.Caption := ACaption;
  if BarMarquee
  then WorkProgress.pBar.Style := pbstMarquee
  else WorkProgress.pBar.Style := pbstNormal;
  WorkProgress.pBar.Smooth := BarSmooth;
  WorkProgress.Show;
  Screen.Cursor :=crHourGlass;
  WorkProgress.Update;
  Application.ProcessMessages;

  {$ifdef USE_Threads}
  if (WorkProgressThread = nil)
  then WorkProgressThread :=TWorkProgressThread.Create;
  {$endif}
end;

procedure WorkProgressClose;
begin
  Screen.Cursor := crDefault;

  {$ifdef USE_Threads}
  if (WorkProgressThread <> nil)
  then begin
            WorkProgressThread.Terminate;
            FreeAndNil(WorkProgressThread);
        end;
  {$endif}
  if (WorkProgress <> nil) then FreeAndNil(WorkProgress);
end;

{$R *.lfm}

{$ifdef USE_Threads}
{ TWorkProgressThread }

constructor TWorkProgressThread.Create;
begin
  inherited Create(False);
end;

destructor TWorkProgressThread.Destroy;
begin
  inherited Destroy;
end;

procedure TWorkProgressThread.Execute;
var
   pp :Integer;

begin
     while not(Terminated) do
     begin
          if (WorkProgress <> nil)
          then begin
                   (* pp :=WorkProgress.pBar.Position;
                    if WorkProgress.pBar.Smooth
                    then begin
                              WorkProgress.pBar.Smooth :=False;
                              //WorkProgress.pBar.Position :=pp+1;
                              //WorkProgress.pBar.StepIt;
                              WorkProgress.pBar.Smooth :=True;
                          end
                    else WorkProgress.pBar.StepIt; *)
                    WorkProgress.pBar.Update;
                    Application.ProcessMessages;
                    Self.Sleep(150);
                end
          else Terminate;
     end;
end;
{$endif}

end.

