(*******************************************************************************
**                                  DigIt                                     **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**   Progress Form ( Implementation of IDigIt_Progress )                      **
*******************************************************************************)

unit MM_Form_Progress;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, Buttons,
  MM_Interface_Progress,
  BGRAFlashProgressBar;

type

  { TMMForm_Progress }

  TMMForm_Progress = class(TForm, IMM_Progress)
    progressTotal: TBGRAFlashProgressBar;
    btCancel: TBitBtn;
    capTotal: TLabel;
    labTotal: TLabel;
    capCurrent: TLabel;
    panelCancel: TPanel;
    panelTotal: TPanel;
    panelCurrent: TPanel;
    progressCurrent: TBGRAFlashProgressBar;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    curEventCallBack: IMM_ProgressCallback;
    rOnCancelClick: TNotifyEvent;
    rCancelled: Boolean;

  public
    //IDigIt_Progress implementation
    procedure SetTotalVisible(AVisible: Boolean); stdcall;
    procedure SetTotalLabel(const ALabel: PChar); stdcall;
    procedure SetTotal(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetTotalCaption(const ACaption: PChar); stdcall;
    procedure SetTotalValue(AValue: Integer); stdcall;

    procedure SetCurrentVisible(AVisible: Boolean); stdcall;
    procedure SetCurrent(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
    procedure SetCurrentCaption(const ACaption: PChar); stdcall;
    procedure SetCurrentValue(AValue: Integer); stdcall;

    function Cancelled: Boolean; stdcall;

    procedure SetEventCallBack(const AEventCallBack: IMM_ProgressCallback); stdcall;

    procedure Show(const ACaption: PChar; CancelVisible: Boolean=True); stdcall;
    procedure ShowWaiting(const ACaption: PChar; CancelVisible: Boolean=False); stdcall;
    procedure Hide; stdcall;

    //Useful functions
    procedure ProgressShow(ACaption: String; TotalMin, TotalMax: Integer; AStyle: TBGRAPBarStyle=pbstNormal); overload;
    procedure ProgressShow(ACaption: String; TotalMin, TotalMax, CurrentMin, CurrentMax: Integer;
                           ATotalStyle: TBGRAPBarStyle=pbstNormal; ACurrentStyle: TBGRAPBarStyle=pbstNormal); overload;

    function ProgressSetTotal(TotalCaption: String; TotalMin, TotalMax, TotalVal: Integer): Boolean; overload;
    function ProgressSetTotal(TotalCaption: String; TotalVal: Integer): Boolean; overload;

    function ProgressSetCurrent(CurrentCaption: String; CurrentVal: Integer): Boolean; overload;
    function ProgressSetCurrent(CurrentCaption: String; CurrentMin, CurrentMax, CurrentVal: Integer): Boolean; overload;

    property OnCancelClick: TNotifyEvent read rOnCancelClick write rOnCancelClick;
  end;

var
  MMForm_Progress: TMMForm_Progress=nil;

implementation

{$R *.lfm}

{ TMMForm_Progress }

procedure TMMForm_Progress.FormCreate(Sender: TObject);
begin
  rCancelled:= False;
  curEventCallBack:= nil;
  {$ifopt D+}
    FormStyle:= fsNormal;
  {$endif}
end;

procedure TMMForm_Progress.btCancelClick(Sender: TObject);
begin
  rCancelled:= True;

  if (curEventCallBack <> nil)
  then curEventCallBack.ProgressCancelClick(progressTotal.Position, progressCurrent.Position);

  if Assigned(rOnCancelClick)
  then rOnCancelClick(Self);
end;

procedure TMMForm_Progress.SetTotalVisible(AVisible: Boolean); stdcall;
begin
  panelTotal.Visible:= AVisible;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetTotalLabel(const ALabel: PChar); stdcall;
begin
  labTotal.Caption:= ALabel;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetTotal(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
begin
  if isMarquee
  then progressTotal.Style:= pbstMarquee
  else progressTotal.Style:= pbstNormal;

  progressTotal.MinValue:= AMin;
  progressTotal.MaxValue:= AMax;
  progressTotal.Value:= AValue;
  panelTotal.Visible:= True;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetTotalCaption(const ACaption: PChar); stdcall;
begin
  capTotal.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetTotalValue(AValue: Integer); stdcall;
begin
  progressTotal.Value:= AValue;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetCurrentVisible(AVisible: Boolean); stdcall;
begin
  panelCurrent.Visible:= AVisible;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetCurrent(AMin, AMax, AValue: Integer; isMarquee: Boolean); stdcall;
begin
  if isMarquee
  then progressCurrent.Style:= pbstMarquee
  else progressCurrent.Style:= pbstNormal;

  progressCurrent.MinValue:= AMin;
  progressCurrent.MaxValue:= AMax;
  progressCurrent.Value:= AValue;
  panelCurrent.Visible:= True;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetCurrentCaption(const ACaption: PChar); stdcall;
begin
  capCurrent.Caption:= ACaption;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.SetCurrentValue(AValue: Integer); stdcall;
begin
  progressCurrent.Value:= AValue;
  Application.ProcessMessages;
end;

function TMMForm_Progress.Cancelled: Boolean; stdcall;
begin
  Application.ProcessMessages;
  Result:= rCancelled;
end;

procedure TMMForm_Progress.SetEventCallBack(const AEventCallBack: IMM_ProgressCallback); stdcall;
begin
  curEventCallBack:= AEventCallBack;
end;

procedure TMMForm_Progress.Show(const ACaption: PChar; CancelVisible: Boolean=True); stdcall;
begin
  BorderIcons:= [biSystemMenu];
  BorderStyle:= bsSingle;
  panelCancel.Visible:= CancelVisible;

  rCancelled:= False;
  Caption:= ACaption;
  capTotal.Caption:= '';
  labTotal.Caption:= '';
  capCurrent.Caption:= '';
  Visible:= True;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.ShowWaiting(const ACaption: PChar; CancelVisible: Boolean=False); stdcall;
begin
  BorderIcons:= [];
  BorderStyle:= bsNone;
  panelCancel.Visible:= CancelVisible;
  progressTotal.Style:= pbstMarquee;
  progressTotal.MinValue:= 0;
  progressTotal.MaxValue:= 100;
  progressTotal.Value:= 0;
  panelCurrent.Visible:= False;

  rCancelled:= False;
  capTotal.Caption:= '';
  labTotal.Caption:= ACaption;
  Visible:= True;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.Hide; stdcall;
begin
  rCancelled:= False;
  Visible:= False;
  Application.ProcessMessages;
end;

procedure TMMForm_Progress.ProgressShow(ACaption: String; TotalMin, TotalMax: Integer; AStyle: TBGRAPBarStyle);
begin
  rCancelled:= False;
  labTotal.Caption:= '';
  capTotal.Caption:= '';
  progressTotal.Style:= AStyle;
  progressTotal.Min:= TotalMin;
  progressTotal.Max:= TotalMax;
  progressTotal.Position:= TotalMin;
  panelCurrent.Visible:= False;
  Show(PChar(ACaption));
end;

procedure TMMForm_Progress.ProgressShow(ACaption: String; TotalMin, TotalMax, CurrentMin, CurrentMax: Integer;
                                       ATotalStyle: TBGRAPBarStyle; ACurrentStyle: TBGRAPBarStyle);
begin
  rCancelled:= False;
  labTotal.Caption:= '';
  capTotal.Caption:= '';
  progressTotal.Style:= ATotalStyle;
  progressTotal.Min:= TotalMin;
  progressTotal.Max:= TotalMax;
  progressTotal.Position:= TotalMin;
  progressCurrent.Style:= ACurrentStyle;
  progressCurrent.Min:= CurrentMin;
  progressCurrent.Max:= CurrentMax;
  progressCurrent.Position:= CurrentMin;
  panelCurrent.Visible:= True;
  Show(PChar(ACaption));
end;


function TMMForm_Progress.ProgressSetTotal(TotalCaption: String; TotalVal: Integer): Boolean;
begin
  Result:= rCancelled;
  if rCancelled then exit;

  progressTotal.Position:= TotalVal;
  capTotal.Caption:= TotalCaption;
  Application.ProcessMessages;
  Result:= rCancelled;
end;

function TMMForm_Progress.ProgressSetTotal(TotalCaption: String; TotalMin, TotalMax, TotalVal: Integer): Boolean;
begin
  Result:= rCancelled;
  if rCancelled then exit;

  progressTotal.Min:= TotalMin;
  progressTotal.Max:= TotalMax;
  progressTotal.Position:= TotalVal;
  capTotal.Caption:= TotalCaption;
  Application.ProcessMessages;
  Result:= rCancelled;
end;

function TMMForm_Progress.ProgressSetCurrent(CurrentCaption: String; CurrentVal: Integer): Boolean;
begin
  Result:= rCancelled;
  if rCancelled then exit;

  progressCurrent.Position:= CurrentVal;
  capCurrent.Caption:= CurrentCaption;
  Application.ProcessMessages;
  Result:= rCancelled;
end;

function TMMForm_Progress.ProgressSetCurrent(CurrentCaption: String; CurrentMin, CurrentMax, CurrentVal: Integer): Boolean;
begin
  Result:= rCancelled;
  if rCancelled then exit;

  progressCurrent.Min:= CurrentMin;
  progressCurrent.Max:= CurrentMax;
  progressCurrent.Position:= CurrentVal;
  capCurrent.Caption:= CurrentCaption;
  Application.ProcessMessages;
  Result:= rCancelled;
end;

end.

