//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2005-2015                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_AsyncCall.pas
//
//  Description : Call a Method Asyncronous (Useful for solve some LCL GUI Bug).
//
//******************************************************************************

unit MM_AsyncCall;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure CallAsync(ASyncMtd :TNotifyEvent; ASyncMtd_Sender: TObject=nil; ATime :Cardinal=100);
procedure FreeAsync;

implementation

uses ExtCtrls;

type
    { TAsyncComp }
    TAsyncComp = class(TTimer)
    public
      SyncMtd :TNotifyEvent;
      SyncMtd_Sender: TObject;

      procedure AsyncClickTimer(Sender: TObject);
    end;

var
   AsyncComp :TAsyncComp = nil;

{ TAsyncComp }

procedure TAsyncComp.AsyncClickTimer(Sender: TObject);
begin
     Self.Enabled :=False;
     if Assigned(SyncMtd)
     then SyncMtd(SyncMtd_Sender);
end;

procedure CallAsync(ASyncMtd: TNotifyEvent; ASyncMtd_Sender: TObject; ATime: Cardinal);
begin
   if (AsyncComp = nil)
   then AsyncComp :=TAsyncComp.Create(Nil);

   if (AsyncComp <> nil) then
   begin
        AsyncComp.Enabled := False;
        AsyncComp.Interval := ATime;
        AsyncComp.SyncMtd := ASyncMtd;
        AsyncComp.SyncMtd_Sender := ASyncMtd_Sender;
        AsyncComp.OnTimer := @AsyncComp.AsyncClickTimer;
        AsyncComp.Enabled := True;
    end;
end;

procedure FreeAsync;
begin
   if (AsyncComp <> nil)
   then FreeAndNil(AsyncComp);
end;

finalization
   FreeAsync;

end.

