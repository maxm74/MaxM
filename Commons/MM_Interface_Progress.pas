(*******************************************************************************
**                          MaxM Commons Package                              **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**                                                                            **
**   File: MM_Interface_Progress.pas                                          **
**                                                                            **
**   Interfaces to control a Progress Bar                                     **
*******************************************************************************)
unit MM_Interface_Progress;

{$mode objfpc}{$H+}

interface

type
  IMM_ProgressCallback = interface
  ['{D101B01A-FAD0-C666-CADE-2344BF2350B0}']
    procedure ProgressCancelClick(ATotalValue, ACurrentValue: Integer); stdcall;
  end;

  IMM_Progress = interface
  ['{D101B01A-FAD0-C666-CADE-2344BF2350B1}']
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
  end;

implementation

end.

