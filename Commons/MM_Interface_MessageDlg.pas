(*******************************************************************************
**                          MaxM Commons Package                              **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**                                                                            **
**   File: MM_Interface_MessageDlg.pas                                        **
**                                                                            **
**   interfaces to control a Dialog Box                                       **
*******************************************************************************)
unit MM_Interface_MessageDlg;

{$mode objfpc}{$H+}

interface

uses System.UITypes;

const
  // Aliases for enum values in UITypes.
  mtWarning      = System.UITypes.TMsgDlgType.mtWarning;
  mtError        = System.UITypes.TMsgDlgType.mtError;
  mtInformation  = System.UITypes.TMsgDlgType.mtInformation;
  mtConfirmation = System.UITypes.TMsgDlgType.mtConfirmation;
  mtCustom       = System.UITypes.TMsgDlgType.mtCustom;

  mbYes      = System.UITypes.TMsgDlgBtn.mbYes;
  mbNo       = System.UITypes.TMsgDlgBtn.mbNo;
  mbOK       = System.UITypes.TMsgDlgBtn.mbOK;
  mbCancel   = System.UITypes.TMsgDlgBtn.mbCancel;
  mbAbort    = System.UITypes.TMsgDlgBtn.mbAbort;
  mbRetry    = System.UITypes.TMsgDlgBtn.mbRetry;
  mbIgnore   = System.UITypes.TMsgDlgBtn.mbIgnore;
  mbAll      = System.UITypes.TMsgDlgBtn.mbAll;
  mbNoToAll  = System.UITypes.TMsgDlgBtn.mbNoToAll;
  mbYesToAll = System.UITypes.TMsgDlgBtn.mbYesToAll;
  mbHelp     = System.UITypes.TMsgDlgBtn.mbHelp;
  mbClose    = System.UITypes.TMsgDlgBtn.mbClose;

  // Combinations of buttons.
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesNo = [mbYes, mbNo];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];

  // Used for ModalResult
  mrNone = System.UITypes.mrNone;
  mrOK = System.UITypes.mrOK;
  mrCancel = System.UITypes.mrCancel;
  mrAbort = System.UITypes.mrAbort;
  mrRetry =System.UITypes.mrRetry;
  mrIgnore = System.UITypes.mrIgnore;
  mrYes = System.UITypes.mrYes;
  mrNo = System.UITypes.mrNo;
  mrAll = System.UITypes.mrAll;
  mrNoToAll = System.UITypes.mrNoToAll;
  mrYesToAll = System.UITypes.mrYesToAll;
  mrClose = System.UITypes.mrClose;
  {$IF FPC_FULLVERSION>=30203}
  mrContinue= System.UITypes.mrContinue;
  mrTryAgain= System.UITypes.mrTryAgain;
  {$ENDIF}
  mrLast = System.UITypes.mrLast;

type
  // Aliases for types in UITypes.
  TMsgDlgType    = System.UITypes.TMsgDlgType;
  TMsgDlgBtn     = System.UITypes.TMsgDlgBtn;
  TMsgDlgButtons = System.UITypes.TMsgDlgButtons;
  TModalResult = System.UITypes.TModalResult;
  PModalResult = System.UITypes.PModalResult;

  IMM_MessageDlg = interface
  ['{D101B01A-FAD0-C666-CADE-2344BF2350B2}']
    function MessageDlg(const aMsg: PChar; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; stdcall; overload;
    function MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; stdcall; overload;
    function MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; stdcall; overload;
    function MessageDlg(const aMsg: PChar; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; stdcall; overload;
    function MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                        Buttons: TMsgDlgButtons; const HelpKeyword: PChar): TModalResult; stdcall; overload;
    function QuestionDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                Buttons: array of const; HelpCtx: Longint): TModalResult; stdcall; overload;
    function QuestionDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                Buttons: array of const; const HelpKeyword: PChar): TModalResult; stdcall; overload;
  end;

implementation

end.

