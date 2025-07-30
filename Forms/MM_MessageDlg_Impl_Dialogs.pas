(*******************************************************************************
**                      MaxM Commons Forms Package                            **
**                                                                            **
**          (s) 2025 Massimo Magnano                                          **
**                                                                            **
********************************************************************************
**                                                                            **
**   File: MM_MessageDlg_Impl_Dialogs.pas                                     **
**                                                                            **
**   Implementation of IMM_MessageDlg using LCL Dialogs                       **
*******************************************************************************)
unit MM_MessageDlg_Impl_Dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, testutils,
  MM_Interface_MessageDlg;

type
  { TMM_MessageDlg_Impl_Dialogs }

  TMM_MessageDlg_Impl_Dialogs = class(TNoRefCountObject, IMM_MessageDlg)
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

    procedure Cursor(ACursor: TCursor); stdcall;
  end;

var
   MessageDlg_Impl_Dialogs: TMM_MessageDlg_Impl_Dialogs=nil;

implementation

uses Dialogs, Forms;

{ TMM_MessageDlg_Impl_Dialogs }

function TMM_MessageDlg_Impl_Dialogs.MessageDlg(const aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.MessageDlg(aMsg, DlgType, Buttons, HelpCtx);
end;

function TMM_MessageDlg_Impl_Dialogs.MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.MessageDlg(aCaption, aMsg, DlgType, Buttons, HelpCtx);
end;

function TMM_MessageDlg_Impl_Dialogs.MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.MessageDlg(aCaption, aMsg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function TMM_MessageDlg_Impl_Dialogs.MessageDlg(const aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.MessageDlg(aMsg, DlgType, Buttons, HelpCtx, DefaultButton);
end;

function TMM_MessageDlg_Impl_Dialogs.MessageDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: TMsgDlgButtons; const HelpKeyword: PChar): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.MessageDlg(aCaption, aMsg, DlgType, Buttons, HelpKeyword);
end;

function TMM_MessageDlg_Impl_Dialogs.QuestionDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: array of const; HelpCtx: Longint): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.QuestionDlg(aCaption, aMsg, DlgType, Buttons, HelpCtx);
end;

function TMM_MessageDlg_Impl_Dialogs.QuestionDlg(const aCaption, aMsg: PChar; DlgType: TMsgDlgType;
                                                Buttons: array of const; const HelpKeyword: PChar): TModalResult; stdcall;
begin
  Screen.Cursor:= crDefault;
  Result:= Dialogs.QuestionDlg(aCaption, aMsg, DlgType, Buttons, HelpKeyword);
end;

procedure TMM_MessageDlg_Impl_Dialogs.Cursor(ACursor: TCursor); stdcall;
begin
  Screen.Cursor:= crDefault;
  Screen.Cursor:= ACursor;
end;

end.

