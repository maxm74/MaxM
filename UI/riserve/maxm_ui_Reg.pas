////////////////////////////////////////////////////////////////////////////////////////
//TJetCheckbox etc.. v1.00 for Delphi32. (C) 1999 Jimmy Theo Weku.'+#10#13+
//for more information about how to use this component please read README.TXT that
//included with this components
////////////////////////////////////////////////////////////////////////////////////////
// 2003 Massimo Magnano

unit maxm_ui_Reg;
//{$Include Conditionals.inc}

interface

uses Classes, 
     {$ifdef NO_DSGNINTF}
       DesignIntf, DesignEditors,
     {$else}
       DsgnIntf,
     {$endif}
     UjetButton, ujetanimbutton, UJetCheckbox,
     UPlasmaForm, UPlasmaEd, UPlasmaRegion,
     XPMenu, UIHighlighter;

procedure Register;

implementation

procedure Register;
begin
  	RegisterPropertyEditor(TypeInfo(TPlasmaRegion),nil,'',TPlasmaMaskProperty);
  	RegisterComponents('MaxM_UI', [TJetButton, TJetAnimButton, TJetCheckbox, TPlasmaForm, TXPMenu, TUIHighlighter]);
end;


end.
