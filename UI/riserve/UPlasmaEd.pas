unit UPlasmaEd;

interface
//{$include Conditionals.inc}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls , Buttons, StdCtrls, UPlasmaRegion, Math,
     {$ifdef DELPHI_4}
       DsgnIntf;
     {$else}
       DesignIntf, DesignEditors;
     {$endif}

type
    TPlasmaMaskProperty=class(TPropertyEditor)
                        private
                         function  GetAttributes:TPropertyAttributes;override;
                         function  GetValue:string;override;
                         procedure Edit;override;
                        end;

implementation

uses UPlasmaEd_Form;

{==========================Property Editor=====================================}
function TPlasmaMaskProperty.GetAttributes:TPropertyAttributes;
begin
Result:=Inherited GetAttributes + [paDialog];
end;

function TPlasmaMaskProperty.GetValue:string;
begin
If TPlasmaRegion(GetOrdValue).Region=0 then
   Result:='(None)'
  else
   Result:='(Mask)';
end;

procedure TPlasmaMaskProperty.Edit;
var Mf :TMaskForm;
begin
Mf:=TMaskForm.Create(Application);
Mf.Bitmap:=TPlasmaRegion(GetOrdvalue).Owner.Picture.Bitmap;
Mf.Region:=TPlasmaRegion(GetOrdvalue).Region;
If Mf.Execute=mrOk then
   begin
   TPlasmaRegion(GetOrdvalue).Region:=Mf.Region;
   Modified;
   end;
Mf.free;
end;

end.
