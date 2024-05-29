{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MaxM_UI;

{$warn 5023 off : no warning about unused units}
interface

uses
  MM_UI_Highlighter, MM_UI_DBGrid, MM_UI_ComboBox, MM_UI_DBValuesComboBox, MM_UI_DBDateEdit, MM_UI_Consts, 
  MM_UI_AutoComplete, MM_UI_DBAutoComplete, MM_UI_DBColorBox, MM_UI_EnumFiles, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MM_UI_Highlighter', @MM_UI_Highlighter.Register);
  RegisterUnit('MM_UI_DBGrid', @MM_UI_DBGrid.Register);
  RegisterUnit('MM_UI_ComboBox', @MM_UI_ComboBox.Register);
  RegisterUnit('MM_UI_DBValuesComboBox', @MM_UI_DBValuesComboBox.Register);
  RegisterUnit('MM_UI_DBDateEdit', @MM_UI_DBDateEdit.Register);
  RegisterUnit('MM_UI_AutoComplete', @MM_UI_AutoComplete.Register);
  RegisterUnit('MM_UI_DBAutoComplete', @MM_UI_DBAutoComplete.Register);
  RegisterUnit('MM_UI_DBColorBox', @MM_UI_DBColorBox.Register);
  RegisterUnit('MM_UI_EnumFiles', @MM_UI_EnumFiles.Register);
end;

initialization
  RegisterPackage('MaxM_UI', @Register);
end.
