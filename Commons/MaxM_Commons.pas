{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MaxM_Commons;

{$warn 5023 off : no warning about unused units}
interface

uses
  MM_IniFiles, MM_StrUtils, MM_VCL_Compatibility, MM_DB_FirebirdUtils, MM_VariantsStringList, MM_DB_Utils, 
  MM_Form_WorkProgress, MM_AsyncCall, MM_StringListList, MM_Form_DatasetDiff, MM_FilesUtils, MM_GeometricalUtils, 
  MM_Form_Special_Chars, MM_Special_Chars, MM_Form_CharsGrid, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MaxM_Commons', @Register);
end.
