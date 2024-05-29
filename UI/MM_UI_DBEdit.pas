unit MM_UI_DBEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, db, DbCtrls;

type

  { TMM_UI_DBEdit }

  TMM_UI_DBEdit = class(TDBEdit)
  protected
    rAutoMaxLength: Boolean;

    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(AValue: string);
    procedure SetDataSource(AValue: TDataSource);
    procedure SetAutoMaxLength(AValue: Boolean);

    procedure Loaded; override;

    procedure UpdateMaxLength; virtual;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AutoMaxLength: Boolean read rAutoMaxLength write SetAutoMaxLength default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBEdit]);
end;

{ TMM_UI_DBEdit }

procedure TMM_UI_DBEdit.SetAutoMaxLength(AValue: Boolean);
begin
     if (rAutoMaxLength <> AValue) then
     begin
          rAutoMaxLength :=AValue;
          if rAutoMaxLength and not(csLoading in ComponentState)
          then UpdateMaxLength;
     end;
end;

procedure TMM_UI_DBEdit.Loaded;
begin
     inherited Loaded;

     UpdateMaxLength;
end;

function TMM_UI_DBEdit.GetDataField: string;
begin
     Result :=inherited DataField;
end;

function TMM_UI_DBEdit.GetDataSource: TDataSource;
begin
     Result :=inherited DataSource;
end;

procedure TMM_UI_DBEdit.SetDataField(AValue: string);
begin
     inherited DataField :=AValue;

     if not(csLoading in ComponentState)
     then UpdateMaxLength;
end;

procedure TMM_UI_DBEdit.SetDataSource(AValue: TDataSource);
begin
     inherited DataSource :=AValue;

     if not(csLoading in ComponentState)
     then UpdateMaxLength;
end;

procedure TMM_UI_DBEdit.UpdateMaxLength;
begin
     if rAutoMaxLength and not(csDesigning in ComponentState) then
     begin
          if (Field <> nil)
          then begin
                    Case Field.DataType of
                    ftString, ftBytes, ftVarBytes : Self.MaxLength :=Field.Size;
                    end;
                end
          else Self.MaxLength :=0;
     end;
end;

constructor TMM_UI_DBEdit.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);

     Self.rAutoMaxLength :=True;
end;

end.
