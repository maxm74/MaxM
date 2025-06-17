unit MM_Form_EditText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls;

type
  { TFormEditText }

  //Set Result to False to Disable Ok Button
  TFormEditText_StatusEvent = function (const AText: String; var AStatusText: String; var AStatusColor: TColor): Boolean;

  TFormEditText = class(TForm)
    btPanel: TButtonPanel;
    lbStatus: TLabel;
    lbEdit: TLabeledEdit;
    procedure lbEditChange(Sender: TObject);
  private
    rTextChange: TFormEditText_StatusEvent;

  public
     class function Execute(const ACaption, ALabel, AStatus: String;
                            var AText: String;
                            ATextChange: TFormEditText_StatusEvent): Boolean;
  end;

var
  FormEditText: TFormEditText = nil;

implementation

{$R *.lfm}

{ TFormEditText }

procedure TFormEditText.lbEditChange(Sender: TObject);
var
  AStatusText: String;
  AStatusColor: TColor;

begin
  if Assigned(rTextChange) then
  begin
    btPanel.OKButton.Enabled:= rTextChange(lbEdit.Text, AStatusText, AStatusColor);
    lbStatus.Caption:= AStatusText;
    lbStatus.Font.Color:= AStatusColor;
  end;
end;

class function TFormEditText.Execute(const ACaption, ALabel, AStatus: String;
                                     var AText: String;
                                     ATextChange: TFormEditText_StatusEvent): Boolean;
begin
  Result:= False;
  try
     if (FormEditText=nil)
     then FormEditText :=TFormEditText.Create(nil);

     if (FormEditText <> nil) then
     with FormEditText do
     begin
       Caption:= ACaption;
       lbEdit.EditLabel.Caption:= ALabel;
       lbEdit.Text:= AText;
       rTextChange:= ATextChange;

       Result:= (ShowModal = mrOk);

       if Result then AText:= lbEdit.Text;
     end;

  finally
    FormEditText.Free; FormEditText:= nil;
  end;
end;

end.

