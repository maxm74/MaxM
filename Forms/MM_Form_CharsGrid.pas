unit MM_Form_CharsGrid;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids;

type

  { TFormCharsGrid }

  TFormCharsGrid = class(TForm)
    sgCharsGrid: TStringGrid;
    Panel1: TPanel;
    btOk: TButton;
    btCancel: TButton;
    lbResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sgCharsGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    rSelected: Char;

    function GetSelected : Char;
    procedure SetSelected(Value :Char);

  public
    { Public declarations }
    property Selected :Char read GetSelected write SetSelected;
  end;

var
  FormCharsGrid: TFormCharsGrid = nil;


function FormCharsGridShow(var SelectedChar: Char): Boolean;

implementation

function FormCharsGridShow(var SelectedChar: Char): Boolean;
begin
  if (FormCharsGrid = nil)
  then FormCharsGrid :=TFormCharsGrid.Create(Application);

  FormCharsGrid.Selected :=SelectedChar;
  Result := (FormCharsGrid.ShowModal=mrOk);

  if Result
  then SelectedChar :=FormCharsGrid.Selected;

  FreeAndNil(FormCharsGrid);
end;

{$R *.lfm}

procedure TFormCharsGrid.FormCreate(Sender: TObject);
Var
   i, k :Integer;

begin
     for i :=1 to 16 do
     begin
           sgCharsGrid.Cells[i, 0] :=IntToHex(i-1, 1);
           sgCharsGrid.Cells[0, i] :=IntToHex(i-1, 1);
      end;

     for i :=0 to 15 do
       for k :=0 to 15 do
       begin
            sgCharsGrid.Cells[k+1, i+1] :=Char( (i shl 4) + k );
       end;
end;

procedure TFormCharsGrid.FormShow(Sender: TObject);
begin
  SetSelected(rSelected);
end;

function TFormCharsGrid.GetSelected : Char;
var
   t:TGridRect;

begin
     t:=sgCharsGrid.Selection;
     rSelected :=Char((Byte(sgCharsGrid.Selection.Top-1) shl 4) + Byte(sgCharsGrid.Selection.Left-1));
     Result :=rSelected;
end;

procedure TFormCharsGrid.SetSelected(Value :Char);
Var
   NewSel :TGridRect;

begin
     rSelected :=Value;

     NewSel.Top :=(Byte(Value) shr 4) +1;
     NewSel.Left :=(Byte(Value) and $0F) +1;
     NewSel.Right :=NewSel.Left;
     NewSel.Bottom :=NewSel.Top;
     sgCharsGrid.Selection :=NewSel;

     lbResult.Caption :=':  '+IntToHex(Byte(rSelected), 2);
end;

procedure TFormCharsGrid.sgCharsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
     lbResult.Caption :=':  '+IntToHex( (Byte(ARow-1) shl 4) + Byte(ACol-1), 2);
     CanSelect :=True;
end;

end.
