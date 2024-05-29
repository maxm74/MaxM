unit UPlasmaEd_Form;

interface
//{$include Conditionals.inc}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls , Buttons, StdCtrls, UPlasmaRegion, Math, ExtDlgs;

type
   	TMaskForm   = class(TForm)
                   HPanel     : TPanel;
                   VPanel     : TPanel;
                   Image      : TImage;
                   ColorPanel : TPanel;
                   Label1     : TLabel;
                   BtGo       : TSpeedButton;
                   BtOk       : TSpeedButton;
                   BtCancel   : TSpeedButton;
                   BtSave     : TSpeedButton;
                   SaveDialog : TSaveDialog;
    btLoad: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    ColorDialog1: TColorDialog;
    procedure ColorPanelClick(Sender: TObject);
    procedure btLoadClick(Sender: TObject);
                   procedure    ImageMouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
                   procedure    BtOkClick(Sender: TObject);
                   procedure    BtCancelClick(Sender: TObject);
                   procedure    BtGoClick(Sender: TObject);
                   procedure    BtSaveClick(Sender: TObject);
                   procedure    FormCreate(Sender: TObject);
                   procedure    FormDestroy(Sender: TObject);
                 	private
                   FRegion    : TPlasmaRegion;
                   IsModal    : Boolean;
                   procedure    SetBitmap(Value:TBitmap);
                   procedure    SetRegion(Value:HRgn);
                   function     GetRegion:HRgn;
                 	public
                   function Execute :TModalResult;
                   procedure    Empty;
                   property     Region:HRgn read GetRegion write SetRegion;
                   property     Bitmap:TBitmap write SetBitmap;
                 	end;

Var
   MaskForm :TMaskForm;


implementation

{$R *.DFM}
{================================Form==========================================}
procedure TMaskForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
ColorPanel.Color:=Image.Picture.Bitmap.Canvas.Pixels[x,y];
end;

procedure TMaskForm.Empty;
begin
     FRegion.Region:=0;
     if IsModal
     then BtOK.Enabled:=False;
     BtSave.Enabled:=False;
end;

procedure TMaskForm.SetRegion(Value:HRgn);
begin
BtSave.Enabled:=True;
BtOk.Enabled:=True;
FRegion.Region:=Value;
end;

function TMaskForm.GetRegion:HRgn;
begin
Result:=FRegion.Region;
end;

procedure TMaskForm.SetBitmap(Value:TBitmap);
begin
Empty;
Image.Picture.Bitmap.Assign(Value);
If Not Value.Empty then
   begin
   ClientWidth:=Max(VPanel.Width + Value.Width, 250);
   ClientHeight:=Max(HPanel.Height + Value.Height, 220);
   ColorPanel.Color:=Value.Canvas.Pixels[0,0];
   BtGo.Enabled:=True;
   end
  else
   begin
   ClientWidth:=250;
   ClientHeight:=220;
   end;
Application.ProcessMessages;
end;

procedure TMaskForm.BtOkClick(Sender: TObject);
begin
     if IsModal
     then ModalResult:=mrOk
     else Close;
end;

procedure TMaskForm.BtCancelClick(Sender: TObject);
begin
     ModalResult:=mrCancel;
end;

procedure TMaskForm.BtGoClick(Sender: TObject);
var RStart :Integer;
    REnd   :Integer;
    i,j    :Integer;

    procedure PaintRegion(x1,x2,y:Integer;Color1,Color2:TColor);
    var k:Integer;
    begin
    for k:=x1 to x2 do
        if (Odd(k div 7)) xor (Odd(y div 7)) then
           Image.Picture.Bitmap.Canvas.Pixels[k,y]:=Color1
          else
           Image.Picture.Bitmap.Canvas.Pixels[k,y]:=Color2;
    Application.ProcessMessages;
    end;

    procedure AddRegion(x1,x2,y:Integer);
    var Aux:HRgn;
    begin
    If FRegion.Region=0  then
       FRegion.Region:=CreateRectRgn(x1,y,x2+1,y+1)
      else
       begin
       Aux:=CreateRectRgn(x1,y,x2+1,y+1);
       CombineRgn(FRegion.Region,FRegion.Region,Aux,RGN_OR);
       DeleteObject(Aux);
       end;
   end;

begin
BtGo.Enabled:=False;
FRegion.Region:=0;
Application.ProcessMessages;
for j:=0 to Image.Picture.Height-1 do
    begin
    RStart:=-1;
    REnd:=-1;
    for i:=0 to Image.Picture.Width-1 do
        begin
        If ((Image.Picture.Bitmap.Canvas.Pixels[i,j]<>ColorPanel.Color) or
            (i=Image.Picture.Width-1)) and
           (RStart=-1) then
           begin
           RStart:=i;
           PaintRegion(REnd+1,RStart-1,j,clSilver,clWhite);
           end;
        If ((Image.Picture.Bitmap.Canvas.Pixels[i,j]=ColorPanel.Color) or
            (i=Image.Picture.Width-1)) and
           (RStart<>-1) then
           begin
           REnd:=i-1;
           PaintRegion(RStart,REnd,j,clRed,clYellow);
           AddRegion(RStart,REnd,j);
           RStart:=-1;
           end;
        end;
    end;
BtSave.Enabled:=True;
BtOk.Enabled:=True;
ShowMessage('Ok, is Finished');
end;

procedure TMaskForm.BtSaveClick(Sender: TObject);
var F:TFileStream;
    p:PRgnData;
    s:Integer;
begin
If FRegion.Region<>0 then
   If SaveDialog.Execute then
      begin
      F:=TFileStream.Create(SaveDialog.FileName,fmCreate);
      s:=GetRegionData(FRegion.Region,0,nil);
      GetMem(p,s);
      GetRegionData(FRegion.Region,s,p);
      F.Write(p^,s);
      FreeMem(p,s);
      F.Free;
      BtSave.Enabled:=False;
      end;
end;

procedure TMaskForm.FormCreate(Sender: TObject);
begin
     FRegion:=TPlasmaRegion.Create(Image);
     IsModal :=False;
     BtOk.Enabled :=True;
end;

procedure TMaskForm.FormDestroy(Sender: TObject);
begin
     FRegion.Free;
end;

function TMaskForm.Execute :TModalResult;
begin
     IsModal :=True;
     BtCancel.Visible :=True;
     BtOk.Enabled :=False;
     Result :=ShowModal;
end;

{==============================================================================}

procedure LoadAsBitmap(NCFileName :String; Dest :TBitmap);
var
   tempPicture :TPicture;

begin
     if FileExists(NCFileName) then
     begin
          tempPicture :=TPicture.Create;
          try
             tempPicture.LoadFromFile(NCFileName);
             Dest.Assign(tempPicture.Graphic);
          except
            On E:Exception do begin end;
          end;
          tempPicture.Free;
      end;
end;

procedure TMaskForm.btLoadClick(Sender: TObject);
Var
   tmpBitmap :TBitmap;

begin
     if OpenPictureDialog1.Execute
     then begin
               tmpBitmap :=TBitmap.Create;
               LoadAsBitmap(OpenPictureDialog1.FileName, tmpBitmap);
               SetBitmap(tmpBitmap);
               tmpBitmap.Free;
          end;
end;

procedure TMaskForm.ColorPanelClick(Sender: TObject);
begin
     if Self.ColorDialog1.Execute
     then ColorPanel.Color :=Self.ColorDialog1.Color;
end;

end.
