unit EnumFiles_Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ComCtrls, MM_UI_EnumFiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    lbSelectedPath: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MM_UI_EnumFilesINMenuItem: TMM_UI_EnumFilesINMenuItem;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure MM_UI_EnumFilesINMenuItemItemClick(Sender: TObject;
      Item: TMenuItem; FileName: String);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  MM_UI_EnumFilesINMenuItem.MenuItem:= MenuItem1;
end;

procedure TForm1.MM_UI_EnumFilesINMenuItemItemClick(Sender: TObject; Item: TMenuItem; FileName: String);
begin
  lbSelectedPath.Caption:= FileName;
end;

end.

