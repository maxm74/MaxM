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
    lbSelectedNode: TLabel;
    lbSelectedItem: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MM_UI_EnumFilesINMenuItem: TMM_UI_EnumFilesINMenuItem;
    MM_UI_EnumFilesINTreeView: TMM_UI_EnumFilesINTreeView;
    TreeView1: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure MM_UI_EnumFilesINMenuItemItemClick(Sender: TMM_UI_EnumFiles; ComponentNode: TObject; Node: TMM_UI_EnumFilesNode);
    procedure MM_UI_EnumFilesINTreeViewNodeClick(Sender: TMM_UI_EnumFiles;
      ComponentNode: TObject; Node: TMM_UI_EnumFilesNode);
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

procedure TForm1.MM_UI_EnumFilesINMenuItemItemClick(Sender: TMM_UI_EnumFiles; ComponentNode: TObject; Node: TMM_UI_EnumFilesNode);
begin
  if Node.IsDir
  then lbSelectedItem.Caption:= 'dir '+Node.FullPath
  else lbSelectedItem.Caption:= Node.FullPath;
end;

procedure TForm1.MM_UI_EnumFilesINTreeViewNodeClick(Sender: TMM_UI_EnumFiles;
  ComponentNode: TObject; Node: TMM_UI_EnumFilesNode);
begin
  if Node.IsDir
  then lbSelectedNode.Caption:= 'dir '+Node.FullPath
  else lbSelectedNode.Caption:= Node.FullPath;
end;

end.

