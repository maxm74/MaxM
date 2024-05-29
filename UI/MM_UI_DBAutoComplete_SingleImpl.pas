unit MM_UI_DBAutoComplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, ExtCtrls, DBGrids, db, sqldb, LCLType, Buttons;

type
    TMM_UI_AutoCompleteImpl = class;

    TMM_UI_AutoCompleteGetText = function :String of Object;
    TMM_UI_AutoCompleteSetText = procedure (ANewText :String) of Object;

    { TDBGridPopup }
    //TReturnSelectedEvent = procedure (Sender: TObject; AItemIndex :Integer) of object;

    TDBGridPopup = class(TForm)
    private
      function getItemIndex: Integer;
    protected
      Closed: boolean;
      Editor: TMM_UI_AutoCompleteImpl;
      SearchGrid: TDBGrid;
      oldText: String;
      btnPanel: TPanel;
      btnInsert: TSpeedButton;  {icon : dbnavinsert}
      btnEdit: TSpeedButton;  {icon : dbnavedit}
      btnDelete: TSpeedButton;  {icon : dbnavdelete}

      procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
      procedure FormDeactivate(Sender: TObject);
      procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure GridKeyPress(Sender: TObject; var Key: Char);
      procedure GridCellClick(Column: TColumn);
      procedure SearchButtonsClick(Sender: TObject);
      procedure InitializeOrigin (const PopupOrigin: TPoint);
      procedure Initialize(AEditor: TMM_UI_AutoCompleteImpl; const PopupOrigin: TRect);
      procedure ReturnSelected;

      procedure Paint; override;

    public
      //OnReturnSelected: TReturnSelectedEvent;
      //OnSearchButtons : TReturnSelectedEvent;

      constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;

      property ItemIndex: Integer read getItemIndex;
    end;


    { TMM_UI_AutoComplete }
    TMM_UI_AutoCompleteSelectEvent = procedure (Sender: TMM_UI_AutoCompleteImpl;
                                                  AItemIndex: Integer;
                                                  var newText: String) of object;
    TMM_UI_AutoCompleteButtonTypes = (acbInsert, acbEdit, acbDelete);
    TMM_UI_AutoCompleteButtons = set of TMM_UI_AutoCompleteButtonTypes;
    TMM_UI_AutoCompleteButtonsAlign = (acbTopLeft, acbTopRight, acbBottomLeft, acbBottomRight);
    TMM_UI_AutoCompleteButtonClick = function (Sender: TMM_UI_AutoCompleteImpl;
                                                 Button: TMM_UI_AutoCompleteButtonTypes;
                                                 AItemIndex: Integer;
                                                 var newText: String): TModalResult of object;

    { TMM_UI_AutoCompleteImpl }

    TMM_UI_AutoCompleteImpl = class
    public
      Owner: TWinControl;
      GetTextFunc: TMM_UI_AutoCompleteGetText;
      SetTextFunc: TMM_UI_AutoCompleteSetText;
      edTimer: TTimer;
      editing: Boolean;
      rSearchMinChars: Cardinal;
      rSearchDataSource: TDataSource;
      rSearchDataSet: TSQLQuery;
      rSearchFields: TStringList;
      rSearchFieldsAND: Boolean;
      rSearchFieldOperator: String;
      rSearchGrid: TDBGrid;
      rSearchGridHeight: Integer;
      rSearchButtons: TMM_UI_AutoCompleteButtons;
      rSearchButtonsHints: TStringList;
      rSearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign;
      rSelectTextValue: String;
      rSearchQuery: String;
      PopupForm: TDBGridPopup;

      rOnSelect: TMM_UI_AutoCompleteSelectEvent;
      rOnPopupHide: TNotifyEvent;
      rOnPopupShow: TNotifyEvent;
      rOnSearchButtonClick: TMM_UI_AutoCompleteButtonClick;

      function GetSearchColumns: TDBGridColumns;
      procedure SetSearchColumns(AValue: TDBGridColumns);
      procedure setLeft(AValue: Integer);
      procedure setTop(AValue: Integer);
      procedure setWidth(AValue: Integer);
      function getSearchPopupInterval: Cardinal;
      procedure setSearchPopupInterval(AValue: Cardinal);
      procedure setSearchFields(AValue: TStringList);
      procedure setSearchDataSet(AValue: TSQLQuery);
      procedure setSearchButtonsHints(AValue: TStringList);
      function getItemIndex: Integer;

      procedure KeyDown(var Key: Word; Shift: TShiftState);
      procedure KeyUp(var Key: Word; Shift: TShiftState);
      procedure KeyPress(var Key: char);
      procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure GridKeyPress(Sender: TObject; var Key: Char);

      procedure edTimerTimer(Sender: TObject);
      procedure edTimerStartStop(Sender: TObject);
      procedure Search;
      procedure ShowGrid;
      function CreateGrid: TDBGrid;

      function getSelectedText: String;
      procedure SearchGrid_OnSelect(Sender: TObject; AItemIndex: Integer);
      procedure SearchGrid_OnShow(Sender: TObject);
      procedure SearchGrid_OnHide(Sender: TObject);
      procedure SearchButtons_Click(Sender: TObject; Button: TMM_UI_AutoCompleteButtonTypes; AItemIndex: Integer);

      constructor Create(AOwner: TWinControl; AGetTextFunc: TMM_UI_AutoCompleteGetText; ASetTextFunc: TMM_UI_AutoCompleteSetText);
      destructor Destroy;

      property ItemIndex: Integer read getItemIndex;
      property SearchPopupInterval: Cardinal read getSearchPopupInterval write setSearchPopupInterval default 2000;
      property SearchMinChars: Cardinal read rSearchMinChars write rSearchMinChars default 2;
      property SearchDataSet: TSQLQuery read rSearchDataSet write setSearchDataSet;
      property SearchFields: TStringList read rSearchFields write setSearchFields;
      property SearchFieldsAND: Boolean read rSearchFieldsAND write rSearchFieldsAND default False;
      property SearchFieldOperator: String read rSearchFieldOperator write rSearchFieldOperator;
      property SearchColumns: TDBGridColumns read GetSearchColumns write SetSearchColumns;
      property SearchGridHeight: Integer read rSearchGridHeight write rSearchGridHeight default 150;
      property SearchButtons: TMM_UI_AutoCompleteButtons read rSearchButtons write rSearchButtons default [];
      property SearchButtonsHints: TStringList read rSearchButtonsHints write setSearchButtonsHints;
      property SearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign read rSearchButtonsAlign write rSearchButtonsAlign default acbTopRight;

      property SelectTextValue: String read rSelectTextValue write rSelectTextValue;

      property OnSelect: TMM_UI_AutoCompleteSelectEvent read rOnSelect write rOnSelect;
      property OnPopupShow: TNotifyEvent read rOnPopupShow write rOnPopupShow;
      property OnPopupHide: TNotifyEvent read rOnPopupHide write rOnPopupHide;
      property OnSearchButtonClick: TMM_UI_AutoCompleteButtonClick read rOnSearchButtonClick write rOnSearchButtonClick;
   end;

    { TMM_UI_DBAutoComplete }

    TMM_UI_DBAutoComplete = class(TDBEdit)
    private
      rSearchMinChars: Cardinal;
      function getOnPopupHide: TNotifyEvent;
      function getOnPopupShow: TNotifyEvent;
      function getOnSearchButtonClick: TMM_UI_AutoCompleteButtonClick;
      function getOnSelect: TMM_UI_AutoCompleteSelectEvent;
      function getSearchButtons: TMM_UI_AutoCompleteButtons;
      function getSearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign;
      function getSearchButtonsHints: TStringList;
      function getSearchDataSet: TSQLQuery;
      function getSearchFieldOperator: String;
      function getSearchFields: TStringList;
      function getSearchFieldsAND: Boolean;
      function getSearchGridHeight: Integer;
      function getSearchMinChars: Cardinal;
      function getSelectTextValue: String;
      procedure setOnPopupHide(AValue: TNotifyEvent);
      procedure setOnPopupShow(AValue: TNotifyEvent);
      procedure setOnSearchButtonClick(AValue: TMM_UI_AutoCompleteButtonClick
        );
      procedure setOnSelect(AValue: TMM_UI_AutoCompleteSelectEvent);
      procedure setSearchButtons(AValue: TMM_UI_AutoCompleteButtons);
      procedure setSearchButtonsAlign(AValue: TMM_UI_AutoCompleteButtonsAlign
        );
      procedure setSearchFieldOperator(AValue: String);
      procedure setSearchFieldsAND(AValue: Boolean);
      procedure setSearchGridHeight(AValue: Integer);
      procedure setSelectTextValue(AValue: String);
    protected
      impl: TMM_UI_AutoCompleteImpl;

      function getLeft: Integer;
      function getTop: Integer;
      function getWidth: Integer;
      function GetSearchColumns: TDBGridColumns;
      procedure SetSearchColumns(AValue: TDBGridColumns);
      procedure setLeft(AValue: Integer);
      procedure setTop(AValue: Integer);
      procedure setWidth(AValue: Integer);
      function getSearchPopupInterval: Cardinal;
      procedure setSearchPopupInterval(AValue: Cardinal);
      procedure setSearchFields(AValue: TStringList);
      procedure setSearchDataSet(AValue: TSQLQuery);
      procedure setSearchButtonsHints(AValue: TStringList);
      function getItemIndex: Integer;

      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      procedure KeyUp(var Key: Word; Shift: TShiftState); override;
      procedure KeyPress(var Key: char); override;
      procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
      procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
      procedure GridKeyPress(Sender: TObject; var Key: Char); virtual;

      procedure edTimerTimer(Sender: TObject); virtual;
      procedure edTimerStartStop(Sender: TObject); virtual;
      procedure Search; virtual;
      procedure ShowGrid; virtual;
      function CreateGrid: TDBGrid; virtual;

      function GetText:String;
      procedure SetText(ANewText :String);

      function getSelectedText: String; virtual;
      procedure SearchGrid_OnSelect(Sender: TObject; AItemIndex: Integer); virtual;
      procedure SearchGrid_OnShow(Sender: TObject); virtual;
      procedure SearchGrid_OnHide(Sender: TObject); virtual;
      procedure SearchButtons_Click(Sender: TObject; Button: TMM_UI_AutoCompleteButtonTypes; AItemIndex: Integer); virtual;
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;

      property ItemIndex: Integer read getItemIndex;
    published
      property SearchPopupInterval: Cardinal read getSearchPopupInterval write setSearchPopupInterval default 2000;
      property SearchMinChars: Cardinal read getSearchMinChars write rSearchMinChars default 2;
      property SearchDataSet: TSQLQuery read getSearchDataSet write setSearchDataSet;
      property SearchFields: TStringList read getSearchFields write setSearchFields;
      property SearchFieldsAND: Boolean read getSearchFieldsAND write setSearchFieldsAND default False;
      property SearchFieldOperator: String read getSearchFieldOperator write setSearchFieldOperator;
      property SearchColumns: TDBGridColumns read GetSearchColumns write SetSearchColumns;
      property SearchGridHeight: Integer read getSearchGridHeight write setSearchGridHeight default 150;
      property SearchButtons: TMM_UI_AutoCompleteButtons read getSearchButtons write setSearchButtons default [];
      property SearchButtonsHints: TStringList read getSearchButtonsHints write setSearchButtonsHints;
      property SearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign read getSearchButtonsAlign write setSearchButtonsAlign default acbTopRight;

      property SelectTextValue: String read getSelectTextValue write setSelectTextValue;
      property Width: Integer read getWidth write setWidth;
      property Left: Integer read getLeft write setLeft;
      property Top: Integer read getTop write setTop;

      property OnSelect: TMM_UI_AutoCompleteSelectEvent read getOnSelect write setOnSelect;
      property OnPopupShow: TNotifyEvent read getOnPopupShow write setOnPopupShow;
      property OnPopupHide: TNotifyEvent read getOnPopupHide write setOnPopupHide;
      property OnSearchButtonClick: TMM_UI_AutoCompleteButtonClick read getOnSearchButtonClick write setOnSearchButtonClick;
    end;

    TMM_UI_AutoCompleteEdit = class(TEdit)

    end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBAutoComplete]);
end;

{ TDBGridPopup }

function TDBGridPopup.getItemIndex: Integer;
begin
     try
        Result :=Editor.SearchDataSet.RecNo;
     except
        Result :=0;
     end;
end;

procedure TDBGridPopup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Closed := True;
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  CloseAction := caFree;
  Editor.PopupForm :=Nil;
end;

procedure TDBGridPopup.FormDeactivate(Sender: TObject);
begin
     if (Self.ItemIndex = 0)
     then Editor.SetTextFunc(Editor.GetTextFunc()); //Editor.Field.AsString :=Editor.Text;

     if not(Closed)
     then Close;
end;

procedure TDBGridPopup.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    case Key of
    VK_ESCAPE : begin
                     Editor.SetTextFunc(oldText);
                     Close;
                     Key :=0;
                end;
    VK_RETURN : begin
                     ReturnSelected;
                     Key :=0;
                end;
    VK_TAB    : begin
                     if (Self.ItemIndex > 0)
                     then ReturnSelected
                     else Editor.SetTextFunc(Editor.GetTextFunc());

                     if (ssShift in Shift)
                     then Editor.Owner.Parent.SelectNext(Editor.Owner, False, True)
                     else Editor.Owner.Parent.SelectNext(Editor.Owner, True, True);
                     Key :=0;
                end;
    VK_PRIOR..VK_DOWN : begin end;
    else Editor.GridKeyDown(Self, Key, Shift);
    end;
end;

procedure TDBGridPopup.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     Case Key of
     VK_ESCAPE, VK_RETURN : Key :=0;
     VK_PRIOR..VK_DOWN : begin end;
     else Editor.GridKeyUp(Self, Key, Shift);
     end;
end;

procedure TDBGridPopup.GridKeyPress(Sender: TObject; var Key: Char);
begin
     Editor.GridKeyPress(Self, Key);
end;

procedure TDBGridPopup.GridCellClick(Column: TColumn);
begin
     ReturnSelected;
end;

procedure TDBGridPopup.SearchButtonsClick(Sender: TObject);
begin
     Application.RemoveOnDeactivateHandler(@FormDeactivate);
     Self.OnDeactivate :=nil;

     if (Sender = btnInsert)
     then Editor.SearchButtons_Click(Self, acbInsert, Self.ItemIndex)
     else if (Sender = btnEdit)
     then Editor.SearchButtons_Click(Self, acbEdit, Self.ItemIndex)
     else if (Sender = btnDelete)
     then Editor.SearchButtons_Click(Self, acbDelete, Self.ItemIndex);

     Application.AddOnDeactivateHandler(@FormDeactivate);
     Self.OnDeactivate :=@FormDeactivate;
end;

procedure TDBGridPopup.InitializeOrigin(const PopupOrigin: TPoint);
var
  ABounds: TRect;

begin
     ABounds :=Screen.MonitorFromPoint(PopupOrigin).BoundsRect;

     if (PopupOrigin.X + Width > ABounds.Right)
     then Left :=ABounds.Right - Width
     else Left :=PopupOrigin.X;

     if (PopupOrigin.Y + Height > ABounds.Bottom)
     then Top :=ABounds.Bottom - Height
     else Top :=PopupOrigin.Y;
end;

procedure TDBGridPopup.Initialize(AEditor: TMM_UI_AutoCompleteImpl; const PopupOrigin: TRect);
Var
   x :Cardinal;

begin
     Editor :=AEditor;
     oldText :=Editor.GetTextFunc();

     Width :=PopupOrigin.Right;
     if (Editor.SearchButtons <> [])
     then Height :=PopupOrigin.Bottom
     else Height :=PopupOrigin.Bottom+16;

     InitializeOrigin(PopupOrigin.TopLeft);

     SearchGrid :=Editor.rSearchGrid;

     if (Editor.SearchButtons <> []) then
     begin
          btnPanel :=TPanel.Create(Self);
          btnPanel.Parent :=Self;
          btnPanel.BorderStyle :=bsSingle;
          btnPanel.BorderWidth :=1;
          btnPanel.BevelOuter :=bvRaised;
          btnPanel.BevelInner :=bvNone;
          btnPanel.Color :=SearchGrid.Color;
          btnPanel.Height :=19;
          btnPanel.Width :=PopupOrigin.Right;

          if (acbInsert in Editor.SearchButtons)
          then begin
                    btnInsert :=TSpeedButton.Create(Self);
                    btnInsert.Parent :=btnPanel;
                    btnInsert.Top :=0;
                    btnInsert.Width :=16;
                    btnInsert.Height :=16;
                    btnInsert.LoadGlyphFromResourceName(hInstance, DBNavButtonResourceName[nbInsert]); //'dbnavinsert'
                    try
                       btnInsert.Hint :=Editor.rSearchButtonsHints.Strings[0];
                       btnInsert.ShowHint :=True;
                    except
                       btnInsert.Hint :='';
                    end;
                    btnInsert.OnClick :=@SearchButtonsClick;
               end
          else btnInsert :=nil;

          if (acbEdit in Editor.SearchButtons)
          then begin
                    btnEdit :=TSpeedButton.Create(Self);
                    btnEdit.Parent :=btnPanel;
                    btnEdit.Top :=0;
                    btnEdit.Width :=16;
                    btnEdit.Height :=16;
                    btnEdit.LoadGlyphFromResourceName(hInstance, DBNavButtonResourceName[nbEdit]);  //'dbnavedit'
                    try
                       btnEdit.Hint :=Editor.rSearchButtonsHints.Strings[1];
                       btnEdit.ShowHint :=True;
                    except
                       btnEdit.Hint :='';
                    end;
                    btnEdit.OnClick :=@SearchButtonsClick;
               end
          else btnEdit :=nil;

          if (acbDelete in Editor.SearchButtons)
          then begin
                    btnDelete :=TSpeedButton.Create(Self);
                    btnDelete.Parent :=btnPanel;
                    btnDelete.Top :=0;
                    btnDelete.Width :=16;
                    btnDelete.Height :=16;
                    btnDelete.LoadGlyphFromResourceName(hInstance, DBNavButtonResourceName[nbDelete]); //'dbnavdelete';
                    try
                       btnDelete.Hint :=Editor.rSearchButtonsHints.Strings[2];
                       btnDelete.ShowHint :=True;
                    except
                       btnDelete.Hint :='';
                    end;
                    btnDelete.OnClick :=@SearchButtonsClick;
               end
          else btnDelete :=nil;

          Case Editor.SearchButtonsAlign of
          acbTopLeft, acbTopRight: btnPanel.Align :=alTop;
          acbBottomLeft, acbBottomRight: btnPanel.Align :=alBottom;
          end;

          Case Editor.SearchButtonsAlign of
          acbTopLeft,
          acbBottomLeft: begin
                              x :=btnPanel.BevelWidth+1;

                              if (btnInsert <> nil) then
                              begin
                                   btnInsert.Left :=x;
                                   inc(x, 16);
                              end;
                              if (btnEdit <> nil) then
                              begin
                                   btnEdit.Left :=x;
                                   inc(x, 16);
                              end;
                              if (btnDelete <> nil) then
                              begin
                                   btnDelete.Left :=x;
                                   inc(x, 16);
                              end;
                         end;
          acbTopRight,
          acbBottomRight: begin
                              x :=btnPanel.Width-16-(btnPanel.BevelWidth+2);

                              if (btnDelete <> nil) then
                              begin
                                   btnDelete.Left :=x;
                                   dec(x, 16);
                              end;
                              if (btnEdit <> nil) then
                              begin
                                   btnEdit.Left :=x;
                                   dec(x, 16);
                              end;
                              if (btnInsert <> nil) then
                              begin
                                    btnInsert.Left :=x;
                                    dec(x, 16);
                              end;
                          end;
          end;
     end;
     SearchGrid.Parent :=Self;
     SearchGrid.Align :=alClient;
     SearchGrid.OnKeyDown:=@GridKeyDown;
     SearchGrid.OnKeyUp :=@GridKeyUp;
     SearchGrid.OnKeyPress :=@GridKeyPress;
     SearchGrid.OnCellClick:=@GridCellClick;
end;


procedure TDBGridPopup.ReturnSelected;
begin
  if (Self.ItemIndex > 0)
  then Editor.SearchGrid_OnSelect(Self, Self.ItemIndex);

  if not(Closed)
  then Close;
end;

procedure TDBGridPopup.Paint;
begin
  inherited Paint;

  Canvas.Pen.Color :=clWindowText;
  Canvas.Pen.Style :=psSolid;
  Canvas.Rectangle(0, 0, Width-1, Height-1);
end;

constructor TDBGridPopup.CreateNew(AOwner: TComponent; Num: Integer = 0);
begin
  inherited CreateNew(AOwner);

  BorderIcons :=[];
  BorderStyle :=bsNone;
  PopupMode :=pmAuto;
  Closed :=False;
  Application.AddOnDeactivateHandler(@FormDeactivate);
  Self.OnClose :=@FormClose;
  Self.OnDeactivate :=@FormDeactivate;
end;

{ TMM_UI_DBAutoComplete }

procedure TMM_UI_DBAutoComplete.edTimerTimer(Sender :TObject);
begin
  impl.edTimerTimer(Sender);
end;

procedure TMM_UI_DBAutoComplete.edTimerStartStop(Sender: TObject);
begin

end;

procedure TMM_UI_DBAutoComplete.Search;
begin
  impl.Search;
end;

procedure TMM_UI_DBAutoComplete.ShowGrid;
begin
  impl.ShowGrid;
end;

function TMM_UI_DBAutoComplete.CreateGrid: TDBGrid;
begin
  Result :=impl.CreateGrid;
end;

function TMM_UI_DBAutoComplete.GetText: String;
begin
     Result :=Self.Text;
end;

procedure TMM_UI_DBAutoComplete.SetText(ANewText: String);
begin
     Self.Field.AsString :=ANewText;
end;

function TMM_UI_DBAutoComplete.getSelectedText: String;
begin
  Result :=impl.getSelectedText;
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnSelect(Sender: TObject; AItemIndex :Integer);
begin
  impl.SearchGrid_OnSelect(Sender, AItemIndex);
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnShow(Sender: TObject);
begin
  impl.SearchGrid_OnShow(Sender);
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnHide(Sender: TObject);
begin
  impl.SearchGrid_OnHide(Sender);
end;

procedure TMM_UI_DBAutoComplete.SearchButtons_Click(Sender: TObject;
  Button: TMM_UI_AutoCompleteButtonTypes; AItemIndex: Integer);
begin
  impl.SearchButtons_Click(Sender, Button, AItemIndex);
end;

constructor TMM_UI_DBAutoComplete.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  impl :=TMM_UI_AutoCompleteImpl.Create(Self, @GetText, @SetText);
  //impl.SearchDataSource :=TDataSource.Create(Self);
end;

destructor TMM_UI_DBAutoComplete.Destroy;
begin
  impl.Free;

  inherited Destroy;
end;

procedure TMM_UI_DBAutoComplete.setSearchDataSet(AValue :TSQLQuery);
begin
  impl.setSearchDataSet(AValue);
end;

function TMM_UI_DBAutoComplete.getItemIndex: Integer;
begin
  Result :=impl.getItemIndex;
end;

function TMM_UI_DBAutoComplete.GetSearchColumns: TDBGridColumns;
begin
  Result :=impl.GetSearchColumns;
end;

procedure TMM_UI_DBAutoComplete.setSearchButtonsHints(AValue: TStringList);
begin
  impl.setSearchButtonsHints(AValue);
end;

function TMM_UI_DBAutoComplete.getOnPopupHide: TNotifyEvent;
begin
  Result :=impl.OnPopupHide;
end;

function TMM_UI_DBAutoComplete.getOnPopupShow: TNotifyEvent;
begin
  Result :=impl.OnPopupShow;
end;

function TMM_UI_DBAutoComplete.getOnSearchButtonClick: TMM_UI_AutoCompleteButtonClick;
begin
  Result :=impl.OnSearchButtonClick;
end;

function TMM_UI_DBAutoComplete.getOnSelect: TMM_UI_AutoCompleteSelectEvent;
begin
  Result :=impl.OnSelect;
end;

function TMM_UI_DBAutoComplete.getSearchButtons: TMM_UI_AutoCompleteButtons;
begin
  Result :=impl.SearchButtons;
end;

function TMM_UI_DBAutoComplete.getSearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign;
begin
  Result :=impl.SearchButtonsAlign;
end;

function TMM_UI_DBAutoComplete.getSearchButtonsHints: TStringList;
begin
  Result :=impl.SearchButtonsHints;
end;

function TMM_UI_DBAutoComplete.getSearchDataSet: TSQLQuery;
begin
  Result :=impl.SearchDataSet;
end;

function TMM_UI_DBAutoComplete.getSearchFieldOperator: String;
begin
  Result :=impl.SearchFieldOperator;
end;

function TMM_UI_DBAutoComplete.getSearchFields: TStringList;
begin
  Result :=impl.SearchFields;
end;

function TMM_UI_DBAutoComplete.getSearchFieldsAND: Boolean;
begin
  Result :=impl.SearchFieldsAND;
end;

function TMM_UI_DBAutoComplete.getSearchGridHeight: Integer;
begin
  Result :=impl.SearchGridHeight;
end;

function TMM_UI_DBAutoComplete.getSearchMinChars: Cardinal;
begin
  Result :=impl.SearchMinChars;
end;

function TMM_UI_DBAutoComplete.getSelectTextValue: String;
begin
  Result :=impl.SelectTextValue;
end;

procedure TMM_UI_DBAutoComplete.setOnPopupHide(AValue: TNotifyEvent);
begin
  impl.OnPopupHide :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setOnPopupShow(AValue: TNotifyEvent);
begin
  impl.OnPopupShow :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setOnSearchButtonClick(
  AValue: TMM_UI_AutoCompleteButtonClick);
begin
  impl.OnSearchButtonClick :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setOnSelect(
  AValue: TMM_UI_AutoCompleteSelectEvent);
begin
  impl.OnSelect :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSearchButtons(
  AValue: TMM_UI_AutoCompleteButtons);
begin
  impl.SearchButtons :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSearchButtonsAlign(
  AValue: TMM_UI_AutoCompleteButtonsAlign);
begin
  impl.SearchButtonsAlign :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSearchFieldOperator(AValue: String);
begin
  impl.SearchFieldOperator :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSearchFieldsAND(AValue: Boolean);
begin
  impl.SearchFieldsAND :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSearchGridHeight(AValue: Integer);
begin
  impl.SearchGridHeight :=AValue;
end;

procedure TMM_UI_DBAutoComplete.setSelectTextValue(AValue: String);
begin
  impl.SelectTextValue :=AValue;
end;

function TMM_UI_DBAutoComplete.getLeft: Integer;
begin
     Result :=inherited Left;
end;

function TMM_UI_DBAutoComplete.getTop: Integer;
begin
     Result :=inherited Top;
end;

function TMM_UI_DBAutoComplete.getWidth: Integer;
begin
     Result :=inherited Width;
end;

procedure TMM_UI_DBAutoComplete.SetSearchColumns(AValue: TDBGridColumns);
begin
     impl.SetSearchColumns(AValue);
end;

procedure TMM_UI_DBAutoComplete.setLeft(AValue: Integer);
begin
     inherited Left :=AValue;

     impl.setLeft(AValue);
end;

procedure TMM_UI_DBAutoComplete.setTop(AValue: Integer);
begin
     inherited Top :=AValue;

     impl.setTop(AValue);
end;

procedure TMM_UI_DBAutoComplete.setWidth(AValue: Integer);
begin
     inherited Width :=AValue;

     impl.setWidth(AValue);
end;

function TMM_UI_DBAutoComplete.getSearchPopupInterval: Cardinal;
begin
     Result :=impl.getSearchPopupInterval;
end;

procedure TMM_UI_DBAutoComplete.setSearchPopupInterval(AValue: Cardinal);
begin
     impl.setSearchPopupInterval(AValue);
end;

procedure TMM_UI_DBAutoComplete.setSearchFields(AValue :TStringList);
begin
     impl.setSearchFields(AValue);
end;

procedure TMM_UI_DBAutoComplete.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  impl.KeyDown(Key, Shift);
end;

procedure TMM_UI_DBAutoComplete.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  impl.KeyUp(Key, Shift);
end;

procedure TMM_UI_DBAutoComplete.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;

procedure TMM_UI_DBAutoComplete.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  impl.GridKeyDown(Sender, Key, Shift);
end;

procedure TMM_UI_DBAutoComplete.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     impl.GridKeyUp(Sender, Key, Shift);
end;

procedure TMM_UI_DBAutoComplete.GridKeyPress(Sender: TObject; var Key: Char);
begin
  impl.GridKeyPress(Sender, Key);
end;

{ TMM_UI_AutoCompleteImpl }

procedure TMM_UI_AutoCompleteImpl.edTimerTimer(Sender :TObject);
begin
     edTimer.Enabled :=False;

     if Length(GetTextFunc())>2
     then Search;
end;

procedure TMM_UI_AutoCompleteImpl.edTimerStartStop(Sender: TObject);
begin

end;

procedure TMM_UI_AutoCompleteImpl.Search;
Var
   i           :Integer;
   curText     :String;
   FieldNamesQuoteChars : TQuoteChars;

begin
     if not(csDesigning in Owner.ComponentState) and  //Paranoic Think
        (rSearchDataSet <> Nil) and (rSearchFields.Count > 0) then
     begin
          FieldNamesQuoteChars := TSQLConnection(rSearchDataSet.DataBase).FieldNameQuoteChars;

          rSearchDataSet.Close;

          rSearchQuery :='';
          curText :=GetTextFunc();

          for i :=0 to rSearchFields.Count-1 do
          begin
               if (i > 0)
               then if rSearchFieldsAND
                     then rSearchQuery :=rSearchQuery+' AND '
                     else rSearchQuery :=rSearchQuery+' OR ';

               rSearchQuery :=rSearchQuery+'('+FieldNamesQuoteChars[0]+rSearchFields[i]+FieldNamesQuoteChars[1]+' '+rSearchFieldOperator+' '+QuotedStr(curText)+')';
          end;
          rSearchDataSet.ServerFilter :=rSearchQuery;
          rSearchDataSet.ServerFiltered :=True;

          rSearchDataSet.Open;

          if (PopupForm = Nil)
          then ShowGrid;
     end;
end;

procedure TMM_UI_AutoCompleteImpl.ShowGrid;
var
  PopupOrigin :TRect;

begin
     PopupOrigin.TopLeft :=Owner.ControlToScreen(Point(0, Owner.Height));
     PopupOrigin.Right :=Owner.Width;
     PopupOrigin.Bottom :=Self.rSearchGridHeight;

     PopupForm :=TDBGridPopup.CreateNew(nil);

     PopupForm.Initialize(Self, PopupOrigin);
     //PopupForm.OnReturnSelected :=@SearchGrid_OnSelect;
     PopupForm.OnShow :=@SearchGrid_OnShow;
     PopupForm.OnHide :=@SearchGrid_OnHide;
     PopupForm.Show;
end;

function TMM_UI_AutoCompleteImpl.CreateGrid: TDBGrid;
begin
     Result :=TDBGrid.Create(Self.Owner);
     Result.DataSource :=rSearchDataSource;
     Result.Flat :=True;
     Result.ReadOnly :=True;
     Result.ScrollBars :=ssAutoBoth;
     Result.Options :=[dgColLines, dgCellHints, dgRowSelect, dgDisableDelete, dgDisableInsert];
end;

function TMM_UI_AutoCompleteImpl.getSelectedText: String;
var
   i :Integer;

begin
   if (rSearchDataSet.RecNo >=0) then
   begin
     Result :=rSelectTextValue;
     if (rSelectTextValue = '')
     then for i :=0 to rSearchFields.Count-1 do
          begin
               if (i > 0)
               then Result :=Result+' ';

               Result :=Result+rSearchDataSet.FieldByName(rSearchFields[i]).AsString;
          end
     else for i :=0 to rSearchDataSet.Fields.Count-1 do
          begin
               Result :=StringReplace(Result, ':'+rSearchDataSet.Fields[i].FieldName,
                                      rSearchDataSet.Fields[i].AsString, [rfReplaceAll]);
          end;
   end
   else Result :=GetTextFunc();
end;

procedure TMM_UI_AutoCompleteImpl.SearchGrid_OnSelect(Sender: TObject; AItemIndex :Integer);
Var
   newText :String;

begin
     newText :=getSelectedText;

     if Assigned(rOnSelect)
     then rOnSelect(Self, AItemIndex, newText);

     SetTextFunc(newText);
end;

procedure TMM_UI_AutoCompleteImpl.SearchGrid_OnShow(Sender: TObject);
begin
     edTimer.Enabled :=False;

     if Assigned(rOnPopupShow)
     then rOnPopupShow(Self);
end;

procedure TMM_UI_AutoCompleteImpl.SearchGrid_OnHide(Sender: TObject);
begin
     edTimer.Enabled :=False;
     Owner.SelStart :=Length(Text);

     if Assigned(rOnPopupHide)
     then rOnPopupHide(Self);
end;

procedure TMM_UI_AutoCompleteImpl.SearchButtons_Click(Sender: TObject;
  Button: TMM_UI_AutoCompleteButtonTypes; AItemIndex: Integer);
var
   mResult :TModalResult;
   newText :String;

begin
     if Assigned(rOnSearchButtonClick) then
     begin
          if (AItemIndex <= 0) and ((Button = acbEdit) or (Button = acbDelete))
          then Exit;

          newText :=Self.Text; //getSelectedText;
          mResult :=rOnSearchButtonClick(Self, Button, AItemIndex, newText);

          if (mResult = mrOk) or (mResult = mrYes) then
          begin
               Self.Field.AsString :=newText;
               Self.Search;
          end;
     end;
end;

constructor TMM_UI_AutoCompleteImpl.Create(AOwner: TWinControl; AGetTextFunc: TMM_UI_AutoCompleteGetText; ASetTextFunc: TMM_UI_AutoCompleteSetText);
begin
  inherited Create;

  Owner :=AOwner;
  GetTextFunc :=AGetTextFunc;
  SetTextFunc :=ASetTextFunc;
  PopupForm :=nil;
  rSearchGrid :=CreateGrid;
  rSearchFields :=TStringList.Create;
  rSearchButtonsHints :=TStringList.Create;
  rSearchButtonsHints.Add('Insert');
  rSearchButtonsHints.Add('Edit');
  rSearchButtonsHints.Add('Delete');
  rSearchMinChars :=2;
  rSearchGridHeight :=150;
  rSearchFieldsAND :=False;
  rSearchFieldOperator :='CONTAINING';
  rSelectTextValue :='';
  rSearchButtons :=[];
  rSearchButtonsAlign :=acbTopRight;
  Editing :=False;
  edTimer :=TTimer.Create(Self);
  edTimer.Enabled :=False;
  edTimer.Interval :=2000;
  edTimer.OnTimer :=@edTimerTimer;
  edTimer.OnStartTimer :=@edTimerStartStop;
  edTimer.OnStopTimer :=@edTimerStartStop;
end;

destructor TMM_UI_AutoCompleteImpl.Destroy;
begin
  rSearchGrid.Free;
  edTimer.Free;
  rSearchFields.Free;
  rSearchButtonsHints.Free;
  rSearchDataSource.Free;

  inherited Destroy;
end;

procedure TMM_UI_AutoCompleteImpl.setSearchDataSet(AValue :TSQLQuery);
begin
  if (rSearchDataSet <> AValue) then
  begin
       rSearchDataSet :=AValue;
       rSearchDataSource.DataSet :=rSearchDataSet;
  end;
end;

function TMM_UI_AutoCompleteImpl.getItemIndex: Integer;
begin
     if (rSearchDataSet <> nil)
     then Result :=rSearchDataSet.RecNo
     else Result :=0;
end;

function TMM_UI_AutoCompleteImpl.getLeft: Integer;
begin

end;

function TMM_UI_AutoCompleteImpl.getTop: Integer;
begin

end;

function TMM_UI_AutoCompleteImpl.getWidth: Integer;
begin

end;

function TMM_UI_AutoCompleteImpl.GetSearchColumns: TDBGridColumns;
begin
     Result :=rSearchGrid.Columns;
end;

procedure TMM_UI_AutoCompleteImpl.setSearchButtonsHints(AValue: TStringList);
begin
  if (AValue <> nil)
  then rSearchButtonsHints.Assign(AValue);
end;

procedure TMM_UI_AutoCompleteImpl.SetSearchColumns(AValue: TDBGridColumns);
begin
     if (AValue <> nil)
     then rSearchGrid.Columns.Assign(AValue);
end;

procedure TMM_UI_AutoCompleteImpl.setLeft(AValue: Integer);
begin
     if (PopupForm <> nil)
     then PopupForm.InitializeOrigin(TheOwner.ControlToScreen(Point(0, Height)));
end;

procedure TMM_UI_AutoCompleteImpl.setTop(AValue: Integer);
begin
     if (PopupForm <> nil)
     then PopupForm.InitializeOrigin(TheOwner.ControlToScreen(Point(0, Height)));
end;

procedure TMM_UI_AutoCompleteImpl.setWidth(AValue: Integer);
begin
     if (PopupForm <> nil)
     then begin
               PopupForm.Width :=Width;
               PopupForm.InitializeOrigin(TheOwner.ControlToScreen(Point(0, Height)));
          end;
end;

function TMM_UI_AutoCompleteImpl.getSearchPopupInterval: Cardinal;
begin
     Result :=edTimer.Interval;
end;

procedure TMM_UI_AutoCompleteImpl.setSearchPopupInterval(AValue: Cardinal);
begin
     edTimer.Interval :=AValue;
end;

procedure TMM_UI_AutoCompleteImpl.setSearchFields(AValue :TStringList);
begin
     if (AValue <> rSearchFields) then
       rSearchFields.Assign(AValue);
end;

procedure TMM_UI_AutoCompleteImpl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  Editing :=True;
  edTimer.Enabled :=False;
end;

procedure TMM_UI_AutoCompleteImpl.KeyUp(var Key: Word; Shift: TShiftState);
begin
  Editing :=False;
  edTimer.Enabled := not(Key in [VK_ESCAPE, VK_TAB, VK_RETURN, VK_PRIOR..VK_INSERT]);
end;

procedure TMM_UI_AutoCompleteImpl.KeyPress(var Key: char);
begin

end;

procedure TMM_UI_AutoCompleteImpl.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    Editing :=True;
    Self.edTimer.Enabled :=False;
    (*
    Case Key of
    VK_TAB :begin
                 Self.Field.AsString :=Self.Text;
                 if (ssShift in Shift)
                 then Self.SelectNext(Self, True, True)
                 else Self.SelectNext(Self, False, True);
            end;
    end;
    *)
end;

procedure TMM_UI_AutoCompleteImpl.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    Editing :=False;
    Self.edTimer.Enabled:=  not(Key in [VK_ESCAPE, VK_TAB, VK_RETURN, VK_PRIOR..VK_INSERT]);
end;

procedure TMM_UI_AutoCompleteImpl.GridKeyPress(Sender: TObject; var Key: Char);
var
   curText :String;

begin
     curText :=Self.Text; //Self.Field.AsString;
     Case Key of
     #8 :begin
              Delete(curText, Length(curText), 1);
              Self.Text :=curText; //Self.Field.AsString :=curText;
          end;
     else begin
               curText :=curText+Key;
               Self.Text :=curText; //Self.Field.AsString :=curText;
          end;
     end;
     Self.SelStart :=Length(curText);
end;

end.
