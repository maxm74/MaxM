unit MM_UI_DBAutoComplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DbCtrls, ExtCtrls, DBGrids, db, sqldb, LCLType, Buttons, MM_UI_AutoComplete;

type
    (*
       Non supportando il Pascal la Discendenza Multipla sono costretto a fare due
       implementazioni identiche,
       una per derivare da TEdit (in MM_UI_AutoComplete.pas),
       una per derivare da TDBEdit (in MM_UI_DBAutoComplete.pas)
    *)
    TMM_UI_DBAutoComplete = class;

    { TDBGridPopup }
    //TReturnSelectedEvent = procedure (Sender: TObject; AItemIndex :Integer) of object;

    TDBGridPopup_ = class(TForm)
    private
      function getItemIndex: Integer;
    protected
      Closed: boolean;
      Editor: TMM_UI_DBAutoComplete;
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
      procedure Initialize(AEditor: TMM_UI_DBAutoComplete; const PopupOrigin: TRect);
      procedure ReturnSelected;

      procedure Paint; override;

    public
      //OnReturnSelected: TReturnSelectedEvent;
      //OnSearchButtons : TReturnSelectedEvent;

      constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;

      property ItemIndex: Integer read getItemIndex;
    end;


    { TMM_UI_DBAutoComplete }
    TMM_UI_DBAutoComplete = class(TDBEdit)
    protected
      edTimer: TTimer;
      editing: Boolean;
      rSearchMinChars: Cardinal;
      rSearchDataSource: TDataSource;
      rSearchDataSet: TDataSet;
      rSearchGrid: TDBGrid;
      rSearchGridHeight: Integer;
      rSearchButtons: TMM_UI_AutoCompleteButtons;
      rSearchButtonsHints: TStringList;
      rSearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign;
      rSelectTextValue: String;
      //rSearchQuery: TStringList;
      rSearchQueryChanged: Boolean;
      PopupForm: TDBGridPopup_;

      rOnSearch: TNotifyEvent;
      rOnSelect: TMM_UI_AutoCompleteSelectEvent;
      rOnPopupHide: TNotifyEvent;
      rOnPopupShow: TNotifyEvent;
      rOnSearchButtonClick: TMM_UI_AutoCompleteButtonClick;

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
      procedure setSearchDataSet(AValue: TDataSet);
      procedure setSearchButtonsHints(AValue: TStringList);
      function getItemIndex: Integer;
      //procedure SetSearchQuery(AValue: TStringList);
      function getInternalText: String;
      procedure setInternalText(AValue: String);

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

      function getSelectedText: String; virtual;
      procedure SearchGrid_OnSelect(Sender: TObject; AItemIndex: Integer); virtual;
      procedure SearchGrid_OnShow(Sender: TObject); virtual;
      procedure SearchGrid_OnHide(Sender: TObject); virtual;
      procedure SearchButtons_Click(Sender: TObject; Button: TMM_UI_AutoCompleteButtonTypes; AItemIndex: Integer); virtual;

      //TDBGridPopup_ use this Property to get-set Text so i can do a single implementation of TDBGridPopup_
      property InternalText: String read getInternalText write setInternalText;
      property SearchQueryChanged: Boolean read rSearchQueryChanged write rSearchQueryChanged stored True;
    public
      constructor Create(TheOwner: TComponent); override;
      destructor Destroy; override;

      property ItemIndex: Integer read getItemIndex;
    published
      property SearchPopupInterval: Cardinal read getSearchPopupInterval write setSearchPopupInterval default 1500;
      property SearchMinChars: Cardinal read rSearchMinChars write rSearchMinChars default 2;
      property SearchDataSet: TDataSet read rSearchDataSet write setSearchDataSet;
      //property SearchQuery: TStringList read rSearchQuery write SetSearchQuery;
      property SearchColumns: TDBGridColumns read GetSearchColumns write SetSearchColumns;
      property SearchGridHeight: Integer read rSearchGridHeight write rSearchGridHeight default 150;
      property SearchButtons: TMM_UI_AutoCompleteButtons read rSearchButtons write rSearchButtons default [];
      property SearchButtonsHints: TStringList read rSearchButtonsHints write setSearchButtonsHints;
      property SearchButtonsAlign: TMM_UI_AutoCompleteButtonsAlign read rSearchButtonsAlign write rSearchButtonsAlign default acbTopRight;

      property SelectTextValue: String read rSelectTextValue write rSelectTextValue;
      property Width: Integer read getWidth write setWidth;
      property Left: Integer read getLeft write setLeft;
      property Top: Integer read getTop write setTop;

      property OnSearch: TNotifyEvent read rOnSearch write rOnSearch;
      property OnSelect: TMM_UI_AutoCompleteSelectEvent read rOnSelect write rOnSelect;
      property OnPopupShow: TNotifyEvent read rOnPopupShow write rOnPopupShow;
      property OnPopupHide: TNotifyEvent read rOnPopupHide write rOnPopupHide;
      property OnSearchButtonClick: TMM_UI_AutoCompleteButtonClick read rOnSearchButtonClick write rOnSearchButtonClick;
    end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBAutoComplete]);
end;

{ TDBGridPopup_ }

function TDBGridPopup_.getItemIndex: Integer;
begin
     try
        Result :=Editor.SearchDataSet.RecNo;
     except
        Result :=0;
     end;
end;

procedure TDBGridPopup_.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Closed := True;
  Application.RemoveOnDeactivateHandler(@FormDeactivate);
  CloseAction := caFree;
  Editor.PopupForm :=Nil;
end;

procedure TDBGridPopup_.FormDeactivate(Sender: TObject);
begin
     if (Self.ItemIndex = 0)
     then Editor.InternalText :=Editor.Text;

     if not(Closed)
     then Close;
end;

procedure TDBGridPopup_.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    case Key of
    VK_ESCAPE : begin
                     Editor.InternalText :=oldText;
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
                     else Editor.InternalText :=Editor.Text;

                     if (ssShift in Shift)
                     then Editor.Parent.SelectNext(Editor, False, True)
                     else Editor.Parent.SelectNext(Editor, True, True);
                     Key :=0;
                end;
    VK_PRIOR..VK_DOWN : begin end;
    else Editor.GridKeyDown(Self, Key, Shift);
    end;
end;

procedure TDBGridPopup_.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     Case Key of
     VK_ESCAPE, VK_RETURN : Key :=0;
     VK_PRIOR..VK_DOWN : begin end;
     else Editor.GridKeyUp(Self, Key, Shift);
     end;
end;

procedure TDBGridPopup_.GridKeyPress(Sender: TObject; var Key: Char);
begin
     Editor.GridKeyPress(Self, Key);
end;

procedure TDBGridPopup_.GridCellClick(Column: TColumn);
begin
     ReturnSelected;
end;

procedure TDBGridPopup_.SearchButtonsClick(Sender: TObject);
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

procedure TDBGridPopup_.InitializeOrigin(const PopupOrigin: TPoint);
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

procedure TDBGridPopup_.Initialize(AEditor: TMM_UI_DBAutoComplete; const PopupOrigin: TRect);
Var
   x :Cardinal;

begin
     Editor :=AEditor;
     oldText :=Editor.InternalText;

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


procedure TDBGridPopup_.ReturnSelected;
begin
  if (Self.ItemIndex > 0)
  then Editor.SearchGrid_OnSelect(Self, Self.ItemIndex);

  if not(Closed)
  then Close;
end;

procedure TDBGridPopup_.Paint;
begin
  inherited Paint;

  Canvas.Pen.Color :=clWindowText;
  Canvas.Pen.Style :=psSolid;
  Canvas.Rectangle(0, 0, Width-1, Height-1);
end;

constructor TDBGridPopup_.CreateNew(AOwner: TComponent; Num: Integer = 0);
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
     edTimer.Enabled :=False;

     if Length(Text)>2
     then Search;
end;

procedure TMM_UI_DBAutoComplete.edTimerStartStop(Sender :TObject);
begin
end;

procedure TMM_UI_DBAutoComplete.Search;
begin
     if not(csDesigning in ComponentState) and  //Paranoic Think
        (rSearchDataSet <> Nil) then
     begin
          rSearchDataSet.Close;

          if (rSearchDataSet is TSQLQuery) then
          begin
               //if (rSearchQuery.Text <> DefaultSearchQuery)
               //then TSQLQuery(rSearchDataSet).SQL.Assign(rSearchQuery);
               TSQLQuery(rSearchDataSet).ParamByName('SEARCH_TXT').AsString := Uppercase(Self.Text);
          end;

          if Assigned(rOnSearch)
          then rOnSearch(Self);

          rSearchDataSet.Open;

          if (PopupForm = Nil)
          then ShowGrid;
     end;
end;

procedure TMM_UI_DBAutoComplete.ShowGrid;
var
  PopupOrigin :TRect;

begin
     PopupOrigin.TopLeft :=ControlToScreen(Point(0, Height));
     PopupOrigin.Right :=Self.Width;
     PopupOrigin.Bottom :=Self.rSearchGridHeight;

     PopupForm :=TDBGridPopup_.CreateNew(nil);

     PopupForm.Initialize(Self, PopupOrigin);
     //PopupForm.OnReturnSelected :=@SearchGrid_OnSelect;
     PopupForm.OnShow :=@SearchGrid_OnShow;
     PopupForm.OnHide :=@SearchGrid_OnHide;
     PopupForm.Show;
end;

function TMM_UI_DBAutoComplete.CreateGrid: TDBGrid;
begin
     Result :=TDBGrid.Create(Self);
     Result.DataSource :=rSearchDataSource;
     Result.Flat :=True;
     Result.ReadOnly :=True;
     Result.ScrollBars :=ssAutoBoth;
     Result.Options :=[dgColLines, dgCellHints, dgRowSelect, dgDisableDelete, dgDisableInsert];
end;

function TMM_UI_DBAutoComplete.getSelectedText: String;
var
   i :Integer;

begin
   if (rSearchDataSet.RecNo >= 0) then
   begin
     Result :=rSelectTextValue;
     if (rSelectTextValue = '')
     then for i :=0 to rSearchDataSet.Fields.Count-1 do
          begin
               if (i > 0)
               then Result :=Result+' ';

               Result :=Result+rSearchDataSet.Fields[i].AsString;
          end
     else for i :=0 to rSearchDataSet.Fields.Count-1 do
          begin
               Result :=StringReplace(Result, ':'+rSearchDataSet.Fields[i].FieldName,
                                      rSearchDataSet.Fields[i].AsString, [rfReplaceAll]);
          end;
   end
   else Result :=Self.Text;
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnSelect(Sender: TObject; AItemIndex :Integer);
Var
   newText :String;

begin
     newText :=getSelectedText;

     if Assigned(rOnSelect)
     then rOnSelect(Self, AItemIndex, newText);

     Self.InternalText :=newText;
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnShow(Sender: TObject);
begin
     edTimer.Enabled :=False;

     if Assigned(rOnPopupShow)
     then rOnPopupShow(Self);
end;

procedure TMM_UI_DBAutoComplete.SearchGrid_OnHide(Sender: TObject);
begin
     edTimer.Enabled :=False;
     SelStart :=Length(Text);

     if Assigned(rOnPopupHide)
     then rOnPopupHide(Self);
end;

procedure TMM_UI_DBAutoComplete.SearchButtons_Click(Sender: TObject;
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
               Self.InternalText :=newText;
               Self.Search;
          end;
     end;
end;

constructor TMM_UI_DBAutoComplete.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  PopupForm :=nil;
  rSearchDataSource :=TDataSource.Create(Self);
  //rSearchQuery :=TStringList.Create;
  //rSearchQuery.Add(DefaultSearchQuery);
  rSearchGrid :=CreateGrid;
  rSearchButtonsHints :=TStringList.Create;
  rSearchButtonsHints.Add('Insert');
  rSearchButtonsHints.Add('Edit');
  rSearchButtonsHints.Add('Delete');
  rSearchMinChars :=2;
  rSearchGridHeight :=150;
  rSelectTextValue :='';
  rSearchButtons :=[];
  rSearchButtonsAlign :=acbTopRight;
  Editing :=False;
  edTimer :=TTimer.Create(Self);
  edTimer.Enabled :=False;
  edTimer.Interval :=1500;
  edTimer.OnTimer :=@edTimerTimer;
  edTimer.OnStartTimer :=@edTimerStartStop;
  edTimer.OnStopTimer :=@edTimerStartStop;
end;

destructor TMM_UI_DBAutoComplete.Destroy;
begin
  rSearchGrid.Free;
  edTimer.Free;
  rSearchButtonsHints.Free;
  rSearchDataSource.Free;
  //rSearchQuery.Free;

  inherited Destroy;
end;

procedure TMM_UI_DBAutoComplete.setSearchDataSet(AValue :TDataSet);
begin
  if (rSearchDataSet <> AValue) then
  begin
       rSearchDataSet :=AValue;
       rSearchDataSource.DataSet :=rSearchDataSet;
  end;
end;

function TMM_UI_DBAutoComplete.getItemIndex: Integer;
begin
     if (rSearchDataSet <> nil)
     then Result :=rSearchDataSet.RecNo
     else Result :=0;
end;

function TMM_UI_DBAutoComplete.GetSearchColumns: TDBGridColumns;
begin
     Result :=rSearchGrid.Columns;
end;

procedure TMM_UI_DBAutoComplete.setSearchButtonsHints(AValue: TStringList);
begin
  if (AValue <> nil)
  then rSearchButtonsHints.Assign(AValue);
end;

(*
procedure TMM_UI_DBAutoComplete.SetSearchQuery(AValue: TStringList);
begin
  rSearchQuery.Assign(AValue);
end;
*)

function TMM_UI_DBAutoComplete.getInternalText: String;
begin
     Result :=Self.Field.AsString;
end;

procedure TMM_UI_DBAutoComplete.setInternalText(AValue: String);
begin
     Self.Field.AsString :=AValue;
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
     if (AValue <> nil)
     then rSearchGrid.Columns.Assign(AValue);
end;

procedure TMM_UI_DBAutoComplete.setLeft(AValue: Integer);
begin
     inherited Left :=AValue;
     if (PopupForm <> nil)
     then PopupForm.InitializeOrigin(ControlToScreen(Point(0, Height)));
end;

procedure TMM_UI_DBAutoComplete.setTop(AValue: Integer);
begin
     inherited Top :=AValue;
     if (PopupForm <> nil)
     then PopupForm.InitializeOrigin(ControlToScreen(Point(0, Height)));
end;

procedure TMM_UI_DBAutoComplete.setWidth(AValue: Integer);
begin
     inherited Width :=AValue;
     if (PopupForm <> nil)
     then begin
               PopupForm.Width :=Width;
               PopupForm.InitializeOrigin(ControlToScreen(Point(0, Height)));
          end;
end;

function TMM_UI_DBAutoComplete.getSearchPopupInterval: Cardinal;
begin
     Result :=edTimer.Interval;
end;

procedure TMM_UI_DBAutoComplete.setSearchPopupInterval(AValue: Cardinal);
begin
     edTimer.Interval :=AValue;
end;

procedure TMM_UI_DBAutoComplete.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  Editing :=True;
  edTimer.Enabled :=False;
end;

procedure TMM_UI_DBAutoComplete.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);

  Editing :=False;
  edTimer.Enabled := not(Key in [VK_ESCAPE, VK_TAB, VK_RETURN, VK_PRIOR..VK_INSERT]);
end;

procedure TMM_UI_DBAutoComplete.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;

procedure TMM_UI_DBAutoComplete.GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

procedure TMM_UI_DBAutoComplete.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    Editing :=False;
    Self.edTimer.Enabled:=  not(Key in [VK_ESCAPE, VK_TAB, VK_RETURN, VK_PRIOR..VK_INSERT]);
end;

procedure TMM_UI_DBAutoComplete.GridKeyPress(Sender: TObject; var Key: Char);
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
