unit MM_UI_DBGrid;
{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Graphics, Grids, db, DBGrids;

type
  TMM_UI_DBGrid = class;

  { TMM_UI_DBGrid }
  TMM_UI_DBGridColumn = class(TColumn)
  protected
    rGrid: TMM_UI_DBGrid;

    procedure ColumnChanged; override;

  public
    constructor Create(ACollection: TCollection); override;
  end;

  { TMM_UI_DBGridColumn }
  TMM_UI_DBGrid = class(TDBGrid)
  private
    rColorFieldName: String;
    function GetShortDateFormat: String;
    procedure SetColorFieldName(AValue: String);
    procedure SetShortDateFormat(AValue: String);

  protected
    rCaptionsList: TStringList;

    procedure SetCaptionsList(AValue: TStringList);
    function CreateColumns: TGridColumns; override;

    procedure DrawColumnText(aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState); override;
    procedure DrawCell(aCol,aRow: Integer; aRect: TRect; aState:TGridDrawState); override;

    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure LoadFromINI(Section, FileName: String);

  published
    property CaptionsList :TStringList read  rCaptionsList
                                       write SetCaptionsList;
    property ShortDateFormat: String read GetShortDateFormat
                                     write SetShortDateFormat;
    property ColorFieldName: String read rColorFieldName
                                    write SetColorFieldName;
  end;

procedure Register;

implementation

uses inifiles;

procedure Register;
begin
  RegisterComponents('MaxM_UI',[TMM_UI_DBGrid]);
end;

{ TMM_UI_DBGridColumn }

procedure TMM_UI_DBGridColumn.ColumnChanged;
var
   colCaption :String;
   curField   :TField;

begin
  curField :=Field;
  if (curField<>Nil) then
  begin
       colCaption :=rGrid.CaptionsList.Values[curField.FieldName];
       if (colCaption<>'')
       then curField.DisplayLabel :=colCaption;
  end;
  inherited ColumnChanged;
end;

constructor TMM_UI_DBGridColumn.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  if (ACollection is TDBGridColumns)
  then rGrid :=TMM_UI_DBGrid(TDBGridColumns(ACollection).Grid);
end;


{ TMM_UI_DBGrid }

function TMM_UI_DBGrid.GetShortDateFormat: String;
begin
     Result :=DefaultFormatSettings.ShortDateFormat;
end;

procedure TMM_UI_DBGrid.SetColorFieldName(AValue: String);
Var
   i, k   :Integer;
   Finded :Boolean;
   F      :TField;
   colColumn :TColumn;

begin
  if (rColorFieldName<>AValue)
  then begin
            rColorFieldName := AValue;

(*
     if (rColorFieldName <> '') and (Self.Columns <> Nil) then
     begin
          i := Self.Columns.Count;
          Finded :=False;
          While (i > 0) and (not(Finded)) do
          begin
               dec(i);
               F := GetFieldFromGridColumn(i);
               Finded := (F <> Nil) and (F.FieldName = rColorFieldName);
          end;
          if Finded then
          begin
               colColumn := Self.Columns.Items[i];
               for k := i downto 1 do
               begin
                    Self.Columns.Items[k] := Self.Columns.Items[k+1];
               end;
               Self.Columns.Items[0] := colColumn;
          end;
     end;
*)
        end;
end;

procedure TMM_UI_DBGrid.SetShortDateFormat(AValue: String);
begin
     DefaultFormatSettings.ShortDateFormat :=AValue;
end;

procedure TMM_UI_DBGrid.SetCaptionsList(AValue: TStringList);
begin
  if (AValue<>Nil)
  then rCaptionsList.Assign(AValue);
end;

function TMM_UI_DBGrid.CreateColumns: TGridColumns;
begin
     Result := TDBGridColumns.Create(Self, TMM_UI_DBGridColumn);
end;

procedure TMM_UI_DBGrid.DrawColumnText(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  F: TField;

begin
     F := GetFieldFromGridColumn(aCol);
     if (F <> Nil) and (F.FieldName <> '') and (F.FieldName = rColorFieldName)
     then DrawCellText(aCol, aRow, aRect, aState, '')
     else inherited DrawColumnText(aCol, aRow, aRect, aState);
end;

procedure TMM_UI_DBGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
var
  F: TField;
  DataCol,
  oldCol :TColor;
  dRect :TRect;

begin
  if (ARow>=FixedRows) then
  begin
     F := GetFieldFromGridColumn(aCol);
     if (F <> Nil) and
        (F.FieldName <> '') and (F.FieldName = rColorFieldName) then
     begin
          PrepareCanvas(aCol, aRow, aState);

          // I Need only Background from DefaultDrawCell
          if (gdFixed in aState) and (TitleStyle=tsNative) then
            DrawThemedCell(aCol, aRow, aRect, aState)
          else
            Canvas.FillRect(aRect);

          DataCol :=TColor(F.AsInteger);
          if not(F.IsNull) and (DataCol<>clNone)
          then begin
                    oldCol :=Canvas.Brush.Color;
                    Canvas.Brush.Color :=DataCol;
                    dRect :=aRect;
                    inc(dRect.Top, 1);
                    inc(dRect.Left, 1);
                    dec(dRect.Right, 2);
                    dec(dRect.Bottom, 2);
                    Canvas.FillRect(dRect);
                    Canvas.Brush.Color :=oldCol;
               end;
          DrawCellGrid(aCol, aRow, aRect, aState);
     end
     else inherited DrawCell(aCol, aRow, aRect, aState);
  end
  else inherited DrawCell(aCol, aRow, aRect, aState);
end;

procedure TMM_UI_DBGrid.Loaded;
Var
   curCol     :TColumn;
   colCaption :String;
   i          :Integer;

begin
     inherited Loaded;
     for i:=0 to Self.Columns.Count-1 do
     begin
          curCol := Self.Columns.Items[i];
          if (curCol <> Nil) then
          begin
               if (curCol.FieldName = rColorFieldName)
               then curCol.Title.Caption := ''
               else begin
                         colCaption := rCaptionsList.Values[curCol.FieldName];
                         if (colCaption<>'')
                         then curCol.Title.Caption :=colCaption;
                    end;
          end;
     end;
end;

constructor TMM_UI_DBGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rCaptionsList :=TStringList.Create;
end;

destructor TMM_UI_DBGrid.Destroy;
begin
  rCaptionsList.Free;
  inherited Destroy;
end;

procedure TMM_UI_DBGrid.LoadFromINI(Section, FileName: String);
var
   theINI :TIniFile;

begin
     if FileExists(FileName) then
     begin
          theINI :=TIniFile.Create(FileName);
          if theINI.SectionExists(Section) then
          begin
               theINI.ReadSectionValues(Section, rCaptionsList);
           end;
          theINI.Free;
      end;
end;

end.
