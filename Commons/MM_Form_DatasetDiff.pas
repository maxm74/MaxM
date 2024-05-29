//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2015                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_Form_DatasetDiff.pas
//
//  Description : A Form that Show Diff between two Datasets,
//                 The User can set the Destination Values (Left Side).
//
//******************************************************************************
//
//TODO: Delete the Red Color when user set Destination Value as Source Value.

unit MM_Form_DatasetDiff;
{$mode objfpc}{$H+}

//{$define DEBUG}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  PairSplitter, Buttons, StdCtrls, db, DBCtrls, ButtonPanel;

{ TFormDatasetDiff }

type
  TFormDatasetDiff = class(TForm)
    ButtonPanel: TButtonPanel;
    DataSourceSource: TDataSource;
    DataSourceDest: TDataSource;
    imgList: TImageList;
    PanelRight: TPanel;
    PanelCaptions: TPanel;
    PanelLeft: TPanel;
    sbMain: TScrollBox;
    btShowEquals: TSpeedButton;
    procedure btShowEqualsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    DestDataset, SourceDataset: TDataset;
    CaptionsStrings: TStrings;
    OthersCaption: String;
    ShowEquals: Boolean;
    diffFields: TStringList;

    procedure SetBtnClick(Sender: TObject);
    procedure UndoBtnClick(Sender: TObject);

    procedure SetButtonRedColor(Sender: TSpeedButton; isRed :Boolean);
    function CreateFieldPanel(ATop: Integer;
                              leftField, rightField: TField;
                              FieldCaption: String; isRed: Boolean; AParent: TWinControl; ResizeParent: Boolean): TPanel;
    function CreateSectionPanel(AName, ACaption: String; AParent: TWinControl): TPanel;
    procedure CreateStructure;
    procedure CreateStructureWithSections;
  public

    function Execute(ADestDataset, ASourceDataset: TDataset;
                     AShowEquals: Boolean = True;
                     ACaptionsStrings: TStrings = nil;
                     DestCaption: String = 'Destination'; SourceCaption: String = 'Source'; AOthersCaption: String = 'Others'): TModalResult;
  end;

var
  FormDatasetDiff: TFormDatasetDiff = nil;

function FormDatasetDiffShow(ADestDataset, ASourceDataset: TDataset;
                     AShowEquals: Boolean = True;
                     ACaptionsStrings: TStrings = nil;
                     DestCaption:String = 'Destination'; SourceCaption: String = 'Source'): TModalResult;

implementation
{$R *.lfm}

uses MM_StringListList;

const
  clDiffRed = $005252FF;

function FormDatasetDiffShow(ADestDataset, ASourceDataset: TDataset;
                     AShowEquals: Boolean = True;
                     ACaptionsStrings: TStrings = nil;
                     DestCaption: String = 'Destination'; SourceCaption: String = 'Source'): TModalResult;
begin
  if (FormDatasetDiff = nil)
  then FormDatasetDiff :=TFormDatasetDiff.Create(Application);

  Result :=FormDatasetDiff.Execute(ADestDataset, ASourceDataset,
                                   AShowEquals,
                                   ACaptionsStrings, DestCaption, SourceCaption);

  FreeAndNil(FormDatasetDiff);
end;


{ TFormDatasetDiff }

procedure TFormDatasetDiff.UndoBtnClick(Sender: TObject);
var
   rightField,
   leftField: TField;
   CurValue,
   OldValue: String;

begin
     leftField :=TField(TSpeedButton(Sender).Tag);
     rightField :=SourceDataset.FieldByName(leftField.FieldName);
     CurValue :=leftField.AsString;
     //Use Hint as Old Value because TBufDataset don't store Field.OldValue
     OldValue :=TSpeedButton(Sender).Hint;
     try
        DataSourceDest.Edit;
        leftField.AsString :=OldValue;
        //SetButtonRedColor(TSpeedButton(OtherBtn), (rightField <> nil) and (leftField.AsString <> rightField.AsString));
     except
        leftField.AsString :=CurValue;
     end;
end;

procedure TFormDatasetDiff.btShowEqualsClick(Sender: TObject);
var
   i: Integer;
   curControl: TControl;

begin
  for i:=0 to sbMain.ControlCount-1 do
  begin
    curControl :=sbMain.Controls[0];
    if (curControl <> nil) then
    begin
         curControl.Parent :=nil;

         FreeAndNil(curControl);
    end;
  end;

  ShowEquals :=btShowEquals.Down;

  if (CaptionsStrings = nil)
  then CreateStructure
  else if (CaptionsStrings is TStringListList)
       then CreateStructureWithSections
       else CreateStructure;
end;

procedure TFormDatasetDiff.SetBtnClick(Sender: TObject);
var
   rightField,
   leftField: TField;
   leftValue,
   rightValue: variant;

begin
     leftField :=TField(TSpeedButton(Sender).Tag);
     rightField :=SourceDataset.FieldByName(leftField.FieldName);
     if (rightField <> nil) then
     begin
          rightValue :=rightField.AsVariant;
          leftValue :=leftField.AsVariant;
          try
            DataSourceDest.Edit;
            leftField.AsVariant :=rightValue;
            //SetButtonRedColor(TSpeedButton(Sender), (rightField <> nil) and (leftField.AsString <> rightField.AsString));
          except
             leftField.AsVariant :=leftValue;
          end;
     end;
end;

procedure TFormDatasetDiff.SetButtonRedColor(Sender: TSpeedButton; isRed: Boolean);
begin
     if isRed
     then begin
            Sender.Transparent :=False;
            Sender.Color :=clDiffRed;
           end
     else begin
            Sender.Color :=clDefault;
            Sender.Transparent :=True;
           end;
end;

function TFormDatasetDiff.CreateFieldPanel(ATop: Integer;
                                           leftField, rightField: TField;
                                           FieldCaption: String; isRed: Boolean; AParent: TWinControl;
                                           ResizeParent: Boolean): TPanel;
var
   xEditor: TDBEdit;
   xLabel: TLabel;
   xButton: TSpeedButton;
   xBitmap: TBitmap;
   edWidth, curLeft: Integer;
   FieldIndex: Integer;
   FieldName: String;

begin
  xBitmap :=TBitmap.Create;
  try
     Result :=TPanel.Create(AParent);
     Result.Parent :=AParent;
     Result.BorderStyle :=bsNone;
     Result.BevelOuter :=bvNone;
     Result.BevelInner :=bvNone;
     Result.ParentColor :=True;

     if (AParent is TScrollBox)
     then Result.SetBounds(0, ATop, AParent.Width-22, 40)
     else Result.SetBounds(0, ATop, AParent.Width, 40);

     if ResizeParent
     then AParent.Height := AParent.Height+40+4;

     edWidth :=(Result.Width div 2)-22;
     curLeft :=23;

     xLabel :=TLabel.Create(Result);
     xLabel.Parent :=Result;
     xLabel.SetBounds(0, 0, Result.Width, 14);

     if (leftField <> nil) then
     begin
       FieldName :=leftField.FieldName;
       FieldIndex :=leftField.Index;

       xButton :=TSpeedButton.Create(Result);
       xButton.Parent :=Result;
       xButton.SetBounds(0, 16, 22, 22);
       xButton.Flat :=True;
       imgList.GetBitmap(0, xBitmap);
       xButton.Glyph :=xBitmap;
       xButton.Tag :=PtrInt(leftField);
       xButton.Hint :=leftField.AsString;
       xButton.OnClick :=@UndoBtnClick;

       xEditor :=TDBEdit.Create(Result);
       xEditor.Parent :=Result;
       xEditor.SetBounds(curLeft, 17, edWidth, 20);
       xEditor.DataSource :=DataSourceDest;
       xEditor.DataField :=FieldName;
       if (DestDataset.FieldDefs[FieldIndex].DataType = ftString)
       then xEditor.MaxLength :=DestDataset.FieldDefs[FieldIndex].Size;
       inc(curLeft, edWidth);

       if (rightField <> nil) then
       begin
            xButton :=TSpeedButton.Create(Result);
            xButton.Parent :=Result;
            xButton.SetBounds(curLeft, 16, 22, 22);
            xButton.Flat :=True;
            SetButtonRedColor(xButton, isRed);
            imgList.GetBitmap(1, xBitmap);
            xButton.Glyph :=xBitmap;
            xButton.Tag :=PtrInt(leftField);
            xButton.OnClick :=@SetBtnClick;
            inc(curLeft, 22);
        end;
     end
     else begin
            if (rightField <> nil) then
            begin
                 FieldName :=rightField.FieldName;
                 FieldIndex :=rightField.Index;
            end;

            xLabel.Alignment :=taRightJustify;
            inc(curLeft, 22+edWidth);
          end;

     if (rightField <> nil) then
     begin
       xEditor :=TDBEdit.Create(Result);
       xEditor.Parent :=Result;
       xEditor.SetBounds(curLeft, 17, edWidth, 20);
       xEditor.DataSource :=DataSourceSource;
       xEditor.DataField :=FieldName;
       xEditor.ReadOnly :=True;
     end;

     if (FieldCaption = '')
     then xLabel.Caption :=FieldName
     else xLabel.Caption :=FieldCaption {$ifdef DEBUG}+' ('+FieldName+')'{$endif};

  finally
     xBitmap.Free;
  end;
end;

function TFormDatasetDiff.CreateSectionPanel(AName, ACaption: String; AParent: TWinControl): TPanel;
var
   xLabel: TLabel;

begin
     Result :=TPanel.Create(AParent);
     Result.Parent :=AParent;
     Result.Name :=AName;
     Result.Caption :='';
     Result.BorderStyle :=bsNone;
     Result.BevelOuter :=bvNone;
     Result.BevelInner :=bvNone;
     Result.ParentColor :=True;

     if (AParent is TScrollBox)
     then Result.Width :=AParent.Width-22
     else Result.Width :=AParent.Width;

     xLabel :=TLabel.Create(Result);
     xLabel.Parent :=Result;
     xLabel.AutoSize :=False;
     xLabel.Top :=0; xLabel.Left :=0;
     xLabel.Width :=Result.Width;
     xLabel.Alignment :=taCenter;
     xLabel.Color :=clBlack;
     xLabel.Font.Color :=clWhite;
     xLabel.Caption :=ACaption;
     xLabel.Height :=xLabel.Canvas.TextHeight(ACaption);

     Result.Height :=xLabel.Height;
end;

procedure TFormDatasetDiff.CreateStructure;
var
   curFieldPanel: TPanel;
   iField,
   curTop: Integer;
   curField,
   curFieldSrc: TField;
   curCaption: String;
   isRed, diffFieldsEmpty: Boolean;

begin
     diffFieldsEmpty :=(diffFields.Count < 1);

     curTop :=0;
     for iField:=0 to DestDataset.Fields.Count-1 do
     begin
       curField :=DestDataset.Fields[iField];
       curFieldSrc :=SourceDataset.FieldByName(curField.FieldName);

       if diffFieldsEmpty
       then begin
              isRed :=(curFieldSrc <> nil) and (curField.AsString <> curFieldSrc.AsString);

              //Is Different on Startup, set as Red
              if isRed
              then diffFields.Add(curField.FieldName);
            end
       else isRed :=(diffFields.IndexOf(curField.FieldName) > -1);

       if isRed or ShowEquals then
       begin
         if (CaptionsStrings = nil)
         then curCaption :=curField.FieldName
         else curCaption :=CaptionsStrings.Values[curField.FieldName];

         curFieldPanel :=CreateFieldPanel(curTop,
                                          curField, curFieldSrc,
                                          curCaption, isRed,
                                          sbMain, False);

         inc(curTop, curFieldPanel.Height+4);
       end;
     end;

     //Fields in Source but not in Dest
     //if ShowEquals then
     for iField:=0 to SourceDataset.Fields.Count-1 do
     begin
          curFieldSrc :=SourceDataset.Fields[iField];
          if (DestDataset.FieldByName(curFieldSrc.FieldName) = nil) then
          begin
            if (CaptionsStrings = nil)
            then curCaption :=curFieldSrc.FieldName
            else curCaption :=CaptionsStrings.Values[curFieldSrc.FieldName];

            curFieldPanel :=CreateFieldPanel(curTop,
                                             nil, curFieldSrc, curCaption, False,
                                             sbMain, False);
            inc(curTop, curFieldPanel.Height+4);
          end;
     end;
end;

procedure TFormDatasetDiff.CreateStructureWithSections;
var
   curFieldPanel,
   curSectionPanel: TPanel;
   iField,
   curTop: Integer;
   curField,
   curFieldSrc: TField;
   curCaption: String;
   curSection :TStringList;
   curSection_Index :Integer;
   curSection_IndexInList :Integer;
   isRed, diffFieldsEmpty: Boolean;

begin
     diffFieldsEmpty :=(diffFields.Count < 1);

     for iField:=0 to DestDataset.Fields.Count-1 do
     begin
       curField :=DestDataset.Fields[iField];
       curFieldSrc :=SourceDataset.FieldByName(curField.FieldName);

       if diffFieldsEmpty
       then begin
              isRed :=(curFieldSrc <> nil) and (curField.AsString <> curFieldSrc.AsString);

              //Is Different on Startup, set as Red
              if isRed
              then diffFields.Add(curField.FieldName);
            end
       else isRed :=(diffFields.IndexOf(curField.FieldName) > -1);

       if isRed or ShowEquals then //not(not(ShowEquals) and isRed) then
       begin
         curSection :=TStringListList(CaptionsStrings).FindListContaining(curField.FieldName, curSection_Index, curSection_IndexInList);
         if (curSection <> nil)
         then begin
                curSectionPanel :=TPanel(sbMain.FindChildControl('section_'+IntToStr(curSection_Index)));
                if (curSectionPanel = nil)
                then curSectionPanel :=CreateSectionPanel('section_'+IntToStr(curSection_Index), TStringListList(CaptionsStrings).Strings[curSection_Index], sbMain);

                curCaption :=curSection.ValueFromIndex[curSection_IndexInList];
              end
         else begin
                curSectionPanel :=TPanel(sbMain.FindChildControl('section_Oth'));
                if (curSectionPanel = nil)
                then curSectionPanel :=CreateSectionPanel('section_Oth', OthersCaption, sbMain);

                curCaption :='';
              end;

          curFieldPanel :=CreateFieldPanel(curSectionPanel.Height,
                                           curField, curFieldSrc,
                                           curCaption, isRed,
                                           curSectionPanel, True);
       end;
     end;

     //Fields in Source but not in Dest
     //if ShowEquals then
     for iField:=0 to SourceDataset.Fields.Count-1 do
     begin
          curFieldSrc :=SourceDataset.Fields[iField];
          if (DestDataset.FieldByName(curFieldSrc.FieldName) = nil) then
          begin
               curSectionPanel :=TPanel(sbMain.FindChildControl('section_Oth'));
               if (curSectionPanel = nil)
               then curSectionPanel :=CreateSectionPanel('section_Oth', OthersCaption, sbMain);

               curCaption :=TStringListList(CaptionsStrings).GetValueOf(curFieldSrc.FieldName, True);

               curFieldPanel :=CreateFieldPanel(curSectionPanel.Height,
                                                nil, curFieldSrc, curCaption, False,
                                                curSectionPanel, True);
           end;
     end;

     curTop :=0;
     for curSection_Index :=0 to TStringListList(CaptionsStrings).Count-1 do
     begin
       curSectionPanel :=TPanel(sbMain.FindChildControl('section_'+IntToStr(curSection_Index)));
       if (curSectionPanel <> nil) then
       begin
            curSectionPanel.Top :=curTop;
            inc(curTop, curSectionPanel.Height+8);
        end;
     end;
     curSectionPanel :=TPanel(sbMain.FindChildControl('section_Oth'));
     if (curSectionPanel <> nil) then
     begin
       curSectionPanel.Top :=curTop;
       //inc(curTop, curSectionPanel.Height+8);
     end;
end;

function TFormDatasetDiff.Execute(ADestDataset, ASourceDataset: TDataset;
                                  AShowEquals: Boolean;
                                  ACaptionsStrings: TStrings;
                                  DestCaption: String; SourceCaption: String; AOthersCaption: String): TModalResult;
begin
     PanelLeft.Caption :=DestCaption;
     PanelRight.Caption :=SourceCaption;
     OthersCaption :=AOthersCaption;
     DestDataset :=ADestDataset;
     SourceDataset :=ASourceDataset;
     DataSourceDest.DataSet :=DestDataset;
     DataSourceSource.DataSet :=SourceDataset;
     CaptionsStrings :=ACaptionsStrings;
     ShowEquals :=AShowEquals;
     btShowEquals.Down :=ShowEquals;

     if not(DestDataset.CanModify)
     then raise Exception.Create('(DatasetDiff) Destination Dataset cannot be Modified');

     //We Use this List to store wich Fields is Different on Startup.
     // If we have a Field "RED" setting it's value as the Source value will not
     // remove it when we show only Different Values
     diffFields.Clear;

     if (CaptionsStrings = nil)
     then CreateStructure
     else if (CaptionsStrings is TStringListList)
          then CreateStructureWithSections
          else CreateStructure;

     Result :=Self.ShowModal;
end;

procedure TFormDatasetDiff.FormCreate(Sender: TObject);
begin
  diffFields :=TStringList.Create;
end;

procedure TFormDatasetDiff.FormDestroy(Sender: TObject);
begin
  FreeAndNil(diffFields);
end;



end.

