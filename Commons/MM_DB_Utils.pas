//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2007-2015                                        ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_DB_Utils.pas
//
//  Description : Datasets Utils Functions.
//
//******************************************************************************

//MaxM 04-12-2015
//  RecordSerialize/deserialize:
//    Added store Null Field as [null] and Quoted for String Fields
//

unit MM_DB_Utils;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db;

const
  NULL_Value = '[null]';

type
  //If Asymmetric Deserialize and Destination is Different Return the Value to Assign to the Field
  TRecordDeserializeCallback = function (Field: TField; oldValue, newValue: String; var Cancel: Boolean):String;
  TArraynames = array of String;

function BuildFieldArray(ADataSet: TDataset; const MaskInclude: String; MaskExclude: String=''; Separator: Char = ';'; const CaseSensitive: Boolean = False): TArraynames;
procedure CopyDataset(ADestinationDataset, ASourceDataset: TDataset);

procedure RecordSerialize(var AList: TStringList; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String); overload;
function RecordSerialize(ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String=''): String; overload;
procedure RecordSerialize(var AStream: TStream; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String=''); overload;
procedure RecordSerialize(AFileName: String; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String=''); overload;

function RecordDeserialize(AList: TStringList; ADataSet: TDataset; Asymmetric: Boolean; ACallback: TRecordDeserializeCallback; Signature: String): Boolean; overload;
function RecordDeserialize(AStream: TStream; ADataSet: TDataset; Asymmetric: Boolean=False; ACallback: TRecordDeserializeCallback=nil; Signature: String=''): Boolean; overload;
function RecordDeserialize(AFileName: TFileName; ADataSet: TDataset; Asymmetric: Boolean=False; ACallback: TRecordDeserializeCallback=nil; Signature: String=''): Boolean; overload;

implementation

uses MM_StrUtils, masks;

function BuildFieldArray(ADataSet: TDataset; const MaskInclude: String; MaskExclude: String; Separator: Char; const CaseSensitive: Boolean): TArraynames;
var
   iField,
   lFields: Integer;
   curFieldName: String;

begin
  for iField :=0 to ADataset.Fields.Count-1 do
  begin
       curFieldName :=ADataset.Fields[iField].FieldName;
       if MatchesMaskList(curFieldName, MaskInclude, Separator, CaseSensitive) then
       begin
            if not(MatchesMaskList(curFieldName, MaskExclude, Separator, CaseSensitive)) then
            begin
                 lFields :=Length(Result);
                 SetLength(Result, lFields+1);
                 Result[lFields] :=curFieldName;
            end;
       end;
  end;
end;

procedure CopyDataset(ADestinationDataset, ASourceDataset: TDataset);
var
   iField: Integer;
   destField,
   sourceField: TField;

begin
     for iField :=0 to ADestinationDataset.FieldCount-1 do
     begin
       destField :=ADestinationDataset.Fields[iField];
       sourceField :=ASourceDataset.FieldByName(destField.FieldName);
       if (sourceField <> nil)
       then destField.Value :=sourceField.Value;
     end;
end;

procedure RecordSerialize(var AList: TStringList; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String);
var
   iField,
   iExclude: Integer;
   curField: TField;
   skipField: Boolean;

   procedure AddFieldNameValue(curFieldValue: String);
   begin
        AList.Add(EncodeControlChars(curField.FieldName));

        if curField.IsNull
        then AList.Add(NULL_Value)
        else if (curFieldValue = '')
             then AList.Add('')
             else if (curField.DataType in [ftDate,  ftTime, ftDateTime])
                  then AList.Add(FloatToStr(curField.AsDateTime))
                  else if (curField.DataType in [ftString, ftMemo, ftFmtMemo, ftWideString, ftWideMemo])
                       then AList.Add(AnsiQuotedStr(EncodeControlChars(curFieldValue), ''''))
                       else AList.Add(EncodeControlChars(curFieldValue));
   end;

begin
  if (Signature <> '')
  then AList.Add(EncodeControlChars(Signature));

  if AFieldsExclude then
  begin
     for iField :=0 to ADataset.Fields.Count-1 do
     begin
          curField :=ADataset.Fields[iField];

          skipField :=False;
          for iExclude :=0 to Length(AFields)-1 do
             if (curField.FieldName = AFields[iExclude])
             then begin
                    skipField :=True;
                    Break;
                  end;

          if not(skipField)
          then AddFieldNameValue(curField.AsString);
      end;
   end
  else begin
         if (Length(AFields) = 0)
         then begin
                   for iField :=0 to ADataset.Fields.Count-1 do
                   begin
                        curField :=ADataset.Fields[iField];
                        AddFieldNameValue(curField.AsString);
                    end;
                end
         else for iField :=0 to Length(AFields)-1 do
              begin
                   curField :=ADataset.FieldByName(AFields[iField]);
                   if (curField <> nil)
                   then AddFieldNameValue(curField.AsString);
               end;
        end;
end;

function RecordSerialize(ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String): String;
Var
   xList: TStringList;

begin
  try
     Result :='';
     xList :=TStringList.Create;

     RecordSerialize(xList, ADataSet, AFields, AFieldsExclude, Signature);

     Result :=xList.Text;
   finally
     xList.Free;
   end;
end;

procedure RecordSerialize(var AStream: TStream; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String='');
Var
   xList: TStringList;

begin
  try
     xList :=TStringList.Create;
     if (AStream = nil)
     then AStream :=TMemoryStream.Create;

     RecordSerialize(xList, ADataSet, AFields, AFieldsExclude, Signature);

     xList.SaveToStream(AStream);
   finally
     xList.Free;
   end;
end;

procedure RecordSerialize(AFileName: String; ADataSet: TDataset; AFields: array of String; AFieldsExclude: Boolean; Signature: String=''); overload;
Var
   xList: TStringList;

begin
  try
     xList :=TStringList.Create;

     RecordSerialize(xList, ADataSet, AFields, AFieldsExclude, Signature);

     xList.SaveToFile(AFileName);
   finally
     xList.Free;
   end;
end;

function RecordDeserialize(AList: TStringList; ADataSet: TDataset;
  Asymmetric: Boolean; ACallback: TRecordDeserializeCallback; Signature: String): Boolean;
Var
   curField: TField;
   iLine: Integer;
   curFieldName,
   newFieldValue,
   oldFieldValue: String;
   newFieldNULL: Boolean;

   procedure SetFieldValue;
   begin
        if newFieldNULL
        then curField.Clear
        else if (curField.DataType in [ftDate,  ftTime, ftDateTime])
             then curField.AsDateTime :=StrToFloat(newFieldValue)
             else curField.AsString :=newFieldValue;
   end;

begin
     Result :=False;
     iLine :=0;
     if (Signature <> '')
     then begin
               if (DecodeControlChars(AList.Strings[0]) <> Signature)
               then raise Exception.Create('Invalid Signature on RecordDeserialize'+#13#10+
                                           DecodeControlChars(AList.Strings[0])+' excepted '+Signature);
               inc(iLine);
           end;

     while (iLine < (AList.Count-1)) do
     begin
          curFieldName :=DecodeControlChars(AList.Strings[iLine]);
          curField :=ADataset.FieldByName(curFieldName);
          inc(iLine);
          if (iLine = AList.Count) //No Last Value, set NULL
          then newFieldNULL :=True
          else begin
                 newFieldValue :=AList.Strings[iLine];
                 newFieldNULL := (newFieldValue = NULL_Value);
                 if not(newFieldNULL)
                 then if (newFieldValue <> '')
                      then if (curField.DataType in [ftString, ftMemo, ftFmtMemo, ftWideString, ftWideMemo])
                           then newFieldValue :=AnsiDequotedStr(DecodeControlChars(newFieldValue), '''')
                           else if not(curField.DataType in [ftDate,  ftTime, ftDateTime])
                                then newFieldValue :=DecodeControlChars(newFieldValue);
                end;
          inc(iLine);

          if (curField <> nil) then
          begin
               if Asymmetric
               then begin
                         if (curField.IsNull)
                         then SetFieldValue
                         else begin
                                   oldFieldValue :=curField.AsString;

                                   //Set Uman Text for DateTime not a Number...
                                   if (curField.DataType in [ftDate,  ftTime, ftDateTime])
                                   then if (newFieldValue <> '')
                                        then newFieldValue :=DateTimeToStr(StrToFloat(newFieldValue));

                                   if (oldFieldValue <> newFieldValue) and
                                      (Assigned(ACallback))
                                   then begin
                                             newFieldValue :=ACallback(curField, oldFieldValue, newFieldValue, Result);

                                             if Result //Callback Cancel...
                                             then begin
                                                       Result :=False;
                                                       Exit;
                                                   end;

                                        curField.AsString :=newFieldValue;
                                    end;

                         end;
                     end
               else SetFieldValue;
          end;
     end;
     Result :=True;
end;

function RecordDeserialize(AStream: TStream; ADataSet: TDataset;
  Asymmetric: Boolean; ACallback: TRecordDeserializeCallback; Signature: String): Boolean;
Var
   xList: TStringList;

begin
  Result :=False;
  try
     xList :=TStringList.Create;

     xList.LoadFromStream(AStream);
     Result :=RecordDeserialize(xList, ADataSet, Asymmetric, ACallback, Signature);
  finally
    xList.Free;
  end;
end;

function RecordDeserialize(AFileName: TFileName; ADataSet: TDataset; Asymmetric: Boolean=False; ACallback: TRecordDeserializeCallback=nil; Signature: String=''): Boolean; overload;
Var
   xList: TStringList;

begin
  Result :=False;
  try
     xList :=TStringList.Create;

     xList.LoadFromFile(AFileName);
     Result :=RecordDeserialize(xList, ADataSet, Asymmetric, ACallback, Signature);
  finally
    xList.Free;
  end;
end;

end.

