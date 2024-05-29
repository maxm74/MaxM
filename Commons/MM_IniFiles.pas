//******************************************************************************
//***                     COMMON DELPHI FUNCTIONS                            ***
//***                                                                        ***
//***        (c) Massimo Magnano 2007-2013                                   ***
//***                                                                        ***
//***                                                                        ***
//******************************************************************************
//  File        : MM_IniFiles.pas
//
//  Description : Extensions to TINiFile Class.
//
//******************************************************************************

//MaxM 01-11-2013
//    Lazarus Porting (some adjustments)
//
//MaxM 07-03-2007
//    Added Class TMM_MemIniFile, Support for read\write INI Files as Resources
//
//MaxM 21-06-2005
//    Solved a Bug in TIniFile.ReadString, Max 2046 Chars.
//       Sufficient to Load strings but ReadBinaryStreams call ReadString and so
//       you can have only 2046/2 Bytes
//
//MaxM 04-07-2005
//    Consider The Same thinks for ReadSection and ReadSections


unit MM_IniFiles;
{$mode objfpc}
{$H+}

interface
uses Classes, inifiles;

type

    { TMM_IniFileFunction }

    TMM_IniFileFunction =class
    protected
       iniClass :TIniFile;

    public
       constructor Create(AiniClass :TIniFile);
       destructor Destroy; override;

       function ReadString_EncodedIdent(const Section, Ident, Default: string): string; virtual;
       procedure WriteString_EncodedIdent(const Section, Ident, Value: string); virtual;

       function ReadEncodedString(const Section, Ident, Default: string; EncodeIdent :Boolean=False): string; virtual;
       procedure WriteEncodedString(const Section, Ident, Value: String; EncodeIdent :Boolean=False); virtual;

       function ReadQuotedString(const Section, Ident, Default: string; QuoteIdent :Boolean=False; Quote: Char='"'): string; virtual;
       procedure WriteQuotedString(const Section, Ident, Value: String; QuoteIdent :Boolean=False; Quote: Char='"'); virtual;

       function ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
       procedure WriteEncodedInteger(const Section, Ident: string; Value: Longint); virtual;

       procedure DeleteEncodedIdent(const Section, Ident: String); virtual;
       procedure DeleteQuotedIdent(const Section, Ident: String; Quote: Char='"'); virtual;

       procedure ReadEncodedSection(const Section: string; Strings: TStrings); virtual;
       procedure RenameIdent(const Section, Ident, NewIdentName :String; EncodeIdent :Boolean); virtual;
       function RenameSection(const Section, NewSection :String; EraseExistingNewSection :Boolean):Boolean; virtual;
       procedure ReadSectionValues(const Section: string; Strings: TStrings);
    end;

    { TMM_IniFile }

    TMM_IniFile = class(TIniFile)
    protected
       iniFuncs :TMM_IniFileFunction;

    public
       constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False; CanCreateFile: Boolean = True); overload;
       constructor Create(AStream: TStream; AEscapeLineFeeds : Boolean = False);
       destructor Destroy; override;

       function ReadString_EncodedIdent(const Section, Ident, Default: string): string; virtual;
       procedure WriteString_EncodedIdent(const Section, Ident, Value: string); virtual;

       function ReadEncodedString(const Section, Ident, Default: string; EncodeIdent :Boolean=False): string; virtual;
       procedure WriteEncodedString(const Section, Ident, Value: String; EncodeIdent :Boolean=False); virtual;

       function ReadQuotedString(const Section, Ident, Default: string; QuoteIdent :Boolean=False; Quote: Char='"'): string; virtual;
       procedure WriteQuotedString(const Section, Ident, Value: String; QuoteIdent :Boolean=False; Quote: Char='"'); virtual;

       function ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
       procedure WriteEncodedInteger(const Section, Ident: string; Value: Longint); virtual;

       procedure DeleteEncodedIdent(const Section, Ident: String); virtual;
       procedure DeleteQuotedIdent(const Section, Ident: String; Quote: Char='"'); virtual;

       procedure ReadEncodedSection(const Section: string; Strings: TStrings); virtual;
       procedure RenameIdent(const Section, Ident, NewIdentName :String; EncodeIdent :Boolean); virtual;
       function RenameSection(const Section, NewSection :String; EraseExistingNewSection :Boolean):Boolean; virtual;
       procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    end;

    { TMM_MemIniFile }

    TMM_MemIniFile = class(TMemIniFile)
    protected
        isRES       :Boolean;
        ModuleFilename,
        ResName,
        ResType     :String;
        iniFuncs    :TMM_IniFileFunction;
    public
       constructor Create(const AFileName: string; AEscapeLineFeeds : Boolean = False); override;
       constructor CreateRES(AResName, AResType :String; AInstance :TFPResourceHMODULE=0);
       destructor Destroy; override;

       function ReadString_EncodedIdent(const Section, Ident, Default: string): string; virtual;
       procedure WriteString_EncodedIdent(const Section, Ident, Value: string); virtual;

       function ReadEncodedString(const Section, Ident, Default: string; EncodeIdent :Boolean=False): string; virtual;
       procedure WriteEncodedString(const Section, Ident, Value: String; EncodeIdent :Boolean=False); virtual;

       function ReadQuotedString(const Section, Ident, Default: string; QuoteIdent :Boolean=False; Quote: Char='"'): string; virtual;
       procedure WriteQuotedString(const Section, Ident, Value: String; QuoteIdent :Boolean=False; Quote: Char='"'); virtual;

       function ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint; virtual;
       procedure WriteEncodedInteger(const Section, Ident: string; Value: Longint); virtual;

       procedure DeleteEncodedIdent(const Section, Ident: String); virtual;
       procedure DeleteQuotedIdent(const Section, Ident: String; Quote: Char='"'); virtual;

       procedure ReadEncodedSection(const Section: string; Strings: TStrings); virtual;
       procedure RenameIdent(const Section, Ident, NewIdentName :String; EncodeIdent :Boolean); virtual;
       function RenameSection(const Section, NewSection :String; EraseExistingNewSection :Boolean):Boolean; virtual;
       procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    end;


implementation

uses SysUtils, MM_StrUtils;

{ TMM_IniFileFunction }

constructor TMM_IniFileFunction.Create(AiniClass: TIniFile);
begin
     iniClass :=AiniClass;
     if (iniClass = Nil)
     then raise Exception.Create('iniClass (TIniFile) cannot be Nil');

     inherited Create;
end;

destructor TMM_IniFileFunction.Destroy;
begin
     inherited Destroy;
end;

function TMM_IniFileFunction.ReadString_EncodedIdent(const Section, Ident, Default: string): string;
begin
     Result :=iniClass.ReadString(Section, EncodeControlChars(Ident), Default);
end;


procedure TMM_IniFileFunction.WriteString_EncodedIdent(const Section, Ident, Value: string);
begin
     if (Ident='')
     then Exit;  //Avoid ntdll.dll exception

     iniClass.WriteString(Section, EncodeControlChars(Ident), Value);
end;


function TMM_IniFileFunction.ReadEncodedString(const Section, Ident, Default: string; EncodeIdent :Boolean=False): string;
begin
     if EncodeIdent
     then Result :=DecodeControlChars(iniClass.ReadString(Section, EncodeControlChars(Ident),
                                      Default))
     else Result :=DecodeControlChars(iniClass.ReadString(Section, Ident, Default));
end;

procedure TMM_IniFileFunction.WriteEncodedString(const Section, Ident, Value: String; EncodeIdent :Boolean=False);
begin
     if (Ident='')
     then Exit;  //Avoid ntdll.dll exception

     if EncodeIdent
     then iniClass.WriteString(Section, EncodeControlChars(Ident), EncodeControlChars(Value))
     else iniClass.WriteString(Section, Ident, EncodeControlChars(Value));
end;

function TMM_IniFileFunction.ReadQuotedString(const Section, Ident, Default: string; QuoteIdent :Boolean=False; Quote: Char='"'): string;
Var
   rString :String;

begin
     if QuoteIdent
     then rString :=iniClass.ReadString(Section, AnsiQuotedStr(Ident, Quote), Default)
     else rString :=iniClass.ReadString(Section, Ident, Default);

     if (rString <> '')
     then begin
            Result :=AnsiDequotedStr(rString, Quote);
            if (Result = Quote+Quote)  //AnsiDequotedStr return the Source String if '' ??? Why???
            then Result :='';
     end
     else Result :='';
end;

procedure TMM_IniFileFunction.WriteQuotedString(const Section, Ident, Value: String; QuoteIdent :Boolean=False; Quote: Char='"');
begin
     if (Ident='')
     then Exit;  //Avoid ntdll.dll exception

     if QuoteIdent
     then iniClass.WriteString(Section, AnsiQuotedStr(Ident, Quote), AnsiQuotedStr(Value, Quote))
     else iniClass.WriteString(Section, Ident, AnsiQuotedStr(Value, Quote));
end;

function TMM_IniFileFunction.ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint;
begin
     Result :=iniClass.ReadInteger(Section, EncodeControlChars(Ident), Default);
end;

procedure TMM_IniFileFunction.WriteEncodedInteger(const Section, Ident: string; Value: Longint);
begin
     if (Ident='')
     then Exit;  //Avoid ntdll.dll exception

     iniClass.WriteInteger(Section, EncodeControlChars(Ident), Value);
end;

procedure TMM_IniFileFunction.DeleteEncodedIdent(const Section, Ident: String);
begin
     iniClass.DeleteKey(Section, EncodeControlChars(Ident));
end;

procedure TMM_IniFileFunction.DeleteQuotedIdent(const Section, Ident: String; Quote: Char='"');
begin
     iniClass.DeleteKey(Section, AnsiQuotedStr(Ident, Quote));
end;

procedure TMM_IniFileFunction.ReadEncodedSection(const Section: string; Strings: TStrings);
Var
   i :Integer;

begin
     if (Strings<>Nil)
     then begin
               iniClass.ReadSection(Section, Strings);
               for i:=0 to Strings.Count-1 do
               begin
                    Strings[i] :=DecodeControlChars(Strings[i]);
               end;
          end;
end;


procedure TMM_IniFileFunction.RenameIdent(const Section, Ident, NewIdentName :String; EncodeIdent :Boolean);
Var
   curValue,
   OldIdent  :String;

begin
     if (Ident<>'') and (NewIdentName<>'') and
        (Ident<>NewIdentName) then
     begin
          if EncodeIdent
          then OldIdent :=EncodeControlChars(Ident)
          else OldIdent :=Ident;

          curValue :=iniClass.ReadString(Section, OldIdent, '');
          if (curValue='')
          then raise Exception.CreateFmt('Error Loading Section %s Ident %s', [Section, Ident]);
          iniClass.DeleteKey(Section, OldIdent);
          if EncodeIdent
          then WriteString_EncodedIdent(Section, NewIdentName, curValue)
          else iniClass.WriteString(Section, NewIdentName, curValue);
     end;
end;

function TMM_IniFileFunction.RenameSection(const Section, NewSection :String; EraseExistingNewSection :Boolean):Boolean;
Var
   SectionIdents :TStringList;
   i             :Integer;

begin
     Result :=False;
     SectionIdents :=Nil;
     try
        if not(iniClass.SectionExists(Section))
        then raise Exception.CreateFmt('Error : Section %s does not Exist', [Section]);

        if EraseExistingNewSection
        then iniClass.EraseSection(NewSection)
        else if iniClass.SectionExists(NewSection)
             then raise Exception.CreateFmt('Error : Section %s already Exist', [NewSection]);

        SectionIdents :=TStringList.Create;
        iniClass.ReadSection(Section, SectionIdents);

        for i:=0 to SectionIdents.Count-1 do
        begin
             iniClass.WriteString(NewSection, SectionIdents[i],
                    iniClass.ReadString(Section, SectionIdents[i], ''));
        end;
        iniClass.EraseSection(Section);
     finally
        if (SectionIdents<>Nil)
        then SectionIdents.Free;
     end;
end;

procedure TMM_IniFileFunction.ReadSectionValues(const Section: string;
  Strings: TStrings);
var
  KeyList: TStringList;
  I: Integer;
begin
  KeyList := TStringList.Create;
  try
    iniClass.ReadSection(Section, KeyList);
    Strings.BeginUpdate;
    try
      Strings.Clear;
      for I := 0 to KeyList.Count - 1 do
        Strings.Add(KeyList[I] + '=' + iniClass.ReadString(Section, KeyList[I], ''))
    finally
      Strings.EndUpdate;
    end;
  finally
    KeyList.Free;
  end;
end;

{TMM_IniFile}

constructor TMM_IniFile.Create(const AFileName: string; AEscapeLineFeeds: Boolean = False; CanCreateFile: Boolean = True);
begin
  iniFuncs :=TMM_IniFileFunction.Create(Self);
  if CanCreateFile and not(FileExists(AFileName))
  then SysUtils.ForceDirectories(ExtractFilePath(AFileName));

  inherited Create(AFileName, AEscapeLineFeeds);
end;

constructor TMM_IniFile.Create(AStream: TStream; AEscapeLineFeeds: Boolean = False);
begin
  iniFuncs :=TMM_IniFileFunction.Create(Self);
  inherited Create(AStream, AEscapeLineFeeds);
end;

destructor TMM_IniFile.Destroy;
begin
  iniFuncs.Free;
  inherited Destroy;
end;

function TMM_IniFile.ReadString_EncodedIdent(const Section, Ident, Default: string): string;
begin
     Result := iniFuncs.ReadString_EncodedIdent(Section, Ident, Default);
end;

procedure TMM_IniFile.WriteString_EncodedIdent(const Section, Ident, Value: string);
begin
     iniFuncs.WriteString_EncodedIdent(Section, Ident, Value);
end;

function TMM_IniFile.ReadEncodedString(const Section, Ident, Default: string; EncodeIdent: Boolean): string;
begin
     Result := iniFuncs.ReadEncodedString(Section, Ident, Default, EncodeIdent);
end;

procedure TMM_IniFile.WriteEncodedString(const Section, Ident, Value: String; EncodeIdent: Boolean);
begin
     iniFuncs.WriteEncodedString(Section, Ident, Value, EncodeIdent);
end;

function TMM_IniFile.ReadQuotedString(const Section, Ident, Default: string; QuoteIdent: Boolean; Quote: Char): string;
begin
     Result := iniFuncs.ReadQuotedString(Section, Ident, Default, QuoteIdent, Quote);
end;

procedure TMM_IniFile.WriteQuotedString(const Section, Ident, Value: String; QuoteIdent: Boolean; Quote: Char);
begin
     iniFuncs.WriteQuotedString(Section, Ident, Value, QuoteIdent, Quote);
end;

function TMM_IniFile.ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint;
begin
     Result := iniFuncs.ReadEncodedInteger(Section, Ident, Default);
end;

procedure TMM_IniFile.WriteEncodedInteger(const Section, Ident: string; Value: Longint);
begin
     iniFuncs.WriteEncodedInteger(Section, Ident, Value);
end;

procedure TMM_IniFile.DeleteEncodedIdent(const Section, Ident: String);
begin
     iniFuncs.DeleteEncodedIdent(Section, Ident);
end;

procedure TMM_IniFile.DeleteQuotedIdent(const Section, Ident: String; Quote: Char);
begin
     iniFuncs.DeleteQuotedIdent(Section, Ident, Quote);
end;

procedure TMM_IniFile.ReadEncodedSection(const Section: string; Strings: TStrings);
begin
     iniFuncs.ReadEncodedSection(Section, Strings);
end;

procedure TMM_IniFile.RenameIdent(const Section, Ident, NewIdentName: String; EncodeIdent: Boolean);
begin
     iniFuncs.RenameIdent(Section, Ident, NewIdentName, EncodeIdent);
end;

function TMM_IniFile.RenameSection(const Section, NewSection: String; EraseExistingNewSection: Boolean): Boolean;
begin
     Result := iniFuncs.RenameSection(Section, NewSection, EraseExistingNewSection);
end;

procedure TMM_IniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin
     iniFuncs.ReadSectionValues(Section, Strings);
end;

{ TMM_MemIniFile }

constructor TMM_MemIniFile.Create(const AFileName: string; AEscapeLineFeeds: Boolean);
begin
  iniFuncs :=TMM_IniFileFunction.Create(Self);
  inherited Create(AFileName, AEscapeLineFeeds);
end;

constructor TMM_MemIniFile.CreateRES(AResName, AResType: String; AInstance :TFPResourceHMODULE);
var
   List       :TStringList;
   resStream  :TResourceStream;

begin
     iniFuncs :=TMM_IniFileFunction.Create(Self);
     inherited Create('');
     try
        try
           ResName :=AResName;
           ResType :=AResType;

           List :=TStringList.Create;
           if (AInstance = 0)
           then AInstance :=HINSTANCE;
           resStream  :=TResourceStream.Create(AInstance, AResName, PChar(AResType));
           List.LoadFromStream(resStream);
           SetStrings(List);
        finally
           List.Free;
        end;
     except
        Clear;
     end;
end;

destructor TMM_MemIniFile.Destroy;
begin
  iniFuncs.Free;
  inherited Destroy;
end;

function TMM_MemIniFile.ReadString_EncodedIdent(const Section, Ident, Default: string): string;
begin
     Result := iniFuncs.ReadString_EncodedIdent(Section, Ident, Default);
end;

procedure TMM_MemIniFile.WriteString_EncodedIdent(const Section, Ident, Value: string);
begin
     iniFuncs.WriteString_EncodedIdent(Section, Ident, Value);
end;

function TMM_MemIniFile.ReadEncodedString(const Section, Ident, Default: string; EncodeIdent: Boolean): string;
begin
     Result := iniFuncs.ReadEncodedString(Section, Ident, Default, EncodeIdent);
end;

procedure TMM_MemIniFile.WriteEncodedString(const Section, Ident, Value: String; EncodeIdent: Boolean);
begin
     iniFuncs.WriteEncodedString(Section, Ident, Value, EncodeIdent);
end;

function TMM_MemIniFile.ReadQuotedString(const Section, Ident, Default: string; QuoteIdent: Boolean; Quote: Char): string;
begin
     Result := iniFuncs.ReadQuotedString(Section, Ident, Default, QuoteIdent, Quote);
end;

procedure TMM_MemIniFile.WriteQuotedString(const Section, Ident, Value: String; QuoteIdent: Boolean; Quote: Char);
begin
     iniFuncs.WriteQuotedString(Section, Ident, Value, QuoteIdent, Quote);
end;

function TMM_MemIniFile.ReadEncodedInteger(const Section, Ident: string; Default: Longint): Longint;
begin
     Result :=iniFuncs.ReadEncodedInteger(Section, Ident, Default);
end;

procedure TMM_MemIniFile.WriteEncodedInteger(const Section, Ident: string; Value: Longint);
begin
     iniFuncs.WriteEncodedInteger(Section, Ident, Value);
end;

procedure TMM_MemIniFile.DeleteEncodedIdent(const Section, Ident: String);
begin
     iniFuncs.DeleteEncodedIdent(Section, Ident);
end;

procedure TMM_MemIniFile.DeleteQuotedIdent(const Section, Ident: String; Quote: Char);
begin
     iniFuncs.DeleteQuotedIdent(Section, Ident, Quote);
end;

procedure TMM_MemIniFile.ReadEncodedSection(const Section: string; Strings: TStrings);
begin
     iniFuncs.ReadEncodedSection(Section, Strings);
end;

procedure TMM_MemIniFile.RenameIdent(const Section, Ident, NewIdentName: String; EncodeIdent: Boolean);
begin
     iniFuncs.RenameIdent(Section, Ident, NewIdentName, EncodeIdent);
end;

function TMM_MemIniFile.RenameSection(const Section, NewSection: String; EraseExistingNewSection: Boolean): Boolean;
begin
     Result := iniFuncs.RenameSection(Section, NewSection, EraseExistingNewSection);
end;

procedure TMM_MemIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin
     iniFuncs.ReadSectionValues(Section, Strings);
end;

end.
