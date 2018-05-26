
{

                DWSII - Common Source for graphical IDEUtils

 Hint: You can enable Delphi Syntax highlightning in .inc files...

}




{ Default colors to use for SynEdit style code complete }
var
  clrConst     : TColor = clGreen;     // constant
  clrEnum      : TColor = clGreen;     // enumeration
  clrFunc      : TColor = clBlue;      // function
  clrProc      : TColor = clTeal;      // procedure
  clrConstruct : TColor = clGreen;     // constructor (reused color)
  clrDestruct  : TColor = clRed;       // destructor
  clrProp      : TColor = clNavy;      // property
  clrType      : TColor = clOlive;     // type
  clrAlias     : TColor = clOlive;     // alias (ie. TColor = Integer)
  clrUnit      : TColor = clBlack;     // unit
  clrVar       : TColor = clMaroon;    // variable

{ Default images to use for code hints.
  Assumes following order of images in list:
    0-Unit,     1-Type,      2-Class,    3-ClassField/Variable/Const,
    4-Function, 5-Procedure, 6-Property }
var
  imgFolder    : Integer = 0;          // Folder graphic
  imgType      : Integer = 1;          // type
  imgAlias     : Integer = 1;          // alias (ie. TColor = Integer)
  imgClass     : Integer = 2;          // class
  imgVar       : Integer = 3;          // variable
  imgConst     : Integer = 3;          // Constant
  imgEnum      : Integer = 3;          // Enumeration
  imgFunc      : Integer = 4;          // function
  imgProc      : Integer = 5;          // procedure
  imgConstruct : Integer = 5;          // constructor (reused color)
  imgDestruct  : Integer = 5;          // destructor
  imgProp      : Integer = 6;          // property
  imgUnit      : Integer = 7;          // unit


function GetTypeWithColor(AText: string; Color: TColor; UseStyle: Boolean=True): string;
function  SymbolToText(StartingTable: TSymbolTable; LookupSym: TSymbol;
             SynOptions: TdSyn_DisplayOptions; ContentOpts: TContentOptions;
             ForwardedImpl: TStrings): string;
procedure LoadClassSymbolToStrings(ItemList, InsertList: TStrings;
             ClassSym: TClassSymbol; IncludeParents, IncludePropertyAccessors,
             IncludeClassSelf: Boolean; ClearLists: Boolean;
             SynOptions: TdSyn_DisplayOptions);

procedure LoadSymbolsToStrings(ItemList, InsertList: TStrings;
             StartingTable: TSymbolTable; LoadUnitSyms, ClearLists,
             IncludePropertyAccessors, IncludeClassSelf: Boolean;
             SynOptions: TdSyn_DisplayOptions; AProg: TProgram=nil;
             ACol: Integer=-1; ALine: Integer=-1);
procedure LoadUnitDeclaredSymbols(Prg: TProgram; UnitItemsList, UnitInserts: TStrings;
                                  SynOptions: TdSyn_DisplayOptions;
                                  ContentOptions: TContentOptions=[];
                                  ClearLists: Boolean=True);


implementation

uses dws2Compiler;


{-----------------------------------------------------------------------------
  Procedure: SymbolToText
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: StartingTable: TSymbolTable; LookupSym: TSymbol; SynOptions: TdSyn_DisplayOptions; ContentOpts: TContentOptions; FuncImpl: TStrings
  Result:    string
  Purpose:   Return how a symbol should be formated for display. If provided
             a StartingTable and the IncludeUnit content option is true, it
             will display the unit that the symbol is defined in. (Used for hints)
             ForwardedImpl will have function/method implementation stubs included.
-----------------------------------------------------------------------------}
function SymbolToText(StartingTable: TSymbolTable; LookupSym: TSymbol;
   SynOptions: TdSyn_DisplayOptions; ContentOpts: TContentOptions; ForwardedImpl: TStrings): string;
const
  FmtValue = '%s = %s';   // how to return text when displaying debug value
var
  i, p : Integer;
  FuncType : string;
  FuncColor: TColor;
  FuncReturnType : string;
  ParamText,
  ThisParam: string;

  FuncSym : TFuncSymbol;
  SymContext : string;   // show to what the symbol belongs (unit, class, record, etc)
  UseImgs : Boolean;     // if images should be included
  FuncImg : Integer;     // index to use for procedure/function
  UseStyle : Boolean;    // If SynEdit syles should be used
  ClassAncestor: string;

  MultiLineText: TStringList;
begin
  if LookupSym = nil then Exit;
  SymContext := '';
  if coMultiLine in ContentOpts then
    MultiLineText := TStringList.Create;
  try
    UseStyle := (doSynStyle in SynOptions);
    UseImgs := (doIncludeImage in SynOptions) and UseStyle; // if using style and images are included

    { Add various Variable types - they are Unit declared, script declared, members of records and objects }
    if (LookupSym is TDataSymbol) or                // script declared
       (LookupSym is TExternalVarSymbol) or         // unit declared
       (LookupSym is TMemberSymbol) or              // member of a record
       (LookupSym is TFieldSymbol) then             // field of script object
    begin
      if coIncludeContext in ContentOpts then begin
        { DONE : Added support for TMemberSymbols to look up their owning records. }
        if LookupSym is TMemberSymbol then
          SymContext := TMemberSymbol(LookupSym).RecordSymbol.Name;
        if LookupSym is TFieldSymbol then
          SymContext := TFieldSymbol(LookupSym).ClassSymbol.Name;
        if SymContext <> '' then
          SymContext := SymContext + '.';
      end;
      Result := Format('%s %s: %s;', [GetImageIndex(imgVar, UseImgs) + GetTypeWithColor('var', clrVar, UseStyle),
                                       SymContext+GetNameWithStyle(LookupSym.Name, UseStyle),
                                       LookupSym.Typ.Name]);
    end
    { Add alias types (type x = TMyType;) }
    else if (LookupSym is TAliasSymbol) then begin
      { TODO -oMark : Finish adding full support for the TAliasSymbol }
      with (LookupSym as TAliasSymbol) do begin
        Result := Format('%s %s = %s;', [GetImageIndex(imgType, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                         GetNameWithStyle(Name, UseStyle), Typ.Name]);
      end;
    end
    { Add dynamic array types }
    else if (LookupSym is TDynamicArraySymbol) then begin
      with (LookupSym as TDynamicArraySymbol) do begin
        Result := Format('%s %s = array of %s;', [GetImageIndex(imgType, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                                  GetNameWithStyle(Name, UseStyle), Typ.Name]);
      end;
    end
    { Add array types }
    else if (LookupSym is TStaticArraySymbol) then begin
      with (LookupSym as TStaticArraySymbol) do begin
        Result := Format('%s %s = array [%d..%d] of %s;', [GetImageIndex(imgType, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                                           GetNameWithStyle(Name, UseStyle), LowBound, HighBound, Typ.Name]);
      end;
    end
    { Add class properties }
    else if (LookupSym is TPropertySymbol) then begin // property of an object
      if coIncludeContext in ContentOpts then begin
        with LookupSym as TPropertySymbol do
          SymContext := ClassSymbol.Name + '.';     // class it belongs to
      end;
      // load array params for property
      ParamText := '';
      for p := 0 to TPropertySymbol(LookupSym).ArrayIndices.Count - 1 do begin
        ThisParam := '';
        if p > 0 then
          ParamText := ParamText + '; ';       // if has previous ones, add a ';'
        // write out param text
        ParamText := ParamText + TPropertySymbol(LookupSym).ArrayIndices[p].Description;
      end;
      // Add array brackets if there are params
      if ParamText <> '' then
        ParamText := '['+ParamText+']';

      Result := Format('%s %s%s : %s;', [GetImageIndex(imgProp, UseImgs) + GetTypeWithColor('property', clrProp, UseStyle),
                                         SymContext+GetNameWithStyle(LookupSym.Name, UseStyle),
                                         ParamText, LookupSym.Typ.Name]);
    end
    { Add functions and methods (TMethodSymbol descend from TFuncSymbol) }
    else if (LookupSym is TFuncSymbol) then begin
      FuncSym := (LookupSym as TFuncSymbol);

      FuncReturnType := '';
      { Get string description of function type }
      case FuncSym.Kind of
      fkFunction    :
        begin
          FuncColor := clrFunc;
          FuncType  := 'function';       // default
          FuncImg   := imgFunc;
          if FuncSym is TMethodSymbol then
            if TMethodSymbol(FuncSym).IsClassMethod then
            begin
              if doShortText in SynOptions then
                FuncType := 'class func' // override name
              else
                FuncType := 'class ' + FuncType; // add 'class' to the front
            end;
          if Assigned(FuncSym.Result) then
            FuncReturnType := ': '+FuncSym.Typ.Name;
        end;
      fkProcedure   :
        begin
          FuncColor := clrProc;
          FuncType := 'procedure';       // default
          FuncImg   := imgProc;
          if FuncSym is TMethodSymbol then
            if TMethodSymbol(FuncSym).IsClassMethod then
            begin
              if doShortText in SynOptions then
                FuncType := 'class proc' // override name
              else
                FuncType := 'class ' + FuncType; // add 'class' to the front
            end;
        end;
      fkConstructor :
        begin
          FuncColor := clrConstruct;
          FuncType := 'constructor';
          FuncImg   := imgProc;
        end;
      fkDestructor  :
        begin
          FuncColor := clrDestruct;
          FuncType := 'destructor';
          FuncImg   := imgProc;
        end;
      else
        FuncType  := '<unknown>';
        FuncColor := clRed;    // don't know what it is... show RED
        FuncImg   := imgProc;
      end;

      // load params for function
      ParamText := '';
      for p := 0 to FuncSym.Params.Count - 1 do begin
        ThisParam := '';
        if p > 0 then
          ParamText := ParamText + '; ';       // if has previous ones, add a ';'
        // write out param text
        ThisParam := ThisParam + FuncSym.Params[p].Description;
        if Length(TParamSymbol(FuncSym.Params[p]).DefaultValue) > 0 then
          ThisParam := '[' + ThisParam + ']';
        ParamText := ParamText + ThisParam;
      end;
      // Add parenthesis if there are params
      if ParamText <> '' then
        ParamText := '('+ParamText+')';

      if coIncludeContext in ContentOpts then begin
        if LookupSym is TMethodSymbol then
          SymContext := TMethodSymbol(LookupSym).ClassSymbol.Name + '.';
      end;
      Result := Format('%s %s%s%s;', [GetImageIndex(FuncImg, UseImgs) + GetTypeWithColor(FuncType, FuncColor, UseStyle),
                                      SymContext+GetNameWithStyle(FuncSym.Name, UseStyle), ParamText, FuncReturnType]);
      if coFunctionAsForward in ContentOpts then
        Result := Result + ' forward;';

      if Assigned(ForwardedImpl) then
      begin
        if coSimpleFunctionImpl in ContentOpts then
          ForwardedImpl.Add(Format('%s %s;', [FuncType, FuncSym.Name]))  // create with simple format
        else
          ForwardedImpl.Add(Format('%s %s%s%s;', [FuncType, FuncSym.Name, ParamText, FuncReturnType]));
        ForwardedImpl.Add('begin');
        ForwardedImpl.Add('  ');
        ForwardedImpl.Add('end;');
        ForwardedImpl.Add('');
      end;
    end
    { Add Enumerations }
    else if (LookupSym is TEnumerationSymbol) then begin
      Result := Format('%s %s = %s;', [GetImageIndex(imgEnum, UseImgs) + GetTypeWithColor('enum', clrEnum, UseStyle),
                                       GetNameWithStyle(LookupSym.Name, UseStyle), LookupSym.Description]);
    end
    { Add Elements (of Enumerations) - is a TConstSymbol, catch prior to constants}
    else if LookupSym is TElementSymbol then begin
      if TElementSymbol(LookupSym).IsUserDef then
        Result := Format('%s %s = %d;', [GetImageIndex(imgEnum, UseImgs) + GetTypeWithColor('const', clrEnum, UseStyle),
                                         GetNameWithStyle(LookupSym.Name, UseStyle), TElementSymbol(LookupSym).UserDefValue])
      else  // not user defined, don't include value
        Result := Format('%s %s;', [GetImageIndex(imgEnum, UseImgs) + GetTypeWithColor('const', clrEnum, UseStyle),
                                    GetNameWithStyle(LookupSym.Name, UseStyle)]);
    end
    { Add constants (but not elements of enumerations) }
    else if (LookupSym is TConstSymbol) then begin
      Assert(Length(TConstSymbol(LookupSym).Data) > 0);
      Result := Format('%s %s: %s = %s;', [GetImageIndex(imgConst, UseImgs) + GetTypeWithColor('const', clrConst, UseStyle),
                                           GetNameWithStyle(LookupSym.Name, UseStyle), LookupSym.Typ.Name,
                                           VariantToStr(TConstSymbol(LookupSym).Data[0])]);
    end
    { Add classes - is also a TTypeSymbol (do first) }
    else if (LookupSym is TClassSymbol) then begin
      with TClassSymbol(LookupSym) do begin
        { Add display text for class }
        ClassAncestor := '';
        if Parent <> nil then
          ClassAncestor := '(' + Parent.Name + ')';
        Result := Format('%s %s = class%s;', [GetImageIndex(imgClass, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                              GetNameWithStyle(Name, UseStyle), ClassAncestor]);
        if (coMultiLine in ContentOpts) and Assigned(MultiLineText) then begin
          MultiLineText.Add(Result);
          for i := 0 to Members.Count - 1 do
            MultiLineText.Add(SymbolToText(StartingTable, Members[i], SynOptions, ContentOpts, ForwardedImpl));
        end;
      end;
    end
    { Add "class of" types }
    else if (LookupSym is TClassOfSymbol) then begin
      Result := Format('%s %s = class of %s;', [GetImageIndex(imgType, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                                GetNameWithStyle(LookupSym.Name, UseStyle), LookupSym.Typ.Name]);
    end
    { Add records types }
    else if (LookupSym is TRecordSymbol) then begin
      Result := Format('%s %s = record;', [GetImageIndex(imgType, UseImgs) + GetTypeWithColor('type', clrType, UseStyle),
                                           GetNameWithStyle(LookupSym.Name, UseStyle)]);
    end
    { Add unit types }
    else if (LookupSym is TUnitSymbol) then begin
      Result := Format('%s %s;', [GetImageIndex(imgUnit, UseImgs) + GetTypeWithColor('unit', clrUnit, UseStyle),
                                  GetNameWithStyle(LookupSym.Name, UseStyle)]);
    end;

    { Add unit location if desired }
    if coIncludeUnit in ContentOpts then begin
      if StartingTable <> nil then
        for i := StartingTable.Count - 1 downto 0 do begin
          if StartingTable[i] is TUnitSymbol then begin
            if TUnitSymbol(StartingTable[i]).Table.FindLocal(LookupSym.Name) = nil then
  //            Result := Result + ' - <script>'     // symbol is declared locally
            else
              Result := Result + ' - ' + StartingTable[i].Name; // symbol is declared in a unit
          end;
        end; {for}
    end;

    // pass in dictionary, do a findDeclaration and add the script line # if we can
  finally
    FreeAndNil(MultiLineText);
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: GetTypeWithColor
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: AText: string; Color: TColor
  Result:    string
  Purpose:   Return the text colored with the desired color.
-----------------------------------------------------------------------------}
function GetTypeWithColor(AText: string; Color: TColor; UseStyle: Boolean): string;
begin
  if UseStyle then
    Result := Format('\color{%s}%s\color{0}', [ColorToString(Color), AText])
  else
    Result := AText;
end;


{-----------------------------------------------------------------------------
  Procedure: LoadSymbolsToStrings
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: ItemList, InsertList: TStrings; StartingTable: TSymbolTable; LoadUnitSyms, ClearLists, IncludeImages: Boolean
  Result:    None
  Purpose:   Load the member symbols of the class and optionally the members of
             the parent classes. This means ancestor methods and properties.
             IncludePropertyAccessors - If a properties read and write accessors should be
             included in the list. If false, they are supressed and only the
             property is listed.
-----------------------------------------------------------------------------}
procedure LoadClassSymbolToStrings(ItemList, InsertList: TStrings;
             ClassSym: TClassSymbol; IncludeParents, IncludePropertyAccessors, IncludeClassSelf: Boolean;
             ClearLists: Boolean; SynOptions: TdSyn_DisplayOptions);
var
  tmpClass: TClassSymbol;
begin
  tmpClass := ClassSym;
  repeat
    // load this class' members to the lists
    LoadSymbolsToStrings(ItemList, InsertList, tmpClass.Members, False,
                         ClearLists, IncludePropertyAccessors, IncludeClassSelf, SynOptions);

    if not IncludeParents then
      Break;

    { TODO : How to prevent 'Self' from being included? remove from finished list? prevent from entering list? }

    // make tmpClass point to any parents
    tmpClass := tmpClass.Parent;   // cycle again until there are no parents
  until tmpClass = nil;  // stop once nothing more to examine
end;



{-----------------------------------------------------------------------------
  Procedure: LoadSymbolsToStrings
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: ItemList, InsertList: TStrings; StartingTable: TSymbolTable;
             LoadUnitSyms, ClearLists, IncludePropertyAccessors: Boolean;
             SynOptions: TdSyn_DisplayOptions; AProg: TProgram; ACol, ALine: Integer
  Result:    None
  Purpose:   Return the symbols from the StartingTable and populate the ItemList and InsertList.
             IncludePropertyAccessors - If a properties read and write accessors should be
             included in the list. If false, they are supressed and only the
             property is listed.
             AProg and ACol & ALine are used to limit entries by scope. Don't
             list a symbol that is declared after the cursor's current position.
             If any of them are not provided then they are ignored and all symbols
             are listed.
-----------------------------------------------------------------------------}
procedure LoadSymbolsToStrings(ItemList, InsertList: TStrings;
  StartingTable: TSymbolTable; LoadUnitSyms, ClearLists,
  IncludePropertyAccessors, IncludeClassSelf: Boolean; SynOptions: TdSyn_DisplayOptions;
  AProg: TProgram; ACol, ALine: Integer);
var
  i: Integer;
  AddSymbol: Boolean;
  SymText: string;      // string representation of symbol
  PropSym: TPropertySymbol;
begin
  if StartingTable = nil then Exit;

  { Clear all existing entries }
  if ClearLists then begin
    ItemList.Clear;
    InsertList.Clear;
  end;

  { Add symbols declared StartingTable - optionally include unit declarations }
  for i := 0 to StartingTable.Count - 1 do begin
    AddSymbol := True;
    // if is a unit and they are being excluded, don't add the symbol
    if StartingTable.Symbols[i] is TUnitSymbol and (not LoadUnitSyms) then
      AddSymbol := False;

    // if still adding and suppressing Get/Set methods, check symbol type
    if AddSymbol and (not IncludePropertyAccessors) then
    begin
      // Check to see if used in property read/write access
      PropSym := GetPropertyForSymbol(StartingTable.Symbols[i]);
      AddSymbol := not Assigned(PropSym);  // only add it if no a property accessor method
    end;

    // if adding and suppressing the Classe's "Self" symbol
    if AddSymbol and (not IncludeClassSelf) then
    begin
      with StartingTable.Symbols[i] do
        if (SameText(Name, 'self') and Assigned(Typ) and (Typ is TClassSymbol)) then
          AddSymbol := False;
    end;

    // if set to add and has extra info provided to limit script symbols to cursor
    // position, then only include symbol if declaration is before position
    if AddSymbol and Assigned(AProg) and (ACol > -1) and (ALine > -1) then
      AddSymbol := SymbolDeclBeforePos(ACol, ALine, AProg, StartingTable.Symbols[i]);

    // if still adding, add it.
    if AddSymbol then      // if symbol is desired
    begin
      SymText := SymbolToText(StartingTable, StartingTable.Symbols[i], SynOptions, [], nil); // get symbol in text form
      // if NOT blank and NOT already in list (class with overriden method with show from current class and ancestor class, duplicates)
      if (SymText <> '') and (ItemList.IndexOf(SymText) < 0) then begin
        InsertList.Add(StartingTable.Symbols[i].Name);
        ItemList.Add(SymText);
      end;
    end;
  end;
end;



{-----------------------------------------------------------------------------
  Procedure: LoadUnitDeclaredSymbols
  Author:    Mark Ericksen
  Date:      18-Sep-2002
  Arguments: Prg: TProgram; UnitItemsList, UnitInserts: TStrings
  Result:    None
  Purpose:   Fill String lists with the symbols defined in 'TUnitSymbol' objects.
             These cannot be changed as the script changes and so only need to
             be processed once.
-----------------------------------------------------------------------------}
procedure LoadUnitDeclaredSymbols(Prg: TProgram; UnitItemsList, UnitInserts: TStrings;
                                  SynOptions: TdSyn_DisplayOptions;
                                  ContentOptions: TContentOptions;
                                  ClearLists: Boolean);
var
  i : Integer;
  AllTables: TList;

  procedure AddTable(Table : TSymbolTable);
  var
    i : Integer;
  begin
    if AllTables.IndexOf(Table) < 0 then begin
      AllTables.Add(Table);
      for i := 0 to Table.ParentCount - 1 do
        AddTable(Table.Parents[i]);
      if Table is TLinkedSymbolTable then
        AddTable(TLinkedSymbolTable(Table).Parent);
    end;
  end;

begin
  if ClearLists then begin
    UnitItemsList.Clear;
    UnitInserts.Clear;
  end;

  AllTables := TList.Create;
  try
    AddTable(Prg.Table);
    AllTables.Remove(Prg.Table);

    { Cycle the parents of the script (system table and declared units) }
    for i := 0 to AllTables.Count - 1 do
      LoadSymbolsToStrings(UnitItemsList, UnitInserts,
                           TSymbolTable(AllTables.Items[i]),
                           False, False, False, False, SynOptions);

    { Add Unit types - As very last entries in the list, add the unit names themselves }
    for i := Prg.Table.Count - 1 downto 0 do
      if Prg.Table[i] is TUnitSymbol then begin
        UnitInserts.Add(Prg.Table[i].Name);    // add unit name
        UnitItemsList.Add(SymbolToText(nil, Prg.Table[i], SynOptions, ContentOptions, nil));
      end;
  finally
    AllTables.Free;
  end;
end;
