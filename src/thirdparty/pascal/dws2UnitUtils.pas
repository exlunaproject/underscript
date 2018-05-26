{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at                                          }
{                                                                      }
{    http://www.mozilla.org/MPL/                                       }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Original Code is dws2UnitUtils source code, released          }
{    October 1, 2002                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Mark Ericksen       }
{    Portions created by Mark Ericksen are                             }
{    Copyright (C) 2002 Mark Ericksen, United States of America.       }
{    All Rights Reserved.                                              }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2UnitUtils;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  SysUtils, Classes, dws2Symbols, dws2Comp, dws2Exprs;

procedure UnitToScript(AUnit: Tdws2Unit; AProgram: TProgram; ScriptLines: TStrings; ClearLines: Boolean = True);
//procedure UnitToScript(AUnit: Tdws2Unit; ScriptLines: TStrings; ClearLines: Boolean = True);
procedure ScriptToUnit(AUnit: Tdws2Unit; AProgram: TProgram; RemoveUndeclared: Boolean = True);
//
procedure GetUnitSymbolText(Symbol: Tdws2Symbol; ScriptLines, ForwardedImpl: TStrings; PreceedingText: string=''; FuncAsForward: Boolean=True);
function GetFunctionText(AClassName: string; Func: Tdws2Function; BareBones: Boolean): string;
procedure AddUpdateSymbolToCollection(Collection: Tdws2Collection; Symbol: TSymbol);
function GetOrCreateSymbolInCollection(Collection: Tdws2Collection; const SymbolName: string): Tdws2Symbol;
procedure PruneCollectionToTable(Collection: Tdws2Collection; SymbolTable: TSymbolTable);
procedure SortUnitToScript(AUnit: Tdws2Unit; AProgram: TProgram);
procedure SortCollectionToScript(Collection: Tdws2Collection;
                                 ATable: TSymbolTable; UsageType: TSymbolUsage;
                                 Dictionary: TSymbolDictionary);
procedure AddWarningsForUnsupportedTypes(AProgram: TProgram);

//
function IndexOfName(Collection: Tdws2Collection; const AName: string): Integer;
function TypesAreEquivalents(ASym: Tdws2Symbol; BSym: TSymbol): Boolean;
function TypeIsSupportedInUnit(Symbol: TSymbol): Boolean;
function MethodClassSymbol(Method: Tdws2Function): Tdws2Class;
function MethodClassSymbolName(Method: Tdws2Function): string;

// Find the function symbol (TFuncSymbol) that matches the unit function symbol (Tdws2Function)
function FindSymbolForUnitFunction(AProgram: TProgram; AUnitFunc: Tdws2Function): TFuncSymbol;

implementation

uses Contnrs, dws2Errors;

procedure GetUnitSymbolText(Symbol: Tdws2Symbol; ScriptLines, ForwardedImpl: TStrings; PreceedingText: string; FuncAsForward: Boolean);
var
  DefValue: string;
  ClassAncestor: string;
  j: Integer;
  isAbstract: Boolean;
  tmpText: string;
  indentStart: Integer;
begin
  if Symbol = nil then
    Exit;

  { Forward Symbol }
  if Symbol is Tdws2Forward then
    ScriptLines.Add(PreceedingText +
                    Format('type %s = class;', [Symbol.Name]))
  { Constant Symbol }
  else if Symbol is Tdws2Constant then
  begin
    if Tdws2Constant(Symbol).DataType = 'String' then
      DefValue := Format('''%s''',[VarToStr(Tdws2Constant(Symbol).Value)])
    else if Tdws2Constant(Symbol).DataType = 'DateTime' then
      DefValue := Format('DateTime(%f)',[Double(Tdws2Constant(Symbol).Value)])
    else
      DefValue := VarToStr(Tdws2Constant(Symbol).Value);

    ScriptLines.Add(PreceedingText +
                    Format('const %s : %s = %s;', [Tdws2Constant(Symbol).Name,
                                                   Tdws2Constant(Symbol).DataType,
                                                   DefValue]));
  end
  { Enumeration Symbol }
  else if Symbol is Tdws2Enumeration then
  begin
    { Create enumerations as following... (each element to its own line.
      Easier than trying to force to a new line. Still nicely formatted.)
      type
        TDays = (
                 dMonday,
                 dTuesday
                ); }
    ScriptLines.Add('type');
    tmpText := Format('  %s = (', [Symbol.Name]);
    indentStart := Length(tmpText);
    ScriptLines.Add(tmpText);

    for j := 0 to Tdws2Enumeration(Symbol).Elements.Count - 1 do
    begin
      if j < Tdws2Enumeration(Symbol).Elements.Count - 1 then
        tmpText := ','
      else
        tmpText := '';
      ScriptLines.Add(StringOfChar(' ', indentStart) + Tdws2Enumeration(Symbol).Elements.Items[j].Name + tmpText);
    end;
    ScriptLines.Add(StringOfChar(' ', indentStart-1) + ');');
//    ScriptLines.Add(PreceedingText +
//                    Format('type %s;', [Symbol.DisplayName]));
  end
  { Synonym Symbol }
  else if Symbol is Tdws2Synonym then
  begin
    ScriptLines.Add(PreceedingText +
                    Format('type %s = %s;', [Symbol.Name, Tdws2Synonym(Symbol).DataType]));
  end
  { Array Symbol }
  else if Symbol is Tdws2Array then
  begin
    if Tdws2Array(Symbol).IsDynamic then
      ScriptLines.Add(PreceedingText +
                      Format('type %s = array of %s;',
                             [Tdws2Array(Symbol).Name,
                              Tdws2Array(Symbol).DataType]))
    else
      ScriptLines.Add(PreceedingText +
                      Format('type %s = array[%d..%d] of %s;',
                             [Tdws2Array(Symbol).Name,
                              Tdws2Array(Symbol).LowBound,
                              Tdws2Array(Symbol).HighBound,
                              Tdws2Array(Symbol).DataType]));
  end
  { Record Symbol }
  else if Symbol is Tdws2Record then
  begin
    ScriptLines.Add(PreceedingText +
                    'type');
    ScriptLines.Add(PreceedingText +
                    Format('  %s = record', [Tdws2Record(Symbol).Name]));
    for j := 0 to Tdws2Record(Symbol).Members.Count - 1 do
      GetUnitSymbolText(Tdws2Record(Symbol).Members.Items[j], ScriptLines, nil, PreceedingText + '    ');
    ScriptLines.Add(       '  end;');
  end
  { Record Member Symbol }
  else if Symbol is Tdws2Member then
  begin
    ScriptLines.Add(PreceedingText +
                    Format('%s : %s;', [Tdws2Member(Symbol).Name,
                                        Tdws2Member(Symbol).DataType]));
  end
  { Class Symbol }
  else if Symbol is Tdws2Class then
  begin
    ClassAncestor := Tdws2Class(Symbol).Ancestor;
    if ClassAncestor <> '' then
      ClassAncestor := '('+ClassAncestor+')';
    ScriptLines.Add(PreceedingText +
                           'type');
    ScriptLines.Add(PreceedingText +
                    Format('  %s = class%s', [Tdws2Class(Symbol).Name, ClassAncestor]));
    // Class fields
    for j := 0 to Tdws2Class(Symbol).Fields.Count - 1 do
      GetUnitSymbolText(Tdws2Class(Symbol).Fields.SortedItems[j], ScriptLines, nil, PreceedingText + '    ');
    // Class Constructors
    for j := 0 to Tdws2Class(Symbol).Constructors.Count - 1 do
      GetUnitSymbolText(Tdws2Class(Symbol).Constructors.SortedItems[j], ScriptLines, ForwardedImpl, PreceedingText, FuncAsForward);
    // Class Methods
    for j := 0 to Tdws2Class(Symbol).Methods.Count - 1 do
      GetUnitSymbolText(Tdws2Class(Symbol).Methods.SortedItems[j], ScriptLines, ForwardedImpl, PreceedingText, FuncAsForward);
    // Class Properties
    for j := 0 to Tdws2Class(Symbol).Properties.Count - 1 do
      GetUnitSymbolText(Tdws2Class(Symbol).Properties.SortedItems[j], ScriptLines, nil, PreceedingText +'    ');
    ScriptLines.Add(PreceedingText +'  end;');
  end
  { Class Field Symbol }
  else if Symbol is Tdws2Field then
  begin
    ScriptLines.Add(PreceedingText +
                    Format('%s : %s;', [Tdws2Field(Symbol).Name,
                                        Tdws2Field(Symbol).DataType]));
  end
  { Class Method Symbol - Should be before Function Symbol (FuncSymbol is an ancestor type) }
  else if (Symbol is Tdws2Method) or (Symbol is Tdws2Constructor) then
  begin
    // get method name for Class declaration
    ScriptLines.Add(PreceedingText +'    '+GetFunctionText('', Tdws2Function(Symbol), False));
    if Symbol is Tdws2Method then
      isAbstract := (maAbstract in Tdws2Method(Symbol).Attributes)
    else if Symbol is Tdws2Constructor then
      isAbstract := (maAbstract in Tdws2Constructor(Symbol).Attributes)
    else
      isAbstract := False;

    // get method name for method implementation if NOT abstract
    if Assigned(ForwardedImpl) and (not isAbstract) then
    begin
      ForwardedImpl.Add(GetFunctionText(MethodClassSymbolName(Tdws2Method(Symbol)), Tdws2Method(Symbol), True));
      ForwardedImpl.Add('begin');
      ForwardedImpl.Add('  // Leave empty.');
      ForwardedImpl.Add('end;');
      ForwardedImpl.Add('');
    end;
  end
  { Class Property Symbol }
  else if Symbol is Tdws2Property then
  begin
    ScriptLines.Add(PreceedingText +
                    Format('%s', [Symbol.DisplayName]));
  end
  { Function Symbol }
  else if (Symbol is Tdws2Function) and not (Symbol is Tdws2Constructor) then
  begin
    DefValue := PreceedingText + GetFunctionText('',  Tdws2Function(Symbol), False);
    if FuncAsForward then
      DefValue := DefValue + ' forward;';  // make it a forward declaration
    ScriptLines.Add(DefValue);

    if Assigned(ForwardedImpl) then
    begin
      ForwardedImpl.Add(GetFunctionText('',  Tdws2Function(Symbol), False));
      ForwardedImpl.Add('begin');
      ForwardedImpl.Add('  // Do NOT put code here. This is here to make script valid.');
      ForwardedImpl.Add('end;');
      ForwardedImpl.Add('');
    end;
  end
  { Variable Symbol }
  else if (Symbol is Tdws2Variable) or (Symbol is Tdws2Instance) then
  begin
    if Symbol is Tdws2Variable then
      tmpText := Tdws2Variable(Symbol).DataType
    else
      tmpText := Tdws2Instance(Symbol).DataType;

    ScriptLines.Add(PreceedingText +
                    Format('var %s : %s;', [Symbol.Name,
                                            tmpText]));
  end;
end;

function GetFunctionText(AClassName: string; Func: Tdws2Function; BareBones: Boolean): string;
var
  i: Integer;
  TypeText: string;
  Params: string;
  Attrib: TMethodAttributes;
begin
  { Assemble the method text piece-wise. }

  // if a method of an object
  if (Func is Tdws2Method) or (Func is Tdws2Constructor) then
  begin
    Params := '';
    if not BareBones then
    begin
      for i := 0 to Func.Parameters.Count - 1 do
      begin
        // if not the first one, add ';' to end
        if i > 0 then
          Params := Params + '; ';
        // add param name (prepend prefix)
        Params := Params + Func.Parameters.Items[i].DisplayName;
      end;
      // Put '()' around parameters if used.
      if Params <> '' then
        Params := '(' + Params + ')';      // wrap up parameters
    end;

    { Get the method kind and build the base }
    TypeText := '';
    if (Func is Tdws2Method) then
    begin
      case Tdws2Method(Func).Kind of
        mkClassProcedure, mkProcedure: TypeText := 'procedure';
        mkClassFunction, mkFunction:   TypeText := 'function';
        mkConstructor:  TypeText := 'constructor';
        mkDestructor:   TypeText := 'destructor';
      else
        Assert(false); // if triggered, this func needs upgrade !
      end;
    end
    else if Func is Tdws2Constructor then
      TypeText := 'constructor';

    { if a class name is provided, add the '.' to the end (used in implementations) }
    if AClassName <> '' then
      AClassName := AClassName + '.';

    { Assemble method with method type, ClassName(if desired), Method name and parameters. }
    Result := Format('%s %s%s', [TypeText, AClassName, Func.Name + Params]);
    { If method kind is a 'class' method, add the 'class' directive }
    if Func is Tdws2Method then
    begin
      if Tdws2Method(Func).Kind in [mkClassProcedure, mkClassFunction] then
        Result := 'class '+Result;
      { If method has a result, add the result type }
      if (Tdws2Method(Func).ResultType <> '') and (not BareBones) then
        Result := Result + ' : ' + Tdws2Method(Func).ResultType;
    end;
    { Method declaration complete. Add trailing semi-colon }
    Result := Result + ';';

    { Add method attributes only if for class declaration }
    if AClassName = '' then begin
      if Func is Tdws2Method then
        Attrib := Tdws2Method(Func).Attributes
      else
        Attrib := Tdws2Constructor(Func).Attributes;

      if maOverride in Attrib then   // overrides will also be marked as virtual
        Result := Result + ' override;'
      else if maVirtual in Attrib then
        Result := Result + ' virtual;';
      if maReintroduce in Attrib then
        Result := Result + ' reintroduce;';
      if maAbstract in Attrib then
        Result := Result + ' abstract;';
    end;
  end
  // if a plain function (not method)
  else
    Result := Func.DisplayName;
end;

procedure AddUpdateSymbolToCollection(Collection: Tdws2Collection; Symbol: TSymbol);
var
  UseSym: Tdws2Symbol;
  i: Integer;
//  funcSym: TFuncSymbol; // pointer for easier use
begin
  { Create or update the Unit declarations based on script symbols. }

  { Get or create the symbol in the desired collection. Settings will follow. }
  UseSym := GetOrCreateSymbolInCollection(Collection, Symbol.Name);// is not called here because of

  { Symbol is Array }
  if Symbol is TArraySymbol then begin
    with Tdws2Array(UseSym) do begin
      DataType  := TArraySymbol(Symbol).Typ.Name;
      if Symbol is TDynamicArraySymbol then
        IsDynamic := True
      else if Symbol is TStaticArraySymbol then
      begin
        LowBound  := TStaticArraySymbol(Symbol).LowBound;
        HighBound := TStaticArraySymbol(Symbol).HighBound;
      end;
    end;
  end;
  { Symbol is Class (Classes or Forwards) }
  if Symbol is TClassSymbol then begin
    { If collection is Classes (no additional processing for forwards) }
    if Collection is Tdws2Classes then begin
      with UseSym as Tdws2Class do begin
        { Ancestor }
          { NOTE: Class will never have a 'nil' parent. If parent is TObject, don't write out the ancestor name.
             This has the side effect of explicitly declared TObject ancestors will be changed to implied. }
        Ancestor  := TClassSymbol(Symbol).Parent.Name;
        if Ancestor = 'TObject' then
          Ancestor  := '';
        { Synch members of class (Fields, Methods, Properties }
        for i := 0 to TClassSymbol(Symbol).Members.Count - 1 do begin
          { Synch Fields }
          if TClassSymbol(Symbol).Members[i] is TFieldSymbol then
            AddUpdateSymbolToCollection(Fields, TClassSymbol(Symbol).Members[i]);
          { Synch Methods }
          if TClassSymbol(Symbol).Members[i] is TMethodSymbol then
          begin
            { Synch Constructors - write to Constructors collection }
            if TMethodSymbol(TClassSymbol(Symbol).Members[i]).Kind = fkConstructor then
              AddUpdateSymbolToCollection(Constructors, TClassSymbol(Symbol).Members[i])
            { Synch Methods - regular methods get written }
            else
              AddUpdateSymbolToCollection(Methods, TClassSymbol(Symbol).Members[i]);
          end;
          { Synch Properties }
          if TClassSymbol(Symbol).Members[i] is TPropertySymbol then
            AddUpdateSymbolToCollection(Properties, TClassSymbol(Symbol).Members[i]);
        end;
      end; {with Class}
    end; {if Collection is Classes}
  end;

  { Symbol is Enumeration }
  if Symbol is TEnumerationSymbol then begin
    { Synch Enumeration Elements (Recursive) }
    for i := 0 to TEnumerationSymbol(Symbol).Elements.Count - 1 do
      AddUpdateSymbolToCollection(Tdws2Enumeration(UseSym).Elements, TEnumerationSymbol(Symbol).Elements[i]);
  end;

  { Symbol is Element (of Enumeration) - type of Constant, process first }
  if Symbol is TElementSymbol then begin
  // Symbol is Tdws2Element then begin
    with Tdws2Element(UseSym) do begin
      IsUserDef := TElementSymbol(Symbol).IsUserDef;
      if TElementSymbol(Symbol).IsUserDef then
        UserDefValue := TElementSymbol(Symbol).UserDefValue;
    end;
  end
  { Symbol is Constant }
  else if Symbol is TConstSymbol then begin
    with Tdws2Constant(UseSym) do begin
      DataType  := TConstSymbol(Symbol).Typ.Name;
      Value     := TConstSymbol(Symbol).Data[0];
    end;
  end;

  { Symbol is Synonym (Alias) }
  if Symbol is TAliasSymbol then begin
    with Tdws2Synonym(UseSym) do
      DataType  := TAliasSymbol(Symbol).Typ.Name;
  end;

  { Symbol is Record }
  if Symbol is TRecordSymbol then begin
    { Synch Record Members (Recursive) }
    for i := 0 to TRecordSymbol(Symbol).Members.Count - 1 do
      AddUpdateSymbolToCollection(Tdws2Record(UseSym).Members, TRecordSymbol(Symbol).Members[i]);
  end;

  { Symbol is Variable (DataSymbol) }
  if Symbol is TDataSymbol then begin
    if Symbol.Typ is TClassSymbol then   // if a class, put as Instance
      Tdws2Instance(UseSym).DataType := TDataSymbol(Symbol).Typ.Name
    else                                 // not a class, regular variable
      Tdws2Variable(UseSym).DataType := TDataSymbol(Symbol).Typ.Name;
  end;

  { Symbol is Function }
  if Symbol is TFuncSymbol then begin
    with Tdws2Function(UseSym) do begin   // this is shared for Functions and Methods
      { Result type }
      if TFuncSymbol(Symbol).Result <> nil then
        ResultType := TFuncSymbol(Symbol).Result.Typ.Name
      else
        ResultType := '';
      { Synch Parameters }
      for i := 0 to TFuncSymbol(Symbol).Params.Count - 1 do
        AddUpdateSymbolToCollection(Parameters, TFuncSymbol(Symbol).Params[i]);
    end;
    { If function is a Method (Class) }
    if Symbol is TMethodSymbol then begin
      with Tdws2Method(UseSym) do begin
        { Result type - Methods internally store ancestor FFuncType and a
          new FResultType. FResultType must be set for it to stick. }
        if TMethodSymbol(Symbol).Result <> nil then
          ResultType := TMethodSymbol(Symbol).Result.Typ.Name
        else
          ResultType := '';
        { Method Attributes }
        Attributes := [];
        if TMethodSymbol(Symbol).IsOverride then     // Override (cannot be set as virtual; override;)
          Attributes := Attributes + [maOverride]
        else if TMethodSymbol(Symbol).IsVirtual then // Virtual
          Attributes := Attributes + [maVirtual];
        if TMethodSymbol(Symbol).IsAbstract then     // Abstract
          Attributes := Attributes + [maAbstract];
        { Method name is declared in class and ancestor }
        if TMethodSymbol(Symbol).IsOverlap then begin // Reintrodue
          { If parent method is virtual, it is reintroduced  }
          if TMethodSymbol(Symbol).ParentMeth.IsVirtual then
            Attributes := Attributes + [maReintroduce];
            { If parent method is NOT virtual, it is static, no special attribute }
        end;
        { Method Kind }
        { If a Class Method }
        if TMethodSymbol(Symbol).IsClassMethod then begin
          if TMethodSymbol(Symbol).Kind = fkFunction then         // class function
            Kind := mkClassFunction
          else if TMethodSymbol(Symbol).Kind = fkProcedure then   // class procedure
            Kind := mkClassProcedure
          else
            Assert(False);   // unexptected
        end
        { Not a Class Method }
        else begin
          if TMethodSymbol(Symbol).Kind = fkFunction then         // function
            Kind := mkFunction
          else if TMethodSymbol(Symbol).Kind = fkProcedure then   // procedure
            Kind := mkProcedure
          else if TMethodSymbol(Symbol).Kind = fkConstructor then // constructor
            Kind := mkConstructor
          else if TMethodSymbol(Symbol).Kind = fkDestructor then  // destructor
            Kind := mkDestructor
          else
            Assert(False);   // unexptected
        end;
      end; {with}
    end; {End Method}
  end; {End function}

  { Symbol is Parameter (Function/Method) }
  if Symbol is TParamSymbol then begin
    with UseSym as Tdws2Parameter do begin
      if Symbol is TVarParamSymbol then begin
        IsVarParam := True;
        IsWritable := TVarParamSymbol(Symbol).IsWritable;
      end
      // not a Var param, remove any settings
      else begin
        IsVarParam := False;
        IsWritable := False;
      end;
      DataType  := TParamSymbol(Symbol).Typ.Name;
    end;
  end;

  { Symbol is Member (Records) }
  if Symbol is TMemberSymbol then begin
    with UseSym as Tdws2Member do
      DataType  := TMemberSymbol(Symbol).Typ.Name;
  end;

  { Symbol is Field (Class) }
  if Symbol is TFieldSymbol then begin
    with UseSym as Tdws2Field do
      DataType := TFieldSymbol(Symbol).Typ.Name;
  end;

  { Symbol is Property (Class) }
  if Symbol is TPropertySymbol then begin
    with UseSym as Tdws2Property do begin
      DataType    := TPropertySymbol(Symbol).Typ.Name;
      { ReadAccess setting }
      if TPropertySymbol(Symbol).ReadSym <> nil then
        ReadAccess  := TPropertySymbol(Symbol).ReadSym.Name
      else
        ReadAccess  := '';
      { WriteAccess setting }
      if TPropertySymbol(Symbol).WriteSym <> nil then
        WriteAccess := TPropertySymbol(Symbol).WriteSym.Name
      else
        WriteAccess := '';
      { Synch property default status }
      IsDefault   := TPropertySymbol(Symbol).IsDefault;
      if Assigned(TPropertySymbol(Symbol).IndexSym) then
        IndexType   := TPropertySymbol(Symbol).IndexSym.Name;
      if Length(TPropertySymbol(Symbol).IndexValue) > 0 then
        IndexValue  := TPropertySymbol(Symbol).IndexValue[0]; 
      { Get property parameters (array parameter) - only if the property's 'read'
        symbol is a function }
      for i := 0 to TPropertySymbol(Symbol).ArrayIndices.Count - 1 do
        AddUpdateSymbolToCollection(Parameters, TPropertySymbol(Symbol).ArrayIndices[i]);
    end;
  end;
end;

function GetOrCreateSymbolInCollection(Collection: Tdws2Collection; const SymbolName: string): Tdws2Symbol;
var
  idx: Integer;
begin
  idx := IndexOfName(Collection, SymbolName);
  // if not found, create it, set properties
  if idx = -1 then
    Result := Tdws2Symbol(Collection.Add)
  else
    Result := Tdws2Symbol(Collection.Items[idx]);

  Result.Name := SymbolName;      // set the name if created, reset if exists (change case, etc)
end;

procedure PruneCollectionToTable(Collection: Tdws2Collection; SymbolTable: TSymbolTable);
var
  i: Integer;
  Sym: TSymbol;
begin
  { Remove items from collection that are not in the comperable SymbolTable.
   (This won't remove Delphi event code, just the event link) }
  i := 0;
  while i <= Collection.Count - 1 do
  begin
    Sym := SymbolTable.FindLocal(Collection.Items[i].Name);
    { If Collection item is not in Table, remove from collection }
    if not Assigned(Sym) then
      Collection.Delete(i)
    else
    { Item is in Table, check for applicable sub-types }
    begin
      { Class - Prune fields, methods, and properties }
      if (Collection.Items[i] is Tdws2Class) and (Sym is TClassSymbol) then
      begin
        // Prune fields
        PruneCollectionToTable(Tdws2Class(Collection.Items[i]).Fields, TClassSymbol(Sym).Members);
        // Prune constructors
        PruneCollectionToTable(Tdws2Class(Collection.Items[i]).Constructors, TClassSymbol(Sym).Members);
        // Prune methods
        PruneCollectionToTable(Tdws2Class(Collection.Items[i]).Methods, TClassSymbol(Sym).Members);
        // Prune Properties
        PruneCollectionToTable(Tdws2Class(Collection.Items[i]).Properties, TClassSymbol(Sym).Members);
      end
      { Method - Prune parameters }
      else if (Collection.Items[i] is Tdws2Method) and (Sym is TMethodSymbol) then
        PruneCollectionToTable(Tdws2Method(Collection.Items[i]).Parameters, TMethodSymbol(Sym).Params)
      { Functions - Prune parameters (works for Constructors too) }
      else if (Collection.Items[i] is Tdws2Function) and (Sym is TFuncSymbol) then
        PruneCollectionToTable(Tdws2Function(Collection.Items[i]).Parameters, TFuncSymbol(Sym).Params)
      { Property - Prune paramecters }
      else if (Collection.Items[i] is Tdws2Property) and (Sym is TPropertySymbol) then
        PruneCollectionToTable(Tdws2Property(Collection.Items[i]).Parameters, TPropertySymbol(Sym).ArrayIndices) 
      { Record - Prune members }
      else if (Collection.Items[i] is Tdws2Record) and (Sym is TRecordSymbol) then // Method (get parameters)
        PruneCollectionToTable(Tdws2Record(Collection.Items[i]).Members, TRecordSymbol(Sym).Members);
      //
      // Up the counter to check the next item in collection
      Inc(i);
    end; {begin - Check sub types}
  end; {while}
end;

procedure SortUnitToScript(AUnit: Tdws2Unit; AProgram: TProgram);
begin
  { Sort the items within each collection according to the order declared in the
    script. (Will recursively sort for Class members, etc.) }

  { Array Symbols }
  SortCollectionToScript(AUnit.Arrays, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  // forwards are not significant for order, still - order them as classes declared
  SortCollectionToScript(AUnit.Forwards, AProgram.Table, suForward, AProgram.SymbolDictionary);

  { Class Symbols }
  SortCollectionToScript(AUnit.Classes, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Constant Symbols }
  SortCollectionToScript(AUnit.Constants, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Enumeration Symbols }
  SortCollectionToScript(AUnit.Enumerations, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Function Symbol }
  SortCollectionToScript(AUnit.Functions, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Record Symbol }
  SortCollectionToScript(AUnit.Records, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Synonym Symbol }
  SortCollectionToScript(AUnit.Synonyms, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Instance Symbol }
  SortCollectionToScript(AUnit.Instances, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);

  { Variable Symbol }
  SortCollectionToScript(AUnit.Variables, AProgram.Table, suDeclaration, AProgram.SymbolDictionary);
end;

procedure SortCollectionToScript(Collection: Tdws2Collection; 
                                 ATable: TSymbolTable; UsageType: TSymbolUsage;
                                 Dictionary: TSymbolDictionary);
var
  i: Integer;
  changed: Boolean;
  CurrPos, NextPos: TSymbolPosition;
  Symbol: TSymbol;
begin
  { Uses the BubbleSort algorithm. I did not care to further optimize the sort
    because it is the final step of a design-time only process. }

  { BubbleSort method }
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for i := 0 to Collection.Count - 2 do
    begin
      // Don't use CollectionItem.Name on Dictionary. Same name may be used
      // within different table scopes.
      CurrPos := Dictionary.FindSymbolUsage(ATable.FindLocal(Collection.Items[i].Name), UsageType);
      NextPos := Dictionary.FindSymbolUsage(ATable.FindLocal(Collection.Items[i+1].Name), UsageType);
      { Compare positions in script }
      if Assigned(CurrPos) and Assigned(NextPos) then
        if CurrPos.ScriptPos.Pos > NextPos.ScriptPos.Pos then  // if current goes after the next, move next up one
        begin
          Collection.Items[i+1].Index := Collection.Items[i+1].Index - 1;
          Changed := True; 
        end;
    end;
  end;

  { Cycle Collection to see if it has sub-types to sort }
  for i := 0 to Collection.Count - 1 do
  begin
    Symbol := ATable.FindLocal(Collection.Items[i].Name);
    { Class - sort Fields, Methods, Properties }
    if (Collection.Items[i] is Tdws2Class) and (Symbol is TClassSymbol) then
    begin
      SortCollectionToScript(Tdws2Class(Collection.Items[i]).Fields, TClassSymbol(Symbol).Members, suDeclaration, Dictionary);
      SortCollectionToScript(Tdws2Class(Collection.Items[i]).Constructors, TClassSymbol(Symbol).Members, suDeclaration, Dictionary);
      SortCollectionToScript(Tdws2Class(Collection.Items[i]).Methods, TClassSymbol(Symbol).Members, suDeclaration, Dictionary);
      SortCollectionToScript(Tdws2Class(Collection.Items[i]).Properties, TClassSymbol(Symbol).Members, suDeclaration, Dictionary);
    end;

    { Record - sort Members }
    if (Collection.Items[i] is Tdws2Record) and (Symbol is TRecordSymbol) then
      SortCollectionToScript(Tdws2Record(Collection.Items[i]).Members, TRecordSymbol(Symbol).Members, suDeclaration, Dictionary);

    { Property - sort parameters }
    if (Collection.Items[i] is Tdws2Property) and (Symbol is TPropertySymbol) then
      SortCollectionToScript(Tdws2Property(Collection.Items[i]).Parameters, TPropertySymbol(Symbol).ArrayIndices, suDeclaration, Dictionary);

    { Enumerations - sort elements }
    if (Collection.Items[i] is Tdws2Enumeration) and (Symbol is TEnumerationSymbol) then
      SortCollectionToScript(Tdws2Enumeration(Collection.Items[i]).Elements, TEnumerationSymbol(Symbol).Elements, suDeclaration, Dictionary);

    { Function - (methods too), sort parameters }
    if (Collection.Items[i] is Tdws2Function) and (Symbol is TFuncSymbol) then
      SortCollectionToScript(Tdws2Function(Collection.Items[i]).Parameters, TFuncSymbol(Symbol).Params, suDeclaration, Dictionary);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: AddWarningsForUnsupportedTypes
  Author:    Mark Ericksen
  Date:      19-Oct-2002
  Arguments: AProgram: TProgram
  Result:    None
  Purpose:   Add program warning messages for types that are not supported in Units.
-----------------------------------------------------------------------------}
procedure AddWarningsForUnsupportedTypes(AProgram: TProgram);
var
  i: Integer;
  SymPos: TSymbolPosition;
begin
  if not Assigned(AProgram) then Exit;

  for i := 0 to AProgram.Table.Count - 1 do
  begin
    SymPos := AProgram.SymbolDictionary.FindSymbolUsage(AProgram.Table[i], suDeclaration);
    if Assigned(SymPos) and (not TypeIsSupportedInUnit(AProgram.Table[i])) then
      AProgram.Msgs.AddCompilerWarning(SymPos.ScriptPos, Format('The type "%s" is not supported in a Tdws2Unit.', [AProgram.Table[i].ClassName]));
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IndexOfName
  Author:    Mark Ericksen
  Date:      14-Oct-2002
  Arguments: Collection: Tdws2Collection; const AName: string
  Result:    Integer
  Purpose:   Return the index of the named item from the Tdws2Collection.
-----------------------------------------------------------------------------}
function IndexOfName(Collection: Tdws2Collection; const AName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Collection.Count - 1 do
    if CompareText(Collection.Items[i].Name, AName) = 0 then
    begin
      Result := i;
      Break;
    end;
end;

type THackedTdws2Unit = class(Tdws2Unit);
{-----------------------------------------------------------------------------
  Procedure: UnitToScript
  Author:    Mark Ericksen
  Date:      25-Jun-2003
  Arguments: AUnit: Tdws2Unit; AProgram: TProgram; ScriptLines: TStrings; ClearLines: Boolean = True
  Result:    None
  Purpose:   Convert the Tdws2Unit declarations to a valid script. Use the
             AProgram to order them correctly for dependencies. 
             If the AProgram has errors it will output all symbols in their default
             order to ensure some level of viewing and editing. The script that
             is compiled to create the AProgram can be empty but it must include
             the unit that is being edited.
  NOTE:      The compiled symbol for a forwarded class will appear out of order
             from the natural order you might expect. In order to create a valid
             script special handling is required for classes that may cause other
             symbols to be moved from their original placements. The helper
             function "WriteOutClass" is called recursively to output classes
             where the ancestor is always output first. When outputing classes,
             it will first output the ancestors and then all other classes that
             appear in the classes collection that are prior to the class being
             written out. The logic can be convoluted but it works. It is possible
             that this process can be further simplified.
-----------------------------------------------------------------------------}
procedure UnitToScript(AUnit: Tdws2Unit; AProgram: TProgram; ScriptLines: TStrings; ClearLines: Boolean = True);
var
  i, j, x: Integer;
  ForwardedImpl: TStringList;   // implementations of forwarded declarations (functions and class methods)
  unitSym: TUnitSymbol;
  lastSym,
  currSym: TSymbol;
  coll: Tdws2Collection;
  suppress: Boolean;
  unitTable: TSymbolTable;
  classList: TObjectList;
  sym: Tdws2Symbol;


      { Write out the classes where the ancestors (if in the list) will be output
        first. Nil the class pointer so as to not upset the indexes. }
      procedure WriteOutClass(AClass: Tdws2Class; AForwardImpl: TStrings);
      var
        current, idx: Integer;
      begin
        if not Assigned(AClass) then EXIT;
        current := classList.IndexOf(AClass);  // index of current class
        if current < 0 then EXIT;              // if class doesn't need to be written, quit

        // find ancestor in classList. If found, write it out first
        idx := classList.IndexOf(AUnit.Classes.Symbols[AClass.Ancestor]);
        if idx > -1 then        // ancestor is in the list, write out ancestor first
          WriteOutClass(Tdws2Class(classList[idx]), ForwardedImpl);    // recurse to write parent first
        GetUnitSymbolText(AClass, ScriptLines, AForwardImpl);
        classList[current] := nil;   // nil the class after writing (clear it without upsetting indexes)
      end;

begin
  if AProgram = nil then
    Exit;

  ScriptLines.BeginUpdate;
  ForwardedImpl:= TStringList.Create;
  classList := TObjectList.Create(False);
  try
    if ClearLines then
      ScriptLines.Clear;

    { If has errors, write out symbols as-is. Can't reliably order any better. }
    if AProgram.Msgs.HasCompilerErrors or AProgram.Msgs.HasErrors then
    begin
      ScriptLines.Add('// IMPORTANT:');
      ScriptLines.Add('// Errors were found in the program. The symbols will be displayed in the default order.');

      ScriptLines.Add('');
      for x := Low(THackedTdws2Unit(AUnit).FCollections) to High(THackedTdws2Unit(AUnit).FCollections) do
      begin
        ScriptLines.Add(Format('{ %s }', [THackedTdws2Unit(AUnit).FCollections[x].ClassName]));
        for i := 0 to THackedTdws2Unit(AUnit).FCollections[x].Count - 1 do
        begin
          if (THackedTdws2Unit(AUnit).FCollections[x] is Tdws2Functions) or
             (THackedTdws2Unit(AUnit).FCollections[x] is Tdws2Classes) then
            GetUnitSymbolText(THackedTdws2Unit(AUnit).FCollections[x].Items[i], ScriptLines, ForwardedImpl)
          else
            GetUnitSymbolText(THackedTdws2Unit(AUnit).FCollections[x].Items[i], ScriptLines, nil);
        end;{for i}
        ScriptLines.Add('');   // line for spacing
      end;{for x}
    end
    { No compiler errors. Try to process in the declared order of the program. }
    else
    begin
      unitSym := AProgram.Table.FindSymbol(AUnit.UnitName) as TUnitSymbol;
      if not Assigned(unitSym) then
        raise Exception.Create('Unit symbol being edited not be found.');

      { Process Forwards }
      { Write out section header if there is something to display }
      for i := 0 to AUnit.Forwards.Count - 1 do
        GetUnitSymbolText(AUnit.Forwards.SortedItems[i], ScriptLines, nil);

      { Add all classes to the object list. }
      for i := 0 to AUnit.Classes.Count - 1 do
        classList.Add(AUnit.Classes.Items[i]);

      lastSym := nil;

      unitTable := unitSym.Table;
      if unitTable is TLinkedSymbolTable then
        unitTable := TLinkedSymbolTable(unitTable).Parent;

      { Cycle all remaining symbols in unit to list them in the order of occurence
        except for classes that have forwards. Any forwarded classes go last. }
      for i := 0 to unitTable.Count - 1 do
      begin
        currSym := unitTable.Symbols[i];

        suppress := False;
        { If a class symbol, check if forwarded. If forwarded in this unit then
          don't process now. Process after all others. }
        if (currSym is TClassSymbol) and (AUnit.Forwards.Symbols[currSym.Name] <> nil) then    // no forward, do now
          suppress := True;

        if not suppress then
        begin
          { If we have a last type then compare the types. If different then separate them. }
          if Assigned(lastSym) and (currSym.ClassType <> lastSym.ClassType) then
            ScriptLines.Add('');
          lastSym := currSym;

          { Search all the collections for the type }
          for x := Low(THackedTdws2Unit(AUnit).FCollections) to High(THackedTdws2Unit(AUnit).FCollections) do
          begin
            coll := Tdws2Collection(THackedTdws2Unit(AUnit).FCollections[x]);
            sym := coll.Symbols[currSym.Name];
            if Assigned(sym) then
            begin
              if (sym is Tdws2Function) then
                GetUnitSymbolText(sym, ScriptLines, ForwardedImpl)
              else if sym is Tdws2Class then
              begin
                for j := coll.IndexOf(sym.Name) downto 0 do
                  WriteOutClass(Tdws2Class(coll.Items[j]), ForwardedImpl);
                classList.Pack;
              end
              else    // all other symbols
                GetUnitSymbolText(sym, ScriptLines, nil);
            end;
          end;{for x}
        end;
      end;{for i}

      { Write out any remaining classes that weren't already written }
      while classList.Count > 0 do
      begin
        ScriptLines.Add('');
        WriteOutClass(Tdws2Class(classList.First), ForwardedImpl);
        classList.Pack;
      end;
    end;

    { Cycle the ScriptLines list and if the text goes beyone 80 chars, look for
      commas from 80 and back. Copy to new line and insert AFTER current line.
      Delete from current line ? }

    { Put class implementations down }
    if ForwardedImpl.Count > 0 then begin
      ScriptLines.Add('');
      ScriptLines.Add('{ ============================================== }');
      ScriptLines.Add('{ ========= Empty Implementation Stubs ========= }');
      ScriptLines.Add('{ ================= Leave Blank ================ }');
      ScriptLines.Add('{ ============================================== }');
      ScriptLines.AddStrings(ForwardedImpl);
    end;
  finally
    ForwardedImpl.Free;
    classList.Free;
    ScriptLines.EndUpdate;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ScriptToUnit
  Author:    Mark Ericksen
  Date:      29-Aug-2003
  Arguments: AUnit: Tdws2Unit; AProgram: TProgram; RemoveUndeclared: Boolean
  Result:    None
  Purpose:   Takes compiled symbols from a script and updates the declaration
             symbols in a Tdws2Unit to match. It will create new symbols, remove
             old ones, and re-arrange declaration orders.
-----------------------------------------------------------------------------}
procedure ScriptToUnit(AUnit: Tdws2Unit; AProgram: TProgram; RemoveUndeclared: Boolean);
var
  i: Integer;
begin
  if AProgram = nil then
    Exit;
  { Don't synch if has syntax errors }
  if AProgram.Msgs.HasCompilerErrors or AProgram.Msgs.HasErrors then EXIT;

  { Examine only the symbols declared in the generated script }
  { Add symbols declared StartingTable - optionally include unit declarations }
  for i := 0 to AProgram.Table.Count - 1 do begin
    // This won't process unit declarations, only script declared symbols

    { Update Arrays }
    if AProgram.Table.Symbols[i] is TArraySymbol then
      AddUpdateSymbolToCollection(AUnit.Arrays, AProgram.Table.Symbols[i]);

    { Update Forwards (Class forward) }
    if AProgram.Table.Symbols[i] is TClassSymbol then
      // if a class forward was found, add the symbol
      if Assigned(AProgram.SymbolDictionary.FindSymbolUsage(AProgram.Table.Symbols[i], suForward)) then
        AddUpdateSymbolToCollection(AUnit.Forwards, AProgram.Table.Symbols[i]);

    { Update Classes }
    if AProgram.Table.Symbols[i] is TClassSymbol then
      AddUpdateSymbolToCollection(AUnit.Classes, AProgram.Table.Symbols[i]);

    { Update Constants - A TElement is a TConstSymbol but is part of TEnumerationSymbol } 
    if AProgram.Table.Symbols[i].ClassType = TConstSymbol then  // exactly match class type
      AddUpdateSymbolToCollection(AUnit.Constants, AProgram.Table.Symbols[i]);

    { Update Enumerations }
    if AProgram.Table.Symbols[i] is TEnumerationSymbol then
      AddUpdateSymbolToCollection(AUnit.Enumerations, AProgram.Table.Symbols[i]);

    { Update Synonyms }
    if AProgram.Table.Symbols[i] is TAliasSymbol then
      AddUpdateSymbolToCollection(AUnit.Synonyms, AProgram.Table.Symbols[i]);

    { Update Functions }
    if AProgram.Table.Symbols[i] is TFuncSymbol then
      AddUpdateSymbolToCollection(AUnit.Functions, AProgram.Table.Symbols[i]);

    { Update Records }
    if AProgram.Table.Symbols[i] is TRecordSymbol then
      AddUpdateSymbolToCollection(AUnit.Records, AProgram.Table.Symbols[i]);

    { Update Variables }
    if AProgram.Table.Symbols[i] is TDataSymbol then
    begin
      // if a dataSymbol type is a class, put under Instances
      if TDataSymbol(AProgram.Table.Symbols[i]).Typ is TClassSymbol then
        AddUpdateSymbolToCollection(AUnit.Instances, AProgram.Table.Symbols[i])
      else
        AddUpdateSymbolToCollection(AUnit.Variables, AProgram.Table.Symbols[i]);
      // else, put under Variables
    end;
  end;

  { Remove from the unit was was removed from the script. }
  if RemoveUndeclared then
  begin
    { Remove from Arrays }
    PruneCollectionToTable(AUnit.Arrays, AProgram.Table);

    { Remove from Forwards (Class forward) }
    i := 0;
    while i <= AUnit.Forwards.Count - 1 do
    begin
      { This is done differently because we have to find if it was forwarded.
        The original forwarded symbol was already removed from the table. }
      if AProgram.SymbolDictionary.FindSymbolUsage(AUnit.Forwards.Items[i].Name, suForward) = nil then
        AUnit.Forwards.Delete(i)
      else
        Inc(i);
    end;

    { Remove from Classes }
    PruneCollectionToTable(AUnit.Classes, AProgram.Table);

    { Remove from Constants }
    PruneCollectionToTable(AUnit.Constants, AProgram.Table);

    { Remove from Enumerations }
    PruneCollectionToTable(AUnit.Enumerations, AProgram.Table);

    { Remove from Functions }
    PruneCollectionToTable(AUnit.Functions, AProgram.Table);

    { Remove from Records }
    PruneCollectionToTable(AUnit.Records, AProgram.Table);

    { Remove from Synonyms }
    PruneCollectionToTable(AUnit.Synonyms, AProgram.Table);

    { Remove from Instances }
    PruneCollectionToTable(AUnit.Instances, AProgram.Table);

    { Remove from Variables }
    PruneCollectionToTable(AUnit.Variables, AProgram.Table);
  end;

  { Sort the declaration following the script }
  SortUnitToScript(AUnit, AProgram);

  { Warn about unsupported types. Types used in the script that are not
    supported in units. }
  AddWarningsForUnsupportedTypes(AProgram);
end;

function TypesAreEquivalents(ASym: Tdws2Symbol; BSym: TSymbol): Boolean;
begin
  if (ASym is Tdws2Array) and (BSym is TArraySymbol) then
    Result := True
  else if (ASym is Tdws2Constant) and (BSym is TConstSymbol) then
    Result := True
  else if (ASym is Tdws2Variable) and (BSym is TDataSymbol) then
    Result := True
  else if (ASym is Tdws2Class) and (BSym is TClassSymbol) then
    Result := True
  else if (ASym is Tdws2Method) and (BSym is TMethodSymbol) then   // compare Method before function (descendent)
    Result := True
  else if (ASym is Tdws2Forward) and (BSym is TClassSymbol) then   // !No direct comparison for forwards
    Result := True
  else if (ASym is Tdws2Field) and (BSym is TFieldSymbol) then
    Result := True
  else if (ASym is Tdws2Property) and (BSym is TPropertySymbol) then
    Result := True
  else if (ASym is Tdws2Record) and (BSym is TRecordSymbol) then
    Result := True
  else if (ASym is Tdws2Member) and (BSym is TMemberSymbol) then
    Result := True
  else if (ASym is Tdws2Function) and (BSym is TFuncSymbol) then
    Result := True
  else if (ASym is Tdws2Parameter) and (BSym is TParamSymbol) then
    Result := True
  else
    Result := False;
end;

{-----------------------------------------------------------------------------
  Procedure: TypeIsSupportedInUnit
  Author:    Mark Ericksen
  Date:      19-Oct-2002
  Arguments: Symbol: TSymbol
  Result:    Boolean
  Purpose:   Return if the symbol is supported for storage in a Tdws2Unit.
-----------------------------------------------------------------------------}
function TypeIsSupportedInUnit(Symbol: TSymbol): Boolean;
begin
  Result := (Symbol is TArraySymbol) or
            (Symbol is TConstSymbol) or
            (Symbol is TEnumerationSymbol) or
            (Symbol is TAliasSymbol) or
            (Symbol is TClassSymbol) or
            (Symbol is TFieldSymbol) or
            (Symbol is TMethodSymbol) or
            (Symbol is TRecordSymbol) or
            (Symbol is TMemberSymbol) or
            (Symbol is TFuncSymbol) or
            (Symbol is TParamSymbol) or
            (Symbol.ClassType = TDataSymbol);    // not Symbol is TDataSymbol because there are unsupported descendants
end;

function MethodClassSymbol(Method: Tdws2Function): Tdws2Class;
var
  tmpOwner: TPersistent;
begin
  Result := nil;
  if Assigned(Method) then
  begin
    tmpOwner := Tdws2Collection(Method.Collection).GetOwner;
    if tmpOwner is Tdws2Class then
      Result := Tdws2Class(tmpOwner);
    //Result := Tdws2Class(Tdws2Collection(Method.Collection).GetOwner);
  end;
end;

function MethodClassSymbolName(Method: Tdws2Function): string;
var
  ClassSym: Tdws2Class;
begin
  Result := '';
  ClassSym := MethodClassSymbol(Method);
  if Assigned(ClassSym) then
    Result := ClassSym.Name;
end;

{-----------------------------------------------------------------------------
  Procedure: FindSymbolForUnitFunction
  Author:    Mark Ericksen
  Date:      20-Nov-2002
  Arguments: AProgram: TProgram; AUnitFunc: Tdws2Function
  Result:    TFuncSymbol
  Purpose:   Find the function symbol (TFuncSymbol) that matches the unit function symbol (Tdws2Function)
-----------------------------------------------------------------------------}
function FindSymbolForUnitFunction(AProgram: TProgram; AUnitFunc: Tdws2Function): TFuncSymbol;
var
  classSym: TClassSymbol;
begin
  Result := nil;
  // dealing with a method of a class
  if (AUnitFunc is Tdws2Method) or (AUnitFunc is Tdws2Constructor) then
  begin
    // find the owning class (compiled symbol)
    classSym := TClassSymbol(AProgram.Table.FindSymbol(Tdws2Class(Tdws2Collection(AUnitFunc.Collection).GetOwner).Name));
    if Assigned(classSym) then
      Result := TMethodSymbol(classSym.Members.FindLocal(AUnitFunc.Name)); // get symbol version of method
  end
  // regular function (not method of a class)
  else
    Result := TFuncSymbol(AProgram.Table.FindSymbol(AUnitFunc.Name));
end;

end.
