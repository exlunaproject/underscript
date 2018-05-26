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
{    The Original Code is DelphiWebScriptII source code, released      }
{    January 1, 2001                                                   }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. Portions created by Matthias Ackermann are             }
{    Copyright (C) 2000 Matthias Ackermann, Switzerland. All           }
{    Rights Reserved.                                                  }
{                                                                      }
{    Contributor(s): Willibald Krenn, Eric Grange, Michael Riepp,      }
{                    Andreas Luleich, Mark Ericksen                    }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Compiler;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, SysUtils, dws2Exprs, dws2Symbols, dws2Tokenizer, dws2Errors,
  dws2Strings, dws2Functions, dws2Stack;

type
  TCompilerOption = (coOptimize, coSymbolDictionary, coContextMap);
  TCompilerOptions = set of TCompilerOption;

  TIncludeEvent = procedure(const scriptName: string; var scriptSource: string) of
    object;

  Tdws2Filter = class;

  TConfiguration = class(TPersistent)
  private
    FCompilerOptions: TCompilerOptions;
    FConnectors: TStrings;
    FDefaultResultType: Tdws2ResultType;
    FFilter: Tdws2Filter;
    FMaxDataSize: Integer;
    FOnInclude: TIncludeEvent;
    FOwner: TComponent;
    FResultType: Tdws2ResultType;
    FScriptPaths: TStrings;
    FStackChunkSize: Integer;
    FSystemTable: TSymbolTable;
    FTimeout: Integer;
    FUnits: TStrings;
  protected
    procedure InitSystemTable;
    procedure SetResultType(const Value: Tdws2ResultType);
    procedure SetFilter(const Value: Tdws2Filter);
  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetScriptPaths(const Value: TStrings);
    property Connectors: TStrings read FConnectors write FConnectors;
    property OnInclude: TIncludeEvent read FOnInclude write FOnInclude;
    property SystemTable: TSymbolTable read FSystemTable write FSystemTable;
    property Units: TStrings read FUnits write FUnits;
  published
    property Filter: Tdws2Filter read FFilter write SetFilter;
    property ResultType: Tdws2ResultType read FResultType write SetResultType;
    property CompilerOptions: TCompilerOptions read FCompilerOptions write FCompilerOptions;
    property MaxDataSize: Integer read FMaxDataSize write FMaxDataSize;
    property ScriptPaths: TStrings read FScriptPaths write SetScriptPaths;
    property Timeout: Integer read FTimeout write FTimeout;
    property StackChunkSize: Integer read FStackChunkSize write FStackChunkSize default C_DefaultStackChunkSize;
  end;

  Tdws2Filter = class(TComponent)
  private
    FSubFilter: Tdws2Filter;
    FDependencies: TStrings;
    FPrivateDependencies: TStrings;
    function GetDependencies: TStrings;
  protected
    procedure SetSubFilter(const Filter: Tdws2Filter); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property PrivateDependencies: TStrings read FPrivateDependencies;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Process(const Text: string; Msgs: TMsgs): string; virtual;
    property SubFilter: Tdws2Filter read FSubFilter write SetSubFilter;
    property Dependencies: TStrings read GetDependencies;
  end;

  TAddArgProcedure = procedure(ArgExpr: TExpr) of object;

  TSpecialKind = (skLength, skLow, skHigh, skSizeOf);

  Tdws2Compiler = class
  private
    FCompilerOptions: TCompilerOptions;
    FConnectors: TStrings;
    FFilter: Tdws2Filter;
    FMsgs: TMsgs;
    FOnInclude: TIncludeEvent;
    FProg: TProgram;
    FScriptPaths: TStrings;
    FTok: TTokenizer;
    FIsExcept: Boolean;
    function CheckFuncParams(ParamsA, ParamsB: TSymbolTable; IndexSym: TSymbol = nil;
      TypSym: TSymbol = nil): Boolean;
    procedure CheckName(Name: string);
    function CheckParams(A, B: TSymbolTable; CheckNames: Boolean): Boolean;
    procedure CompareFuncSymbols(A, B: TFuncSymbol; IsCheckingParameters: Boolean);
    function FindScriptPathForFile(const FileName: string): string;
    function GetScriptSource(ScriptName: string): string;
    function GetVarExpr(dataSym: TDataSymbol): TDataExpr;
    function GetVarParamExpr(dataSym: TVarParamSymbol): TExpr;
    function ReadAssign(Left: TExpr): TExpr;
    function ReadArray(TypeName: string): TTypeSymbol;
    function ReadArrayConstant: TExpr;
    function ReadCase: TCaseExpr;
    function ReadClass(TypeName: string): TTypeSymbol;
    function ReadConnectorSym(Name: string; var BaseExpr: TExpr; ConnectorType:
      IConnectorType; IsWrite: Boolean): TExpr;
    function ReadConnectorArray(Name: String; var BaseExpr: TExpr; ConnectorType: IConnectorType;
      IsWrite: Boolean): TExpr;
    procedure ReadConstDecl;
    function ReadConstValue: TExpr;
    function ReadBlock: TExpr;
    function ReadBlocks(EndTokens: TTokenTypes; var FinalToken: TTokenType): TExpr;
    function ReadEnumeration(TypeName: string): TEnumerationSymbol;
    function ReadExcept(TryExpr: TExpr): TExceptExpr;
    function ReadExpr: TExpr;
    function ReadExprAdd: TExpr;
    function ReadExprMult: TExpr;
    function ReadExternalVar(Sym: TExternalVarSymbol; IsWrite: Boolean): TFuncExpr;
    function ReadField(var Expr: TDataExpr; Sym: TFieldSymbol): TExpr;
    function ReadFor: TExpr;
    function ReadFunc(FuncSym: TFuncSymbol; IsWrite: Boolean; CodeExpr: TDataExpr = nil): TExpr;
    procedure ReadFuncArgs(AddArgProc: TAddArgProcedure; LDelim: TTokenType = ttBLEFT; RDelim: TTokenType = ttBRIGHT);
    function ReadIf: TIfExpr;
    function ReadInherited(IsWrite: Boolean): TExpr;
    function ReadInstr: TExpr;
    function ReadInstrSwitch: TExpr;
    function ReadMethodDecl(ClassSym: TClassSymbol; FuncKind: TFuncKind;
      IsClassMethod: Boolean): TMethodSymbol;
    function ReadMethodImpl(ClassSym: TClassSymbol; FuncKind: TFuncKind;
      IsClassMethod: Boolean): TMethodSymbol;
    function ReadName(IsWrite: Boolean = False): TExpr;
    // Created overloaded ReadNameList to deal with script positions
    procedure ReadNameList(Names: TStrings); overload;
    procedure ReadNameList(Names: TStrings; out PosArray: TScriptPosArray); overload;
    procedure ReadArrayParams(ArrayIndices: TSymbolTable);
    // Don't want to add param symbols to dictionary when a method implementation (they get thrown away)
    procedure ReadParams(Proc: TFuncSymbol; ParamsToDictionary: Boolean=True);
    function ReadProcDecl(FuncKind: TFuncKind; ClassSym: TClassSymbol;
      IsClassMethod: Boolean = False; IsType : Boolean = False): TFuncSymbol;
    procedure ReadProcBody(Proc: TFuncSymbol);
    function ReadProperty(ClassSym: TClassSymbol): TPropertySymbol;
    function ReadPropertyExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol; IsWrite: Boolean): TExpr;
    function ReadRecord(TypeName: string): TTypeSymbol;
    function ReadRaise: TExpr;
    function ReadRepeat: TExpr;
    function ReadRootStatement: TExpr;
    function ReadScript(AName: string=''; ScriptType: TScriptSourceType=stMain): TExpr;  // AName might be the name of an INCLUDEd script
    function ReadSpecialFunction(NamePos: TScriptPos; SpecialKind: TSpecialKind): TExpr;
    function ReadStatement: TExpr;
    function ReadStringArray(Expr: TExpr; IsWrite: Boolean): TExpr;
    function ReadSwitch(SwitchName: string): Boolean;
    function ReadSymbol(Expr: TExpr; IsWrite: Boolean = False): TExpr;
    function ReadTerm: TExpr;
    function ReadTry: TExpr;
    function ReadType(TypeName: string = ''): TTypeSymbol;
    function ReadTypeCast(NamePos: TScriptPos; TypeSym: TSymbol): TExpr;
    procedure ReadTypeDecl;
    procedure ReadUses;
    function ReadVarDecl: TExpr;
    function ReadWhile: TExpr;
    function ResolveUnitReferences(Units: TStrings): TInterfaceList;
  protected
    function CreateProgram(SystemTable: TSymbolTable; ResultType: Tdws2ResultType;
      MaxDataSize: Integer; StackChunkSize: Integer): TProgram; virtual;
    function CreateProcedure(Parent : TProgram) : TProcedure; virtual;
  public
    function Compile(Text: string; Conf: TConfiguration): TProgram;
    class function Evaluate(AContext: TProgram; AExpression: string): TExpr;
  end;

  Tdws2DefaultResult = class(Tdws2Result)
  private
    FText: string;
    function GetText: string;
  public
    procedure AddString(const Str: string);
    property Text: string read GetText;
  end;


  Tdws2DefaultResultType = class(Tdws2ResultType)
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); override;
    function CreateProgResult: Tdws2Result; override;
  end;

  TPrintFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TPrintLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

implementation

uses Contnrs;

function GetBaseType(Sym : TSymbol) : TTypeSymbol;
begin
  if Assigned(Sym) then
    Result := Sym.BaseType
  else
    Result := nil;
end;

type
  TExceptionCreateMethod = class(TInternalMethod)
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TDelphiExceptionCreateMethod = class(TInternalMethod)
    procedure Execute(var ExternalObject: TObject); override;
  end;

  TParamFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TParamStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TParamCountFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

function Tdws2Compiler.ResolveUnitReferences(Units: TStrings): TInterfaceList;
var
  x, y, z: Integer;
  deps: TStrings;
  refCount: array of Integer;
  changed: Boolean;
  unitName: string;
begin
  // initialize reference count vector
  SetLength(refCount, Units.Count);

  // Calculate number of outgoing references
  for x := 0 to Units.Count - 1 do
  begin
    deps := IUnit(Pointer(Units.Objects[x])).GetDependencies;
    for y := 0 to deps.Count - 1 do
    begin
      if Units.IndexOf(deps[y]) < 0 then
        FMsgs.AddCompilerStop(NullPos, Format(CPE_UnitNotFound, [deps[y],
          Units[x]]));
    end;
    refCount[x] := deps.Count;
  end;

  Result := TInterfaceList.Create;
  try

    // Resolve references
    changed := True;
    while changed do
    begin
      changed := False;
      for x := 0 to Units.Count - 1 do
        // Find unit that is not referencing other units
        if refCount[x] = 0 then
        begin
          Result.Add(IUnit(Pointer(Units.Objects[x])));

          // Remove the references to this unit from all other units
          unitName := Units[x];
          for y := 0 to Units.Count - 1 do
          begin
            deps := IUnit(Pointer(Units.Objects[y])).GetDependencies;
            for z := 0 to deps.Count - 1 do
              if SameText(deps[z], unitName) then
                Dec(refCount[y]);
          end;

          refCount[x] := -1;
          changed := True;
        end;
    end;

    if Result.Count <> Units.Count then
      FMsgs.AddCompilerStop(NullPos, CPE_UnitCircularReference);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.Compile(Text: string; Conf: TConfiguration): TProgram;
var
  x: Integer;
  stackChunkSize: Integer;
  maxDataSize: Integer;
  unitsResolved: TInterfaceList;
  unitsTable: TSymbolTable;
  unitTables: TObjectList;
  unitTable: TSymbolTable;
  codeCompleteInfo: TClassCompleteErrorInfo;
begin
  FIsExcept := False;
  FFilter := Conf.Filter;
  FConnectors := Conf.Connectors;
  FCompilerOptions := Conf.CompilerOptions;
  FOnInclude := Conf.OnInclude;
  FScriptPaths := Conf.ScriptPaths;

  maxDataSize := Conf.MaxDataSize;
  if maxDataSize = 0 then
    maxDataSize := MaxInt;

  StackChunkSize := Conf.StackChunkSize;
  if StackChunkSize <= 0 then
    StackChunkSize := 1;

  // Create the TProgram
  FProg := CreateProgram(Conf.SystemTable, Conf.ResultType, maxDataSize, stackChunkSize);
  Result := FProg;
  FMsgs := FProg.Msgs;

  FProg.Timeout := Conf.Timeout;

  try
    // Check for missing units
    if Assigned(FFilter) then
      for x := 0 to FFilter.Dependencies.Count - 1 do
      begin
        if Conf.Units.IndexOf(FFilter.Dependencies[x]) = -1 then
          FMsgs.AddCompilerError(Format(CPE_FilterDependsOnUnit,[FFilter.ClassName, FFilter.Dependencies[x]]));
      end;

    // Handle unit dependencies
    unitsResolved := ResolveUnitReferences(Conf.Units);
    try
      unitTables := TObjectList.Create(False);
      unitsTable := TSymbolTable.Create;
      try
        try
          // Get the symboltables of the units
          for x := 0 to unitsResolved.Count - 1 do
          begin
            unitTable := IUnit(unitsResolved[x]).GetUnitTable(Conf.SystemTable, unitsTable);
            unitTables.Add(unitTable);
            unitsTable.AddSymbol(TUnitSymbol.Create(IUnit(unitsResolved[x]).GetUnitName,unitTable));
          end;
        except
          on e: Exception do
          begin
            unitTables.OwnsObjects := True;
            raise;
          end;
        end;

        // Add the units to the program-symboltable
        for x := 0 to unitsTable.Count - 1 do
        begin
          FProg.Table.AddSymbol(TUnitSymbol.Create(
            TUnitSymbol(unitsTable[x]).Name,
            TUnitSymbol(unitsTable[x]).Table,
            True));
          FProg.Table.AddParent(TUnitSymbol(unitsTable[x]).Table);
        end;

      finally
        unitsTable.Free;
        unitTables.Free;
      end;

    finally
      unitsResolved.Free;
    end;

    // Filter stuff
    if Assigned(FFilter) then
      Text := FFilter.Process(Text, FMsgs);

    // Initialize tokenizer
    FTok := TTokenizer.Create(Text, MSG_MainModule, FProg.Msgs);
    try
      FTok.SwitchHandler := ReadSwitch;

      // Start compilation
      FProg.Expr := ReadScript('', stMain);

      // Do some optimizations
      if coOptimize in FCompilerOptions then
        FProg.Expr := FProg.Expr.Optimize;

      // Initialize symbol table
      FProg.Table.Initialize;

      // Initialize the expressions
      FProg.Expr.Initialize;

      // Every thing is done, set program state to "prepared"
      FProg.ReadyToRun;
    finally
      FTok.Free;
    end;
  except
    on e: EScriptError do
      ;
    on e: EClassMethodImplIncompleteError do
      begin
        with codeCompleteInfo do
        begin
          if e.ClassSymObj is TClassSymbol then
            ErrorClass := TClassSymbol(e.ClassSymObj)
          else
            ErrorClass := nil;
          ErrorType := cceMethodImplMissing;
          SuggestedFix := '';
        end;
        FProg.AddClassCompleteInfo(codeCompleteInfo);
        FMsgs.AddCompilerError(e.Message);
      end;
    on e: Exception do
      FMsgs.AddCompilerError(e.Message);
  end;
end;

function Tdws2Compiler.ReadScript(AName: string; ScriptType: TScriptSourceType): TExpr;
var
  Stmt: TExpr;
begin
  Result := TBlockExpr.Create(FProg, FTok.DefaultPos);
  try
    FProg.SourceList.Add(AName, FTok.HotPos.SourceFile, ScriptType);
    while FTok.HasTokens do
    begin
      Stmt := ReadRootStatement;
      if Assigned(Stmt) then
        TBlockExpr(Result).AddStatement(Stmt);

      if not FTok.TestDelete(ttSEMI) then
      begin
        if FTok.HasTokens then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadRootStatement: TExpr;
begin
  Result := nil;
  if FTok.TestDelete(ttTYPE) then
    ReadTypeDecl
  else if FTok.TestDelete(ttPROCEDURE) then
    ReadProcBody(ReadProcDecl(fkProcedure, nil))
  else if FTok.TestDelete(ttFUNCTION) then
    ReadProcBody(ReadProcDecl(fkFunction, nil))
  else if FTok.TestDelete(ttCONSTRUCTOR) then
    ReadProcBody(ReadProcDecl(fkConstructor, nil))
  else if FTok.TestDelete(ttDESTRUCTOR) then
    ReadProcBody(ReadProcDecl(fkDestructor, nil))
  else if FTok.TestDelete(ttCLASS) then
  begin
    if FTok.TestDelete(ttPROCEDURE) then
      ReadProcBody(ReadProcDecl(fkProcedure, nil, True))
    else if FTok.TestDelete(ttFUNCTION) then
      ReadProcBody(ReadProcDecl(fkFunction, nil, True))
    else
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
  end
  else
    Result := ReadStatement;
end;

function Tdws2Compiler.ReadStatement: TExpr;
begin
  Result := nil;
  if FTok.TestDelete(ttVAR) then
    Result := ReadVarDecl
  else if FTok.TestDelete(ttCONST) then
    ReadConstDecl
  else if FTok.TestDelete(ttUSES) then
    ReadUses
  else
    Result := ReadBlock;
end;

class function Tdws2Compiler.Evaluate(AContext: TProgram; AExpression: string): TExpr;
var
  OldProgMsgs: TMsgs;
begin
  { This will evaluate an expression by tokenizing it evaluating it in the
    Context provided. }

  Result := nil;

  with Self.Create do
    try
      FProg := AContext;
      try
        OldProgMsgs := FProg.Msgs;

        FMsgs := TMsgs.Create;
        FProg.Msgs := FMsgs;
        try
          FTok := TTokenizer.Create(AExpression, MSG_MainModule, FMsgs);
          try
            try
              Result := ReadExpr;
              try
                Result.Initialize;
              except
                FreeAndNil(Result);
                raise;
              end;
            except
              on E: EScriptError do
              begin
                if FMsgs.Count > 0 then
                begin
                  E.Message := FMsgs[0].AsString;
                  raise;    // change the message and re-raise the EScriptError exception
                end;
              end;
            end;
          finally
            FreeAndNil(FTok);
          end;
        finally
          FProg.Msgs := OldProgMsgs;
          FreeAndNil(FMsgs);
        end;
      finally
        FProg := nil;
      end;
    finally
      Free;
    end;
end;

function Tdws2Compiler.ReadVarDecl: TExpr;
var
  x: Integer;
  names: TStringList;
  sym, typ: TSymbol;
  pos: TScriptPos;
  posArray: TScriptPosArray;
  vars: TList;
  initData: TData;
  initExpr: TExpr;
begin
  Result := nil;

  names := TStringList.Create;
  vars := TList.Create;
  initExpr := nil;
  try
    // Conditionally pass in dynamic array
    if coSymbolDictionary in FCompilerOptions then
      ReadNameList(names, posArray)     // use overloaded version
    else
      ReadNameList(names);

    if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

    pos := FTok.HotPos;
    typ := ReadType('');

    for x := 0 to names.Count - 1 do
    begin
      CheckName(names[x]);
      sym := TDataSymbol.Create(names[x], typ);
      vars.Add(sym);
      FProg.Table.AddSymbol(sym);
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(sym, posArray[x], [suDeclaration]);   // entry for variable
    end;

    if names.Count = 1 then
    begin
      if FTok.TestDelete(ttEQ) then
        initExpr := ReadExpr
    end;

    // Create variable initializations
    for x := 0 to vars.Count - 1 do
    begin
      sym := vars[x];

      if Assigned(initExpr) then
      begin
        // Initialize with an expression
        Result :=
          TAssignExpr.Create(FProg, pos,
          GetVarExpr(vars[x]),
          initExpr);
        initExpr := nil;

        try
          Result.TypeCheck;
        except
          Result.Free;
          raise;
        end;
      end
      else
      begin
        if sym.Typ is TArraySymbol then
        begin // TODO: if Sym.DynamicInit?
          TBlockExpr(FProg.InitExpr).AddStatement(
            TInitDataExpr.Create(FProg, Pos, GetVarExpr(vars[x]) as TDataExpr));
        end
        else
        begin
          // Initialize with default value
          initData := nil;
          SetLength(initData, sym.Typ.Size);
          TDataSymbol(sym).initData(initData, 0);

          TBlockExpr(FProg.InitExpr).AddStatement(
            TAssignDataExpr.Create(FProg, pos,
            GetVarExpr(vars[x]),
            TConstExpr.Create(FProg, pos, sym.Typ, initData)))
        end;
      end;
    end;
  finally
    initExpr.Free;
    names.Free;
    vars.Free;
  end;
end;

procedure Tdws2Compiler.ReadConstDecl;
var
  Name: string;
  Expr: TExpr;
  Typ: TSymbol;
  constPos: TScriptPos;
  sym: TSymbol;
begin
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected)
  else
  begin
    Name := FTok.GetToken.FString;
    constPos := FTok.HotPos;
    FTok.KillToken;

    CheckName(Name);

    if FTok.TestDelete(ttCOLON) then
      Typ := ReadType('')
    else
      Typ := nil;

    if Typ is TFuncSymbol then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_InvalidConstType,[Typ.Caption]));

    if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

    Expr := ReadExpr;
    try
      Expr.TypeCheck;
      Expr := Expr.Optimize;
      if not (Expr is TConstExpr) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);

      if Assigned(Typ) then
      begin
        if not Typ.IsCompatible(Expr.Typ) then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_AssignIncompatibleTypes, [Expr.Typ.Caption, Typ.Caption]));
      end
      else
        Typ := Expr.Typ;

      if Typ.Size > 1 then
        sym := TConstSymbol.Create(Name, Typ, TConstExpr(Expr).Data, TConstExpr(Expr).Addr)
      else
        sym := TConstSymbol.Create(Name, Typ, Expr.Eval);
      FProg.Table.AddSymbol(sym);
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(sym, constPos, [suDeclaration]);
    finally
      Expr.Free;
    end;
  end;
end;

procedure Tdws2Compiler.ReadTypeDecl;
var
  Name: string;
  typNew, typOld: TSymbol;
  typePos: TScriptPos;
  oldSymPos: TSymbolPosition; // Mark *where* the old declaration was
begin
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected)
  else
  begin
    Name := FTok.GetToken.FString;
    typePos := FTok.HotPos;
    FTok.KillToken;

    if not FTok.TestDelete(ttEQ) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

    typOld := FProg.Table.FindSymbol(Name);
    oldSymPos := nil;
    if coSymbolDictionary in FCompilerOptions then
    begin
      if Assigned(typOld) then
        oldSymPos := FProg.SymbolDictionary.FindSymbolUsage(typOld, suDeclaration);  // may be nil
    end;

    typNew := ReadType(Name);

    // Wrap whole type declarations in a context.
    if coContextMap in FCompilerOptions then
      FProg.ContextMap.OpenContext(typePos, typNew);

    try
      try
        // typOld = typNew if a forwarded class declaration was overwritten
        if typOld <> typNew then
        begin
          CheckName(Name);
          FProg.Table.AddSymbol(typNew);
        end
        // Handle overwriting forwards in Dictionary
        // Original symbol was a forward. Update symbol entry
        else
        begin
          // If the type is in the SymbolDictionary (disabled dictionary would leave pointer nil),
          if Assigned(oldSymPos) then              // update original position information
            oldSymPos.SymbolUsages := [suForward]; // update old postion to reflect that the type was forwarded
        end;

        // Add symbol position as being the type being declared (works for forwards too)
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(typNew, typePos, [suDeclaration]);
      except
        typNew.Free;
        raise;
      end;
    finally
      if coContextMap in FCompilerOptions then
        FProg.ContextMap.CloseContext(FTok.CurrentPos);
    end;
  end;
end;

function Tdws2Compiler.ReadProcDecl(FuncKind: TFuncKind; ClassSym: TClassSymbol;
  IsClassMethod: Boolean; IsType : Boolean): TFuncSymbol;
var
  Name: string;
  sym: TSymbol;
  funcPos: TScriptPos;
  forwardedSym: TFuncSymbol;
  forwardedSymPos: TSymbolPosition;
  methSym: TMethodSymbol;
  i: Integer;
begin

  if not IsType then
  begin
    // Find Symbol for Functionname
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    Name := FTok.GetToken.FString;
    funcPos := FTok.HotPos;
    FTok.KillToken;

    sym := FProg.Table.FindSymbol(Name);

    // Open context for procedure declaration. Closed in ReadProcBody.
    if coContextMap in FCompilerOptions then
      FProg.ContextMap.OpenContext(funcPos, sym);
  end
  else begin
    sym := nil;
    Name := '';
  end;

  // Name is the name of class -> Method
  if sym is TClassSymbol then
  begin
    // Store reference to class in dictionary
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(sym, funcPos);
    Result := ReadMethodImpl(TClassSymbol(sym), FuncKind, IsClassMethod);
  end
  else
  begin
    // Read normal procedure/function declaration
    if IsClassMethod or (FuncKind = fkConstructor) or (FuncKind = fkDestructor) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ImplClassNameExpected);

    if (sym is TFuncSymbol) and TFuncSymbol(sym).IsForwarded then
      // There was already a (forward) declaration
      forwardedSym := TFuncSymbol(sym)
    else
      forwardedSym := nil;

    if not Assigned(forwardedSym) then
      CheckName(Name);

    if IsType then
      Result := TFuncSymbol.Create('', FuncKind, -1)
    else
      Result := TFuncSymbol.Create(Name, FuncKind,
        FProg.Stack.NextLevel(FProg.Level));
    try
      ReadParams(Result, forwardedSym=nil);  // Don't add params to dictionary when function is forwarded. It is already declared.

      if FuncKind = fkFunction then
      begin
        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionTypeExpected);
        Result.Typ := ReadType('');
      end;

      if not IsType then
      begin
        if Assigned(forwardedSym) then
          CompareFuncSymbols(forwardedSym, Result, True);

        // forward declarations
        if not Assigned(forwardedSym) then
        begin
          if FTok.Test(ttSEMI) then
          begin
            FTok.KillToken; // SEMI
            if FTok.Test(ttFORWARD) then
            begin
              Result.IsForwarded := True;
              FTok.TestDelete(ttFORWARD);
            end;
          end;
        end
        else if not FTok.TestDelete(ttSEMI) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);


        if Assigned(forwardedSym) then
        begin
          // Get forwarded position in script. If compiled without symbols it will just return from empty list (could optimize here to prevent the push/pop of call stack
          forwardedSymPos := FProg.SymbolDictionary.FindSymbolUsage(forwardedSym, suDeclaration);  // may be nil
          // Adapt dictionary entry to reflect that it was a forward
          // If the record is in the SymbolDictionary (disabled dictionary would leave pointer nil)
          if Assigned(forwardedSymPos) then
            forwardedSymPos.SymbolUsages := [suForward];  // update old postion to reflect that the type was forwarded

          Result.Free;
          Result := forwardedSym;
          Result.IsForwarded := False;
        end
        else
          FProg.Table.AddSymbol(Result);
      end
      else if FTok.TestDelete(ttOF) then
      begin
        if not FTok.TestDelete(ttOBJECT) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectExpected);
        methSym := TMethodSymbol.Create('',FuncKind, FProg.TypObject,-1);
        methSym.Typ := Result.Typ;
        for i := 0 to Result.Params.Count - 1 do
          methSym.Params.AddSymbol(Result.Params[i]);
        Result.Params.Clear;
        Result.Free;
        Result := methSym;
      end;

      // Procedure is both Declared and Implemented here
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(Result, funcPos, [suDeclaration, suImplementation]);
    except
      // Remove reference to symbol (gets freed)
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Remove(Result);
      Result.Free;
      raise;
    end;
  end;
end;

function Tdws2Compiler.ReadMethodDecl(ClassSym: TClassSymbol; FuncKind: TFuncKind;
  IsClassMethod: Boolean): TMethodSymbol;

  function ParamsCheck(newMeth, oldMeth: TMethodSymbol): Boolean;
  var
    x: Integer;
  begin
    Result := False;
    if newMeth.Params.Count = oldMeth.Params.Count then
    begin
      for x := 0 to newMeth.Params.Count - 1 do
        if not newMeth.Params[x].Typ.IsCompatible(oldMeth.Params[x].Typ) then
          exit;
      Result := True;
    end;
  end;
var
  Name: string;
  meth: TSymbol;
  IsReintroduced: Boolean;
  methPos: TScriptPos;
begin
  // Find Symbol for Functionname
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
  Name := FTok.GetToken.FString;
  FTok.KillToken;

  methPos := FTok.HotPos;

  // Check if name is already used
  meth := ClassSym.Members.FindSymbol(Name);
  if meth is TFieldSymbol then
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_FieldRedefined, [Name]))
  else if meth is TPropertySymbol then
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_PropertyRedefined, [Name]))
  else if meth is TMethodSymbol then
  begin
    if TMethodSymbol(meth).ClassSymbol = ClassSym then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_MethodRedefined, [Name]));
  end;

  // Read declaration of method implementation
  if IsClassMethod then
    Result := TMethodSymbol.Create(Name, FuncKind, ClassSym.ClassOf)
  else
    Result := TMethodSymbol.Create(Name, FuncKind, ClassSym);

  try
    if meth is TMethodSymbol then
    begin
      Result.SetOverlap(TMethodSymbol(meth));
      IsReintroduced := TMethodSymbol(meth).IsVirtual;
    end
    else
      IsReintroduced := False;

    ReadParams(Result);

    if FuncKind = fkFunction then
    begin
      if not FTok.TestDelete(ttCOLON) then
        FMsgs.AddCompilerStop(methPos, CPE_FunctionTypeExpected);
      Result.Typ := ReadType('');
    end;
{
    else if FuncKind = fkConstructor then
      Result.Typ := ClassSym;
}
    if not FTok.TestDelete(ttSEMI) then
      FMsgs.AddCompilerStop(methPos, CPE_SemiExpected);

    if FTok.Test(ttVIRTUAL) or FTok.Test(ttOVERRIDE) or FTok.Test(ttREINTRODUCE) then
    begin
      if FTok.TestDelete(ttVIRTUAL) then
      begin
        TMethodSymbol(Result).IsVirtual := True;
        if FTok.Test(ttSEMI) and FTok.NextTest(ttABSTRACT) then
        begin
          FTok.KillToken;
          FTok.TestDelete(ttABSTRACT);
          TMethodSymbol(Result).IsAbstract := True;
        end;
      end
      else if FTok.TestDelete(ttOVERRIDE) then
      begin
        if not Assigned(meth) or not (meth is TMethodSymbol) then
          FMsgs.AddCompilerStop(methPos, Format(CPE_CantOverrideNotInherited, [Name]))
        else if not TMethodSymbol(meth).IsVirtual then
          FMsgs.AddCompilerStop(methPos, Format(CPE_CantOverrideNotVirtual, [Name]))
        else
        begin
          if not ParamsCheck(TMethodSymbol(Result), TMethodSymbol(meth)) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_CantOverrideWrongParameterList);
          TMethodSymbol(Result).SetOverride(TMethodSymbol(meth));
          IsReintroduced := False;
        end;
      end
      else if FTok.TestDelete(ttREINTRODUCE) then
      begin
        if not IsReintroduced then
          FMsgs.AddCompilerStop(methPos, Format(CPE_CantReintroduce, [Name]));
        IsReintroduced := False;
      end;

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
    end;

    if IsReintroduced then
      FMsgs.AddCompilerWarning(methPos, Format(CPE_ReintroduceWarning, [Name]));

    // Added as last step. OnExcept, won't need to be freed.
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(Result, methPos, [suDeclaration]);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadMethodImpl(ClassSym: TClassSymbol;
  FuncKind: TFuncKind; IsClassMethod: Boolean): TMethodSymbol;
var
  methName: string;
  meth: TSymbol;
  methPos: TScriptPos;
begin
  if not (FTok.TestDelete(ttDOT) and FTok.TestName) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

  methName := FTok.GetToken.FString;
  methPos := FTok.HotPos;
  FTok.KillToken;
  FTok.Test(ttBLEFT);

  meth := ClassSym.Members.FindSymbol(methName);

  if not (meth is TMethodSymbol) then
    FMsgs.AddCompilerStop(methPos, CPE_ImplNotAMethod);

  if TMethodSymbol(meth).ClassSymbol <> ClassSym then
    FMsgs.AddCompilerStop(methPos, Format(CPE_ImplInvalidClass, [methName,
      ClassSym.Name]));

  if TMethodSymbol(meth).IsAbstract then
    FMsgs.AddCompilerError(methPos, Format(CPE_ImplAbstract, [ClassSym.Name, methName]));

  if TMethodSymbol(meth).IsClassMethod and not IsClassMethod then
    FMsgs.AddCompilerStop(methPos, CPE_ImplClassExpected)
  else if not TMethodSymbol(meth).IsClassMethod and IsClassMethod then
    FMsgs.AddCompilerStop(methPos, CPE_ImplNotClassExpected);

  Result := TMethodSymbol.Create(methName, FuncKind, ClassSym);
  try
    if not FTok.TestDelete(ttSEMI) then
    begin
      ReadParams(Result, False);  // Don't store these params to Dictionary. They will become invalid when the method is freed.

      if FuncKind = fkFunction then
      begin
        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionTypeExpected);
        Result.Typ := ReadType('');
      end;

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

      CompareFuncSymbols(TMethodSymbol(meth), Result, True);
    end
    else
      CompareFuncSymbols(TMethodSymbol(meth), Result, False);
  finally
    Result.Free;
    Result := TMethodSymbol(meth);
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(Result, methPos, [suImplementation]);
  end;
end;

procedure Tdws2Compiler.ReadProcBody(Proc: TFuncSymbol);
var
  x: Integer;
  oldprog: TProgram;
  stmt: TExpr;
  names: TStringList;
  typ: TSymbol;
  dataSym: TDataSymbol;
  initData: TData;
  pos: TScriptPos;
  posArray: TScriptPosArray;
begin
  // Stop if declaration was forwarded or external
  if (TFuncSymbol(Proc).IsForwarded) then
  begin
    // Closed context of procedure (was only a forward)
    if coContextMap in FCompilerOptions then
      FProg.ContextMap.CloseContext(FTok.HotPos);
    Exit;
  end;

  // Open context of full procedure body (may include a 'var' section)
  if coContextMap in FCompilerOptions then
    FProg.ContextMap.OpenContext(FTok.CurrentPos, Proc);   // attach to symbol that it belongs to (perhaps a class)

  try
    // Funktion Body
    oldprog := FProg;
    FProg := CreateProcedure(FProg);
    try
      TProcedure(FProg).AssignTo(Proc);
      // Set the current context's LocalTable to be the table of the new procedure
      if coContextMap in FCompilerOptions then
        FProg.ContextMap.Current.LocalTable := FProg.Table;

      // Read local variable declarations
      if FTok.TestDelete(ttVAR) then
      begin
        names := TStringList.Create;
        try
          // Read names of local variable
          repeat
            // Track Procedure local variables positions
            if coSymbolDictionary in FCompilerOptions then
              ReadNameList(names, posArray)
            else
              ReadNameList(names);
            if not FTok.TestDelete(ttCOLON) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

            // Read type of local variables
            pos := FTok.HotPos;
            typ := ReadType('');
            for x := 0 to names.Count - 1 do
            begin
              CheckName(names[x]);
              dataSym := TDataSymbol.Create(names[x], typ);
              FProg.Table.AddSymbol(dataSym);
              // Add local proc variable declarations
              if coSymbolDictionary in FCompilerOptions then
                FProg.SymbolDictionary.Add(dataSym, posArray[x], [suDeclaration]);

              // Initialize with default value
              initData := nil;
              SetLength(initData, typ.Size);
              dataSym.initData(initData, 0);

              TBlockExpr(FProg.InitExpr).AddStatement(
                TAssignDataExpr.Create(FProg, pos,
                GetVarExpr(dataSym),
                TConstExpr.Create(FProg, pos, typ, initData)))
            end;

            if not FTok.TestDelete(ttSEMI) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

          until FTok.Test(ttBEGIN);
        finally
          names.Free;
        end;
      end;

      if coContextMap in FCompilerOptions then
        FProg.ContextMap.OpenContext(FTok.CurrentPos, nil);
      try
        // Read procedure body
        if not FTok.TestDelete(ttBEGIN) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_BeginExpected);

        // Read Statements enclosed in "begin" and "end"
        FProg.Expr := TBlockExpr.Create(FProg, FTok.HotPos);
        while not FTok.TestDelete(ttEND) do
        begin
          stmt := ReadRootStatement;
          if Assigned(stmt) then
            TBlockExpr(FProg.Expr).AddStatement(Stmt);
          if not FTok.TestDelete(ttSEMI) then
          begin
            if not FTok.Test(ttEND) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
          end;
        end;
      finally
        if coContextMap in FCompilerOptions then
          FProg.ContextMap.CloseContext(FTok.HotPos);  // close with inside procedure end
      end;

      // Optimize procedure
      // ALu: TODO Optimize fails if procs called inside only declared and not defined yet  
      if coOptimize in FCompilerOptions then
        FProg.Expr := FProg.Expr.Optimize;

    finally
      FProg := oldprog;
    end;
  finally
    // Closed procedure body and procedure implementation (from declaration to body)
    if coContextMap in FCompilerOptions then
    begin
      FProg.ContextMap.CloseContext(FTok.CurrentPos);  // closed begin..end body (may include 'var' section)
      FProg.ContextMap.CloseContext(FTok.CurrentPos);  // closed from declaration through implementation
    end;
  end;
end;

function Tdws2Compiler.ReadBlocks(EndTokens: TTokenTypes; var FinalToken: TTokenType): TExpr;
var
  blk: TExpr;
  oldTable: TSymbolTable;
  x: Integer;
  closePos: TScriptPos; // Position at which the ending token was found (for context)
begin

  // Read a block of instructions enclosed in "begin" and "end"
  Result := TBlockExpr.Create(FProg, FTok.HotPos);
  try
    if coContextMap in FCompilerOptions then
    begin
      FProg.ContextMap.OpenContext(FTok.CurrentPos, nil);
      closePos := FTok.CurrentPos;     // default to close context where it openned (used on errors)
    end;
    oldTable := FProg.Table;
    FProg.Table := TBlockExpr(Result).Table;
    try
      // Add local table to context for the new block
      if coContextMap in FCompilerOptions then
        FProg.ContextMap.Current.LocalTable := FProg.Table;

      while True do
      begin

        if FTok.HasTokens then
        begin
          if FTok.GetToken.FTyp in EndTokens then
          begin
            FinalToken := FTok.GetToken.FTyp;
            closePos := FTok.GetToken.FPos;    // get start position of ending token
            FTok.KillToken;
            exit;
          end;
        end
        else
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndOfBlockExpected);

        blk := ReadStatement;
        if Assigned(blk) then
          TBlockExpr(Result).AddStatement(blk);
        if not FTok.TestDelete(ttSEMI) then
        begin
          if not (FTok.GetToken.FTyp in EndTokens) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
        end;
      end;
    finally
      FProg.Table := oldTable;
      if coContextMap in FCompilerOptions then
        FProg.ContextMap.CloseContext(closePos);   // get to end of block
    end;
  except
    // Remove any symbols in the expression's table. Table will be freed.
    if coSymbolDictionary in FCompilerOptions then
      for x := 0 to TBlockExpr(Result).Table.Count - 1 do
        FProg.SymbolDictionary.Remove(TBlockExpr(Result).Table[x]);
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadBlock: TExpr;
var
  tt: TTokenType;
begin
  Result := nil;
  if FTok.TestDelete(ttBEGIN) then
    Result := ReadBlocks([ttEND], tt)
  else if FTok.HasTokens then
    // Read a single instruction
    Result := ReadInstr;
end;

function Tdws2Compiler.ReadInstr: TExpr;
begin
  // Decide which instruction to read
  if FTok.TestDelete(ttIF) then
    Result := ReadIf
  else if FTok.TestDelete(ttCASE) then
    Result := ReadCase
  else if FTok.TestDelete(ttFOR) then
    Result := ReadFor
  else if FTok.TestDelete(ttWHILE) then
    Result := ReadWhile
  else if FTok.TestDelete(ttREPEAT) then
    Result := ReadRepeat
  else if FTok.TestDelete(ttTRY) then
    Result := ReadTry
  else if FTok.TestDelete(ttRAISE) then
    Result := ReadRaise
  else if FTok.TestDelete(ttBREAK) then
    Result := TBreakExpr.Create(FProg, FTok.HotPos)
  else if FTok.TestDelete(ttEXIT) then
    Result := TExitExpr.Create(FProg, FTok.HotPos)
  else if FTok.TestDelete(ttCONTINUE) then
    Result := TContinueExpr.Create(FProg, FTok.HotPos)
  // Try to read a function call, method call or an assignment
  else if FTok.Test(ttSWITCH) then
    Result := ReadInstrSwitch
  else if FTok.Test(ttBLEFT) or FTok.Test(ttINHERITED) or FTok.TestName  then // !! TestName must be the last !!
  begin
    if FTok.Test(ttBLEFT) then // (X as TY)
      Result := ReadSymbol(ReadTerm)
    else
      Result := ReadName(True);
    try
      if FTok.TestDelete(ttASSIGN) then
      begin
        if not (Result is TDataExpr) or not TDataExpr(Result).IsWritable then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_CantWriteToLeftSide);
        Result := ReadAssign(Result);
      end
      else if not (Result is TFuncExpr)
        and not (Result is TAssignExpr)
        and not (Result is TConnectorCallExpr)
        and not (Result is TConnectorWriteExpr)
        and not (Result is TStringArraySetExpr) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_InvalidInstruction)
    except
      Result.Free;
      raise;
    end;
  end
  else
    Result := TNullExpr.Create(FProg, FTok.HotPos);

  if Assigned(Result) then
  try
    Result.TypeCheck;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadInherited(IsWrite: Boolean): TExpr;
var
  name: string;
  sym: TSymbol;
  methSym: TMethodSymbol;
  classSym, parentSym: TClassSymbol;
  varExpr: TDataExpr;
begin
  Result := nil;
  if not ((FProg is TProcedure) and (TProcedure(FProg).Func is TMethodSymbol)) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedOnlyInMethodsAllowed);

  methSym := TMethodSymbol(TProcedure(FProg).Func);
  classSym := methSym.ClassSymbol;
  parentSym := ClassSym.Parent;
  sym := nil;

  if FTok.TestName then
  begin
    name := FTok.GetToken.FString;
    FTok.KillToken;

    sym := ParentSym.Members.FindSymbol(name);
  end
  else if not methSym.IsOverride then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName)
  else
    sym := methSym.ParentMeth;

  if Assigned(sym) then
  begin
    if sym is TMethodSymbol then
    begin
      if methSym.IsClassMethod then
        varExpr := TConstExpr.Create(FProg, FTok.HotPos, parentSym.ClassOf, parentSym.Name)
      else
        varExpr := TVarExpr.Create(FProg, FTok.HotPos, parentSym, methSym.SelfSym);
      try
        if methSym.IsClassMethod then
          Result := GetMethodExpr(TMethodSymbol(sym),varExpr,rkClassOfRef,FTok.HotPos,True,True)
        else
          Result := GetMethodExpr(TMethodSymbol(sym),varExpr,rkObjRef,FTok.HotPos,True,True);
      except
        varExpr.Free;
        raise;
      end;
      try
        ReadFuncArgs(TFuncExpr(Result).AddArg);
        if TMethodSymbol(sym).Kind = fkConstructor then
          Result.Typ := methSym.ClassSymbol.Parent;
      except
        Result.Free;
        raise;
      end;
    end
    else if sym is TPropertySymbol then
    begin
      varExpr := TVarExpr.Create(FProg, FTok.HotPos, parentSym, methSym.SelfSym);
      try
        Result := ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite);
      except
        varExpr.Free;
        raise;
      end;
    end
    else
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_InheritedWithoutName);
  end
  else
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_InheritedMethodNotFound, [Name]));
end;

function Tdws2Compiler.ReadName(IsWrite: Boolean): TExpr;
type
  TSpecialMap = array[TSpecialKind] of string;
const
  specialMap: TSpecialMap = ('length', 'low', 'high', 'sizeof');
var
  name: string;
  sym: TSymbol;
  namePos: TScriptPos;
  varExpr: TDataExpr;
  progMeth: TMethodSymbol;
  baseType: TTypeSymbol;
  sk: TSpecialKind;
begin
  Result := nil;

  if FTok.TestDelete(ttINHERITED) then
  begin
    // Name with inherited
    Result := ReadInherited(IsWrite);
    try
      Result := ReadSymbol(Result, IsWrite);
    except
      Result.Free;
      raise;
    end;
  end
  else
  begin
    // Get name
    FTok.TestName;
    name := FTok.GetToken.FString;
    namePos := FTok.HotPos;
    FTok.KillToken;

    // Test for special functions
    for sk := Low(sk) to High(sk) do
      if SameText(name, specialMap[sk]) then
      begin
        Result := ReadSpecialFunction(namePos, sk);
        Break;
      end;

    if Result = nil then
    begin
      // Find name in symboltable
      sym := FProg.Table.FindSymbol(Name);
      try

        baseType := GetBaseType(sym);

        // Add the symbol usage to Dictionary
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(sym, namePos);

        // Unit prefix found
        if baseType is TUnitSymbol then
        begin
          if not FTok.TestDelete(ttDOT) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
          if not FTok.TestName then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
          name := FTok.GetToken.FString;
          namePos := FTok.HotPos;   // reuse token pos variable
          FTok.KillToken;
          sym := TUnitSymbol(baseType).Table.FindLocal(Name);
          // Already added symbol usage of the unit. Now add for the unit's specified symbol.
          if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(sym, namePos);
        end;

        if baseType is TEnumerationSymbol then
          baseType := TEnumerationSymbol(baseType).Typ.BaseType;

        if not Assigned(sym) then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownName, [name]))
        // "Variables"
        else if sym is TVarParamSymbol then
          Result := ReadSymbol(GetVarParamExpr(TVarParamSymbol(sym)), IsWrite)
        else if sym is TConstSymbol then
          Result := ReadSymbol(TConstExpr.Create(FProg, FTok.HotPos, sym.Typ,
            TConstSymbol(sym).Data), IsWrite)
        else if sym is TDataSymbol then
        begin
          if sym.Typ is TFuncSymbol then
            Result := ReadFunc(TFuncSymbol(sym.Typ), IsWrite, GetVarExpr(TDataSymbol(sym)))
          else
            Result := ReadSymbol(GetVarExpr(TDataSymbol(sym)), IsWrite);
        end
        else if sym is TExternalVarSymbol then
          Result := ReadSymbol(ReadExternalVar(TExternalVarSymbol(sym), IsWrite),
            IsWrite)
        // OOP related stuff
        else if baseType is TClassSymbol then
        begin
          if FTok.TestDelete(ttBLEFT) then
          begin
            // Cast
            Result := ReadExpr;
            Result.Typ := sym;
            if not (FTok.TestDelete(ttBRIGHT)) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
            Result := ReadSymbol(Result, IsWrite);
          end
          else
            Result := ReadSymbol(TConstExpr.Create(FProg, FTok.HotPos,
              TClassSymbol(baseType).ClassOf, baseType.Name), IsWrite)
        end
        else if sym is TFieldSymbol then
        begin
          progMeth := TMethodSymbol(TProcedure(FProg).Func);
          if progMeth.IsClassMethod then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
          varExpr := TVarExpr.Create(FProg, FTok.HotPos, progMeth.SelfSym.Typ, progMeth.SelfSym);
          try
            Result := ReadSymbol(ReadField(varExpr, TFieldSymbol(sym)), IsWrite);
          except
            varExpr.Free;
            raise;
          end;
        end
        else if sym is TPropertySymbol then
        begin
          progMeth := TMethodSymbol(TProcedure(FProg).Func);
          if progMeth.IsClassMethod then
            varExpr := TConstExpr.Create(FProg, FTok.HotPos, progMeth.ClassSymbol, nil)
          else
            varExpr := TVarExpr.Create(FProg, FTok.HotPos, progMeth.SelfSym.Typ, progMeth.SelfSym);
          try
            Result := ReadSymbol(ReadPropertyExpr(varExpr, TPropertySymbol(sym), IsWrite), IsWrite);
          except
            varExpr.Free;
            raise;
          end;
        end
        else if sym is TMethodSymbol then
        begin
          progMeth := TMethodSymbol(TProcedure(FProg).Func);
          if not progMeth.IsClassMethod then
            Result := GetMethodExpr(TMethodSymbol(sym), TVarExpr.Create(FProg,
              FTok.HotPos, progMeth.SelfSym.Typ, progMeth.SelfSym), rkObjRef, FTok.HotPos, IsWrite)
          else if (TMethodSymbol(sym).Kind = fkConstructor) or (TMethodSymbol(sym).IsClassMethod) then
            Result := GetMethodExpr(TMethodSymbol(sym), TConstExpr.Create(FProg,
              FTok.HotPos, progMeth.ClassSymbol,
              nil), rkClassOfRef, FTok.HotPos, IsWrite, True)
          else
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);

          ReadFuncArgs(TFuncExpr(Result).AddArg);
          Result := ReadSymbol(Result, IsWrite);
        end
        // Functions/Procedures
        else if sym is TFuncSymbol then
          Result := ReadSymbol(ReadFunc(TFuncSymbol(sym), IsWrite), IsWrite)
        // Type casts
        else if sym is TTypeSymbol then
          Result := ReadTypeCast(namePos, sym)
        else
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownType, [sym.Caption]));

      except
        Result.Free;
        raise;
      end;
    end;
  end;
end;

function Tdws2Compiler.ReadField(var Expr: TDataExpr; Sym: TFieldSymbol): TExpr;
begin
  Result := TFieldExpr.Create(FProg, FTok.HotPos, Sym.Typ, Sym, Expr);
end;

// Parses statements like "property[i, j, k] := expr" and "expr := property[i, j, k]"
function Tdws2Compiler.ReadPropertyExpr(var Expr: TDataExpr; PropertySym: TPropertySymbol; IsWrite: Boolean): TExpr;
var
  sym: TSymbol;
  arrayArgs: TExprList;
begin
  Result := nil;
  arrayArgs := TExprList.Create;
  try
    if PropertySym.ArrayIndices.Count > 0 then
      ReadFuncArgs(arrayArgs.AddExpr, ttALEFT, ttARIGHT);

    if IsWrite and FTok.TestDelete(ttASSIGN) then
    begin
      sym := PropertySym.WriteSym;

      // No WriteSym
      if sym = nil then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_ReadOnlyProperty)
      // WriteSym is a Field
      else if sym is TFieldSymbol then
      begin
        if Expr.Typ is TClassOfSymbol then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
        Result := TFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym),
          TDataExpr(Expr));
        Result := ReadAssign(Result);
      end
      // WriteSym is a Method
      else if sym is TMethodSymbol then
      begin
        // Convert an assignment to a function call f := x  -->  f(x)
        if Expr.Typ is TClassOfSymbol then
        begin
          // Class properties
          if not TMethodSymbol(sym).IsClassMethod then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticPropertyWriteExpected);

          Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkClassOfRef,
            Expr.Pos, True);
        end
        else
          Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkObjRef,
            Expr.Pos, True);

        try
          Expr := nil; // is part of Result

          // Add array indizes (if any)
          while arrayArgs.Count > 0 do
          begin
            TFuncExpr(Result).AddArg(arrayArgs[0]);
            arrayArgs.Delete(0);
          end;

          if Assigned(PropertySym.IndexSym) then
            TFuncExpr(Result).AddArg(TConstExpr.Create(FProg, FTok.HotPos,
              PropertySym.IndexSym, PropertySym.IndexValue));

          // Add right side of assignment
          TFuncExpr(Result).AddArg(ReadExpr);
        except
          Result.Free;
          raise;
        end;
      end;
    end
    else
    begin
      sym := PropertySym.ReadSym;

      // No ReadSym
      if sym = nil then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_WriteOnlyProperty)
      // ReadSym is a field
      else if sym is TFieldSymbol then
      begin
        if Expr.Typ is TClassSymbol then
          Result := TFieldExpr.Create(FProg, FTok.HotPos, sym.Typ, TFieldSymbol(sym),
            TDataExpr(Expr))
        else
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ObjectReferenceExpected);
      end
      // ReadSym is a method
      else if sym is TMethodSymbol then
      begin
        if Expr.Typ is TClassOfSymbol then
          Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkClassOfRef,
            Expr.Pos, False)
        else
          Result := GetMethodExpr(TMethodSymbol(sym), TDataExpr(Expr), rkObjRef,
            Expr.Pos, False);

        try
          // Add array indizes if any
          while ArrayArgs.Count > 0 do
          begin
            TFuncExpr(Result).AddArg(ArrayArgs[0]);
            ArrayArgs.Delete(0);
          end;

          if Assigned(PropertySym.IndexSym) then
            TFuncExpr(Result).AddArg(TConstExpr.Create(FProg,FTok.HotPos,PropertySym.IndexSym,PropertySym.IndexValue));
        except
          Result.Free;
          raise;
        end;

      end;
    end;

  finally
    arrayArgs.Free;
  end;
end;

function Tdws2Compiler.ReadSymbol(Expr: TExpr; IsWrite: Boolean): TExpr;

  function GetDefaultProperty(cls: TClassSymbol): TPropertySymbol;
  begin
    while Assigned(cls) and not Assigned(cls.DefaultProperty) do
      cls := cls.Parent;

    if Assigned(cls) then
      Result := cls.DefaultProperty
    else
      Result := nil;
  end;

  function ReadArrayExpr(var BaseExpr: TDataExpr): TArrayExpr;
  var
    indexExpr: TExpr;
    baseType: TTypeSymbol;
  begin
    FTok.KillToken;

    Result := nil;

    if FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

    // There is at one index expression
    repeat
      indexExpr := ReadExpr;
      baseType := BaseExpr.BaseType;

      try
        if baseType is TStaticArraySymbol then
          Result := TStaticArrayExpr.Create(FProg, FTok.HotPos, BaseExpr, indexExpr,
            TStaticArraySymbol(baseType).LowBound, TStaticArraySymbol(baseType).HighBound)
        else if baseType is TDynamicArraySymbol then
          Result := TDynamicArrayExpr.Create(FProg, FTok.HotPos, BaseExpr, indexExpr)
        else
          FMsgs.AddCompilerStop(FTok.HotPos, RTE_TooManyIndices);
      except
        indexExpr.Free;
        raise;
      end;

      BaseExpr := Result;
    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
  end;

var
  name: string;
  member: TSymbol;
  defaultProperty: TPropertySymbol;
  symPos: TScriptPos;
  baseType: TTypeSymbol;
begin
  Result := Expr;
  try
    repeat
      Expr := Result;
      baseType := Result.BaseType;

      // Member
      if FTok.TestDelete(ttDOT) then
      begin
        if FTok.TestName then
        begin
          Name := FTok.GetToken.FString;
          symPos := FTok.HotPos;
          FTok.KillToken;

          // Record
          if baseType is TRecordSymbol then
          begin
            member := TRecordSymbol(baseType).Members.FindLocal(Name);
            if coSymbolDictionary in FCompilerOptions then
              FProg.SymbolDictionary.Add(member, symPos);

            if Assigned(member) then
              Result := TRecordExpr.Create(FProg, FTok.HotPos, TDataExpr(Result), TMemberSymbol(member))
            else
              FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownMember, [Name]));
            Expr := nil;
          end
          // Class
          else if baseType is TClassSymbol then
          begin
            member := TClassSymbol(baseType).Members.FindSymbol(Name);
            if coSymbolDictionary in FCompilerOptions then
              FProg.SymbolDictionary.Add(member, symPos);

            if member is TMethodSymbol then
            begin
              // Member is a method
              if Assigned(TMethodSymbol(member).SelfSym) then
                Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result),
                  rkObjRef, Result.Pos, IsWrite)
              else
                Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result),
                  rkClassOfRef, Result.Pos, IsWrite);
              ReadFuncArgs(TFuncExpr(Result).AddArg);
            end
            else if member is TFieldSymbol then
              // Member is a field
              Result := TFieldExpr.Create(FProg, FTok.HotPos, member.Typ,
                TFieldSymbol(member), TDataExpr(Result))
            else if member is TPropertySymbol then
              // Member is a property
              Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)
            else
              FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownMember, [Name]));
          end
          // Class Of
          else if baseType is TClassOfSymbol then
          begin
            member := TClassSymbol(baseType.Typ).Members.FindSymbol(Name);
            if coSymbolDictionary in FCompilerOptions then
              FProg.SymbolDictionary.Add(member, FTok.HotPos);

            // Class method
            if member is TMethodSymbol then
            begin
              case TMethodSymbol(member).Kind of
                fkFunction, fkProcedure:
                  if not TMethodSymbol(member).IsClassMethod then
                    FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
                fkDestructor:
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
              end;
              Result := GetMethodExpr(TMethodSymbol(member), TDataExpr(Result),
                rkClassOfRef, Result.Pos, IsWrite);
              ReadFuncArgs(TFuncExpr(Result).AddArg);
            end
              // Static property
            else if member is TPropertySymbol then
              Result := ReadPropertyExpr(TDataExpr(Result), TPropertySymbol(member), IsWrite)
            else
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_StaticMethodExpected);
          end
          // Connector symbol
          else if baseType is TConnectorSymbol then
          begin
            Result := ReadConnectorSym(Name, Result,
              TConnectorSymbol(baseType).ConnectorType, IsWrite)
          end
          else
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMemberExpected);
        end
        else
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      end
      // Arrays
      else if FTok.Test(ttALEFT) then
      begin
        if Assigned(Result) then
        begin
          if baseType is TClassSymbol then
          begin
            // array property
            DefaultProperty := GetDefaultProperty(TClassSymbol(baseType));
            if Assigned(DefaultProperty) then
              Result := ReadPropertyExpr(TDataExpr(Result), DefaultProperty, IsWrite)
            else
              FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NoDefaultProperty, [Result.Typ.Name]));
          end
          else
          begin
            // Type "array"
            if baseType is TArraySymbol then
              Result := ReadArrayExpr(TDataExpr(Result))
            else if baseType is TConnectorSymbol then
              Result := ReadConnectorArray('', Result, TConnectorSymbol(baseType).ConnectorType, IsWrite)
            else
            begin
              FTok.KillToken;
              Result := ReadStringArray(Result, IsWrite)
            end;
          end;
        end;
      end
      else if FTok.Test(ttBLEFT) then
      begin
        if baseType is TFuncSymbol then
          Result := ReadFunc(TFuncSymbol(baseType), IsWrite, Result as TDataExpr)
        else
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoMethodExpected);
      end;

    until (Expr = Result);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadExternalVar;
begin
  Result := nil;
  try
    if IsWrite then
    begin
      if FTok.TestDelete(ttASSIGN) then
      begin
        if not Assigned(Sym.WriteFunc) then
          FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_CantWriteToLeftSide);
        // Transform a := b into a(b)
        Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.WriteFunc, True);
        Result.AddArg(ReadExpr);
      end
      else if (Sym.Typ is TClassSymbol) or (Sym.Typ is TClassOfSymbol) then
      begin
        if not Assigned(Sym.ReadFunc) then
          FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_RightSideNeedsReturnType);
        Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc, False)
      end
      else
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_AssignExpected);
    end
    else if Assigned(Sym.ReadFunc) then
      Result := TFuncExpr.Create(FProg, FTok.HotPos, Sym.ReadFunc, False)
    else
      FProg.Msgs.AddCompilerStop(FTok.HotPos,CPE_WriteOnlyProperty); // ??
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadFor: TExpr;
var
  Expr: TExpr;
begin
  Result := TForExpr.Create(FProg, FTok.HotPos);
  try
    Expr := ReadName;

    if not (Expr is TDataExpr) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_VariableExpected);

    TForExpr(Result).VarExpr := TDataExpr(Expr);

    if not FTok.TestDelete(ttASSIGN) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EqualityExpected);

    TForExpr(Result).FromExpr := ReadExpr;

    if FTok.TestDelete(ttTO) then
      TForExpr(Result).IsUpward := True
    else if FTok.TestDelete(ttDOWNTO) then
      TForExpr(Result).IsUpward := False
    else
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ToOrDowntoExpected);

    TForExpr(Result).ToExpr := ReadExpr;

    if not FTok.TestDelete(ttDO) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

    TForExpr(Result).DoExpr := ReadBlock;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadIf: TIfExpr;
begin
  Result := TIfExpr.Create(FProg, FTok.HotPos);
  try
    Result.FCond := ReadExpr;

    if not FTok.TestDelete(ttTHEN) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ThenExpected);

    if not FTok.Test(ttELSE) then // if () then else;
      Result.FThen := ReadBlock;

    if not Assigned(Result.FThen) then  // if () then <EOF>
      Result.FThen := TNullExpr.Create(FProg,FTok.HotPos);

    if FTok.TestDelete(ttELSE) then
      Result.FElse := ReadBlock;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadCase;
var
  Expr, exprFrom, exprTo: TExpr;
  condList: TList;
  tt: TTokenType;
  x: Integer;
begin
  condList := TList.Create;
  try
    Result := TCaseExpr.Create(FProg, FTok.HotPos);
    try
      Result.ValueExpr := ReadExpr;

      if not FTok.TestDelete(ttOF) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

      while not FTok.TestDelete(ttEND) do
      begin
        if FTok.TestDelete(ttELSE) then
        begin
          Result.ElseExpr := ReadBlocks([ttEND], tt);
          break;
        end
        else
        begin
          try
            // Find a comma sparated list of case conditions  0, 1, 2..4: ;
            repeat

              exprFrom := ReadExpr;

              try
                if not Assigned(exprFrom) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);

                if FTok.TestDelete(ttDOTDOT) then
                begin
                  // range condition e. g. 0..12
                  exprTo := ReadExpr;
                  if not Assigned(exprTo) then
                  begin
                    exprTo.Free;
                    FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
                  end;
                  condList.Add(TRangeCaseCondition.Create(Result.ValueExpr,
                    exprFrom, exprTo));
                end
                else
                  // compare condition e. g. 123:
                  condList.Add(TCompareCaseCondition.Create(Result.ValueExpr,
                    exprFrom));

              except
                exprFrom.Free;
                raise;
              end;

            until not FTok.TestDelete(ttCOMMA);

            if not FTok.TestDelete(ttCOLON) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

            Expr := ReadBlock;

          except
            for x := 0 to condList.Count - 1 do
              TCaseCondition(condList[x]).Free;
            raise;
          end;

          // Add case conditions to TCaseExpr
          for x := 0 to condList.Count - 1 do
          begin
            TCaseCondition(condList[x]).TrueExpr := Expr;
            if x = 0 then
              TCaseCondition(condList[0]).OwnsTrueExpr := True;
            Result.CaseConditions.Add(condList[x]);
          end;
          condList.Clear;

          if not (FTok.Test(ttELSE) or FTok.Test(ttEND) or FTok.TestDelete(ttSEMI)) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
        end;
      end;
    except
      Result.Free;
      raise;
    end;
  finally
    condList.Free;
  end;
end;

function Tdws2Compiler.ReadWhile: TExpr;
begin
  Result := TWhileExpr.Create(FProg, FTok.HotPos);
  try
    TWhileExpr(Result).CondExpr := ReadExpr;

    if not FTok.TestDelete(ttDO) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

    TWhileExpr(Result).LoopExpr := ReadBlock;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadRepeat: TExpr;
var
  tt: TTokenType;
begin
  Result := TRepeatExpr.Create(FProg, FTok.HotPos);
  try
    TRepeatExpr(Result).LoopExpr := ReadBlocks([ttUNTIL], tt);
    TRepeatExpr(Result).CondExpr := ReadExpr;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadAssign(Left: TExpr): TExpr;
var
  pos: TScriptPos;
  right: TExpr;
begin
  Result := nil;
  pos := FTok.HotPos;
  right := ReadExpr;
  try

    if (Left is TFuncExpr) and TFuncExpr(Left).IsWritable then
    begin
      Left := TFuncCodeExpr.Create(FProg,FTok.HotPos,TFuncExpr(Left));
      if right.Typ = FProg.TypNil then
      begin
        right.Free;
        Result := TInitDataExpr.Create(FProg,FTok.HotPos,TDataExpr(Left));
        Exit;
      end
      else if right is TFuncExpr then
        right := TFuncCodeExpr.Create(FProg,FTok.HotPos,TFuncExpr(right));
    end;
    
    if Assigned(right.Typ) then
    begin
      if (right is TDataExpr) and (right.Typ.Size <> 1) then
      begin
        if right is TFuncExpr then
          TFuncExpr(right).SetResultAddr;

        Result := TAssignDataExpr.Create(FProg, pos, Left, right)
      end
      else
        Result := TAssignExpr.Create(FProg, pos, Left, right);
    end
    else
      FMsgs.AddCompilerStop(Pos, CPE_RightSideNeedsReturnType);
  except
    right.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadFunc(FuncSym: TFuncSymbol; IsWrite: Boolean;
  CodeExpr: TDataExpr = nil): TExpr;
begin
  if (FuncSym is TMethodSymbol) and not (TMethodSymbol(FuncSym).IsClassMethod) then
    Result := TMethodStaticExpr.Create(FProg, FTok.HotPos, TMethodSymbol(FuncSym),
      TMethodObjExpr.Create(FProg, FTok.HotPos, CodeExpr), True, CodeExpr,
      IsWrite and Assigned(CodeExpr))
  else
    Result := TFuncExpr.Create(FProg, FTok.HotPos, FuncSym, True, CodeExpr,
                               IsWrite and Assigned(CodeExpr));
  try
    ReadFuncArgs(TFuncExpr(Result).AddArg);
  except
    Result.Free;
    raise;
  end;
end;

procedure Tdws2Compiler.ReadFuncArgs(AddArgProc: TAddArgProcedure; LDelim: TTokenType; RDelim: TTokenType);
begin
  if FTok.TestDelete(LDelim) then
  begin
    if not FTok.TestDelete(RDelim) then
    begin
      // At least one argument was found
      repeat
        AddArgProc(ReadExpr);
      until not FTok.TestDelete(ttCOMMA);
      if not FTok.TestDelete(RDelim) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    end;
  end
end;

function Tdws2Compiler.ReadArray(TypeName: string): TTypeSymbol;
var
  x: Integer;
  min, max: TExprList;
  typ: TSymbol;
begin
  min := TExprList.Create;
  max := TExprList.Create;
  try

    if FTok.TestDelete(ttALEFT) then
    begin

      repeat
        // Lower bound
        min.Insert(0, ReadExpr.Optimize);

        if not (min[0] is TConstExpr) then
          FMsgs.AddCompilerStop(min[0].Pos, CPE_ArrayBoundNotAConstant);

        if not (min[0].Typ = FProg.TypInteger) then
          FMsgs.AddCompilerStop(min[0].Pos, CPE_ArrayBoundNotInteger);

        if not FTok.TestDelete(ttDOTDOT) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotDotExpected);

        // Upper bound
        max.Insert(0, ReadExpr.Optimize);

        if not (max[0] is TConstExpr) then
          FMsgs.AddCompilerStop(max[0].Pos, CPE_ArrayBoundNotAConstant);

        if not (max[0].Typ = FProg.TypInteger) then
          FMsgs.AddCompilerStop(max[0].Pos, CPE_ArrayBoundNotInteger);

        if max[0].Eval < min[0].Eval then
          FMsgs.AddCompilerStop(min[0].Pos, CPE_LowerBoundBiggerThanUpperBound);

        if FTok.Test(ttARIGHT) then
          Break;
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
    end;

    if not FTok.TestDelete(ttOF) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_OfExpected);

    typ := ReadType('');

    if min.Count > 0 then
    begin
      // initialize innermost array
      Result := TStaticArraySymbol.Create('', typ, min[0].Eval, max[0].Eval);
      try
        // add outer arrays
        for x := 1 to min.Count - 1 do
        begin
          FProg.Table.AddToDestructionList(Result);
          Result := TStaticArraySymbol.Create('', Result, min[x].Eval, max[x].Eval);
        end;

        // only outermost array is named
        Result.Name := TypeName;
      except
        Result.Free;
        raise;
      end;
    end
    else
      Result := TDynamicArraySymbol.Create(TypeName, typ);
  finally
    min.Free;
    max.Free;
  end;
end;

function Tdws2Compiler.ReadArrayConstant: TExpr;
begin
  Result := TArrayConstantExpr.Create(FProg, FTok.HotPos);
  try
    if not FTok.TestDelete(ttARIGHT) then
    begin
      // At least one argument was found
      repeat
        TArrayConstantExpr(Result).AddElementExpr(ReadExpr);
      until not FTok.TestDelete(ttCOMMA);

      if not FTok.TestDelete(ttARIGHT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure Tdws2Compiler.ReadNameList(Names: TStrings);
begin
  Names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    Names.Add(FTok.GetToken.FString);
    FTok.KillToken;
  until not FTok.TestDelete(ttCOMMA);
end;

procedure Tdws2Compiler.ReadNameList(Names: TStrings; out PosArray: TScriptPosArray);
begin
  Names.Clear;
  repeat
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    // Added HotPos positions to PosArray. Used for dictionary
    if coSymbolDictionary in FCompilerOptions then
    begin
      SetLength(PosArray, Length(PosArray)+1);  // grow the array as needed
      PosArray[High(PosArray)] := FTok.HotPos;
    end;
    Names.Add(FTok.GetToken.FString);
    FTok.KillToken;
  until not FTok.TestDelete(ttCOMMA);
end;

function Tdws2Compiler.ReadClass(TypeName: string): TTypeSymbol;
var
  x: Integer;
  Name: string;
  Names: TStringList;
  Fields: TList;
  sym, Typ: TSymbol;
  propSym: TPropertySymbol;
  defProp: Boolean;
  PosArray: TScriptPosArray;    // positions of items pulled from ReadNameList call
  isInSymbolTable: Boolean;
begin
  if FTok.TestDelete(ttOF) then
  begin
    // Declaration of a class reference
    if not FTok.TestName then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
    Name := FTok.GetToken.FString;
    FTok.KillToken;

    Typ := FProg.Table.FindSymbol(Name);
    if not Assigned(Typ) then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownClass, [Name]));
    if not (Typ is TClassSymbol) then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NotAClass, [Name]));

    if TypeName <> '' then
    begin
      Result := TClassOfSymbol.Create(TypeName, TClassSymbol(Typ));
      // Add reference of class type to Dictionary
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(Typ, FTok.HotPos);
    end
    else
      Result := TClassSymbol(Typ).ClassOf;
  end
  else
  begin
    // Check for a forward declaration of this class
    sym := FProg.Table.FindSymbol(TypeName);
    Result := nil;

    if Assigned(sym) then
      if sym is TClassSymbol then
      begin
        if TClassSymbol(sym).IsForward then
          Result := TClassSymbol(sym)
      end
      else
        FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NameAlreadyExists,
          [sym.Caption]));

    isInSymbolTable := Assigned(Result);

    if not Assigned(Result) then
      Result := TClassSymbol.Create(TypeName);

    // forwarded declaration
    if FTok.Test(ttSEMI) then
    begin
      if TClassSymbol(Result).IsForward then
        FMsgs.AddCompilerError(FTok.HotPos, CPE_ForwardAlreadyExists);

      TClassSymbol(Result).IsForward := True;
      Exit;
    end
    else
      TClassSymbol(Result).IsForward := False;

    try
      // inheritance
      if FTok.TestDelete(ttBLEFT) then
      begin
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

        Name := FTok.GetToken.FString;
        FTok.KillToken;

        Typ := FProg.Table.FindSymbol(Name);
        if not (Typ is TClassSymbol) then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NotAClass, [Name]));

        if TClassSymbol(Typ).IsForward then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_ClassNotImplementedYet,
            [Name]));

        TClassSymbol(Result).InheritFrom(TClassSymbol(Typ));

        if not FTok.TestDelete(ttBRIGHT) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
      end
      else
        TClassSymbol(Result).InheritFrom(FProg.TypObject);

      // standard class definition
      Names := TStringList.Create;
      Fields := TList.Create;
      try
        repeat
          // Read methods and properties
          if FTok.TestDelete(ttFUNCTION) then
            TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkFunction, False)))
          else if FTok.TestDelete(ttPROCEDURE) then
            TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkProcedure, False)))
          else if FTok.TestDelete(ttCONSTRUCTOR) then
            TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkConstructor, False)))
          else if FTok.TestDelete(ttDESTRUCTOR) then
            TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkDestructor, False)))
          else if FTok.TestDelete(ttCLASS) then
          begin
            if FTok.TestDelete(ttPROCEDURE) then
              TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkProcedure, True)))
            else if FTok.TestDelete(ttFUNCTION) then
              TClassSymbol(Result).AddMethod(TMethodSymbol(ReadMethodDecl(TClassSymbol(Result), fkFunction, True)))
            else
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcOrFuncExpected);
          end
          else if FTok.TestDelete(ttPROPERTY) then
          begin
            propSym := ReadProperty(TClassSymbol(Result));
            defProp := False;
            // Array-Prop can be default
            if propSym.ArrayIndices.Count > 0 then
            begin
              defProp := FTok.TestDelete(ttDEFAULT);
              if defProp then
              begin
                if not FTok.TestDelete(ttSEMI) then
                  FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
                if Assigned(TClassSymbol(Result).DefaultProperty) then
                  FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_MultipleDefaultProperties, [TClassSymbol(Result).Name]));
              end;
            end;
            TClassSymbol(Result).AddProperty(propSym);
            if defProp then
              TClassSymbol(Result).DefaultProperty := propSym;
          end
          else if FTok.Test(ttPRIVATE) or FTok.Test(ttPROTECTED) or
            FTok.Test(ttPUBLIC) or FTok.Test(ttPUBLISHED) then
          begin
            // visibility ignored
            FTok.KillToken;
          end
          else if FTok.Test(ttEND) then
            Break
          else if FTok.TestName then
          begin
            // Read fields
            Fields.Clear;
            // Conditionally pass in dynamic array
            if coSymbolDictionary in FCompilerOptions then
              ReadNameList(Names, PosArray)     // use overloaded version
            else
              ReadNameList(Names);

            if not FTok.TestDelete(ttCOLON) then
              FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

            Typ := ReadType('');
            for x := 0 to Names.Count - 1 do
            begin
              // Check if name isn't already used
              sym := TClassSymbol(Result).Members.FindLocal(Names[x]);
              if Assigned(sym) then
                FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NameAlreadyExists,
                  [Names[x]]));

              // Create Internal Field
              sym := TFieldSymbol.Create(Names[x], Typ);
              Fields.Add(sym);
              TClassSymbol(Result).AddField(TFieldSymbol(sym));
              // Enter Field symbol in dictionary
              if coSymbolDictionary in FCompilerOptions then
                FProg.SymbolDictionary.Add(sym, PosArray[x], [suDeclaration]);
            end;
            if not (FTok.TestDelete(ttSEMI) or FTok.Test(ttEND)) then
              Break;
          end
          else
            Break;
        until FTok.Test(ttEND);

        if not FTok.TestDelete(ttEND) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
      finally
        Names.Free;
        Fields.Free;
      end;
    except
      on E: EClassIncompleteError do
        ;                  // leave it handled
      else
      begin
        // if not ClassCompleteError then free the class and re-raise error
        if not isInSymbolTable then
        begin
          if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Remove(Result);
          Result.Free;
        end;
        raise;
      end;
    end; {except}
  end;
end;

function Tdws2Compiler.CheckFuncParams(ParamsA, ParamsB: TSymbolTable;
  IndexSym: TSymbol; TypSym: TSymbol): Boolean;
begin
  Result := False;

  if Assigned(IndexSym) then
  begin
    if Assigned(TypSym) then
    begin
      if ParamsB.Count <> ParamsA.Count + 2 then
        Exit;
      if ParamsB[ParamsA.Count + 1].Typ <> TypSym then
        Exit;
      if ParamsB[ParamsA.Count].Typ <> IndexSym then
        Exit;
    end
    else if ParamsB.Count <> ParamsA.Count + 1 then
      Exit
    else
      if ParamsB[ParamsA.Count].Typ <> IndexSym then
        Exit;
  end
  else
  begin
    if Assigned(TypSym) then
    begin
      if ParamsB.Count <> ParamsA.Count + 1 then
        Exit;
      if ParamsB[ParamsA.Count].Typ <> TypSym then
        Exit;
    end
    else if ParamsA.Count <> ParamsB.Count then
      Exit;
  end;

  Result := CheckParams(ParamsA,ParamsB,False);
end;

function Tdws2Compiler.ReadProperty(ClassSym: TClassSymbol): TPropertySymbol;
var
  x: Integer;
  name: string;
  sym: TSymbol;
  arrayIndices: TSymbolTable;
  propPos: TScriptPos;
  accessPos: TScriptPos;  // Position where either a Read or Write symbol is found
  PropertyComplete: TClassCompleteErrorInfo;
  SuggParamList: string;
  indexExpr: TExpr;
  indexTyp: TSymbol;
begin
  // Read property name
  if not FTok.TestName then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
  name := FTok.GetToken.FString;
  propPos := FTok.HotPos;
  FTok.KillToken;

  // Check if property name is free
  sym := ClassSym.Members.FindSymbol(name);
  if Assigned(sym) then
    if sym is TPropertySymbol then
    begin
      if TPropertySymbol(sym).ClassSymbol = ClassSym then
        FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NameAlreadyExists, [Name]));
    end
    else
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NameAlreadyExists, [Name]));

  arrayIndices := TSymbolTable.Create;
  try
    // Check if it is an array property
    ReadArrayParams(arrayIndices);

    if not FTok.TestDelete(ttCOLON) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

    sym := ReadType('');
    Result := TPropertySymbol.Create(name, sym);
    try
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(Result, propPos, [suDeclaration]);

      if FTok.TestDelete(ttINDEX) then
      begin
        indexExpr := ReadExpr.Optimize;
        if not (indexExpr is TConstExpr) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstantExpressionExpected);
        indexTyp := indexExpr.Typ;
        Result.SetIndex(TConstExpr(indexExpr).Data, TConstExpr(indexExpr).Addr,
          indexTyp);
      end
      else
        indexTyp := nil;

      // Generates a suggestion of how to fix it for class completion
      if FTok.TestDelete(ttREAD) then
      begin
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        Name := FTok.GetToken.FString;
        accessPos := FTok.HotPos;
        FTok.KillToken;

        sym := ClassSym.Members.FindSymbol(name);

        if not Assigned(sym) or (sym is TPropertySymbol) then
        begin
          { If read access not found, suggest class completion for it. }
          if not Assigned(sym) then
          begin
            SuggParamList := '';
            for x := 0 to arrayIndices.Count - 1 do
            begin
              if x <> 0 then
                SuggParamList := SuggParamList + '; ';
              SuggParamList := SuggParamList + arrayIndices[x].Description;
            end;
            // Get method *may* have need for params.
            if SuggParamList <> '' then
              SuggParamList := '(' + SuggParamList + ')';

            with PropertyComplete do
            begin
              ErrorClass := ClassSym;
              ErrorType := ccePropAccessDeclMissing;
              { determine the suggested fix }
              // if the name starts with an 'F' and property doesn't have an array,
              // then complete as a field, else method (like Delphi)
              if Assigned(Result.Typ) then     // ensure property type is something valid
              begin
                if (Pos('F', UpperCase(name)) = 1) and (arrayIndices.Count = 0) then
                  SuggestedFix := name + ': ' + Result.Typ.Name + ';'
                else
                  SuggestedFix := Format('function %s%s: %s;', [name, SuggParamList, Result.Typ.Name]);
              end
              else
                SuggestedFix := '';
            end;
            // add new suggestion to program's class complete needs list
            FProg.AddClassCompleteInfo(PropertyComplete);
          end;
          { Register the error and break the compilation process }
          FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_FieldMethodUnknown, [name]));
          raise EClassPropertyIncompleteError.Create('');
        end;

        if sym is TMethodSymbol then
        begin
          if not CheckFuncParams(arrayIndices, TMethodSymbol(sym).Params, indexTyp) then
            FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_IncompatibleType, [name]));
        end
        else if arrayIndices.Count > 0 then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionMethodExpected);

        if Result.Typ <> sym.Typ then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_IncompatibleType, [name]));

        Result.ReadSym := sym;
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(sym, accessPos)
      end;

      // Generates a suggestion of how to fix it for class completion
      if FTok.TestDelete(ttWRITE) then
      begin
        // Read name
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        Name := FTok.GetToken.FString;
        accessPos := FTok.HotPos;
        FTok.KillToken;

        // Check if symbol exists
        sym := ClassSym.Members.FindSymbol(Name);

        if not Assigned(sym) or (sym is TPropertySymbol) then
        begin
          { If read access not found, suggest class completion for it. }
          if not Assigned(sym) then
          begin
            SuggParamList := '';
            for x := 0 to arrayIndices.Count - 1 do
              SuggParamList := SuggParamList + arrayIndices[x].Description + '; '; // add ';' to end of all params
            if Assigned(Result.Typ) then
              SuggParamList := SuggParamList + 'Value: ' + Result.Typ.Name;    // add 'Value' parameter (guarenteed)

            with PropertyComplete do
            begin
              ErrorClass := ClassSym;
              ErrorType := ccePropAccessDeclMissing;
              { determine the suggested fix }
              // if the name starts with an 'F' and property doesn't have an array,
              // then complete as a field, else method (like Delphi)
              if Assigned(Result.Typ) then
              begin
                if (Pos('F', UpperCase(name)) = 1) and (arrayIndices.Count = 0) then
                  SuggestedFix := name + ': ' + Result.Typ.Name + ';'
                else
                  SuggestedFix := Format('procedure %s(%s);', [name, SuggParamList]);
              end
              else
                SuggestedFix := '';
            end;
            // add new suggestion to program's class complete needs list
            FProg.AddClassCompleteInfo(PropertyComplete);
          end;
          { Register the error and break the compilation process }
          FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_FieldMethodUnknown, [name]));
          raise EClassPropertyIncompleteError.Create('');
        end;

        if sym is TFuncSymbol then
        begin
          if TFuncSymbol(sym).Kind <> fkProcedure then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureMethodExpected);
          if not CheckFuncParams(arrayIndices, TFuncSymbol(sym).Params, indexTyp, Result.Typ) then
            FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_IncompatibleType, [name]));
        end
        else if Result.Typ <> sym.Typ then
          FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_IncompatibleWriteSymbol,
            [Name]));

        Result.WriteSym := sym;
        if coSymbolDictionary in FCompilerOptions then
          FProg.SymbolDictionary.Add(sym, accessPos)
      end;

      if (Result.ReadSym = nil) and (Result.WriteSym = nil) then
        FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_ReadOrWriteExpected, [name]));

      if not FTok.TestDelete(ttSEMI) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);

      // Add array indices to property symbol (if any)
       for x := 0 to arrayIndices.Count - 1 do
         Result.ArrayIndices.AddSymbol(arrayIndices[x]);
       arrayIndices.Clear;

    except
      // Remove reference to symbol (gets freed)
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Remove(Result);
      Result.Free;
      raise;
    end;
  finally
    arrayIndices.Free;
  end;

end;

function Tdws2Compiler.ReadRecord(TypeName: string): TTypeSymbol;
var
  x: Integer;
  Names: TStringList;
  member,
  Typ: TSymbol;
  PosArray: TScriptPosArray;
begin
  Result := TRecordSymbol.Create(TypeName);
  try
    Names := TStringList.Create;
    try
      repeat

        if FTok.Test(ttEND) then
          break;

        if coSymbolDictionary in FCompilerOptions then
          ReadNameList(names, posArray)     // use overloaded version
        else
          ReadNameList(names);

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

        Typ := ReadType('');
        for x := 0 to Names.Count - 1 do
        begin
          if TRecordSymbol(Result).Members.FindLocal(Names[x]) <> nil then
            FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_NameAlreadyExists,
              [Names[x]]));

          member := TMemberSymbol.Create(Names[x], Typ);
          TRecordSymbol(Result).AddMember(TMemberSymbol(member));

          // Add member symbols and positions
          if coSymbolDictionary in FCompilerOptions then
            FProg.SymbolDictionary.Add(member, PosArray[x], [suDeclaration]);
        end;

      until not FTok.TestDelete(ttSEMI) or FTok.Test(ttEND);
    finally
      Names.Free;
    end;

    if not FTok.TestDelete(ttEND) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
  except
    // Removed added record symbols. Destroying object
    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Remove(Result);
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadTry: TExpr;
var
  TryBlock: TExpr;
  tt: TTokenType;
  wasExcept: Boolean;
begin
  wasExcept := FIsExcept;
  FIsExcept := False;
  try
    TryBlock := ReadBlocks([ttFINALLY, ttEXCEPT], tt);
    if tt = ttEXCEPT then
    begin
      FIsExcept := True;
      Result := ReadExcept(TryBlock);
      // TryBlock is freed by ReadExcept in case of exception
    end
    else
    begin
      Result := TFinallyExpr.Create(FProg, FTok.HotPos);
      TExceptionExpr(Result).TryExpr := TryBlock;
      try
        TExceptionExpr(Result).HandlerExpr := ReadBlocks([ttEND], tt);
      except
        Result.Free;
        raise;
      end;
    end;
  finally
    FIsExcept := wasExcept;
  end;
end;

function Tdws2Compiler.ReadRaise: TExpr;
begin
  if FIsExcept and (FTok.Test(ttSEMI) or FTok.Test(ttEND)) then
    Result := TReraiseExpr.Create(FProg, FTok.HotPos)
  else
  begin
    Result := ReadExpr;
    try
      if not Assigned(Result.Typ) then
        FProg.Msgs.AddCompilerError(FTok.HotPos,CPE_TypeExpected);
      Result := TRaiseExpr.Create(FProg, FTok.HotPos, Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function Tdws2Compiler.ReadExcept(TryExpr: TExpr): TExceptExpr;
var
  tt: TTokenType;
  DoExpr: TExceptDoExpr;
  varName: string;
  ClassSym: TSymbol;
begin
  Result := TExceptExpr.Create(FProg, FTok.HotPos);
  try
    Result.TryExpr := TryExpr;
    if FTok.Test(ttON) then
    begin
      while FTok.TestDelete(ttON) do
      begin
        if not FTok.TestName then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
        varName := FTok.GetToken.FString;
        FTok.KillToken;

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected);

        ClassSym := ReadType('');
        if not FTok.TestDelete(ttDO) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_DoExpected);

        DoExpr := TExceptDoExpr.Create(FProg, FTok.HotPos);
        try
          DoExpr.ExceptionVar := TDataSymbol.Create(varName, ClassSym);

          FProg.Table.AddSymbol(DoExpr.ExceptionVar);
          try
            DoExpr.DoBlockExpr := ReadBlock;
          finally
            FProg.Table.Remove(DoExpr.ExceptionVar);
          end;
        except
          DoExpr.Free;
          raise;
        end;

        Result.DoExprs.Add(DoExpr);

        if not FTok.Test(ttEND) then
          if not FTok.TestDelete(ttSEMI) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_SemiExpected);
      end;

      if FTok.TestDelete(ttELSE) then
        Result.ElseExpr := ReadBlocks([ttEND], tt)
      else if not FTok.TestDelete(ttEND) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_EndExpected);
    end
    else
      Result.HandlerExpr := ReadBlocks([ttEND], tt);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadType(TypeName: string): TTypeSymbol;
var
  name: string;
  namePos: TScriptPos;
  sym : TSymbol;
begin
  if FTok.TestDelete(ttRECORD) then
    Result := ReadRecord(TypeName)
  else if FTok.TestDelete(ttARRAY) then
    Result := ReadArray(TypeName)
  else if FTok.TestDelete(ttCLASS) then
    Result := ReadClass(TypeName)
  else if FTok.TestDelete(ttBLEFT) then
    Result := ReadEnumeration(TypeName)
  else if FTok.TestDelete(ttPROCEDURE) then
  begin
    Result := ReadProcDecl(fkProcedure,nil,False,True);
    Result.Name := TypeName;
  end
  else if FTok.TestDelete(ttFUNCTION) then
  begin
    Result := ReadProcDecl(fkFunction,nil,False,True);
    Result.Name := TypeName;
  end
  else if FTok.TestName then
  begin
    name := FTok.GetToken.FString;
    namePos := FTok.HotPos;        // get the position before token is deleted
    FTok.KillToken;
    sym := FProg.Table.FindSymbol(name);
    Result := nil;

    if sym is TUnitSymbol then
    begin
      if not FTok.TestDelete(ttDOT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_DotExpected);
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);
      name := FTok.GetToken.FString;
      FTok.KillToken;
      sym := TUnitSymbol(sym).Table.FindLocal(name);
    end;

    if not Assigned(sym) then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_TypeUnknown, [name]))
    else if not (sym is TTypeSymbol) then
      FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_InvalidType, [name]))
    else
      Result := TTypeSymbol(sym); // TTypeSymbol(sym).BaseType ??
    // Create name symbol, e. g.: type a = integer;
    if TypeName <> '' then
      Result := TAliasSymbol.Create(TypeName, Result);

    if coSymbolDictionary in FCompilerOptions then
      FProg.SymbolDictionary.Add(Result, namePos);
  end
  else
  begin
    Result := nil;
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_TypeExpected);
  end;

  // Ensure that unnamed symbols will be freed
  if Result.Name = '' then
    FProg.Table.AddToDestructionList(Result);
end;

function Tdws2Compiler.ReadExpr: TExpr;
var
  r: TExpr;
  tt: TTokenType;
  Pos: TScriptPos;
begin
  // Read left argument
  Result := ReadExprAdd;
  try
    // Read operator
    while (FTok.Test(ttEQ) or FTok.Test(ttNOTEQ) or
      FTok.Test(ttLESS) or FTok.Test(ttLESSEQ) or
      FTok.Test(ttGTR) or FTok.Test(ttGTREQ)) or FTok.Test(ttIS) do
    begin
      tt := FTok.GetToken.FTyp;
      FTok.TestDelete(tt);
      Pos := FTok.HotPos;

      // Read right argument
      r := ReadExprAdd;
      try
        if (Result.Typ is TClassSymbol) or (Result.Typ = FProg.TypNil) then
        begin
          case tt of
            ttEQ, ttNOTEQ:
              Result := TObjCmpExpr.Create(FProg, Pos, Result, r, tt = ttEQ);
            ttIS: Result := TIsOpExpr.Create(FProg, Pos, Result, r);
          else
            FProg.Msgs.AddCompilerStop(Pos, CPE_InvalidOperands);
          end;
        end
        else
        begin
          case tt of
            ttEQ: Result := TRelOpExpr.Create(FProg, Pos, Result, r, roEqual);
            ttLESS: Result := TRelOpExpr.Create(FProg, Pos, Result, r, roLess);
            ttGTR: Result := TRelOpExpr.Create(FProg, Pos, Result, r, roMore);
            ttLESSEQ: Result := TRelOpExpr.Create(FProg, Pos, Result, r,
                roLessEqual);
            ttGTREQ: Result := TRelOpExpr.Create(FProg, Pos, Result, r, roMoreEqual);
            ttNOTEQ: Result := TRelOpExpr.Create(FProg, Pos, Result, r, roUnEqual);
          else
            FProg.Msgs.AddCompilerStop(Pos, CPE_InvalidOperands);
          end;
        end;
      except
        r.Free;
        raise;
      end;
      Result.TypeCheck;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadExprAdd: TExpr;
var
  r: TExpr;
  tt: TTokenType;
  Pos: TScriptPos;
begin
  // Read left argument
  Result := ReadExprMult;
  try

    while (FTok.Test(ttPLUS) or FTok.Test(ttMINUS) or
      FTok.Test(ttOR) or FTok.Test(ttXOR)) do
    begin

      tt := FTok.GetToken.FTyp;
      FTok.TestDelete(tt);
      Pos := FTok.HotPos;

      // Read right argument
      r := ReadExprMult;
      try
        // Generate function and add left and right argument
        case tt of
          ttPLUS: Result := TAddExpr.Create(FProg, Pos, Result, r);
          ttMINUS: Result := TSubExpr.Create(FProg, Pos, Result, r);
          ttOR: Result := TOrExpr.Create(FProg, Pos, Result, r);
          ttXOR: Result := TXorExpr.Create(FProg, Pos, Result, r);
        end;
      except
        r.Free;
        raise;
      end;

      Result.TypeCheck;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadExprMult: TExpr;
var
  r: TExpr;
  tt: TTokenType;
  Pos: TScriptPos;
begin
  // Read left argument
  Result := ReadTerm;
  try
    while (FTok.Test(ttTIMES) or FTok.Test(ttDIVIDE) or
      FTok.Test(ttMOD) or FTok.Test(ttDIV) or
      FTok.Test(ttAND) or FTok.Test(ttAS)) do
    begin

      tt := FTok.GetToken.FTyp;
      FTok.TestDelete(tt);

      // Save position of the operator
      Pos := FTok.HotPos;

      // Read right argument
      r := ReadTerm;
      try
        // Generate function and add left and right argument
        case tt of
          ttTIMES: Result := TMultExpr.Create(FProg, Pos, Result, r);
          ttDIVIDE: Result := TDivideExpr.Create(FProg, Pos, Result, r);
          ttDIV: Result := TDivExpr.Create(FProg, Pos, Result, r);
          ttMOD: Result := TModExpr.Create(FProg, Pos, Result, r);
          ttAND: Result := TAndExpr.Create(FProg, Pos, Result, r);
          ttAS: Result := TAsOpExpr.Create(FProg, Pos, Result, r);
        end;
      except
        r.Free;
        raise;
      end;

      Result.TypeCheck;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadTerm: TExpr;
const
  nilIntf: IUnknown = nil;
begin
  if FTok.TestDelete(ttPLUS) then
    // (redundant) plus sign
    Result := ReadTerm
  else if FTok.TestDelete(ttMINUS) then
  begin
    // Negation
    Result := TNegExpr.Create(FProg, FTok.HotPos, ReadTerm);
    try
      Result.TypeCheck;
    except
      Result.Free;
      raise;
    end;
  end
  else if FTok.TestDelete(ttALEFT) then
    Result := ReadArrayConstant
  else if FTok.TestDelete(ttNOT) then
  begin
    Result := TNotExpr.Create(FProg, FTok.HotPos, ReadTerm);
    try
      Result.TypeCheck;
    except
      Result.Free;
      raise;
    end;
  end
  else if FTok.TestDelete(ttBLEFT) then
  begin
    // Read expression in brackets
    Result := ReadExpr;
    if not FTok.TestDelete(ttBRIGHT) then
    begin
      Result.Free;
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    end;
  end
  else if FTok.TestDelete(ttNIL) then
    Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypNil, nilIntf)
  else if FTok.TestDelete(ttTRUE) then
    Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypBoolean, True)
  else if FTok.TestDelete(ttFALSE) then
    Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypBoolean, False)
  else if FTok.Test(ttINHERITED) or FTok.TestName  then
    // Variable or Function
    Result := ReadName
  else
    // Constant values in the code
    Result := ReadConstValue;

  // No expression found
  if not Assigned(Result) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_ExpressionExpected);
end;

function Tdws2Compiler.ReadConstValue: TExpr;
var
  t: TToken;
  tt: TTokenType;
begin
  Result := nil;
  if FTok.Test(ttStrVal) or FTok.Test(ttIntVal)
    or FTok.Test(ttFloatVal) then
  begin
    t := FTok.GetToken;
    tt := t.FTyp;
    case tt of
      ttIntVal:
        Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypInteger,
          t.FInteger);
      ttFloatVal:
        Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypFloat, t.FFloat);
      ttStrVal:
        Result := TConstExpr.Create(FProg, FTok.HotPos, FProg.TypString,
          t.FString);
    end;
    FTok.KillToken;
  end;
end;

procedure Tdws2Compiler.ReadArrayParams(ArrayIndices: TSymbolTable);
var
  x: Integer;
  names: TStringList;
  typSym: TSymbol;
  isVarParam, isConstParam: Boolean;
begin
  if FTok.TestDelete(ttALEFT) then
  begin
    if FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ParamsExpected);

    // At least one argument was found
    names := TStringList.Create;
    try
      repeat
        isVarParam := FTok.TestDelete(ttVAR);

        if not isVarParam then
        begin
          isConstParam := FTok.TestDelete(ttCONST);
          isVarParam := isConstParam;
        end
        else
          isConstParam := False;

        ReadNameList(names);

        if not FTok.TestDelete(ttCOLON) then
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
        else
        begin
          typSym := ReadType('');
          for x := 0 to names.Count - 1 do
          begin
            if isVarParam then
              ArrayIndices.AddSymbol(TVarParamSymbol.Create(names[x], typSym, not isConstParam))
            else
              ArrayIndices.AddSymbol(TParamSymbol.Create(names[x], typSym));
          end;
        end;
      until not FTok.TestDelete(ttSEMI);

    finally
      names.Free;
    end;

    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);
  end;
end;

procedure Tdws2Compiler.ReadParams(Proc: TFuncSymbol; ParamsToDictionary: Boolean);
var
  x: Integer;
  names: TStringList;
  Typ: TSymbol;
  varpar, constpar: Boolean;
  PosArray: TScriptPosArray;
  sym: TParamSymbol;
  defaultExpr : TExpr;
begin
  if FTok.TestDelete(ttBLEFT) then
  begin
    if not FTok.TestDelete(ttBRIGHT) then
    begin
      // At least one argument was found
      names := TStringList.Create;
      try
        repeat
          varpar := FTok.TestDelete(ttVAR);

          if not varpar then
          begin
            constpar := FTok.TestDelete(ttCONST);
            varpar := constpar;
          end
          else
            constpar := False;

          // Conditionally pass in dynamic array
          if ParamsToDictionary and (coSymbolDictionary in FCompilerOptions) then
            ReadNameList(names, PosArray)     // use overloaded version
          else
            ReadNameList(names);

          if not FTok.TestDelete(ttCOLON) then
            FMsgs.AddCompilerStop(FTok.HotPos, CPE_ColonExpected)
          else
          begin
            defaultExpr := nil;
            Typ := ReadType('');
            try
              if FTok.TestDelete(ttEQ) then
              begin
                if varpar and not constpar then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_DefaultVarParam);

                defaultExpr := ReadExpr;
                defaultExpr := defaultExpr.Optimize;

                if not (defaultExpr is TConstExpr) then
                  FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);

                if not Typ.IsCompatible(defaultExpr.Typ) then
                  FMsgs.AddCompilerError(FTok.HotPos,
                    Format(CPE_IncompatibleTypes,[Typ.Caption,defaultExpr.Typ.Caption]));
              end;

              for x := 0 to names.Count - 1 do
              begin
                if varpar then
                  sym := TVarParamSymbol.Create(names[x], Typ, not constpar)
                else
                  sym := TParamSymbol.Create(names[x], Typ);

                if Assigned(defaultExpr) then
                  sym.SetDefaultValue(TConstExpr(defaultExpr).Data,TConstExpr(defaultExpr).Addr);

                Proc.AddParam(sym);
                // Enter Field symbol in dictionary
                if ParamsToDictionary and (coSymbolDictionary in FCompilerOptions) then
                begin
                  FProg.SymbolDictionary.Add(sym, PosArray[x], [suDeclaration]);  // add variable symbol
                  FProg.SymbolDictionary.Add(Typ, FTok.HotPos);  // add type symbol
                end;
              end;
            finally
              FreeAndNil(defaultExpr);
            end;
          end;
        until not FTok.TestDelete(ttSEMI);

      finally
        names.Free;
      end;

      if not FTok.TestDelete(ttBRIGHT) then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    end;
  end
end;

function Tdws2Compiler.ReadSwitch(SwitchName: string): Boolean;
begin
  // This procedure is called by the tokenizer if it finds {$xx in the string
  if (SwitchName = SWI_INCLUDE_LONG) or (SwitchName = SWI_INCLUDE_SHORT) then
    Result := True
  else if (SwitchName = SWI_FILTER_LONG) or (SwitchName = SWI_FILTER_SHORT) then
    Result := True
  else
  begin
    Result := False;

    FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_CompilerSwitchUnknown,
      [SwitchName]));

    while not FTok.TestDelete(ttCRIGHT) do
      FTok.KillToken;
  end;
end;

function Tdws2Compiler.ReadInstrSwitch: TExpr;
var
  switchName, name, scriptSource: string;
  oldTok: TTokenizer;
begin
  Result := nil;

  switchName := FTok.GetToken.FString;
  FTok.KillToken;

  // {$INCLUDE ''} or {$I ''} or {$FILTER ''} or {$A ''}
  if (switchName = SWI_INCLUDE_LONG) or (switchName = SWI_INCLUDE_SHORT)
    or (switchName = SWI_FILTER_LONG) or (switchName = SWI_FILTER_SHORT) then
  begin
    if not FTok.Test(ttStrVal) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_IncludeFileExpected);
    name := FTok.GetToken.FString;
    FTok.KillToken;

    try
      oldTok := FTok;
      scriptSource := GetScriptSource(name);

      if (switchName = SWI_FILTER_LONG) or (switchName = SWI_FILTER_SHORT) then
      begin
        if Assigned(FFilter) then
          // Include file is processed by the filter
          FTok := TTokenizer.Create(FFilter.Process(scriptSource, FMsgs),
            name, FProg.Msgs)
        else
          FMsgs.AddCompilerStop(FTok.HotPos, CPE_NoFilterAvailable);
      end
      else
        // Include file is included as-is
        FTok := TTokenizer.Create(scriptSource, name, FProg.Msgs);

      try
        FTok.SwitchHandler := ReadSwitch;
        Result := ReadScript(name, stInclude);
      finally
        FTok.Free;
        FTok := oldTok;
      end;
    except
      on e: EScriptError do
        raise;
      on e: Exception do
        FMsgs.AddCompilerStop(FTok.HotPos, e.Message);
    end;

  end
  else
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_CompilerSwitchUnknown, [Name]));

  // Kill everthing up to the next "}"
  while not FTok.Test(ttCRIGHT) do
    FTok.KillToken;

  // Simulate a semicolon
  FTok.GetToken.FTyp := ttSEMI;
end;

// Checks if a name already exists in the Symboltable

procedure Tdws2Compiler.CheckName(Name: string);
var
  sym: TSymbol;
begin
  sym := FProg.Table.FindLocal(Name);

  if not Assigned(sym) and (FProg is TProcedure) then
    sym := TProcedure(FProg).Func.Params.FindLocal(Name);

  if Assigned(sym) then
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_NameAlreadyExists, [Name]));
end;

function Tdws2Compiler.FindScriptPathForFile(const FileName: string): string;
var
  x: Integer;
begin
  if FileExists(FileName) then
    Result := ''
  else
  begin
    for x := 0 to FScriptPaths.Count - 1 do
      if FileExists(FScriptPaths[x] + FileName) then
      begin
        Result := FScriptPaths[x];
        exit;
      end;
    FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_IncludeFileNotFound, [FileName]));
  end;
end;

function Tdws2Compiler.GetVarExpr(dataSym: TDataSymbol): TDataExpr;
begin
  if FProg.Level = dataSym.Level then
    Result := TVarExpr.Create(FProg, FTok.HotPos, dataSym.Typ, dataSym)
  else
    Result := TVarParentExpr.Create(FProg, FTok.HotPos, dataSym.Typ, dataSym)
end;

function Tdws2Compiler.GetVarParamExpr(dataSym: TVarParamSymbol): TExpr;
begin
  if FProg.Level = dataSym.Level then
    Result := TVarParamExpr.Create(FProg, FTok.HotPos, dataSym.Typ, dataSym)
  else
    Result := TVarParamParentExpr.Create(FProg, FTok.HotPos, dataSym.Typ, dataSym)
end;

function Tdws2Compiler.CheckParams(A, B: TSymbolTable; CheckNames: Boolean): Boolean;
var
  x: Integer;
  r: Boolean;
begin
  Result := True;
  for x := 0 to A.Count - 1 do begin
    r := False;
    if CheckNames and not SameText(A[x].Name, B[x].Name) then
        FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_BadParameterName, [x,
          A[x].Name]))
    else if not A[x].Typ.IsCompatible(B[x].Typ) then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_BadParameterType, [x,
        A[x].Typ.Caption, B[x].Typ.Caption]))
    else if (A[x] is TVarParamSymbol) and not (B[x] is TVarParamSymbol) then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_VarParameterExpected, [x,
        A[x].Name]))
    else if not (A[x] is TVarParamSymbol) and (B[x] is TVarParamSymbol) then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_ValueParameterExpected, [x,
        A[x].Name]))
    else if (A[x] is TVarParamSymbol) and (B[x] is TVarParamSymbol) and
      (TVarParamSymbol(A[x]).IsWritable <> TVarParamSymbol(B[x]).IsWritable) then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_BadParameterType, [x,
        A[x].Description, B[x].Description]))
    else
      r := True;
    Result := Result and r;
  end;
end;

procedure Tdws2Compiler.CompareFuncSymbols(A, B: TFuncSymbol; IsCheckingParameters:
  Boolean);
begin
  if A.Kind <> B.Kind then
  begin
    case A.Kind of
      fkFunction: FMsgs.AddCompilerStop(FTok.HotPos, CPE_FunctionExpected);
      fkProcedure: FMsgs.AddCompilerStop(FTok.HotPos, CPE_ProcedureExpected);
      fkConstructor: FMsgs.AddCompilerStop(FTok.HotPos, CPE_ConstructorExpected);
      fkDestructor: FMsgs.AddCompilerStop(FTok.HotPos, CPE_DestructorExpected);
    end;
  end;

  if IsCheckingParameters then
  begin

    if Assigned(A.Typ) and not A.Typ.IsCompatible(B.Typ) then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_BadResultType,
        [A.Typ.Caption]));

    if A.Params.Count <> B.Params.Count then
      FMsgs.AddCompilerError(FTok.HotPos, Format(CPE_BadNumberOfParameters,
        [A.Params.Count, B.Params.Count]))
    else
      CheckParams(A.Params,B.Params,True);
  end;
end;

function Tdws2Compiler.ReadConnectorSym(Name: string;
  var BaseExpr: TExpr; ConnectorType: IConnectorType; IsWrite: Boolean): TExpr;

  function TryConnectorCall: TConnectorCallExpr;
  begin
    // Try to read the call of a connector function
    Result := TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr,
      IsWrite);

    BaseExpr := nil;

    ReadFuncArgs(TConnectorCallExpr(Result).AddArg);

    if not TConnectorCallExpr(Result).AssignConnectorSym(ConnectorType) then
      FreeAndNil(Result);
  end;

begin
  if FTok.Test(ttALEFT) then begin
    Result := ReadConnectorArray(Name,BaseExpr,ConnectorType,IsWrite);
  end
  else if FTok.Test(ttBLEFT) then
  begin
    // Brackets -> always a function
    Result := TryConnectorCall;

    if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos,
        Format(CPE_ConnectorCall, [Name, ConnectorType.ConnectorCaption]));
  end
  else if not IsWrite then
  begin
    // The assignment ":=" was already read.
    Result := TConnectorReadExpr.Create(FProg, FTok.HotPos, Name, BaseExpr);

    BaseExpr := nil;

    if not TConnectorReadExpr(Result).AssignConnectorSym(ConnectorType) then
    begin
      Result.Free;
      FMsgs.AddCompilerStop(FTok.HotPos,
        Format(CPE_ConnectorMember, [Name, ConnectorType.ConnectorCaption]));
    end;
  end
  else if FTok.TestDelete(ttASSIGN) then
  begin
    // A assignment of the form "connector.member := expr" was found
    // and is transformed into "connector.member(expr)"
    Result := TConnectorWriteExpr.Create(FProg, FTok.HotPos,
      Name, BaseExpr, ReadExpr);

    BaseExpr := nil;

    if not TConnectorWriteExpr(Result).AssignConnectorSym(ConnectorType) then
    begin
      Result.Free;
      FMsgs.AddCompilerStop(FTok.HotPos,
        Format(CPE_ConnectorMember, [Name, ConnectorType.ConnectorCaption]));
    end;
  end
  else
  begin
    // It's possible that we should read a connector member or
    // call a connector function without arguments.
    Result := TConnectorReadExpr.Create(FProg, FTok.HotPos, Name, BaseExpr);

    if not TConnectorReadExpr(Result).AssignConnectorSym(ConnectorType) then
    begin
      // Don't destroy BaseExpr!
      TConnectorReadExpr(Result).BaseExpr := nil;
      Result.Free;

      // Try to read a connector call
      Result := TryConnectorCall;
    end;

    if not Assigned(Result) then
      FMsgs.AddCompilerStop(FTok.HotPos,
        Format(CPE_ConnectorMember, [Name, ConnectorType.ConnectorCaption]));
  end;
end;

function Tdws2Compiler.ReadConnectorArray(Name: String; var BaseExpr: TExpr;
  ConnectorType: IConnectorType; IsWrite: Boolean): TExpr;
begin
  Result := TConnectorCallExpr.Create(FProg, FTok.HotPos, Name, BaseExpr, IsWrite, True);
  try
    BaseExpr := nil; // on Exception BaseExpr is freed by our Result!

    ReadFuncArgs(TConnectorCallExpr(Result).AddArg,ttALEFT,ttARIGHT);

    if IsWrite and FTok.TestDelete(ttASSIGN) then
      TConnectorCallExpr(Result).AddArg(ReadExpr);

    if not TConnectorCallExpr(Result).AssignConnectorSym(ConnectorType) then
      FMsgs.AddCompilerStop(FTok.HotPos,
        Format(CPE_ConnectorIndex, [ConnectorType.ConnectorCaption]));
  except
    Result.Free;
    raise;
  end;
end;

{ TEnvironment }

procedure TConfiguration.Assign(Source: TPersistent);
begin
  if Source is TConfiguration then
  begin
    FCompilerOptions := TConfiguration(Source).CompilerOptions;
    FMaxDataSize := TConfiguration(Source).MaxDataSize;
    FScriptPaths.Assign(TConfiguration(Source).ScriptPaths);
    FTimeout := TConfiguration(Source).Timeout;
  end
  else
    inherited;
end;

constructor TConfiguration.Create(Owner: TComponent);
begin
  inherited Create;
  FOwner := Owner;
  FSystemTable := TStaticSymbolTable.Create;
  FConnectors := TStringList.Create;
  FScriptPaths := TStringList.Create;
  FUnits := TStringList.Create;
  InitSystemTable;
  FUnits.AddObject(SYS_INTERNAL, Pointer(IUnit(InternalUnit)));
  FStackChunkSize := C_DefaultStackChunkSize;
  FDefaultResultType := Tdws2DefaultResultType.Create(nil);
  FResultType := FDefaultResultType;
end;

destructor TConfiguration.Destroy;
begin
  inherited;
  (FSystemTable as TStaticSymbolTable)._Release;
  FConnectors.Free;
  FScriptPaths.Free;
  FUnits.Free;
  FDefaultResultType.Free;
end;

procedure TConfiguration.InitSystemTable;
var
  clsObject, clsException, clsDelphiException: TClassSymbol;
  meth: TMethodSymbol;
  varSym: TBaseSymbol;
begin
  // Create base data types
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_BOOLEAN, typBooleanID, false));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_DATETIME, typDateTimeID, VarAsType(0.0, varDate)));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_FLOAT, typFloatID, 0.0));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_INTEGER, typIntegerID, VarAsType(0, varInteger)));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_STRING, typStringID, ''));
  varSym := TBaseSymbol.Create(SYS_VARIANT, typVariantID, Unassigned);
  SystemTable.AddSymbol(varSym);
  SystemTable.AddSymbol(TConstSymbol.Create('Null', varSym, Null));
  SystemTable.AddSymbol(TConstSymbol.Create('Unassigned', varSym, Unassigned));

  // Create "root" class TObject
  clsObject := TClassSymbol.Create(SYS_TOBJECT);
  // Add constructor Create
  meth := TMethodSymbol.Create(SYS_TOBJECT_CREATE, fkConstructor, clsObject);
  meth.Executable := ICallable(TEmptyFunc.Create);
  clsObject.AddMethod(meth);
  // Add destructor Destroy
  meth := TMethodSymbol.Create(SYS_TOBJECT_DESTROY, fkDestructor, clsObject);
  meth.IsVirtual := True;
  meth.Executable := ICallable(TEmptyFunc.Create);
  clsObject.AddMethod(meth);
  SystemTable.AddSymbol(clsObject);

  // Create class Exception
  clsException := TClassSymbol.Create(SYS_EXCEPTION);
  clsException.InheritFrom(clsObject);
  clsException.AddField(TFieldSymbol.Create(SYS_EXCEPTION_MESSAGE,
    SystemTable.FindSymbol(SYS_STRING)));
  TExceptionCreateMethod.Create(mkConstructor, [], 0, SYS_TOBJECT_CREATE, ['Msg',
    SYS_STRING], '', clsException, SystemTable);
  SystemTable.AddSymbol(clsException);

  // Create class EDelphi
  clsDelphiException := TClassSymbol.Create(SYS_EDELPHI);
  clsDelphiException.InheritFrom(clsException);
  clsDelphiException.AddField(TFieldSymbol.Create(SYS_EDELPHI_EXCEPTIONCLASS,
    SystemTable.FindSymbol(SYS_STRING)));
  TDelphiExceptionCreateMethod.Create(mkConstructor, [], 0, SYS_TOBJECT_CREATE,
    ['Cls', SYS_STRING, 'Msg', SYS_STRING], '', clsDelphiException, SystemTable);
  SystemTable.AddSymbol(clsDelphiException);

  TParamFunc.Create(SystemTable, 'Param', ['Index', SYS_INTEGER], SYS_VARIANT);
  TParamStrFunc.Create(SystemTable, 'ParamStr', ['Index', SYS_INTEGER],
    SYS_STRING);
  TParamCountFunc.Create(SystemTable, 'ParamCount', [], SYS_INTEGER);
end;

procedure TConfiguration.SetFilter(const Value: Tdws2Filter);
begin
  if Assigned(FFilter) then
    FFilter.RemoveFreeNotification(FOwner);

  FFilter := Value;

  if Assigned(FFilter) then
    FFilter.FreeNotification(FOwner);
end;

procedure TConfiguration.SetResultType(const Value: Tdws2ResultType);
begin
  if Assigned(FResultType) and (FResultType <> FDefaultResultType) then
    FResultType.RemoveFreeNotification(FOwner);

  FResultType := Value;

  if Assigned(FResultType) then
    FResultType.FreeNotification(FOwner)
  else
    FResultType := FDefaultResultType;
end;

procedure TConfiguration.SetScriptPaths(const Value: TStrings);
const
{$IFDEF LINUX}
  PathSeparator = '/';
{$ELSE}
  PathSeparator = '\';
{$ENDIF}
var
  x: Integer;
begin
  FScriptPaths.Assign(Value);

  for x := 0 to FScriptPaths.Count - 1 do
  begin
    if Copy(FScriptPaths[x], Length(FScriptPaths[x]), 1) <> PathSeparator then
      FScriptPaths[x] := FScriptPaths[x] + PathSeparator;
  end;
end;

{ TExceptionCreateMethod }

procedure TExceptionCreateMethod.Execute;
begin
  Info[SYS_EXCEPTION_MESSAGE] := Info['Msg'];
end;

{ TDelphiExceptionCreateMethod }

procedure TDelphiExceptionCreateMethod.Execute(var ExternalObject: TObject);
begin
  Info[SYS_EXCEPTION_MESSAGE] := Info['Msg'];
  Info[SYS_EDELPHI_EXCEPTIONCLASS] := Info['Cls']
end;

{ TParamFunc }

procedure TParamFunc.Execute;
begin
  Info.Result := Info.Caller.Parameters[Integer(Info['Index'])];
end;

{ TParamStrFunc }

procedure TParamStrFunc.Execute;
begin
  Info.Result := VarToStr(Info.Caller.Parameters[Integer(Info['Index'])]);
end;

{ TParamCount }

procedure TParamCountFunc.Execute;
begin
  Info.Result := Length(Info.Caller.Parameters);
end;

function Tdws2Compiler.GetScriptSource(ScriptName: string): string;
var
  path: string;
  sFile: TFileStream;
begin
  Result := '';

  if Assigned(FOnInclude) then
    FOnInclude(ScriptName, Result);

  if Result = '' then
  begin
    path := FindScriptPathForFile(ScriptName);
    sFile := TFileStream.Create(path + ScriptName, fmOpenRead +
      fmShareDenyWrite);
    try
      SetLength(Result, sFile.Size);
      sFile.Read(Result[1], sFile.Size);
    finally
      sFile.Free;
    end;
  end;
end;

function Tdws2Compiler.ReadStringArray(Expr: TExpr;
  IsWrite: Boolean): TExpr;
var
  indexExpr: TExpr;
  pos: TScriptPos;
begin
  pos := FTok.HotPos;
  indexExpr := ReadExpr;
  try
    if not FTok.TestDelete(ttARIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

    if FTok.TestDelete(ttASSIGN) and IsWrite then
      Result := TStringArraySetExpr.Create(FProg, pos, Expr, indexExpr, ReadExpr)
    else
      Result := TStringArrayOpExpr.Create(FProg, pos, TDataExpr(Expr), indexExpr)

  except
    indexExpr.Free;
    raise;
  end;
end;

function Tdws2Compiler.CreateProgram(SystemTable: TSymbolTable;
  ResultType: Tdws2ResultType; MaxDataSize: Integer; StackChunkSize: Integer): TProgram;
begin
  Result := TProgram.Create(SystemTable, ResultType, MaxDataSize, StackChunkSize);
end;

{ Tdws2Filter }

constructor Tdws2Filter.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TStringList.Create;
  FPrivateDependencies := TStringList.Create;
end;

destructor Tdws2Filter.Destroy;
begin
  inherited;
  FDependencies.Free;
  FPrivateDependencies.Free;
end;

function Tdws2Filter.GetDependencies: TStrings;
begin
  FDependencies.Clear;
  FDependencies.AddStrings(FPrivateDependencies);

  // Merge dependencies with subfilter dependencies
  if Assigned(FSubFilter) then
    FDependencies.AddStrings(FSubFilter.Dependencies);

  Result := FDependencies;
end;

procedure Tdws2Filter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSubFilter) then
    SetSubFilter(nil);
end;

function Tdws2Filter.Process(const Text: string; Msgs: TMsgs): string;
begin
  if Assigned(FSubFilter) then
    Result := FSubFilter.Process(Text, Msgs)
  else
    Result := Text;
end;

procedure Tdws2Filter.SetSubFilter(const Filter: Tdws2Filter);
begin
  if Assigned(FSubFilter) then
    FSubFilter.RemoveFreeNotification(Self);

  FSubFilter := Filter;

  if Assigned(FSubFilter) then
    FSubFilter.FreeNotification(Self);
end;


{ Tdws2DefaultResult }


procedure Tdws2DefaultResult.AddString(const Str: string);
begin
  FText := FText + Str;
end;

function Tdws2DefaultResult.GetText: string;
begin
  Result := FText;
end;

{ Tdws2DefaultResultType }

function Tdws2DefaultResultType.CreateProgResult: Tdws2Result;
begin
  Result := Tdws2DefaultResult.Create(Self);
end;

procedure Tdws2DefaultResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  inherited;
  TPrintFunction.Create(SymbolTable, 'Print', ['v', 'Variant'], '');
  TPrintLnFunction.Create(SymbolTable, 'PrintLn', ['v', 'Variant'], '');
end;


{ TPrintFunction }

procedure TPrintFunction.Execute;
begin
  Tdws2DefaultResult(Info.Caller.Result).AddString(VarToStr(Info['v']));
end;

{ TPrintLnFunction }

procedure TPrintLnFunction.Execute;
begin
  Tdws2DefaultResult(Info.Caller.Result).AddString(VarToStr(Info['v']) + #13#10);
end;

function Tdws2Compiler.ReadEnumeration(TypeName: string): TEnumerationSymbol;
var
  name: string;
  elemSym: TElementSymbol;
  constExpr: TExpr;
  enumInt: Integer;
  namePos: TScriptPos;
  isUserDef: Boolean;
begin
  Result := TEnumerationSymbol.Create(TypeName, FProg.TypInteger);
  try
    enumInt := 0;

    repeat
      // Read a member of the enumeration
      if not FTok.TestName then
        FMsgs.AddCompilerStop(FTok.HotPos, CPE_NameExpected);

      name := FTok.GetToken.FString;
      namePos := FTok.HotPos;
      FTok.KillToken;

      // Member has a user defined value
      if FTok.TestDelete(ttEQ) then
      begin
        constExpr := ReadExpr.Optimize;

        if not(constExpr is TConstExpr) then
        begin
          FreeAndNil(constExpr);
          FMsgs.AddCompilerError(FTok.HotPos, CPE_ConstantExpressionExpected);
        end
        else if not(constExpr.Typ = FProg.TypInteger) then
        begin
          FreeAndNil(constExpr);
          FMsgs.AddCompilerError(FTok.HotPos, CPE_IntegerExpressionExpected);
        end;

        if Assigned(constExpr) then
          enumInt := constExpr.Eval;

        isUserDef := True;
      end
      else
        isUserDef := False;

      // Create member symbol
      elemSym := TElementSymbol.Create(name, Result, enumInt, isUserDef);

      Inc(enumInt);

      // Add member symbol to table and enumeration type
      FProg.Table.AddSymbol(elemSym);
      Result.AddElement(elemSym);

      // Add member symbol to Symbol Dictionary
      if coSymbolDictionary in FCompilerOptions then
        FProg.SymbolDictionary.Add(elemSym, namePos, [suDeclaration]);

    until not FTok.TestDelete(ttCOMMA);

    if not FTok.TestDelete(ttBRIGHT) then
      FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);

  except
    Result.Free;
    raise;
  end;
end;

procedure Tdws2Compiler.ReadUses;
var
  Names : TStringList;
  x, y, z, u : Integer;
begin
  Names := TStringList.Create;
  try
    ReadNameList(Names);
    u := 0;
    for x := 0 to Names.Count - 1 do
    begin
      y := 0;
      z := -1;
      while (y < FProg.Root.RootTable.Count) do
      begin
        if (FProg.Root.RootTable[y] is TUnitSymbol) and SameText(FProg.Root.RootTable[y].Name,Names[x]) then
        begin
          z := FProg.Root.RootTable.IndexOfParent(TUnitSymbol(FProg.Root.RootTable[y]).Table);
          if z >= u then // uses A,B,A,C => uses A,B,C
          begin
            FProg.Root.RootTable.MoveParent(z,u);
            Inc(u);
          end;
          Break;
        end;
        Inc(y);
      end;
      if z < 0 then
        FMsgs.AddCompilerStop(FTok.HotPos, Format(CPE_UnknownUnit,[Names[x]]));
    end;
  finally
    Names.Free;
  end;
end;

function Tdws2Compiler.CreateProcedure(Parent : TProgram): TProcedure;
begin
  Result := TProcedure.Create(Parent);
end;

function Tdws2Compiler.ReadSpecialFunction(NamePos: TScriptPos; SpecialKind: TSpecialKind): TExpr;
var
  argExpr: TExpr;
  argTyp: TSymbol;
begin
  if not FTok.TestDelete(ttBLEFT) then
    FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

  // Test for statements like "Low(Integer)"
  if FTok.Test(ttName) and FTok.NextTest(ttBRIGHT) then
    argTyp := FProg.Table.FindSymbol(FTok.GetToken.FString)
  else
    argTyp := nil;

  if Assigned(argTyp) and (argTyp is TTypeSymbol) then
  begin
    argExpr := nil;
    FTok.KillToken;
    FTok.KillToken;
  end
  else
  begin
    argExpr := ReadName;
    argTyp := argExpr.BaseType;
  end;

  try
    if Assigned(argExpr) then
      argExpr.TypeCheck;

    if not Assigned(argTyp) then
      FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);

    Result := nil;

    case SpecialKind of
      skLength:
        begin
          if (argTyp is TDynamicArraySymbol) and Assigned(argExpr) then
            Result := TArrayLengthExpr.Create(FProg, NamePos, TDataExpr(argExpr), 0)
          else if (argTyp = FProg.TypString) and Assigned(argExpr) then
            Result := TStringLengthExpr.Create(FProg, NamePos, argExpr)
          else if argTyp is TStaticArraySymbol then
          begin
            FreeAndNil(argExpr);
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger,
              TStaticArraySymbol(argTyp).ElementCount);
          end
          else
            FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
        end;
      skLow:
        begin
          FreeAndNil(argExpr); // not needed
          if argTyp is TStaticArraySymbol then
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger,
              TStaticArraySymbol(argTyp).LowBound)
          else if argTyp = FProg.TypString then
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger, 1)
          else if (argTyp = FProg.TypInteger) or (argTyp is TDynamicArraySymbol) then
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger, 0)
          else
            FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
        end;
      skHigh:
        begin
          if argTyp is TDynamicArraySymbol and Assigned(argExpr) then
            Result := TArrayLengthExpr.Create(FProg, NamePos, TDataExpr(argExpr), -1)
          else if (argTyp = FProg.TypString) and Assigned(argExpr) then
            Result := TStringLengthExpr.Create(FProg, NamePos, argExpr)
          else if argTyp is TStaticArraySymbol then
          begin
            FreeAndNil(argExpr);
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger,
              TStaticArraySymbol(argTyp).HighBound);
          end
          else if argTyp = FProg.TypInteger then
          begin
            FreeAndNil(argExpr);
            Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger, MaxInt);
          end
          else
            FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);
        end;
      skSizeOf:
        begin
          FreeAndNil(argExpr);
          Result := TConstExpr.Create(FProg, NamePos, FProg.TypInteger, argTyp.Size);
        end;
    end;

    try
      if not FTok.TestDelete(ttBRIGHT) then
        FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
    except
      Result.Free;
      raise;
    end;

  except
    argExpr.Free;
    raise;
  end;
end;

function Tdws2Compiler.ReadTypeCast(NamePos: TScriptPos; TypeSym: TSymbol): TExpr;
var
  argExpr: TExpr;
begin
  if not FTok.TestDelete(ttBLEFT) then
    FMsgs.AddCompilerStop(FTok.HotPos, CPE_BrackLeftExpected);

  Result := nil;

  argExpr := ReadExpr;

  try
    // Cast DateTime(...)
    if TypeSym = FProg.TypDateTime then
      Result := TConvDateTimeExpr.Create(FProg, namePos, argExpr)
    // Cast Integer(...)
    else if TypeSym = FProg.TypInteger then
      Result := TConvIntegerExpr.Create(FProg, namePos, argExpr)
    // Cast Float(...)
    else if TypeSym = FProg.TypFloat then
      Result := TConvFloatExpr.Create(FProg, namePos, argExpr)
    // Cast Variant(...)
    else if TypeSym = FProg.TypVariant then
      Result := TConvVariantExpr.Create(FProg, namePos, argExpr)
    else
      FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_InvalidOperands);

    if not FTok.TestDelete(ttBRIGHT) then
      FProg.Msgs.AddCompilerStop(FTok.HotPos, CPE_BrackRightExpected);
  except
    argExpr.Free;
    raise;
  end;
end;

end.

