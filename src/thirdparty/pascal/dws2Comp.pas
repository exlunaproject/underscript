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
{    Contributor(s): Willibald Krenn, Eric Grange, Andreas Luleich     }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Comp;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}             
  Classes, SysUtils, TypInfo, dws2Compiler, dws2Debugger,
  dws2Exprs, dws2Symbols, dws2Stack, dws2Functions,
  // Built-In functions
{$IFNDEF DWS_NO_BUILTIN_FUNCTIONS}
  dws2MathFunctions, dws2StringFunctions, dws2TimeFunctions, dws2VariantFunctions,
{$ENDIF}
  dws2Errors;

type
  TDelphiWebScriptII = class;

  Tdws2EmptyUnit = class(TComponent, IUnknown, IUnit)
  private
    function GetUnitName: string;
    function GetDependencies: TStrings;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
  protected
    FUnitName: string;
    FDependencies: TStrings;
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  Tdws2UnitComponent = class(Tdws2EmptyUnit)
  private
    FScript: TDelphiWebScriptII;
  protected
    procedure SetScript(const Value: TDelphiWebScriptII);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    destructor Destroy; override;
  published
    property Script: TDelphiWebScriptII read FScript write SetScript;
  end;

  TDelphiWebScriptII = class(Tdws2EmptyUnit)
  private
    FCompiler: Tdws2Compiler;
    FConfig: TConfiguration;
  protected
    function GetOnInclude: TIncludeEvent;
    function GetVersion: string;
    procedure SetConfig(const Value: TConfiguration);
    procedure SetOnInclude(const Value: TIncludeEvent);
    procedure SetVersion(const Value: string);
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddUnit(Un: IUnit);
    function Compile(const Text: string): TProgram; virtual;
    function RemoveUnit(Un: IUnit): Boolean;
  published
    property Config: TConfiguration read FConfig write SetConfig stored True;
    property OnInclude: TIncludeEvent read GetOnInclude write SetOnInclude;
    property Version: string read GetVersion write SetVersion stored False;
  end;

  Tdws2AbstractUnit = class(TComponent, IUnknown, IUnit)
  private
    FDependencies: TStrings;
    FScript: TDelphiWebScriptII;
    FUnitName: string;
    function GetDependencies: TStrings;
    procedure SetDependencies(const Value: TStrings);
    procedure SetScript(const Value: TDelphiWebScriptII);
    procedure SetUnitName(const Value: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetUnitName: string; virtual;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable; virtual; abstract;
    property Dependencies: TStrings read FDependencies write SetDependencies;
    property UnitName: string read GetUnitName write SetUnitName;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Script: TDelphiWebScriptII read FScript write SetScript;
  end;

  TSymbolTableType = (sttDefault, sttStatic, sttLinked);

  Tdws2AbstractStaticUnit = class(Tdws2AbstractUnit)
  private
    FStaticSymbols: Boolean;
    FStaticTable: TStaticSymbolTable;
  protected
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable; override;
    function CreateUnitTable(Parent: TSymbolTable; Typ: TSymbolTableType = sttDefault): TSymbolTable; virtual;
    procedure SetStaticSymbols(const Value: Boolean); // static symbols
    procedure InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable); virtual;
    procedure AddUnitSymbols(Table: TSymbolTable); virtual; abstract;
    property StaticSymbols: Boolean read FStaticSymbols write SetStaticSymbols;
    property StaticTable: TStaticSymbolTable read FStaticTable;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    function InitStaticSymbols(SystemTable: TSymbolTable; UnitSyms: TSymbolTable): Boolean;
    procedure ReleaseStaticSymbols;
  end;

  TDataType = string;
  Tdws2Unit = class;

  Tdws2Symbol = class(TCollectionItem)
  private
    FIsGenerating: Boolean;
    FUnit: Tdws2Unit;
    FName: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckName(Table: TSymbolTable; Name: string);
    function GetDataType(Table: TSymbolTable; Name: string): TTypeSymbol;
    procedure Reset;
    property IsGenerating: Boolean read FIsGenerating;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function Generate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; virtual; abstract;
    function GetNamePath: string; override;
    function GetUnit: Tdws2Unit;
  published
    property Name: string read FName write FName;
  end;

  Tdws2SymbolArray = array of Tdws2Symbol;

  Tdws2SymbolClass = class of Tdws2Symbol;

  Tdws2Collection = class(TOwnedCollection)
  private
    FUnit: Tdws2Unit;
    FSortedSymbols: Tdws2SymbolArray;
    function GetSortedItem(Index: Integer): Tdws2Symbol;
  protected
    class function GetSymbolClass : Tdws2SymbolClass; virtual;
    function GetSymbols(const Name: String): Tdws2Symbol;
    function GetItem(Index: Integer): Tdws2Symbol;
    procedure SetItem(Index: Integer; Value: Tdws2Symbol);
    procedure Reset;
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    function GetUnit: Tdws2Unit;
    function GetSortedItems: Tdws2SymbolArray;
    function IndexOf(const Name: string): Integer;
    property Symbols[const Name: String]: Tdws2Symbol read GetSymbols;
    property Items[Index: Integer]: Tdws2Symbol read GetItem write SetItem;
    property SortedItems[Index: Integer]: Tdws2Symbol read GetSortedItem;
  end;

  Tdws2Variable = class(Tdws2Symbol)
  private
    FDataType: TDataType;
  protected
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DataType: TDataType read FDataType write FDataType;
  end;

  Tdws2Variables = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
    function GetDisplayName: string;
  end;

  Tdws2VariablesClass = class of Tdws2Variables;

  Tdws2Parameter = class(Tdws2Variable)
  private
    FIsVarParam: Boolean;
    FIsWritable: Boolean;
    FDefaultValue: Variant;
    FHasDefaultValue: Boolean;
    procedure SetIsVarParam(const Value: Boolean);
    procedure SetHasDefaultValue(const Value: Boolean);
    procedure SetIsWritable(const Value: Boolean);
  protected
    procedure SetDefaultValue(const Value: Variant);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property IsVarParam: Boolean read FIsVarParam write SetIsVarParam default False;
    property IsWritable: Boolean read FIsWritable write SetIsWritable default True;
    property HasDefaultValue: Boolean read FHasDefaultValue write SetHasDefaultValue default False;
    property DefaultValue: Variant read FDefaultValue write SetDefaultValue;
  end;

  Tdws2Parameters = class(Tdws2Variables)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Function = class;

  TFuncEvalEvent = procedure(Info: TProgramInfo) of object;
  TInitSymbolEvent = procedure(Sender: TObject; Symbol: TSymbol) of object;
  TInitExprEvent = procedure(Sender: TObject; Expr: TExprBase) of object;
  TOptimizeEvent = function(Sender: TObject; FuncExpr: TExprBase): TExprBase of object;

  Tdws2Function = class(Tdws2Symbol, IUnknown, ICallable)
  private
    FOnEval: TFuncEvalEvent;
    FFuncType: TDataType;
    FParameters: Tdws2Parameters;
    FOnInitSymbol: TInitSymbolEvent;
    FOnInitExpr: TInitExprEvent;
    FOnOptimize: TOptimizeEvent;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  protected
    function GetDisplayName: string; override;
    procedure Call(Caller: TProgram; Func: TFuncSymbol); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
    function GetParameters(Table: TSymbolTable): TParamList;
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
    function Optimize(FuncExpr: TExprBase): TExprBase;
  published
    property Parameters: Tdws2Parameters read FParameters write FParameters;
    property ResultType: TDataType read FFuncType write FFuncType;
    property OnEval: TFuncEvalEvent read FOnEval write FOnEval;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
    property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
    property OnOptimize: TOptimizeEvent read FOnOptimize write FOnOptimize;
  end;

  Tdws2Functions = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2FunctionsClass = class of Tdws2Functions;

  Tdws2Array = class(Tdws2Symbol)
  private
    FDataType: TDataType;
    FLowBound: Integer;
    FHighBound: Integer;
  protected
    procedure SetIsDynamic(const Value: Boolean);
    function GetIsDynamic: Boolean;
    function GetDisplayName: string; override;
    function GetBoundStored: Boolean;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
    procedure Assign(Source: TPersistent); override;
  published
    property DataType: TDataType read FDataType write FDataType;
    property LowBound: Integer read FLowBound write FLowBound stored GetBoundStored;
    property HighBound: Integer read FHighBound write FHighBound stored GetBoundStored;
    property IsDynamic: Boolean read GetIsDynamic write SetIsDynamic default False;
  end;

  Tdws2Arrays = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2ArraysClass = class of Tdws2Arrays;

  Tdws2Constant = class(Tdws2Variable)
  protected
    FValue: Variant;
    function GetDisplayName: string; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property Value: Variant read FValue write FValue;
  end;

  Tdws2Constants = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2ConstantsClass = class of Tdws2Constants;

  Tdws2Forward = class(Tdws2Symbol)
  protected
    function GetDisplayName: string; override;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  Tdws2Forwards = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2ForwardsClass = class of Tdws2Forwards;

  Tdws2Field = class(Tdws2Variable)
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  Tdws2Fields = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Property = class(Tdws2Symbol)
  private
    FDataType: TDataType;
    FReadAccess: string;
    FWriteAccess: string;
    FParameters: Tdws2Parameters;
    FIsDefault: Boolean;
    FIndexType: TDataType;
    FIndexValue: Variant;
  protected
    function GetDisplayName: string; override;
    function GetIsDefault: Boolean;
    procedure SetIsDefault(Value: Boolean);
    procedure SetParameters(const Value: Tdws2Parameters);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property DataType: TDataType read FDataType write FDataType;
    property ReadAccess: string read FReadAccess write FReadAccess;
    property WriteAccess: string read FWriteAccess write FWriteAccess;
    property Parameters: Tdws2Parameters read FParameters write SetParameters;
    property IsDefault: Boolean read GetIsDefault write SetIsDefault;
    property IndexType: TDataType read FIndexType write FIndexType;
    property IndexValue: Variant read FIndexValue write FIndexValue;
  end;

  Tdws2Properties = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;


  TAssignExternalObjectEvent = procedure(Info: TProgramInfo; var ExtObject: TObject) of object;
  TMethodEvalEvent = procedure(Info: TProgramInfo; ExtObject: TObject) of object;

  Tdws2Method = class(Tdws2Function)
  private
    FAttributes: TMethodAttributes;
    FKind: TMethodKind;
    FOnEval: TMethodEvalEvent;
    FResultType: TDataType;
    procedure SetResultType(const Value: TDataType);
  protected
    function GetDisplayName: string; override;
    procedure Call(Caller: TProgram; Func: TFuncSymbol); override;
  public
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
    property Kind: TMethodKind read FKind write FKind;
    property OnEval: TMethodEvalEvent read FOnEval write FOnEval;
    property ResultType: TDataType read FResultType write SetResultType;
  end;

  Tdws2Methods = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Constructor = class(Tdws2Function)
  private
    FAttributes: TMethodAttributes;
    FOnAssignExternalObject: TAssignExternalObjectEvent;
    FOnEval: TMethodEvalEvent;
    function GetResultType: string;
  protected
    function GetDisplayName: string; override;
    procedure Call(Caller: TProgram; Func: TFuncSymbol); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property Attributes: TMethodAttributes read FAttributes write FAttributes default [];
    property OnAssignExternalObject: TAssignExternalObjectEvent read FOnAssignExternalObject write FOnAssignExternalObject;
    property OnEval: TMethodEvalEvent read FOnEval write FOnEval;
    property ResultType: string read GetResultType;
  end;

  Tdws2Constructors = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Class = class(Tdws2Symbol)
  private
    FAncestor: string;
    FConstructors: Tdws2Constructors;
    FFields: Tdws2Fields;
    FMethods: Tdws2Methods;
    FOnObjectDestroy: TObjectDestroyEvent;
    FProperties: Tdws2Properties;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Ancestor: string read FAncestor write FAncestor;
    property Constructors: Tdws2Constructors read FConstructors write FConstructors;
    property Fields: Tdws2Fields read FFields write FFields;
    property Methods: Tdws2Methods read FMethods write FMethods;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
    property Properties: Tdws2Properties read FProperties write FProperties;
  end;

  Tdws2Classes = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2ClassesClass = class of Tdws2Classes;

  Tdws2Member = class(Tdws2Variable)
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  Tdws2Members = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Record = class(Tdws2Symbol)
  private
    FMembers: Tdws2Members;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  published
    property Members: Tdws2Members read FMembers write FMembers;
  end;

  Tdws2Records = class(Tdws2Collection)
  protected
    class function GetSymbolClass: Tdws2SymbolClass; override;
  end;

  Tdws2RecordsClass = class of Tdws2Records;

  Tdws2Element = class(Tdws2Symbol)
  private
    FIsUserDef: Boolean;
    FUserDefValue: Integer;
    procedure SetUserDefValue(const Value: Integer);
    procedure SetIsUserDef(const Value: Boolean);
  protected
    function GetDisplayName: string; override;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property UserDefValue: Integer read FUserDefValue write SetUserDefValue;
    property IsUserDef: Boolean read FIsUserDef write SetIsUserDef;
  end;

  Tdws2Elements = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2Enumeration = class(Tdws2Symbol)
  private
    FElements: Tdws2Elements;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
  published
    property Elements: Tdws2Elements read FElements write FElements;
  end;

  Tdws2Enumerations = class(Tdws2Collection)
  protected
    class function GetSymbolClass: Tdws2SymbolClass; override;
  end;

  Tdws2EnumerationsClass = class of Tdws2Enumerations;

  TReadVarEvent = procedure(var Value: Variant) of object;
  TWriteVarEvent = procedure(Value: Variant) of object;
  TInstantiateEvent = procedure(var ExtObject: TObject) of object;

  Tdws2Global = class(Tdws2Variable)
  private
    FOnReadVar: TReadVarEvent;
    FOnWriteVar: TWriteVarEvent;
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol; override;
    procedure Assign(Source: TPersistent); override;
  published
    property OnReadVar: TReadVarEvent read FOnReadVar write FOnReadVar;
    property OnWriteVar: TWriteVarEvent read FOnWriteVar write FOnWriteVar;
  end;

  Tdws2Instances = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2InstancesClass = class of Tdws2Instances;

  Tdws2CustomInstance = class(Tdws2Variable)
  private
    FOnObjectDestroy: TObjectDestroyEvent;
    FOnInstantiate: TInstantiateEvent;
    FAutoDestroyExternalObject: Boolean;
    FOnInitSymbol: TInitSymbolEvent;
    FOnInitExpr: TInitExprEvent;
//    FOnInitialize: TInitializeEvent;
    FOnOptimize: TOptimizeEvent;
  protected
    procedure DoDestroy(ExternalObject: TObject); virtual;
    procedure DoInstantiate(var ExternalObject: TObject); virtual;
    function DoOptimize(Sender: TObject; FuncExpr: TExprBase): TExprBase; virtual;
    procedure DoInitSymbol(Sender: TObject; Symbol: TSymbol); virtual;
    procedure DoInitExpr(Sender: TObject; Expr: TExprBase); virtual;
  public
    constructor Create(Collection: TCollection); override;
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
    procedure Assign(Source: TPersistent); override;
    property AutoDestroyExternalObject: Boolean read FAutoDestroyExternalObject
      write FAutoDestroyExternalObject default False;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write
      FOnObjectDestroy;
    property OnInstantiate: TInstantiateEvent read FOnInstantiate write
      FOnInstantiate;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
    property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
    property OnOptimize: TOptimizeEvent read FOnOptimize write FOnOptimize;
  end;

  Tdws2Instance = class(Tdws2CustomInstance)
  published
    property AutoDestroyExternalObject;
    property OnObjectDestroy;
    property OnInstantiate;
    property OnInitSymbol;
    property OnInitExpr;
    property OnOptimize;
  end;

  Tdws2Synonyms = class(Tdws2Collection)
  protected
    class function GetSymbolClass : Tdws2SymbolClass; override;
  end;

  Tdws2SynonymsClass = class of Tdws2Synonyms;

  Tdws2Synonym = class(Tdws2Variable)
  public
    function DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil): TSymbol;
      override;
  end;

  Tdws2Unit = class(Tdws2AbstractStaticUnit)
  private
    FArrays: Tdws2Arrays;
    FClasses: Tdws2Classes;
    FConstants: Tdws2Constants;
    FEnumerations: Tdws2Enumerations;
    FForwards: Tdws2Forwards;
    FFunctions: Tdws2Functions;
    FInstances: Tdws2Instances;
    FRecords: Tdws2Records;
    FSynonyms: Tdws2Synonyms;
    FVariables: Tdws2Variables;
    FTable: TSymbolTable;
  protected
    FCollections: array[0..9] of Tdws2Collection;
    class function GetArraysClass: Tdws2ArraysClass; virtual;
    class function GetClassesClass: Tdws2ClassesClass; virtual;
    class function GetConstantsClass: Tdws2ConstantsClass; virtual;
    class function GetEnumerationsClass: Tdws2EnumerationsClass; virtual;
    class function GetForwardsClass: Tdws2ForwardsClass; virtual;
    class function GetFunctionsClass: Tdws2FunctionsClass; virtual;
    class function GetInstancesClass: Tdws2InstancesClass; virtual;
    class function GetRecordsClass: Tdws2RecordsClass; virtual;
    class function GetVariablesClass: Tdws2VariablesClass; virtual;
    class function GetSynonymsClass: Tdws2SynonymsClass; virtual;
    procedure SetArrays(const Value: Tdws2Arrays);
    procedure SetClasses(const Value: Tdws2Classes);
    procedure SetConstants(const Value: Tdws2Constants);
    procedure SetEnumerations(const Value: Tdws2Enumerations);
    procedure SetForwards(const Value: Tdws2Forwards);
    procedure SetFunctions(const Value: Tdws2Functions);
    procedure SetRecords(const Value: Tdws2Records);
    procedure SetVariables(const Value: Tdws2Variables);
    procedure SetInstances(const Value: Tdws2Instances);
    procedure SetSynonyms(const Value: Tdws2Synonyms);
  protected
    function GetSymbol(Table: TSymbolTable; const Name: string): TSymbol;
    procedure AddCollectionSymbols(Collection: Tdws2Collection; Table: TSymbolTable); virtual;
    procedure AddUnitSymbols(Table: TSymbolTable); override;
    procedure InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable); override;

    // Method to support get/set property values for dynamicly registered classes
    procedure HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
    procedure HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);
  public
    procedure GetDataTypes(List: TStrings);
    procedure GetClassTypes(List: TStrings);
    procedure ExposeClassToUnit(AClass, AAncestor: TClass; ASearchProgram: TProgram=nil; const ScriptAncestorType: string='');
    procedure ExposeInstanceToUnit(const AName, AClassType: string; AInstance: TObject);
    property Table: TSymbolTable read FTable;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Arrays: Tdws2Arrays read FArrays write SetArrays;
    property Classes: Tdws2Classes read FClasses write SetClasses;
    property Constants: Tdws2Constants read FConstants write SetConstants;
    property Dependencies;
    property Enumerations: Tdws2Enumerations read FEnumerations write SetEnumerations;
    property Forwards: Tdws2Forwards read FForwards write SetForwards;
    property Functions: Tdws2Functions read FFunctions write SetFunctions;
    property Instances: Tdws2Instances read FInstances write SetInstances;
    property Records: Tdws2Records read FRecords write SetRecords;
    property Synonyms: Tdws2Synonyms read FSynonyms write SetSynonyms;
    property UnitName;
    property Variables: Tdws2Variables read FVariables write SetVariables;
    property StaticSymbols;
  end;

  TCustomInstantiateFunc = class(TAnonymousFunction, IObjectOwner)
  protected
    FClassSym: TClassSymbol;
    FScriptObj: IScriptObj;
  public
    procedure ReleaseObject;
    property ClassSym: TClassSymbol read FClassSym write FClassSym;
  end;

  TDynamicInstantiateFunc = class(TCustomInstantiateFunc)
  protected
    FExternalObject: TObject;
  public
    constructor Create(FuncSym: TFuncSymbol; AExternalObject: TObject); reintroduce; virtual;
    procedure Execute; override;
  end;

  TInstantiateFunc = class(TCustomInstantiateFunc)
  private
    FOnInstantiate: TInstantiateEvent;
    FOnObjectDestroy: TObjectDestroyEvent;
    FOnInitSymbol: TInitSymbolEvent;
    FOnInitExpr: TInitExprEvent;
    FOnOptimize: TOptimizeEvent;
  public
    procedure Execute; override;
    procedure InitSymbol(Symbol: TSymbol); override;
    procedure InitExpression(Expr: TExprBase); override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
    property OnInstantiate: TInstantiateEvent read FOnInstantiate write FOnInstantiate;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
    property OnInitSymbol: TInitSymbolEvent read FOnInitSymbol write FOnInitSymbol;
    property OnInitExpr: TInitExprEvent read FOnInitExpr write FOnInitExpr;
    property OnOptimize: TOptimizeEvent read FOnOptimize write FOnOptimize;
  end;

  TReadVarEventFunc = class(TAnonymousFunction)
  private
    FOnReadVar: TReadVarEvent;
  public
    procedure Execute; override;
    property OnReadVar: TReadVarEvent read FOnReadVar write FOnReadVar;
  end;

  TWriteVarEventFunc = class(TAnonymousFunction)
  private
    FOnWriteVar: TWriteVarEvent;
  public
    procedure Execute; override;
    property OnWriteVar: TWriteVarEvent read FOnWriteVar write FOnWriteVar;
  end;

  TReadVarFunc = class(TAnonymousFunction)
  private
    FData: TData;
    FTyp: TSymbol;
  public
    constructor Create(FuncSym: TFuncSymbol);
    procedure Execute; override;
    procedure SetValue(const Data: TData);
  end;

  TWriteVarFunc = class(TAnonymousFunction)
  private
    FReadVarFunc: TReadVarFunc;
  public
    constructor Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
    procedure Execute; override;
  end;

// Return the external object for a variable name.
function GetExternalObjForID(Info: TProgramInfo; const AVarName: string): TObject;

// Get or create the DWS object ID (like a pointer) for a Delphi object instance.
//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: string = ''): Integer;

function GetParameters(Symbol: Tdws2Symbol;
  Parameters: Tdws2Parameters; Table: TSymbolTable): TParamList;

implementation

uses
  dws2Strings, dws2CompStrings;

type
  EGenerationError = class(Exception);
  EHandledGenerationError = class(Exception);

function ValueToString(const Value : Variant) : String;
begin
  case VarType(Value) of
    varEmpty : Result := 'Unassigned';
    varNull : Result := 'Null';
    varString, varOleStr, varStrArg : Result := Format('''%s''', [VarToStr(Value)]);
    varDate : Result := Format('DateTime(%f)', [TVarData(Value).VDate]);
  else
    Result := VarToStr(Value);
  end;
end;

function GetExternalObjForID(Info: TProgramInfo; const AVarName: string): TObject;
begin
  // Get param "Source" as object in Source_Obj
  Result := IScriptObj(IUnknown(Info[AVarName])).ExternalObject;
end;

//function GetOrCreateObjectID(Info: TProgramInfo; AObject: TObject; AClassName: string): Integer;
//var
//  ScriptObj: TScriptObj;
//begin
//  if Assigned(AObject) then                // if object was returned
//  begin
//    if AClassName = '' then
//      AClassName := AObject.ClassName;
//
//    // Find the Delphi object and return the Id
//    ScriptObj := Info.Caller.FindExternalObject(AObject);
//    if Assigned(ScriptObj) then            // if object found
//      Result := ScriptObj.Id               // return the object's Id
//    else                                   // if not found, register the object and return the Id
//      Result := Info.Vars[AClassName].GetConstructor('Create', AObject).Call.Value;
//  end
//  else                                     // no object returned
//    Result := 0;                           // return 'nil' Id
//end;

{ TDelphiWebScriptII }

constructor TDelphiWebScriptII.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUnitName := 'Default';
  FCompiler := Tdws2Compiler.Create;
  FConfig := TConfiguration.Create(Self);
  AddUnit(Self);
end;

destructor TDelphiWebScriptII.Destroy;
begin
  inherited;
  FCompiler.Free;
  FConfig.Free;
end;

function TDelphiWebScriptII.GetVersion: string;
begin
  Result := '2.0.beta.1';
end;

procedure TDelphiWebScriptII.SetVersion(const Value: string);
begin
  // This method is needed to make the IDE show the version in
  // the object inspector
end;

function TDelphiWebScriptII.Compile(const Text: string): TProgram;
begin
  Result := FCompiler.Compile(Text, FConfig);
end;

procedure TDelphiWebScriptII.AddUnit(Un: IUnit);
begin
  RemoveUnit(Un);
  if Assigned(Un) then
    FConfig.Units.AddObject(Un.GetUnitName, Pointer(Un));
end;

function TDelphiWebScriptII.RemoveUnit(Un: IUnit): Boolean;
var
  x: Integer;
begin
  x := FConfig.Units.IndexOfObject(Pointer(Un));
  if x >= 0 then
    FConfig.Units.Delete(x);
  Result := x >= 0;
end;

procedure TDelphiWebScriptII.SetConfig(const Value: TConfiguration);
begin
  FConfig.Assign(Value);
end;

// Implementation of Tdws2EmptyUnit.AddUnitSymbols
procedure TDelphiWebScriptII.AddUnitSymbols(SymbolTable: TSymbolTable);
begin
  // The TDelphiWebScriptII component is the unit "Default"
  Config.ResultType.AddResultSymbols(SymbolTable);
end;

procedure TDelphiWebScriptII.SetOnInclude(const Value: TIncludeEvent);
begin
  Config.OnInclude := Value;
end;

function TDelphiWebScriptII.GetOnInclude: TIncludeEvent;
begin
  Result := Config.OnInclude;
end;

procedure TDelphiWebScriptII.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FConfig.Filter then
      Config.Filter := nil
    else if AComponent = Config.ResultType then
      Config.ResultType := nil
    else if AComponent is Tdws2UnitComponent then
      Self.RemoveUnit(Tdws2UnitComponent(AComponent))
    else if AComponent is Tdws2AbstractUnit then
      Self.RemoveUnit(Tdws2AbstractUnit(AComponent));
  end;
end;

{ Tdws2Collection }

constructor Tdws2Collection.Create;
begin
  inherited Create(AOwner, GetSymbolClass);
  if AOwner is Tdws2Unit then
    FUnit := Tdws2Unit(AOwner)
  else if AOwner is Tdws2Symbol then
    FUnit := Tdws2Symbol(AOwner).GetUnit
  else
    FUnit := nil;

  FSortedSymbols := nil;
end;

function Tdws2Collection.GetOwner: TPersistent;
begin
  Result := inherited GetOwner;
end;

function Tdws2Collection.GetUnit: Tdws2Unit;
begin
  Result := FUnit;
end;

function Tdws2Collection.GetItem(Index: Integer): Tdws2Symbol;
begin
  Result := Tdws2Symbol(inherited Items[Index]);
end;

procedure Tdws2Collection.Reset;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Items[x].Reset;
  setlength(FSortedSymbols,0);
  FSortedSymbols := nil;
end;

procedure Tdws2Collection.SetItem(Index: Integer; Value: Tdws2Symbol);
begin
  Items[Index].Assign(Value);
  setlength(FSortedSymbols,0);
  FSortedSymbols := nil;
end;

function Tdws2Collection.GetSymbols(const Name: String): Tdws2Symbol;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
  begin
    Result := Items[x];
    if SameText(Result.Name,Name) then
      Exit;
  end;
  Result := nil;
end;

class function Tdws2Collection.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Symbol;
end;

function Tdws2Collection.GetSortedItems: Tdws2SymbolArray;
var
  x: Integer;
  FSortedItems: TStringList;
begin
  if not assigned(FSortedSymbols) then
  begin
    FSortedItems := TStringList.Create;
    FSortedItems.Sorted := true;
    FSortedItems.Duplicates := dupAccept;

    for x := 0 to Count - 1 do
      FSortedItems.AddObject(Items[x].Name,Items[x]);

    SetLength(FSortedSymbols,FSortedItems.Count);
    for x := Count - 1 downto 0 do
    begin
      FSortedSymbols[x] := Tdws2Symbol(FSortedItems.Objects[x]);
      FSortedItems.Objects[x] := nil;
    end;

    FSortedItems.Free;
  end;

  result := FSortedSymbols;
end;

function Tdws2Collection.GetSortedItem(Index: Integer): Tdws2Symbol;
begin
  result := GetSortedItems[Index];
end;

function Tdws2Collection.IndexOf(const Name: string): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to Self.Count - 1 do
    if SameText(Name, Items[x].Name) then
    begin
      Result := x;
      Break;
    end;
end;

{ Tdws2Unit }

procedure Tdws2Unit.AddCollectionSymbols(Collection: Tdws2Collection;
  Table: TSymbolTable);
var
  y: Integer;
begin
  for y := 0 to Collection.Count - 1 do
  begin
    if not Tdws2Symbol(Collection.Items[y]).IsGenerating then
    try
      Tdws2Symbol(Collection.Items[y]).Generate(Table);
    except
      on e: Exception do
        raise EGenerationError.CreateFmt(UNT_UnitGenerationError, [UnitName,
          e.Message]);
    end;
  end;
end;

procedure Tdws2Unit.AddUnitSymbols(Table: TSymbolTable);
var
  x: Integer;
begin
  for x := Low(FCollections) to High(FCollections) do
    FCollections[x].Reset;

  for x := Low(FCollections) to High(FCollections) do
    AddCollectionSymbols(FCollections[x], Table);
end;

constructor Tdws2Unit.Create(AOwner: TComponent);
begin
  inherited;
  FArrays := GetArraysClass.Create(Self);
  FClasses := GetClassesClass.Create(Self);
  FConstants := GetConstantsClass.Create(Self);
  FEnumerations := GetEnumerationsClass.Create(Self);
  FForwards := GetForwardsClass.Create(Self);
  FFunctions := GetFunctionsClass.Create(Self);
  FRecords := GetRecordsClass.Create(Self);
  FVariables := GetVariablesClass.Create(Self);
  FInstances := GetInstancesClass.Create(Self);
  FSynonyms := GetSynonymsClass.Create(Self);
  FCollections[0] := FForwards;
  FCollections[1] := FArrays;
  FCollections[2] := FClasses;
  FCollections[3] := FRecords;
  FCollections[4] := FEnumerations;
  FCollections[5] := FSynonyms;
  FCollections[6] := FFunctions;
  FCollections[7] := FVariables;
  FCollections[8] := FConstants;
  FCollections[9] := FInstances;
end;

destructor Tdws2Unit.Destroy;
begin
  FArrays.Free;
  FClasses.Free;
  FConstants.Free;
  FEnumerations.Free;
  FForwards.Free;
  FRecords.Free;
  FFunctions.Free;
  FVariables.Free;
  FInstances.Free;
  FSynonyms.Free;
  inherited;
end;

procedure Tdws2Unit.GetClassTypes(List: TStrings);
var
  x: Integer;
begin
  if not Assigned(List) then
    Exit;

  if Assigned(FScript) then
    for x := 0 to FScript.Config.SystemTable.Count - 1 do
    begin
      if FScript.Config.SystemTable[x] is TClassSymbol then
        List.Add(Script.Config.SystemTable[x].Name);
    end;

  for x := 0 to FClasses.Count - 1 do
    List.Add(FClasses.Items[x].Name);
end;

procedure Tdws2Unit.GetDataTypes(List: TStrings);
var
  x, y: Integer;
  coll: Tdws2Collection;
begin
  if not Assigned(List) then
    Exit;

  if Assigned(FScript) then
    // Add all type symbols from the systemtable
    for x := 0 to FScript.Config.SystemTable.Count - 1 do
    begin
      if FScript.Config.SystemTable[x] is TTypeSymbol then
        List.Add(FScript.Config.SystemTable[x].Name);
    end;

  // Only return array-, record- and class symbols, synonyms and enums
  for x := 1 to 5 do
  begin
    coll := FCollections[x];
    for y := 0 to coll.Count - 1 do
      List.Add(coll.Items[y].Name);
  end;
end;

function Tdws2Unit.GetSymbol(Table: TSymbolTable; const Name: string): TSymbol;
var
  x, y: Integer;
  item: Tdws2Symbol;
  coll: Tdws2Collection;
begin
  Result := Table.FindSymbol(Name);
  if not Assigned(Result) then
    for x := Low(FCollections) to High(FCollections) do
    begin
      // Check if the symbol is defined but not yet generated
      coll := FCollections[x];
      for y := 0 to coll.Count - 1 do
        if SameText(coll.Items[y].Name, Name) then
        begin
          item := coll.Items[y];

          // Check for circular references
          if item.IsGenerating then
            raise Exception.CreateFmt(UNT_CircularReference, [Name]);

          // Generate the symbol now
          try
            Result := item.Generate(Table);
          except
            on e: Exception do
              raise EHandledGenerationError.Create(e.Message);
          end;

          Exit;
        end;
    end;
end;

procedure Tdws2Unit.SetArrays(const Value: Tdws2Arrays);
begin
  FArrays.Assign(Value);
end;

procedure Tdws2Unit.SetClasses(const Value: Tdws2Classes);
begin
  FClasses.Assign(Value);
end;

procedure Tdws2Unit.SetConstants(const Value: Tdws2Constants);
begin
  FConstants.Assign(Value);
end;

procedure Tdws2Unit.SetForwards(const Value: Tdws2Forwards);
begin
  FForwards.Assign(Value);
end;

procedure Tdws2Unit.SetFunctions(const Value: Tdws2Functions);
begin
  FFunctions.Assign(Value);
end;

procedure Tdws2Unit.SetRecords(const Value: Tdws2Records);
begin
  FRecords.Assign(Value);
end;

procedure Tdws2Unit.SetVariables(const Value: Tdws2Variables);
begin
  FVariables.Assign(Value);
end;

procedure Tdws2Unit.SetEnumerations(const Value: Tdws2Enumerations);
begin
  FEnumerations.Assign(Value);
end;

procedure Tdws2Unit.SetInstances(const Value: Tdws2Instances);
begin
  FInstances.Assign(Value);
end;

class function Tdws2Unit.GetFunctionsClass: Tdws2FunctionsClass;
begin
  Result := Tdws2Functions;
end;

class function Tdws2Unit.GetArraysClass: Tdws2ArraysClass;
begin
  Result := Tdws2Arrays;
end;

class function Tdws2Unit.GetClassesClass: Tdws2ClassesClass;
begin
  Result := Tdws2Classes;
end;

class function Tdws2Unit.GetConstantsClass: Tdws2ConstantsClass;
begin
  Result := Tdws2Constants;
end;

class function Tdws2Unit.GetEnumerationsClass: Tdws2EnumerationsClass;
begin
  Result := Tdws2Enumerations;
end;

class function Tdws2Unit.GetForwardsClass: Tdws2ForwardsClass;
begin
  Result := Tdws2Forwards;
end;

class function Tdws2Unit.GetInstancesClass: Tdws2InstancesClass;
begin
  Result := Tdws2Instances;
end;

class function Tdws2Unit.GetRecordsClass: Tdws2RecordsClass;
begin
  Result := Tdws2Records;
end;

class function Tdws2Unit.GetVariablesClass: Tdws2VariablesClass;
begin
  Result := Tdws2Variables;
end;

class function Tdws2Unit.GetSynonymsClass: Tdws2SynonymsClass;
begin
  Result := Tdws2Synonyms;
end;

procedure Tdws2Unit.SetSynonyms(const Value: Tdws2Synonyms);
begin
  FSynonyms.Assign(Value);
end;

procedure Tdws2Unit.HandleDynamicProperty(Info: TProgramInfo; ExtObject: TObject);
var
  propName: string;
  param: TParamSymbol;
  setValue: Variant;
begin
  { NOTE: Special handling is required for Boolean types. At least with Delphi 5 }
  if (Info.FuncSym is TMethodSymbol) and Assigned(ExtObject) then
  begin
    propName := Copy(Info.FuncSym.Name, 4, Length(Info.FuncSym.Name));  // get property name. Trim off Get/Set prefix
    case TMethodSymbol(Info.FuncSym).Kind of
    fkFunction  :   // function is a "Get" method
      begin
        { Return property value for property GetXXX function }
        // Class
        if Info.FuncSym.Typ is TClassSymbol then   // don't free the object instance returned
          Info.Result := Info.RegisterExternalObject(GetObjectProp(ExtObject, propName), False, False)  // wrap as best we can (find a match)
        // Boolean
        else if SameText(Info.FuncSym.Typ.Name, SYS_BOOLEAN) then
          Info.Result := Boolean(GetOrdProp(ExtObject, propName))
        // All others
        else
          Info.Result := GetPropValue(ExtObject, propName);
      end;
    fkProcedure :   // procedure is a "Set" method
      begin
        // Set property value for property SetXXX function
        if Info.FuncSym.Params.Count > 0 then
        begin
          param := Info.FuncSym.Params.Symbols[0] as TParamSymbol;
          // fetch param value by name
          VarCopy(setValue, Info.Data[ param.Name ][0]);
          // Class
          if param.Typ is TClassSymbol then
            SetObjectProp(ExtObject, propName, Info.GetExternalObjForVar(param.Name))
          // Boolean
          else if VarType(setValue) = varBoolean then
            SetOrdProp(ExtObject, propName, Integer(setValue))
          // All others
          else
            SetPropValue(ExtObject, propName, setValue);
        end;
      end;
    end;
  end;
end;


{ AClass is the class to expose to the unit. All published properties of standard
  simple datatypes that are supported in DWS will be exposed that were introduced
  between AAncestor and AClass. The ScriptAncestorType is the type that will be
  used for the new Script class inherited class type. If none is provided then
  AAncestor.ClassName is used. }
procedure Tdws2Unit.ExposeClassToUnit(AClass, AAncestor: TClass;  ASearchProgram: TProgram; const ScriptAncestorType: string);

    { Determine if the type is available to the program. If so, add the owning
       unit as a dependency. }
    function IsTypeSupported(const ATypeName: string): Boolean;
    var
      x: Integer;
      list: TStringList;
    begin
      Result := False;
      // if given a compiled program to search through for type declarations
      if Assigned(ASearchProgram) then
      begin
        for x := 0 to ASearchProgram.Table.Count - 1 do
        begin
          if ASearchProgram.Table.Symbols[x] is TUnitSymbol then
            // unit has the type declared
            if TUnitSymbol(ASearchProgram.Table.Symbols[x]).Table.FindLocal(ATypeName) <> nil then
            begin
              Result := True;
              // add the declaring unit as a dependency
              if Self.Dependencies.IndexOf(ASearchProgram.Table.Symbols[x].Name) < 0 then
                Self.Dependencies.Add(ASearchProgram.Table.Symbols[x].Name);
            end;
        end;
      end
      // No compiled program provided. Look up type locally
      else
      begin
        list := TStringList.Create;
        try
          Self.GetDataTypes(list);
          Result := list.IndexOf(ATypeName) >= 0;
        finally
          list.Free;
        end;
      end;
    end;

var
  newForward: Tdws2Forward;
  useClass: Tdws2Class;
  newCreate: Tdws2Constructor;
  newMeth: Tdws2Method;
  TypeData: PTypeData;
  propTypeData: PTypeData;
  PropList: PPropList;
  PropertyName, PropertyType: String;
  i: Integer;
  Include: Boolean;
  getMethName, setMethName: string;
  propIsDefault: Boolean;
begin
  if not Assigned(AClass) then
    EXIT;

  // Look for the class. If found use it, otherwise create new.
  useClass := Tdws2Class(Classes.Symbols[AClass.ClassName]);
  if not Assigned(useClass) then
  begin
    // Create the class declaration
    useClass := Classes.Add as Tdws2Class;
    if ScriptAncestorType <> '' then
      useClass.Ancestor := ScriptAncestorType
    else
      useClass.Ancestor := AAncestor.ClassName;
    useClass.Name := AClass.ClassName;
    newCreate := Tdws2Constructor(useClass.Constructors.Add);
    newCreate.Name := 'Create';
    newCreate.OnAssignExternalObject := HandleDynamicCreate;

    // Create a forward for the class. Handles all issues with class nestings and orderings
    newForward := Tdws2Forward(Forwards.Add);
    newForward.Name := useClass.Name;
  end;

  { Adds the published property names in AClass which are delcared from AAncestor }
  TypeData := GetTypeData(AClass.ClassInfo);
  New(PropList);
  try
    GetPropInfos(AClass.ClassInfo, PropList);
    for i := 0 to Pred(TypeData^.PropCount) do
    begin
      PropertyName := PropList^[i]^.Name;
      propIsDefault := WordBool(PropList^[i]^.Default);
      propTypeData := GetTypeData(PropList^[i]^.PropType^);

      Include := True;
      if IsTypeSupported(PropList^[i]^.PropType^.Name) then
        PropertyType := PropList^[i]^.PropType^.Name
      else
      begin
        { NOTE: Could attempt to use the actual type name (ex: TComponentName is a string).
          This would require trying to find the type when it is not yet compiled
          or risking using the type name without the alias being declared.
          It is easiest and safest to just support the standard native types. }
        case PropList^[i]^.PropType^.Kind of
        tkInteger : PropertyType := SYS_INTEGER;
        tkFloat : PropertyType := SYS_FLOAT;
        tkString, tkLString, tkWString : PropertyType := SYS_STRING;
        tkVariant : PropertyType := SYS_VARIANT;
        tkEnumeration :    // Booleans are reported as enumerations. Only support booleans
          begin
            if propTypeData^.BaseType^ = TypeInfo(Boolean) then
              PropertyType := SYS_BOOLEAN
            else
              Include := False;
          end;
        { TODO : How to support TDateTime? }
        //  CreateGetSetMethodsForType(newClass, SYS_DATETIME);
        else
          Include := False;
        end;
      end;

      { Include property if it does not exist in AAncestor class. }
      // NOTE: In D5, TObject.ClassInfo = nil... would cause AV errors. First test for a valid pointer
      if Include and (AAncestor.ClassInfo <> nil) then
        Include := (GetPropInfo(AAncestor, PropertyName) = nil);

      // if set to include and property not already added
      if Include then 
      begin
        getMethName := 'Get'+PropertyName;
        setMethName := 'Set'+PropertyName;
        // Don't add if already present
        if useClass.Methods.Symbols[getMethName] = nil then
        begin
          // read value
          newMeth := (useClass.Methods.Add as Tdws2Method);
          newMeth.Name := getMethName;
          newMeth.ResultType := PropertyType;
          newMeth.OnEval := HandleDynamicProperty;
          // write value
          newMeth := (useClass.Methods.Add as Tdws2Method);
          newMeth.Name := setMethName;
          newMeth.OnEval := HandleDynamicProperty;
          with newMeth.Parameters.Add as Tdws2Parameter do
          begin
            Name := 'Value';
            DataType := PropertyType;
          end;
        end;
        // Create the property that uses the methods
        with useClass.Properties.Add as Tdws2Property do
        begin
          Name := PropertyName;
          DataType := PropertyType;
          ReadAccess := getMethName;
          WriteAccess := setMethName;
          IsDefault := propIsDefault;
        end;
      end;{if Include}
    end;{for i}
  finally
    Dispose(PropList);
  end;
end;

procedure Tdws2Unit.ExposeInstanceToUnit(const AName, AClassType: string;
  AInstance: TObject);
var
  typSym: TSymbol;
  instFunc: TDynamicInstantiateFunc;
  externalVar: TExternalVarSymbol;
  funcSym: TFuncSymbol;
begin
  { CheckName }
  if AName = '' then
    raise Exception.Create(UNT_NameIsEmpty);
  if Assigned(Table.FindLocal(AName)) then
    raise Exception.CreateFmt(UNT_NameAlreadyExists, [AName]);

  typSym := Table.FindLocal(AClassType);
  // Get the type symbol of this variable
  if not (typSym is TTypeSymbol) then
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [Name]);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    instFunc := TDynamicInstantiateFunc.Create(funcSym, AInstance);
    Table.AddObjectOwner(instFunc);
    instFunc.ClassSym := TClassSymbol(typSym);
    funcSym.Executable := ICallable(instFunc);

    externalVar := TExternalVarSymbol.Create(Name, typSym);
    externalVar.ReadFunc := funcSym;
    Table.AddSymbol(externalVar);
  end
  else
    raise Exception.CreateFmt(UNT_AutoInstantiateWithoutClass, [AClassType]);
end;

procedure Tdws2Unit.InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
begin
  FTable := UnitTable;
  try
    inherited InitUnitTable(SystemTable, UnitSyms, UnitTable);
  finally
    FTable := nil;
  end;
end;

procedure Tdws2Unit.HandleDynamicCreate(Info: TProgramInfo; var ExtObject: TObject);
begin
  { TODO : If accepted, create a string declaration in appropriate unit. }
  raise Exception.CreateFmt('Cannot create dynamic class "%s". Must be obtained from supported objects.', [Info.ScriptObj.GetClassSym.Name]);
end;

{ Tdws2Constant }

procedure Tdws2Constant.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Constant then
    FValue := Tdws2Constant(Source).Value;
end;

function Tdws2Constant.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TConstSymbol.Create(Name, GetDataType(Table, DataType), Value);

  GetUnit.Table.AddSymbol(Result);
end;

function Tdws2Constant.GetDisplayName: string;
begin
  Result := Format('const %s: %s = %s', [Name, DataType, VarToStr(Value)]);
end;

{ Tdws2Variable }

procedure Tdws2Variable.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Variable then
    FDataType := Tdws2Variable(Source).DataType;
end;

function Tdws2Variable.GetDisplayName: string;
begin
  Result := Name + ' : ' + DataType;
end;

{ Tdws2Variables }

function Tdws2Variables.GetDisplayName: string;
var
  i: Integer;
begin
  if Count > 0 then
  begin
    Result := Items[0].GetDisplayName;
    for i := 1 to Count - 1 do
      Result := Result + '; ' + Items[i].GetDisplayName;
  end
  else
    Result := '';
end;

class function Tdws2Variables.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Global;
end;

{ Tdws2Global }

function Tdws2Global.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
var
  typSym: TTypeSymbol;
  readEventFunc: TReadVarEventFunc;
  writeEventFunc: TWriteVarEventFunc;
  readFunc: TReadVarFunc;
  funcSym: TFuncSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  // Get the type symbol of this variable
  typSym := GetDataType(Table, DataType);

  if (Assigned(FOnReadVar) or Assigned(FOnWriteVar)) then
  begin
    Result := TExternalVarSymbol.Create(Name, typSym);

    if Assigned(FOnReadVar) then
    begin
      funcSym := TFuncSymbol.Create('', fkFunction, 1);
      funcSym.Typ := typSym;

      readEventFunc := TReadVarEventFunc.Create(funcSym);
      readEventFunc.OnReadVar := FOnReadVar;

      funcSym.Executable := ICallable(readEventFunc);

      TExternalVarSymbol(Result).ReadFunc := funcSym;
    end;

    if Assigned(FOnWriteVar) then
    begin
      funcSym := TFuncSymbol.Create('', fkProcedure, 1);
      funcSym.AddParam(TParamSymbol.Create('Value', typSym));

      writeEventFunc := TWriteVarEventFunc.Create(funcSym);
      writeEventFunc.OnWriteVar := FOnWriteVar;

      funcSym.Executable := ICallable(writeEventFunc);

      TExternalVarSymbol(Result).WriteFunc := funcSym;
    end;
  end
  else
  begin
    Result := TExternalVarSymbol.Create(Name, typSym);

    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    readFunc := TReadVarFunc.Create(funcSym);
    TExternalVarSymbol(Result).ReadFunc := funcSym;

    funcSym := TFuncSymbol.Create('', fkProcedure, 1);
    funcSym.AddParam(TParamSymbol.Create('Value', typSym));
    TWriteVarFunc.Create(funcSym, readFunc);
    TExternalVarSymbol(Result).WriteFunc := funcSym;
  end;

  GetUnit.Table.AddSymbol(Result);
end;

procedure Tdws2Global.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Global then
  begin
    FOnReadVar := Tdws2Global(Source).OnReadVar;
    FOnWriteVar := Tdws2Global(Source).OnWriteVar;
  end;
end;

{ TInstantiateFunc }

procedure TInstantiateFunc.Execute;
var
  scriptObj: TScriptObj;
  extObj: TObject;
begin
  if Assigned(FScriptObj) then
    // Instance was already created
    Info.Result := FScriptObj
  else
  begin
    // First access to this variable. Create object instance!
    scriptObj := TScriptObj.Create(FClassSym{, Info.Caller});
    scriptObj.OnObjectDestroy := FOnObjectDestroy;
    FScriptObj := scriptObj;

    FOnInstantiate(extObj);
    FScriptObj.ExternalObject := extObj;

    Info.Result := FScriptObj;
  end;
end;

procedure TInstantiateFunc.InitSymbol(Symbol: TSymbol);
begin
  inherited;
  if Assigned(FOnInitSymbol) then
    FOnInitSymbol(Self,Symbol);
end;

procedure TInstantiateFunc.InitExpression(Expr: TExprBase);
begin
  inherited;
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr);
end;

{
procedure TInstantiateFunc.Initialize;
begin
  inherited;
  if Assigned(FOnInitialize) then
    FOnInitialize(Self);
end;
}

function TInstantiateFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  if Assigned(FOnOptimize) then
    Result := FOnOptimize(Self,FuncExpr)
  else
    Result := inherited Optimize(FuncExpr);
end;

{ Tdws2Parameter }

procedure Tdws2Parameter.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Parameter then
  begin
    FIsVarParam := Tdws2Parameter(Source).IsVarParam;
    FIsWritable := Tdws2Parameter(Source).IsWritable;
  end;
end;

constructor Tdws2Parameter.Create(Collection: TCollection);
begin
  inherited;
  FIsWritable := True;
  FIsVarParam := False;
  FDefaultValue := Unassigned;
  FHasDefaultValue := False;
end;

function Tdws2Parameter.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
var ParamSym : TParamSymbol;
begin
  FIsGenerating := True;
  if IsVarParam then
    ParamSym := TVarParamSymbol.Create(Name, GetDataType(Table, DataType),IsWritable)
  else
    ParamSym := TParamSymbol.Create(Name, GetDataType(Table, DataType));
  if HasDefaultValue then
    ParamSym.SetDefaultValue(DefaultValue);
  Result := ParamSym;
end;

function Tdws2Parameter.GetDisplayName: string;
begin
  if IsVarParam then
    if IsWritable then
      Result := 'var ' + inherited GetDisplayName
    else
      Result := 'const ' + inherited GetDisplayName
  else
    Result := inherited GetDisplayName;
  if HasDefaultValue then
    Result := Result + Format(' = %s',[ValueToString(DefaultValue)]);
end;

procedure Tdws2Parameter.SetDefaultValue(const Value: Variant);
begin
  FDefaultValue := Value;
  FHasDefaultValue := not (FIsVarParam and FIsWritable);
end;

procedure Tdws2Parameter.SetHasDefaultValue(const Value: Boolean);
begin
  FHasDefaultValue := Value and not (FIsVarParam and FIsWritable);
end;

procedure Tdws2Parameter.SetIsVarParam(const Value: Boolean);
begin
  FIsVarParam := Value;
  if FIsVarParam and FIsWritable then
    FHasDefaultValue := False;
end;

procedure Tdws2Parameter.SetIsWritable(const Value: Boolean);
begin
  FIsWritable := Value;
  if FIsVarParam and FIsWritable then
    FHasDefaultValue := False;
end;

{ Tdws2Function }

constructor Tdws2Function.Create;
begin
  inherited;
  FParameters := Tdws2Parameters.Create(Self);
end;

destructor Tdws2Function.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure Tdws2Function.Call(Caller: TProgram; Func: TFuncSymbol);
var
  info: TProgramInfo;
begin
  if Assigned(FOnEval) then
  begin
    info := TProgramInfo.Create(Func.Params, Caller);
    try
      info.FuncSym := Func;
      FOnEval(info);
    finally
      info.Free;
    end;
  end;
end;

function Tdws2Function._AddRef: Integer;
begin
  Result := -1;
end;

function Tdws2Function._Release: Integer;
begin
  Result := -1;
end;

function Tdws2Function.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function Tdws2Function.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  if ResultType <> '' then
    GetDataType(Table, ResultType);

  Result := TFuncSymbol.Generate(Table, Name, GetParameters(Table), ResultType);
  try
    TFuncSymbol(Result).Params.AddParent(Table);

    // Connect Tdws2Function to TFuncSymbol
    TFuncSymbol(Result).Executable := ICallable(Self);
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

function GetParameters(Symbol: Tdws2Symbol;
  Parameters: Tdws2Parameters; Table: TSymbolTable): TParamList;
var
  x, y: Integer;
  name: string;
begin
  SetLength(Result, Parameters.Count);
  for x := 0 to Parameters.Count - 1 do
  begin
    name := Parameters.Items[x].Name;

    // Check wether parameter name is unique
    for y := x - 1 downto 0 do
    begin
      if SameText(Result[y].ParamName, name) then
        raise Exception.CreateFmt(UNT_ParameterNameAlreadyExists, [name]);
    end;

    Result[x].IsVarParam := Tdws2Parameter(Parameters.Items[x]).IsVarParam;
    Result[x].IsWritable := Tdws2Parameter(Parameters.Items[x]).IsWritable;
    Result[x].ParamName := name;
    Result[x].ParamType := Tdws2Parameter(Parameters.Items[x]).DataType;
    Result[x].HasDefaultValue := Tdws2Parameter(Parameters.Items[x]).HasDefaultValue;
    if Result[x].HasDefaultValue then
    begin
      SetLength(Result[x].DefaultValue,1);
      Result[x].DefaultValue[0] := Tdws2Parameter(Parameters.Items[x]).DefaultValue;
    end
    else
      Result[x].DefaultValue := nil;

    Symbol.GetUnit.GetSymbol(Table, Result[x].ParamType);
  end;
end;


function Tdws2Function.GetParameters(Table: TSymbolTable): TParamList;
begin
  Result := dws2Comp.GetParameters(Self,Parameters,Table);
end;

function Tdws2Function.GetDisplayName: string;
begin
  Result := Parameters.GetDisplayName;

  if Result <> '' then
    Result := '(' + Result + ')';

  if ResultType = '' then
    Result := Format('procedure %s%s;', [Name, Result])
  else
    Result := Format('function %s%s : %s;', [Name, Result, ResultType]);
end;

procedure Tdws2Function.InitSymbol(Symbol: TSymbol);
begin
  if Assigned(FOnInitSymbol) then
    FOnInitSymbol(Self,Symbol);
end;

procedure Tdws2Function.InitExpression(Expr: TExprBase);
begin
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr);
end;

{
procedure Tdws2Function.Initialize;
begin
  if Assigned(FOnInitialize) then
    FOnInitialize(Self);
end;
}

function Tdws2Function.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  if Assigned(FOnOptimize) then
    Result := FOnOptimize(Self,FuncExpr)
  else
    Result := FuncExpr;
end;

procedure Tdws2Function.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Function then
  begin
    FFuncType := Tdws2Function(Source).ResultType;
    FParameters.Assign(Tdws2Function(Source).Parameters);
  end;
end;

{ Tdws2Field }

function Tdws2Field.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);
  Result := TFieldSymbol.Create(Name, GetDataType(Table, DataType));
end;

{ Tdws2Method }

function Tdws2Method.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);

  if ResultType <> '' then
    GetUnit.GetSymbol(Table, ResultType);

  Result := TMethodSymbol.Generate(Table, Kind, Attributes, Name,
    GetParameters(Table), ResultType, TClassSymbol(ParentSym));
  try
    TFuncSymbol(Result).Params.AddParent(Table);

    TMethodSymbol(Result).Executable := ICallable(Self);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Method.GetDisplayName: string;
begin
  Result := Parameters.GetDisplayName;

  if Result <> '' then
    Result := '(' + Result + ')';

  case FKind of
    mkProcedure:
      Result := Format('procedure %s%s;', [Name, Result]);
    mkFunction:
      Result := Format('function %s%s : %s;', [Name, Result, ResultType]);
    mkConstructor:
      Result := Format('constructor %s%s;', [Name, Result]);
    mkDestructor:
      Result := Format('destructor %s%s;', [Name, Result]);
    mkClassProcedure:
      Result := Format('class procedure %s%s;', [Name, Result]);
    mkClassFunction:
      Result := Format('class function %s%s : %s;', [Name, Result, ResultType]);
  else
    Assert(false); // if triggered, this func needs upgrade !
  end;
end;

procedure Tdws2Method.SetResultType(const Value: TDataType);
begin
  FResultType := Value;
  if Value <> '' then
    case FKind of
      mkProcedure:
        FKind := mkFunction;
      mkClassProcedure:
        FKind := mkClassFunction;
    end
  else
    case FKind of
      mkFunction:
        FKind := mkProcedure;
      mkClassFunction:
        FKind := mkClassProcedure;
    end;
end;

procedure Tdws2Method.Call(Caller: TProgram; Func: TFuncSymbol);
var
  info: TProgramInfo;
  scriptObj: IScriptObj;
begin
  if Assigned(FOnEval) then
  begin
    info := TProgramInfo.Create(Func.Params, Caller);
    try
      info.FuncSym := Func;

      if not (Func as TMethodSymbol).IsClassMethod then
        scriptObj := info.Vars['Self'].ScriptObj;

      info.ScriptObj := scriptObj;

      if Assigned(scriptObj) then
        FOnEval(info, scriptObj.ExternalObject)
      else
        FOnEval(info, nil);

    finally
      info.Free;
    end;
  end;
end;

procedure Tdws2Method.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Method then
  begin
    FAttributes := Tdws2Method(Source).Attributes;
    FKind := Tdws2Method(Source).Kind;
    FResultType := Tdws2Method(Source).ResultType;
  end;
end;

{ Tdws2Constructor }

procedure Tdws2Constructor.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Method then
    FAttributes := Tdws2Method(Source).Attributes;
end;

procedure Tdws2Constructor.Call(Caller: TProgram; Func: TFuncSymbol);
var
  info: TProgramInfo;
  extObj: TObject;
  scriptObj: IScriptObj;
begin
  info := TProgramInfo.Create(Func.Params, Caller);
  try
    info.FuncSym := Func;

    scriptObj := info.Vars['Self'].ScriptObj;
    info.ScriptObj := scriptObj;

    if Assigned(FOnAssignExternalObject) then
      if Assigned(scriptObj) then
      begin
        extObj := scriptObj.ExternalObject; // may assigned by Info.GetConstructor()
        FOnAssignExternalObject(info, extObj);
        scriptObj.ExternalObject := extObj;
      end;

    if Assigned(FOnEval) then
    begin
      if Assigned(scriptObj) then
        FOnEval(info, scriptObj.ExternalObject)
      else
        FOnEval(info, nil);
    end
  finally
    info.Free;
  end;
end;

constructor Tdws2Constructor.Create(Collection: TCollection);
begin
  inherited;
  // Name the first constructor "Create" by default
  if Collection.Count = 1 then
    FName := 'Create';
end;

function Tdws2Constructor.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(TClassSymbol(ParentSym).Members, Name);

  Result := TMethodSymbol.Generate(Table, mkConstructor, Attributes, Name,
    GetParameters(Table), '', TClassSymbol(ParentSym));
  try
    TFuncSymbol(Result).Params.AddParent(Table);
    TMethodSymbol(Result).Executable := ICallable(Self);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Constructor.GetDisplayName: string;
begin
  Result := Parameters.GetDisplayName;

  if Result <> '' then
    Result := '(' + Result + ')';

  Result := Format('constructor %s%s;', [Name, Result]);
end;

function Tdws2Constructor.GetResultType: string;
begin
  // Hides the property "ResultType" in the object inspector
  Result := '';
end;

{ Tdws2Class }

procedure Tdws2Class.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Class then
  begin
    FAncestor := Tdws2Class(Source).Ancestor;
    FFields.Assign(Tdws2Class(Source).Fields);
    FMethods.Assign(Tdws2Class(Source).Methods);
    FProperties.Assign(Tdws2Class(Source).Properties);
  end;
end;

constructor Tdws2Class.Create(Collection: TCollection);
begin
  inherited;
  FFields := Tdws2Fields.Create(Self);
  FConstructors := Tdws2Constructors.Create(Self);
  FMethods := Tdws2Methods.Create(Self);
  FProperties := Tdws2Properties.Create(Self);
end;

destructor Tdws2Class.Destroy;
begin
  FFields.Free;
  FConstructors.Free;
  FMethods.Free;
  FProperties.Free;
  inherited;
end;

function Tdws2Class.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
var
  x: Integer;
  ancestorSym: TClassSymbol;
begin
  FIsGenerating := True;

  Result := GetUnit.Table.FindSymbol(Name);

  if Assigned(Result) then
  begin
    if Result is TClassSymbol then
    begin
      if not TClassSymbol(Result).IsForward then
        raise Exception.Create(UNT_ClassAlreadyDefined);
    end
    else
      raise Exception.CreateFmt(UNT_ClassNameAlreadyDefined, [Name,
        Result.Caption]);
  end;

  try

    if not Assigned(Result) then
      Result := TClassSymbol.Create(Name);

    TClassSymbol(Result).OnObjectDestroy := FOnObjectDestroy;

    if FAncestor = '' then
      FAncestor := SYS_TOBJECT;

    ancestorSym := TClassSymbol(GetUnit.GetSymbol(Table, FAncestor));
    if ancestorSym = nil then
      raise Exception.CreateFmt(UNT_SuperClassUnknwon, [FAncestor]);

    TClassSymbol(Result).InheritFrom(ancestorSym);

    for x := 0 to FFields.Count - 1 do
      TClassSymbol(Result).AddField(TFieldSymbol(Tdws2Field(FFields.Items[x]).Generate(Table, Result)));

    for x := 0 to FConstructors.Count - 1 do
      TClassSymbol(Result).AddMethod(TMethodSymbol(Tdws2Constructor(FConstructors.Items[x]).Generate(Table, Result)));

    for x := 0 to FMethods.Count - 1 do
      TClassSymbol(Result).AddMethod(TMethodSymbol(Tdws2Method(FMethods.Items[x]).Generate(Table, Result)));

    for x := 0 to FProperties.Count - 1 do
      TClassSymbol(Result).AddProperty(TPropertySymbol(Tdws2Property(FProperties.Items[x]).Generate(Table, Result)));

  except
    if not TClassSymbol(Result).IsForward then
      Result.Free;
    raise;
  end;

  if TClassSymbol(Result).IsForward then
    TClassSymbol(Result).IsForward := false
      // The symbol is already in the symbol table
  else
    GetUnit.Table.AddSymbol(Result);
end;

function Tdws2Class.GetDisplayName: string;
begin
  if Ancestor <> '' then
    Result := Name + ' (' + Ancestor + ')'
  else
    Result := Name + ' (TObject)';
end;

{ Tdws2Member }

function Tdws2Member.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(TRecordSymbol(ParentSym).Members, Name);
  Result := TMemberSymbol.Create(Name, GetDataType(Table, DataType));
end;

{ Tdws2Record }

procedure Tdws2Record.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Record then
    FMembers := Tdws2Record(Source).Members;
end;

constructor Tdws2Record.Create;
begin
  inherited;
  FMembers := Tdws2Members.Create(Self);
end;

destructor Tdws2Record.Destroy;
begin
  FMembers.Free;
  inherited;
end;

function Tdws2Record.DoGenerate;
var
  x: Integer;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TRecordSymbol.Create(Name);
  try
    for x := 0 to FMembers.Count - 1 do
      TRecordSymbol(Result).AddMember(TMemberSymbol(Tdws2Member(FMembers.Items[x]).Generate(Table, Result)));
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Record.GetDisplayName: string;
begin
  Result := 'Record ' + Name;
end;

{ Tdws2Array }

procedure Tdws2Array.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Array then
  begin
    FDataType := Tdws2Array(Source).DataType;
    FLowBound :=  Tdws2Array(Source).LowBound;
    FHighBound :=  Tdws2Array(Source).HighBound;
  end;
end;

function Tdws2Array.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  if (LowBound = 0) and (HighBound = -1) then
    Result := TDynamicArraySymbol.Create(Name, GetDataType(Table, DataType))
  else
  begin
    if LowBound > HighBound then
      raise Exception.Create(UNT_InvalidArrayBounds);
    Result := TStaticArraySymbol.Create(Name, GetDataType(Table, DataType), LowBound, HighBound);
  end;
  GetUnit.Table.AddSymbol(Result);
end;

function Tdws2Array.GetBoundStored: Boolean;
begin
  Result := not IsDynamic;
end;

function Tdws2Array.GetDisplayName: string;
begin
  if IsDynamic then
    Result := Format('%s = array of %s', [Name, DataType])
  else
    Result := Format('%s = array [%d .. %d] of %s', [Name, LowBound, HighBound,
      DataType]);
end;

function Tdws2Array.GetIsDynamic: Boolean;
begin
  Result := (FLowBound = 0) and (FHighBound = -1);
end;

procedure Tdws2Array.SetIsDynamic(const Value: Boolean);
begin
  if Value then
  begin
    FLowBound := 0;
    FHighBound := -1;
  end
  else if IsDynamic then
    FHighBound := 0;
end;

{ Tdws2Property }

procedure Tdws2Property.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Property then
  begin
    FDataType := Tdws2Property(Source).DataType;
    FReadAccess := Tdws2Property(Source).ReadAccess;
    FWriteAccess := Tdws2Property(Source).WriteAccess;
    FParameters.Assign(Tdws2Property(Source).Parameters);
    FIsDefault := Tdws2Property(Source).IsDefault;
  end;
end;

constructor Tdws2Property.Create(Collection: TCollection);
begin
  inherited;
  FParameters := Tdws2Parameters.Create(Self);
end;

destructor Tdws2Property.Destroy;
begin
  FParameters.Free;
  inherited;
end;

function Tdws2Property.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol = nil):
  TSymbol;
var
  sym: TSymbol;
  propSym: TPropertySymbol;
  indexData: TData;
begin
  FIsGenerating := True;

  propSym := TPropertySymbol.Create(Name, GetDataType(Table, DataType));
  Result := PropSym;

  propSym.GenerateParams(Table,GetParameters(Self, Parameters, Table));

  if FReadAccess <> '' then
  begin
    // ReadAccess
    sym := TClassSymbol(ParentSym).Members.FindLocal(FReadAccess);

    if not Assigned(sym) then
      raise Exception.CreateFmt(UNT_ReadAccessNotFound, [ReadAccess]);

    propSym.ReadSym := sym;
  end;

  if FWriteAccess <> '' then
  begin
    // WriteAccess
    sym := TClassSymbol(ParentSym).Members.FindLocal(FWriteAccess);

    if not Assigned(sym) then
      raise Exception.CreateFmt(UNT_WriteAccessNotFound, [WriteAccess]);

    propSym.WriteSym := sym;
  end;

  if FIndexType <> '' then
  begin
    SetLength(indexData,1);
    indexData[0] := FIndexValue;
    propSym.SetIndex(indexData,0,GetDataType(Table, IndexType));
  end;

  if IsDefault then
    TClassSymbol(ParentSym).DefaultProperty := propSym;
end;

function Tdws2Property.GetDisplayName: string;
var
  Params: string;
  Index : String;
begin
  if FParameters.Count > 0 then
    Params := '[' + FParameters.GetDisplayName + ']';
  if IndexType <> '' then
    Index := Format(' index %s',[ValueToString(IndexValue)]);
  if (ReadAccess = '') and (WriteAccess = '') then
    Result := Format('property %s%s: %s%s;', [Name, Params, DataType, Index])
  else if (ReadAccess = '') and (WriteAccess <> '') then
    Result := Format('property %s%s: %s%s write %s;', [Name, Params, DataType, Index, WriteAccess])
  else if (ReadAccess <> '') and (WriteAccess = '') then
    Result := Format('property %s%s: %s%s read %s;', [Name, Params, DataType, Index, ReadAccess])
  else
    Result := Format('property %s%s: %s%s read %s write %s;', [Name, Params, DataType, Index,
      ReadAccess, WriteAccess]);
  if IsDefault then
    Result := Result + ' default;';
end;

function Tdws2Property.GetIsDefault: Boolean;
begin
  Result := FIsDefault and (Parameters.Count > 0);
end;

procedure Tdws2Property.SetIsDefault(Value: Boolean);
var
  i: Integer;
  properties: Tdws2Properties;
begin
  Value := Value and (Parameters.Count > 0);
  if IsDefault <> Value then
  begin
    FIsDefault := Value;
    if FIsDefault then
    begin
      properties := Tdws2Class(Tdws2Collection(Collection).GetOwner).Properties;
      for i := 0 to properties.Count - 1 do
        if properties.Items[i] <> Self then
          Tdws2Property(properties.Items[i]).FIsDefault := False;
    end;
  end;
end;

procedure Tdws2Property.SetParameters(const Value: Tdws2Parameters);
begin
  FParameters.Assign(Value);
end;

{ Tdws2Symbol }

constructor Tdws2Symbol.Create(Collection: TCollection);
begin
  inherited;
  FUnit := Tdws2Collection(Collection).GetUnit;
end;

function Tdws2Symbol.GetUnit: Tdws2Unit;
begin
  Result := FUnit;
end;

procedure Tdws2Symbol.Reset;
begin
  FIsGenerating := false;
end;

function Tdws2Symbol.GetNamePath: string;
begin
  if FName <> '' then
    Result := Collection.GetNamePath + FName
  else
    Result := Collection.GetNamePath + IntToStr(Index);
end;

function Tdws2Symbol.Generate;
begin
  try
    Result := DoGenerate(Table, ParentSym);
  except
    on e: EHandledGenerationError do
      raise;
    on e: Exception do
      raise Exception.CreateFmt(UNT_SymbolGenerationError, [ClassName, Name,
        e.Message]);
  end;
end;

function Tdws2Symbol.GetDataType(Table: TSymbolTable; Name: string): TTypeSymbol;
var sym : TSymbol;
begin
  sym := GetUnit.GetSymbol(Table, Name);
  if not (sym is TTypeSymbol) then
    raise Exception.CreateFmt(UNT_DatatypeUnknown, [Name]);
  Result := TTypeSymbol(sym);
end;

procedure Tdws2Symbol.CheckName(Table: TSymbolTable;
  Name: string);
begin
  if Name = '' then
    raise Exception.Create(UNT_NameIsEmpty);

  if Assigned(Table.FindLocal(Name)) then
    raise Exception.CreateFmt(UNT_NameAlreadyExists, [Name]);
end;

procedure Tdws2Symbol.Assign(Source: TPersistent);
begin
  if Source is Tdws2Symbol then
    FName := Tdws2Symbol(Source).Name
  else
    inherited;
end;

procedure Tdws2Symbol.AssignTo(Dest: TPersistent);
begin
  if Dest is Tdws2Symbol then
    Tdws2Symbol(Dest).Name := Name
  else
    inherited;
end;

{ Tdws2Forward }

function Tdws2Forward.DoGenerate;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TClassSymbol.Create(Name);
  TClassSymbol(Result).IsForward := True;
  GetUnit.Table.AddSymbol(Result);
end;

function Tdws2Forward.GetDisplayName: string;
begin
  Result := Format('type %s = class;', [Name]);
end;

{ TReadVarEventFunc }

procedure TReadVarEventFunc.Execute;
var
  Value: Variant;
begin
  VarClear(Value);
  if Assigned(FOnReadVar) then
    FOnReadVar(Value);
  Info.Result := Value;
end;

{ TWriteVarEventFunc }

procedure TWriteVarEventFunc.Execute;
begin
  if Assigned(FOnWriteVar) then
    FOnWriteVar(Info['Value']);
end;

{ TReadVarFunc }

constructor TReadVarFunc.Create(FuncSym: TFuncSymbol);
begin
  inherited;
  FTyp := FuncSym.Typ;
  SetLength(FData, FTyp.Size);
  FTyp.InitData(FData, 0);
end;

procedure TReadVarFunc.Execute;
begin
  Info.Data[SYS_RESULT] := FData;
end;

procedure TReadVarFunc.SetValue(const Data: TData);
begin
  CopyData(Data, 0, FData, 0, FTyp.Size);
end;

{ TWriteVarFunc }

constructor TWriteVarFunc.Create(FuncSym: TFuncSymbol; ReadVarFunc: TReadVarFunc);
begin
  inherited Create(FuncSym);
  FReadVarFunc := ReadVarFunc;
end;

procedure TWriteVarFunc.Execute;
begin
  FReadVarFunc.SetValue(Info.Data['Value']);
end;

{ Tdws2Component }

constructor Tdws2AbstractUnit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependencies := TStringList.Create;
end;

destructor Tdws2AbstractUnit.Destroy;
begin
  Script := nil;
  FDependencies.Free;
  inherited;
end;

function Tdws2AbstractUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function Tdws2AbstractUnit.GetUnitName: string;
begin
  Result := FUnitName;
end;

procedure Tdws2AbstractUnit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FScript) then
    FScript := nil;
end;

procedure Tdws2AbstractUnit.SetDependencies(const Value: TStrings);
begin
  FDependencies.Assign(Value);
end;

procedure Tdws2AbstractUnit.SetScript(const Value: TDelphiWebScriptII);
begin
  if Assigned(FScript) then
  begin
    FScript.RemoveUnit(Self);
    FScript.RemoveFreeNotification(Self);
  end;

  FScript := Value;

  if Assigned(FScript) then
  begin
    FScript.AddUnit(Self);
    FScript.FreeNotification(Self);
  end;
end;

procedure Tdws2AbstractUnit.SetUnitName(const Value: string);
begin
  if not (csDesigning in ComponentState) and Assigned(FScript)
     and not SameText(Value,FUnitName) then
    raise Exception.Create(UNT_CantChangeUnitName)
  else
    FUnitName := Value;
end;

{ Tdws2EmptyUnit }

constructor Tdws2EmptyUnit.Create(AOwner: TComponent);
begin
  inherited;
  FDependencies := TStringList.Create;
end;

destructor Tdws2EmptyUnit.Destroy;
begin
  inherited;
  FDependencies.Free;
end;

function Tdws2EmptyUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function Tdws2EmptyUnit.GetUnitName: string;
begin
  Result := FUnitName;
end;

function Tdws2EmptyUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
var
  x: Integer;
  sym: TSymbol;
begin
  Result := TSymbolTable.Create(SystemTable);
  try
    // insert links to units this unit depends of
    for x := 0 to FDependencies.Count - 1 do
    begin
      sym := UnitSyms.FindSymbol(FDependencies[x]);
      Result.AddParent(TUnitSymbol(sym).Table);
      Result.AddSymbol(TUnitSymbol.Create(TUnitSymbol(sym).Name,
        TUnitSymbol(sym).Table));
    end;

    // create the symbols of this unit
    AddUnitSymbols(Result);

  except
    Result.Free;
    raise;
  end;
end;

{ Tdws2EmptyUnit }

destructor Tdws2UnitComponent.Destroy;
begin
  Script := nil;
  inherited;
end;

procedure Tdws2UnitComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FScript) then
    FScript := nil;
end;

procedure Tdws2UnitComponent.SetScript(const Value: TDelphiWebScriptII);
begin
  if Assigned(FScript) then
  begin
    FScript.RemoveUnit(Self);
    FScript.RemoveFreeNotification(Self);
  end;

  FScript := Value;

  if Assigned(FScript) then
  begin
    FScript.AddUnit(Self);
    FScript.FreeNotification(Self);
  end;
end;

{ Tdws2Enumeration }

procedure Tdws2Enumeration.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2Enumeration then
    FElements := Tdws2Enumeration(Source).Elements;
end;

constructor Tdws2Enumeration.Create(Collection: TCollection);
begin
  inherited;
  FElements := Tdws2Elements.Create(Self);
end;

destructor Tdws2Enumeration.Destroy;
begin
  FElements.Free;
  inherited;
end;

function Tdws2Enumeration.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
var
  x: Integer;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  Result := TEnumerationSymbol.Create(Name, Table.FindSymbol(SYS_INTEGER) as TTypeSymbol);
  try
    for x := 0 to FElements.Count - 1 do
      TEnumerationSymbol(Result).AddElement(
        TElementSymbol(Tdws2Element(FElements.Items[x]).Generate(Table, Result)));
    GetUnit.Table.AddSymbol(Result);
  except
    Result.Free;
    raise;
  end;
end;

function Tdws2Enumeration.GetDisplayName: string;
var
  x: Integer;
begin
  Result := Name + ' = (';
  for x := 0 to FElements.Count - 1 do
  begin
    if x <> 0 then
      Result := Result + ', ';
    Result := Result + FElements.Items[x].Name;
  end;
  Result := Result + ')';
end;

{ Tdws2Element }

function Tdws2Element.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
var
  enumInt: Integer;
  enumSym: TEnumerationSymbol;
begin
  FIsGenerating := True;
  enumSym := TEnumerationSymbol(ParentSym);

  CheckName(enumSym.Elements, Name);

  if FIsUserDef then
    enumInt := FUserDefValue
  else if enumSym.Elements.Count > 0 then
    enumInt := TElementSymbol(enumSym.Elements[enumSym.Elements.Count - 1]).Data[0] + 1
  else
    enumInt := 0;

  Result := TElementSymbol.Create(Name, enumSym, enumInt, FIsUserDef);
  GetUnit.Table.AddSymbol(Result);
end;

function Tdws2Element.GetDisplayName: string;
begin
  if FIsUserDef then
    Result := Name + ' = ' + IntToStr(FUserDefValue)
  else
    Result := Name;
end;

procedure Tdws2Element.SetIsUserDef(const Value: Boolean);
begin
  FIsUserDef := Value;
  if not Value then
    FUserDefValue := 0;
end;

procedure Tdws2Element.SetUserDefValue(const Value: Integer);
begin
  FIsUserDef := True;
  FUserDefValue := Value;
end;

{ Tdws2CustomInstance }

procedure Tdws2CustomInstance.Assign(Source: TPersistent);
begin
  inherited;
  if Source is Tdws2CustomInstance then
  begin
    FOnInstantiate := Tdws2CustomInstance(Source).OnInstantiate;
    FOnObjectDestroy := Tdws2CustomInstance(Source).OnObjectDestroy;
    FAutoDestroyExternalObject := Tdws2CustomInstance(Source).AutoDestroyExternalObject;
  end;
end;

constructor Tdws2CustomInstance.Create(Collection: TCollection);
begin
  inherited;
  FAutoDestroyExternalObject := False;
end;

procedure Tdws2CustomInstance.DoDestroy(ExternalObject: TObject);
begin
  if Assigned(FOnObjectDestroy) then
    FOnObjectDestroy(ExternalObject);
  if FAutoDestroyExternalObject then
    ExternalObject.Free;
end;

function Tdws2CustomInstance.DoGenerate(Table: TSymbolTable;
  ParentSym: TSymbol): TSymbol;
var
  typSym: TSymbol;
  instFunc: TInstantiateFunc;
  funcSym: TFuncSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);

  // Get the type symbol of this variable
  typSym := GetDataType(Table, DataType);

  if typSym is TClassSymbol then
  begin
    funcSym := TFuncSymbol.Create('', fkFunction, 1);
    funcSym.Typ := typSym;

    instFunc := TInstantiateFunc.Create(funcSym);
    GetUnit.Table.AddObjectOwner(instFunc);
    instFunc.FClassSym := TClassSymbol(typSym);
    instFunc.OnInstantiate := DoInstantiate;
    instFunc.OnObjectDestroy := DoDestroy;
    instFunc.OnOptimize := DoOptimize;
    instFunc.OnInitSymbol := DoInitSymbol;
    instFunc.OnInitExpr := DoInitExpr;
    funcSym.Executable := ICallable(instFunc);

    Result := TExternalVarSymbol.Create(Name, typSym);
    TExternalVarSymbol(Result).ReadFunc := funcSym;
  end
  else
    raise Exception.CreateFmt(UNT_AutoInstantiateWithoutClass, [DataType]);

  GetUnit.Table.AddSymbol(Result);
end;

procedure Tdws2CustomInstance.DoInitSymbol(Sender: TObject; Symbol: TSymbol);
begin
  if Assigned(FOnInitSymbol) then
    FOnInitSymbol(Self,Symbol)
end;

procedure Tdws2CustomInstance.DoInitExpr(Sender: TObject; Expr: TExprBase);
begin
  if Assigned(FOnInitExpr) then
    FOnInitExpr(Self,Expr)
end;

procedure Tdws2CustomInstance.DoInstantiate(var ExternalObject: TObject);
begin
  if Assigned(FOnInstantiate) then
    FOnInstantiate(ExternalObject);
end;

function Tdws2CustomInstance.DoOptimize(Sender: TObject;
  FuncExpr: TExprBase): TExprBase;
begin
  if Assigned(FOnOptimize) then
    Result := FOnOptimize(Self,FuncExpr)
  else
    Result := FuncExpr;
end;

{ Tdws2Functions }

class function Tdws2Functions.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Function;
end;

{ Tdws2Forwards }

class function Tdws2Forwards.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Forward;
end;

{ Tdws2Enumerations }

class function Tdws2Enumerations.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Enumeration;
end;

{ Tdws2Constants }

class function Tdws2Constants.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Constant;
end;

{ Tdws2Classes }

class function Tdws2Classes.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Class;
end;

{ Tdws2Arrays }

class function Tdws2Arrays.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Array;
end;

{ Tdws2Records }

class function Tdws2Records.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Record;
end;

{ Tdws2Parameters }

class function Tdws2Parameters.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Parameter;
end;

{ Tdws2Instances }

class function Tdws2Instances.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Instance;
end;

{ Tdws2Fields }

class function Tdws2Fields.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Field;
end;

{ Tdws2Constructors }

class function Tdws2Constructors.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Constructor;
end;

{ Tdws2Methods }

class function Tdws2Methods.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Method;
end;

{ Tdws2Properties }

class function Tdws2Properties.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Property;
end;

{ Tdws2Members }

class function Tdws2Members.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Member;
end;

{ Tdws2Elements }

class function Tdws2Elements.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Element;
end;

{ Tdws2Synonyms }

class function Tdws2Synonyms.GetSymbolClass: Tdws2SymbolClass;
begin
  Result := Tdws2Synonym;
end;

{ Tdws2Synonym }

function Tdws2Synonym.DoGenerate(Table: TSymbolTable; ParentSym: TSymbol): TSymbol;
begin
  FIsGenerating := True;
  CheckName(Table, Name);
  Result := TAliasSymbol.Create(Name, GetDataType(Table, DataType));
  GetUnit.Table.AddSymbol(Result);
end;

{ Tdws2AbstractStaticUnit }

constructor Tdws2AbstractStaticUnit.Create(AOwner: TComponent);
begin
  inherited;
  FStaticTable := nil;
  FStaticSymbols := False;
end;

function Tdws2AbstractStaticUnit.CreateUnitTable(Parent: TSymbolTable; Typ: TSymbolTableType): TSymbolTable;
begin
  case Typ of
    sttLinked: Result := TLinkedSymbolTable.Create(Parent as TStaticSymbolTable);
    sttStatic: Result := TStaticSymbolTable.Create(Parent as TStaticSymbolTable);
  else
    Result := TSymbolTable.Create(Parent);
  end;
end;

procedure Tdws2AbstractStaticUnit.BeforeDestruction;
begin
  ReleaseStaticSymbols;
  inherited;
end;

destructor Tdws2AbstractStaticUnit.Destroy;
begin
  inherited;
end;

function Tdws2AbstractStaticUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
begin
  if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms) then
    Result := CreateUnitTable(FStaticTable, sttLinked) as TLinkedSymbolTable // typecheck
  else
  begin
    Result := CreateUnitTable(SystemTable); // sttDefault
    try
      InitUnitTable(SystemTable, UnitSyms, Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

function Tdws2AbstractStaticUnit.InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
var
  staticParent: TStaticSymbolTable;
begin
  if not Assigned(FStaticTable) then
  begin
    if SystemTable is TStaticSymbolTable then
      staticParent := TStaticSymbolTable(SystemTable)
    else if SystemTable is TLinkedSymbolTable then
      staticParent := TLinkedSymbolTable(SystemTable).Parent
    else
      staticParent := nil;
      
    if Assigned(staticParent) then
    begin
      FStaticTable := CreateUnitTable(staticParent, sttStatic) as TStaticSymbolTable;
      try
        InitUnitTable(SystemTable, UnitSyms, FStaticTable);
      except
        ReleaseStaticSymbols;
        raise;
      end;
    end;
  end; // else check FSymbolTable = StaticTable
  Result := Assigned(FStaticTable);
end;

procedure Tdws2AbstractStaticUnit.InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
var
  x: Integer;
  sym: TSymbol;
begin
  if UnitName = '' then
    raise Exception.CreateFmt(UNT_UnitNameNotDefined, [Name]);

  for x := 0 to FDependencies.Count - 1 do
  begin
    sym := UnitSyms.FindSymbol(FDependencies[x]);
    try
      UnitTable.AddParent(TUnitSymbol(sym).Table);
    except
      on e: Exception do
        raise Exception.CreateFmt(UNT_DependencyError,[UnitName, Sym.Name, e.Message]);
    end;
    UnitTable.AddSymbol(TUnitSymbol.Create(TUnitSymbol(sym).Name,
      TUnitSymbol(sym).Table));
  end;

  AddUnitSymbols(UnitTable);
end;

procedure Tdws2AbstractStaticUnit.ReleaseStaticSymbols;
var
  s: TStaticSymbolTable;
begin
  if Assigned(FStaticTable) then
  begin
    s := FStaticTable;
    FStaticTable := nil;
    s._Release;
  end;
end;

procedure Tdws2AbstractStaticUnit.SetStaticSymbols(const Value: Boolean);
begin
  FStaticSymbols := Value;
  if not FStaticSymbols then
    ReleaseStaticSymbols;
end;

{ TCustomInstantiateFunc }

procedure TCustomInstantiateFunc.ReleaseObject;
begin
  FScriptObj := nil;
end;

{ TDynamicInstantiateFunc }

constructor TDynamicInstantiateFunc.Create(FuncSym: TFuncSymbol;
  AExternalObject: TObject);
begin
  inherited Create(FuncSym);
  FExternalObject := AExternalObject;
end;

procedure TDynamicInstantiateFunc.Execute;
begin
  if Assigned(FScriptObj) then
    // Instance was already created
    Info.Result := FScriptObj
  else
  begin
    // First access to this variable. Create object instance!
    FScriptObj := TScriptObj.Create(FClassSym);
    FScriptObj.ExternalObject := FExternalObject;
    Info.Result := FScriptObj;
  end;
end;

end.



