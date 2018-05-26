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

unit dws2Exprs;

interface

uses
  Classes, dws2Symbols, dws2Errors, dws2Strings, dws2Stack;

const
  C_DefaultStackChunkSize = 4096;

type
  TRelOps = (roEqual, roUnEqual, roLess, roLessEqual, roMore, roMoreEqual);

  TRefKind = (rkObjRef, rkClassOfRef);

  TExpr = class;
  TExprList = class;
  TProgram = class;
  TSymbolPositionList = class;

  // Interface for units
  IUnit = interface
    ['{8D534D12-4C6B-11D5-8DCB-0000216D9E86}']
    function GetUnitName: string;
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
    function GetDependencies: TStrings;
  end;

  TScriptSourceType = (stMain, stInclude{, stUnit}); // stUnit is left for the future

  // A specific ScriptSource entry. The text of the script contained in that unit.
  TScriptSourceItem = class
  private
    FNameReference: string;
    FSourceFile: TSourceFile;
    FSourceType: TScriptSourceType;
  public
    constructor Create(ANameReference: string; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
    property NameReference: string read FNameReference write FNameReference;
    property SourceFile: TSourceFile read FSourceFile;
    property SourceType: TScriptSourceType read FSourceType;
  end;

  // Manage a list of all the different Script Texts (files) used in the program.
  TScriptSourceList = class
  private
    FSourceList: TList;
    FMainScript: TScriptSourceItem;
    function GetSourceItem(Index: Integer): TScriptSourceItem;
    procedure SetSourceItem(Index: Integer; SourceItem: TScriptSourceItem);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(ANameReference: string; ASourceFile: TSourceFile; ASourceType: TScriptSourceType);
    function FindScriptSourceItem(ScriptPos: TScriptPos): TScriptSourceItem; overload;
    function FindScriptSourceItem(SourceFile: TSourceFile): TScriptSourceItem; overload;
    function FindScriptSourceItem(SourceFileName: string): TScriptSourceItem; overload;
    function IndexOf(AScriptPos: TScriptPos): Integer; overload;
    function IndexOf(ASourceFile: TSourceFile): Integer; overload;
    function IndexOf(SourceFileName: string): Integer; overload;
    function Count: Integer;
    property Items[Index: Integer]: TScriptSourceItem read GetSourceItem write SetSourceItem; default;
    property MainScript: TScriptSourceItem read FMainScript;
  end;

  { Describe how the symbol at the position is being used. suReference would be
    a typical usage of the symbol. }
  TSymbolUsage = (suForward, suDeclaration, suImplementation, suReference);
  TSymbolUsages = set of TSymbolUsage;

  TSymbolPosition = class
  private
    FOwnerList: TSymbolPositionList; // pointer back to owning list
    FScriptPos: TScriptPos;     // location of symbol instance in script
    FSymUsages: TSymbolUsages;  // how symbol is used at this location (mutiple uses possible, Functions are Delcared/Implemented at same spot)
    function GetSymbol: TSymbol;// get symbol from parent
  public
    constructor Create(AOwningList: TSymbolPositionList; AScriptPos: TScriptPos; AUsages: TSymbolUsages);
    property Symbol: TSymbol read GetSymbol;     // get owner symbol
    property ScriptPos: TScriptPos read FScriptPos;
    property SymbolUsages: TSymbolUsages read FSymUsages write FSymUsages;
  end;

  {Re-list every symbol (pointer to it) and every position it is in in the script }
  TSymbolPositionList = class
  private
    FSymbol: TSymbol;       // pointer to the symbol
    FPosList: TList;        // list of positions where symbol is declared and used
    function GetPosition(Index: Integer): TSymbolPosition;
    procedure SetPosition(Index: Integer; SymPos: TSymbolPosition);
  protected
    // Used by TSymbolDictionary. Not meaningful to make public (symbol is known).
    function FindSymbolAtPosition(AbsolutePos: Integer): TSymbol; overload;
    function FindSymbolAtPosition(ACol, ALine: Integer): TSymbol; overload;
  public
    constructor Create(ASymbol: TSymbol);
    destructor Destroy; override;
    procedure Add(Pos: TScriptPos; UseTypes: TSymbolUsages);
    function FindUsage(SymbolUse: TSymbolUsage): TSymbolPosition;
    function Count: Integer;
    property Items[Index: Integer]: TSymbolPosition read GetPosition write SetPosition; default;
    property Symbol: TSymbol read FSymbol;
  end;

  { List all symbols in the script. Each symbol list contains a list of the
    positions where it was used. }
  TSymbolDictionary = class
  protected
    FSymbolList: TList;
    function GetList(Index: Integer): TSymbolPositionList;
    procedure SetList(Index: Integer; PosList: TSymbolPositionList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;  // clear the lists
    procedure Add(Sym: TSymbol; Pos: TScriptPos; UseTypes: TSymbolUsages=[suReference]);
    procedure Remove(Sym: TSymbol); // remove references to the symbol
    function FindSymbolAtPosition(AbsolutePos: Integer): TSymbol; overload;
    function FindSymbolAtPosition(ACol, ALine: Integer): TSymbol; overload;
    function FindSymbolPosList(Sym: TSymbol): TSymbolPositionList; overload;  // return list of symbol
    function FindSymbolPosList(SymName: string): TSymbolPositionList; overload;  // return list of symbol
    function FindSymbolPosListOfType(SymName: string; SymbolType: TSymbolClass): TSymbolPositionList; // return list of symbol given the desired type
    function FindSymbolUsage(Symbol: TSymbol; SymbolUse: TSymbolUsage): TSymbolPosition; overload;
    function FindSymbolUsage(SymName: string; SymbolUse: TSymbolUsage): TSymbolPosition; overload;
    function FindSymbolUsageOfType(SymName: string; SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
    function Count: Integer;
    property Items[Index: Integer]: TSymbolPositionList read GetList write SetList; default;
  end;

  // Context within the script. (A block of code) Can be nested
  TContext = class
  private
    FParentContext: TContext;
    FParentSymbol: TSymbol;     // a parent symbol would be a procedure/method, etc.
    FSubContexts: TList;        // contexts that are inside of this one
    FEndPos: TScriptPos;
    FStartPos: TScriptPos;
    FData: Pointer;             // pointer to some data element (for users)
    FLocalTable: TSymbolTable;  // symbol table associated with the context (begin..end blocks, TProcedures, etc)
  public
    constructor Create(AParent: TContext; AStartPos: TScriptPos; AParentSymbol: TSymbol);
    destructor Destroy; override;
    function IsPositionInContext(ACol, ALine: Integer; SourceFile: TSourceFile=nil): Boolean;
    function HasParentSymbolOfClass(SymbolType: TSymbolClass; SearchParents: Boolean): Boolean;
    property Parent: TContext read FParentContext;
    property ParentSym: TSymbol read FParentSymbol;
    property SubContexts: TList read FSubContexts;
    property StartPos: TScriptPos read FStartPos;
    property EndPos: TScriptPos read FEndPos;
    property Data: Pointer read FData write FData;
    property LocalTable: TSymbolTable read FLocalTable write FLocalTable;
  end;

  // Map the various script contexts. (Code blocks)
  TContextMap = class
  private
    FScriptContexts: TList;     // list of top-level contexts
    FCurrentContext: TContext;  // current context (used when adding and leaving)
  public
    constructor Create;
    destructor Destroy; override;
    { Push a context on to the stack - procedures have a symbol context.
      Standard Begin..end blocks do not have a ParentSymbol. }
    procedure OpenContext(AStartPos: TScriptPos; AParentSymbol: TSymbol);
    { Pop a context off the stack }
    procedure CloseContext(AEndPos: TScriptPos);
    function FindContext(AParentSymbol: TSymbol): TContext; overload;// return the first context group based on its parent
    function FindContext(ACol, ALine: Integer; SourceFile: TSourceFile=nil): TContext; overload;
    function FindContext(ScriptPos: TScriptPos): TContext; overload;
    property Contexts: TList read FScriptContexts;
    property Current: TContext read FCurrentContext; 
  end;

  // If a class needs completing this refers to the type of error
  TClassCompleteErrorType =
                   (ccePropAccessDeclMissing,    // property read/write access symbol declaration is missing
                    cceMethodImplMissing         // method implementation is missing
                   );

  // If a class needs completing this contains the information about the error
  // and includes a suggested fix
  TClassCompleteErrorInfo = record
    ErrorClass: TClassSymbol;
    ErrorType: TClassCompleteErrorType;   // TClassMemberError declared in dws2Symbols.pas
    SuggestedFix: string;
  end;

  // Dynamic list of all class completion errors encountered
  TClassCompleteNeeds = array of TClassCompleteErrorInfo;

  TProgramEvent = procedure (Prog: TProgram) of object;

  Tdws2ResultType = class;

  Tdws2Result = class
  private
    FResultType: Tdws2ResultType;
  protected
    constructor Create(ResultType: Tdws2ResultType);
    procedure InitializeProgram(Prog: TProgram); virtual;
    procedure FinalizeProgram(Prog: TProgram); virtual;
    property ResultType: Tdws2ResultType read FResultType;
  end;

  Tdws2ResultType = class(TComponent)
  private
    FOnInitializeProgram: TProgramEvent;
    FOnFinalizeProgram: TProgramEvent;
  public
    procedure AddResultSymbols(SymbolTable: TSymbolTable); virtual;
    function CreateProgResult: Tdws2Result; virtual;
  published
    property OnInitializeProgram: TProgramEvent read FOnInitializeProgram write FOnInitializeProgram;
    property OnFinalizeProgram: TProgramEvent read FOnFinalizeProgram write FOnFinalizeProgram;
  end;

  // Interface for external debuggers
  IDebugger = interface
    ['{8D534D14-4C6B-11D5-8DCB-0000216D9E86}']
    procedure StartDebug(MainProg: TProgram);
    procedure DoDebug(Prog: TProgram; Expr: TExpr);
    procedure StopDebug(MainProg: TProgram);
    procedure EnterFunc(Prog: TProgram; Expr: TExpr);
    procedure LeaveFunc(Prog: TProgram; Expr: TExpr);
  end;

  // Stops the script after given time (Timeout)
  TTerminatorThread = class(TThread)
    FProg: TProgram;
    FMillisecRemaining: Integer;
    constructor Create(Prog: TProgram; MilliSecToLive: Integer);
    procedure Execute; override;
  end;

  TProgramInfo = class;

  TProgramState = (psUndefined, psReadyToRun, psRunning, psRunningStopped, psTerminated);

  // A script executable program
  TProgram = class(TInterfacedObject)
  private
    FAddrGenerator: TAddrGenerator;
    FDebugger: IDebugger;
    FClassCompleteNeeds: TClassCompleteNeeds;
    FContextMap: TContextMap;
    FExpr: TExpr;
    FGlobalAddrGenerator: TAddrGenerator;
    FInitExpr: TExpr;
    FInfo: TProgramInfo;
    FIsDebugging: Boolean;
    FMsgs: TMsgs;
    FParameters: TData;
    FParent: TProgram;
    FProgramState: TProgramState;
    FResult: Tdws2Result;
    FResultType: Tdws2ResultType;
    FRoot: TProgram;
    FRootTable: TSymbolTable;
    FSourceList: TScriptSourceList;
    FStack: TStack;
    FSymbolDictionary: TSymbolDictionary;
    FTable: TSymbolTable;
    FTimeout: Integer;
    FTypBoolean: TTypeSymbol;
    FTypDateTime: TTypeSymbol;
    FTypFloat: TTypeSymbol;
    FTypInteger: TTypeSymbol;
    FTypNil: TNilSymbol;
    FTypObject: TClassSymbol;
    FTypString: TTypeSymbol;
    FTypVariant: TTypeSymbol;
    FUserDef: TObject;
  protected
    procedure DoStep(Expr: TExpr);
    function GetLevel: Integer;
    function GetResult: Tdws2Result; virtual;
    function GetUserDef: TObject; virtual;
    procedure SetDebugger(const Value: IDebugger);
    procedure SetResult(const Value: Tdws2Result); virtual;
    procedure SetUserDef(const Value: TObject); virtual;
    procedure RegisterExpr(Expr: TExpr); virtual;
    procedure UnregisterExpr(Expr: TExpr); virtual;
    procedure Evaluate; virtual;                     
  public
    constructor Create(SystemTable: TSymbolTable; ResultType: Tdws2ResultType; MaxDataSize: Integer; StackChunkSize: Integer = C_DefaultStackChunkSize);
    destructor Destroy; override;
    procedure AddClassCompleteInfo(Info: TClassCompleteErrorInfo);
    procedure BeginProgram(IsRunningMainProgram: Boolean = True);
    procedure DestroyScriptObj(ScriptObj: IScriptObj);
    procedure EndProgram;
    procedure Execute; overload; virtual;
    procedure Execute(TimeoutValue: Integer); overload;
    procedure ExecuteParam(const Params: array of Variant); overload;
    procedure ExecuteParam(const Params: array of Variant; TimeoutValue: Integer); overload;
    procedure ExecuteParam(Params: OleVariant); overload;
    procedure ExecuteParam(Params: OleVariant; TimeoutValue: Integer); overload;
    function GetGlobalAddr(DataSize: Integer): Integer;
    function GetTempAddr(DataSize: Integer = -1): Integer;
    procedure ReadyToRun;
    procedure RunProgram(TimeoutValue: Integer);
    procedure Stop; virtual;
    property Debugger: IDebugger read FDebugger write SetDebugger;
    property Expr: TExpr read FExpr write FExpr;
    property InitExpr: TExpr read FInitExpr;
    property Info: TProgramInfo read FInfo;
    property IsDebugging: Boolean read FIsDebugging;
    property Level: Integer read GetLevel;
    property Msgs: TMsgs read FMsgs write FMsgs;
    property Parameters: TData read FParameters;
    property Parent: TProgram read FParent;
    property ProgramState: TProgramState read FProgramState;
    property Result: Tdws2Result read GetResult write SetResult;
    property Root: TProgram read FRoot write FRoot;
    property Stack: TStack read FStack;
    property RootTable: TSymbolTable read FRootTable;
    property Table: TSymbolTable read FTable write FTable;
    property Timeout: Integer read FTimeout write FTimeout;
    property TypBoolean: TTypeSymbol read FTypBoolean;
    property TypDateTime: TTypeSymbol read FTypDateTime;
    property TypFloat: TTypeSymbol read FTypFloat;
    property TypInteger: TTypeSymbol read FTypInteger;
    property TypNil: TNilSymbol read FTypNil;
    property TypObject: TClassSymbol read FTypObject;
    property TypString: TTypeSymbol read FTypString;
    property TypVariant: TTypeSymbol read FTypVariant;
    property UserDef: TObject read GetUserDef write SetUserDef;
    property SymbolDictionary: TSymbolDictionary read FSymbolDictionary;
    property ContextMap: TContextMap read FContextMap;
    property SourceList: TScriptSourceList read FSourceList;
    property ClassCompleteNeeds: TClassCompleteNeeds read FClassCompleteNeeds write FClassCompleteNeeds;
  end;

  // Functions callable from a script program implement this interfaces
  ICallable = interface(IExecutable)
    ['{8D534D15-4C6B-11D5-8DCB-0000216D9E86}']
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
  end;

  // A script procedure
  TProcedure = class(TProgram, IUnknown, ICallable)
  private
    FFunc: TFuncSymbol;
  protected
    function GetResult: Tdws2Result; override;
    function GetUserDef: TObject; override;
    procedure SetResult(const Value: Tdws2Result); override;
    procedure SetUserDef(const Value: TObject); override;
  public
    constructor Create(Parent: TProgram);
    destructor Destroy; override;
    procedure AssignTo(sym: TFuncSymbol);
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure Execute; override;
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
//    procedure Initialize;
    function Optimize(FuncExpr: TExprBase): TExprBase;
    procedure Stop; override;
    property Func: TFuncSymbol read FFunc write FFunc;
  end;

  // Base class of all expressions
  TExpr = class(TExprBase)
  private
    FPos: TScriptPos;
    FProg: TProgram;
    FTyp: TSymbol;
    function CreateEDelphiObj(ClassName, Message: string): IScriptObj;
  protected
    function IsBooleanType(Typ: TSymbol): Boolean;
    function IsDateTimeType(Typ: TSymbol): Boolean;
    function IsFloatType(Typ: TSymbol): Boolean;
    function IsIntegerType(Typ: TSymbol): Boolean;
    function IsNumberType(Typ: TSymbol): Boolean;
    function IsStringType(Typ: TSymbol): Boolean;
    function IsVariantType(Typ: TSymbol): Boolean;
    function GetBaseType: TTypeSymbol;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Eval: Variant; virtual; abstract;
    procedure Initialize; virtual;
    function Optimize: TExpr; virtual;
    procedure TypeCheck; virtual;
    property Pos: TScriptPos read FPos;
    property Prog: TProgram read FProg;
    property Typ: TSymbol read FTyp write FTyp;
    property BaseType: TTypeSymbol read GetBaseType;
  end;

  // Does nothing! E. g.: "for x := 1 to 10 do {TNullExpr};"
  TNullExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  // Encapsulates data
  TDataExpr = class(TExpr)
  private
    FIsWritable: Boolean;
    function GetAddr: Integer; virtual;
    function GetData: TData; virtual; abstract;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol);
    procedure AssignData(SourceData: TData; SourceAddr: Integer); virtual;
    procedure AssignDataExpr(DataExpr: TDataExpr); virtual;
    procedure AssignExpr(Expr: TExpr); virtual;
    procedure AssignValue(const Value: Variant); virtual;
    function Eval: Variant; override;
    property Addr: Integer read GetAddr;
    property Data: TData read GetData;
    property IsWritable: Boolean read FIsWritable write FIsWritable;
  end;

  // Encapsulates a local variable (on the stack)
  TVarExpr = class(TDataExpr)
  private
    FDataSym: TDataSymbol;
    FStackAddr: Integer; // = DataSym.StackAddr
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; DataSym: TDataSymbol);
    procedure AssignData(SourceData: TData; SourceAddr: Integer); override;
    procedure AssignDataExpr(DataExpr: TDataExpr); override;
    procedure AssignExpr(Expr: TExpr); override;
    procedure AssignValue(const Value: Variant); override;
    function Eval: Variant; override;
    property DataSym: TDataSymbol read FDataSym;
  end;

  TVarParentExpr = class(TVarExpr)
  private
    FLevel: Integer;
    function GetAddr: Integer; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; DataSym: TDataSymbol);
  end;

  // Encapsulates a var parameter
  TVarParamExpr = class(TVarExpr)
  private
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    procedure AssignData(SourceData: TData; SourceAddr: Integer); override;
    procedure AssignDataExpr(DataExpr: TDataExpr); override;
    procedure AssignExpr(Expr: TExpr); override;
    procedure AssignValue(const Value: Variant); override;
    function Eval: Variant; override;
  end;

  // Encapsulates a var parameter
  TVarParamParentExpr = class(TVarParamExpr)
  private
    FLevel: Integer;
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; DataSym: TDataSymbol);
  end;

  // A constant value (like 0, 3.14159, 'Hello' or true)
  TConstExpr = class(TDataExpr)
  private
    FData: TData;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; Value: Variant); overload;
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; Data: TData); overload;
    function Eval: Variant; override;
  end;

  TArrayConstantExpr = class(TDataExpr)
  private
    FArrayAddr: Integer;
    FElementExprs: TExprList;
    FData: TData;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos);
    destructor Destroy; override;
    procedure AddElementExpr(ElementExpr: TExpr);
    procedure Prepare(ElementTyp : TSymbol);
    procedure TypeCheck; override;     
    function Eval: Variant; override;
    function Optimize: TExpr; override;
    procedure Initialize; override;
    property ElementExprs: TExprList read FElementExprs;
  end;

  // Array expressions x[index]
  TArrayExpr = class(TDataExpr)
  protected
    FBaseExpr: TDataExpr;
    FElementSize: Integer;
    FIndexExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
  end;

  // Array expressions x[index] for static arrays
  TStaticArrayExpr = class(TArrayExpr)
  private
    FLowBound: Integer;
    FLastIndex: Integer;
  protected
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TExpr; LowBound, HighBound: Integer);
  end;

  // Array expressions: x[index0] for dynamic arrays
  TDynamicArrayExpr = class(TArrayExpr)
  protected
    function GetAddr: Integer; override;
    function GetData: TData; override;
  end;

  // Record expression: record.member
  TRecordExpr = class(TDataExpr)
  protected
    FBaseExpr: TDataExpr;
    FMemberOffset: Integer;
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
    destructor Destroy; override;
    procedure Initialize; override;
  end;

  TInitDataExpr = class(TExpr)
  private
    FExpr: TDataExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TDataExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
  end;

  // Field expression: obj.Field
  TFieldExpr = class(TDataExpr)
  private
    FObjectExpr: TExpr;
    FFieldAddr: Integer;
    function GetAddr: Integer; override;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol;
      FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
    destructor Destroy; override;
    procedure Initialize; override;
  end;

  TPushExpr = class(TExpr)
  protected
    FStackAddr: Integer;
    FArgExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; StackAddr: Integer;
      ArgExpr: TExpr);
    procedure Execute; virtual; abstract;
    function Eval: Variant; override;
    property ArgExpr: TExpr read FArgExpr write FArgExpr;
  end;

  TPushAddrExpr = class(TPushExpr)
    procedure Execute; override;
  end;

  TPushResultExpr = class(TPushExpr)
    procedure Execute; override;
  end;

  TPushDataExpr = class(TPushExpr)
  private
    FParamSym: TSymbol;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; StackAddr: Integer;
      ArgExpr: TExpr; ParamSym: TSymbol);
    procedure Execute; override;
  end;

  // Function call: func(arg0, arg1, ...);
  TFuncExpr = class(TDataExpr)
  private
    FArgs: TExprList;
    FFunc: TFuncSymbol;
    FInitResultExpr: TExpr;
    FIsInstruction: Boolean;
    FPushExprs: TExprList;
    FHasResult: Boolean;
    FResultAddr: Integer;
    FCodeExpr : TDataExpr;
  protected
    function PostCall(ScriptObj: IScriptObj): Variant; virtual;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; virtual;
    function OptimizeExecutable: TExpr; virtual;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TFuncSymbol;
      IsInstruction: Boolean = True; CodeExpr: TDataExpr = nil;
      IsWritable: Boolean = False);
    destructor Destroy; override;
    procedure AddArg(Arg: TExpr);
    procedure AddPushExprs;
    function Eval: Variant; override;
    function GetData: TData; override;
    function GetAddr: Integer; override;
    function Optimize: TExpr; override;
    function GetCode(Func : TFuncSymbol) : ICallable; virtual;
    procedure Initialize; override;
    procedure SetResultAddr(ResultAddr: Integer = -1);
    procedure TypeCheck; override;
    property Args: TExprList read FArgs;
    property FuncSym: TFuncSymbol read FFunc;
    property CodeExpr : TDataExpr read FCodeExpr;
  end;

  TFuncCodeExpr = class(TDataExpr)
  private
    FFuncExpr : TFuncExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; FuncExpr: TFuncExpr);
    destructor Destroy; override;
    procedure TypeCheck; override;
    procedure AssignDataExpr(Right: TDataExpr); override;
    function Eval: Variant; override;
    function GetData: TData; override;
    function GetAddr: Integer; override;
    property FuncExpr : TFuncExpr read FFuncExpr;
  end;

  TMethodObjExpr = class(TDataExpr)
  private
    FBaseExpr : TDataExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr);
    function GetData: TData; override;
    function GetAddr: Integer; override;
  end;

  TConnectorCallExpr = class(TDataExpr)
  private
    FArgs: TExprList;
    FBaseExpr: TExpr;                    
    FConnectorArgs: TConnectorArgs;
    FConnectorCall: IConnectorCall;
    FConnectorParams: TConnectorParams;
    FIsInstruction: Boolean;
    FName: string;
    FResultData: TData;
    FIsIndex: Boolean;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Name: string; BaseExpr:
      TExpr; IsWritable: Boolean = True; IsIndex: Boolean = False);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    procedure AddArg(ArgExpr: TExpr);
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
  end;

  TConnectorReadExpr = class(TDataExpr)
  private
    FBaseExpr: TExpr;
    FConnectorMember: IConnectorMember;
    FName: string;
    FResultData: TData;
    function GetData: TData; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Name: string; BaseExpr:
      TExpr);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property BaseExpr: TExpr write FBaseExpr;
  end;

  TConnectorWriteExpr = class(TExpr)
  private
    FBaseExpr: TExpr;
    FValueExpr: TExpr;
    FConnectorMember: IConnectorMember;
    FName: string;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Name: string; BaseExpr,
      ValueExpr: TExpr);
    destructor Destroy; override;
    function AssignConnectorSym(ConnectorType: IConnectorType): Boolean;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
  end;

  // Call of static methods (not virtual)
  TMethodStaticExpr = class(TFuncExpr)
  private
    FBaseExpr: TDataExpr;
    FSelfAddr: Integer;
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol;
      BaseExpr: TDataExpr; IsInstruction: Boolean = True;
      CodeExpr: TDataExpr = nil; IsWritable: Boolean = False);
    destructor Destroy; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property BaseExpr: TDataExpr read FBaseExpr;
  end;

  // Class methods
  TClassMethodStaticExpr = class(TMethodStaticExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TConstructorStaticExpr = class(TMethodStaticExpr)
  private
    FExternalObject: TObject;
  protected
    function PostCall(ScriptObj: IScriptObj): Variant; override;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol; Base:
      TDataExpr; IsInstruction: Boolean = True);
    procedure TypeCheck; override;
    property ExternalObject: TObject read FExternalObject write FExternalObject;
  end;

  TDestructorStaticExpr = class(TMethodStaticExpr)
  end;

  TMethodVirtualExpr = class(TMethodStaticExpr)
  private
    FMethName: string;
  protected
    function FindVirtualMethod(ClassSym: TClassSymbol): TMethodSymbol;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
    function OptimizeExecutable: TExpr; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol; Base:
      TDataExpr; IsInstruction: Boolean = True);
    property MethName: string read FMethName;
  end;

  // Call to Class method with class reference: TMyClass.ClassMethod(..)
  TClassMethodVirtualExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TClassMethodVirtualNameExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
    function OptimizeExecutable: TExpr; override;
  end;

  // Call to Class method with object reference: obj.ClassMethod(..)
  TClassMethodObjVirtualExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  end;

  TClassMethodObjVirtualNameExpr = class(TMethodVirtualExpr)
  protected
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
    function OptimizeExecutable: TExpr; override;
  end;

  TConstructorVirtualExpr = class(TMethodVirtualExpr)
  private
    FExternalObject: TObject;
  protected
    function PostCall(ScriptObj: IScriptObj): Variant; override;
    function PreCall(var ScriptObj: IScriptObj): TFuncSymbol; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol; Base:
      TDataExpr; IsInstruction: Boolean = True);
    property ExternalObject: TObject read FExternalObject write FExternalObject;
  end;

  TConstructorStaticObjExpr = class(TMethodStaticExpr)
  protected
    function PostCall(ScriptObj: IScriptObj): Variant; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol;
      BaseExpr: TDataExpr; IsInstruction: Boolean = True;
      CodeExpr: TDataExpr = nil; IsWritable: Boolean = False);
  end;

  TConstructorVirtualObjExpr = class(TMethodVirtualExpr)
  protected
    function PostCall(ScriptObj: IScriptObj): Variant; override;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Func: TMethodSymbol; Base:
      TDataExpr; IsInstruction: Boolean = True);
  end;

  TDestructorVirtualExpr = class(TMethodVirtualExpr)
  end;

  // left := right;
  TAssignExpr = class(TExpr)
  protected
    FLeft: TDataExpr;
    FRight: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
    function Optimize: TExpr; override;
    procedure Initialize; override;
    procedure TypeCheck; override;
  end;

  // left := right;
  TAssignDataExpr = class(TAssignExpr)
  protected
    FSize: Integer;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
    function Eval: Variant; override;
    function Optimize: TExpr; override;
  end;

  // statement; statement; statement;
  TBlockExpr = class(TExpr)
  private
    FStatements: TExprList;
    FTable: TSymbolTable;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos);
    destructor Destroy; override;
    procedure AddStatement(Expr: TExpr);
    function Eval: Variant; override;
    function Optimize: TExpr; override;
    procedure Initialize; override;
    property Table: TSymbolTable read FTable;
  end;

  TIncrExpr = class(TExpr)
  private
    FLeft: TDataExpr;
    FRight: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, right: TExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
  end;

  // if FCond then FThen else FElse
  TIfExpr = class(TExpr)
    FCond: TExpr;
    FElse: TExpr;
    FThen: TExpr;
  public
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    procedure TypeCheck; override;
    function Optimize: TExpr; override;
  end;

  // Part of a case statement
  TCaseCondition = class
  private
    FOwnsTrueExpr: Boolean;
    FTrueExpr: TExpr;
    FValueExpr: TExpr;
  public
    constructor Create(ValueExpr: TExpr);
    destructor Destroy; override;
    procedure Initialize; virtual;
    function IsTrue(Value: Variant): Boolean; virtual; abstract;
    procedure Optimize; virtual;
    procedure TypeCheck(Typ: TSymbol); virtual; abstract;
    property TrueExpr: TExpr read FTrueExpr write FTrueExpr;
    property OwnsTrueExpr: Boolean read FOwnsTrueExpr write FOwnsTrueExpr;
  end;

  TCompareCaseCondition = class(TCaseCondition)
  private
    FCompareExpr: TExpr;
  public
    constructor Create(ValueExpr, CompareExpr: TExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    function IsTrue(Value: Variant): Boolean; override;
    procedure Optimize; override;
    procedure TypeCheck(Typ: TSymbol); override;
  end;

  TRangeCaseCondition = class(TCaseCondition)
  private
    FFromExpr: TExpr;
    FToExpr: TExpr;
  public
    constructor Create(ValueExpr, FromExpr, ToExpr: TExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    function IsTrue(Value: Variant): Boolean; override;
    procedure Optimize; override;
    procedure TypeCheck(Typ: TSymbol); override;
  end;

  // case FValueExpr of {CaseConditions} else FElseExpr end;
  TCaseExpr = class(TExpr)
  private
    FCaseConditions: TList;
    FElseExpr: TExpr;
    FValueExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    procedure TypeCheck; override;
    property ValueExpr: TExpr read FValueExpr write FValueExpr;
    property CaseConditions: TList read FCaseConditions;
    property ElseExpr: TExpr read FElseExpr write FElseExpr;
  end;

  // for FVarExpr := FFromExpr to FToExpr do FDoExpr;
  TForExpr = class(TExpr)
  private
    FDoExpr: TExpr;
    FFromExpr: TExpr;
    FToExpr: TExpr;
    FVarExpr: TDataExpr;
    FIsUpWard: Boolean;
  public
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    procedure TypeCheck; override;
    property DoExpr: TExpr read FDoExpr write FDoExpr;
    property FromExpr: TExpr read FFromExpr write FFromExpr;
    property ToExpr: TExpr read FToExpr write FToExpr;
    property IsUpward: Boolean read FIsUpWard write FIsUpWard;
    property VarExpr: TDataExpr read FVarExpr write FVarExpr;
  end;

  TLoopExpr = class(TExpr)
  private
    FCondExpr: TExpr;
    FLoopExpr: TExpr;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    procedure TypeCheck; override;
    function Optimize: TExpr; override;
    property CondExpr: TExpr read FCondExpr write FCondExpr;
    property LoopExpr: TExpr read FLoopExpr write FLoopExpr;
  end;

  // while FCondExpr do FLoopExpr
  TWhileExpr = class(TLoopExpr)
  public
    function Eval: Variant; override;
  end;

  // repeat FLoopExpr while FCondExpr
  TRepeatExpr = class(TLoopExpr)
  public
    function Eval: Variant; override;
  end;

  TBreakExpr = class(TExpr)
  public
    function Eval: Variant; override;
  end;

  TExitExpr = class(TExpr)
  public
    function Eval: Variant; override;
  end;

  TContinueExpr = class(TExpr)
  public
    function Eval: Variant; override;
  end;

  // raise TExceptionClass.Create;
  TRaiseExpr = class(TExpr)
  private
    FExceptionExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; ExceptionExpr: TExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    procedure TypeCheck; override;
    function Optimize: TExpr; override;
  end;

  TReraiseExpr = class(TExpr)
  public
    function Eval: Variant; override;
  end;

  TExceptionExpr = class(TExpr)
  private
    FTryExpr: TExpr;
    FHandlerExpr: TExpr;
  public
    destructor Destroy; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property TryExpr: TExpr read FTryExpr write FTryExpr;
    property HandlerExpr: TExpr read FHandlerExpr write FHandlerExpr;
  end;

  // try FTryExpr except {FDoExprs}; else FElseExpr end;
  TExceptExpr = class(TExceptionExpr)
  private
    FDoExprs: TExprList;
    FElseExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property DoExprs: TExprList read FDoExprs write FDoExprs;
    property ElseExpr: TExpr read FElseExpr write FElseExpr;
  end;

  // try..except on FExceptionVar: FExceptionVar.Typ do FDoBlockExpr; ... end;
  TExceptDoExpr = class(TExpr)
  private
    FExceptionVar: TDataSymbol;
    FDoBlockExpr: TExpr;
  public
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property DoBlockExpr: TExpr read FDoBlockExpr write FDoBlockExpr;
    property ExceptionVar: TDataSymbol read FExceptionVar write FExceptionVar;
  end;

  // try FTryExpr finally FHandlerExpr end;
  TFinallyExpr = class(TExceptionExpr)
  public
    function Eval: Variant; override;
  end;

  TStringArraySetExpr = class(TExpr)
  private
    FStringExpr: TExpr;
    FIndexExpr: TExpr;
    FValueExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; StringExpr, IndexExpr, ValueExpr: TExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
  end;

  TUnaryOpExpr = class(TExpr)
  private
    FExpr: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    destructor Destroy; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    property Expr: TExpr read FExpr write FExpr;
  end;

  // left "op" right
  TBinaryOpExpr = class(TExpr)
  private
    FLeft: TExpr;
    FRight: TExpr;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
    destructor Destroy; override;
    function Eval: Variant; override;
    procedure Initialize; override;
    function Optimize: TExpr; override;
    procedure TypeCheck; override;
    property Left: TExpr read FLeft write FLeft;
    property right: TExpr read FRight write FRight;
  end;

  // length of dynamic arrays
  TArrayLengthExpr = class(TUnaryOpExpr)
  private
    FDelta: Integer;
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr; Delta: Integer);
    function Eval: Variant; override;
  end;

  TStringArrayOpExpr = class(TBinaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  TStringLengthExpr = class(TUnaryOpExpr)
  public
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    function Eval: Variant; override;
  end;

  // obj is TMyClass
  TIsOpExpr = class(TBinaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // obj as TMyClass
  TAsOpExpr = class(TBinaryOpExpr)
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // >, <, =, <=, >=, <>
  TRelOpExpr = class(TBinaryOpExpr)
    FRelOp: TRelOps;
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr; RelOp:
      TRelOps);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  TObjCmpExpr = class(TBinaryOpExpr)
    FEqual: Boolean;
    constructor Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr; Equal:
      Boolean);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // -x
  TNegExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  TNumberOpExpr = class(TBinaryOpExpr)
    procedure TypeCheck; override;
  end;

  TNumberBooleanOpExpr = class(TBinaryOpExpr)
  protected
    FMode: (omBoolean, omInteger);
  public
    procedure TypeCheck; override;
  end;

  TNumberStringOpExpr = class(TBinaryOpExpr)
    procedure TypeCheck; override;
  end;

  // a + b
  TAddExpr = class(TNumberStringOpExpr)
    function Eval: Variant; override;
  end;

  // a - b
  TSubExpr = class(TNumberOpExpr)
    function Eval: Variant; override;
  end;

  // a * b
  TMultExpr = class(TNumberOpExpr)
    function Eval: Variant; override;
  end;

  // a / b
  TDivideExpr = class(TNumberOpExpr)
    function Eval: Variant; override;
  end;

  // a div b
  TDivExpr = class(TNumberOpExpr)
    function Eval: Variant; override;
  end;

  // a mod b
  TModExpr = class(TNumberOpExpr)
    function Eval: Variant; override;
  end;

  // not a
  TNotExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // a and b
  TAndExpr = class(TNumberBooleanOpExpr)
    function Eval: Variant; override;
  end;

  // a or b
  TOrExpr = class(TNumberBooleanOpExpr)
    function Eval: Variant; override;
  end;

  // a xor b
  TXorExpr = class(TNumberBooleanOpExpr)
    function Eval: Variant; override;
  end;

  // Float(x)
  TConvFloatExpr = class(TUnaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // DateTime(x)
  TConvDateTimeExpr = class(TUnaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // Integer(float)
  TConvIntegerExpr = class(TUnaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // Variant(simple)
  TConvVariantExpr = class(TUnaryOpExpr)
    constructor Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
    function Eval: Variant; override;
    procedure TypeCheck; override;
  end;

  // A list of expressions
  TExprList = class(TList)
  protected
    function GetExpr(const x: Integer): TExpr;
    procedure SetExpr(const x: Integer; const Value: TExpr);
  public
    destructor Destroy; override;
    procedure AddExpr(AExpr: TExpr);
    procedure Initialize;
    procedure Optimize(Shrink: Boolean = True);
    procedure TypeCheck(ExpectedTyp : TSymbol);
    property Expr[const x: Integer]: TExpr read GetExpr write SetExpr; default;
  end;

  // Helper object for access to symbols
  IInfo = interface
    ['{8D534D16-4C6B-11D5-8DCB-0000216D9E86}']
    function Call: IInfo; overload;
    function Call(const Params: array of Variant): IInfo; overload;
    function Element(const Indices: array of Integer): IInfo;
    function GetConstructor(MethName: string; ExtObject: TObject): IInfo;
    function GetData: TData;
    function GetExternalObject: TObject;
    function GetMember(s: string): IInfo;
    function GetMethod(s: string): IInfo;
    function GetScriptObj: IScriptObj;
    function GetParameter(s: string): IInfo;
    function GetTypeSym: TSymbol;
    function GetValue: Variant;
    function GetInherited: IInfo;
    procedure SetData(const Data: TData);
    procedure SetExternalObject(ExtObject: TObject);
    procedure SetValue(const Value: Variant);
    property Data: TData read GetData write SetData;
    property ExternalObject: TObject read GetExternalObject write
      SetExternalObject;
    property Member[s: string]: IInfo read GetMember;
    property Method[s: string]: IInfo read GetMethod;
    property ScriptObj: IScriptObj read GetScriptObj;
    property Parameter[s: string]: IInfo read GetParameter;
    property TypeSym: TSymbol read GetTypeSym;
    property Value: Variant read GetValue write SetValue;
  end;


  // Informations about the program in external procedures
  TProgramInfo = class
  private
    FCaller: TProgram;
    FFuncSym: TFuncSymbol;
    FLevel: Integer;
    FScriptObj: IScriptObj;
    FTable: TSymbolTable;
    function GetData(s: string): TData;
    function GetFunc(s: string): IInfo;
    procedure SetFuncSym(const Value: TFuncSymbol);
    function GetValue(s: string): Variant;
    function GetVars(s: string): IInfo;
    procedure SetData(s: string; const Value: TData);
    procedure SetValue(s: string; const Value: Variant);
    procedure SetResult(const Value: Variant);
    function GetResult: Variant;
  protected
    function CreateUnitList: TList; dynamic;
    function FindSymbolInUnits(AUnitList: TList; const Name: string): TSymbol; overload; virtual;
  public
    constructor Create(Table: TSymbolTable; Caller: TProgram = nil);
    function RegisterExternalObject(AObject: TObject; AutoFree: Boolean=False; ExactClassMatch: Boolean=True): Variant;
    function GetExternalObjForVar(s: string): TObject;
    // cycle ancestry hierarchy and find the nearest matching type
    function FindClassMatch(AObject: TObject; ExactMatch: Boolean=True): TClassSymbol; dynamic;
    function FindSymbolInUnits(const Name: string): TSymbol; overload; virtual;
    function GetTemp(DataType: string): IInfo;
    property Caller: TProgram read FCaller write FCaller;
    property Data[s: string]: TData read GetData write SetData;
    property Func[s: string]: IInfo read GetFunc;
    property FuncSym: TFuncSymbol read FFuncSym write SetFuncSym;
    property Method[s: string]: IInfo read GetFunc;
    property ScriptObj: IScriptObj read FScriptObj write FScriptObj;
    property Result: Variant read GetResult write SetResult;
    property Value[s: string]: Variant read GetValue write SetValue; default;
    property Vars[s: string]: IInfo read GetVars;
  end;

  // A instance of a script class FClassSym. Instance data in FData,
  TScriptObj = class(TInterfacedObject, IScriptObj)
  private
    FClassSym: TClassSymbol;
    FData: TData;
    FExternalObj: TObject;
    FProg: TProgram;
    FOnObjectDestroy: TObjectDestroyEvent;
  protected
    { IScriptObj }
    function GetClassSym: TClassSymbol;
    function GetData: TData;
    procedure SetData(Dat: TData);
    function GetExternalObject: TObject;
    procedure SetExternalObject(Value: TObject);
  public
    constructor Create(ClassSym: TClassSymbol; Prog: TProgram = nil);
    destructor Destroy; override;
    procedure BeforeDestruction; override;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
  end;

  function GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
    Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean = False): TFuncExpr;

  function GetDataInfo(Typ: TSymbol) : IInfo;

implementation

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  SysUtils, dws2Functions, TypInfo
{$IFDEF WIN32},
  Windows
{$ENDIF};

type
  IDataMaster = interface
    ['{8D534D17-4C6B-11D5-8DCB-0000216D9E86}']
    function GetCaption: string;
    function GetSize: Integer;
    procedure Read(const Data: TData);
    procedure Write(const Data: TData);
    property Caption: string read GetCaption;
    property Size: Integer read GetSize;
  end;

  // private implementation of IInfo
  TInfo = class(TInterfacedObject, IUnknown, IInfo)
  protected
    FCaller: TProgram;
    FChild: IInfo;
    FData: TData;
    FOffset: Integer;
    FProgramInfo: TProgramInfo;
    FDataMaster: IDataMaster;
    FTypeSym: TSymbol;
    function GetData: TData; virtual;
    function GetExternalObject: TObject; virtual;
    function GetMember(s: string): IInfo; virtual;
    function GetMethod(s: string): IInfo; virtual;
    function GetScriptObj: IScriptObj; virtual;
    function GetParameter(s: string): IInfo; virtual;
    function GetTypeSym: TSymbol;
    function GetValue: Variant; virtual;
    function GetInherited: IInfo; virtual;
    procedure SetData(const Value: TData); virtual;
    procedure SetExternalObject(ExtObject: TObject); virtual;
    procedure SetValue(const Value: Variant); virtual;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; DataMaster: IDataMaster = nil);
    function Call: IInfo; overload; virtual;
    function Call(const Params: array of Variant): IInfo; overload; virtual;
    function Element(const Indices: array of Integer): IInfo; virtual;
    function GetConstructor(MethName: string; ExtObject: TObject): IInfo; virtual;
    class function SetChild(ProgramInfo: TProgramInfo; ChildTypeSym: TSymbol;
      ChildData: TData; ChildOffset: Integer; ChildDataMaster: IDataMaster = nil):
      IInfo;
  end;

  TInfoConst = class(TInfo)
  private
    FData: TData;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Value: Variant);
    function GetValue: Variant; override;
    function GetData: TData; override;
  end;

  TInfoData = class(TInfo)
    function GetValue: Variant; override;
    function GetData: TData; override;
    procedure SetData(const Value: TData); override;
    procedure SetValue(const Value: Variant); override;
  end;

  TInfoClass = class(TInfoData)
    FScriptObj: IScriptObj;
    function GetConstructor(MethName: string; ExtObject: TObject): IInfo; override;
    function GetMethod(s: string): IInfo; override;
    function GetScriptObj: IScriptObj; override;
    function GetInherited: IInfo; override;
  end;

  TInfoClassObj = class(TInfoClass)
    function GetMember(s: string): IInfo; override;
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; DataMaster: IDataMaster = nil);
  end;

  TInfoClassOf = class(TInfoClass)
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; DataMaster: IDataMaster = nil);
  end;

  TInfoRecord = class(TInfoData)
    function GetMember(s: string): IInfo; override;
  end;

  TInfoStaticArray = class(TInfoData)
    function Element(const Indices: array of Integer): IInfo; override;
    function GetMember(s: string): IInfo; override;
  end;

  TInfoDynamicArray = class(TInfoData)
    function Element(const Indices: array of Integer): IInfo; override;
    function GetMember(s: string): IInfo; override;
  end;

  TTempParam = class(TParamSymbol)
  private
    FData: TData;
    FIsVarParam: Boolean;
  public
    constructor Create(ParamSym: TSymbol);
    property Data: TData read FData;
    property IsVarParam: Boolean read FIsVarParam;
  end;

  TInfoFunc = class(TInfo)
  protected
    FClassSym: TClassSymbol;
    FExternalObject: TObject;
    FScriptObj: IScriptObj;
    FParams: TSymbolTable;
    FParamSize: Integer;
    FResult: TData;
    FTempParams: TSymbolTable;
    FTempParamSize: Integer;
    FUsesTempParams: Boolean;
    FForceStatic: Boolean;
    procedure InitTempParams;
    function GetParameter(s: string): IInfo; override;
    function GetExternalObject: TObject; override;
    procedure SetExternalObject(ExtObject: TObject); override;
    function GetInherited: IInfo; override;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; DataMaster: IDataMaster; ScriptObj: IScriptObj;
      ClassSym: TClassSymbol; ForceStatic: Boolean = False);
    destructor Destroy; override;
    function Call: IInfo; overload; override;
    function Call(const Params: array of Variant): IInfo; overload; override;
  end;

  TInfoProperty = class(TInfo)
  private
    FScriptObj: IScriptObj;
    FPropSym: TPropertySymbol;
    FTempParams: TSymbolTable;
    procedure AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
  protected
    procedure InitTempParams;
    function GetParameter(s: string): IInfo; override;
    function GetValue: Variant; override;
    function GetData: TData; override;
    procedure SetData(const Value: TData); override;
    procedure SetValue(const Value: Variant); override;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; PropSym: TPropertySymbol; ScriptObj: IScriptObj);
    destructor Destroy; override;
  end;

  TInfoConnector = class(TInfoData)
    function GetMethod(s: string): IInfo; override;
    function GetMember(s: string): IInfo; override;
  end;

  TInfoConnectorCall = class(TInfo)
  protected
    FName: string;
    FConnectorType: IConnectorType;
  public
    constructor Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol; const Data:
      TData; Offset: Integer; ConnectorType: IConnectorType; Name: string);
    function Call(const Params: array of Variant): IInfo; overload; override;
  end;

  TDataMaster = class(TInterfacedObject, IUnknown, IDataMaster)
  private
    FCaller: TProgram;
    FSym: TSymbol;
    function GetCaption: string;
    function GetSize: Integer;
  public
    constructor Create(Caller: TProgram; Sym: TSymbol);
    procedure Read(const Data: TData); virtual;
    procedure Write(const Data: TData); virtual;
  end;

  TExternalVarDataMaster = class(TDataMaster)
  public
    procedure Read(const Data: TData); override;
    procedure Write(const Data: TData); override;
  end;

  TConnectorMemberDataMaster = class(TDataMaster)
  private
    FBaseValue: Variant;
    FName: string;
  public
    constructor Create(Caller: TProgram; Sym: TSymbol; BaseValue: Variant; Name:
      string);
    procedure Read(const Data: TData); override;
    procedure Write(const Data: TData); override;
  end;

  TCleanUpEvent = procedure(ScriptObj: IScriptObj; ExternalObject: TObject) of object;

function GetFuncExpr(Prog: TProgram; FuncSym: TFuncSymbol; ScriptObj: IScriptObj;
  ClassSym: TClassSymbol; ForceStatic: Boolean = False): TFuncExpr;
begin
  if FuncSym is TMethodSymbol then
  begin
    if Assigned(ScriptObj) then
      Result := GetMethodExpr(
        TMethodSymbol(funcSym),
        TConstExpr.Create(Prog, NullPos, {ScriptObj.}ClassSym, ScriptObj),
        rkObjRef, NullPos, True, ForceStatic)
    else
      Result := GetMethodExpr(
        TMethodSymbol(funcSym),
        TConstExpr.Create(Prog, NullPos, ClassSym.ClassOf, ClassSym.Name),
        rkClassOfRef, NullPos, True, ForceStatic)
  end
  else
    Result := TFuncExpr.Create(Prog, NullPos, TFuncSymbol(funcSym), True);
end;

function GetMethodExpr(meth: TMethodSymbol; Expr: TDataExpr; RefKind: TRefKind;
  Pos: TScriptPos; IsInstruction: Boolean; ForceStatic : Boolean): TFuncExpr;
begin
  // Create the correct TExpr for a method symbol
  Result := nil;

  // Return the right expression
  case meth.Kind of
    fkFunction, fkProcedure:
      if meth.IsClassMethod then
      begin
        if not ForceStatic and meth.IsVirtual and (RefKind = rkClassOfRef) then
          if (Expr is TConstExpr) and (VarType(Expr.Eval) = varString) then
            Result := TClassMethodVirtualNameExpr.Create(Expr.Prog, Pos, meth, Expr,
              IsInstruction)
          else
            Result := TClassMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr,
              IsInstruction)
        else if not ForceStatic and meth.IsVirtual and (RefKind = rkObjRef) then
          if (Expr is TConstExpr) and (VarType(Expr.Eval) = varString) then
            Result := TClassMethodObjVirtualNameExpr.Create(Expr.Prog, Pos, meth, Expr,
              IsInstruction)
          else
            Result := TClassMethodObjVirtualExpr.Create(Expr.Prog, Pos, meth, Expr,
              IsInstruction)
        else
          Result := TClassMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
      end
      else
      begin
        Assert(RefKind = rkObjRef);
        if not ForceStatic and meth.IsVirtual then
          Result := TMethodVirtualExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
        else
          Result := TMethodStaticExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction);
      end;
    fkConstructor:
      if RefKind = rkClassOfRef then
      begin
        if not ForceStatic and meth.IsVirtual then
          Result := TConstructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
        else
          Result := TConstructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction);
      end
      else
      begin
        if not ForceStatic and meth.IsVirtual then
          Result := TConstructorVirtualObjExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
        else
          Result := TConstructorStaticObjExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction);
      end;
    fkDestructor:
      begin
        Assert(RefKind = rkObjRef);
        if not ForceStatic and meth.IsVirtual then
          Result := TDestructorVirtualExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
        else
          Result := TDestructorStaticExpr.Create(Expr.Prog, Pos, meth, Expr,
            IsInstruction)
      end;
  end;
end;

function GetDataInfo(Typ: TSymbol): IInfo;
var
  data: TData;
begin
  SetLength(data, Typ.Size);
  Typ.InitData(data, 0);
  Result := TInfo.SetChild(nil, Typ, data, 0);
end;

{ TProgram }

constructor TProgram.Create(SystemTable: TSymbolTable; ResultType: Tdws2ResultType; MaxDataSize: Integer; StackChunkSize: Integer = C_DefaultStackChunkSize);
begin
  FResultType := ResultType;
  FProgramState := psUndefined;

  FMsgs := TMsgs.Create;
  FRoot := Self;

  // Create the Symbol Dictionary
  FSymbolDictionary := TSymbolDictionary.Create;
  // Create Context Map
  FContextMap := TContextMap.Create;
  //Create Script Source List
  FSourceList := TScriptSourceList.Create;

  // Create the program stack
  FStack := TStack.Create(StackChunkSize, MaxDataSize);
  FAddrGenerator := TAddrGenerator.Create(0, agmPositive);
  FGlobalAddrGenerator := TAddrGenerator.Create(0, agmPositive);

  // Initialize the system table
  FRootTable := TSymbolTable.Create(SystemTable, FAddrGenerator);
  FTable := FRootTable;

  FInitExpr := TBlockExpr.Create(Self, NullPos);

  // Initialize shortcuts to often used symbols
  FTypBoolean := SystemTable.FindSymbol(SYS_BOOLEAN) as TTypeSymbol;
  FTypDateTime := SystemTable.FindSymbol(SYS_DATETIME) as TTypeSymbol;
  FTypFloat := SystemTable.FindSymbol(SYS_FLOAT) as TTypeSymbol;
  FTypInteger := SystemTable.FindSymbol(SYS_INTEGER) as TTypeSymbol;
  FTypString := SystemTable.FindSymbol(SYS_STRING) as TTypeSymbol;
  FTypVariant := SystemTable.FindSymbol(SYS_VARIANT) as TTypeSymbol;
  FTypNil := TNilSymbol.Create;
  FTypObject := TClassSymbol(SystemTable.FindSymbol(SYS_TOBJECT));
end;

destructor TProgram.Destroy;
begin
  FResult.Free;
  FExpr.Free;
  FInitExpr.Free;
  FRootTable.Free;
  FStack.Free;
  FAddrGenerator.Free;
  FGlobalAddrGenerator.Free;
  FTypNil.Free;
  FMsgs.Free;
  FSymbolDictionary.Free;
  FContextMap.Free;
  FSourceList.Free;
  inherited;
end;

// Starts the program but does not terminate it.
// Use .RunProgram() to run the main program or .Info property to call procedures.
// Call EndProgram() to terminate the program
procedure TProgram.BeginProgram;
begin
  try
    // Program is already running
    if FProgramState = psRunning then
      Msgs.AddErrorStop(RTE_ScriptAlreadyRunning);

    // Compilation terminated with errors
    if FProgramState = psUndefined then
      Msgs.AddErrorStop(RTE_CantRunScript);

    if FProgramState <> psReadyToRun then
      Msgs.AddErrorStop('ProgramState should be "ReadyToRun"');

    // Initialize Result
    FResult.Free;
    FResult := FResultType.CreateProgResult;

    Msgs.Clear;

    // Stack
    FStack.Reset;
    FStack.Push(
      FGlobalAddrGenerator.DataSize +
      FAddrGenerator.DataSize);
    FStack.SaveBp(0, FStack.BasePointer);

    FInfo := TProgramInfo.Create(FTable, Self);

    // Result
    FResult.InitializeProgram(Self);

    FProgramState := psRunning;

    // Debugger
    FIsDebugging := Assigned(FDebugger);
    if FIsDebugging then
      FDebugger.StartDebug(Self);

    // Initialize global variables
    FInitExpr.Eval;
  except
    on e: EScriptError do
      ;
    on e: Exception do
      FMsgs.AddExecutionError(e.Message);
  end;
end;

procedure TProgram.EndProgram;
begin
  if not (FProgramState in [psRunning, psRunningStopped]) then
    raise Exception.Create('Program was not started!');

  FProgramState := psTerminated;

  try
    // Result
    FResult.FinalizeProgram(Self);

    // Flags
    FIsDebugging := False;

    // Stack
    FStack.Pop(
      FAddrGenerator.DataSize +
      FGlobalAddrGenerator.DataSize);

    // Debugger
    if Assigned(FDebugger) then
      FDebugger.StopDebug(Self);

    FProgramState := psReadyToRun;

    FreeAndNil(FInfo);
  except
    on e: EScriptError do
      ;
    on e: Exception do
      Msgs.AddExecutionError(e.Message);
  end;
end;

procedure TProgram.Execute;
begin
  Execute(0);
end;

procedure TProgram.Execute(TimeoutValue: Integer);
begin
  BeginProgram;
  if ProgramState = psRunning then
    RunProgram(TimeoutValue);
  if ProgramState in [psRunning, psRunningStopped] then
    EndProgram;
end;

procedure TProgram.Evaluate;
begin
  try
    FExpr.Eval;
  except
    on e: EExit do // valid Exception
      ;
  else
//      Result.RunningError(Self); ??
    raise;
  end;
end;

procedure TProgram.RunProgram(TimeoutValue: Integer);
var
  terminator: TTerminatorThread;
begin
  try
    if FProgramState <> psRunning then
      raise Exception.Create('Program state psRunning expected');

    if TimeoutValue > 0 then
      terminator := TTerminatorThread.Create(Self, TimeoutValue * 1000)
    else if FTimeout > 0 then
      terminator := TTerminatorThread.Create(Self, FTimeout * 1000)
    else
      terminator := nil;

    try

      try
        // Run the script
        Evaluate;
      except
        on e: EBreak do
          Msgs.AddInfo(RTE_InvalidBreak);
        on e: EContinue do
          Msgs.AddInfo(RTE_InvalidContinue);
        on e: EScriptException do
          Msgs.AddExecutionError(e.Pos, e.Message);
        on e: EScriptError do
          ;
        on e: Exception do
          Msgs.AddExecutionError(e.Message);
      end;

    finally
      if Assigned(terminator) then
        terminator.Terminate;
    end;

    Msgs.SetScriptError(NullPos);
    
  except
    on e: EScriptError do
      ; // Error message in FMsgs
    on e: Exception do
      Msgs.AddExecutionError(e.Message);
  end;
end;


procedure TProgram.ExecuteParam(const Params: array of Variant);
begin
  ExecuteParam(Params, 0)
end;

procedure TProgram.ExecuteParam(const Params: array of Variant; TimeoutValue: Integer);
var
  x, index: Integer;
begin
  SetLength(FParameters, High(Params) - Low(Params) + 1);
  index := 0;
  for x := Low(Params) to High(Params) do
  begin
    FParameters[index] := Params[x];
    Inc(index);
  end;

  Execute(TimeoutValue);
end;

procedure TProgram.ExecuteParam(Params: OleVariant);
begin
  ExecuteParam(Params, 0);
end;

procedure TProgram.ExecuteParam(Params: OleVariant; TimeoutValue: Integer);
var
  x: Integer;
begin
  if VarIsArray(Params) then
  begin
    SetLength(FParameters, VarArrayHighBound(Params, 1) + 1);
    for x := 0 to VarArrayHighBound(Params, 1) do
      FParameters[x] := Params[x];
  end
  else
  begin
    SetLength(FParameters, 1);
    FParameters[0] := Params;
  end;

  Execute(TimeoutValue);
end;

procedure TProgram.DoStep(Expr: TExpr);
begin
  if FRoot.ProgramState = psRunningStopped then
    Msgs.AddExecutionStop(Expr.Pos, RTE_ScriptStopped);

  if FRoot.IsDebugging then
    FRoot.Debugger.DoDebug(Self, Expr);
end;

procedure TProgram.SetDebugger(const Value: IDebugger);
begin
  if FRoot = Self then
    FDebugger := Value
  else
    FRoot.Debugger := Value;
end;

procedure TProgram.Stop;
begin
  if FProgramState = psRunning then
    FProgramState := psRunningStopped;
end;

function TProgram.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

function TProgram.GetResult: Tdws2Result;
begin
  Result := FResult;
end;

procedure TProgram.SetResult(const Value: Tdws2Result);
begin
  FResult := Value;
end;

function TProgram.GetUserDef: TObject;
begin
  Result := FUserDef;
end;

procedure TProgram.SetUserDef(const Value: TObject);
begin
  FUserDef := Value;
end;

// Called by the compiler if compilation has been finished successfully
procedure TProgram.ReadyToRun;
begin
  if FProgramState = psUndefined then
    FProgramState := psReadyToRun;
end;

function TProgram.GetGlobalAddr(DataSize: Integer): Integer;
begin
  Result := FRoot.FAddrGenerator.GetStackAddr(DataSize);
end;

procedure TProgram.DestroyScriptObj(ScriptObj: IScriptObj);
var
  sym: TSymbol;
  func: TMethodSymbol;
  expr: TExpr;
begin
  try
    sym := ScriptObj.ClassSym.Members.FindSymbol(SYS_TOBJECT_DESTROY);

    if sym is TMethodSymbol then
    begin
      func := TMethodSymbol(sym);
      if (func.Kind = fkDestructor) and (func.Params.Count = 0) then
      begin
        expr :=
          TDestructorVirtualExpr.Create(Self, NullPos, func,
            TConstExpr.Create(Self, NullPos, ScriptObj.ClassSym, ScriptObj));
        try
          expr.Eval;
        finally
          expr.Free;
        end;
      end;
    end;
  except
    on e: Exception do
      Msgs.AddError(e.Message);
  end;
end;

procedure TProgram.AddClassCompleteInfo(Info: TClassCompleteErrorInfo);
begin
  // grow the array for the new entry
  SetLength(FClassCompleteNeeds, Length(FClassCompleteNeeds)+1);
  FClassCompleteNeeds[High(FClassCompleteNeeds)] := Info;
end;

procedure TProgram.RegisterExpr(Expr: TExpr);
begin
end;

procedure TProgram.UnregisterExpr(Expr: TExpr);
begin
end;

function TProgram.GetTempAddr(DataSize: Integer): Integer;
begin
  Assert(Root.ProgramState = psUndefined);
  Result := FAddrGenerator.GetStackAddr(DataSize);
end;

{ TProcedure }

procedure TProcedure.AssignTo(sym: TFuncSymbol);
begin
  // Add parameter symboltable into the symboltable chain
  FTable.InsertParent(0, sym.Params);
  sym.Executable := ICallable(Self);
  FFunc := sym;
end;

procedure TProcedure.Call(Caller: TProgram; Func: TFuncSymbol);
begin
  if Caller.Root = Root then
    Execute
  else begin
    raise Exception.Create('Feature not supported!');
    // TODO
  end;
end;

constructor TProcedure.Create(Parent: TProgram);
begin
  FParent := Parent;

  // Create a local symbol table and connect it to the parent symboltable
  FAddrGenerator := TAddrGenerator.Create(Parent.Level + 1, agmPositive);
  FRootTable := TSymbolTable.Create(Parent.Table, FAddrGenerator);
  FTable := FRootTable;

  FInitExpr := TBlockExpr.Create(Self, NullPos);

  // Connect the procedure to the root TProgram
  FRoot := Parent.Root;
  FMsgs := Parent.FMsgs;
  FTypBoolean := FRoot.TypBoolean;
  FTypFloat := FRoot.TypFloat;
  FTypInteger := FRoot.TypInteger;
  FTypNil := FRoot.TypNil;
  FTypString := FRoot.TypString;
  FTypVariant := FRoot.TypVariant;
  FTypDateTime := FRoot.FTypDateTime;
  FTypObject := FRoot.TypObject;
  FStack := Root.Stack;
  FSymbolDictionary := Parent.SymbolDictionary;
  FContextMap := Parent.ContextMap;
end;

destructor TProcedure.Destroy;
begin
  FRootTable.Free;
  FAddrGenerator.Free;
  FExpr.Free;
  FInitExpr.Free;
end;

procedure TProcedure.Execute;
begin
  // Allocate stack space for local variables
  FRoot.Stack.Push(FAddrGenerator.DataSize);

  // Run the procedure
  try
    FInitExpr.Eval;
    FExpr.Eval;
  finally
    // Free stack space for local variables
    FRoot.Stack.Pop(FAddrGenerator.DataSize);
  end;
end;

function TProcedure.GetResult: Tdws2Result;
begin
  Result := FRoot.Result;
end;

function TProcedure.GetUserDef: TObject;
begin
  Result := FRoot.UserDef;
end;

procedure TProcedure.InitSymbol(Symbol: TSymbol);
begin
  FTable.Initialize;
  FExpr.Initialize;
end;

procedure TProcedure.InitExpression(Expr: TExprBase);
begin
end;

function TProcedure.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  Result := FuncExpr;
end;

procedure TProcedure.SetResult(const Value: Tdws2Result);
begin
  Root.Result := Value;
end;

procedure TProcedure.SetUserDef(const Value: TObject);
begin
  FRoot.UserDef := Value;
end;

procedure TProcedure.Stop;
begin
  FRoot.Stop;
end;

{ Tdws2ResultType }

procedure Tdws2ResultType.AddResultSymbols(SymbolTable: TSymbolTable);
begin
  // no symbols
end;

function Tdws2ResultType.CreateProgResult: Tdws2Result;
begin
  Result := Tdws2Result.Create(Self);
end;

{ Tdws2Result }

constructor Tdws2Result.Create(ResultType: Tdws2ResultType);
begin
  FResultType := ResultType;
end;

procedure Tdws2Result.FinalizeProgram(Prog: TProgram);
begin
  if Assigned(FResultType.FOnFinalizeProgram) then
    FResultType.FOnFinalizeProgram(Prog);
end;

procedure Tdws2Result.InitializeProgram(Prog: TProgram);
begin
  if Assigned(FResultType.FOnInitializeProgram) then
    FResultType.FOnInitializeProgram(Prog);
end;

{ TTerminatorThread }

constructor TTerminatorThread.Create(Prog: TProgram; MilliSecToLive: Integer);
begin
  inherited Create(True);
  FProg := Prog;
  FMillisecRemaining := MilliSecToLive;
  FreeOnTerminate := True;
{$IFDEF WIN32}
  Priority := tpTimeCritical;
{$ENDIF}
  Resume;
end;

procedure TTerminatorThread.Execute;
begin
  while (not Terminated) and (FMillisecRemaining > 0) do
  begin
    Sleep(50);
    Dec(FMillisecRemaining, 50);
  end;

  if (not Terminated) and Assigned(FProg) then
    FProg.Stop;

  // Wait until TProgram terminates the thread
  while not Terminated do
    Sleep(10);
end;

{ TExpr }

constructor TExpr.Create(Prog: TProgram; Pos: TScriptPos);
begin
  inherited Create;
  FProg := Prog;
  FPos := Pos;
  FTyp := nil;
end;

procedure TExpr.AfterConstruction;
begin
  inherited;
  Prog.RegisterExpr(Self);
end;

procedure TExpr.BeforeDestruction;
begin
  Prog.UnregisterExpr(Self);
  inherited;
end;

function TExpr.CreateEDelphiObj(ClassName, Message: string): IScriptObj;
var
  info: TProgramInfo;
begin
  info := TProgramInfo.Create(FProg.Table, FProg);
  try
    Result := IScriptObj(IUnknown(
      Info.Vars[SYS_EDELPHI].Method[SYS_TOBJECT_CREATE].Call([
        ClassName, Message]).Value));
  finally
    info.Free;
  end;
end;

procedure TExpr.Initialize;
begin
end;

function IsType(Typ: TSymbol; BType: TBaseTypeId): Boolean;
begin
  Result := Assigned(Typ) and (Typ.BaseType is TBaseSymbol)
            and IsBaseTypeCompatible(TBaseSymbol(Typ.BaseType).Id, BType);
end;

function TExpr.IsBooleanType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypBooleanId);
end;

function TExpr.IsDateTimeType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypDateTimeId);
end;

function TExpr.IsFloatType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypFloatId);
end;

function TExpr.IsIntegerType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypIntegerId);
end;

function TExpr.IsNumberType(Typ: TSymbol): Boolean;
begin
  Result := IsIntegerType(Typ) or IsFloatType(Typ);
end;

function TExpr.IsStringType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypStringId);
end;

function TExpr.IsVariantType(Typ: TSymbol): Boolean;
begin
  Result := IsType(Typ,TypVariantId);
end;

function TExpr.Optimize: TExpr;
begin
  Result := Self;
end;

procedure TExpr.TypeCheck;
begin
end;

function TExpr.GetBaseType: TTypeSymbol;
begin
  if Assigned(Typ) then
    result := Typ.BaseType
  else
    result := nil;
end;

{ TNullExpr }

function TNullExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
end;

{ TDataExpr }

constructor TDataExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol);
begin
  inherited Create(Prog, Pos);
  FTyp := Typ;
  FIsWritable := True;
end;

function TDataExpr.Eval: Variant;
begin
  Result := Data[Addr];
end;

function TDataExpr.GetAddr: Integer;
begin
  Result := 0;
end;

procedure TDataExpr.AssignData(SourceData: TData; SourceAddr: Integer);
begin
  Assert(FIsWritable);
  CopyData(SourceData, SourceAddr, Data, Addr, Typ.Size);
end;

procedure TDataExpr.AssignValue(const Value: Variant);
begin
  Assert(FIsWritable);
  VarCopy(Data[Addr], Value);
end;

procedure TDataExpr.AssignExpr(Expr: TExpr);
begin
  Assert(FIsWritable);
  VarCopy(Data[Addr], Expr.Eval);
end;

procedure TDataExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  CopyData(DataExpr.Data, DataExpr.Addr, Data, Addr, Typ.Size);
end;

{ TVarExpr }

constructor TVarExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Typ: TSymbol; DataSym: TDataSymbol);
begin
  inherited Create(Prog, Pos, Typ);
  FDataSym := DataSym; // For optimization of TAssignExpr
  FStackAddr := DataSym.StackAddr;
end;

function TVarExpr.Eval: Variant;
begin
  Result := FProg.Stack.ReadValue(Addr);
end;

function TVarExpr.GetAddr: Integer;
begin
  Result := FProg.Stack.BasePointer + FStackAddr;
end;

function TVarExpr.GetData: TData;
begin
  Result := FProg.Stack.Data;
end;

procedure TVarExpr.AssignValue(const Value: Variant);
begin
  FProg.Stack.WriteValue(Addr, Value);
end;

procedure TVarExpr.AssignData(SourceData: TData; SourceAddr: Integer);
begin
  FProg.Stack.WriteData(SourceAddr, Addr, Typ.Size, SourceData);
end;

procedure TVarExpr.AssignExpr(Expr: TExpr);
begin
  FProg.Stack.WriteValue(Addr, Expr.Eval);
end;

procedure TVarExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  FProg.Stack.WriteData(DataExpr.Addr, Addr, Typ.Size, DataExpr.Data);
end;

{ TVarParentExpr }

constructor TVarParentExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol;
  DataSym: TDataSymbol);
begin
  inherited;
  FLevel := DataSym.Level;
end;

function TVarParentExpr.GetAddr: Integer;
begin
  Result := FProg.Stack.GetSavedBp(FLevel) + FStackAddr;
end;

type
  IVarParamData = interface
    function GetData: TData;
    function GetAddr: Integer;
    property Data: TData read GetData;
    property Addr: Integer read GetAddr;
  end;

  TVarParamData = class (TInterfacedObject, IVarParamData)
  private
    FData: TData;
    FAddr: Integer;
  protected
    function GetData: TData;
    function GetAddr: Integer;
  public
    constructor Create(Data: TData; Addr: Integer);
  end;

{ TVarParamData }

constructor TVarParamData.Create(Data: TData; Addr: Integer);
begin
  inherited Create;
  FData := Data;
  FAddr := Addr;
end;

function TVarParamData.GetAddr: Integer;
begin
  Result := FAddr;
end;

function TVarParamData.GetData: TData;
begin
  Result := FData;
end;

{ TVarParamExpr }

function TVarParamExpr.GetAddr: Integer;
begin
  Result := IVarParamData(IUnknown(FProg.Stack.Data[FProg.Stack.BasePointer + FStackAddr])).Addr;
end;

function TVarParamExpr.GetData: TData;
begin
  Result := IVarParamData(IUnknown(FProg.Stack.Data[FProg.Stack.BasePointer + FStackAddr])).Data;
end;

procedure TVarParamExpr.AssignData(SourceData: TData; SourceAddr: Integer);
begin
  CopyData(SourceData, SourceAddr, Data, Addr, Typ.Size);
end;

procedure TVarParamExpr.AssignValue(const Value: Variant);
begin
  VarCopy(Data[Addr], Value);
end;

procedure TVarParamExpr.AssignExpr(Expr: TExpr);
begin
  VarCopy(Data[Addr], Expr.Eval);
end;

procedure TVarParamExpr.AssignDataExpr(DataExpr: TDataExpr);
begin
  CopyData(DataExpr.Data, DataExpr.Addr, Data, Addr, Typ.Size);
end;

function TVarParamExpr.Eval: Variant;
begin
  Result := Data[Addr];
end;

{ TVarParamParentExpr }

constructor TVarParamParentExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ:
  TSymbol; DataSym: TDataSymbol);
begin
  inherited;
  FLevel := DataSym.Level;
end;

function TVarParamParentExpr.GetAddr: Integer;
begin
  Result := IVarParamData(IUnknown(FProg.Stack.Data[FProg.Stack.GetSavedBp(FLevel) + FStackAddr])).Addr;
end;

function TVarParamParentExpr.GetData: TData;
begin
  Result := IVarParamData(IUnknown(FProg.Stack.Data[FProg.Stack.GetSavedBp(FLevel) + FStackAddr])).Data;
end;

{ TConstExpr }

constructor TConstExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; Value: Variant);
begin
  inherited Create(Prog, Pos, Typ);
  SetLength(FData, Typ.Size);
  FData[0] := Value;
  FIsWritable := False;
end;

constructor TConstExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol; Data: TData);
begin
  inherited Create(Prog, Pos, Typ);
  FData := Data;
  FIsWritable := False;
end;

function TConstExpr.Eval: Variant;
begin
  Result := FData[0];
end;

function TConstExpr.GetData: TData;
begin
  Result := FData;
end;

{ TArrayExpr }

constructor TArrayExpr.Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TExpr);
begin
  inherited Create(Prog, Pos, BaseExpr.BaseType.Typ);
  FBaseExpr := BaseExpr;
  FIndexExpr := IndexExpr;
  FElementSize := FTyp.Size; // Necessary because of arrays of records!
  FIsWritable := BaseExpr.IsWritable;
end;

destructor TArrayExpr.Destroy;
begin
  FBaseExpr.Free;
  FIndexExpr.Free;
  inherited;
end;

procedure TArrayExpr.Initialize;
begin
  FBaseExpr.Initialize;
  FIndexExpr.Initialize;
  inherited;
end;

function TArrayExpr.Optimize: TExpr;
begin
  FBaseExpr := FBaseExpr.Optimize as TDataExpr;
  FIndexExpr := FIndexExpr.Optimize;
  Result := inherited Optimize;
end;

{ TStaticArrayExpr }

constructor TStaticArrayExpr.Create(Prog: TProgram; Pos: TScriptPos; BaseExpr: TDataExpr; IndexExpr: TExpr; LowBound, HighBound: Integer);
begin
  inherited Create(Prog, Pos, BaseExpr, IndexExpr);
  FLowBound := LowBound;
  FLastIndex := HighBound - LowBound;
end;

function TStaticArrayExpr.GetAddr: Integer;
var
  index: Integer;
begin
  // Get index
  index := FIndexExpr.Eval - FLowBound;

  if index > FLastIndex then
    FProg.FMsgs.AddExecutionStop(FIndexExpr.Pos, RTE_UpperBoundExceeded);

  if index < 0 then
    FProg.FMsgs.AddExecutionStop(FIndexExpr.Pos, RTE_LowerBoundExceeded);

  // Calculate the address
  Result := FBaseExpr.Addr + (index * FElementSize);
end;

function TStaticArrayExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

{ TDynamicArrayExpr }

function TDynamicArrayExpr.GetAddr: Integer;
var
  index, length: Integer;
  baseAddr: Integer;
begin
  baseAddr := FBaseExpr.Eval;
  index := FIndexExpr.Eval;

  length := FProg.Stack.Data[baseAddr - 1];

  if index >= length then
    FProg.FMsgs.AddExecutionStop(FIndexExpr.Pos, RTE_UpperBoundExceeded);

  if index < 0 then
    FProg.FMsgs.AddExecutionStop(FIndexExpr.Pos, RTE_LowerBoundExceeded);

  // Calculate the address
  Result := baseAddr + (index * FElementSize);
end;

function TDynamicArrayExpr.GetData: TData;
begin
  Result := FProg.Stack.Data;
end;

{ TArrayConstantExpr }

constructor TArrayConstantExpr.Create(Prog: TProgram; Pos: TScriptPos);
begin
  inherited Create(Prog, Pos, TDynamicArraySymbol.Create('', Prog.TypNil));
  FElementExprs := TExprList.Create;
  FIsWritable := False;
  SetLength(FData, 1);
end;

destructor TArrayConstantExpr.Destroy;
begin
  FElementExprs.Free;
  FTyp.Free;
  inherited;
end;

procedure TArrayConstantExpr.AddElementExpr(ElementExpr: TExpr);
begin
  if FElementExprs.Count = 0 then
  begin
    FTyp.Free;
    FTyp := TDynamicArraySymbol.Create('', ElementExpr.Typ);
  end;
  FElementExprs.Add(ElementExpr);
end;

procedure TArrayConstantExpr.Prepare(ElementTyp : TSymbol);
var
  x : Integer;
begin
  if FTyp.Typ <> ElementTyp then
  begin
    FTyp.Free;
    FTyp := TDynamicArraySymbol.Create('', ElementTyp);
  end;
  
  for x := 0 to FElementExprs.Count - 1 do
    if FElementExprs[x] is TArrayConstantExpr then
      TArrayConstantExpr(FElementExprs[x]).Prepare(FTyp.Typ.Typ);

  FArrayAddr := FProg.GetGlobalAddr(FElementExprs.Count * FTyp.Typ.Size + 1);
  FData[0] := FArrayAddr + 1;
end;

function TArrayConstantExpr.GetData: TData;
begin
  Eval;
  Result := FData;
end;

function TArrayConstantExpr.Eval: Variant;
var
  x: Integer;
  elemSize: Integer;
begin
  FProg.Stack.WriteValue(FArrayAddr, FElementExprs.Count);

  elemSize := FTyp.Typ.Size;
  if elemSize = 1 then
  begin
    for x := 0 to FElementExprs.Count - 1 do
    begin
      FProg.Stack.WriteValue(FArrayAddr + 1 + x,FElementExprs[x].Eval);
    end;
  end
  else begin
    for x := 0 to FElementExprs.Count - 1 do
    begin
      FProg.Stack.WriteData(
        TDataExpr(FElementExprs[x]).Addr,
        FArrayAddr + 1 + x * elemSize,
        elemSize,
        TDataExpr(FElementExprs[x]).Data);
    end;
  end;
  Result := FArrayAddr + 1;
end;

function TArrayConstantExpr.Optimize: TExpr;
begin
  FElementExprs.Optimize(False);
  result := inherited Optimize;
end;

procedure TArrayConstantExpr.Initialize;
begin
  inherited;
  FElementExprs.Initialize;
end;

procedure TArrayConstantExpr.TypeCheck;
begin
  FElementExprs.TypeCheck(Typ.Typ);
end;

{ TRecordExpr }

constructor TRecordExpr.Create(Prog: TProgram; Pos: TScriptPos;
  BaseExpr: TDataExpr; MemberSymbol: TMemberSymbol);
begin
  inherited Create(Prog, Pos, MemberSymbol.Typ);
  FBaseExpr := BaseExpr;
  FMemberOffset := MemberSymbol.Offset;
  FIsWritable := FBaseExpr.IsWritable;
end;

destructor TRecordExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TRecordExpr.GetAddr: Integer;
begin
  Result := FBaseExpr.Addr + FMemberOffset;
end;

function TRecordExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

procedure TRecordExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

{ TBlockExpr }

procedure TBlockExpr.AddStatement(Expr: TExpr);
begin
  FStatements.Add(Expr);
end;

constructor TBlockExpr.Create(Prog: TProgram; Pos: TScriptPos);
begin
  inherited Create(Prog, Pos);
  FStatements := TExprList.Create;
  FTable := TSymbolTable.Create(Prog.Table, Prog.Table.AddrGenerator);
end;

destructor TBlockExpr.Destroy;
begin
  FStatements.Free;
  FTable.Free;
  inherited;
end;

function TBlockExpr.Eval: Variant;
var
  x: Integer;
  oldTable: TSymbolTable;
begin
  oldTable := FProg.Table;
  try
    FProg.Table := FTable;
    for x := 0 to FStatements.Count - 1 do
      FStatements[x].Eval;
  finally
    FProg.Table := oldTable;
  end;
end;

procedure TBlockExpr.Initialize;
begin
  FStatements.Initialize;
end;

function TBlockExpr.Optimize: TExpr;
begin
  FStatements.Optimize;
  case FStatements.Count of
    0:
      begin
        Result := TNullExpr.Create(Prog, Pos);
        Free;
      end;
    1:
      begin
        Result := FStatements[0];
        FStatements.Clear;
        Free;
      end;
  else
    Result := Self;
  end;
end;

{ TPushExpr }

constructor TPushExpr.Create(Prog: TProgram; Pos: TScriptPos; StackAddr: Integer;
  ArgExpr: TExpr);
begin
  inherited Create(Prog, Pos);
  FStackAddr := StackAddr;
  FArgExpr := ArgExpr;
end;

function TPushExpr.Eval: Variant;
begin
  // Unused. Use .Execute() instead
end;

{ TPushAddrExpr }

procedure TPushAddrExpr.Execute;
var
  vpd: IVarParamData;
begin
  vpd := TVarParamData.Create(TDataExpr(FArgExpr).Data, TDataExpr(FArgExpr).Addr);
  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FStackAddr, vpd);
end;

{ TPushResultExpr }

procedure TPushResultExpr.Execute;
begin
  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FStackAddr, FArgExpr.Eval);
end;

{ TPushDataExpr }

constructor TPushDataExpr.Create(Prog: TProgram; Pos: TScriptPos;
  StackAddr: Integer; ArgExpr: TExpr; ParamSym: TSymbol);
begin
  inherited Create(Prog, Pos, StackAddr, ArgExpr);
  FParamSym := ParamSym;
end;

procedure TPushDataExpr.Execute;
begin
  FProg.Stack.WriteData(
    TDataExpr(FArgExpr).Addr,
    FProg.Stack.StackPointer + FStackAddr,
    FParamSym.Typ.Size,
    TDataExpr(FArgExpr).Data);
end;

{ TFuncExpr }

constructor TFuncExpr.Create(Prog: TProgram; Pos: TScriptPos; Func: TFuncSymbol;
  IsInstruction : Boolean; CodeExpr: TDataExpr; IsWritable: Boolean);
var
  initData: TData;
begin
  inherited Create(Prog, Pos, nil);
  FIsInstruction := IsInstruction;
  FArgs := TExprList.Create;
  FPushExprs := TExprList.Create;
  FIsWritable := IsWritable;
  FResultAddr := -1;
  FCodeExpr := CodeExpr;

  FFunc := Func;
  FTyp := Func.Typ;

  if Assigned(FTyp) then
  begin
    FHasResult := True;

    // Initialize Result
    SetLength(initData, FTyp.Size);
    FTyp.InitData(initData, 0);
    FInitResultExpr := TConstExpr.Create(Prog, Pos, FTyp, initData);

    SetResultAddr;
  end;
end;

destructor TFuncExpr.Destroy;
begin
  FArgs.Free;
  FPushExprs.Free;
  FInitResultExpr.Free;
  FCodeExpr.Free;
  inherited;
end;

procedure TFuncExpr.AddArg(Arg: TExpr);
begin
  if (FArgs.Count < FFunc.Params.Count) and (Arg is TFuncExpr)
     and (FFunc.Params[FArgs.Count].Typ is TFuncSymbol) then
    Arg := TFuncCodeExpr.Create(Prog,Pos,TFuncExpr(Arg));

  FArgs.AddExpr(Arg);
end;

function TFuncExpr.Eval: Variant;
var
  x: Integer;
  sourceData: TData;
  prevProgBp, oldBasePointer: Integer;
  func: TFuncSymbol;
  scriptObj: IScriptObj;
  code: ICallable;
begin
  try
    if FIsInstruction then
      FProg.DoStep(Self);

    if FProg.Root.IsDebugging then
      FProg.Root.Debugger.EnterFunc(FProg, Self);

    sourceData := nil;

    // Allocate memory for parameters on the stack
    FProg.Stack.Push(FFunc.ParamSize);
    try

      // Push parameters
      for x := 0 to FPushExprs.Count - 1 do
        TPushExpr(FPushExprs[x]).Execute;

      // Special operations
      func := PreCall(scriptObj);

      code := GetCode(func);
      if not Assigned(Code) then
        FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidFunctionCall);

      // Switch frame
      FProg.Stack.SwitchFrame(oldBasePointer);
      prevProgBp := FProg.Stack.SaveBp(FProg.Level, oldBasePointer);

      // Call function
      try
        try
          // The call itself
          code.Call(FProg, func);
        except
          on e: EExit do
            ;
          on e: EBreak do
            FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidBreak);
          on e: EContinue do
            FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidContinue);
          on e: EScriptException do
            raise;
        else
          FProg.Msgs.SetScriptError(FPos,ExceptObject);
          raise;
        end;
      finally
        // Restore frame
        FProg.Stack.RestoreFrame(oldBasePointer);
        FProg.Stack.SaveBp(FProg.Level, prevProgBp);

        Result := PostCall(scriptObj);
      end;

    finally
      // Remove parameters from stack
      FProg.Stack.Pop(FFunc.ParamSize);

      if FProg.Root.IsDebugging then
        FProg.Root.Debugger.LeaveFunc(FProg, Self);
    end;
  except
    FProg.Msgs.SetScriptError(FPos,ExceptObject);
    raise;
  end;
end;

function TFuncExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;
end;

function TFuncExpr.PostCall(ScriptObj: IScriptObj): Variant;
var
  data: TData;
  sourceAddr, destAddr: Integer;
begin
  if FHasResult then
  begin
    // Copy return value
    data := FProg.Stack.Data;
    // Result.StackAddr is relative to BasePointer of the called function
    // But the frame is already restored so its relative to the stackpointer here 
    sourceAddr := FProg.Stack.StackPointer + FFunc.Result.StackAddr;

    Result := data[sourceAddr];

    if FResultAddr >= 0 then
    begin
      destAddr := FProg.Stack.BasePointer + FResultAddr;
      FProg.Stack.CopyData(sourceAddr, destAddr, FFunc.Typ.Size);
    end;
  end
  else
    data:= nil;
end;

procedure TFuncExpr.TypeCheck;
var
  Arg: TExpr;
  x, paramCount: Integer;
begin
  paramCount := FFunc.Params.Count;

  // Check number of arguments = number of parameters
  if FArgs.Count > paramCount then
    FProg.FMsgs.AddCompilerStop(FPos, CPE_TooManyArguments);

  while Args.Count < paramCount do
  begin
    // Complete missing args by default values
    if Assigned(TParamSymbol(FFunc.Params[FArgs.Count]).DefaultValue) then
      FArgs.AddExpr(
        TConstExpr.Create(Prog, Pos, FFunc.Params[FArgs.Count].Typ,
          TParamSymbol(FFunc.Params[FArgs.Count]).DefaultValue))
    else
      FProg.FMsgs.AddCompilerStop(FPos, CPE_TooLessArguments);
  end;

  for x := 0 to FArgs.Count - 1 do
  begin
    arg := FArgs[x];

    if arg is TArrayConstantExpr then
      TArrayConstantExpr(arg).Prepare(FFunc.Params[x].Typ.Typ);

    // Check arguments type
    arg.TypeCheck;

    // Expand integer arguments to float if necessary
    if (FFunc.Params[x].Typ = FProg.TypFloat) and (Arg.Typ = FProg.TypInteger) then
      arg := TConvFloatExpr.Create(FProg, FPos, arg);
    Args[x] := arg;

    if arg.Typ = nil then
      FProg.FMsgs.AddCompilerError(arg.Pos, Format(CPE_WrongArgumentType, [x,
        FFunc.Params[x].Typ.Caption]))
    else if (FFunc.Params[x] is TVarParamSymbol) and (arg is TDataExpr)
      and not TDataExpr(arg).IsWritable and TVarParamSymbol(FFunc.Params[x]).IsWritable then
      FProg.FMsgs.AddCompilerError(arg.Pos, Format(CPE_ConstVarParam, [x]))
    else if not FFunc.Params[x].Typ.IsCompatible(arg.Typ) then
      FProg.FMsgs.AddCompilerError(arg.Pos,
        Format(CPE_WrongArgumentType_Long, [x, FFunc.Params[x].Typ.Caption,
        arg.Typ.Caption]));
  end;
end;

function TFuncExpr.GetData: TData;
begin
  Eval;
  Result := FProg.Stack.Data;
end;

function TFuncExpr.GetAddr: Integer;
begin
  Result := FProg.Stack.BasePointer + FResultAddr;
end;

function TFuncExpr.OptimizeExecutable: TExpr;
begin
  if Assigned(FFunc.Executable) then
    Result := TExpr(FFunc.Executable.Optimize(Self))
  else
    Result := Self;
end;

function TFuncExpr.Optimize: TExpr;
begin
  if Assigned(FCodeExpr) then
    FCodeExpr := FCodeExpr.Optimize as TDataExpr;
  FArgs.Optimize(False); // do not shrink
  Result := OptimizeExecutable;
end;

procedure TFuncExpr.AddPushExprs;
var
  x: Integer;
  arg: TExpr;
  param: TParamSymbol;
begin
  FPushExprs.Free;
  FPushExprs := TExprList.Create;

  for x := 0 to Args.Count - 1 do
  begin
    arg := Args[x];
    param := TParamSymbol(FFunc.Params[x]);
    if arg is TDataExpr then
    begin
      if param is TVarParamSymbol then
        FPushExprs.Add(TPushAddrExpr.Create(FProg, arg.Pos, param.StackAddr, arg))
      else if param.Size > 1 then
        FPushExprs.Add(TPushDataExpr.Create(FProg, arg.Pos, param.StackAddr, arg, param))
      else
        FPushExprs.Add(TPushResultExpr.Create(FProg, arg.Pos, param.StackAddr, arg))
    end
    else
      FPushExprs.Add(TPushResultExpr.Create(FProg, arg.Pos, param.StackAddr, arg))
  end;

  if Assigned(FInitResultExpr) then
    FPushExprs.Add(TPushDataExpr.Create(FProg, Pos,
      FFunc.Result.StackAddr, FInitResultExpr, FFunc.Result));
end;

procedure TFuncExpr.Initialize;
begin
  if Assigned(FCodeExpr) then
    FCodeExpr.Initialize
  else if Assigned(FFunc.Executable) then
    FFunc.Executable.InitExpression(Self);
  Args.Initialize;
  AddPushExprs;
end;

procedure TFuncExpr.SetResultAddr(ResultAddr: Integer);
begin
  if ResultAddr = -1 then
  begin
    if FProg.Root.ProgramState = psUndefined then
      FResultAddr := FProg.GetTempAddr(FTyp.Size)
    else
      FResultAddr := -1; // TFuncExpr.Create called from TInfoFunc.Call
  end
  else
    FResultAddr := ResultAddr;
end;

function TFuncExpr.GetCode(Func: TFuncSymbol): ICallable;
begin
  if Assigned(FCodeExpr) then
    Result := ICallable(IUnknown(FCodeExpr.Eval))
  else
    Result := ICallable(Func.Executable);
end;

{ TExprList }

procedure TExprList.AddExpr(AExpr: TExpr);
begin
  Add(AExpr);
end;

destructor TExprList.Destroy;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Self[x].Free;
  inherited;
end;

function TExprList.GetExpr(const x: Integer): TExpr;
begin
  Result := TExpr(Get(x));
end;

procedure TExprList.SetExpr(const x: Integer; const Value: TExpr);
begin
  Items[x] := Value;
end;

procedure TExprList.Optimize(Shrink: Boolean);
var
  x: Integer;
begin
  for x := Count - 1 downto 0 do
  begin
    Self[x] := Self[x].Optimize;
    if Shrink and (Self[x] is TNullExpr) then
    begin
      Self[x].Free;
      Delete(x);
    end;
  end;
end;

procedure TExprList.Initialize;
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
    Self[x].Initialize;
end;

procedure TExprList.TypeCheck(ExpectedTyp: TSymbol);
var
  x: Integer;
begin
  for x := 0 to Count - 1 do
  begin
    Self[x].TypeCheck;
    if not Self[x].Typ.IsCompatible(ExpectedTyp) then
      Self[x].Prog.Msgs.AddCompilerError(Self[x].Pos,
        Format(CPE_AssignIncompatibleTypes,[Self[x].Typ.Caption, ExpectedTyp.Caption]));
  end;
end;

{ TRelOpExpr }

constructor TRelOpExpr.Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr;
  RelOp: TRelOps);
begin
  inherited Create(Prog, Pos, Left, Right);
  FRelOp := RelOp;
  FTyp := FProg.TypBoolean;
end;

function TRelOpExpr.Eval: Variant;
begin
  case FRelOp of
    roEqual:
      Result := FLeft.Eval = FRight.Eval;
    roUnEqual:
      Result := FLeft.Eval <> FRight.Eval;
    roLess:
      Result := FLeft.Eval < FRight.Eval;
    roLessEqual:
      Result := FLeft.Eval <= FRight.Eval;
    roMore:
      Result := FLeft.Eval > FRight.Eval;
    roMoreEqual:
      Result := FLeft.Eval >= FRight.Eval;
  end;
end;

procedure TRelOpExpr.TypeCheck;
begin
  inherited;
  if not (FLeft.Typ.IsCompatible(FRight.Typ)) then
    FProg.FMsgs.AddCompilerStop(FPos, CPE_InvalidOperands);
end;

{ TBinaryOpExpr }

constructor TBinaryOpExpr.Create(Prog: TProgram; Pos: TScriptPos; Left, Right:
  TExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := Left;
  FRight := Right;
end;

destructor TBinaryOpExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function TBinaryOpExpr.Eval: Variant;
begin
  Result := FLeft.Eval + FRight.Eval;
end;

procedure TBinaryOpExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

function TBinaryOpExpr.Optimize: TExpr;
begin
  FLeft := FLeft.Optimize;
  FRight := FRight.Optimize;
  if (FLeft is TConstExpr) and (FRight is TConstExpr) then
  begin
    Result := TConstExpr.Create(FProg, FPos, FTyp, Self.Eval);
    Self.Free;
  end
  else
    Result := Self;
end;

procedure TBinaryOpExpr.TypeCheck;
begin
  FLeft.TypeCheck;
  FRight.TypeCheck;
  if (FLeft.Typ = FProg.TypInteger) and (FRight.Typ = FProg.TypFloat) then
    FLeft := TConvFloatExpr.Create(FProg, FPos, FLeft)
  else if (FLeft.Typ = FProg.TypFloat) and (FRight.Typ = FProg.TypInteger) then
    FRight := TConvFloatExpr.Create(FProg, FPos, FRight)
end;

{ TNumberOpExpr }

procedure TNumberOpExpr.TypeCheck;
begin
  inherited;
  if IsIntegerType(FLeft.Typ) and IsIntegerType(FRight.Typ) then
    FTyp := FProg.TypInteger
  else if IsFloatType(FLeft.Typ) and IsFloatType(FRight.Typ) then
    FTyp := FProg.TypFloat
  else if IsVariantType(FLeft.Typ) and IsVariantType(FRight.Typ) then
    FTyp := FProg.TypVariant
  else
    FProg.FMsgs.AddCompilerStop(FPos, CPE_InvalidOperands);
end;

{ TNumberBooleanOpExpr }

procedure TNumberBooleanOpExpr.TypeCheck;
begin
  inherited;
  if IsBooleanType(FLeft.Typ) and IsBooleanType(FRight.Typ) then
  begin
    FMode := omBoolean;
    FTyp := FProg.TypBoolean;
  end
  else if IsVariantType(FLeft.Typ) and IsVariantType(FRight.Typ) or
    IsIntegerType(FLeft.Typ) and IsIntegerType(FRight.Typ) then
  begin
    FMode := omInteger;
    FTyp := FProg.TypInteger;
  end
  else
    FProg.FMsgs.AddCompilerStop(FPos, CPE_InvalidOperands);
end;

{ TNumberStringOpExpr }

procedure TNumberStringOpExpr.TypeCheck;
begin
  inherited;
  if IsIntegerType(FLeft.Typ) and IsIntegerType(FRight.Typ) then
    FTyp := FProg.TypInteger
  else if IsFloatType(FLeft.Typ) and IsFloatType(FRight.Typ) then
    FTyp := FProg.TypFloat
  else if IsStringType(FLeft.Typ) and IsStringType(FRight.Typ) then
    FTyp := FProg.TypString
  else if IsVariantType(FLeft.Typ) and IsVariantType(FRight.Typ) then
    FTyp := FProg.TypVariant
  else
    FProg.FMsgs.AddCompilerStop(FPos, CPE_IncompatibleOperands);
end;

{ TAssignExpr }

constructor TAssignExpr.Create(Prog: TProgram; Pos: TScriptPos; Left,
  Right: TExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := TDataExpr(Left);
  FRight := Right;
end;

destructor TAssignExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function TAssignExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  FLeft.AssignExpr(FRight);
end;

procedure TAssignExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

function TAssignExpr.Optimize: TExpr;
begin
  FLeft := FLeft.Optimize as TDataExpr;
  FRight := FRight.Optimize;
  if (FLeft is TVarExpr) and (FRight is TAddExpr) then
  begin
    // detected "v:=x+y"
    if (TAddExpr(FRight).FLeft is TVarExpr)
      and (TVarExpr(TAddExpr(FRight).FLeft).DataSym = TVarExpr(FLeft).DataSym) then
    begin
      // case "v:=v+y"
      Result := TIncrExpr.Create(Prog, Pos, FLeft, TAddExpr(FRight).FRight);
      FLeft := nil;
      TAddExpr(FRight).FRight := nil;
      Free;
    end
    else if (TAddExpr(FRight).FRight is TVarExpr)
      and (TVarExpr(TAddExpr(FRight).FRight).DataSym = TVarExpr(FLeft).DataSym)
      and ((FLeft.Typ = FProg.TypInteger) or (FRight.Typ = FProg.TypFloat)) then
    begin
      // case "v:=y+v"
      Result := TIncrExpr.Create(Prog, Pos, FLeft, TAddExpr(FRight).FLeft);
      FLeft := nil;
      TAddExpr(FRight).FLeft := nil;
      Free;
    end
    else
      Result := Self;
  end
  else
    Result := Self;
end;

procedure TAssignExpr.TypeCheck;
var
  cleft, cright: string;
begin
  if FLeft.Typ = nil then
    cleft := SYS_VOID
  else
    cleft := FLeft.Typ.Caption;

  if FRight.Typ = nil then
    cright := SYS_VOID
  else
    cright := FRight.Typ.Caption;

  if (FRight.Typ = nil) or (FLeft.Typ = nil) then
    FProg.FMsgs.AddCompilerError(FPos, Format(CPE_AssignIncompatibleTypes, [cright,
      cleft]))
  else begin
    if FRight is TArrayConstantExpr then
      TArrayConstantExpr(FRight).Prepare(FLeft.Typ.Typ);

    FRight.TypeCheck;

    // Automatic conversion from int to float values
    if (FLeft.Typ = FProg.TypFloat) and (FRight.Typ = FProg.TypInteger) then
      FRight := TConvFloatExpr.Create(FProg, FPos, FRight);

    // Look if Types are compatible
    if not FLeft.Typ.IsCompatible(FRight.Typ) then
      FProg.FMsgs.AddCompilerError(FPos, Format(CPE_AssignIncompatibleTypes, [cright,
        cleft]));
  end;
end;

{ TAssignDataExpr }

constructor TAssignDataExpr.Create(Prog: TProgram; Pos: TScriptPos; Left, Right:
  TExpr);
begin
  inherited Create(Prog, Pos, Left, Right);
  FSize := FLeft.Typ.Size;
end;

function TAssignDataExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  FLeft.AssignDataExpr(TDataExpr(FRight));
end;

function TAssignDataExpr.Optimize: TExpr;
begin
  FLeft := FLeft.Optimize as TDataExpr;
  FRight := FRight.Optimize;
  Result := Self;
end;

{ TUnaryOpExpr }

constructor TUnaryOpExpr.Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
begin
  inherited Create(Prog, Pos);
  FExpr := Expr;
end;

destructor TUnaryOpExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

procedure TUnaryOpExpr.Initialize;
begin
  FExpr.Initialize;
end;

function TUnaryOpExpr.Optimize: TExpr;
begin
  FExpr := FExpr.Optimize;
  if FExpr is TConstExpr then
  begin
    Result := TConstExpr.Create(FProg, FPos, FTyp, Self.Eval);
    Self.Free;
  end
  else
    Result := Self;
end;

{ TMethodStaticExpr }

constructor TMethodStaticExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Func: TMethodSymbol; BaseExpr: TDataExpr; IsInstruction: Boolean;
  CodeExpr: TDataExpr; IsWritable: Boolean);
begin
  inherited Create(Prog, Pos, Func, IsInstruction, CodeExpr, IsWritable);
  FBaseExpr := BaseExpr;
  if not Func.IsClassMethod then
    FSelfAddr := TDataSymbol(Func.SelfSym).StackAddr;
end;

destructor TMethodStaticExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TMethodStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;
  // Set Self variable
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
  VarCopy(FProg.Stack.Data[FProg.Stack.StackPointer + FSelfAddr], ScriptObj);
end;

procedure TMethodStaticExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

function TMethodStaticExpr.Optimize: TExpr;
begin
  FBaseExpr := FBaseExpr.Optimize as TDataExpr;
  Result := inherited Optimize;
end;

{ TClassMethodStaticExpr }

function TClassMethodStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;
end;

{ TConstructorStaticExpr }

constructor TConstructorStaticExpr.Create(Prog: TProgram; Pos: TScriptPos; Func:
  TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean = True);
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  if Base.Typ is TClassOfSymbol then
    FTyp := Base.Typ.Typ
  else
    FTyp := Base.Typ;
end;

function TConstructorStaticExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FFunc;

  // Create object
  ScriptObj := TScriptObj.Create(TClassSymbol(FTyp), FProg);
  ScriptObj.ExternalObject := ExternalObject;

  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

function TConstructorStaticExpr.PostCall(ScriptObj: IScriptObj): Variant;
begin
  Assert(FResultAddr = -1);
  Result := ScriptObj;
end;

procedure TConstructorStaticExpr.TypeCheck;
begin
  if TClassSymbol(FTyp).IsAbstract then
    FProg.Msgs.AddCompilerError(FPos, RTE_InstanceOfAbstractClass);
end;

{ TMethodVirtualExpr }

constructor TMethodVirtualExpr.Create;
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  FMethName := Func.Name;
end;

function TMethodVirtualExpr.FindVirtualMethod(ClassSym: TClassSymbol): TMethodSymbol;
begin
  Result := TMethodSymbol(ClassSym.Members.FindSymbol(FMethName));
  Assert(Result <> nil);

  while not TMethodSymbol(Result).IsVirtual and TMethodSymbol(Result).IsOverlap do
    Result := TMethodSymbol(Result).ParentMeth;
end;

function TMethodVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  // Find virtual method
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
  Result := FindVirtualMethod(ScriptObj.ClassSym);
  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

function TMethodVirtualExpr.OptimizeExecutable: TExpr;
var
  SO : IScriptObj;
begin
  if BaseExpr is TConstExpr then begin
    SO := IScriptObj(IUnknown(BaseExpr.Eval));
    Result := TExpr(FindVirtualMethod(SO.ClassSym).Executable.Optimize(Self));
  end
  else
    Result := inherited OptimizeExecutable;
end;

{ TClassMethodVirtualExpr }

function TClassMethodVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
  Result := FindVirtualMethod(ScriptObj.ClassSym);
end;

{ TClassMethodVirtualNameExpr }

function TClassMethodVirtualNameExpr.OptimizeExecutable: TExpr;
var
  methSym : TMethodSymbol;
begin
  if BaseExpr is TConstExpr then begin
    methSym := FindVirtualMethod(FProg.Table.FindSymbol(BaseExpr.Eval) as TClassSymbol);
    Result := TExpr(methSym.Executable.Optimize(Self));
  end
  else
    Result := inherited OptimizeExecutable;
end;

function TClassMethodVirtualNameExpr.PreCall(
  var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FindVirtualMethod(FProg.Table.FindSymbol(FBaseExpr.Eval) as TClassSymbol);
end;

{ TClassMethodObjVirtualExpr }
function TClassMethodObjVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  ScriptObj := IScriptObj(IUnknown(FBaseExpr.Eval));
  Result := FindVirtualMethod(ScriptObj.ClassSym);
end;

{ TClassMethodObjVirtualNameExpr }

function TClassMethodObjVirtualNameExpr.OptimizeExecutable: TExpr;
var
  methSym : TMethodSymbol;
begin
  if BaseExpr is TConstExpr then begin
    methSym := FindVirtualMethod(FProg.Table.FindSymbol(BaseExpr.Eval) as TClassSymbol);
    Result := TExpr(methSym.Executable.Optimize(Self));
  end
  else
    Result := inherited OptimizeExecutable;
end;

function TClassMethodObjVirtualNameExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
begin
  Result := FindVirtualMethod(FProg.Table.FindSymbol(FBaseExpr.Eval) as TClassSymbol);
end;

{ TConstructorVirtualExpr }

constructor TConstructorVirtualExpr.Create(Prog: TProgram; Pos: TScriptPos; Func:
  TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean);
begin
  inherited Create(Prog, Pos, Func, Base, IsInstruction);
  FTyp := Base.Typ.Typ;
end;

function TConstructorVirtualExpr.PreCall(var ScriptObj: IScriptObj): TFuncSymbol;
var
  className: string;
  classSym: TClassSymbol;
begin
  // Get class symbol
  className := FBaseExpr.Eval;
  classSym := TClassSymbol(FProg.Table.FindSymbol(className));
  Assert(classSym <> nil);

  if classSym.IsAbstract then
    FProg.Msgs.AddExecutionStop(FPos, RTE_InstanceOfAbstractClass);

  Result := FindVirtualMethod(classSym);

  // Create object
  ScriptObj := TScriptObj.Create(classSym, FProg);
  ScriptObj.ExternalObject := ExternalObject;

  FProg.Stack.WriteValue(FProg.Stack.StackPointer + FSelfAddr, ScriptObj);
end;

function TConstructorVirtualExpr.PostCall(ScriptObj: IScriptObj): Variant;
begin
  // Return Self as Result
  Assert(FResultAddr = -1);
  Result := ScriptObj;
end;

{ TIfExpr }

destructor TIfExpr.Destroy;
begin
  FCond.Free;
  FThen.Free;
  FElse.Free;
  inherited;
end;

function TIfExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  if FCond.Eval then
    FThen.Eval
  else if Assigned(FElse) then
    FElse.Eval;
end;

procedure TIfExpr.Initialize;
begin
  FCond.Initialize;
  FThen.Initialize;
  if Assigned(FElse) then
    FElse.Initialize;
end;

function TIfExpr.Optimize: TExpr;
begin
  FCond := FCond.Optimize;
  if FCond is TConstExpr then begin
    if FCond.Eval then begin
      Result := FThen.Optimize;
      FThen := nil;
    end
    else begin
      if Assigned(FElse) then begin
        Result := FElse.Optimize;
        FElse := nil;
      end
      else
        Result := TNullExpr.Create(Prog,Pos);
    end;
    Free;
  end
  else begin
    Result := Self;
    FThen := FThen.Optimize;
    if Assigned(FElse) then
      FElse := FElse.Optimize;
  end;
end;

procedure TIfExpr.TypeCheck;
begin
  FCond.TypeCheck;
  if not IsBooleanType(FCond.Typ) then
    FProg.FMsgs.AddCompilerStop(FCond.Pos, CPE_BooleanExpected);
end;

{ TCaseExpr }

constructor TCaseExpr.Create(Prog: TProgram; Pos: TScriptPos);
begin
  inherited;
  FCaseConditions := TList.Create;
end;

destructor TCaseExpr.Destroy;
var
  x: Integer;
begin
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions[x]).Free;
  FCaseConditions.Free;
  FValueExpr.Free;
  FElseExpr.Free;
  inherited;
end;

function TCaseExpr.Eval: Variant;
var
  x: Integer;
  Value: Variant;
begin
  FProg.DoStep(Self);

  Value := FValueExpr.Eval;
  for x := 0 to FCaseConditions.Count - 1 do
    if TCaseCondition(FCaseConditions[x]).IsTrue(Value) then
      exit;

  if Assigned(FElseExpr) then
    FElseExpr.Eval;
end;

procedure TCaseExpr.Initialize;
var
  x: Integer;
begin
  FValueExpr.Initialize;
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions[x]).Initialize;
  if Assigned(FElseExpr) then
    FElseExpr.Initialize;
end;

function TCaseExpr.Optimize: TExpr;
var
  x: Integer;
begin
  Result := Self;
  FValueExpr := FValueExpr.Optimize;
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions[x]).Optimize;
  if Assigned(FElseExpr) then
    FElseExpr := FElseExpr.Optimize;
end;

procedure TCaseExpr.TypeCheck;
var
  x: Integer;
begin
  for x := 0 to FCaseConditions.Count - 1 do
    TCaseCondition(FCaseConditions[x]).TypeCheck(FValueExpr.Typ);
end;

{ TCaseCondition }

constructor TCaseCondition.Create(ValueExpr: TExpr);
begin
  FValueExpr := ValueExpr;
end;

destructor TCaseCondition.Destroy;
begin
  if FOwnsTrueExpr then
    FTrueExpr.Free;
  inherited;
end;

procedure TCaseCondition.Initialize;
begin
  FTrueExpr.Initialize;
end;

procedure TCaseCondition.Optimize;
begin
  FTrueExpr := FTrueExpr.Optimize;
end;

{ TCompareCaseCondition }

constructor TCompareCaseCondition.Create;
begin
  inherited Create(ValueExpr);
  FCompareExpr := CompareExpr;
end;

destructor TCompareCaseCondition.Destroy;
begin
  FCompareExpr.Free;
  inherited;
end;

procedure TCompareCaseCondition.Initialize;
begin
  inherited;
  FCompareExpr.Initialize;
end;

function TCompareCaseCondition.IsTrue(Value: Variant): Boolean;
begin
  Result := FCompareExpr.Eval = Value;
  if Result then
    FTrueExpr.Eval;
end;

procedure TCompareCaseCondition.Optimize;
begin
  inherited;
  FCompareExpr := FCompareExpr.Optimize;
end;

procedure TCompareCaseCondition.TypeCheck(Typ: TSymbol);
begin
  if FTrueExpr.IsFloatType(FValueExpr.Typ) then
    if FTrueExpr.IsIntegerType(FCompareExpr.Typ) then
      FCompareExpr := TConvFloatExpr.Create(FValueExpr.Prog, FCompareExpr.Pos,
        FCompareExpr);

  if not FCompareExpr.Typ.IsCompatible(FValueExpr.Typ) then
    FCompareExpr.Prog.Msgs.AddCompilerError(FCompareExpr.Pos,
      Format(CPE_IncompatibleTypes, [FValueExpr.Typ.Caption,
      FCompareExpr.Typ.Caption]));
end;

{ TRangeCaseCondition }

constructor TRangeCaseCondition.Create;
begin
  inherited Create(ValueExpr);
  FFromExpr := FromExpr;
  FToExpr := ToExpr;
end;

destructor TRangeCaseCondition.Destroy;
begin
  FFromExpr.Free;
  FToExpr.Free;
  inherited;
end;

procedure TRangeCaseCondition.Initialize;
begin
  inherited;
  FFromExpr.Initialize;
  FToExpr.Initialize;
end;

function TRangeCaseCondition.IsTrue(Value: Variant): Boolean;
begin
  Result := (Value >= FFromExpr.Eval) and (Value <= FToExpr.Eval);
  if Result then
    FTrueExpr.Eval;
end;

procedure TRangeCaseCondition.Optimize;
begin
  inherited;
  FFromExpr := FFromExpr.Optimize;
  FToExpr := FToExpr.Optimize;
end;

procedure TRangeCaseCondition.TypeCheck(Typ: TSymbol);
var
  Prog: TProgram;
begin
  Prog := FValueExpr.Prog;

  if FTrueExpr.IsFloatType(FValueExpr.Typ) then
  begin
    // Convert integers to float if necessary
    if FTrueExpr.IsIntegerType(FFromExpr.Typ) then
      FFromExpr := TConvFloatExpr.Create(Prog, FFromExpr.Pos, FFromExpr);

    if FTrueExpr.IsIntegerType(FToExpr.Typ) then
      FToExpr := TConvFloatExpr.Create(Prog, FToExpr.Pos, FToExpr);
  end;

  if not FFromExpr.Typ.IsCompatible(FToExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerError(FFromExpr.Pos,
      Format(CPE_RangeIncompatibleTypes, [FFromExpr.Typ.Caption,
      FToExpr.Typ.Caption]));

  if not FValueExpr.Typ.IsCompatible(FFromExpr.Typ) then
    FFromExpr.Prog.Msgs.AddCompilerError(FFromExpr.Pos,
      Format(CPE_IncompatibleTypes, [FValueExpr.Typ.Caption,
      FFromExpr.Typ.Caption]));
end;

{ TForExpr }

destructor TForExpr.Destroy;
begin
  FDoExpr.Free;
  FFromExpr.Free;
  FToExpr.Free;
  FVarExpr.Free;
  inherited;
end;

function TForExpr.Eval: Variant;
var
  x: Integer;
  fromValue, toValue: Integer;
begin
  fromValue := FFromExpr.Eval;
  toValue := FToExpr.Eval;
  if FIsUpWard then
  begin
    if fromValue <= toValue then
    begin
      // Set start value for loop variable
      x := fromValue;
      FVarExpr.AssignValue(x);
      while (x <= toValue) do
      begin
        FProg.DoStep(Self);
        try
          // Loop expression
          FDoExpr.Eval;
        except
          on e: EBreak do
            break;
          on e: EContinue do
            ;
        else
          raise;
        end;
        // Increment loop variable
        Inc(x);
        FVarExpr.AssignValue(x);
      end; // while
    end
  end
  else
  begin
    if fromValue >= toValue then
    begin
      // Set start value for loop variable
      x := fromValue;
      FVarExpr.AssignValue(x);
      while (x >= toValue) do
      begin
        FProg.DoStep(Self);
        try
          // Loop expression
          FDoExpr.Eval;
        except
          on e: EBreak do
            break;
          on e: EContinue do
            ;
        else
          raise;
        end;
        // Decrement loop variable
        Dec(x);
        FVarExpr.AssignValue(x);
      end; // while
    end
  end;
end;

procedure TForExpr.Initialize;
begin
  inherited;
  FFromExpr.Initialize;
  FToExpr.Initialize;
  FDoExpr.Initialize;
end;

function TForExpr.Optimize: TExpr;
begin
  FFromExpr := FFromExpr.Optimize;
  FToExpr := FToExpr.Optimize;
  FDoExpr := FDoExpr.Optimize;
  Result := Self;
end;

procedure TForExpr.TypeCheck;
begin
  FFromExpr.TypeCheck;
  if not IsIntegerType(FFromExpr.Typ) then
    FProg.FMsgs.AddCompilerStop(FFromExpr.Pos, CPE_IntegerExpected);
  FToExpr.TypeCheck;
  if not IsIntegerType(FToExpr.Typ) then
    FProg.FMsgs.AddCompilerStop(FToExpr.Pos, CPE_IntegerExpected);
end;

{ TLoopExpr }

destructor TLoopExpr.Destroy;
begin
  FCondExpr.Free;
  FLoopExpr.Free;
  inherited;
end;

procedure TLoopExpr.Initialize;
begin
  FCondExpr.Initialize;
  FLoopExpr.Initialize;
end;

function TLoopExpr.Optimize: TExpr;
begin
  FCondExpr := FCondExpr.Optimize;
  FLoopExpr := FLoopExpr.Optimize;
  Result := inherited Optimize;
end;

procedure TLoopExpr.TypeCheck;
begin
  FCondExpr.TypeCheck;
  if not IsBooleanType(FCondExpr.Typ) then
    FProg.FMsgs.AddCompilerStop(FCondExpr.Pos, CPE_BooleanExpected);
end;

{ TWhileExpr }

function TWhileExpr.Eval: Variant;
begin
  while FCondExpr.Eval do
  try
    FProg.DoStep(Self);
    FLoopExpr.Eval;
  except
    on e: EBreak do
      break;
    on e: EContinue do
      ;
  else
    raise;
  end;
end;

{ TRepeatExpr }

function TRepeatExpr.Eval: Variant;
begin
  repeat
    try
      FProg.DoStep(Self);
      FLoopExpr.Eval;
    except
      on e: EBreak do
        break;
      on e: EContinue do
        ;
    else
      raise;
    end;
  until FCondExpr.Eval;
end;

{ TBreakExpr }

function TBreakExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  raise EBreak.Create('');
end;

{ TExitExpr }

function TExitExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  raise EExit.Create('');
end;

{ TContinueExpr }

function TContinueExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  raise EContinue.Create('');
end;

{ TNegExpr }

function TNegExpr.Eval: Variant;
begin
  Result := -FExpr.Eval;
end;

procedure TNegExpr.TypeCheck;
begin
  FExpr.TypeCheck;
  if IsIntegerType(FExpr.Typ) then
    FTyp := FProg.TypInteger
  else if IsFloatType(FExpr.Typ) then
    FTyp := FProg.TypFloat
  else if IsVariantType(FExpr.Typ) then
    FTyp := FProg.TypVariant
  else
    FProg.FMsgs.AddCompilerStop(FPos, CPE_NumericalExpected);
end;

{ TAddExpr }

function TAddExpr.Eval: Variant;
begin
  Result := FLeft.Eval + FRight.Eval;
end;

{ TSubExpr }

function TSubExpr.Eval: Variant;
begin
  Result := FLeft.Eval - FRight.Eval;
end;

{ TMultExpr }

function TMultExpr.Eval: Variant;
begin
  Result := FLeft.Eval * FRight.Eval;
end;

{ TDivideExpr }

function TDivideExpr.Eval: Variant;
begin
  Result := FLeft.Eval / FRight.Eval;
end;

{ TDivExpr }

function TDivExpr.Eval: Variant;
begin
  Result := FLeft.Eval div FRight.Eval;
end;

{ TModExpr }

function TModExpr.Eval: Variant;
begin
  Result := FLeft.Eval mod FRight.Eval;
end;

{ TNotExpr }

function TNotExpr.Eval: Variant;
begin
  Result := not FExpr.Eval;
end;

procedure TNotExpr.TypeCheck;
begin
  FExpr.TypeCheck;
  if IsVariantType(FExpr.Typ) then
    FTyp := FProg.TypVariant
  else if IsBooleanType(FExpr.Typ) then
    FTyp := FProg.TypBoolean
  else if IsIntegerType(FExpr.Typ) then
    FTyp := FProg.TypInteger
  else
    FProg.FMsgs.AddCompilerStop(FPos, CPE_BooleanOrIntegerExpected);
end;

{ TAndExpr }

function TAndExpr.Eval: Variant;
begin
  if FMode = omInteger then
    Result := Integer(FLeft.Eval) and Integer(FRight.Eval)
  else
    Result := Boolean(FLeft.Eval) and Boolean(FRight.Eval);
end;

{ TOrExpr }

function TOrExpr.Eval: Variant;
begin
  if FMode = omInteger then
    Result := Integer(FLeft.Eval) or Integer(FRight.Eval)
  else
    Result := Boolean(FLeft.Eval) or Boolean(FRight.Eval);
end;

{ TXorExpr }

function TXorExpr.Eval: Variant;
begin
  if FMode = omInteger then
    Result := Integer(FLeft.Eval) xor Integer(FRight.Eval)
  else
    Result := Boolean(FLeft.Eval) xor Boolean(FRight.Eval);
end;

{ TFieldExpr }

constructor TFieldExpr.Create(Prog: TProgram; Pos: TScriptPos; Typ: TSymbol;
  FieldSym: TFieldSymbol; ObjExpr: TDataExpr);
begin
  inherited Create(Prog, Pos, Typ);
  FObjectExpr := ObjExpr;
  FFieldAddr := FieldSym.Offset;
end;

destructor TFieldExpr.Destroy;
begin
  FObjectExpr.Free;
  inherited;
end;

function TFieldExpr.GetAddr: Integer;
begin
  Result := FFieldAddr;
end;

function TFieldExpr.GetData: TData;
begin
  Result := IScriptObj(IUnknown(FObjectExpr.Eval)).Data;
end;

procedure TFieldExpr.Initialize;
begin
  FObjectExpr.Initialize;
end;

{ TExceptionExpr }

destructor TExceptionExpr.Destroy;
begin
  FTryExpr.Free;
  FHandlerExpr.Free;
  inherited;
end;

procedure TExceptionExpr.Initialize;
begin
  FTryExpr.Initialize;
  if Assigned(FHandlerExpr) then
    FHandlerExpr.Initialize;
end;

function TExceptionExpr.Optimize: TExpr;
begin
  FTryExpr := FTryExpr.Optimize;
  if Assigned(FHandlerExpr) then
    FHandlerExpr := FHandlerExpr.Optimize;
  Result := inherited Optimize;
end;

{ TExceptExpr }

constructor TExceptExpr.Create(Prog: TProgram; Pos: TScriptPos);
begin
  inherited;
  FDoExprs := TExprList.Create;
end;

destructor TExceptExpr.Destroy;
begin
  inherited;
  FDoExprs.Free;
  FElseExpr.Free;
end;

function TExceptExpr.Eval: Variant;
var
  x: Integer;
  obj: Variant;
  objSym: TSymbol;
  doExpr: TExceptDoExpr;
  isCatched: Boolean;
  isReraise: Boolean;
begin
  try
    FTryExpr.Eval;
  except
    on e: EFlowControl do
      // exit, break or continue
      raise;
    on e: Exception do
    begin
      if e is EScriptException then
      begin
        // a raise-statement created an Exception object
        obj := EScriptException(e).Value;
        objSym := EScriptException(e).Typ;
      end
      else
      begin
        // A Delphi exception. Transform it to a EDelphi-dws exception
        obj := CreateEDelphiObj(e.ClassName, e.Message);
        objSym := IScriptObj(IUnknown(Obj)).ClassSym;
      end;

      // script exceptions
      if FDoExprs.Count > 0 then
      begin

        isCatched := False;

        for x := 0 to FDoExprs.Count - 1 do
        begin
          // Find a "on x: Class do ..." statement matching to this exception class
          doExpr := TExceptDoExpr(FDoExprs[x]);
          if doExpr.ExceptionVar.Typ.IsCompatible(objSym) then
          begin
            FProg.Stack.Data[FProg.Stack.BasePointer +
              doExpr.FExceptionVar.StackAddr] := obj;

            isReraise := False;
            try
              doExpr.Eval;
            except
              on E : EReraise do
                isReraise := True;
            end;

            if isReraise then
              raise;

            VarClear(FProg.Stack.Data[FProg.Stack.BasePointer +
              doExpr.FExceptionVar.StackAddr]);

            isCatched := True;
            Break;
          end;
        end;

        if not isCatched and Assigned(FElseExpr) then
        begin
          isReraise := False;
          try
            FElseExpr.Eval;
          except
            on E : EReraise do
              isReraise := True;
          end;
          if isReraise then
            raise;
        end;
      end
      else
      begin
        isReraise := False;
        try
          FHandlerExpr.Eval;
        except
          on E : EReraise do
            isReraise := True;
        end;
        if isReraise then
          raise;
      end;
    end;
  end;
  FProg.Msgs.SetScriptError(NullPos);
end;

procedure TExceptExpr.Initialize;
begin
  inherited;
  FDoExprs.Initialize;
  if Assigned(FElseExpr) then
    FElseExpr.Initialize;
end;

function TExceptExpr.Optimize: TExpr;
begin
  FDoExprs.Optimize;
  if Assigned(FElseExpr) then
    FElseExpr := FElseExpr.Optimize;
  Result := inherited Optimize;
end;

{ TFinallyExpr }

function TFinallyExpr.Eval: Variant;
begin
  try
    FTryExpr.Eval;
  finally
    FHandlerExpr.Eval;
  end;
end;

{ TRaiseExpr }

constructor TRaiseExpr.Create;
begin
  inherited Create(Prog, Pos);
  FExceptionExpr := ExceptionExpr;
end;

destructor TRaiseExpr.Destroy;
begin
  FExceptionExpr.Free;
  inherited;
end;

function TRaiseExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  raise EScriptException.Create(RTE_UserDefinedException,
    FExceptionExpr.Eval, FExceptionExpr.Typ, FPos);
end;

procedure TRaiseExpr.Initialize;
begin
  FExceptionExpr.Initialize;
end;

function TRaiseExpr.Optimize: TExpr;
begin
  FExceptionExpr := FExceptionExpr.Optimize;
  Result := inherited Optimize;
end;

procedure TRaiseExpr.TypeCheck;
begin
  FExceptionExpr.TypeCheck;
end;

{ TReraiseExpr }

function TReraiseExpr.Eval: Variant;
begin
  FProg.DoStep(Self);
  raise EReraise.Create('');
end;

{ TExceptDoExpr }

destructor TExceptDoExpr.Destroy;
begin
  FDoBlockExpr.Free;
  FExceptionVar.Free;
  inherited;
end;

function TExceptDoExpr.Eval: Variant;
begin
  Result := DoBlockExpr.Eval;
end;

procedure TExceptDoExpr.Initialize;
begin
  FDoBlockExpr.Initialize;
  FExceptionVar.Initialize;
end;

function TExceptDoExpr.Optimize: TExpr;
begin
  FDoBlockExpr := FDoBlockExpr.Optimize;
  Result := inherited Optimize;
end;

{ TStringArraySetExpr }

constructor TStringArraySetExpr.Create(Prog: TProgram; Pos: TScriptPos;
  StringExpr, IndexExpr, ValueExpr: TExpr);
begin
  inherited Create(Prog,Pos);
  FStringExpr := StringExpr;
  FIndexExpr := IndexExpr;
  FValueExpr := ValueExpr;
end;

destructor TStringArraySetExpr.Destroy;
begin
  FStringExpr.Free;
  FIndexExpr.Free;
  FValueExpr.Free;
  inherited;
end;

function TStringArraySetExpr.Eval: Variant;
var
  s: string;
begin
  s := FStringExpr.Eval;
  s[Integer(FIndexExpr.Eval)] := string(FValueExpr.Eval)[1];
  TDataExpr(FStringExpr).AssignValue(s);
end;

procedure TStringArraySetExpr.Initialize;
begin
  FStringExpr.Initialize;
  FIndexExpr.Initialize;
  FValueExpr.Initialize;
end;

function TStringArraySetExpr.Optimize: TExpr;
begin
  FStringExpr.Optimize;
  FIndexExpr.Optimize;
  FValueExpr.Optimize;
  Result := Self;
end;

{ TIsOpExpr }

constructor TIsOpExpr.Create(Prog: TProgram; Pos: TScriptPos; Left,
  Right: TExpr);
begin
  inherited;
  FTyp := Prog.FTypBoolean;
end;

function TIsOpExpr.Eval: Variant;
var
  scriptObj: IScriptObj;
begin
  scriptObj := IScriptObj(IUnknown(FLeft.Eval));
  if Assigned(scriptObj) then
    Result := FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym)
  else
    Result := False;
end;

procedure TIsOpExpr.TypeCheck;
begin
  FLeft.TypeCheck;
  FRight.TypeCheck;
  if not (FLeft.Typ is TClassSymbol) then
    FProg.Msgs.AddCompilerStop(FLeft.Pos, CPE_ObjectExpected);
  if not (FRight.Typ is TClassOfSymbol) then
    FProg.Msgs.AddCompilerStop(FRight.Pos, CPE_ClassRefExpected);
end;

{ TAsOpExpr }

function TAsOpExpr.Eval: Variant;
var
  scriptObj: IScriptObj;
begin
  Result := FLeft.Eval;
  scriptObj := IScriptObj(IUnknown(Result));

  if Assigned(scriptObj) and
    not (FRight.Typ.Typ.IsCompatible(scriptObj.ClassSym)) then
    FProg.Msgs.AddExecutionStop(FPos, Format(RTE_ClassCastFailed,
      [scriptObj.ClassSym.Caption, FRight.Typ.Typ.Caption]));
end;

procedure TAsOpExpr.TypeCheck;
begin
  FLeft.TypeCheck;
  FRight.TypeCheck;
  if not (FLeft.Typ is TClassSymbol) then
    FProg.Msgs.AddCompilerStop(FLeft.Pos, CPE_ObjectExpected);
  if not (FRight.Typ is TClassOfSymbol) then
    FProg.Msgs.AddCompilerStop(FRight.Pos, CPE_ClassRefExpected);
  FTyp := FRight.Typ.Typ;
end;

{ TConvFloatExpr }

constructor TConvFloatExpr.Create(Prog: TProgram; Pos: TScriptPos; Expr: TExpr);
begin
  inherited;
  FTyp := Prog.FTypFloat;
end;

function TConvFloatExpr.Eval: Variant;
begin
  VarCast(Result, FExpr.Eval, varDouble);
end;

procedure TConvFloatExpr.TypeCheck;
begin
  inherited;
  if not IsIntegerType(FExpr.Typ) then
    FProg.Msgs.AddCompilerError(FPos, CPE_IntegerExpected);
end;

{ TConvDateTimeExpr }

constructor TConvDateTimeExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Expr: TExpr);
begin
  inherited;
  FTyp := Prog.FTypDateTime;
end;

function TConvDateTimeExpr.Eval: Variant;
begin
  VarCast(Result, FExpr.Eval, varDate);
end;

procedure TConvDateTimeExpr.TypeCheck;
begin
  inherited;
  if not IsFloatType(FExpr.Typ) then
    FProg.Msgs.AddCompilerError(FPos, CPE_FloatExpected);
end;

{ TConvIntegerExpr }

constructor TConvIntegerExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Expr: TExpr);
begin
  inherited;
  FTyp := Prog.FTypInteger;
end;

function TConvIntegerExpr.Eval: Variant;
begin
  VarCast(Result, FExpr.Eval, varInteger);
end;

procedure TConvIntegerExpr.TypeCheck;
begin
  inherited;
  if not (IsIntegerType(FExpr.Typ) or IsFloatType(FExpr.Typ) or IsBooleanType(FExpr.Typ)
  or (FExpr.Typ is TEnumerationSymbol)) then
    FProg.Msgs.AddCompilerError(FPos, CPE_IntegerCastInvalid);
end;

{ TConvVariantExpr }

constructor TConvVariantExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Expr: TExpr);
begin
  inherited;
  FTyp := FProg.TypInteger;
end;

function TConvVariantExpr.Eval: Variant;
begin
  Result := FExpr.Eval;
end;

procedure TConvVariantExpr.TypeCheck;
begin
  inherited;
  if not IsVariantType(FExpr.Typ) then
    FProg.Msgs.AddCompilerError(FPos, CPE_VariantExpected);
end;

{ TProgramInfo }

constructor TProgramInfo.Create(Table: TSymbolTable; Caller: TProgram);
begin
  FTable := Table;
  FCaller := Caller;
end;

function TProgramInfo.GetValue(s: string): Variant;
begin
  Result := GetVars(s).Value;
end;

function TProgramInfo.GetData(s: string): TData;
begin
  Result := GetVars(s).Data;
end;

procedure TProgramInfo.SetValue(s: string; const Value: Variant);
begin
  GetVars(s).Value := Value;
end;

procedure TProgramInfo.SetData(s: string; const Value: TData);
begin
  GetVars(s).Data := Value;
end;

function TProgramInfo.GetVars(s: string): IInfo;
var
  sym: TSymbol;
  dat: TData;
  basePointer: Integer;
  vpd: IVarParamData;
  extVDM: TExternalVarDataMaster;
begin
  sym := FTable.FindSymbol(s);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_VariableNotFound, [s]);

  if sym is TDataSymbol then
  begin
    if TDataSymbol(sym).Level = FLevel then
      basePointer := FCaller.Stack.BasePointer
    else
      basePointer := FCaller.Stack.GetSavedBp(FCaller.Level);

    if sym is TVarParamSymbol then
    begin
      // Local or global variable
      vpd := IVarParamData(IUnknown(FCaller.Stack.Data[basePointer + TDataSymbol(sym).StackAddr]));
      Result := TInfo.SetChild(Self, sym.Typ, vpd.Data, vpd.Addr);
    end
    else
      // Local or global variable
      Result := TInfo.SetChild(Self, sym.Typ, FCaller.Stack.Data,
        basePointer + TDataSymbol(sym).StackAddr);
  end
  else if sym is TConstSymbol then
    Result := TInfo.SetChild(Self, sym.Typ, TConstSymbol(sym).Data, 0)
  else if sym is TFieldSymbol then
    // Field of the Self object
    Result := TInfo.SetChild(Self, sym.Typ, FScriptObj.Data, TFieldSymbol(sym).Offset)
  else if sym is TExternalVarSymbol then
  begin
    dat := nil;
    SetLength(dat, sym.Typ.Size);
    extVDM := TExternalVarDataMaster.Create(FCaller, TExternalVarSymbol(sym));
    if sym.Typ is TClassSymbol then
      extVDM.Read(dat); // initialize 'Self'-Object
    Result := TInfo.SetChild(Self, sym.Typ, dat, 0, extVDM);
  end
  else if sym is TTypeSymbol then
  begin
    if sym.BaseType is TClassSymbol then
    begin
      SetLength(dat, 1);
      VarClear(dat[0]);
      Result := TInfoClassObj.Create(Self, sym, dat, 0)
    end;
  end
  else
    raise Exception.CreateFmt(RTE_OnlyVarSymbols, [sym.Caption]);
end;

function TProgramInfo.GetFunc(s: string): IInfo;
var
  sym: TSymbol;
begin
  sym := FTable.FindSymbol(s);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_FunctionNotFound, [s]);

  if sym is TFuncSymbol then
  begin
    if Assigned(FScriptObj) then
      Result := TInfoFunc.Create(Self, sym, nil, 0, nil, FScriptObj, FScriptObj.ClassSym)
    else
      Result := TInfoFunc.Create(Self, sym, nil, 0, nil, nil, nil)
  end
  else
    raise Exception.CreateFmt(RTE_OnlyFuncSymbols, [sym.Caption]);
end;

function TProgramInfo.GetTemp(DataType: string): IInfo;
var
  data: TData;
  typSym: TSymbol;
begin
  typSym := FTable.FindSymbol(DataType);

  if not Assigned(typSym) then
    raise Exception.CreateFmt(RTE_DatatypeNotFound, [DataType]);

  data := nil;
  SetLength(data, typSym.Size);
  typSym.InitData(data, 0);

  Result := TInfo.SetChild(Self, typSym, data, 0);
end;

procedure TProgramInfo.SetFuncSym(const Value: TFuncSymbol);
begin
  FFuncSym := Value;
  if Assigned(FFuncSym) then
    FLevel := 1
  else
    FLevel := 0;
end;

procedure TProgramInfo.SetResult(const Value: Variant);
begin
  GetVars(SYS_RESULT).Value := Value;
end;

function TProgramInfo.GetResult: Variant;
begin
  Result := GetVars(SYS_RESULT).Value;
end;

{ TScriptObjectWrapper }

// wrapper to interact with an released script object
type TScriptObjectWrapper = class (TInterfacedObject, IScriptObj)
  private
    FScriptObj : TScriptObj;
  protected
    { IScriptObj }
    function GetClassSym: TClassSymbol;
    function GetData: TData;
    procedure SetData(Dat: TData);
    function GetExternalObject: TObject;
    procedure SetExternalObject(Value: TObject);
  public
    constructor Create(ScriptObj : TScriptObj);
end;

constructor TScriptObjectWrapper.Create(ScriptObj: TScriptObj);
begin
  inherited Create;
  FScriptObj := ScriptObj;
end;

function TScriptObjectWrapper.GetClassSym: TClassSymbol;
begin
  Result := FScriptObj.FClassSym;
end;

function TScriptObjectWrapper.GetData: TData;
begin
  Result := FScriptObj.GetData;
end;

function TScriptObjectWrapper.GetExternalObject: TObject;
begin
  Result := FScriptObj.GetExternalObject;
end;

procedure TScriptObjectWrapper.SetData(Dat: TData);
begin
  FScriptObj.SetData(Dat);
end;

procedure TScriptObjectWrapper.SetExternalObject(Value: TObject);
begin
  FScriptObj.SetExternalObject(Value);
end;

function TProgramInfo.FindClassMatch(AObject: TObject; ExactMatch: Boolean): TClassSymbol;
var
  ParentRTTI: PPTypeInfo;
  unitList: TList;      // build the list once, may search for many symbols
  typeSym: TSymbol;
begin
  Result := nil;
  if AObject = nil then
    Exit;

  { Cycle the AObject class hierarchy and determine the DWS class type that most
    closely matches the AObject type. Return the IInfo for that matching
    class. It should *always* at least match at TObject. }
  unitList := CreateUnitList;
  try
    // Check current class type. If not found cycle object ancestry
    typeSym := FindSymbolInUnits(unitList, AObject.ClassName);
    // If no exact match found then look for supported ancestors
    if Assigned(typeSym) and (typeSym is TClassSymbol) then
      Result := TClassSymbol(typeSym);

    // Allowed to look through ancestor types
    if not ExactMatch then
    begin
      if AObject.ClassInfo <> nil then
      begin
        ParentRTTI := GetTypeData(AObject.ClassInfo).ParentInfo;
        repeat
          typeSym := FindSymbolInUnits(unitList, ParentRTTI^.Name);
          if Assigned(typeSym) and (typeSym is TClassSymbol) then       // match found, stop searching
          begin
            Result := TClassSymbol(typeSym);
            Break;
          end
          else                               // if no match found yet, try higher ancestor
            ParentRTTI := GetTypeData(ParentRTTI^)^.ParentInfo;
        until ParentRTTI = nil;
      end;{if Assigned}
    end;{if not ExactMatch}
    // If no matches found, error
    if Result = nil then
      raise Exception.CreateFmt(RTE_ClassMatchNotFound, [AObject.ClassName]);
  finally
    unitList.Free;
  end;
end;

function TProgramInfo.RegisterExternalObject(AObject: TObject; AutoFree: Boolean; ExactClassMatch: Boolean): Variant;
var
  NewScriptObj: IScriptObj;
  ClassSym: TClassSymbol;
  prg: TProgram;
begin
  Assert(Assigned(FCaller));
  { This will register an external object (known or not known to the system)
    with the DWS system. If an object that is already registered is passed in
    it will NOT point to the same script object. Currently it is too difficult
    (if not impossible) to obtain the IScriptObj for an existing
    registered external object. This is very useful for registering a new object
    like a TField (ie TDataset.FieldByName()) so the script can refer to the
    object and act on it. }
  ClassSym := FindClassMatch(AObject, ExactClassMatch) as TClassSymbol;
  if Assigned(ClassSym) and Assigned(AObject) then
  begin
    if AutoFree then
      prg := FCaller
    else
      prg := nil;
    NewScriptObj := TScriptObj.Create(ClassSym, prg);
    NewScriptObj.ExternalObject := AObject;
    Result := IScriptObj(NewScriptObj);
  end
  else                                     // no class returned or no object provided
    Result := Unassigned;                  // return 'nil' Id
end;

function TProgramInfo.GetExternalObjForVar(s: string): TObject;
var
  sObj: IScriptObj;
begin
  sObj := Vars[s].ScriptObj;
  if Assigned(sObj) then
    Result := sObj.ExternalObject
  else
    Result := nil;
end;

function TProgramInfo.FindSymbolInUnits(AUnitList: TList; const Name: string): TSymbol;
var
  i: Integer;
begin
  // Search all units for the symbol
  Result := nil;
  for i := 0 to AUnitList.Count - 1 do
  begin
    Result := TUnitSymbol(AUnitList[i]).Table.FindLocal(Name);
    if Assigned(Result) then
      Break;
  end;
end;

function TProgramInfo.FindSymbolInUnits(const Name: string): TSymbol;
var
  list: TList;
begin
  list := CreateUnitList;
  try
    Result := FindSymbolInUnits(list, Name);
  finally
    list.Free;
  end;
end;

function TProgramInfo.CreateUnitList: TList;
var
  root: TSymbolTable;
  i: Integer;
begin
  // Find the root table for the full compiled program (not just the function)
  if Assigned(FCaller) then
    root := FCaller.RootTable
  else
  // if no caller provided, make a 'best effort' to find a root.
  begin
    root := FTable;
    while root.ParentCount > 0 do
      root := root.Parents[0];
  end;

  Result := TList.Create;                         // caller reponsible for freeing
  // Add all unit symbols to a list
  for i := 0 to root.Count - 1 do
    if root.Symbols[i] is TUnitSymbol then        // if a unit symbol
      if Result.IndexOf(root.Symbols[i]) < 0 then // and not already in list (units may reuse others)
        Result.Add(root.Symbols[i]);
end;

{ TScriptObj }

constructor TScriptObj.Create(ClassSym: TClassSymbol; Prog: TProgram);
var
  x: Integer;
  c: TClassSymbol;
begin
  FClassSym := ClassSym;
  FProg := Prog;
  SetLength(FData, ClassSym.InstanceSize);

  // Initialize fields
  c := TClassSymbol(ClassSym);
  while c <> nil do
  begin
    for x := 0 to c.Members.Count - 1 do
      if c.Members[x] is TFieldSymbol then
        with TFieldSymbol(c.Members[x]) do
          Typ.InitData(FData, Offset);
    c := c.Parent;
  end;

  FOnObjectDestroy := ClassSym.OnObjectDestroy;
end;

procedure TScriptObj.BeforeDestruction;
var SO : IScriptObj;
begin
  // we are released, so never do: Self as IScriptObj
  SO := TScriptObjectWrapper.Create(Self);
  if Assigned(FProg) then
    FProg.DestroyScriptObj(SO);
  inherited;
end;

destructor TScriptObj.Destroy;
begin
  if Assigned(FOnObjectDestroy) then
    FOnObjectDestroy(FExternalObj);
  inherited;
end;

function TScriptObj.GetClassSym: TClassSymbol;
begin
  Result := FClassSym;
end;

function TScriptObj.GetData: TData;
begin
  Result := FData;
end;

function TScriptObj.GetExternalObject: TObject;
begin
  Result := FExternalObj;
end;

procedure TScriptObj.SetData(Dat: TData);
begin
  FData := Dat;
end;

procedure TScriptObj.SetExternalObject(Value: TObject);
begin
  FExternalObj := Value;
end;

{ TInfo }

function TInfo.Call(const Params: array of Variant): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

function TInfo.Call: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Call', FTypeSym.Caption]);
end;

constructor TInfo.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; DataMaster: IDataMaster = nil);
begin
  FProgramInfo := ProgramInfo;
  if Assigned(ProgramInfo) then
    FCaller := ProgramInfo.Caller;
  FTypeSym := TypeSym;
  FData := Data;
  FOffset := Offset;
  FDataMaster := DataMaster;
end;

function TInfo.Element(const Indices: array of Integer): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Element', FTypeSym.Caption]);
end;

function TInfo.GetConstructor(MethName: string; ExtObject: TObject): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetConstructor', FTypeSym.Caption]);
end;

function TInfo.GetData: TData;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Data', FTypeSym.Caption]);
end;

function TInfo.GetExternalObject: TObject;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['ExternalObject', FTypeSym.Caption]);
end;

function TInfo.GetMember(s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Member', FTypeSym.Caption]);
end;

function TInfo.GetMethod(s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Method', FTypeSym.Caption]);
end;

function TInfo.GetScriptObj: IScriptObj;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Obj', FTypeSym.Caption]);
end;

function TInfo.GetParameter(s: string): IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Parameter', FTypeSym.Caption]);
end;

function TInfo.GetTypeSym: TSymbol;
begin
  Result := FTypeSym;
end;

function TInfo.GetValue: Variant;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Value', FTypeSym.Caption]);
end;

class function TInfo.SetChild(ProgramInfo: TProgramInfo;
  ChildTypeSym: TSymbol; ChildData: TData; ChildOffset: Integer;
  ChildDataMaster: IDataMaster): IInfo;
var BaseType : TTypeSymbol;
begin
  Assert(Assigned(ChildTypeSym));
  BaseType := ChildTypeSym.BaseType;

  if BaseType is TBaseSymbol then // most used first!
    Result := TInfoData.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
      ChildDataMaster)
  else if BaseType is TEnumerationSymbol then
    Result := TInfoData.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
      ChildDataMaster)
  else if BaseType is TConnectorSymbol then
    Result := TInfoData.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
      ChildDataMaster)
  else if ChildTypeSym is TFuncSymbol then
    Result := TInfoFunc.Create(ProgramInfo, ChildTypeSym, ChildData, ChildOffset,
      ChildDataMaster, nil, nil)
  else if BaseType is TRecordSymbol then
    Result := TInfoRecord.Create(ProgramInfo, ChildTypeSym, ChildData,
      ChildOffset, ChildDataMaster)
  else if BaseType is TStaticArraySymbol then
    Result := TInfoStaticArray.Create(ProgramInfo, ChildTypeSym, ChildData,
      ChildOffset, ChildDataMaster)
  else if BaseType is TDynamicArraySymbol then
    Result := TInfoDynamicArray.Create(ProgramInfo, ChildTypeSym, ChildData,
      ChildOffset, ChildDataMaster)
  else if BaseType is TClassSymbol then
    Result := TInfoClassObj.Create(ProgramInfo, ChildTypeSym, ChildData,
      ChildOffset, ChildDataMaster)
  else if BaseType is TClassOfSymbol then
    Result := TInfoClassOf.Create(ProgramInfo, ChildTypeSym, ChildData,
      ChildOffset, ChildDataMaster)
  else
    Assert(False); // Shouldn't be ever executed
end;

procedure TInfo.SetData(const Value: TData);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Data', FTypeSym.Caption]);
end;

procedure TInfo.SetExternalObject(ExtObject: TObject);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['ExternalObject', FTypeSym.Caption]);
end;

procedure TInfo.SetValue(const Value: Variant);
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['Value', FTypeSym.Caption]);
end;

function TInfo.GetInherited: IInfo;
begin
  raise Exception.CreateFmt(RTE_InvalidOp, ['GetInherited', FTypeSym.Caption]);
end;

{ TInfoData }

function TInfoData.GetData;
begin
  if Assigned(FDataMaster) then
    FDataMaster.Read(FData);

  SetLength(Result, FTypeSym.Size);
  CopyData(FData, FOffset, Result, 0, FTypeSym.Size);
end;

function TInfoData.GetValue: Variant;
begin
  if Assigned(FDataMaster) then
    FDataMaster.Read(FData);

  if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
    Result := FData[FOffset]
  else
    raise Exception.CreateFmt(RTE_CanNotReadComplexType, [FTypeSym.Caption]);
end;

procedure TInfoData.SetData(const Value: TData);
begin
  if Length(Value) <> FTypeSym.Size then
    raise Exception.CreateFmt(RTE_InvalidInputDataSize, [Length(Value),
      FTypeSym.Size]);
  CopyData(Value, 0, FData, FOffset, FTypeSym.Size);

  if Assigned(FDataMaster) then
  begin
    if FTypeSym.Size = FDataMaster.Size then
      FDataMaster.Write(FData)
    else
      raise Exception.CreateFmt(RTE_CanOnlyWriteBlocks, [FDataMaster.Caption,
        FTypeSym.Caption]);
  end;
end;

procedure TInfoData.SetValue(const Value: Variant);
begin
  if Assigned(FTypeSym) and (FTypeSym.Size = 1) then
    FData[FOffset] := Value
  else
    raise Exception.CreateFmt(RTE_CanNotSetValueForType, [FTypeSym.Caption]);

  if Assigned(FDataMaster) then
    FDataMaster.Write(FData);
end;

{ TInfoClass }

function TInfoClass.GetConstructor(MethName: string;
  ExtObject: TObject): IInfo;
begin
  Result := GetMethod(MethName);
  Result.ExternalObject := ExtObject;
end;

function TInfoClass.GetInherited: IInfo;
begin
  result := SetChild(FProgramInfo,(FTypeSym as TClassSymbol).Parent,FData,
    FOffset,FDataMaster);
end;

function TInfoClass.GetMethod(s: string): IInfo;
var
  sym: TSymbol;
begin
  if not (FTypeSym is TClassSymbol) then
    raise Exception.CreateFmt(RTE_NoClassNoMethod, [FTypeSym.Caption, s]);

  sym := TClassSymbol(FTypeSym).Members.FindSymbol(s);

  if not (sym is TMethodSymbol) then
    sym := nil;

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_MethodNotFoundInClass, [s, FTypeSym.Caption]);

  Result := TInfoFunc.Create(FProgramInfo, sym, nil, 0, nil, FScriptObj, TClassSymbol(FTypeSym));
{
  if Assigned(FScriptObj) then
    Result := TInfoFunc.Create(FProgramInfo, sym, nil, 0, nil, FScriptObj,
      FScriptObj.ClassSym)
  else
    Result := TInfoFunc.Create(FProgramInfo, sym, nil, 0, nil, nil,
      TClassSymbol(FTypeSym))
}
end;

function TInfoClass.GetScriptObj: IScriptObj;
begin
  Result := FScriptObj;
end;

{ TInfoClassObj }

constructor TInfoClassObj.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; DataMaster: IDataMaster);
begin
  inherited;
  if VarType(Data[Offset]) = varUnknown then
    FScriptObj := IScriptObj(IUnknown(Data[Offset]))
  else
    FScriptObj := nil;
end;

function TInfoClassObj.GetMember(s: string): IInfo;
var
  member: TSymbol;
begin
  member := FScriptObj.ClassSym.Members.FindLocal(s);

  if member is TFieldSymbol then
    Result := SetChild(FProgramInfo, member.Typ, FScriptObj.Data,
      TFieldSymbol(member).Offset)
  else if member is TPropertySymbol then
  begin
    Result := TInfoProperty.Create(FProgramInfo,member.Typ,nil,0,
      TPropertySymbol(member),FScriptObj);
  end
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TInfoClassOf }

constructor TInfoClassOf.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; DataMaster: IDataMaster);
begin
  inherited;
  FTypeSym := FCaller.Table.FindSymbol(FData[FOffset]);
end;

{ TTempParam }

constructor TTempParam.Create(ParamSym: TSymbol);
begin
  inherited Create(ParamSym.Name, ParamSym.Typ);
  FIsVarParam := ParamSym is TVarParamSymbol;
  SetLength(FData, FSize);
  ParamSym.InitData(FData, 0);
end;

{ TInfoFunc }

constructor TInfoFunc.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Data: TData; Offset: Integer; DataMaster: IDataMaster;
  ScriptObj: IScriptObj; ClassSym: TClassSymbol; ForceStatic: Boolean);
begin
  inherited Create(ProgramInfo, TypeSym, Data, Offset, DataMaster);
  FScriptObj := ScriptObj;
  FClassSym := ClassSym;
  FParams := TFuncSymbol(FTypeSym).Params;
  FParamSize := TFuncSymbol(FTypeSym).ParamSize;
  FTempParams := TSymbolTable.Create;
  FForceStatic := ForceStatic;

  if Assigned(TFuncSymbol(FTypeSym).Typ) then
    SetLength(FResult, TFuncSymbol(FTypeSym).Typ.Size)
  else if (FTypeSym is TMethodSymbol) and (TMethodSymbol(FTypeSym).Kind = fkConstructor) then
    SetLength(FResult, 1);
end;

destructor TInfoFunc.Destroy;
begin
  FTempParams.Free;
  inherited;
end;

function TInfoFunc.Call: IInfo;
var
  x, basePointer: Integer;
  tp: TTempParam;
  funcExpr: TFuncExpr;
  resultAddr: Integer;
  resultData: TData;
begin
  resultData := nil;
  if not FUsesTempParams then
    InitTempParams;

  // Simulate the params of the functions as local variables
  FCaller.Stack.Push(FParamSize);
  basePointer := FCaller.Stack.BasePointer;

  // Create the TFuncExpr
  funcExpr := GetFuncExpr(FCaller, TFuncSymbol(FTypeSym), FScriptObj, FClassSym, FForceStatic);
  try
    if funcExpr is TConstructorVirtualExpr then
      TConstructorVirtualExpr(funcExpr).ExternalObject := FExternalObject
    else if funcExpr is TConstructorStaticExpr then
      TConstructorStaticExpr(funcExpr).ExternalObject := FExternalObject;

    // Write the var-params as local variables to the stack.
    for x := 0 to FTempParams.Count - 1 do
    begin
      tp := TTempParam(FTempParams[x]);
      if tp.IsVarParam then
      begin
        FCaller.Stack.WriteData(0, basePointer + tp.StackAddr, tp.Size, tp.Data);
        funcExpr.AddArg(TVarExpr.Create(FCaller, NullPos, tp.Typ, tp));
      end
      else
        funcExpr.AddArg(TConstExpr.Create(FCaller, NullPos, tp.Typ, tp.Data));
    end;
    funcExpr.Initialize;
    if Assigned(funcExpr.Typ) then
    begin
      if funcExpr.Typ.Size > 1 then
      begin
        // Allocate space on the stack to store the Result value
        FCaller.Stack.Push(funcExpr.Typ.Size);
        try
          // Result-space is just behind the temporary-params
          // (Calculated relative to the basepointer of the caller!) 
          funcExpr.SetResultAddr(FParamSize);

          // Execute function.
          // Result is stored on the stack
          resultData := funcExpr.GetData;
          resultAddr := funcExpr.GetAddr;

          // Copy Result
          CopyData(resultData, resultAddr, FResult, 0, funcExpr.Typ.Size);
        finally
          FCaller.Stack.Pop(funcExpr.Typ.Size);
        end;
      end
      else
        VarCopy(FResult[0], funcExpr.Eval);

      Result := SetChild(FProgramInfo, funcExpr.Typ, FResult, 0);
    end
    else
    begin
      // Execute as procedure
      funcExpr.Eval;
      Result := nil;
    end;

    // Copy back the Result of var-parameters
    for x := 0 to FTempParams.Count - 1 do
    begin
      tp := TTempParam(FTempParams[x]);
      if tp.IsVarParam then
        FCaller.Stack.ReadData(basePointer + tp.StackAddr, 0, tp.Size, tp.Data);
    end;

  finally
    FCaller.Stack.Pop(FParamSize);
    funcExpr.Free;
  end;
end;

function TInfoFunc.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  funcSym: TFuncSymbol;
  dataSym: TDataSymbol;
  funcExpr: TFuncExpr;
  resultAddr: Integer;
  resultData: TData;
begin
  resultData := nil;
  funcSym := TFuncSymbol(FTypeSym);

  if Length(Params) <> funcSym.Params.Count then
    raise Exception.CreateFmt(RTE_InvalidNumberOfParams, [Length(Params),
      funcSym.Params.Count, FTypeSym.Caption]);

  // Create the TFuncExpr
  funcExpr := GetFuncExpr(FCaller, funcSym, FScriptObj, FClassSym, FForceStatic);

  if funcExpr is TConstructorVirtualExpr then
    TConstructorVirtualExpr(funcExpr).ExternalObject := FExternalObject
  else if funcExpr is TConstructorStaticExpr then
    TConstructorStaticExpr(funcExpr).ExternalObject := FExternalObject;

  try
    // Add arguments to the expression
    for x := Low(Params) to High(Params) do
    begin
      dataSym := TDataSymbol(FParams[x]);

      if dataSym.Size > 1 then
        raise Exception.CreateFmt(RTE_UseParameter, [dataSym.Caption,
          funcSym.Caption]);

      funcExpr.AddArg(TConstExpr.Create(FCaller, NullPos, dataSym.Typ, Params[x]));
    end;
    funcExpr.Initialize;
    if Assigned(funcExpr.Typ) then
    begin
      if funcExpr.Typ.Size > 1 then
      begin
        // Allocate space on the stack to store the Result value
        FCaller.Stack.Push(funcExpr.Typ.Size);
        try
          // Result-space is just behind the temporary-params
          funcExpr.SetResultAddr(FParamSize);

          // Execute function.
          // Result is stored on the stack
          resultData := funcExpr.GetData;
          resultAddr := funcExpr.GetAddr;

          // Copy Result
          for x := 0 to funcExpr.Typ.Size - 1 do
            FResult[x] := resultData[resultAddr + x];
        finally
          FCaller.Stack.Pop(funcExpr.Typ.Size);
        end;
      end
      else
        FResult[0] := funcExpr.Eval;

      Result := SetChild(FProgramInfo, funcExpr.Typ, FResult, 0);
    end
    else
      funcExpr.Eval;
  finally
    funcExpr.Free;
  end;
end;

function TInfoFunc.GetParameter(s: string): IInfo;
var
  tp: TTempParam;
begin
  if not FUsesTempParams then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s));

  if Assigned(tp) then
    Result := SetChild(FProgramInfo, tp.Typ, tp.FData, 0)
  else
    raise Exception.CreateFmt(RTE_NoParameterFound, [s, FTypeSym.Caption]);
end;

procedure TInfoFunc.InitTempParams;
var
  x: Integer;
  tp: TTempParam;
begin
  FTempParamSize := 0;
  for x := 0 to FParams.Count - 1 do
  begin
    tp := TTempParam.Create(FParams[x]);
    FTempParams.AddSymbol(tp);
    if tp.FIsVarParam then
    begin
      tp.StackAddr := FTempParamSize + FCaller.Stack.FrameSize;
      Inc(FTempParamSize, tp.Size);
    end;
  end;
  FUsesTempParams := True;
end;

function TInfoFunc.GetExternalObject: TObject;
begin
  Result := FExternalObject;
end;

procedure TInfoFunc.SetExternalObject(ExtObject: TObject);
begin
  FExternalObject := ExtObject;
end;

function TInfoFunc.GetInherited: IInfo;
begin
  if FTypeSym is TMethodSymbol then
    result := TInfoFunc.Create(FProgramInfo,TMethodSymbol(FTypeSym).ParentMeth,
      FData,FOffset,FDataMaster,FScriptObj,FClassSym.Parent,True)
  else
    result := inherited GetInherited;
end;

{ TInfoRecord }

function TInfoRecord.GetMember(s: string): IInfo;
var
  sym: TSymbol;
begin
  sym := TRecordSymbol(FTypeSym).Members.FindLocal(s);

  if not Assigned(sym) then
    raise Exception.CreateFmt(RTE_NoRecordMemberFound, [s, FTypeSym.Caption]);

  Result := SetChild(FProgramInfo, sym.Typ, FData, FOffset +
    TMemberSymbol(sym).Offset, FDataMaster);
end;

{ TInfoStaticArray }

function TInfoStaticArray.Element(const Indices: array of Integer): IInfo;
var
  x: Integer;
  elemTyp: TSymbol;
  arrTyp: TStaticArraySymbol;
  elemOff, elemIdx: Integer;
begin
  elemTyp := FTypeSym;
  elemOff := FOffset;
  for x := 0 to Length(Indices) - 1 do
  begin
    if Assigned(elemTyp) and (elemTyp.BaseType is TStaticArraySymbol) then
      arrTyp := TStaticArraySymbol(elemTyp.BaseType)
    else
      raise Exception.Create(RTE_TooManyIndices);

    if Indices[x] > arrTyp.HighBound then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded,[x]);

    if Indices[x] < arrTyp.LowBound then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded,[x]);

    elemTyp := arrTyp.Typ;
    elemIdx := Indices[x] - arrTyp.LowBound;
    elemOff := elemOff + elemIdx * elemTyp.Size;
  end;

  Result := SetChild(FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

function TInfoStaticArray.GetMember(s: string): IInfo;
var
  h, l: Integer;
begin
  h := TStaticArraySymbol(FTypeSym).HighBound;
  l := TStaticArraySymbol(FTypeSym).LowBound;
  if SameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, h - l + 1)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, h)
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TInfoDynamicArray }

function TInfoDynamicArray.Element(const Indices: array of Integer): IInfo;
var
  x: Integer;
  elemTyp: TSymbol;
  elemOff: Integer;
begin
  elemTyp := FTypeSym;
  elemOff := FOffset;
  for x := 0 to Length(Indices) - 1 do
  begin
    if Assigned(elemTyp) and (elemTyp.BaseType is TDynamicArraySymbol) then
      elemTyp := elemTyp.BaseType.Typ
    else
      raise Exception.Create(RTE_TooManyIndices);

    elemOff := FData[elemOff];

    if Indices[x] >= FData[elemOff - 1] then
      raise Exception.CreateFmt(RTE_ArrayUpperBoundExceeded,[x]);

    if Indices[x] < 0 then
      raise Exception.CreateFmt(RTE_ArrayLowerBoundExceeded,[x]);

    elemOff := elemOff + Indices[x] * elemTyp.Size;
  end;

  Result := SetChild(FProgramInfo, elemTyp, FData, elemOff, FDataMaster);
end;

function TInfoDynamicArray.GetMember(s: string): IInfo;
var
  l : Integer;
  elemOff: Integer;
begin
  elemOff := FData[FOffset];
  l := FData[elemOff - 1];
  if SameText('length', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l)
  else if SameText('low', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, 0)
  else if SameText('high', s) then
    Result := TInfoConst.Create(FProgramInfo, FProgramInfo.Caller.TypInteger, l - 1)
  else
    raise Exception.CreateFmt(RTE_NoMemberOfClass, [s, FTypeSym.Caption]);
end;

{ TIncrExpr }

constructor TIncrExpr.Create(Prog: TProgram; Pos: TScriptPos; Left, Right: TExpr);
begin
  inherited Create(Prog, Pos);
  FLeft := TDataExpr(Left);
  FRight := Right;
end;

destructor TIncrExpr.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  inherited;
end;

function TIncrExpr.Eval: Variant;
begin
  FLeft.AssignValue(FLeft.Eval + FRight.Eval);
end;

procedure TIncrExpr.Initialize;
begin
  FLeft.Initialize;
  FRight.Initialize;
end;

function TIncrExpr.Optimize: TExpr;
begin
  FLeft := FLeft.Optimize as TDataExpr;
  FRight := FRight.Optimize;
  Result := inherited Optimize;
end;

{ TConnectorExpr }

constructor TConnectorCallExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Name: string; BaseExpr: TExpr; IsWritable: Boolean; IsIndex: Boolean);
begin
  inherited Create(Prog, Pos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
  FIsInstruction := IsWritable;
  FArgs := TExprList.Create;
  FIsWritable := IsWritable;
  FIsIndex := IsIndex;
end;

destructor TConnectorCallExpr.Destroy;
begin
  FBaseExpr.Free;
  FArgs.Free;
  inherited;
end;

procedure TConnectorCallExpr.AddArg(ArgExpr: TExpr);
begin
  FArgs.Add(ArgExpr);
end;

function TConnectorCallExpr.AssignConnectorSym(ConnectorType: IConnectorType):
  Boolean;
var
  x: Integer;
  typSym: TSymbol;
begin
  // Prepare the parameter information array to query the connector symbol
  SetLength(FConnectorParams, FArgs.Count);
  for x := 0 to FArgs.Count - 1 do
  begin
    FConnectorParams[x].IsVarParam := (FArgs[x] is TDataExpr) and TDataExpr(FArgs[x]).IsWritable;
    FConnectorParams[x].TypSym := FArgs[x].Typ;
  end;

  // Ask the connector symbol if such a method exists
  if FIsIndex then
    FConnectorCall := ConnectorType.HasIndex(FName, FConnectorParams, typSym, FIsWritable)
  else begin
    FIsWritable := False;
    FConnectorCall := ConnectorType.HasMethod(FName, FConnectorParams, typSym);
  end;

  Result := Assigned(FConnectorCall);

  // Prepare the arguments for the method call
  if Result then
  begin
    SetLength(FConnectorArgs, FArgs.Count);
    for x := 0 to FArgs.Count - 1 do
      SetLength(FConnectorArgs[x], FConnectorParams[x].TypSym.Size);

    FTyp := typSym;
  end;
end;

function TConnectorCallExpr.Eval: Variant;
var
  dataSource, dataDest: TData;
  addrSource: Integer;
  x: Integer;
begin
  if FIsInstruction then
    FProg.DoStep(Self);

  if FProg.Root.IsDebugging then
    FProg.Debugger.EnterFunc(FProg, Self);

  // Call function
  try
    dataSource := nil;
    dataDest := nil;

    for x := 0 to Length(FConnectorArgs) - 1 do
    begin
      if FConnectorParams[x].TypSym.Size = 1 then
        VarCopy(FConnectorArgs[x][0], FArgs[x].Eval)
      else
      begin
        dataSource := TDataExpr(FArgs[x]).Data;
        addrSource := TDataExpr(FArgs[x]).Addr;
        dataDest := FConnectorArgs[x];
        CopyData(dataSource, addrSource, dataDest, 0, FConnectorParams[x].TypSym.Size);
      end;
    end;

    try
      // The call itself
      if FBaseExpr is TDataExpr then
        FResultData := FConnectorCall.Call(TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr], FConnectorArgs)
      else
        FResultData := FConnectorCall.Call(FBaseExpr.Eval, FConnectorArgs);
    except
      on e: EExit do
        ;
      on e: EBreak do
        FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidBreak);
      on e: EContinue do
        FProg.Msgs.AddExecutionStop(FPos, RTE_InvalidContinue);
      on e: EScriptException do
        raise;
    else
      FProg.Msgs.SetScriptError(FPos,ExceptObject);
      raise;
    end;

    for x := 0 to Length(FConnectorArgs) - 1 do
      if FConnectorParams[x].IsVarParam then
        TDataExpr(FArgs[x]).AssignData(FConnectorArgs[x], 0);

  finally
    if FProg.Root.IsDebugging then
      FProg.Debugger.LeaveFunc(FProg, Self);
  end;

  if Assigned(FResultData) then
    Result := FResultData[0]
  else
    VarClear(Result);
end;

function TConnectorCallExpr.GetData: TData;
begin
  Eval;
  Result := FResultData;
end;

procedure TConnectorCallExpr.Initialize;
begin
	inherited;
  FBaseExpr.Initialize;
  FArgs.Initialize;
end;

function TConnectorCallExpr.Optimize: TExpr;
begin
  FBaseExpr := FBaseExpr.Optimize;
  FArgs.Optimize(False);
	Result := inherited Optimize;
end;

{ TConnectorReadExpr }

function TConnectorReadExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp,False);
  Result := Assigned(FConnectorMember);
end;

constructor TConnectorReadExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Name: string; BaseExpr: TExpr);
begin
  inherited Create(Prog, Pos, nil);
  FName := Name;
  FBaseExpr := BaseExpr;
end;

destructor TConnectorReadExpr.Destroy;
begin
  FBaseExpr.Free;
  inherited;
end;

function TConnectorReadExpr.Eval: Variant;
begin
  try
    if FBaseExpr is TDataExpr then
      FResultData := FConnectorMember.Read(TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr])
    else
      FResultData := FConnectorMember.Read(FBaseExpr.Eval);
    Result := FResultData[0];
  except
    FProg.Msgs.SetScriptError(FPos,ExceptObject);
    raise;
  end;
end;

function TConnectorReadExpr.GetData: TData;
begin
  Eval;
  Result := FResultData;
end;

procedure TConnectorReadExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
end;

function TConnectorReadExpr.Optimize: TExpr;
begin
  FBaseExpr := FBaseExpr.Optimize;
	Result := inherited Optimize;
end;

{ TConnectorWriteExpr }

function TConnectorWriteExpr.AssignConnectorSym(
  ConnectorType: IConnectorType): Boolean;
begin
  FConnectorMember := ConnectorType.HasMember(FName, FTyp, True);
  Result := Assigned(FConnectorMember);
  if Result and not (Assigned(FTyp) and Assigned(FValueExpr.Typ) and
    FTyp.IsCompatible(FValueExpr.Typ)) then
    FProg.Msgs.AddCompilerError(FPos, CPE_ConnectorTypeMismatch);
end;

constructor TConnectorWriteExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Name: string; BaseExpr, ValueExpr: TExpr);
begin
  inherited Create(Prog, Pos);
  FName := Name;
  FBaseExpr := BaseExpr;
  FValueExpr := ValueExpr;
end;

destructor TConnectorWriteExpr.Destroy;
begin
  FBaseExpr.Free;
  FValueExpr.Free;
  inherited;
end;

function TConnectorWriteExpr.Eval: Variant;
var
  dat: TData;
  tmp: Variant;
  Base: pVariant;
begin
  if FBaseExpr is TDataExpr then
    Base := @TDataExpr(FBaseExpr).Data[TDataExpr(FBaseExpr).Addr]
  else begin
    tmp := FBaseExpr.Eval;
    Base := @tmp;
  end;

  if FValueExpr is TDataExpr then
    dat := TDataExpr(FValueExpr).GetData
  else
  begin
    SetLength(dat, 1);
    dat[0] := FValueExpr.Eval;
  end;

  try
    FConnectorMember.Write(Base^, dat);
  except
    FProg.Msgs.SetScriptError(FPos,ExceptObject);
    raise;
  end;
end;

procedure TConnectorWriteExpr.Initialize;
begin
  inherited;
  FBaseExpr.Initialize;
  FValueExpr.Initialize;
end;

function TConnectorWriteExpr.Optimize: TExpr;
begin
  FBaseExpr := FBaseExpr.Optimize;
  FValueExpr := FValueExpr.Optimize;
  Result := inherited Optimize;
end;

{ TInfoConnector }

function TInfoConnector.GetMember(s: string): IInfo;
begin
  Result := TInfo.SetChild(FProgramInfo, FTypeSym, FData, FOffset,
    TConnectorMemberDataMaster.Create(FCaller, FTypeSym, s, FData[FOffset]));
end;

function TInfoConnector.GetMethod(s: string): IInfo;
begin
  Result := TInfoConnectorCall.Create(FProgramInfo, FTypeSym,
    FData, FOffset, TConnectorSymbol(FTypeSym).ConnectorType, s);
end;

{ TInfoConnectorCall }

constructor TInfoConnectorCall.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  ConnectorType: IConnectorType; Name: string);
begin
  inherited Create(ProgramInfo, TypeSym, Data, Offset);
  FConnectorType := ConnectorType;
  FName := Name;
end;

function TInfoConnectorCall.Call(const Params: array of Variant): IInfo;
var
  x: Integer;
  expr: TConnectorCallExpr;
  resultData: TData;
begin
  expr := TConnectorCallExpr.Create(FCaller, NullPos, FName,
    TConstExpr.Create(FCaller, NullPos, FCaller.TypVariant, FData[FOffset]));

  try
    for x := 0 to Length(Params) - 1 do
      expr.AddArg(TConstExpr.Create(FCaller, NullPos,
        FCaller.TypVariant, Params[x]));

    if expr.AssignConnectorSym(FConnectorType) then
    begin
      if Assigned(expr.Typ) then
      begin
        SetLength(resultData, 1);
        resultData[0] := expr.Eval;
        Result := TInfo.SetChild(FProgramInfo, expr.Typ, resultData, 0);
      end
      else
      begin
        resultData := nil;
        expr.Eval;
        Result := nil;
      end;
    end
    else
      raise Exception.CreateFmt(RTE_ConnectorCallFailed, [FName]);
  finally
    expr.Free;
  end;
end;

{ TDataMaster }

constructor TDataMaster.Create(Caller: TProgram; Sym: TSymbol);
begin
  FCaller := Caller;
  FSym := Sym;
end;

function TDataMaster.GetCaption: string;
begin
  Result := FSym.Caption;
end;

function TDataMaster.GetSize: Integer;
begin
  Result := FSym.Size;
end;

procedure TDataMaster.Read(const Data: TData);
begin
end;

procedure TDataMaster.Write(const Data: TData);
begin
end;

{ TExternalVarDataMaster }

procedure TExternalVarDataMaster.Read(const Data: TData);
var
  x: Integer;
  resultData: TData;
  resultAddr: Integer;
  funcExpr: TFuncExpr;
begin
  resultData := nil;
  // Read an external var
  funcExpr := GetFuncExpr(FCaller, TExternalVarSymbol(FSym).ReadFunc, nil, nil);
  try
    funcExpr.Initialize;
    if funcExpr.Typ.Size > 1 then // !! > 1 untested !!
    begin
      funcExpr.SetResultAddr(FCaller.Stack.FrameSize);
      // Allocate space on the stack to store the Result value
      FCaller.Stack.Push(funcExpr.Typ.Size);
      try
        // Execute function.
        resultData := funcExpr.GetData;
        resultAddr := funcExpr.GetAddr;
        // Copy Result
        for x := 0 to funcExpr.Typ.Size - 1 do
          Data[x] := resultData[resultAddr + x];
      finally
        FCaller.Stack.Pop(funcExpr.Typ.Size);
      end;
    end
    else
      VarCopy(Data[0],funcExpr.Eval);
  finally
    funcExpr.Free;
  end;
end;

procedure TExternalVarDataMaster.Write(const Data: TData);
var
  funcExpr: TFuncExpr;
begin
  funcExpr := GetFuncExpr(FCaller, TExternalVarSymbol(FSym).WriteFunc, nil, nil);
  try
    funcExpr.AddArg(TConstExpr.Create(FCaller, NullPos, FSym.Typ, Data));
    funcExpr.AddPushExprs;
    funcExpr.Eval;
  finally
    funcExpr.Free;
  end;
end;

{ TConnectorMemberDataMaster }

constructor TConnectorMemberDataMaster.Create(Caller: TProgram;
  Sym: TSymbol; BaseValue: Variant; Name: string);
begin
  inherited Create(Caller, Sym);
  FName := Name;
end;

procedure TConnectorMemberDataMaster.Read(const Data: TData);
var
  readExpr: TConnectorReadExpr;
  dataSource: TData;
begin
  dataSource := nil;
  readExpr := TConnectorReadExpr.Create(FCaller, NullPos, FName,
    TConstExpr.Create(FCaller, NullPos, FCaller.TypVariant, FBaseValue));

  if readExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    dataSource := readExpr.GetData;
    CopyData(dataSource, 0, Data, 0, readExpr.Typ.Size);
  end
  else
    raise Exception.Create(RTE_ConnectorReadError);
end;

procedure TConnectorMemberDataMaster.Write(const Data: TData);
var
  writeExpr: TConnectorWriteExpr;
begin
  writeExpr := TConnectorWriteExpr.Create(FCaller, NullPos, FName,
    TConstExpr.Create(FCaller, NullPos, FCaller.TypVariant, FBaseValue),
    TConstExpr.Create(FCaller, NullPos, FCaller.TypVariant, Data));

  if writeExpr.AssignConnectorSym(TConnectorSymbol(FSym).ConnectorType) then
  begin
    writeExpr.Eval;
  end
  else
    raise Exception.Create(RTE_ConnectorWriteError);
end;

{ TStringArrayOpExpr }

constructor TStringArrayOpExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Left, Right: TExpr);
begin
  inherited;
  FTyp := FProg.TypString;
end;

function TStringArrayOpExpr.Eval: Variant;
begin
  Result := string(FLeft.Eval)[Integer(FRight.Eval)];
end;

procedure TStringArrayOpExpr.TypeCheck;
begin
  inherited;
  if not (IsStringType(FLeft.Typ) and IsIntegerType(FRight.Typ)) then
    FProg.FMsgs.AddCompilerStop(FPos, CPE_StringExpected);
end;

{ TArrayLengthExpr }

constructor TArrayLengthExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Expr: TExpr; Delta: Integer);
begin
  inherited Create(Prog, Pos, Expr);
  FDelta := Delta;
  FTyp := FProg.TypInteger;
end;

function TArrayLengthExpr.Eval: Variant;
var
  adr: Integer;
begin
  adr := TDataExpr(FExpr).Data[TDataExpr(FExpr).Addr];
  Result := TDataExpr(FExpr).Data[adr - 1] + FDelta;
end;

{ TInitDataExpr }

constructor TInitDataExpr.Create(Prog: TProgram; Pos: TScriptPos; Expr: TDataExpr);
begin
  inherited Create(Prog, Pos);
  FExpr := Expr;
end;

destructor TInitDataExpr.Destroy;
begin
  FExpr.Free;
  inherited;
end;

function TInitDataExpr.Eval: Variant;
begin
  FExpr.Typ.InitData(FExpr.Data, FExpr.Addr);
end;

{ TInfoConst }

constructor TInfoConst.Create(ProgramInfo: TProgramInfo; TypeSym: TSymbol;
  const Value: Variant);
begin
  inherited Create(ProgramInfo, TypeSym, FData, 0);
  SetLength(FData, TypeSym.Size);
  VarCopy(FData[0], Value);
end;

function TInfoConst.GetData: TData;
begin
  Result := FData;
end;

function TInfoConst.GetValue: Variant;
begin
  Result := FData[0];
end;

{ TSymbolDictionary }

procedure TSymbolDictionary.Add(Sym: TSymbol; Pos: TScriptPos; UseTypes: TSymbolUsages);
var
  SymPosList: TSymbolPositionList;
begin
  if not Assigned(Sym) then
    Exit;   // don't add a nil pointer
  if Sym is TBaseSymbol then
    Exit;    // don't store references to base symbols

  { Check to see if symbol list already exists, if not create it }
  SymPosList := FindSymbolPosList(Sym);
  if SymPosList = nil then
  begin
    SymPosList := TSymbolPositionList.Create(Sym);
    FSymbolList.Add(SymPosList);      // add list for new symbol
  end;
  SymPosList.Add(Pos, UseTypes);      // add the instance of the symbol to the position list
end;

constructor TSymbolDictionary.Create;
begin
  FSymbolList := TList.Create;
end;

destructor TSymbolDictionary.Destroy;
begin
  Clear;
  FSymbolList.Free;
  inherited;
end;

function TSymbolDictionary.FindSymbolAtPosition(AbsolutePos: Integer): TSymbol;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
  begin
    Result := TSymbolPositionList(FSymbolList[x]).FindSymbolAtPosition(AbsolutePos);
    if Assigned(Result) then
      Break;           // found symbol, stop searching
  end;
end;

function TSymbolDictionary.FindSymbolAtPosition(ACol, ALine: Integer): TSymbol;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
  begin
    Result := TSymbolPositionList(FSymbolList[x]).FindSymbolAtPosition(ACol, ALine);
    if Assigned(Result) then
      Break;            // found symbol, stop searching
  end;
end;

function TSymbolDictionary.GetList(Index: Integer): TSymbolPositionList;
begin
  Result := TSymbolPositionList(FSymbolList[Index]);
end;

function TSymbolDictionary.Count: Integer;
begin
  Result := FSymbolList.Count;
end;

function TSymbolDictionary.FindSymbolPosList(Sym: TSymbol): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
    if TSymbolPositionList(FSymbolList[x]).Symbol = Sym then
    begin
      Result := TSymbolPositionList(FSymbolList[x]);
      Break;
    end;
end;

function TSymbolDictionary.FindSymbolPosList(SymName: string): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FSymbolList.Count - 1 do
    if CompareText(TSymbolPositionList(FSymbolList[x]).Symbol.Name, SymName) = 0 then // same name (not case-sensitive)
    begin
      Result := TSymbolPositionList(FSymbolList[x]);
      Break;
    end;
end;

procedure TSymbolDictionary.Remove(Sym: TSymbol);
var
  idx, x: Integer;
  SymList: TSymbolPositionList;
begin
  // TFuncSymbol - remove params
  if Sym is TFuncSymbol then
  begin
    for x := 0 to TFuncSymbol(Sym).Params.Count - 1 do
      Remove(TFuncSymbol(Sym).Params[x]);   
  end
  // TClassSymbol - remove members (methods, fields, properties)
  else if Sym is TClassSymbol then
  begin
    for x := 0 to TClassSymbol(Sym).Members.Count - 1 do
      Remove(TClassSymbol(Sym).Members[x]);    
  end
  // TRecordSymbol - remove members
  else if Sym is TRecordSymbol then
  begin
    for x := 0 to TRecordSymbol(Sym).Members.Count - 1 do
      Remove(TRecordSymbol(Sym).Members[x]);
  end;

  // basic entry to remove
  SymList := FindSymbolPosList(Sym);
  if Assigned(SymList) then
  begin
    // remove SymList from internal list
    idx := FSymbolList.IndexOf(SymList);
    if idx >= 0 then
    begin
      FSymbolList.Delete(idx);     // delete entry from the list
      SymList.Free;                // free the object
    end;
  end;
end;

procedure TSymbolDictionary.Clear;
var
  x: Integer;
begin
  for x := 0 to FSymbolList.Count - 1 do
    try
      TSymbolPositionList(FSymbolList[x]).Free;
    except
      FSymbolList[x] := nil;   // initialized things *could* be left
    end;
end;

procedure TSymbolDictionary.SetList(Index: Integer; PosList: TSymbolPositionList);
begin
  FSymbolList[Index] := PosList;
end;

function TSymbolDictionary.FindSymbolUsage(Symbol: TSymbol;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(Symbol);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolUsage(SymName: string;
  SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosList(SymName);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolUsageOfType(SymName: string;
  SymbolType: TSymbolClass; SymbolUse: TSymbolUsage): TSymbolPosition;
var
  list: TSymbolPositionList;
begin
  Result := nil;
  list := FindSymbolPosListOfType(SymName, SymbolType);
  if Assigned(list) then
    Result := list.FindUsage(SymbolUse);
end;

function TSymbolDictionary.FindSymbolPosListOfType(SymName: string;
  SymbolType: TSymbolClass): TSymbolPositionList;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to Self.Count - 1 do
    if SameText(Self.Items[x].Symbol.Name, SymName) and (Self.Items[x].Symbol is SymbolType) then // same name (not case-sensitive)
    begin
      Result := Self.Items[x];
      Break;
    end;
end;

{ TSymbolPositionList }

procedure TSymbolPositionList.Add(Pos: TScriptPos; UseTypes: TSymbolUsages);
var
  SymPos: TSymbolPosition;
begin
  if (Pos.Pos = -1) or (Pos.SourceFile = nil) then EXIT; // don't add invalid entry

  SymPos := TSymbolPosition.Create(Self, Pos, UseTypes);
  FPosList.Add(SymPos);              // add position information to the list
end;

constructor TSymbolPositionList.Create(ASymbol: TSymbol);
begin
  FSymbol := ASymbol;
  FPosList := TList.Create;
end;

destructor TSymbolPositionList.Destroy;
var
  x: Integer;
begin
  for x := 0 to FPosList.Count - 1 do
    TSymbolPosition(FPosList[x]).Free;
  FPosList.Free;
  inherited;
end;

function TSymbolPositionList.FindSymbolAtPosition(AbsolutePos: Integer): TSymbol;
var
  x: Integer;
begin
  Result := nil;          // default to not found
  for x := 0 to FPosList.Count - 1 do
    if TSymbolPosition(FPosList[x]).ScriptPos.Pos = AbsolutePos then
    begin
      Result := FSymbol;
      Break;    // found the symbol, stop searching
    end;
end;

function TSymbolPositionList.FindSymbolAtPosition(ACol, ALine: Integer): TSymbol;
var
  x: Integer;
begin
  Result := nil;          // default to not found
  for x := 0 to FPosList.Count - 1 do
    with TSymbolPosition(FPosList[x]) do
      if (ScriptPos.Line = ALine) and (ScriptPos.Col = ACol) then
      begin
        Result := Symbol;
        Break;    // found the symbol, stop searching
      end;
end;

function TSymbolPositionList.GetPosition(Index: Integer): TSymbolPosition;
begin
  Result := TSymbolPosition(FPosList[Index]);
end;

function TSymbolPositionList.Count: Integer;
begin
  Result := FPosList.Count;
end;

procedure TSymbolPositionList.SetPosition(Index: Integer; SymPos: TSymbolPosition);
begin
  FPosList[Index] := SymPos;
end;

function TSymbolPositionList.FindUsage(SymbolUse: TSymbolUsage): TSymbolPosition;
var
  x: Integer;
begin
  Result := nil;          // default to not found
  for x := 0 to FPosList.Count - 1 do
    if SymbolUse in TSymbolPosition(FPosList[x]).SymbolUsages then
    begin
      Result := TSymbolPosition(FPosList[x]);
      Break;    // found the symbol, stop searching
    end;
end;

{ TSymbolPosition }

constructor TSymbolPosition.Create(AOwningList: TSymbolPositionList; AScriptPos: TScriptPos; AUsages: TSymbolUsages);
begin
  FOwnerList := AOwningList;
  FScriptPos := AScriptPos;
  FSymUsages := AUsages;
end;

function TSymbolPosition.GetSymbol: TSymbol;
begin
  if Assigned(FOwnerList) then
    Result := FOwnerList.Symbol
  else
    Result := nil;
end;

{ TContext }

constructor TContext.Create(AParent: TContext; AStartPos: TScriptPos;
  AParentSymbol: TSymbol);
begin
  FSubContexts := TList.Create;

  { Initialize variables }
  FParentContext := AParent;
  FParentSymbol  := AParentSymbol;
  FLocalTable    := nil;             // default to nil. Didn't pass in because uses didn't have access to that data when context is openned
  // starting position
  FStartPos := AStartPos;
  // invalid end position
  FEndPos.Pos  := -1;
  FEndPos.Line := -1;
  FEndPos.Col  := -1;
  FEndPos.SourceFile := nil;
end;

destructor TContext.Destroy;
var
  x: Integer;
begin
  for x := 0 to FSubContexts.Count - 1 do
    TContext(FSubContexts[x]).Free;
  inherited;
end;

function TContext.HasParentSymbolOfClass(SymbolType: TSymbolClass;
  SearchParents: Boolean): Boolean;
begin
  // Return if the context has a parent symbol of the specified type. Optionally
  // search up through other parent contexts
  Result := False;
  if Assigned(ParentSym) then
    Result := (ParentSym is SymbolType);
  // if not found and parents should be searched also, recurse until no more
  // parents or the symbol type is found.
  if (not Result) and SearchParents then
    if Assigned(Parent) then
      Result := Parent.HasParentSymbolOfClass(SymbolType, SearchParents);
end;

function TContext.IsPositionInContext(ACol, ALine: Integer; SourceFile: TSourceFile): Boolean;
begin
  // check if the position is in the same SourceFile
  if Assigned(SourceFile) then  // if not assigned, don't check it
    if SourceFile <> FStartPos.SourceFile then
    begin
      Result := False;
      Exit;
    end;

  // if inside a multi-line context
  Result := (ALine > FStartPos.Line) and (ALine < FEndPos.Line);
  if not Result then
  begin
    // if not, check for a one-line context (inside the context begin and end cols)
    if FStartPos.Line = FEndPos.Line then
      Result := (ALine = FStartPos.Line) and (ACol >= FStartPos.Col) and (ACol <= FEndPos.Col)
    else  // not a single-line context
      Result := ((ALine = FStartPos.Line) and (ACol >= FStartPos.Col)) or // on top line, inside start
                ((ALine = FEndPos.Line) and (ACol <= FEndPos.Col));       // on bottom line, inside end
  end;
end;

{ TContextMap }

procedure TContextMap.CloseContext(AEndPos: TScriptPos);
begin
  FCurrentContext.FEndPos := AEndPos;       // close the current context
  { if the CurrentContext is not a top-level one, then pop the stack and make
    the new context the closed one's parent }
  FCurrentContext := FCurrentContext.Parent;
end;

constructor TContextMap.Create;
begin
  FScriptContexts := TList.Create;
  FCurrentContext := nil;
end;

destructor TContextMap.Destroy;
var
  x: Integer;
begin
  for x := 0 to FScriptContexts.Count - 1 do
    TContext(FScriptContexts[x]).Free;
  FScriptContexts.Free;
  inherited;
end;

function TContextMap.FindContext(AParentSymbol: TSymbol): TContext;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FScriptContexts.Count - 1 do
  begin
    if TContext(FScriptContexts[x]).FParentSymbol = AParentSymbol then
    begin
      Result := TContext(FScriptContexts[x]);
      BREAK;
    end;
  end;
end;

function TContextMap.FindContext(ACol, ALine: Integer; SourceFile: TSourceFile): TContext;
var
  ReturnContext: TContext;    // Gets set to the context found
  HitEnd: Boolean;            // Followed branch to end, stop searching

    function FoundContext(Context: TContext): Boolean;
    var
      x: Integer;
    begin
      Result := False;
      { Record that this context contains it and should be returned (provided it
        doesn't go deeper) }
      ReturnContext := Context;
      { Search sub-contexts }
      for x := 0 to Context.SubContexts.Count - 1 do
      begin
        if TContext(Context.SubContexts[x]).IsPositionInContext(ACol, ALine, SourceFile) then
          Result := FoundContext(TContext(Context.SubContexts[x]))
      end;
      { We got here because it was found. After all subContexts were checked,
        it wasn't found so we've hit the end. }
      if not Result then
        HitEnd := True;
    end;

var
  i: Integer;
begin
  { If this position is not in the top level contexts then it won't be in
    subcontexts. Use a recursive search to find the lowest context at which the
    position can be found. }

  ReturnContext := nil;
  HitEnd        := False;
  { Cycle all top level contexts. Burrow into each to find the lowest level that
    matches the criteria. }
  for i := 0 to FScriptContexts.Count - 1 do
  begin
    if HitEnd then
      BREAK;
    { If in top-level context, burrow into subcontexts }
    if TContext(FScriptContexts[i]).IsPositionInContext(ACol, ALine, SourceFile) then
      if not FoundContext(TContext(FScriptContexts[i])) then
        Break;
  end;
  Result := ReturnContext;
end;

function TContextMap.FindContext(ScriptPos: TScriptPos): TContext;
begin
  Result := FindContext(ScriptPos.Col, ScriptPos.Line, ScriptPos.SourceFile);
end;

procedure TContextMap.OpenContext(AStartPos: TScriptPos; AParentSymbol: TSymbol);
var
  NewContext: TContext;
begin
  { Uses a simple 'stack' concept. If currently in a context and a new context
    is openned then the new context is a sub context of the current context. }
  NewContext := TContext.Create(FCurrentContext, AStartPos, AParentSymbol);  // new context is owned by the current context
  { Add new context to the appropriate 'parent' context }
  if FCurrentContext = nil then           // if top-level,
    FScriptContexts.Add(NewContext)       // Add to top-level contexts
  else
    FCurrentContext.SubContexts.Add(NewContext);
  FCurrentContext := NewContext;
end;

{ TScriptSourceItem }

constructor TScriptSourceItem.Create(ANameReference: string; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
begin
  FNameReference := ANameReference;
  FSourceFile := ASourceFile;
  FSourceType := ASourceType;
end;

{ TScriptSourceList }

procedure TScriptSourceList.Add(ANameReference: string; ASourceFile: TSourceFile;
  ASourceType: TScriptSourceType);
var
  testItem: TScriptSourceItem;
begin
  { Determine if that script item alread exists, if not add it. }
  testItem := FindScriptSourceItem(ASourceFile);
  if not Assigned(testItem) then   // if not found, create a new one and add it to the list
  begin
    testItem := TScriptSourceItem.Create(ANameReference, ASourceFile, ASourceType);
    FSourceList.Add(testItem);
    // get a pointer to the 'main' script item
    if ASourceType = stMain then
      FMainScript := testItem;
  end;
end;

procedure TScriptSourceList.Clear;
var
  x: Integer;
begin
  for x := 0 to FSourceList.Count - 1 do
    try
      TScriptSourceItem(FSourceList[x]).Free;
    except
      FSourceList[x] := nil;   // initialized things *could* be left
    end;
end;

function TScriptSourceList.Count: Integer;
begin
  Result := FSourceList.Count;
end;

constructor TScriptSourceList.Create;
begin
  FSourceList := TList.Create;
  FMainScript := nil;
end;

destructor TScriptSourceList.Destroy;
begin
  Clear;
  FSourceList.Free;
  inherited;
end;

function TScriptSourceList.FindScriptSourceItem(SourceFileName: string): TScriptSourceItem;
var
  x: Integer;
begin
  Result := nil;
  x := IndexOf(SourceFileName);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.FindScriptSourceItem(SourceFile: TSourceFile): TScriptSourceItem;
var
  x: Integer;
begin
  Result :=  nil;
  x := IndexOf(SourceFile);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.FindScriptSourceItem(ScriptPos: TScriptPos): TScriptSourceItem;
var
  x: Integer;
begin
  Result :=  nil;
  x := IndexOf(ScriptPos);
  if x > -1 then
    Result := TScriptSourceItem(FSourceList[x]);
end;

function TScriptSourceList.GetSourceItem(Index: Integer): TScriptSourceItem;
begin
  Result := TScriptSourceItem(FSourceList[Index]);
end;

function TScriptSourceList.IndexOf(ASourceFile: TSourceFile): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to FSourceList.Count - 1 do
  begin
    // if they both point to the same TSourceFile then they match
    if TScriptSourceItem(FSourceList[x]).SourceFile = ASourceFile then
    begin
      Result := x;
      Break;           // found match, stop searching
    end;
  end;
end;

function TScriptSourceList.IndexOf(SourceFileName: string): Integer;
var
  x: Integer;
begin
  Result := -1;
  for x := 0 to FSourceList.Count - 1 do
  begin
    // if both names match, consider it a match
    if CompareText(TScriptSourceItem(FSourceList[x]).SourceFile.SourceFile, SourceFileName) = 0 then
    begin
      Result := x;
      Break;           // found match, stop searching
    end;
  end;
end;

function TScriptSourceList.IndexOf(AScriptPos: TScriptPos): Integer;
begin
  Result := IndexOf(AScriptPos.SourceFile);
end;

procedure TScriptSourceList.SetSourceItem(Index: Integer;
  SourceItem: TScriptSourceItem);
begin
  FSourceList[Index] := SourceItem;
end;

{ TFuncCodeExpr }

procedure TFuncCodeExpr.AssignDataExpr(Right: TDataExpr);
begin
  Assert(Right is TFuncCodeExpr);
  Assert(FFuncExpr is TMethodStaticExpr);
  Assert(TFuncCodeExpr(Right).FuncExpr is TMethodStaticExpr);
  FFuncExpr.CodeExpr.AssignValue(TFuncCodeExpr(Right).Eval);
  TMethodStaticExpr(FFuncExpr).BaseExpr.AssignValue(
    TMethodStaticExpr(TFuncCodeExpr(Right).FuncExpr).BaseExpr.Eval);
end;

constructor TFuncCodeExpr.Create(Prog: TProgram; Pos: TScriptPos;
  FuncExpr: TFuncExpr);
begin
  inherited Create(Prog,Pos,FuncExpr.FuncSym);
  FFuncExpr := FuncExpr;
end;

destructor TFuncCodeExpr.Destroy;
begin
  FFuncExpr.Free;
  inherited;
end;

function TFuncCodeExpr.Eval: Variant;
begin
  Result := FFuncExpr.GetCode(FFuncExpr.FuncSym);
end;

function TFuncCodeExpr.GetAddr: Integer;
begin
  Result := FFuncExpr.CodeExpr.Addr;
end;

function TFuncCodeExpr.GetData: TData;
begin
  Result := FFuncExpr.CodeExpr.Data;
end;

procedure TFuncCodeExpr.TypeCheck;
begin
  if FFuncExpr.Args.Count > 0 then
    FProg.FMsgs.AddCompilerError(FPos, CPE_NoArgumentsExpected);
end;

{ TMethodObjExpr }

constructor TMethodObjExpr.Create(Prog: TProgram; Pos: TScriptPos;
  BaseExpr: TDataExpr);
begin
  Assert(BaseExpr.Typ is TMethodSymbol);
  inherited Create(Prog,Pos,TMethodSymbol(BaseExpr.Typ).ClassSymbol);
  FBaseExpr := BaseExpr;
end;

function TMethodObjExpr.GetAddr: Integer;
begin
  Result := FBaseExpr.Addr + 1;
end;

function TMethodObjExpr.GetData: TData;
begin
  Result := FBaseExpr.Data;
end;

{ TStringLengthExpr }

constructor TStringLengthExpr.Create(Prog: TProgram; Pos: TScriptPos;
  Expr: TExpr);
begin
  inherited;
  FTyp := FProg.TypInteger;
end;

function TStringLengthExpr.Eval: Variant;
begin
  Result := Length(string(FExpr.Eval));
end;

{ TObjCmpExpr }

constructor TObjCmpExpr.Create(Prog: TProgram; Pos: TScriptPos; Left,
  Right: TExpr; Equal: Boolean);
begin
  inherited Create(Prog, Pos, Left, Right);
  FEqual := Equal;
  FTyp := FProg.TypBoolean;
end;

function TObjCmpExpr.Eval: Variant;

  function SameInterface(const Left, Right: Variant) : Boolean;
  begin
    result := TVarData(Left).VUnknown = TVarData(Right).VUnknown;
    // if not result then compare external objects ?
  end;

begin
  if FEqual then
    Result := SameInterface(FLeft.Eval, FRight.Eval)
  else
    Result := not SameInterface(FLeft.Eval, FRight.Eval);
end;

procedure TObjCmpExpr.TypeCheck;
begin
  FLeft.TypeCheck;
  FRight.TypeCheck;
  if not ((FLeft.Typ is TClassSymbol) or (FLeft.Typ = FProg.TypNil)) then
    FProg.Msgs.AddCompilerStop(FLeft.Pos, CPE_ObjectExpected);
  if not ((FRight.Typ is TClassSymbol) or (FRight.Typ = FProg.TypNil)) then
    FProg.Msgs.AddCompilerStop(FRight.Pos, CPE_ObjectExpected);
end;

{ TConstructorStaticObjExpr }

constructor TConstructorStaticObjExpr.Create(Prog: TProgram;
  Pos: TScriptPos; Func: TMethodSymbol; BaseExpr: TDataExpr;
  IsInstruction: Boolean; CodeExpr: TDataExpr; IsWritable: Boolean);
begin
  inherited Create(Prog,Pos,Func,BaseExpr,IsInstruction,CodeExpr,IsWritable);
  Typ := BaseExpr.Typ;
end;

function TConstructorStaticObjExpr.PostCall(ScriptObj: IScriptObj): Variant;
begin
  result := ScriptObj;
end;

{ TConstructorVirtualObjExpr }

constructor TConstructorVirtualObjExpr.Create(Prog: TProgram;
  Pos: TScriptPos; Func: TMethodSymbol; Base: TDataExpr; IsInstruction: Boolean);
begin
  inherited Create(Prog,Pos,Func,Base,IsInstruction);
  Typ := Base.Typ;
end;

function TConstructorVirtualObjExpr.PostCall(ScriptObj: IScriptObj): Variant;
begin
  result := ScriptObj;
end;

{ TInfoProperty }

constructor TInfoProperty.Create(ProgramInfo: TProgramInfo;
  TypeSym: TSymbol; const Data: TData; Offset: Integer;
  PropSym: TPropertySymbol; ScriptObj: IScriptObj);
begin
  inherited Create(ProgramInfo,TypeSym,Data,Offset);
  FPropSym := PropSym;
  FScriptObj := ScriptObj;
end;

destructor TInfoProperty.Destroy;
begin
  FTempParams.Free;
  inherited;
end;

function TInfoProperty.GetParameter(s: string): IInfo;
var
  tp: TTempParam;
begin
  if not Assigned(FTempParams) then
    InitTempParams;

  tp := TTempParam(FTempParams.FindSymbol(s));

  if Assigned(tp) then
    Result := SetChild(FProgramInfo, tp.Typ, tp.FData, 0)
  else
    raise Exception.CreateFmt(RTE_NoIndexFound, [s, FPropSym.Name]);
end;

procedure TInfoProperty.InitTempParams;
var
  x: Integer;
  tp: TTempParam;
begin
  FTempParams := TSymbolTable.Create;
  for x := 0 to FPropSym.ArrayIndices.Count - 1 do
  begin
    tp := TTempParam.Create(FPropSym.ArrayIndices[x]);
    FTempParams.AddSymbol(tp);
  end;
end;

function TInfoProperty.GetValue: Variant;
begin
  result := IInfo(Self).Data[0];
end;

procedure TInfoProperty.AssignIndices(const Func: IInfo; FuncParams: TSymbolTable);
var
  paramName: String;
  x: Integer;
  destParam: IInfo;
begin
  if Assigned(FTempParams) then
    for x := 0 to FTempParams.Count - 1 do
    begin
      paramName := FuncParams[x].Name;
      destParam := Func.Parameter[paramName];
      destParam.Data := TTempParam(FTempParams[x]).Data;
    end;
end;

function TInfoProperty.GetData: TData;
var
  func : IInfo;
begin
  if FPropSym.ReadSym is TFuncSymbol then begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.ReadSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);
    AssignIndices(func,TFuncSymbol(FPropSym.ReadSym).Params);
    result := func.Call.Data;
  end
  else if FPropSym.ReadSym is TFieldSymbol then
  begin
    func := SetChild(FProgramInfo,FPropSym.ReadSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.ReadSym).Offset);
    result := func.Data;
{
    fieldSym := TFieldSymbol(FPropSym.ReadSym); // var fieldSym : TFieldSymbol;
    SetLength(result,fieldSym.Typ.Size);
    CopyData(FScriptObj.Data,fieldSym.Offset,result,0,fieldSym.Typ.Size);
}
  end
  else
    raise Exception.Create(CPE_WriteOnlyProperty);
end;

procedure TInfoProperty.SetData(const Value: TData);
var
  func: IInfo;
  paramName: String;
  params: TSymbolTable;
begin
  if FPropSym.WriteSym is TFuncSymbol then
  begin
    func := TInfoFunc.Create(FProgramInfo,FPropSym.WriteSym,FData,FOffset,
      FDataMaster,FScriptObj,FScriptObj.ClassSym);

    params := TFuncSymbol(FPropSym.WriteSym).Params;
    AssignIndices(func,params);

    paramName := params[params.Count - 1].Name;
    func.Parameter[paramName].Data := Value;

    func.Call;
  end
  else if FPropSym.WriteSym is TFieldSymbol then
  begin
    func := SetChild(FProgramInfo,FPropSym.WriteSym.Typ,FScriptObj.Data,
      TFieldSymbol(FPropSym.WriteSym).Offset);
    func.Data := Value;
{
    fieldSym := TFieldSymbol(FPropSym.WriteSym); // var fieldSym : TFieldSymbol;
    CopyData(Value, 0, FScriptObj.Data, fieldSym.Offset, fieldSym.Typ.Size);
}
  end
  else
    raise Exception.Create(CPE_ReadOnlyProperty);
end;

procedure TInfoProperty.SetValue(const Value: Variant);
var dat: TData;
begin
  SetLength(dat,1);
  dat[0] := Value;
  IInfo(Self).Data := dat;
end;

end.


