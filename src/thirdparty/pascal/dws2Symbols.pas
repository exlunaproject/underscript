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
{    Contributor(s): Andreas Luleich                                   }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Symbols;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, dws2Strings, dws2Stack;

type
  TBaseTypeId = Integer;

const
  SymbolCacheSize = 5; // do not make too large!

  TypIntegerID: TBaseTypeId = 1;
  TypFloatID: TBaseTypeId = 2;
  TypStringID: TBaseTypeId = 3;
  TypBooleanID: TBaseTypeId = 4;
  TypDateTimeID: TBaseTypeId = 5;
  TypVariantID: TBaseTypeId = 6;
  TypConnectorID: TBaseTypeId = 7;

type
  // Base class for all Exprs
  TExprBase = class
  end;

  TSymbol = class;
  TBaseSymbol = class;
  TDataSymbol = class;
  TFuncSymbol = class;
  TMethodSymbol = class;
  TFieldSymbol = class;
  TClassSymbol = class;
  TRecordSymbol = class;
  TParamSymbol = class;
  TVarParamSymbol = class;
  IScriptObj = interface;
  TSymbolTable = class;
  TDimensions = array of Integer;
  TTypeSymbol = class;

  // All functions callable from the script implement this interface
  IExecutable = interface
    ['{8D534D18-4C6B-11D5-8DCB-0000216D9E86}']
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
    function Optimize(FuncExpr: TExprBase): TExprBase;
  end;

  TAddrGeneratorMode = (agmPositive, agmNegative);

  TAddrGenerator = class
  protected
    FDataSize: Integer;
    FLevel: Integer;
    FMode: TAddrGeneratorMode;
    function GetDataSize: Integer;
  public
    constructor Create(Level: Integer; Mode: TAddrGeneratorMode; InitialSize:
      Integer = 0);
    function GetStackAddr(Size: Integer): Integer;
    property DataSize: Integer read GetDataSize;
    property Level: Integer read FLevel;
  end;

  // Named item in the script
  TSymbol = class
  protected
    FName: string;
    FSize: Integer;
    FTyp: TSymbol;
    function GetCaption: string; virtual;
    function GetDescription: string; virtual;
  public
    constructor Create(Name: string; Typ: TSymbol);
    procedure InitData(Data: TData; Offset: Integer); virtual;
    procedure Initialize; virtual;
    function IsCompatible(typSym: TSymbol): Boolean; virtual;
    function BaseType: TTypeSymbol; virtual;
    property Caption: string read GetCaption;
    property Description: string read GetDescription;
    property Name: string read FName write FName;
    property Typ: TSymbol read FTyp write FTyp;
    property Size: Integer read FSize;
  end;

  TSymbolClass = class of TSymbol;

  // All Symbols containing a value
  TValueSymbol = class(TSymbol)
  protected
    function GetCaption: string; override;
    function GetDescription: string; override;
  end;

  // named constant: const x = 123;
  TConstSymbol = class(TValueSymbol)
  protected
    FData: TData;
    function GetCaption: string; override;
    function GetDescription: string; override;
  public
    constructor Create(Name: string; Typ: TSymbol; const Value: Variant); overload;
    constructor Create(Name: string; Typ: TSymbol; Data: TData; Addr: Integer); overload;
    procedure Initialize; override;
    property Data: TData read FData;
  end;

  // variable: var x: Integer;
  TDataSymbol = class(TValueSymbol)
  protected
    FLevel: Integer;
    FStackAddr: Integer;
    function GetDescription: string; override;
  public
    procedure InitData(Data: TData; Offset: Integer); override;
    property Level: Integer read FLevel write FLevel;
    property StackAddr: Integer read FStackAddr write FStackAddr;
  end;

  // parameter: procedure P(x: Integer);
  TParamSymbol = class(TDataSymbol)
  private
    FDefaultValue : TData;
  protected
    function GetDescription: string; override;
  public
    procedure SetDefaultValue(Data: TData; Addr: Integer); overload;
    procedure SetDefaultValue(const Value: Variant); overload;
    property DefaultValue : TData read FDefaultValue;
  end;

  // var parameter: procedure P(var x: Integer)
  TVarParamSymbol = class(TParamSymbol)
  private
    FIsWritable: Boolean;
  protected
    function GetDescription: string; override;
  public
    constructor Create(Name: string; Typ: TSymbol; IsWritable: Boolean = True);
    property IsWritable: Boolean read FIsWritable;
  end;

  // variable with functions for read/write: var x: integer; extern 'type' in 'selector';
  TExternalVarSymbol = class(TValueSymbol)
  private
    FReadFunc: TFuncSymbol;
    FWriteFunc: TFuncSymbol;
  protected
    function GetReadFunc: TFuncSymbol; virtual;
    function GetWriteFunc: TFuncSymbol; virtual;
  public
    destructor Destroy; override;
    property ReadFunc: TFuncSymbol read GetReadFunc write FReadFunc;
    property WriteFunc: TFuncSymbol read GetWriteFunc write FWriteFunc;
  end;

  // Base class for all types
  TTypeSymbol = class(TSymbol)
    function BaseType: TTypeSymbol; override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
  end;

  TFuncKind = (fkFunction, fkProcedure, fkConstructor, fkDestructor);

  // Record used for TFuncSymbol.Generate
  TParamRec = record
    IsVarParam: Boolean;
    IsWritable: Boolean;
    ParamName: string;
    ParamType: string;
    HasDefaultValue: Boolean;
    DefaultValue: TData;
  end;
  TParamList = array of TParamRec;

  // A script function / procedure: procedure X(param: Integer);
  TFuncSymbol = class(TTypeSymbol)
  protected
    FAddrGenerator: TAddrGenerator;
    FExecutable: IExecutable;
    FInternalParams: TSymbolTable;
    FIsForwarded: Boolean;
    FKind: TFuncKind;
    FParams: TSymbolTable;
    FResult: TDataSymbol;
    procedure SetType(const Value: TSymbol); virtual;
    function GetCaption: string; override;
    function GetDescription: string; override;
    function GetLevel: Integer;
    function GetParamSize: Integer;
  public
    constructor Create(Name: string; FuncKind: TFuncKind; FuncLevel: Integer);
    constructor Generate(Table: TSymbolTable; FuncName: string; FuncParams:
      TParamList; FuncType: string);
    destructor Destroy; override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
    procedure AddParam(param: TParamSymbol); virtual;
    procedure GenerateParams(Table: TSymbolTable; FuncParams: TParamList);
    procedure Initialize; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; virtual;
    procedure InitData(Data: TData; Offset: Integer); override;
    property Executable: IExecutable read FExecutable write FExecutable;
    property IsForwarded: Boolean read FIsForwarded write FIsForwarded;
    property Kind: TFuncKind read FKind write FKind;
    property Level: Integer read GetLevel;
    property Params: TSymbolTable read FParams;
    property ParamSize: Integer read GetParamSize;
    property Result: TDataSymbol read FResult;
    property Typ: TSymbol read FTyp write SetType;
    property InternalParams: TSymbolTable read FInternalParams;
  end;

  TMethodKind = (mkProcedure, mkFunction, mkConstructor, mkDestructor,
    mkClassProcedure, mkClassFunction);
  TMethodAttribute = (maVirtual, maOverride, maReintroduce, maAbstract);
  TMethodAttributes = set of TMethodAttribute;

  // A method of a script class: TMyClass = class procedure X(param: String); end;
  TMethodSymbol = class(TFuncSymbol)
  private
    FClassSymbol: TClassSymbol;
    FIsAbstract: Boolean;
    FIsVirtual: Boolean;
    FIsOverride: Boolean;
    FIsOverlap: Boolean;
    FParentMeth: TMethodSymbol;
    FSelfSym: TDataSymbol;
  protected
    function GetIsClassMethod: Boolean;
  public
    constructor Create(Name: string; FuncKind: TFuncKind; ClassSym: TSymbol;
      FuncLevel: Integer = 1); virtual;
    constructor Generate(Table: TSymbolTable; MethKind: TMethodKind; Attributes:
      TMethodAttributes; MethName: string; MethParams: TParamList; MethType: string;
      Cls: TClassSymbol);
    procedure SetOverride(meth: TMethodSymbol);
    procedure SetOverlap(meth: TMethodSymbol);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
    property ClassSymbol: TClassSymbol read FClassSymbol;
    property IsAbstract: Boolean read FIsAbstract write FIsAbstract;
    property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
    property IsOverride: Boolean read FIsOverride;
    property IsOverlap: Boolean read FIsOverlap;
    property IsClassMethod: Boolean read GetIsClassMethod;
    property ParentMeth: TMethodSymbol read FParentMeth;
    property SelfSym: TDataSymbol read FSelfSym write FSelfSym;
  end;

  TNameSymbol = class(TTypeSymbol)
  end;

  // type x = TMyType;
  TAliasSymbol = class(TNameSymbol)
  public
    constructor Create(Name: string; Typ: TTypeSymbol);
    function BaseType: TTypeSymbol; override;
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
  end;

  // integer/string/float/boolean
  TBaseSymbol = class(TNameSymbol)
  protected
    FDefault: Variant;
    FId: TBaseTypeId;
  public
    constructor Create(Name: string; Id: TBaseTypeId; Default: Variant);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
    property Id: TBaseTypeId read FId;
  end;

  IConnectorType = interface;

  IConnector = interface
    ['{8D534D1A-4C6B-11D5-8DCB-0000216D9E86}']
    function ConnectorCaption: string;
    function ConnectorName: string;
    function GetUnit(UnitName: string): IConnectorType;
  end;

  TConnectorArgs = array of TData;

  IConnectorCall = interface
    ['{8D534D1B-4C6B-11D5-8DCB-0000216D9E86}']
    function Call(const Base: Variant; Args: TConnectorArgs): TData;
  end;

  IConnectorMember = interface
    ['{8D534D1C-4C6B-11D5-8DCB-0000216D9E86}']
    function Read(const Base: Variant): TData;
    procedure Write(const Base: Variant; Data: TData);
  end;

  TConnectorParam = record
    IsVarParam: Boolean;
    TypSym: TSymbol;
  end;

  TConnectorParams = array of TConnectorParam;

  IConnectorType = interface
    ['{8D534D1D-4C6B-11D5-8DCB-0000216D9E86}']
    function ConnectorCaption: string;
    function HasMethod(MethodName: string; Params: TConnectorParams; var TypSym:
      TSymbol): IConnectorCall;
    function HasMember(MemberName: string; var TypSym: TSymbol; IsWrite: Boolean): IConnectorMember;
    function HasIndex(PropName: string; Params: TConnectorParams; var TypSym: TSymbol; IsWrite: Boolean): IConnectorCall;
  end;

  TConnectorSymbol = class(TBaseSymbol)
  private
    FConnectorType: IConnectorType;
  public
    constructor Create(Name: string; ConnectorType: IConnectorType);
    procedure InitData(Data: TData; Offset: Integer); override;
    property ConnectorType: IConnectorType read FConnectorType write
      FConnectorType;
  end;

  TArraySymbol = class(TTypeSymbol)
  end;

  // array of FTyp
  TDynamicArraySymbol = class(TArraySymbol)
  protected
    function GetCaption: string; override;
  public
    constructor Create(Name: string; Typ: TSymbol);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(TypSym: TSymbol): Boolean; override;
  end;

  // array [FLowBound..FHighBound] of FTyp
  TStaticArraySymbol = class(TArraySymbol)
  private
    FHighBound: Integer;
    FLowBound: Integer;
    FElementCount: Integer;
  protected
    function GetCaption: string; override;
  public
    constructor Create(Name: string; Typ: TSymbol; LowBound, HighBound: Integer);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(TypSym: TSymbol): Boolean; override;
    property HighBound: Integer read FHighBound;
    property LowBound: Integer read FLowBound;
    property ElementCount: Integer read FElementCount;
  end;

  // Member of a record
  TMemberSymbol = class(TValueSymbol)
  protected
    FRecordSymbol: TRecordSymbol;
    FOffset: Integer;
  public
    procedure InitData(Data: TData; Offset: Integer); override;
    property Offset: Integer read FOffset write FOffset;
    property RecordSymbol: TRecordSymbol read FRecordSymbol write FRecordSymbol;
  end;

  // record member1: Integer; member2: Integer end;
  TRecordSymbol = class(TTypeSymbol)
  private
  protected
    FMembers: TSymbolTable;
    function GetCaption: string; override;
    function GetDescription: string; override;
  public
    constructor Create(Name: string);
    destructor Destroy; override;
    procedure AddMember(Member: TMemberSymbol);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
    property Members: TSymbolTable read FMembers;
  end;

  // Field of a script object
  TFieldSymbol = class(TValueSymbol)
  protected
    FClassSymbol: TClassSymbol;
    FOffset: Integer;
  public
    property Offset: Integer read FOffset;
    property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
  end;

  // property X: Integer read FReadSym write FWriteSym;
  TPropertySymbol = class(TValueSymbol)
  private
    FClassSymbol: TClassSymbol;
    FReadSym: TSymbol;
    FWriteSym: TSymbol;
    FArrayIndices: TSymbolTable;
    FIndexSym: TSymbol;
    FIndexValue: TData;
  protected
    function GetCaption: string; override;
    function GetDescription: string; override;
    function GetReadSym: TSymbol; virtual;
    function GetWriteSym: TSymbol; virtual;
    function GetIsDefault: Boolean; virtual;
    procedure AddParam(Param: TParamSymbol);
  public
    constructor Create(Name: string; Typ: TSymbol);
    destructor Destroy; override;
    procedure GenerateParams(Table: TSymbolTable; FuncParams: TParamList);
    procedure SetIndex(Data: TData; Addr: Integer; Sym: TSymbol);
    property ArrayIndices: TSymbolTable read FArrayIndices;
    property ReadSym: TSymbol read GetReadSym write FReadSym;
    property WriteSym: TSymbol read GetWriteSym write FWriteSym;
    property ClassSymbol: TClassSymbol read FClassSymbol write FClassSymbol;
    property IsDefault: Boolean read GetIsDefault;
    property IndexValue: TData read FIndexValue;
    property IndexSym: TSymbol read FIndexSym;
  end;

  // type X = class of TMyClass;
  TClassOfSymbol = class(TTypeSymbol)
  protected
    function GetCaption: string; override;
  public
    constructor Create(Name: string; Typ: TClassSymbol);
    procedure InitData(Data: TData; Offset: Integer); override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
  end;

  TObjectDestroyEvent = procedure(ExternalObject: TObject) of object;

  // type X = class ... end;
  TClassSymbol = class(TTypeSymbol)
  private
    FClassOfSymbol: TClassOfSymbol;
    FIsAbstract: Boolean;
    FIsForward: Boolean;
    FMembers: TSymbolTable;
    FInstanceSize: Integer;
    FOnObjectDestroy: TObjectDestroyEvent;
    FParent: TClassSymbol;
    FDefaultProperty: TPropertySymbol;
  protected
    function CreateMembersTable: TSymbolTable; virtual;
    function GetDescription: string; override;
  public
    constructor Create(Name: string);
    destructor Destroy; override;
    procedure AddField(Sym: TFieldSymbol);
    procedure AddMethod(Sym: TMethodSymbol);
    procedure AddProperty(Sym: TPropertySymbol);
    procedure InheritFrom(Typ: TClassSymbol);
    procedure InitData(Data: TData; Offset: Integer); override;
    procedure Initialize; override;
    function IsCompatible(typSym: TSymbol): Boolean; override;
    function InstanceSize : Integer; // avoids warning
    property ClassOf: TClassOfSymbol read FClassOfSymbol;
    property IsAbstract: Boolean read FIsAbstract write FIsAbstract;
    property IsForward: Boolean read FIsForward write FIsForward;
    property Members: TSymbolTable read FMembers;
    property OnObjectDestroy: TObjectDestroyEvent read FOnObjectDestroy write FOnObjectDestroy;
    property Parent: TClassSymbol read FParent;
    property DefaultProperty: TPropertySymbol read FDefaultProperty write FDefaultProperty;
  end;

  // nil "class"
  TNilSymbol = class(TTypeSymbol)
  protected
    function GetCaption: string; override;
  public
    constructor Create;
    function IsCompatible(typSym: TSymbol): Boolean; override;
  end;

  // Invisible symbol for units (e. g. for Tdws2Unit)
  TUnitSymbol = class(TNameSymbol)
  private
    FIsTableOwner: Boolean;
    FTable: TSymbolTable;
    FInitialized: Boolean;
  public
    constructor Create(Name: string; Table: TSymbolTable; IsTableOwner: Boolean = False);
    destructor Destroy; override;
    procedure Initialize; override;
    property Table: TSymbolTable read FTable write FTable;
  end;

  // Element of an enumeration type. E. g. "type DummyEnum = (Elem1, Elem2, Elem3);"
  TElementSymbol = class(TConstSymbol)
  private
    FIsUserDef: Boolean;
    FUserDefValue: Integer;
  protected
    function GetDescription: string; override;
  public
    constructor Create(Name: string; Typ: TSymbol; Value: Integer; IsUserDef: Boolean);
    property IsUserDef: Boolean read FIsUserDef;
    property UserDefValue: Integer read FUserDefValue;
  end;

  // Enumeration type. E. g. "type myEnum = (One, Two, Three);"
  TEnumerationSymbol = class(TNameSymbol)
  private
    FElements: TSymbolTable;
  protected
    function GetCaption: string; override;
    function GetDescription: string; override;
  public
    constructor Create(Name: string; BaseType: TTypeSymbol);
    destructor Destroy; override;
    procedure AddElement(Element: TElementSymbol);
    property Elements: TSymbolTable read FElements;
  end;

  IObjectOwner = interface
    procedure ReleaseObject;
  end;

  // A table of symbols connected to other symboltables (property Parents)
  TSymbolTable = class
  private
    FAddrGenerator: TAddrGenerator;
    FCurrHot: Word;
    FHot: array[0..SymbolCacheSize - 1] of TSymbol;
    FSymbols: TList;
    FParents: TList;
    FDestructionList: TList;
    FObjects: TInterfaceList;
    function GetParentCount: Integer;
    function GetParents(Index: Integer): TSymbolTable;
  protected
    procedure ClearHotList;
    function GetSymbol(Index: Integer): TSymbol;
    procedure SetSymbol(Index: Integer; Value: TSymbol);
    function GetCount: Integer;
  public
    constructor Create(Parent: TSymbolTable = nil; AddrGenerator: TAddrGenerator = nil);
    destructor Destroy; override;

    procedure BeforeDestruction; override;
    procedure InsertParent(Index: Integer; Parent: TSymbolTable); virtual;
    function RemoveParent(Parent: TSymbolTable): Integer; virtual;
    function IndexOfParent(Parent: TSymbolTable): Integer;
    procedure MoveParent(CurIndex, NewIndex: Integer);
    procedure ClearParents;
    procedure AddParent(Parent: TSymbolTable);

    function AddSymbol(Sym: TSymbol): Integer;
    function FindLocal(const Name: string): TSymbol; virtual;
    procedure AddToDestructionList(Sym: TSymbol);
    function Remove(Sym: TSymbol): Integer;
    procedure Clear;

    function FindSymbol(const Name: string): TSymbol; virtual;
    function HasSymbol(const Name: string): Boolean;
    procedure ReplaceSymbol(OldSym, NewSym: TSymbol);

    procedure AddObjectOwner(AOwner : IObjectOwner);

    procedure Initialize; virtual;

    property AddrGenerator: TAddrGenerator read FAddrGenerator;
    property Count: Integer read GetCount;
    property Symbols[x: Integer]: TSymbol read GetSymbol write SetSymbol; default;
    property ParentCount: Integer read GetParentCount;
    property Parents[Index: Integer]: TSymbolTable read GetParents;
  end;

  TSymbolTableClass = class of TSymbolTable;

  TStaticSymbolTable = class (TSymbolTable)
  private
    FRefCount: Integer;
    FInitialized: Boolean;
  public
    constructor Create(Parent: TStaticSymbolTable = nil; Reference: Boolean = True);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure InsertParent(Index: Integer; Parent: TSymbolTable); override;
    function RemoveParent(Parent: TSymbolTable): Integer; override;
    procedure _AddRef;
    procedure _Release;
  end;

  TLinkedSymbolTable = class (TSymbolTable)
  private
    FParent: TStaticSymbolTable;
  public
    constructor Create(Parent: TStaticSymbolTable; AddrGenerator: TAddrGenerator = nil);
    destructor Destroy; override;
    function FindLocal(const Name: string): TSymbol; override;
    function FindSymbol(const Name: string): TSymbol; override;
    procedure Initialize; override;
    property Parent: TStaticSymbolTable read FParent;
  end;

  IScriptObj = interface
    ['{8D534D1E-4C6B-11D5-8DCB-0000216D9E86}']
    function GetClassSym: TClassSymbol;
    function GetData: TData;
    procedure SetData(Dat: TData);
    function GetExternalObject: TObject;
    procedure SetExternalObject(Value: TObject);
    property ClassSym: TClassSymbol read GetClassSym;
    property Data: TData read GetData write SetData;
    property ExternalObject: TObject read GetExternalObject write SetExternalObject;
  end;

function IsBaseTypeCompatible(AType, BType: TBaseTypeId): Boolean;

implementation

uses
  SysUtils, dws2Functions, dws2Errors
{$IFDEF WIN32}
  ,Windows
{$ENDIF};

{ TSymbol }

constructor TSymbol.Create(Name: string; Typ: TSymbol);
begin
  FName := Name;
  FTyp := Typ;
  if Assigned(FTyp) then
    FSize := FTyp.FSize
  else
    FSize := 0;
end;

{ TVarSymbol }

function TSymbol.GetCaption: string;
begin
  Result := FName;
end;

function TSymbol.GetDescription: string;
begin
  Result := Caption;
end;

procedure TSymbol.InitData(Data: TData; Offset: Integer);
begin
end;

procedure TSymbol.Initialize;
begin
end;

function TSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := False;
end;

function TSymbol.BaseType: TTypeSymbol;
begin
  Result := nil;
end;

{ TRecordSymbol }

constructor TRecordSymbol.Create;
begin
  inherited Create(Name, nil);
  FMembers := TSymbolTable.Create(nil);
end;

destructor TRecordSymbol.Destroy;
begin
  FMembers.Free;
  inherited;
end;

procedure TRecordSymbol.AddMember(Member: TMemberSymbol);
begin
  Member.RecordSymbol := Self;
  Member.Offset := FSize;
  FSize := FSize + Member.Typ.Size;
  FMembers.AddSymbol(Member);
end;

procedure TRecordSymbol.InitData(Data: TData; Offset: Integer);
var
  x: Integer;
begin
  for x := 0 to FMembers.Count - 1 do
    FMembers[x].InitData(Data, Offset + TMemberSymbol(FMembers[x]).Offset);
end;

function TRecordSymbol.IsCompatible(typSym: TSymbol): Boolean;
var
  x: Integer;
begin
  typSym := typSym.BaseType;
  Result := (typSym is TRecordSymbol) and (FMembers.Count =
    TRecordSymbol(typSym).FMembers.Count);

  x := 0;
  while Result and (x < FMembers.Count) do
  begin
    Result := FMembers[x].Typ.IsCompatible(TRecordSymbol(TypSym).FMembers[x].Typ);
    Inc(x);
  end;
end;

function TRecordSymbol.GetCaption: string;
var
  x: Integer;
begin
  Result := 'record';
  if FMembers.Count > 0 then
  begin
    Result := Result + ' ' + FMembers[0].Typ.Caption;
    for x := 1 to FMembers.Count - 1 do
      Result := Result + ', ' + FMembers[x].Typ.Caption;
  end;
  Result := Result + ' end';
end;

function TRecordSymbol.GetDescription: string;
var
  x: Integer;
begin
  Result := 'record';
  for x := 0 to FMembers.Count - 1 do
    Result := Result + ' ' + FMembers[x].Description;
  Result := Result + ' end';
end;

{ TFuncSymbol }

constructor TFuncSymbol.Create(Name: string; FuncKind: TFuncKind;
  FuncLevel: Integer);
begin
  inherited Create(Name, nil);
  FKind := FuncKind;
  FAddrGenerator := TAddrGenerator.Create(FuncLevel, agmNegative);
  FInternalParams := TSymbolTable.Create(nil, FAddrGenerator);
  FParams := TSymbolTable.Create(FInternalParams, FAddrGenerator);
  FSize := 1;
end;

destructor TFuncSymbol.Destroy;
begin
  FParams.Free;
  FInternalParams.Free;
  FAddrGenerator.Free;
  inherited;
end;

constructor TFuncSymbol.Generate(Table: TSymbolTable; FuncName: string;
  FuncParams: TParamList; FuncType: string);
var
  typSym: TSymbol;
begin
  if FuncType <> '' then
  begin
    Self.Create(FuncName, fkFunction, 1);
    // Set function type
    typSym := Table.FindSymbol(FuncType);
    if not (Assigned(typSym) and (typSym.BaseType <> nil)) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [FuncType]);
    Self.SetType(typSym);
  end
  else
    Self.Create(FuncName, fkProcedure, 1);

  GenerateParams(Table, FuncParams);
end;

procedure TFuncSymbol.AddParam(param: TParamSymbol);
begin
  Params.AddSymbol(param);
end;

procedure TFuncSymbol.SetType;
begin
  FTyp := Value;
  FResult := TDataSymbol.Create(SYS_RESULT, Value);
  FInternalParams.AddSymbol(FResult);
end;

type TAddParamProc = procedure (param: TParamSymbol) of object;

procedure GenerateParams(const Name: String; Table: TSymbolTable; FuncParams: TParamList; AddProc: TAddParamProc);
var
  x: Integer;
  typSym: TSymbol;
  paramSym: TParamSymbol;
begin
  for x := 0 to Length(FuncParams) - 1 do
  begin
    typSym := Table.FindSymbol(FuncParams[x].ParamType);
    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeForParamNotFound, [FuncParams[x].ParamType,
        FuncParams[x].ParamName, Name]);

    if FuncParams[x].IsVarParam then
      paramSym := TVarParamSymbol.Create(FuncParams[x].ParamName, typSym, FuncParams[x].IsWritable)
    else
      paramSym := TParamSymbol.Create(FuncParams[x].ParamName, typSym);

    if FuncParams[x].HasDefaultValue then
      paramSym.SetDefaultValue(FuncParams[x].DefaultValue,0);

    AddProc(paramSym);
  end;
end;

procedure TFuncSymbol.GenerateParams(Table: TSymbolTable; FuncParams: TParamList);
begin
  dws2Symbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

function TFuncSymbol.GetCaption: string;
var
  i: Integer;
  nam : String;
begin
  if Name <> '' then
    nam := Name
  else
    case Kind of
      fkFunction    : nam := 'function ';
      fkProcedure   : nam := 'procedure ';
      fkConstructor : nam := 'constructor ';
      fkDestructor  : nam := 'destructor ';
    end;

  if Params.Count > 0 then
  begin
    Result := Params[0].Typ.Caption;
    for i := 1 to Params.Count - 1 do
      Result := Result + ', ' + Params[i].Typ.Caption;
    Result := '(' + Result + ')';
  end
  else
    Result := '';

  if Typ <> nil then
    Result := nam + Result + ': ' + Typ.Name
  else
    Result := nam + Result;
end;

function TFuncSymbol.GetDescription: string;
var
  i: Integer;
begin
  if Params.Count > 0 then
  begin
    Result := Params.Symbols[0].Description;
    for i := 1 to Params.Count - 1 do
      Result := Result + '; ' + Params.Symbols[i].Description;
    Result := '(' + Result + ')';
  end
  else
    Result := '';
  case FKind of
    fkFunction:
      begin
        if Typ <> nil then
          Result := 'function ' + Name + Result + ': ' + Typ.Name
        else
          Result := 'function ' + Name + Result + ': ???';
      end;
    fkProcedure: Result := 'procedure ' + Name + Result;
    fkConstructor: Result := 'constructor ' + Name + Result;
    fkDestructor: Result := 'destructor ' + Name + Result;
  else
    Assert(False)
  end;
end;

procedure TFuncSymbol.Initialize;
begin
  inherited;
  FInternalParams.Initialize;
  if Assigned(FExecutable) then
    FExecutable.InitSymbol(Self)
  else if Level >= 0 then
    raise Exception.CreateFmt(CPE_ForwardNotImplemented, [Name]);
end;

function TFuncSymbol.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  if Assigned(FExecutable) then
    Result := FExecutable.Optimize(FuncExpr)
  else
    Result := FuncExpr;
end;

function TFuncSymbol.GetLevel: Integer;
begin
  Result := FAddrGenerator.Level;
end;

function TFuncSymbol.GetParamSize: Integer;
begin
  Result := FAddrGenerator.DataSize;
end;

function TFuncSymbol.IsCompatible(typSym: TSymbol): Boolean;
var funcSym : TFuncSymbol;
begin
  typSym := typSym.BaseType;
  if typSym is TNilSymbol then
    Result := True
  else if Size <> typSym.Size then
    Result := False
  else begin
    Result := False;
    if not (typSym is TFuncSymbol) then
      Exit;
    funcSym := TFuncSymbol(typSym);
    if (Kind <> funcSym.Kind) or (Params.Count <> funcSym.Params.Count) then
      Exit;
    // TODO : Compare Params
    Result := True;
  end;
end;

procedure TFuncSymbol.InitData(Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := nilIntf;
end;

{ TMethodSymbol }

constructor TMethodSymbol.Create(Name: string; FuncKind: TFuncKind;
  ClassSym: TSymbol; FuncLevel: Integer);
begin
  inherited Create(Name, FuncKind, FuncLevel);
  if ClassSym is TClassSymbol then
  begin
    // Method
    FClassSymbol := TClassSymbol(ClassSym);
    FSelfSym := TDataSymbol.Create(SYS_SELF, ClassSym);
    FInternalParams.AddSymbol(FSelfSym);
    FSize := 2; // code + data
  end
  else
    // Class function -> self is "class of"
    FClassSymbol := TClassSymbol(ClassSym.Typ);
  FParams.AddParent(FClassSymbol.Members);
end;

constructor TMethodSymbol.Generate(Table: TSymbolTable; MethKind: TMethodKind;
  Attributes: TMethodAttributes; MethName: string; MethParams: TParamList;
  MethType: string; Cls: TClassSymbol);
var
  typSym: TSymbol;
  meth: TSymbol;
begin
  // Check if name is already used
  meth := Cls.Members.FindSymbol(MethName);
  if meth is TFieldSymbol then
    raise Exception.CreateFmt(CPE_FieldExists, [MethName])
  else if meth is TPropertySymbol then
    raise Exception.CreateFmt(CPE_PropertyExists, [MethName])
  else if meth is TMethodSymbol then
  begin
    if TMethodSymbol(meth).ClassSymbol = Cls then
      raise Exception.CreateFmt(CPE_MethodExsists, [MethName]);
  end;

  // Initialize MethodSymbol
  case MethKind of
    mkConstructor:
      Create(MethName, fkConstructor, Cls);
    mkDestructor:
      Create(MethName, fkDestructor, Cls);
    mkProcedure:
      Create(MethName, fkProcedure, Cls);
    mkFunction:
      Create(MethName, fkFunction, Cls);
    mkClassProcedure:
      Create(MethName, fkProcedure, Cls.ClassOf);
    mkClassFunction:
      Create(MethName, fkFunction, Cls.ClassOf);
  end;

  // Set Resulttype
  if MethType <> '' then
  begin
    if Kind <> fkFunction then
      raise Exception.Create(CPE_NoResultTypeRequired);

    typSym := Table.FindSymbol(MethType);
    if not Assigned(typSym) then
      raise Exception.CreateFmt(CPE_TypeIsUnknown, [MethType]);
    SetType(typSym);
  end;

  if (Kind = fkFunction) and (MethType = '') then
    raise Exception.Create(CPE_ResultTypeExpected);

  GenerateParams(Table, MethParams);

  if Assigned(meth) then
    SetOverlap(TMethodSymbol(meth));

  if Attributes = [maVirtual] then
    FIsVirtual := True
  else if Attributes = [maVirtual, maAbstract] then
  begin
    FIsVirtual := True;
    FIsAbstract := True;
  end
  else if Attributes = [maOverride] then
  begin
    if FIsOverlap then
      SetOverride(TMethodSymbol(meth))
    else
      raise Exception.CreateFmt(CPE_CanNotOverride, [Name]);
  end
  else if Attributes = [maReintroduce] then
  else if Attributes = [] then
  else
    raise Exception.Create(CPE_InvalidArgCombination);
end;

function TMethodSymbol.GetIsClassMethod: Boolean;
begin
  Result := not Assigned(FSelfSym);
end;

procedure TMethodSymbol.InitData(Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  inherited;
  if Size = 2 then
    Data[Offset + 1] := nilIntf;
end;

function TMethodSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := inherited IsCompatible(typSym);
end;

procedure TMethodSymbol.SetOverlap(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  FIsOverride := False;
  FIsOverlap := True;
end;

procedure TMethodSymbol.SetOverride(meth: TMethodSymbol);
begin
  FParentMeth := meth;
  FIsOverride := True;
  FIsVirtual := True;
  FIsOverlap := False;
end;

{ TPropertySymbol }

procedure TPropertySymbol.AddParam(Param: TParamSymbol);
begin
  ArrayIndices.AddSymbol(Param);
end;

constructor TPropertySymbol.Create(Name: string; Typ: TSymbol);
begin
  inherited;
  FArrayIndices := TSymbolTable.Create;
  FIndexValue := nil;
end;

destructor TPropertySymbol.Destroy;
begin
  FArrayIndices.Free;
  inherited;
end;

procedure TPropertySymbol.GenerateParams(Table: TSymbolTable; FuncParams: TParamList);
begin
  dws2Symbols.GenerateParams(Name,Table,FuncParams,AddParam);
end;

function TPropertySymbol.GetCaption: string;
begin
  Result := GetDescription;
end;

function TPropertySymbol.GetDescription: string;
var
  x: Integer;
  indexDesc: string;
begin
  if ArrayIndices.Count > 0 then
  begin
    indexDesc := '';
    for x := 0 to ArrayIndices.Count - 1 do
    begin
      if x = 0 then
        indexDesc := Format('%s: %s', [ArrayIndices[x].Name, ArrayIndices[x].Typ.Name])
      else
        indexDesc := Format('%s, %s: %s', [indexDesc, ArrayIndices[x].Name, ArrayIndices[x].Typ.Name]);
    end;
    Result := Format('property %s[%s]: %s', [Name, indexDesc, Typ.Name])
  end
  else
    Result := Format('property %s: %s', [Name, Typ.Name]);

  if Assigned(FReadSym) then
    Result := Result + ' read ' + FReadSym.Name;

  if Assigned(FWriteSym) then
    Result := Result + ' write ' + FWriteSym.Name;

  if ClassSymbol.DefaultProperty = Self then
    Result := Result + '; default';
end;

function TPropertySymbol.GetIsDefault: Boolean;
begin
  Result := ClassSymbol.DefaultProperty = Self;
end;

function TPropertySymbol.GetReadSym: TSymbol;
begin
  Result := FReadSym;
end;

function TPropertySymbol.GetWriteSym: TSymbol;
begin
  Result := FWriteSym;
end;

procedure TPropertySymbol.SetIndex(Data: TData; Addr: Integer; Sym: TSymbol);
begin
  FIndexSym := Sym;
  SetLength(FIndexValue,FIndexSym.Size);
  CopyData(Data, Addr, FIndexValue, 0, FIndexSym.Size);
end;

{ TClassSymbol }

constructor TClassSymbol.Create;
begin
  inherited Create(Name, nil);
  FSize := 1;
  FMembers := CreateMembersTable;
  FClassOfSymbol := TClassOfSymbol.Create('class of ' + Name, Self);
  FMembers.AddSymbol(TAliasSymbol.Create('Self',Self)); // private member ?!
end;

destructor TClassSymbol.Destroy;
begin
  FMembers.Free;
  FClassOfSymbol.Free;
  inherited;
end;

function TClassSymbol.CreateMembersTable: TSymbolTable;
begin
  Result := TSymbolTable.Create(nil);
end;

procedure TClassSymbol.AddField(Sym: TFieldSymbol);
begin
  FMembers.AddSymbol(Sym);
  Sym.FClassSymbol := Self;

  Sym.FOffset := FInstanceSize;
  FInstanceSize := FInstanceSize + Sym.Typ.Size;
end;

procedure TClassSymbol.AddMethod(Sym: TMethodSymbol);
var
  x: Integer;
begin
  FMembers.AddSymbol(Sym);
  sym.FClassSymbol := Self;

  // Check if class is abstract or not
  if Sym.IsAbstract then
    FIsAbstract := True
  else if Sym.IsOverride and Sym.FParentMeth.IsAbstract then
  begin
    FIsAbstract := False;
    for x := 0 to FMembers.Count - 1 do
      if (FMembers[x] is TMethodSymbol) and (TMethodSymbol(FMembers[x]).IsAbstract) then
      begin
        FIsAbstract := True;
        break;
      end;
  end;
end;

procedure TClassSymbol.AddProperty(Sym: TPropertySymbol);
begin
  FMembers.AddSymbol(Sym);
  sym.FClassSymbol := Self;
end;

procedure TClassSymbol.InitData(Data: TData; Offset: Integer);
const
  nilIntf: IUnknown = nil;
begin
  Data[Offset] := IUnknown(nilIntf);
end;

procedure TClassSymbol.Initialize;
var
  x: Integer;
  Err: EClassMethodImplIncompleteError;
begin
  // Check validity of the class declaration

  if FIsForward then
    raise Exception.CreateFmt(CPE_ClassNotCompletelyDefined, [Caption]);

  for x := 0 to FMembers.Count - 1 do
    if FMembers[x] is TMethodSymbol then
    begin
      if not TMethodSymbol(FMembers[x]).IsAbstract then
      begin
        if Assigned(TMethodSymbol(FMembers[x]).FExecutable) then
          TMethodSymbol(FMembers[x]).FExecutable.InitSymbol(FMembers[x])
        else
        begin
          Err := EClassMethodImplIncompleteError.CreateFmt(CPE_MethodNotImplemented,
            [FMembers[x].Caption, TMethodSymbol(FMembers[x]).ClassSymbol.Caption]);
          Err.ClassSymObj := Self;
          raise Err;
        end;
      end;
    end;
end;

procedure TClassSymbol.InheritFrom(Typ: TClassSymbol);
begin
  FMembers.AddParent(Typ.Members);
  FInstanceSize := Typ.InstanceSize;
  FParent := Typ;
end;

function TClassSymbol.IsCompatible(typSym: TSymbol): Boolean;
var
  csym: TClassSymbol;
begin
  Result := False;
  typSym := typSym.BaseType;
  if typSym is TNilSymbol then
    Result := True
  else if typSym is TClassSymbol then
  begin
    csym := TClassSymbol(typSym);
    while csym <> nil do
    begin
      if csym = Self then
      begin
        Result := True;
        exit;
      end;
      csym := csym.Parent;
    end;
  end;
end;

function TClassSymbol.GetDescription: string;
var
  i: Integer;
begin
  if FParent <> nil then
    Result := Name + ' = class (' + FParent.Name + ')'#13#10
  else
    Result := Name + ' = class'#13#10;

  for i := 0 to Members.Count - 1 do
    Result := Result + '   ' + Members.Symbols[i].Description + ';'#13#10;

  Result := Result + 'end';
end;

function TClassSymbol.InstanceSize: Integer;
begin
  Result := FInstanceSize;
end;

{ TNilSymbol }

constructor TNilSymbol.Create;
begin
  inherited Create('', nil);
  FSize := 1;
end;

function TNilSymbol.GetCaption: string;
begin
  Result := 'nil';
end;

function TNilSymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  Result := (TypSym is TClassSymbol) or (TypSym is TNilSymbol);
end;

{ TClassOfSymbol }

constructor TClassOfSymbol.Create;
begin
  inherited Create(Name, Typ);
end;

function TClassOfSymbol.GetCaption: string;
begin
  if Typ <> nil then
    Result := 'class of ' + Typ.Name
  else
    Result := 'class of ???';
end;

procedure TClassOfSymbol.InitData(Data: TData; Offset: Integer);
begin
  Data[Offset] := '';
end;

function TClassOfSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  Result := (typSym is TClassOfSymbol) and Typ.IsCompatible(typSym.Typ);
end;

function IsBaseTypeCompatible(AType, BType: TBaseTypeId): Boolean;
const
{(*}
  compatiblityMask: array[1..7, 1..7] of Boolean =
  (
   //int    flt    str    bool   dt     var   conn
    (true,  false, false, false, false, true, true), // int
    (false, true,  false, false, false, true, true), // flt
    (false, false, true,  false, false, true, true), // str
    (false, false, false, true,  false, true, true), // bool
    (false, false, false, false, true,  true, true), // dt
    (true,  true,  true,  true,  true,  true, true), // var
    (true,  true,  true,  true,  true,  true, true)  // conn
  );
{*)}
begin
  Result := compatiblityMask[AType, BType];
end;

{ TBaseSymbol }

constructor TBaseSymbol.Create(Name: string; Id: Integer; Default: Variant);
begin
  inherited Create(Name, nil);
  FId := Id;
  FDefault := Default;
  FSize := 1;
end;

procedure TBaseSymbol.InitData(Data: TData; Offset: Integer);
begin
  VarCopy(Data[Offset], FDefault);
end;

function TBaseSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  typSym := typSym.BaseType;
  if typSym is TEnumerationSymbol then
    typSym := TEnumerationSymbol(typSym).Typ.BaseType;
  Result := (typSym is TBaseSymbol) and
    IsBaseTypeCompatible(Self.FId, TBaseSymbol(typSym).FId);
end;

{ TConnectorSymbol }

constructor TConnectorSymbol.Create(Name: string; ConnectorType: IConnectorType);
begin
  inherited Create(Name, typConnectorID, Null);
  FConnectorType := ConnectorType;
end;

procedure TConnectorSymbol.InitData(Data: TData; Offset: Integer);
begin
  VarClear(Data[Offset]);
end;

{ TValueSymbol }

function TValueSymbol.GetCaption: string;
begin
  Result := FName + ': ' + FTyp.Caption;
end;

function TValueSymbol.GetDescription: string;
begin
  Result := FName + ': ' + FTyp.Description;
end;

{ TConstSymbol }

constructor TConstSymbol.Create(Name: string; Typ: TSymbol; const Value: Variant);
begin
  inherited Create(Name, Typ);
  SetLength(FData, 1);
  VarCopy(FData[0], Value);
end;

constructor TConstSymbol.Create(Name: string; Typ: TSymbol; Data: TData;
  Addr: Integer);
begin
  inherited Create(Name, Typ);
  SetLength(FData, Typ.Size);
  CopyData(Data, Addr, FData, 0, Typ.Size);
end;

function TConstSymbol.GetCaption: string;
begin
  Result := 'const ' + inherited GetCaption;
end;

function TConstSymbol.GetDescription: string;
begin
  if VarType(FData[0]) = varError then
    Result := 'const ' + inherited GetDescription + ' = [varError]'
  else
    Result := 'const ' + inherited GetDescription + ' = ' + VarToStr(FData[0]);
end;

procedure TConstSymbol.Initialize;
begin
end;

{ TMemberSymbol }

procedure TMemberSymbol.InitData(Data: TData; Offset: Integer);
begin
  Typ.InitData(Data, Offset);
end;

{ TDataSymbol }

function TDataSymbol.GetDescription: string;
begin
  if Assigned(FTyp) then
    Result := FName + ': ' + FTyp.Name
  else
    Result := FName;
end;

procedure TDataSymbol.InitData(Data: TData; Offset: Integer);
begin
  Typ.InitData(Data, Offset);
end;

{ TParamSymbol }

function TParamSymbol.GetDescription: string;
begin
  if Typ <> nil then
    Result := Name + ': ' + Typ.Name
  else
    Result := Name + ': ???';

  // Has a default parameter. Format display of param to show it.
  if Length(FDefaultValue) > 0 then
    if VarType(FDefaultValue[0]) = varString then    // does not support OLE types
      Result := Result + ' = ''' + VarToStr(FDefaultValue[0]) + ''''  // put quotes around value
    else
      Result := Result + ' = ' + VarToStr(FDefaultValue[0]);
end;

procedure TParamSymbol.SetDefaultValue(Data: TData; Addr: Integer);
begin
  SetLength(FDefaultValue, Typ.Size);
  CopyData(Data, Addr, FDefaultValue, 0, Typ.Size);
end;

procedure TParamSymbol.SetDefaultValue(const Value: Variant);
begin
  Assert(Typ.Size = 1);
  SetLength(FDefaultValue, 1);
  VarCopy(FDefaultValue[0], Value);
end;

{ TVarParamSymbol }

constructor TVarParamSymbol.Create(Name: string; Typ: TSymbol; IsWritable: Boolean);
begin
  inherited Create(Name, Typ);
  FSize := 1;
  FIsWritable := IsWritable;
end;

function TVarParamSymbol.GetDescription: string;
begin
  if FIsWritable then
    Result := 'var '
  else
    Result := 'const ';
  Result := Result + inherited GetDescription;  
end;

{ TSymbolTable }

function TSymbolTable.AddSymbol(Sym: TSymbol): Integer;
begin
  Result := FSymbols.Add(sym);
  if (sym is TDataSymbol) and (FAddrGenerator <> nil) then
  begin
    TDataSymbol(sym).Level := FAddrGenerator.Level;
    TDataSymbol(sym).StackAddr := FAddrGenerator.GetStackAddr(sym.Size);
  end;
end;

constructor TSymbolTable.Create(Parent: TSymbolTable; AddrGenerator: TAddrGenerator);
begin
  FSymbols := TList.Create;
  FParents := TList.Create;
  FDestructionList := TList.Create;
  FAddrGenerator := AddrGenerator;
  if Assigned(Parent) then
    AddParent(Parent);
end;

destructor TSymbolTable.Destroy;
var
  x: Integer;
begin
  FObjects.Free;
  for x := 0 to FSymbols.Count - 1 do
    TSymbol(FSymbols[x]).Free;
  for x := 0 to FDestructionList.Count - 1 do
    TSymbol(FDestructionList[x]).Free;
  FSymbols.Free;
  FDestructionList.Free;
  ClearParents;
  FParents.Free;
  inherited;
end;

function TSymbolTable.FindLocal(const Name: string): TSymbol;
var
  x: Integer;
begin
  Result := nil;
  // Lookup in Hot-list
  for x := 0 to SymbolCacheSize - 1 do
    if Assigned(FHot[x]) and (SameText(FHot[x].Name, Name)) then
    begin
      Result := FHot[x];
      Exit;
    end;

  // Lookup in symbol table
  for x := FSymbols.Count - 1 downto 0 do
    if SameText(TSymbol(FSymbols[x]).Name, Name) then
    begin
      Result := FSymbols[x];
      FHot[FCurrHot] := Result;
      Inc(FCurrHot);
      FCurrHot := FCurrHot mod SymbolCacheSize;
      Exit;
    end;
end;

function TSymbolTable.FindSymbol(const Name: string): TSymbol;
var
  x: Integer;
begin
  // Find Symbol in the local List
  Result := FindLocal(Name);
  if Assigned(Result) then
    Exit;

  // Find Symbol in all parent lists
  x := 0;
  while not Assigned(Result) and (x < ParentCount) do
  begin
    Result := Parents[x].FindSymbol(Name);
    Inc(x);
  end;
end;

function TSymbolTable.GetCount: Integer;
begin
  Result := FSymbols.Count;
end;

function TSymbolTable.GetSymbol(Index: Integer): TSymbol;
begin
  Result := TSymbol(FSymbols[Index])
end;

procedure TSymbolTable.Initialize;
var
  x: Integer;
begin
  for x := 0 to FSymbols.Count - 1 do
    TSymbol(FSymbols[x]).Initialize;
end;

function TSymbolTable.Remove(Sym: TSymbol): Integer;
begin
  ClearHotList;
  Result := FSymbols.Remove(Sym);
end;

procedure TSymbolTable.AddParent(Parent: TSymbolTable);
begin
  InsertParent(ParentCount,Parent);
end;

procedure TSymbolTable.SetSymbol(Index: Integer; Value: TSymbol);
begin
  FSymbols[Index] := Value;
end;

procedure TSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
begin
  FParents.Insert(Index,Parent);
end;

function TSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
  Result := FParents.Remove(Parent);
end;

procedure TSymbolTable.ClearParents;
begin
  while ParentCount > 0 do
    RemoveParent(Parents[0]);
end;

procedure TSymbolTable.ReplaceSymbol(OldSym, NewSym: TSymbol);
var
  i: Integer;
begin
  ClearHotList;
  i := FSymbols.IndexOf(OldSym);
  if (i >= 0) then
    FSymbols[i] := newSym
  else if Assigned(newSym) then
    AddSymbol(newSym);
  OldSym.Free;
end;

procedure TSymbolTable.Clear;
begin
  ClearHotList;
  FSymbols.Clear;
end;

function TSymbolTable.HasSymbol(const Name: string): Boolean;
begin
  Result := Assigned(FindSymbol(Name));
end;

procedure TSymbolTable.AddToDestructionList(sym: TSymbol);
begin
  FDestructionList.Add(sym);
end;

procedure TSymbolTable.ClearHotList;
var
  x: Integer;
begin
  for x := 0 to SymbolCacheSize - 1 do
    FHot[x] := nil;
  FCurrHot := 0;
end;

function TSymbolTable.GetParentCount: Integer;
begin
  Result := FParents.Count;
end;

function TSymbolTable.GetParents(Index: Integer): TSymbolTable;
begin
  Result := TSymbolTable(FParents[Index]);
end;

function TSymbolTable.IndexOfParent(Parent: TSymbolTable): Integer;
begin
  Result := FParents.IndexOf(Parent);
end;

procedure TSymbolTable.AddObjectOwner(AOwner: IObjectOwner);
begin
  if not Assigned(FObjects) then
    FObjects := TInterfaceList.Create;
  FObjects.Add(AOwner);
end;

procedure TSymbolTable.BeforeDestruction;
var
  O : IObjectOwner;
begin
  if Assigned(FObjects) then begin
    while FObjects.Count > 0 do
    begin
      O := IObjectOwner(FObjects[0]);
      FObjects.Delete(0);
      O.ReleaseObject;
    end;
  end;
  inherited;
end;

procedure TSymbolTable.MoveParent(CurIndex, NewIndex: Integer);
begin
  FParents.Move(CurIndex,NewIndex);
end;

{ TExternalVarSymbol }

destructor TExternalVarSymbol.Destroy;
begin
  FReadFunc.Free;
  FWriteFunc.Free;
  inherited;
end;

function TExternalVarSymbol.GetReadFunc: TFuncSymbol;
begin
  Result := FReadFunc;
end;

function TExternalVarSymbol.GetWriteFunc: TFuncSymbol;
begin
  Result := FWriteFunc;
end;

{ TUnitSymbol }

constructor TUnitSymbol.Create(Name: string; Table: TSymbolTable;
  IsTableOwner: Boolean = False);
begin
  inherited Create(Name, nil);
  FIsTableOwner := IsTableOwner;
  FTable := Table;
  FInitialized := False;
end;

destructor TUnitSymbol.Destroy;
begin
  if FIsTableOwner then
    FTable.Free;
  inherited;
end;

procedure TUnitSymbol.Initialize;
begin
  if not FInitialized then
  begin
    FTable.Initialize;
    FInitialized := True;
  end;
end;

{ TAddrGenerator }

constructor TAddrGenerator.Create;
begin
  FLevel := Level;
  FMode := Mode;
  FDataSize := InitialSize;
end;

function TAddrGenerator.GetDataSize: Integer;
begin
  if FMode = agmPositive then
    Result := FDataSize
  else
    Result := -FDataSize;
end;

function TAddrGenerator.GetStackAddr(Size: Integer): Integer;
begin
  if FMode = agmPositive then
  begin
    Result := FDataSize;
    Inc(FDataSize, Size);
  end
  else
  begin
    Dec(FDataSize, Size);
    Result := FDataSize;
  end;
end;

{ TDynamicArraySymbol }

constructor TDynamicArraySymbol.Create(Name: string; Typ: TSymbol);
begin
  inherited Create(Name, Typ);
  FSize := 1;
end;

function TDynamicArraySymbol.GetCaption: string;
begin
  Result := 'array of ' + FTyp.Caption
end;

procedure TDynamicArraySymbol.InitData(Data: TData; Offset: Integer);
begin
  Data[Offset] := Null; // ADR
end;

function TDynamicArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  Result := (TypSym is TDynamicArraySymbol) and
    (Typ.IsCompatible(TypSym.Typ) or (TypSym.Typ is TNilSymbol));
end;

{ TStaticArraySymbol }

constructor TStaticArraySymbol.Create(Name: string; Typ: TSymbol; LowBound, HighBound: Integer);
begin
  inherited Create(Name, Typ);
  FLowBound := LowBound;
  FHighBound := HighBound;
  FElementCount := HighBound - LowBound + 1;
  FSize := FElementCount * Typ.Size;
end;

procedure TStaticArraySymbol.InitData(Data: TData; Offset: Integer);
var
  x: Integer;
begin
  for x := 1 to ElementCount do
  begin
    Typ.InitData(Data, Offset);
    Inc(Offset, Typ.BaseType.Size);
  end;
end;

function TStaticArraySymbol.IsCompatible(TypSym: TSymbol): Boolean;
begin
  TypSym := TypSym.BaseType;
  Result := (TypSym is TStaticArraySymbol)
    and (ElementCount = TStaticArraySymbol(TypSym).ElementCount)
    and Typ.IsCompatible(TypSym.Typ);
end;

function TStaticArraySymbol.GetCaption;
begin
  Result := 'array [' + IntToStr(FLowBound) + '..' + IntToStr(FHighBound) + '] of ';
  if Assigned(FTyp) then
    Result := Result + FTyp.Caption
  else
    Result := Result + '<unknown>';
end;

{ TElementSymbol }

constructor TElementSymbol.Create(Name: string; Typ: TSymbol;
  Value: Integer; IsUserDef: Boolean);
begin
  inherited Create(Name, Typ, Value);
  FIsUserDef := IsUserDef;
  FUserDefValue := Value;
end;

function TElementSymbol.GetDescription: string;
begin
  if FIsUserDef then
    Result := FName + ' = ' + IntToStr(Data[0])
  else
    Result := FName;  //inherited GetDescription; <= can cause stack overflow
end;

{ TEnumerationSymbol }

procedure TEnumerationSymbol.AddElement(Element: TElementSymbol);
begin
  FElements.AddSymbol(Element);
end;

constructor TEnumerationSymbol.Create(Name: string; BaseType: TTypeSymbol);
begin
  inherited Create(Name, BaseType);
  FElements := TSymbolTable.Create;
end;

destructor TEnumerationSymbol.Destroy;
begin
  FElements.Clear;
  FElements.Free;
  inherited;
end;

function TEnumerationSymbol.GetCaption: string;
begin
  Result := Name;
end;

function TEnumerationSymbol.GetDescription: string;
var
  x: Integer;
begin
  Result := '(';
  for x := 0 to FElements.Count - 1 do
  begin
    if x <> 0 then
      Result := Result + ', ';
    Result := Result + FElements[x].GetDescription;
  end;
  Result := Result + ')';
end;

{ TStaticSymbolTable }

constructor TStaticSymbolTable.Create(Parent: TStaticSymbolTable; Reference: Boolean);
begin
  inherited Create(Parent);
  FInitialized := False;
  FRefCount := 0;
  if Reference then
    _AddRef;
end;

procedure TStaticSymbolTable._AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

procedure TStaticSymbolTable._Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
    Free;
end;

procedure TStaticSymbolTable.InsertParent(Index: Integer; Parent: TSymbolTable);
var
  staticSymbols: TStaticSymbolTable;
begin
  // accept only static parents
  if Parent is TLinkedSymbolTable then
    staticSymbols := TLinkedSymbolTable(Parent).Parent
  else if Parent is TStaticSymbolTable then
    staticSymbols := TStaticSymbolTable(Parent)
  else
    staticSymbols := nil;

  if Assigned(StaticSymbols) then
  begin
    staticSymbols._AddRef;
    inherited InsertParent(Index, staticSymbols);
  end
  else
    raise Exception.Create(CPE_NoStaticSymbols);
end;

function TStaticSymbolTable.RemoveParent(Parent: TSymbolTable): Integer;
begin
  Result := inherited RemoveParent(Parent);
  (Parent as TStaticSymbolTable)._Release;
end;

destructor TStaticSymbolTable.Destroy;
begin
  Assert(FRefCount = 0);
  inherited;
end;

procedure TStaticSymbolTable.Initialize;
begin
  if not FInitialized then
  begin
    inherited;
    FInitialized := True;
  end;
end;

{ TLinkedSymbolTable }

constructor TLinkedSymbolTable.Create(Parent: TStaticSymbolTable;
  AddrGenerator: TAddrGenerator);
begin
  inherited Create(nil,AddrGenerator);
  FParent := Parent;
  FParent._AddRef;
end;

destructor TLinkedSymbolTable.Destroy;
begin
  FParent._Release;
  inherited;
end;

function TLinkedSymbolTable.FindLocal(const Name: string): TSymbol;
begin
  Result := FParent.FindLocal(Name);
  if not Assigned(Result) then
    Result := inherited FindLocal(Name);
end;

function TLinkedSymbolTable.FindSymbol(const Name: string): TSymbol;
begin
  Result := FParent.FindSymbol(Name);
  if not Assigned(Result) then
    Result := inherited FindSymbol(Name);
end;

procedure TLinkedSymbolTable.Initialize;
begin
  FParent.Initialize;
  inherited;
end;

{ TAliasSymbol }

function TAliasSymbol.BaseType: TTypeSymbol;
begin
  Result := Typ.BaseType;
end;

constructor TAliasSymbol.Create(Name: string; Typ: TTypeSymbol);
begin
  Assert(Assigned(Typ));
  inherited Create(Name,Typ);
end;

procedure TAliasSymbol.InitData(Data: TData; Offset: Integer);
begin
  BaseType.InitData(Data, Offset);
end;

function TAliasSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := BaseType.IsCompatible(typSym);
end;

{ TTypeSymbol }

function TTypeSymbol.BaseType: TTypeSymbol;
begin
  Result := Self;
end;

function TTypeSymbol.IsCompatible(typSym: TSymbol): Boolean;
begin
  Result := BaseType = typSym.BaseType;
end;

end.



