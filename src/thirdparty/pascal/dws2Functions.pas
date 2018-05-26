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
{    Contributor(s): ______________________________________.           }
{                                                                      }
{    Compatibility:                                                    }
{       [x] D5 (WK)   [x] D6 (WK)    [x] K1 (WK)                       }
{**********************************************************************}

{$I dws2.inc}

unit dws2Functions;

interface

uses
  Classes, dws2Exprs, dws2Symbols, dws2Stack;

type
  TEmptyFunc = class(TInterfacedObject, ICallable)
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure InitSymbol(Symbol: TSymbol);
    procedure InitExpression(Expr: TExprBase);
    function Optimize(Expr: TExprBase): TExprBase;
  end;

  TFunctionPrototype = class(TInterfacedObject)
  protected
    FInfo: TProgramInfo;
  public
    destructor Destroy; override;
    procedure InitSymbol(Symbol: TSymbol); virtual;
    procedure InitExpression(Expr: TExprBase); virtual;
    function Optimize(FuncExpr: TExprBase): TExprBase; virtual;
    property Info: TProgramInfo read FInfo;
  end;

  TAnonymousFunction = class(TFunctionPrototype, IUnknown, ICallable)
    constructor Create(FuncSym: TFuncSymbol);
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure Execute; virtual; abstract;
  end;

  TInternalFunction = class(TFunctionPrototype, IUnknown, ICallable)
  public
    constructor Create(Table: TSymbolTable; FuncName: string;
      FuncParams: array of string; FuncType: string);
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure Execute; virtual; abstract;
  end;
  TInternalFunctionClass = class of TInternalFunction;

  TAnonymousMethod = class(TFunctionPrototype, IUnknown, ICallable)
    constructor Create(MethSym: TMethodSymbol);
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure Execute(var ExternalObject: TObject); virtual; abstract;
  end;

  TInternalMethod = class(TFunctionPrototype, IUnknown, ICallable)
  public
    constructor Create(MethKind: TMethodKind; Attributes: TMethodAttributes;
      bugFix: Integer; methName: string; MethParams: array of string; MethType:
      string; Cls: TClassSymbol; Table: TSymbolTable);
    procedure Call(Caller: TProgram; Func: TFuncSymbol);
    procedure Execute(var ExternalObject: TObject); virtual; abstract;
  end;

  TInternalInitProc = procedure (SystemTable, UnitSyms, UnitTable: TSymbolTable);

  TInternalUnit = class(TObject, IUnknown, IUnit)
  private
    FDependencies: TStrings;
    FInitProcs: TList;
    FRegisteredInternalFunctions: TList;
    FStaticSymbols: Boolean;
    FStaticTable: TStaticSymbolTable; // static symbols
  protected
    procedure SetStaticSymbols(const Value: Boolean);
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function GetDependencies: TStrings;
    function GetUnitName: string;
    procedure InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
    function GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddInternalFunction(rif: Pointer);
    procedure AddInitProc(Proc: TInternalInitProc);
    function InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
    procedure ReleaseStaticSymbols;
    property StaticTable: TStaticSymbolTable read FStaticTable;
    property StaticSymbols: Boolean read FStaticSymbols write SetStaticSymbols;
  end;

procedure RegisterInternalFunction(InternalFunctionClass:
  TInternalFunctionClass; const FuncName: string; const FuncParams: array of
  string; const FuncType: string);

procedure RegisterInternalInitProc(Proc: TInternalInitProc);

var
  InternalUnit: TInternalUnit;

implementation

uses
  SysUtils, dws2Strings;

procedure RegisterInternalInitProc(Proc: TInternalInitProc);
begin
  InternalUnit.AddInitProc(Proc);
end;

type
  TRegisteredInternalFunction = record
    InternalFunctionClass: TInternalFunctionClass;
    FuncName: string;
    FuncParams: array of string;
    FuncType: string
  end;
  PRegisteredInternalFunction = ^TRegisteredInternalFunction;

procedure RegisterInternalFunction(InternalFunctionClass:
  TInternalFunctionClass; const FuncName: string;
  const FuncParams: array of string; const FuncType: string);
var
  i: Integer;
  rif: PRegisteredInternalFunction;
begin
  New(rif);
  rif.InternalFunctionClass := InternalFunctionClass;
  rif.FuncName := FuncName;

  SetLength(rif.FuncParams, Length(FuncParams));

  for i := 0 to Length(FuncParams) - 1 do
    rif.FuncParams[i] := FuncParams[i];
  rif.FuncType := FuncType;

  InternalUnit.AddInternalFunction(rif);
end;

procedure ConvertFuncParams(var Params: TParamList;
  const FuncParams: array of string);
var
  x: Integer;
  c: Char;
begin
  SetLength(Params, Length(FuncParams) div 2);
  x := 0;
  while x < Length(FuncParams) - 1 do
  begin
    with Params[x div 2] do begin
      if Length(FuncParams[x]) > 0 then
        c := FuncParams[x][1]
      else
        c := #0;

      case c of
        '@','&': begin
          IsVarParam := True;
          IsWritable := c = '@';
          ParamName  := Copy(FuncParams[x], 2, MaxInt)
        end;
      else
        IsVarParam := False;
        IsWritable := True; // ignored
        ParamName := FuncParams[x];
      end;
      ParamType := FuncParams[x + 1];
    end;
    Inc(x, 2);
  end;
end;

{ TEmptyFunc }

procedure TEmptyFunc.Call(Caller: TProgram; Func: TFuncSymbol);
begin
end;

procedure TEmptyFunc.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TEmptyFunc.InitExpression(Expr: TExprBase);
begin
end;

function TEmptyFunc.Optimize(Expr: TExprBase): TExprBase;
begin
  Result := Expr;
end;

{ TFunctionPrototype }

destructor TFunctionPrototype.Destroy;
begin
  FInfo.Free;
  inherited;
end;

procedure TFunctionPrototype.InitSymbol(Symbol: TSymbol);
begin
end;

procedure TFunctionPrototype.InitExpression(Expr: TExprBase);
begin
end;

function TFunctionPrototype.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  Result := FuncExpr;
end;

{ TInternalFunction }

constructor TInternalFunction.Create(Table: TSymbolTable;
  FuncName: string; FuncParams: array of string; FuncType: string);
var
  sym: TFuncSymbol;
  Params: TParamList;
begin
  ConvertFuncParams(Params, FuncParams);

  sym := TFuncSymbol.Generate(Table, FuncName, Params, FuncType);
  sym.Params.AddParent(Table);
  sym.Executable := ICallable(Self);
  Table.AddSymbol(sym);

  FInfo := TProgramInfo.Create(sym.Params);
  FInfo.FuncSym := sym;
end;

procedure TInternalFunction.Call(Caller: TProgram; Func: TFuncSymbol);
begin
  FInfo.Caller := Caller;
  Execute;
end;

{ TInternalMethod }

constructor TInternalMethod.Create;
var
  sym: TMethodSymbol;
  Params: TParamList;
begin
  ConvertFuncParams(Params, MethParams);

  sym := TMethodSymbol.Generate(Table, MethKind, Attributes, methName, Params,
    MethType, Cls);
  sym.Params.AddParent(Table);
  sym.Executable := ICallable(Self);

  // Add method to its class
  Cls.AddMethod(sym);

  FInfo := TProgramInfo.Create(sym.Params);
  FInfo.FuncSym := sym;
end;

procedure TInternalMethod.Call(Caller: TProgram; Func: TFuncSymbol);
var
  scriptObj: IScriptObj;
  extObj: TObject;
begin
  FInfo.Caller := Caller;
  scriptObj := Info.Vars['Self'].ScriptObj;

  if Assigned(scriptObj) then
  begin
    FInfo.ScriptObj := scriptObj;
    extObj := scriptObj.ExternalObject;
    try
      Execute(extObj);
    finally
      scriptObj.ExternalObject := extObj;
      FInfo.ScriptObj := nil;
    end;
  end
  else
  begin
    // Class methods or method calls on nil-object-references
    extObj := nil;
    Execute(extObj);
  end;
end;

{ TSimpleFunction }

constructor TAnonymousFunction.Create;
begin
  FInfo := TProgramInfo.Create(FuncSym.Params);
  FInfo.FuncSym := FuncSym;
  FuncSym.Executable := ICallable(Self);
end;

procedure TAnonymousFunction.Call(Caller: TProgram; Func: TFuncSymbol);
begin
  FInfo.Caller := Caller;
  Execute;
end;

{ TAnonymousMethod }

procedure TAnonymousMethod.Call(Caller: TProgram; Func: TFuncSymbol);
var
  scriptObj: IScriptObj;
  extObj: TObject;
begin
  FInfo.Caller := Caller;
  scriptObj := Info.Vars['Self'].ScriptObj;

  if Assigned(scriptObj) then
  begin
    FInfo.ScriptObj := scriptObj;
    extObj := scriptObj.ExternalObject;
    try
      Execute(extObj);
    finally
      scriptObj.ExternalObject := extObj;
    end;
  end
  else
  begin
    // Class methods or method calls on nil-object-references
    extObj := nil;
    Execute(extObj);
  end;
end;

constructor TAnonymousMethod.Create(MethSym: TMethodSymbol);
begin
  FInfo := TProgramInfo.Create(MethSym.Params);
  FInfo.FuncSym := MethSym;
  MethSym.Executable := ICallable(Self);
end;

{ TInternalUnit }

procedure TInternalUnit.AddInitProc(Proc: TInternalInitProc);
begin
  FInitProcs.Add(@Proc);
end;

procedure TInternalUnit.AddInternalFunction(rif: Pointer);
begin
  FRegisteredInternalFunctions.Add(rif);
end;

function TInternalUnit._AddRef: Integer;
begin
  Result := -1;
end;

constructor TInternalUnit.Create;
begin
  FDependencies := TStringList.Create;
  FRegisteredInternalFunctions := TList.Create;
  FInitProcs := TList.Create;
  FStaticSymbols := False;
  FStaticTable := nil;
end;

destructor TInternalUnit.Destroy;
var
  i: Integer;
  rif: PRegisteredInternalFunction;
begin
  ReleaseStaticSymbols;
  FDependencies.Free;
  for i := 0 to FRegisteredInternalFunctions.Count - 1 do
  begin
    rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
    Dispose(rif);
  end;
  FRegisteredInternalFunctions.Free;
  FInitProcs.Free;
  inherited;
end;

function TInternalUnit.GetDependencies: TStrings;
begin
  Result := FDependencies;
end;

function TInternalUnit.GetUnitName: string;
begin
  Result := SYS_INTERNAL;
end;

function TInternalUnit.InitStaticSymbols(SystemTable, UnitSyms: TSymbolTable): Boolean;
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
      FStaticTable := TStaticSymbolTable.Create(staticParent);
      try
        InitUnitTable(SystemTable, UnitSyms, FStaticTable);
      except
        ReleaseStaticSymbols;
        raise;
      end;
    end;
  end;
  Result := Assigned(FStaticTable);
end;

procedure TInternalUnit.ReleaseStaticSymbols;
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

function TInternalUnit.GetUnitTable(SystemTable, UnitSyms: TSymbolTable): TSymbolTable;
begin
  if StaticSymbols and InitStaticSymbols(SystemTable, UnitSyms) then
    Result := TLinkedSymbolTable.Create(FStaticTable)
  else
  begin
    Result := TSymbolTable.Create(SystemTable);
    try
      InitUnitTable(SystemTable, UnitSyms, Result);
    except
      Result.Free;
      raise;
    end;
  end;
end;

procedure TInternalUnit.InitUnitTable(SystemTable, UnitSyms, UnitTable: TSymbolTable);
var
  i: Integer;
  rif: PRegisteredInternalFunction;
begin
  for i := 0 to FInitProcs.Count - 1 do
    TInternalInitProc(FInitProcs[i])(SystemTable, UnitSyms, UnitTable);

  for i := 0 to FRegisteredInternalFunctions.Count - 1 do
  begin
    rif := PRegisteredInternalFunction(FRegisteredInternalFunctions[i]);
    try
      with rif^ do
        InternalFunctionClass.Create(UnitTable, FuncName, FuncParams, FuncType);
    except
      on e: Exception do
        raise
          Exception.CreateFmt('AddInternalFunctions failed on %s'#13#10'%s',
          [rif.FuncName, e.Message]);
    end;
  end;
end;

function TInternalUnit.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := 0;
end;

function TInternalUnit._Release: Integer;
begin
  Result := -1;
end;

procedure TInternalUnit.SetStaticSymbols(const Value: Boolean);
begin
  FStaticSymbols := Value;
  if not FStaticSymbols then
    ReleaseStaticSymbols;
end;

initialization
  InternalUnit := TInternalUnit.Create;
finalization
  InternalUnit.Free;
end.
