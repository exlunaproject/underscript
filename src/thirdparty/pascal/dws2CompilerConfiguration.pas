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
{                    Andreas Luleich                                   }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2CompilerConfiguration;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, SysUtils, dws2Exprs, dws2Symbols, dws2Tokenizer, dws2Errors,
  dws2Strings, dws2Functions, dws2Stack;

type
  TCompilerOption = (coOptimize);
  TCompilerOptions = set of TCompilerOption;

  TIncludeEvent = procedure(const scriptName: string; var scriptSource: string) of object;

  TConfiguration = class(TPersistent)
  private
    FMaxDataSize: Integer;
    FCompilerOptions: TCompilerOptions;
    FAdapter: IAdapter;
    FUnits: TStrings;
    FConnectors: TStrings;
    FScriptPaths: TStrings;
    FSystemTable: TSymbolTable;
    FTimeout: Integer;
    FOnInclude: TIncludeEvent;
    FStackChunkSize: Integer;
    FMemoryleakWarning: Boolean;
    procedure InitSystemTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetScriptPaths(const Value: TStrings);
    property Adapter: IAdapter read FAdapter write FAdapter;
    property Connectors: TStrings read FConnectors write FConnectors;
    property OnInclude: TIncludeEvent read FOnInclude write FOnInclude;
    property SystemTable: TSymbolTable read FSystemTable write FSystemTable;
    property Units: TStrings read FUnits write FUnits;
  published
    property CompilerOptions: TCompilerOptions read FCompilerOptions write FCompilerOptions;
    property MaxDataSize: Integer read FMaxDataSize write FMaxDataSize;
    property MemoryleakWarning: Boolean read FMemoryleakWarning write FMemoryleakWarning;
    property ScriptPaths: TStrings read FScriptPaths write SetScriptPaths;
    property Timeout: Integer read FTimeout write FTimeout;
    property StackChunkSize: Integer read FStackChunkSize write FStackChunkSize default C_DefaultStackChunkSize;
  end;

implementation

{ TConfiguration }

procedure TConfiguration.Assign(Source: TPersistent);
begin
  if Source is TConfiguration then
  begin
    FCompilerOptions := TConfiguration(Source).CompilerOptions;
    FMaxDataSize := TConfiguration(Source).MaxDataSize;
    FMemoryleakWarning := TConfiguration(Source).MemoryleakWarning;
    FScriptPaths.Assign(TConfiguration(Source).ScriptPaths);
    FTimeout := TConfiguration(Source).Timeout;
  end
  else
    inherited;
end;

constructor TConfiguration.Create;
begin
  FSystemTable := TSymbolTable.Create(nil);
  FConnectors := TStringList.Create;
  FScriptPaths := TStringList.Create;
  FUnits := TStringList.Create;
  InitSystemTable;
  FUnits.AddObject(SYS_INTERNAL, Pointer(IUnit(InternalUnit)));
  FStackChunkSize := C_DefaultStackChunkSize;
end;

destructor TConfiguration.Destroy;
begin
  inherited;
  FSystemTable.Free;
  FConnectors.Free;
  FScriptPaths.Free;
  FUnits.Free;
end;

procedure TConfiguration.InitSystemTable;
var
  clsObject, clsException, clsDelphiException: TClassSymbol;
  meth: TMethodSymbol;
begin
  // Create base data types
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_BOOLEAN, typBooleanID, false));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_DATETIME, typDateTimeID,
    VarAsType(0.0, varDate)));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_FLOAT, typFloatID, 0.0));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_INTEGER, typIntegerID, VarAsType(0,varInteger)));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_STRING, typStringID, ''));
  SystemTable.AddSymbol(TBaseSymbol.Create(SYS_VARIANT, typVariantID, Unassigned));

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
  // Add Method Free
  TObjectFreeMethod.Create(mkProcedure, [], 0, SYS_TOBJECT_FREE, [], '', clsObject,
    SystemTable);
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

{ TObjectFreeMethod }

procedure TObjectFreeMethod.Execute(var ExternalObject: TObject);
begin
  if Assigned(Info.Obj) then
    Info.Method[SYS_TOBJECT_DESTROY].Call;
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
      FMsgs.AddSyntaxStop(FTok.HotPos, CPE_ArrayBracketRightExpected);

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
  Adapter: IAdapter; MaxDataSize: Integer; StackChunkSize: Integer): TProgram;
begin
  Result := TProgram.Create(SystemTable, Adapter, MaxDataSize, StackChunkSize);
end;

end.

