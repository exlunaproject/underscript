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
{    Contributor(s):                                                   }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2StringResult;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, dws2Exprs, dws2Symbols, dws2Comp;

type
  Tdws2StringResult = class(Tdws2Result)
  private
    FStr: string;
  public
    procedure AddStr(const Str: string);
    procedure SetStr(const Str: string);
    function ReadLn: string;
    function ReadChar: string;
    property Str: string read FStr;
  end;

  TChangeStringEvent = procedure (Result: Tdws2StringResult; const Str: string) of object;
  TReadStringEvent = procedure (Result: Tdws2StringResult; var Str: string) of object;

  Tdws2StringResultType = class(Tdws2ResultType)
  private
    FOnAddString: TChangeStringEvent;
    FOnSetString: TChangeStringEvent;
    FOnReadLn: TReadStringEvent;
    FOnReadChar: TReadStringEvent;
  public
    function CreateProgResult: Tdws2Result; override;
  published
    property OnAddString: TChangeStringEvent read FOnAddString write FOnAddString;
    property OnSetString: TChangeStringEvent read FOnSetString write FOnSetString;
    property OnReadLn: TReadStringEvent read FOnReadLn write FOnReadLn;
    property OnReadChar: TReadStringEvent read FOnReadChar write FOnReadChar;
  end;

  Tdws2StringsUnit = class(Tdws2UnitComponent)
  protected
    procedure AddUnitSymbols(SymbolTable: TSymbolTable); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  dws2Functions, dws2Strings;

type
  TWriteFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TWriteLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TWriteAllFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadCharFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadLnFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;

  TReadAllFunction = class(TInternalFunction)
  public
    procedure Execute; override;
  end;


{ Tdws2StringResult }

procedure Tdws2StringResult.AddStr(const Str: string);
begin
  FStr := FStr + Str;
  if Assigned(Tdws2StringResultType(ResultType).OnAddString) then
    Tdws2StringResultType(ResultType).OnAddString(Self, Str)
end;

procedure Tdws2StringResult.SetStr(const Str: string);
begin
  FStr := Str;
  if Assigned(Tdws2StringResultType(ResultType).OnSetString) then
    Tdws2StringResultType(ResultType).OnSetString(Self, Str)
end;

function Tdws2StringResult.ReadLn: string;
begin
  if Assigned(Tdws2StringResultType(ResultType).OnReadLn) then
    Tdws2StringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
end;

function Tdws2StringResult.ReadChar: string;
begin
  if Assigned(Tdws2StringResultType(ResultType).OnReadLn) then
    Tdws2StringResultType(ResultType).OnReadLn(Self, Result)
  else
    Result := '';
end;

{ Tdws2StringResultType }

function Tdws2StringResultType.CreateProgResult: Tdws2Result;
begin
  Result := Tdws2StringResult.Create(Self);
end;

{ Tdws2StringUnit }

procedure Tdws2StringsUnit.AddUnitSymbols(SymbolTable: TSymbolTable);
var
  emptyArg: array of string;
begin
  TWriteFunction.Create(SymbolTable, 'WriteStr', ['Str', SYS_VARIANT], '');
  TWriteLnFunction.Create(SymbolTable, 'WriteLn', ['Str', SYS_VARIANT], '');
  TWriteAllFunction.Create(SymbolTable, 'WriteAll', ['Str', SYS_VARIANT], '');

  SetLength(emptyArg, 0);
  TReadCharFunction.Create(SymbolTable, 'ReadChar', emptyArg, SYS_STRING);
  TReadLnFunction.Create(SymbolTable, 'ReadLn', emptyArg, SYS_STRING);
  TReadAllFunction.Create(SymbolTable, 'ReadAll', emptyArg, SYS_STRING);
end;

constructor Tdws2StringsUnit.Create(AOwner: TComponent);
begin
  inherited;
  FUnitName := 'Strings';
end;

{ TWriteFunction }

procedure TWriteFunction.Execute;
begin
  Tdws2StringResult(Info.Caller.Result).AddStr(VarToStr(Info['Str']));
end;

{ TWriteLnFunction }

procedure TWriteLnFunction.Execute;
begin
  Tdws2StringResult(Info.Caller.Result).AddStr(VarToStr(Info['Str']) + #13#10);
end;

{ TWriteAllFunction }

procedure TWriteAllFunction.Execute;
begin
  Tdws2StringResult(Info.Caller.Result).SetStr(VarToStr(Info['Str']));
end;

{ TReadCharFunction }

procedure TReadCharFunction.Execute;
begin
  Info.Result := Tdws2StringResult(Info.Caller.Result).ReadChar;
end;

{ TReadLnFunction }

procedure TReadLnFunction.Execute;
begin
  Info.Result := Tdws2StringResult(Info.Caller.Result).ReadLn;
end;

{ TReadAllFunction }

procedure TReadAllFunction.Execute;
begin
  Info.Result := Tdws2StringResult(Info.Caller.Result).Str;
end;

end.
 