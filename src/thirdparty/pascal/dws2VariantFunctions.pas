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
{    Ackermann.                                                        }
{                                                                      }
{    Portions created by Matthias Ackermann are Copyright              }
{                        (C) 2000 Matthias Ackermann, Switzerland.     }
{                                                                      }
{    All Rights Reserved.                                              }
{                                                                      }
{    Contributor(s): Andreas Luleich                                   }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2VariantFunctions;

interface

uses
  Classes, dws2Functions, dws2Exprs, dws2Symbols;

type
  TVarClearFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TVarIsNullFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TVarIsEmptyFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TVarAsTypeFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TVarToStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

implementation

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  SysUtils;

{$IFNDEF NEWVARIANTS}
const
	varShortInt = $0010; { vt_i1          16 }
	varWord     = $0012; { vt_ui2         18 }
	varLongWord = $0013; { vt_ui4         19 }
	varInt64    = $0014; { vt_i8          20 }
{$ENDIF}

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';
  cVariant = 'Variant';

{ TVarClearFunc }

procedure TVarClearFunc.Execute;
begin
  Info['v'] := Unassigned;
end;

{ TVarIsNullFunc }

procedure TVarIsNullFunc.Execute;
begin
  Info.Result := VarIsNull(Info['v']);
end;

{ TVarIsEmptyFunc }

procedure TVarIsEmptyFunc.Execute;
begin
  Info.Result := VarIsEmpty(Info['v']);
end;

{ TVarAsTypeFunc }

procedure TVarAsTypeFunc.Execute;
begin
  Info.Result := VarAsType(Info['v'], Info['VarType']);
end;

{ TVarToStrFunc }

procedure TVarToStrFunc.Execute;
begin
  Info.Result := VarToStr(Info['v']);
end;

{ InitVariants }

procedure InitVariants(SystemTable, UnitSyms, UnitTable : TSymbolTable);
var
  T, E : TTypeSymbol;
begin
  T := SystemTable.FindSymbol('Integer') as TTypeSymbol;
  E := TEnumerationSymbol.Create('TVarType',T);
  UnitTable.AddSymbol(E);
  UnitTable.AddSymbol(TElementSymbol.Create('varEmpty',E,varEmpty,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varNull',E,varNull,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varSmallint',E,varSmallint,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varInteger',E,varInteger,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varSingle',E,varSingle,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varDouble',E,varDouble,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varCurrency',E,varCurrency,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varDate',E,varDate,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varOleStr',E,varOleStr,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varDispatch',E,varDispatch,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varError',E,varError,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varBoolean',E,varBoolean,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varVariant',E,varVariant,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varUnknown',E,varUnknown,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varShortInt',E,varShortInt,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varByte',E,varByte,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varWord',E,varWord,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varLongWord',E,varLongWord,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varInt64',E,varInt64,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varStrArg',E,varStrArg,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varString',E,varString,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varAny',E,varAny,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varTypeMask',E,varTypeMask,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varArray',E,varArray,True));
  UnitTable.AddSymbol(TElementSymbol.Create('varByRef',E,varByRef,True));
end;

initialization

  RegisterInternalInitProc(@InitVariants);
  RegisterInternalFunction(TVarClearFunc, 'VarClear', ['@v', cVariant], '');
  RegisterInternalFunction(TVarIsNullFunc, 'VarIsNull', ['v', cVariant], cBoolean);
  RegisterInternalFunction(TVarIsEmptyFunc, 'VarIsEmpty', ['v', cVariant], cBoolean);
  RegisterInternalFunction(TVarAsTypeFunc, 'VarAsType', ['v', cVariant, 'VarType', cInteger], cVariant);
  RegisterInternalFunction(TVarToStrFunc, 'VarToStr', ['v', cVariant], cString);
end.

