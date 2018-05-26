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
{    Portions created by Eric Grange are Copyright                     }
{                        (C) 2001 Eric Grange, France.                 }
{                                                                      }
{                                                                      }
{    All Rights Reserved.                                              }
{                                                                      }
{                                                                      }
{    Contributor(s):  Eric Grange, Jeremy Coulter                      }
{                                                                      }
{    Compatibility:                                                    }
{       [x] D5 (WK)   [x] D6 (WK)    [x] K1 (WK)                       }
{**********************************************************************}

// dws2StringFunctions - Generated : 27.02.01 12:43:46

{$I dws2.inc}
unit dws2StringFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols;

type

  TIntToStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TIntToStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToIntFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TStrToIntFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToIntDefFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TIntToHexFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TFloatToStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TFloatToStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToFloatFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TStrToFloatFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToFloatDefFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TChrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TOrdFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCharAtFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TSetCharAtFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCopyFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLeftStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRightStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TMidStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TDeleteFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TInsertFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLowerCaseFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TLowerCaseFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TAnsiLowerCaseFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TAnsiLowerCaseFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TUpperCaseFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TUpperCaseFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TAnsiUpperCaseFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TAnsiUpperCaseFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TPosFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLengthFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TLengthFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TSetLengthFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TTrimLeftFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTrimLeftFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTrimRightFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTrimRightFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTrimFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTrimFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCompareTextFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TAnsiCompareTextFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCompareStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TAnsiCompareStrFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TIsDelimiterFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLastDelimiterFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TQuotedStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TQuotedStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStringOfCharFunc = class(TInternalFunction)
    procedure Execute; override;
  end;
  
implementation

uses SysUtils;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

  { TIntToStrFunc }

procedure TIntToStrFunc.Execute;
begin
  Info.Result := IntToStr(Integer(Info['i']));
end;

function TIntToStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        IntToStr(Args[0].Eval))
    else
      Result := TIntToStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TIntToStrFuncExpr.Eval: Variant;
begin
  Result := IntToStr(Expr.Eval);
end;

{ TStrToIntFunc }

procedure TStrToIntFunc.Execute;
begin
  Info.Result := StrToInt(Info['str']);
end;

function TStrToIntFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypInteger,
        StrToInt(Args[0].Eval))
    else
      Result := TStrToIntFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TStrToIntFuncExpr.Eval: Variant;
begin
  Result := StrToInt(Expr.Eval);
end;

{ TStrToIntDefFunc }

procedure TStrToIntDefFunc.Execute;
begin
  Info.Result := StrToIntDef(Info['str'], Integer(Info['def']));
end;

{ TIntToHexFunc }

procedure TIntToHexFunc.Execute;
begin
  Info.Result := IntToHex(Integer(Info['v']), Integer(Info['digits']));
end;

{ TFloatToStrFunc }

procedure TFloatToStrFunc.Execute;
begin
  Info.Result := FloatToStr(Info['f']);
end;

function TFloatToStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        FloatToStr(Args[0].Eval))
    else
      Result := TFloatToStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TFloatToStrFuncExpr.Eval: Variant;
begin
  Result := FloatToStr(Expr.Eval);
end;

{ TStrToFloatFunc }

procedure TStrToFloatFunc.Execute;
begin
  Info.Result := StrToFloat(Info['str']);
end;

function TStrToFloatFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        StrToFloat(Args[0].Eval))
    else
      Result := TStrToFloatFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TStrToFloatFuncExpr.Eval: Variant;
begin
  Result := StrToFloat(Expr.Eval);
end;

{ TStrToFloatDefFunc }

procedure TStrToFloatDefFunc.Execute;
begin
  try
    Info.Result := StrToFloat(Info['str']);
  except
    Info.Result := Info['def'];
  end;
end;

{ TCopyFunc }

procedure TCopyFunc.Execute;
begin
  Info.Result := Copy(Info['str'], Integer(Info['index']),
    Integer(Info['len']));
end;

{ TLeftStrFunc }

procedure TLeftStrFunc.execute;
begin
  Info.Result := Copy(Info['AText'], 1, integer(info['ACount']));
end;

{ TRightStrFunc }

procedure TRightStrFunc.execute;
begin
  Info.Result := Copy(Info['AText'], Length(Info['AText']) + 1 - integer(info['ACount']), integer(info['ACount']));
end;

{ TMidStrFunc }

procedure TMidStrFunc.execute;
begin
  Info.Result := Copy(Info['AText'], integer(info['AStart']), integer(info['ACount']));
end;

{ TDeleteFunc }

procedure TDeleteFunc.Execute;
var
  s: string;
begin
  s := Info['S'];
  Delete(s, Integer(Info['Index']), Integer(Info['Len']));
  Info['S'] := s;
end;

{ TInsertFunc }

procedure TInsertFunc.Execute;
var
  s: string;
begin
  s := Info['S'];
  Insert(Info['src'], s, Integer(Info['Index']));
  Info['S'] := s;
end;

{ TLowerCaseFunc }

procedure TLowerCaseFunc.Execute;
begin
  Info.Result := LowerCase(Info['str']);
end;

function TLowerCaseFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        LowerCase(Args[0].Eval))
    else
      Result := TLowerCaseFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TLowerCaseFuncExpr.Eval: Variant;
begin
  Result := LowerCase(Expr.Eval);
end;

{ TAnsiLowerCaseFunc }

procedure TAnsiLowerCaseFunc.Execute;
begin
  Info.Result := AnsiLowerCase(Info['str']);
end;

function TAnsiLowerCaseFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        AnsiLowerCase(Args[0].Eval))
    else
      Result := TAnsiLowerCaseFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TAnsiLowerCaseFuncExpr.Eval: Variant;
begin
  Result := AnsiLowerCase(Expr.Eval);
end;

{ TUpperCaseFunc }

procedure TUpperCaseFunc.Execute;
begin
  Info.Result := UpperCase(Info['str']);
end;

function TUpperCaseFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        UpperCase(Args[0].Eval))
    else
      Result := TUpperCaseFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TUpperCaseFuncExpr.Eval: Variant;
begin
  Result := UpperCase(Expr.Eval);
end;

{ TAnsiUpperCaseFunc }

procedure TAnsiUpperCaseFunc.Execute;
begin
  Info.Result := AnsiUpperCase(Info['str']);
end;

function TAnsiUpperCaseFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        AnsiUpperCase(Args[0].Eval))
    else
      Result := TAnsiUpperCaseFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TAnsiUpperCaseFuncExpr.Eval: Variant;
begin
  Result := AnsiUpperCase(Expr.Eval);
end;

{ TPosFunc }

procedure TPosFunc.Execute;
begin
  Info.Result := Pos(Info['subStr'], Info['str']);
end;

{ TLengthFunc }

procedure TLengthFunc.Execute;
begin
  Info.Result := Length(Info['str']);
end;

function TLengthFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypInteger,
        Length(Args[0].Eval))
    else
      Result := TLengthFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TLengthFuncExpr.Eval: Variant;
begin
  Result := Length(Expr.Eval);
end;

{ TTrimLeftFunc }

procedure TTrimLeftFunc.Execute;
begin
  Info.Result := TrimLeft(Info['str']);
end;

function TTrimLeftFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        TrimLeft(Args[0].Eval))
    else
      Result := TTrimLeftFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTrimLeftFuncExpr.Eval: Variant;
begin
  Result := TrimLeft(Expr.Eval);
end;

{ TTrimRightFunc }

procedure TTrimRightFunc.Execute;
begin
  Info.Result := TrimRight(Info['str']);
end;

function TTrimRightFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        TrimRight(Args[0].Eval))
    else
      Result := TTrimRightFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTrimRightFuncExpr.Eval: Variant;
begin
  Result := TrimRight(Expr.Eval);
end;

{ TTrimFunc }

procedure TTrimFunc.Execute;
begin
  Info.Result := Trim(Info['str']);
end;

function TTrimFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString, Trim(Args[0].Eval))
    else
      Result := TTrimFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTrimFuncExpr.Eval: Variant;
begin
  Result := Trim(Expr.Eval);
end;

{ TCompareTextFunc }

procedure TCompareTextFunc.Execute;
begin
  Info.Result := CompareText(Info['str1'], Info['str2']);
end;

{ TAnsiCompareTextFunc }

procedure TAnsiCompareTextFunc.Execute;
begin
  Info.Result := AnsiCompareText(Info['str1'], Info['str2']);
end;

{ TCompareStrFunc }

procedure TCompareStrFunc.Execute;
begin
  Info.Result := CompareStr(Info['str1'], Info['str2']);
end;

{ TAnsiCompareStrFunc }

procedure TAnsiCompareStrFunc.Execute;
begin
  Info.Result := AnsiCompareStr(Info['str1'], Info['str2']);
end;

{ TIsDelimiterFunc }

procedure TIsDelimiterFunc.Execute;
begin
  Info.Result := IsDelimiter(Info['delims'], Info['s'],
    Integer(Info['index']));
end;

{ TLastDelimiterFunc }

procedure TLastDelimiterFunc.Execute;
begin
  Info.Result := LastDelimiter(Info['delims'], Info['s']);
end;

{ TQuotedStrFunc }

procedure TQuotedStrFunc.Execute;
begin
  Info.Result := QuotedStr(Info['str']);
end;

function TQuotedStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        QuotedStr(Args[0].Eval))
    else
      Result := TQuotedStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TQuotedStrFuncExpr.Eval: Variant;
begin
  Result := QuotedStr(Expr.Eval);
end;

{ TChrFunc }

procedure TChrFunc.Execute;
begin
  Info.Result := Chr(Integer(Info['x']));
end;

{ TOrdFunc }

procedure TOrdFunc.Execute;
begin
  Info.Result := Ord(String(Info['s'])[1]);
end;

{ TCharAtFunc }

procedure TCharAtFunc.Execute;
begin
  Info.Result := String(Info['s'])[Integer(Info['x'])];
end;

{ TSetCharAtFunc }

procedure TSetCharAtFunc.Execute;
var
  s: string;
begin
  s := Info['s'];
  s[Integer(Info['x'])] := String(Info['c'])[1];
  Info['s'] := s;
end;

{ TSetLengthFunc }

procedure TSetLengthFunc.Execute;
var
  S: string;
begin
  //procedure SetLength(var S : String; NewLength : Integer);
  S := Info['S'];
  SetLength(S, Integer(Info['NewLength']));
  Info['S'] := S;              // re-assign 'var' value
end;

{ TStringOfCharFunc }

procedure TStringOfCharFunc.Execute;
var
  Ch: string;
begin
  //function StringOfChar(Ch : String; Count : Integer) : String;
  Ch := Info['Ch'];
  if Length(Ch) < 1 then
    Ch := ' ';   // default to blank if an empty string
  Info.Result := StringOfChar(Ch[1], Integer(Info['Count']));
end;

initialization
  RegisterInternalFunction(TIntToStrFunc, 'IntToStr', ['i', cInteger], cString);
  RegisterInternalFunction(TStrToIntFunc, 'StrToInt', ['str', cString], cInteger);
  RegisterInternalFunction(TStrToIntDefFunc, 'StrToIntDef', ['str', cString, 'def', cInteger], cInteger);
  RegisterInternalFunction(TIntToHexFunc, 'IntToHex', ['v', cInteger, 'digits', cInteger], cString);
  RegisterInternalFunction(TFloatToStrFunc, 'FloatToStr', ['f', cFloat], cString);
  RegisterInternalFunction(TStrToFloatFunc, 'StrToFloat', ['str', cString], cFloat);
  RegisterInternalFunction(TStrToFloatDefFunc, 'StrToFloatDef', ['str', cString, 'def', cFloat], cFloat);
  RegisterInternalFunction(TChrFunc, 'Chr', ['x', cInteger], cString);
  RegisterInternalFunction(TOrdFunc, 'Ord', ['s', cString], cInteger);
  RegisterInternalFunction(TCharAtFunc, 'CharAt', ['s', cString, 'x', cInteger], cString);
  RegisterInternalFunction(TSetCharAtFunc, 'SetCharAt', ['@s', cString, 'x', cInteger, 'c', cString], '');
  RegisterInternalFunction(TDeleteFunc, 'Delete', ['@S', cString, 'Index', cInteger, 'Len', cInteger], '');
  RegisterInternalFunction(TInsertFunc, 'Insert', ['src', cString, '@S', cString, 'Index', cInteger], '');
  RegisterInternalFunction(TLowerCaseFunc, 'LowerCase', ['str', cString], cString);
  RegisterInternalFunction(TAnsiLowerCaseFunc, 'AnsiLowerCase', ['str', cString], cString);
  RegisterInternalFunction(TUpperCaseFunc, 'UpperCase', ['str', cString], cString);
  RegisterInternalFunction(TAnsiUpperCaseFunc, 'AnsiUpperCase', ['str', cString], cString);
  RegisterInternalFunction(TPosFunc, 'Pos', ['subStr', cString, 'str', cString], cInteger);
  RegisterInternalFunction(TLengthFunc, 'Length', ['str', cString], cInteger);
  RegisterInternalFunction(TSetLengthFunc, 'SetLength', ['@S', cString, 'NewLength', cInteger], '');
  RegisterInternalFunction(TTrimLeftFunc, 'TrimLeft', ['str', cString], cString);
  RegisterInternalFunction(TTrimRightFunc, 'TrimRight', ['str', cString], cString);
  RegisterInternalFunction(TTrimFunc, 'Trim', ['str', cString], cString);
  RegisterInternalFunction(TCompareTextFunc, 'CompareText', ['str1', cString, 'str2', cString], cInteger);
  RegisterInternalFunction(TAnsiCompareTextFunc, 'AnsiCompareText', ['str1', cString, 'str2', cString], cInteger);
  RegisterInternalFunction(TCompareStrFunc, 'CompareStr', ['str1', cString, 'str2', cString], cInteger);
  RegisterInternalFunction(TAnsiCompareStrFunc, 'AnsiCompareStr', ['str1', cString, 'str2', cString], cInteger);
  RegisterInternalFunction(TIsDelimiterFunc, 'IsDelimiter', ['delims', cString, 's', cString, 'index', cInteger], cBoolean);
  RegisterInternalFunction(TLastDelimiterFunc, 'LastDelimiter', ['delims', cString, 's', cString], cInteger);
  RegisterInternalFunction(TQuotedStrFunc, 'QuotedStr', ['str', cString], cString);
  RegisterInternalFunction(TCopyFunc, 'Copy', ['str', cString, 'Index', cInteger, 'Len', cInteger], cString);

  RegisterInternalFunction(TLeftStrFunc, 'LeftStr', ['AText', cString, 'ACount', cInteger], cString);
  RegisterInternalFunction(TRightStrFunc, 'RightStr', ['AText', cString, 'ACount', cInteger], cString);
  RegisterInternalFunction(TMidStrFunc, 'MidStr', ['AText', cString,'AStart', cInteger, 'ACount', cInteger], cString);

  RegisterInternalFunction(TStringOfCharFunc, 'StringOfChar', ['Ch', cString, 'Count', cInteger], cString);
end.
