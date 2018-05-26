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
{    Contributor(s): Eric Grange                                       }
{                                                                      }
{    Compatibility:                                                    }
{       [x] D5 (WK)   [x] D6 (WK)    [x] K1 (WK)                       }
{**********************************************************************}

// dws2MathFunctions - Generated : 27.02.01 12:43:46

{$I dws2.inc}
unit dws2MathFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols;

type
  TIncFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;

  TSinFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TSinFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TSinhFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TSinhFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCosFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TCosFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCoshFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TCoshFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTanFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTanFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTanhFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTanhFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcSinFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcSinFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcSinhFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcSinhFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcCosFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcCosFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcCoshFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcCoshFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcTanFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcTanFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TArcTanhFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TArcTanhFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCotanFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TCotanFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  THypotFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TAbsFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TAbsFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TExpFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExpFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TLnFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TLnFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TLog2Func = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TLog2FuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TLog10Func = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TLog10FuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TLogNFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TSqrtFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TSqrtFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TSqrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TSqrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TIntFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TIntFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TFracFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TFracFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTruncFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRoundFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TPowerFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TDegToRadFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TDegToRadFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TRadToDegFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TRadToDegFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TMaxFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TMinFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TPiFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TPiFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TRandomFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRandomIntFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRandomizeFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TRandomizeFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TRandGFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRandSeedFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TSetRandSeedFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

implementation

uses Math;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

{ TIncFunc }

procedure TIncFunc.Execute;
begin
  Info['a'] := Info['a'] + Info['b'];
end;

function TIncFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TIncrExpr.Create(Prog, Pos, Args[0], Args[1]);
    Args.Clear;
    Free;
  end;
end;

{ TSinFunc }

procedure TSinFunc.Execute;
begin
  Info.Result := Sin(Info['a']);
end;

function TSinFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Sin(Args[0].Eval))
    else
      Result := TSinFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TSinFuncExpr.Eval: Variant;
begin
  Result := Sin(Expr.Eval);
end;

{ TSinhFunc }

procedure TSinhFunc.Execute;
begin
  Info.Result := Sinh(Info['a']);
end;

function TSinhFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Sinh(Args[0].Eval))
    else
      Result := TSinhFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TSinhFuncExpr.Eval: Variant;
begin
  Result := Sinh(Expr.Eval);
end;

{ TCosFunc }

procedure TCosFunc.Execute;
begin
  Info.Result := Cos(Info['a']);
end;

function TCosFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Cos(Args[0].Eval))
    else
      Result := TCosFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TCosFuncExpr.Eval: Variant;
begin
  Result := Cos(Expr.Eval);
end;

{ TCoshFunc }

procedure TCoshFunc.Execute;
begin
  Info.Result := Cosh(Info['a']);
end;

function TCoshFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Cosh(Args[0].Eval))
    else
      Result := TCoshFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TCoshFuncExpr.Eval: Variant;
begin
  Result := Cosh(Expr.Eval);
end;

{ TTanFunc }

procedure TTanFunc.Execute;
begin
  Info.Result := Tan(Info['a']);
end;

function TTanFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Tan(Args[0].Eval))
    else
      Result := TTanFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTanFuncExpr.Eval: Variant;
begin
  Result := Tan(Expr.Eval);
end;

{ TTanhFunc }

procedure TTanhFunc.Execute;
begin
  Info.Result := Tanh(Info['a']);
end;

function TTanhFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Tanh(Args[0].Eval))
    else
      Result := TTanhFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTanhFuncExpr.Eval: Variant;
begin
  Result := Tanh(Expr.Eval);
end;

{ TArcSinFunc }

procedure TArcSinFunc.Execute;
begin
  Info.Result := ArcSin(Info['v']);
end;

function TArcSinFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, ArcSin(Args[0].Eval))
    else
      Result := TArcSinFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcSinFuncExpr.Eval: Variant;
begin
  Result := ArcSin(Expr.Eval);
end;

{ TArcSinhFunc }

procedure TArcSinhFunc.Execute;
begin
  Info.Result := ArcSinh(Info['v']);
end;

function TArcSinhFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        ArcSinh(Args[0].Eval))
    else
      Result := TArcSinhFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcSinhFuncExpr.Eval: Variant;
begin
  Result := ArcSinh(Expr.Eval);
end;

{ TArcCosFunc }

procedure TArcCosFunc.Execute;
begin
  Info.Result := ArcCos(Info['v']);
end;

function TArcCosFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, ArcCos(Args[0].Eval))
    else
      Result := TArcCosFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcCosFuncExpr.Eval: Variant;
begin
  Result := ArcCos(Expr.Eval);
end;

{ TArcCoshFunc }

procedure TArcCoshFunc.Execute;
begin
  Info.Result := ArcCosh(Info['v']);
end;

function TArcCoshFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        ArcCosh(Args[0].Eval))
    else
      Result := TArcCoshFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcCoshFuncExpr.Eval: Variant;
begin
  Result := ArcCosh(Expr.Eval);
end;

{ TArcTanFunc }

procedure TArcTanFunc.Execute;
begin
  Info.Result := ArcTan(Info['v']);
end;

function TArcTanFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, ArcTan(Args[0].Eval))
    else
      Result := TArcTanFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcTanFuncExpr.Eval: Variant;
begin
  Result := ArcTan(Expr.Eval);
end;

{ TArcTanhFunc }

procedure TArcTanhFunc.Execute;
begin
  Info.Result := ArcTanh(Info['v']);
end;

function TArcTanhFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        ArcTanh(Args[0].Eval))
    else
      Result := TArcTanhFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TArcTanhFuncExpr.Eval: Variant;
begin
  Result := ArcTanh(Expr.Eval);
end;

{ TCotanFunc }

procedure TCotanFunc.Execute;
begin
  Info.Result := Cotan(Info['a']);
end;

function TCotanFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Cotan(Args[0].Eval))
    else
      Result := TCotanFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TCotanFuncExpr.Eval: Variant;
begin
  Result := Cotan(Expr.Eval);
end;

{ THypotFunc }

procedure THypotFunc.Execute;
begin
  Info.Result := Hypot(Info['x'], Info['y']);
end;

{ TAbsFunc }

procedure TAbsFunc.Execute;
begin
  Info.Result := Abs(Info['v']);
end;

function TAbsFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Abs(Args[0].Eval))
    else
      Result := TAbsFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TAbsFuncExpr.Eval: Variant;
begin
  Result := Abs(Expr.Eval);
end;

{ TExpFunc }

procedure TExpFunc.Execute;
begin
  Info.Result := Exp(Info['v']);
end;

function TExpFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Exp(Args[0].Eval))
    else
      Result := TExpFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExpFuncExpr.Eval: Variant;
begin
  Result := Exp(Expr.Eval);
end;

{ TLnFunc }

procedure TLnFunc.Execute;
begin
  Info.Result := Ln(Info['v']);
end;

function TLnFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Ln(Args[0].Eval))
    else
      Result := TLnFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TLnFuncExpr.Eval: Variant;
begin
  Result := Ln(Expr.Eval);
end;

{ TLog2Func }

procedure TLog2Func.Execute;
begin
  Info.Result := Log2(Info['v']);
end;

function TLog2Func.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Log2(Args[0].Eval))
    else
      Result := TLog2FuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TLog2FuncExpr.Eval: Variant;
begin
  Result := Log2(Expr.Eval);
end;

{ TLog10Func }

procedure TLog10Func.Execute;
begin
  Info.Result := Log10(Info['v']);
end;

function TLog10Func.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Log10(Args[0].Eval))
    else
      Result := TLog10FuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TLog10FuncExpr.Eval: Variant;
begin
  Result := Log10(Expr.Eval);
end;

{ TLogNFunc }

procedure TLogNFunc.Execute;
begin
  Info.Result := LogN(Info['n'], Info['x']);
end;

{ TSqrtFunc }

procedure TSqrtFunc.Execute;
begin
  Info.Result := Sqrt(Info['v']);
end;

function TSqrtFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Sqrt(Args[0].Eval))
    else
      Result := TSqrtFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TSqrtFuncExpr.Eval: Variant;
begin
  Result := Sqrt(Expr.Eval);
end;

{ TSqrFunc }

procedure TSqrFunc.Execute;
begin
  Info.Result := Sqr(Info['v']);
end;

function TSqrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Sqr(Args[0].Eval))
    else
      Result := TSqrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TSqrFuncExpr.Eval: Variant;
begin
  Result := Sqr(Expr.Eval);
end;

{ TIntFunc }

procedure TIntFunc.Execute;
begin
  Info.Result := INT(Info['v']);
end;

function TIntFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, INT(Args[0].Eval))
    else
      Result := TIntFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TIntFuncExpr.Eval: Variant;
begin
  Result := INT(Expr.Eval);
end;

{ TFracFunc }

procedure TFracFunc.Execute;
begin
  Info.Result := Frac(Info['v']);
end;

function TFracFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat, Frac(Args[0].Eval))
    else
      Result := TFracFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TFracFuncExpr.Eval: Variant;
begin
  Result := Frac(Expr.Eval);
end;

{ TTruncFunc }

procedure TTruncFunc.Execute;
begin
  Info.Result := Integer(Trunc(Info['v']));
end;

{ TRoundFunc }

procedure TRoundFunc.Execute;
begin
  Info.Result := Integer(Round(Info['v']));
end;

{ TPowerFunc }

procedure TPowerFunc.Execute;
begin
  Info.Result := Power(Info['base'], Info['exponent']);
end;

{ TDegToRadFunc }

procedure TDegToRadFunc.Execute;
begin
  Info.Result := DegToRad(Info['a']);
end;

function TDegToRadFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        DegToRad(Args[0].Eval))
    else
      Result := TDegToRadFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TDegToRadFuncExpr.Eval: Variant;
begin
  Result := DegToRad(Expr.Eval);
end;

{ TRadToDegFunc }

procedure TRadToDegFunc.Execute;
begin
  Info.Result := RadToDeg(Info['a']);
end;

function TRadToDegFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        RadToDeg(Args[0].Eval))
    else
      Result := TRadToDegFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TRadToDegFuncExpr.Eval: Variant;
begin
  Result := RadToDeg(Expr.Eval);
end;

{ TMaxFunc }

procedure TMaxFunc.Execute;
begin
  Info.Result := max(Info['v1'], Info['v2']);
end;

{ TMinFunc }

procedure TMinFunc.Execute;
begin
  Info.Result := min(Info['v1'], Info['v2']);
end;

{ TPiFunc }

procedure TPiFunc.Execute;
begin
  Info.Result := Pi;
end;

function TPiFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TPiFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TPiFuncExpr.Eval: Variant;
begin
  Result := Pi;
end;

{ TRandomFunc }

procedure TRandomFunc.Execute;
begin
  Info.Result := Random;
end;

{ TRandomIntFunc }

procedure TRandomIntFunc.Execute;
begin
  Info.Result := Random(Integer(Info['range']));
end;

{ TRandomizeFunc }

procedure TRandomizeFunc.Execute;
begin
  Randomize;
end;

function TRandomizeFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TRandomizeFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TRandomizeFuncExpr.Eval: Variant;
begin
  Randomize;
end;

{ TRandGFunc }

procedure TRandGFunc.Execute;
begin
  Info.Result := RandG(Info['mean'], Info['stdDev']);
end;

{ TRandSeedFunc }

procedure TRandSeedFunc.Execute;
begin
  Info.Result := RandSeed;
end;

{ TSetRandSeedFunc }

procedure TSetRandSeedFunc.Execute;
begin
  RandSeed := Integer(Info['seed']);
end;

initialization

  RegisterInternalFunction(TSinFunc, 'Sin', ['a', cFloat], cFloat);
  RegisterInternalFunction(TSinhFunc, 'Sinh', ['a', cFloat], cFloat);
  RegisterInternalFunction(TCosFunc, 'Cos', ['a', cFloat], cFloat);
  RegisterInternalFunction(TCoshFunc, 'Cosh', ['a', cFloat], cFloat);
  RegisterInternalFunction(TTanFunc, 'Tan', ['a', cFloat], cFloat);
  RegisterInternalFunction(TTanhFunc, 'Tanh', ['a', cFloat], cFloat);
  RegisterInternalFunction(TArcSinFunc, 'ArcSin', ['v', cFloat], cFloat);
  RegisterInternalFunction(TArcSinhFunc, 'ArcSinh', ['v', cFloat], cFloat);
  RegisterInternalFunction(TArcCosFunc, 'ArcCos', ['v', cFloat], cFloat);
  RegisterInternalFunction(TArcCoshFunc, 'ArcCosh', ['v', cFloat], cFloat);
  RegisterInternalFunction(TArcTanFunc, 'ArcTan', ['v', cFloat], cFloat);
  RegisterInternalFunction(TArcTanhFunc, 'ArcTanh', ['v', cFloat], cFloat);
  RegisterInternalFunction(TCotanFunc, 'Cotan', ['a', cFloat], cFloat);
  RegisterInternalFunction(THypotFunc, 'Hypot', ['x', cFloat, 'y', cFloat],
    cFloat);
  RegisterInternalFunction(TIncFunc, 'Inc', ['@a', cInteger, 'b', cInteger], cInteger);
  RegisterInternalFunction(TAbsFunc, 'Abs', ['v', cFloat], cFloat);
  RegisterInternalFunction(TExpFunc, 'Exp', ['v', cFloat], cFloat);
  RegisterInternalFunction(TLnFunc, 'Ln', ['v', cFloat], cFloat);
  RegisterInternalFunction(TLog2Func, 'Log2', ['v', cFloat], cFloat);
  RegisterInternalFunction(TLog10Func, 'Log10', ['v', cFloat], cFloat);
  RegisterInternalFunction(TLogNFunc, 'LogN', ['n', cFloat, 'x', cFloat],
    cFloat);
  RegisterInternalFunction(TSqrtFunc, 'Sqrt', ['v', cFloat], cFloat);
  RegisterInternalFunction(TSqrFunc, 'Sqr', ['v', cFloat], cFloat);
  RegisterInternalFunction(TIntFunc, 'Int', ['v', cFloat], cFloat);
  RegisterInternalFunction(TFracFunc, 'Frac', ['v', cFloat], cFloat);
  RegisterInternalFunction(TTruncFunc, 'Trunc', ['v', cFloat], cInteger);
  RegisterInternalFunction(TRoundFunc, 'Round', ['v', cFloat], cInteger);
  RegisterInternalFunction(TPowerFunc, 'Power', ['base', cFloat, 'exponent',
    cFloat], cFloat);
  RegisterInternalFunction(TDegToRadFunc, 'DegToRad', ['a', cFloat], cFloat);
  RegisterInternalFunction(TRadToDegFunc, 'RadToDeg', ['a', cFloat], cFloat);
  RegisterInternalFunction(TMaxFunc, 'Max', ['v1', cFloat, 'v2', cFloat],
    cFloat);
  RegisterInternalFunction(TMinFunc, 'Min', ['v1', cFloat, 'v2', cFloat],
    cFloat);
  RegisterInternalFunction(TPiFunc, 'Pi', [], cFloat);
  RegisterInternalFunction(TRandomFunc, 'Random', [], cFloat);
  RegisterInternalFunction(TRandomIntFunc, 'RandomInt', ['range', cInteger],
    cInteger);
  RegisterInternalFunction(TRandomizeFunc, 'Randomize', [], '');
  RegisterInternalFunction(TRandGFunc, 'RandG', ['mean', cFloat, 'stdDev',
    cFloat], cFloat);
  RegisterInternalFunction(TRandSeedFunc, 'RandSeed', [], cInteger);
  RegisterInternalFunction(TSetRandSeedFunc, 'SetRandSeed', ['seed', cInteger],
    '');

end.
