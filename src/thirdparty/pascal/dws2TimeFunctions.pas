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

// dws2TimeFunctions - Generated : 27.02.01 12:43:46

{$I dws2.inc}
unit dws2TimeFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols;

type

  TNowFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TNowFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TDateFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TDateFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TTimeFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTimeFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TDateTimeToStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TDateTimeToStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToDateTimeFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TStrToDateTimeFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TDateToStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TDateToStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToDateFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TStrToDateFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TTimeToStrFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TTimeToStrFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TStrToTimeFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TStrToTimeFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TDayOfWeekFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TDayOfWeekFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TFormatDateTimeFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TIsLeapYearFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TIsLeapYearFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TIncMonthFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TDecodeDateFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TEncodeDateFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TDecodeTimeFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TEncodeTimeFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

implementation

uses SysUtils;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cDateTime = 'DateTime';
  cBoolean = 'Boolean';

  { TNowFunc }

procedure TNowFunc.Execute;
begin
  Info.Result := Now;
end;

function TNowFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TNowFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TNowFuncExpr.Eval: Variant;
begin
  Result := Now;
end;

{ TDateFunc }

procedure TDateFunc.Execute;
begin
  Info.Result := Date;
end;

function TDateFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TDateFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TDateFuncExpr.Eval: Variant;
begin
  Result := Date;
end;

{ TTimeFunc }

procedure TTimeFunc.Execute;
begin
  Info.Result := Time;
end;

function TTimeFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TTimeFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TTimeFuncExpr.Eval: Variant;
begin
  Result := Time;
end;

{ TDateTimeToStrFunc }

procedure TDateTimeToStrFunc.Execute;
begin
  Info.Result := DateTimeToStr(Info['dt']);
end;

function TDateTimeToStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        DateTimeToStr(Args[0].Eval))
    else
      Result := TDateTimeToStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TDateTimeToStrFuncExpr.Eval: Variant;
begin
  Result := DateTimeToStr(Expr.Eval);
end;

{ TStrToDateTimeFunc }

procedure TStrToDateTimeFunc.Execute;
begin
  Info.Result := StrToDateTime(Info['str']);
end;

function TStrToDateTimeFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        StrToDateTime(Args[0].Eval))
    else
      Result := TStrToDateTimeFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TStrToDateTimeFuncExpr.Eval: Variant;
begin
  Result := StrToDateTime(Expr.Eval);
end;

{ TDateToStrFunc }

procedure TDateToStrFunc.Execute;
begin
  Info.Result := DateToStr(Info['dt']);
end;

function TDateToStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        DateToStr(Args[0].Eval))
    else
      Result := TDateToStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TDateToStrFuncExpr.Eval: Variant;
begin
  Result := DateToStr(Expr.Eval);
end;

{ TStrToDateFunc }

procedure TStrToDateFunc.Execute;
begin
  Info.Result := StrToDate(Info['str']);
end;

function TStrToDateFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        StrToDate(Args[0].Eval))
    else
      Result := TStrToDateFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TStrToDateFuncExpr.Eval: Variant;
begin
  Result := StrToDate(Expr.Eval);
end;

{ TTimeToStrFunc }

procedure TTimeToStrFunc.Execute;
begin
  Info.Result := TimeToStr(Info['dt']);
end;

function TTimeToStrFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        TimeToStr(Args[0].Eval))
    else
      Result := TTimeToStrFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TTimeToStrFuncExpr.Eval: Variant;
begin
  Result := TimeToStr(Expr.Eval);
end;

{ TStrToTimeFunc }

procedure TStrToTimeFunc.Execute;
begin
  Info.Result := StrToTime(Info['str']);
end;

function TStrToTimeFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypFloat,
        StrToTime(Args[0].Eval))
    else
      Result := TStrToTimeFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TStrToTimeFuncExpr.Eval: Variant;
begin
  Result := StrToTime(Expr.Eval);
end;

{ TDayOfWeekFunc }

procedure TDayOfWeekFunc.Execute;
begin
  Info.Result := DayOfWeek(Info['dt']);
end;

function TDayOfWeekFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypInteger,
        DayOfWeek(Args[0].Eval))
    else
      Result := TDayOfWeekFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TDayOfWeekFuncExpr.Eval: Variant;
begin
  Result := DayOfWeek(Expr.Eval);
end;

{ TFormatDateTimeFunc }

procedure TFormatDateTimeFunc.Execute;
begin
  Info.Result := FormatDateTime(Info['frm'], Info['dt']);
end;

{ TIsLeapYearFunc }

procedure TIsLeapYearFunc.Execute;
begin
  Info.Result := IsLeapYear(Integer(Info['year']));
end;

function TIsLeapYearFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypBoolean,
        IsLeapYear(Args[0].Eval))
    else
      Result := TIsLeapYearFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TIsLeapYearFuncExpr.Eval: Variant;
begin
  Result := IsLeapYear(Expr.Eval);
end;

{ TIncMonthFunc }

procedure TIncMonthFunc.Execute;
begin
  Info.Result := IncMonth(Info['dt'], Integer(Info['nb']));
end;

{ TDecodeDateFunc }

procedure TDecodeDateFunc.Execute;
var
  y, m, d: word;
begin
  DecodeDate(Info['dt'], y, m, d);
  Info['y'] := y;
  Info['m'] := m;
  Info['d'] := d;
end;

{ TEncodeDateFunc }

procedure TEncodeDateFunc.Execute;
begin
  Info.Result := EncodeDate(Integer(Info['y']), Integer(Info['m']),
    Integer(Info['d']));
end;

{ TDecodeTimeFunc }

procedure TDecodeTimeFunc.Execute;
var
  h, m, s, ms: word;

begin
  DecodeTime(Info['dt'], h, m, s, ms);
  Info['h'] := h;
  Info['m'] := m;
  Info['s'] := s;
  Info['ms'] := ms;
end;

{ TEncodeTimeFunc }

procedure TEncodeTimeFunc.Execute;
begin
  Info.Result := EncodeTime(Integer(Info['h']), Integer(Info['m']),
    Integer(Info['s']), Integer(Info['ms']));
end;

initialization

  RegisterInternalFunction(TNowFunc, 'Now', [], cDateTime);
  RegisterInternalFunction(TDateFunc, 'Date', [], cDateTime);
  RegisterInternalFunction(TTimeFunc, 'Time', [], cDateTime);
  RegisterInternalFunction(TDateTimeToStrFunc, 'DateTimeToStr', ['dt',
    cDateTime], cString);
  RegisterInternalFunction(TStrToDateTimeFunc, 'StrToDateTime', ['str',
    cString], cDateTime);
  RegisterInternalFunction(TDateToStrFunc, 'DateToStr', ['dt', cDateTime],
    cString);
  RegisterInternalFunction(TStrToDateFunc, 'StrToDate', ['str', cString],
    cDateTime);
  RegisterInternalFunction(TTimeToStrFunc, 'TimeToStr', ['dt', cDateTime],
    cString);
  RegisterInternalFunction(TStrToTimeFunc, 'StrToTime', ['str', cString],
    cDateTime);
  RegisterInternalFunction(TDayOfWeekFunc, 'DayOfWeek', ['dt', cDateTime],
    cInteger);
  RegisterInternalFunction(TFormatDateTimeFunc, 'FormatDateTime', ['frm',
    cString, 'dt', cDateTime], cString);
  RegisterInternalFunction(TIsLeapYearFunc, 'IsLeapYear', ['year', cInteger],
    cBoolean);
  RegisterInternalFunction(TIncMonthFunc, 'IncMonth', ['dt', cDateTime, 'nb',
    cInteger], cDateTime);
  RegisterInternalFunction(TDecodeDateFunc, 'DecodeDate', ['dt', cDateTime,
    '@y', cInteger, '@m', cInteger, '@d', cInteger], '');
  RegisterInternalFunction(TEncodeDateFunc, 'EncodeDate', ['y', cInteger, 'm',
    cInteger, 'd', cInteger], cDateTime);
  RegisterInternalFunction(TDecodeTimeFunc, 'DecodeTime', ['dt', cDateTime,
    '@h', cInteger, '@m', cInteger, '@s', cInteger, '@ms', cInteger], '');
  RegisterInternalFunction(TEncodeTimeFunc, 'EncodeTime', ['h', cInteger, 'm',
    cInteger, 's', cInteger, 'ms', cInteger], cDateTime);

end.
