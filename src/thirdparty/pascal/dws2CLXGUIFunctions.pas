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
{    Portions created by Danilo Luiz Rheinheimer are Copyright         }
{                        (C) 2001 Danilo Luiz Rheinheimer, Brazil.     }
{                                                                      }
{                                                                      }
{    All Rights Reserved.                                              }
{                                                                      }
{                                                                      }
{    Contributor(s):  Eric Grange, Danilo Luiz Rheinheimer             }
{                                                                      }
{    Compatibility:                                                    }
{       [x] D5 (WK)   [x] D6 (WK)    [x] K1 (WK)                       }
{**********************************************************************}

// dws2CLXGUIFunctions - Generated : 27.02.01 12:43:46
unit dws2CLXGUIFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols, QControls;

type

  TCLXShowMessageFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TCLXShowMessageFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCLXInputBoxFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCLXErrorDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCLXInformationDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCLXQuestionDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TCLXOkCancelDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  Tdws2GUIFunctions = class(TComponent)
  end;

implementation

uses QForms, QDialogs;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

  { TCLXShowMessageFunc }

procedure TCLXShowMessageFunc.Execute;
var
  Msg: string;
begin
  Msg := Info['msg'];
  ShowMessage(Msg);
end;

function TCLXShowMessageFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TCLXShowMessageFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TCLXShowMessageFuncExpr.Eval: Variant;
var
  Msg: string;
begin
  Msg := Expr.Eval;
  ShowMessage(Msg);
end;

{ TInputBoxFunc }

procedure TCLXInputBoxFunc.Execute;
var
  Param1, Param2, Param3: string;
begin
  Param1 := Info['aCaption'];
  Param2 := Info['aPrompt'];
  Param3 := Info['aDefault'];
  Info['Result'] := InputBox(Param1, Param2, Param3);
end;

{ TCLXErrorDlgFunc }

procedure TCLXErrorDlgFunc.Execute;
begin
  Application.MessageBox(PChar(string(Info['msg'])), 'Error',
    [smbOK]);
end;

{ TCLXInformationDlgFunc }

procedure TCLXInformationDlgFunc.Execute;
begin
  Application.MessageBox(PChar(string(Info['msg'])), 'Information',
    [smbOK]);
end;

{ TCLXQuestionDlgFunc }

procedure TCLXQuestionDlgFunc.Execute;
begin
  Info['Result'] := (Application.MessageBox(PChar(string(Info['msg'])), 'Question',
    [smbYES, smbNO]) = smbYES)
end;

{ TCLXOkCancelDlgFunc }

procedure TCLXOkCancelDlgFunc.Execute;
begin
  Info['Result'] := (Application.MessageBox(PChar(string(Info['msg'])),
    'Confirmation',
    [smbOK, smbCANCEL]) = smbOK)
end;

initialization

  RegisterInternalFunction(TCLXShowMessageFunc, 'ShowMessage', ['msg', cString],
    '');
  RegisterInternalFunction(TCLXInputBoxFunc, 'InputBox', ['aCaption', cString,
    'aPrompt', cString, 'aDefault', cString], cString);
  RegisterInternalFunction(TCLXErrorDlgFunc, 'ErrorDlg', ['msg', cString], '');
  RegisterInternalFunction(TCLXInformationDlgFunc, 'InformationDlg', ['msg',
    cString], '');
  RegisterInternalFunction(TCLXQuestionDlgFunc, 'QuestionDlg', ['msg', cString],
    cBoolean);
  RegisterInternalFunction(TCLXOkCancelDlgFunc, 'OkCancelDlg', ['msg', cString],
    cBoolean);

end.
