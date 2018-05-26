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
{    Contributor(s):  Eric Grange                                      }
{                                                                      }
{    Compatibility:                                                    }
{       [x] D5 (WK)   [x] D6 (WK)    [ ] K1 (WK)                       }
{**********************************************************************}

// dws2VCLGUIFunctions - Generated : 27.02.01 12:43:46
unit dws2VCLGUIFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols;

type

  TShowMessageFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TShowMessageFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TInputBoxFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TErrorDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TInformationDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TQuestionDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TOkCancelDlgFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  Tdws2GUIFunctions = class(TComponent)
  end;

implementation

uses Windows, Forms, Dialogs;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

  { TShowMessageFunc }

procedure TShowMessageFunc.Execute;
var
  Msg: string;
begin
  Msg := Info['msg'];
  ShowMessage(Msg);
end;

function TShowMessageFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TShowMessageFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TShowMessageFuncExpr.Eval: Variant;
var
  Msg: string;
begin
  Msg := Expr.Eval;
  ShowMessage(Msg);
end;

{ TInputBoxFunc }

procedure TInputBoxFunc.Execute;
var
  Param1, Param2, Param3: string;
begin
  Param1 := Info['aCaption'];
  Param2 := Info['aPrompt'];
  Param3 := Info['aDefault'];
  Info['Result'] := InputBox(Param1, Param2, Param3);
end;

{ TErrorDlgFunc }

procedure TErrorDlgFunc.Execute;
begin
  Application.MessageBox(PChar(string(Info['msg'])), 'Error',
    MB_ICONERROR + MB_OK + MB_APPLMODAL + MB_TOPMOST);
end;

{ TInformationDlgFunc }

procedure TInformationDlgFunc.Execute;
begin
  Application.MessageBox(PChar(string(Info['msg'])), 'Information',
    MB_ICONINFORMATION + MB_OK + MB_APPLMODAL + MB_TOPMOST);
end;

{ TQuestionDlgFunc }

procedure TQuestionDlgFunc.Execute;
begin
  Info['Result'] := (Application.MessageBox(PChar(string(Info['msg'])), 'Question',
    MB_ICONQUESTION + MB_YESNO + MB_APPLMODAL + MB_TOPMOST) = IDYES)
end;

{ TOkCancelDlgFunc }

procedure TOkCancelDlgFunc.Execute;
begin
  Info['Result'] := (Application.MessageBox(PChar(string(Info['msg'])),
    'Confirmation',
    MB_ICONQUESTION + MB_OKCANCEL + MB_APPLMODAL + MB_TOPMOST) = IDOK)
end;

initialization
  RegisterInternalFunction(TShowMessageFunc, 'ShowMessage', ['msg', cString], '');
  RegisterInternalFunction(TInputBoxFunc, 'InputBox', ['aCaption', cString,
    'aPrompt', cString, 'aDefault', cString], cString);
  RegisterInternalFunction(TErrorDlgFunc, 'ErrorDlg', ['msg', cString], '');
  RegisterInternalFunction(TInformationDlgFunc, 'InformationDlg', ['msg', cString],
    '');
  RegisterInternalFunction(TQuestionDlgFunc, 'QuestionDlg', ['msg', cString],
    cBoolean);
  RegisterInternalFunction(TOkCancelDlgFunc, 'OkCancelDlg', ['msg', cString],
    cBoolean);
end.
