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
{    Contributor(s): Danilo Luiz Rheinheimer                           }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Debugger;

interface

uses
  Classes, dws2Exprs;

type
  TOnDebugStartStopEvent = procedure(MainProg: TProgram) of object;
  TOnDebugEvent = procedure(Prog: TProgram; Expr: TExpr) of object;

  Tdws2SimpleDebugger = class(TComponent, IUnknown, IDebugger)
  private
    FOnDebug: TOnDebugEvent;
    FOnStartDebug: TOnDebugStartStopEvent;
    FOnStopDebug: TOnDebugStartStopEvent;
    FOnEnterFunc: TOnDebugEvent;
    FOnLeaveFunc: TOnDebugEvent;
    procedure StartDebug(MainProg: TProgram);
    procedure DoDebug(Prog: TProgram; Expr: TExpr);
    procedure StopDebug(MainProg: TProgram);
    procedure EnterFunc(Prog: TProgram; Expr: TExpr);
    procedure LeaveFunc(Prog: TProgram; Expr: TExpr);
  published
    property OnDebug: TOnDebugEvent read FOnDebug write FOnDebug;
    property OnDebugStart: TOnDebugStartStopEvent read FOnStartDebug write
      FOnStartDebug;
    property OnDebugStop: TOnDebugStartStopEvent read FOnStopDebug write
      FOnStopDebug;
    property OnEnterFunc: TOnDebugEvent read FOnEnterFunc write FOnEnterFunc;
    property OnLeaveFunc: TOnDebugEvent read FOnLeaveFunc write FOnLeaveFunc;
  end;

implementation

{ TdwsSimpleDebugger }

procedure Tdws2SimpleDebugger.DoDebug(Prog: TProgram; Expr: TExpr);
begin
  if Assigned(FOnDebug) then
    FOnDebug(Prog, Expr);
end;

procedure Tdws2SimpleDebugger.EnterFunc(Prog: TProgram; Expr: TExpr);
begin
  if Assigned(FOnEnterFunc) then
    FOnEnterFunc(Prog, Expr);
end;

procedure Tdws2SimpleDebugger.LeaveFunc(Prog: TProgram; Expr: TExpr);
begin
  if Assigned(FOnLeaveFunc) then
    FOnLeaveFunc(Prog, Expr);
end;

procedure Tdws2SimpleDebugger.StartDebug(MainProg: TProgram);
begin
  if Assigned(FOnStartDebug) then
    FOnStartDebug(MainProg);
end;

procedure Tdws2SimpleDebugger.StopDebug(MainProg: TProgram);
begin
  if Assigned(FOnStopDebug) then
    FOnStopDebug(MainProg);
end;

end.
