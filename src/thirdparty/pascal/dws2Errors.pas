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
{    Contributor(s): Andreas Luleich                                   }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Errors;

interface

uses
  Classes, SysUtils, dws2Strings, dws2Symbols;

type
  TMsgs = class;

  TSourceFile = class
  public
    SourceFile: string;
    SourceCode: string;
  end;

  TScriptPos = record
    Pos: Integer;
    Line: Integer;
    Col: Integer;
    SourceFile: TSourceFile;
  end;
  TScriptPosArray = array of TScriptPos; // dynamic array that can hold ScriptPos settings (needed for ReadNameList)

  Tdws2Msg = class
  private
    FMsgs: TMsgs;
    FText: string;
  public
    constructor Create(Msgs: TMsgs; Text: string);
    function AsInfo: string; virtual;
    function AsString: string; virtual;
  end;

  // Messages without position
  TInfoMsg = class(Tdws2Msg)
    function AsInfo: string; override;
  end;

  TErrorMsg = class(Tdws2Msg)
    function AsInfo: string; override;
  end;

  // Messages with position
  TScriptMsg = class(Tdws2Msg)
    Pos: TScriptPos;
    constructor Create(Msgs: TMsgs; Text: string; P: TScriptPos);
    function AsInfo: string; override;
  end;

  THintMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TWarningMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TCompilerErrorMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TExecutionErrorMsg = class(TScriptMsg)
    function AsInfo: string; override;
  end;

  TLastScriptError = record
    Pos : TScriptPos;
    ExceptObj : TObject;
  end;

  TMsgs = class
  private
    FSourceFiles: TList;
    FMessages: TList;
    FHasErrors: Boolean;
    FHasCompilerErrors: Boolean;
    FHasExecutionErrors: Boolean;
    FLastScriptError: TLastScriptError;
    function GetMsg(Index: Integer): Tdws2Msg;
    function GetMsgCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterSourceFile(const SourceFile: string; const SourceCode: string): TSourceFile;
    function GetSourceFile(SourceFile: string): TSourceFile;

    // Helper functions for IDE's
    function GetErrorLineStart(Pos: TScriptPos): Integer;
    function GetErrorLineEnd(Pos: TScriptPos): Integer;
    function GetErrorLine(Pos: TScriptPos): string;

    procedure AddInfo(const Text: string);
    procedure AddError(const Text: string);
    procedure AddErrorStop(const Text: string);

    // Called in compiler
    procedure AddCompilerInfo(const Text: string);
    procedure AddCompilerHint(Pos: TScriptPos; const Text: string);
    procedure AddCompilerWarning(Pos: TScriptPos; const Text: string);
    procedure AddCompilerError(const Text: string); overload;
    procedure AddCompilerError(Pos: TScriptPos; const Text: string); overload;
    procedure AddCompilerStop(Pos: TScriptPos; const Text: string);

    // Called during execution
    procedure AddExecutionError(const Text: string); overload;
    procedure AddExecutionError(Pos: TScriptPos; const Text: string); overload;
    procedure AddExecutionStop(Pos: TScriptPos; const Text: string);

    procedure SetScriptError(const Pos: TScriptPos; ExceptObj : TObject = nil);

    procedure Clear;

    function AsInfo: string;
    function AsString: string;

    property Msgs[Index: Integer]: Tdws2Msg read GetMsg; default;
    property Count: Integer read GetMsgCount;
    property HasErrors: Boolean read FHasErrors;
    property HasCompilerErrors: Boolean read FHasCompilerErrors;
    property HasExecutionErrors: Boolean read FHasExecutionErrors;
  end;

  // The script initialization failed because a class needs one or more methods
  // to be implemented.
  EClassIncompleteError = class(Exception)
  private
    FClassSymObj: TObject;   // object that refers to the TClassSymbol
  public
    property ClassSymObj: TObject read FClassSymObj write FClassSymObj;
  end;

  EClassMethodImplIncompleteError = class(EClassIncompleteError);
  EClassPropertyIncompleteError = class(EClassIncompleteError);

  // The script has to be stopped because of an error
  EScriptError = class(Exception);

  EFlowControl = class(Exception);
  EExit = class(EFlowControl); // Leaves the actual procedure
  EBreak = class(EFlowControl); // Leaves the current loop
  EContinue = class(EFlowControl); // Continues with the next cycle of a loop
  EReraise = class(Exception);

  // Is thrown by "raise" statements in script code
  EScriptException = class(Exception)
  private
    FTyp: TSymbol;
    FValue: Variant;
    FPos: TScriptPos;
  public
    constructor Create(Message: string; ExceptionObj: IScriptObj; Pos: TScriptPos); overload;
    constructor Create(Message: string; Value: Variant; Typ: TSymbol; Pos: TScriptPos); overload;
    property ExceptionObj: Variant read FValue;
    property Value: Variant read FValue;
    property Typ: TSymbol read FTyp;
    property Pos: TScriptPos read FPos;
  end;

const
  NullPos: TScriptPos = (Pos: - 1; Line: - 1; Col: - 1; SourceFile: nil);

implementation

const
  NoScriptError: TLastScriptError
    = (Pos: (Pos: - 1; Line: - 1; Col: - 1; SourceFile: nil); ExceptObj: nil);

{ TMsgs }

constructor TMsgs.Create;
begin
  FSourceFiles := TList.Create;
  FMessages := TList.Create;
  FLastScriptError := NoScriptError;
end;

destructor TMsgs.Destroy;
var
  x: Integer;
begin
  Clear;
  for x := 0 to FSourceFiles.Count - 1 do
    TSourceFile(FSourceFiles[x]).Free;
  FSourceFiles.Free;
  FMessages.Free;
  inherited;
end;

function TMsgs.RegisterSourceFile(const SourceFile: string; const SourceCode:
  string): TSourceFile;
var
  sf: TSourceFile;
begin
  sf := GetSourceFile(SourceFile);
  if not Assigned(sf) or (sf.SourceCode <> SourceCode) then
  begin
    Result := TSourceFile.Create;
    if SourceFile = MSG_MainModule then
      Result.SourceFile := MSG_MainModule
    else
      Result.SourceFile := AnsiLowerCase(ExpandFileName(SourceFile));
    Result.SourceCode := SourceCode;
    FSourceFiles.Add(Result);
  end
  else
    Result := sf;
end;

function TMsgs.GetSourceFile(SourceFile: string): TSourceFile;
var
  x: Integer;
begin
  Result := nil;
  if SourceFile <> MSG_MainModule then
    SourceFile := AnsiLowerCase(ExpandFileName(SourceFile));
  for x := 0 to FSourceFiles.Count - 1 do
    if TSourceFile(FSourceFiles[x]).SourceFile = SourceFile then
    begin
      Result := TSourceFile(FSourceFiles[x]);
      exit;
    end;
end;

function TMsgs.GetErrorLineStart(Pos: TScriptPos): Integer;
var
  s: string;
  x: Integer;
begin
  if Pos.Pos < 0 then
    Result := -1
  else
  begin
    s := Pos.SourceFile.SourceCode;
    Result := 1;

    for x := Pos.Pos downto 1 do
      if s[x] = #10 then
      begin
        Result := x + 1;
        exit;
      end;
  end;
end;

function TMsgs.GetErrorLineEnd(Pos: TScriptPos): Integer;
var
  s: string;
  x: Integer;
begin
  if Pos.Pos < 0 then
    Result := -1
  else
  begin
    s := Pos.SourceFile.SourceCode;
    Result := Length(s);

    for x := Pos.Pos to Length(s) do
      if s[x] = #10 then
      begin
        Result := x - 1;
        exit;
      end;
  end;
end;

function TMsgs.GetErrorLine(Pos: TScriptPos): string;
var
  Start: Integer;
begin
  if Pos.Pos = -1 then
    Result := ''
  else
  begin
    Start := GetErrorLineStart(Pos);
    Result := Copy(Pos.SourceFile.SourceCode, Start, GetErrorLineEnd(Pos) - Start +
      1);
  end;
end;

procedure TMsgs.AddInfo;
begin
  FMessages.Add(TInfoMsg.Create(Self, Text));
end;

procedure TMsgs.AddError(const Text: string);
begin
  FMessages.Add(TErrorMsg.Create(Self, Text));
  FHasErrors := True;
end;

procedure TMsgs.AddErrorStop(const Text: string);
begin
  AddError(Text);
  raise EScriptError.Create('')
end;

procedure TMsgs.AddCompilerInfo;
begin
  FMessages.Add(TInfoMsg.Create(Self, Text));
end;

procedure TMsgs.AddCompilerHint(Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(THintMsg.Create(Self, Text, Pos));
end;

procedure TMsgs.AddCompilerWarning(Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(TWarningMsg.Create(Self, Text, Pos));
end;

procedure TMsgs.AddCompilerError(Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(TCompilerErrorMsg.Create(Self, Text, Pos));
  FHasCompilerErrors := True;
end;

procedure TMsgs.AddCompilerError(const Text: string);
begin
  AddCompilerError(NullPos, Text);
end;

procedure TMsgs.AddCompilerStop(Pos: TScriptPos; const Text: string);
begin
  AddCompilerError(Pos, Text);
  raise EScriptError.Create('')
end;

procedure TMsgs.AddExecutionError(Pos: TScriptPos; const Text: string);
begin
  FMessages.Add(TExecutionErrorMsg.Create(Self, Text, Pos));
  FHasExecutionErrors := True;
end;

procedure TMsgs.AddExecutionError(const Text: string);
begin
  if Assigned(FLastScriptError.ExceptObj)
     and (FLastScriptError.ExceptObj = ExceptObject) then
    AddExecutionError(FLastScriptError.Pos, Text)
  else
    AddExecutionError(NullPos, Text);
end;

procedure TMsgs.AddExecutionStop(Pos: TScriptPos; const Text: string);
begin
  AddExecutionError(Pos, Text);
  raise EScriptError.Create('');
end;

procedure TMsgs.Clear;
var
  x: Integer;
begin
  for x := 0 to FMessages.Count - 1 do
    Tdws2Msg(FMessages[x]).Free;
  FMessages.Clear;
  FHasErrors := False;
  FHasCompilerErrors := False;
  FHasExecutionErrors := False;
  FLastScriptError := NoScriptError;
end;

function TMsgs.GetMsg(Index: Integer): Tdws2Msg;
begin
  Result := FMessages[Index];
end;

function TMsgs.GetMsgCount: Integer;
begin
  Result := FMessages.Count;
end;

procedure TMsgs.SetScriptError(const Pos: TScriptPos; ExceptObj : TObject);
begin
  // new exception or non-NullPos if same Exception
  if (FLastScriptError.ExceptObj <> ExceptObj)
     or (FLastScriptError.Pos.Pos = -1) and Assigned(FLastScriptError.ExceptObj) then
  begin
    FLastScriptError.Pos := Pos;
    FLastScriptError.ExceptObj := ExceptObj;
  end;
end;

function TMsgs.AsInfo: string;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to Count - 1 do
    Result := Result + Msgs[x].AsInfo + #13#10
end;

function TMsgs.AsString: string;
var
  x: Integer;
begin
  Result := '';
  for x := 0 to Count - 1 do
    Result := Result + Msgs[x].AsString + #13#10
end;

{ Tdws2Msg }

function Tdws2Msg.AsInfo: string;
begin
  Result := FText;
end;

function Tdws2Msg.AsString: string;
begin
  Result := FText;
end;

constructor Tdws2Msg.Create;
begin
  FMsgs := Msgs;
  FText := Text;
end;

{ TInfoMsg }

function TInfoMsg.AsInfo: string;
begin
  Result := Format(MSG_Info, [inherited AsInfo]);
end;

{ TErrorMsg }

function TErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_Error, [inherited AsInfo]);
end;

{ TScriptMsg }

function TScriptMsg.AsInfo: string;
begin
  if (Pos.Line = NullPos.Line) and (Pos.Col = NullPos.Col) then
    Result := FText
  else if not Assigned(Pos.SourceFile) or (Pos.SourceFile.SourceFile = MSG_MainModule) then
    Result := Format(MSG_ScriptMsg, [FText, Pos.Line, Pos.Col])
  else
    Result := Format(MSG_ScriptMsgLong, [FText, Pos.Line, Pos.Col,
      Pos.SourceFile.SourceFile])
end;

constructor TScriptMsg.Create;
begin
  inherited Create(Msgs, Text);
  Pos := P;
end;

{ THintMsg }

function THintMsg.AsInfo: string;
begin
  Result := Format(MSG_Hint, [inherited AsInfo]);
end;

{ TWarningMsg }

function TWarningMsg.AsInfo: string;
begin
  Result := Format(MSG_Warning, [inherited AsInfo]);
end;

{ TErrorMsg }

function TCompilerErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_SyntaxError, [inherited AsInfo]);
end;

{ TRuntimeErrorMsg }

function TExecutionErrorMsg.AsInfo: string;
begin
  Result := Format(MSG_RuntimeError, [inherited AsInfo]);
end;

{ EScriptException }

constructor EScriptException.Create(Message: string; Value: Variant;
  Typ: TSymbol; Pos: TScriptPos);
begin
  inherited Create(Message);
  FValue := Value;
  FTyp := Typ;
  FPos := Pos;
end;

constructor EScriptException.Create(Message: string;
  ExceptionObj: IScriptObj; Pos: TScriptPos);
begin
  Create(Message,ExceptionObj,ExceptionObj.ClassSym,Pos);
end;

end.
