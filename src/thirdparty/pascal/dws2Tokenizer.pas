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
{    Contributor(s): Martin Waldenburg                                 }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Tokenizer;

interface

uses
  SysUtils, Classes, dws2Errors;

type
  TTokenType =
    (ttNone, ttStrVal, ttIntVal, ttFloatVal, ttNAME, ttSWITCH,
    ttVAR, ttCONST, ttTYPE, ttRECORD, ttARRAY, ttDOT, ttDOTDOT, ttOF,
    ttTRY, ttEXCEPT, ttRAISE, ttFINALLY, ttON, ttREAD, ttWRITE, ttPROPERTY,
    ttPROCEDURE, ttFUNCTION, ttCONSTRUCTOR, ttDESTRUCTOR, ttCLASS, ttNIL, ttIS,
    ttAS, ttINDEX, ttOBJECT,
    ttVIRTUAL, ttOVERRIDE, ttREINTRODUCE, ttINHERITED, ttABSTRACT,
    ttEXTERNAL, ttFORWARD, ttIN,
    ttBEGIN, ttEND, ttBREAK, ttCONTINUE, ttEXIT,
    ttIF, ttTHEN, ttELSE, ttWHILE, ttREPEAT, ttUNTIL, ttFOR, ttTO, ttDOWNTO, ttDO,
    ttCASE,
    ttTRUE, ttFALSE, ttAND, ttOR, ttXOR, ttDIV, ttMOD, ttNOT, ttPLUS, ttMINUS,
    ttTIMES, ttDIVIDE,
    ttEQ, ttNOTEQ, ttGTR, ttGTREQ, ttLESS, ttLESSEQ, ttSEMI, ttCOMMA, ttCOLON,
    ttASSIGN,
    ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttCRIGHT,
    ttDEFAULT, ttUSES,

    // Tokens for compatibility to Delphi
    ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
    ttREGISTER, ttPASCAL, ttCDECL, ttSTDCALL, ttFASTCALL);

  TTokenTypes = set of TTokenType;

  TToken = class
    FTyp: TTokenType;
    FPos: TScriptPos;
    FString: string;
    FFloat: Double;
    FInteger: Integer;
    constructor Create;
  end;

  TCharsType = set of char;
  TTransition = class;

  TState = class
    FTransitions: TList;
    FElseTransition: TTransition;
    constructor Create;
    destructor Destroy; override;
    function FindTransition(c: char): TTransition;
    procedure AddTransition(o: TTransition);
    procedure SetElse(o: TTransition);
  end;

  TConvertAction = (caNone, caClear, caName, caHex, caInteger, caFloat, caChar,
    caCharHex, caString, caSwitch, caDotDot);
  TTransitionOptions = set of (toStart, toFinal);

  TTransition = class
    Chars: set of char;
    NextState: TState;
    Start: Boolean; // Marks the begin of a Token
    Final: Boolean; // Marks the end of a Token
    Action: TConvertAction;
    constructor Create(chrs: TCharsType; nstate: TState; opts: TTransitionOptions;
      actn: TConvertAction);
  end;

  TElseTransition = class(TTransition)
    constructor Create(actn: TConvertAction);
  end;

  TErrorTransition = class(TTransition)
    ErrorMessage: string;
    constructor Create(msg: string);
  end;

  TCheckTransition = class(TTransition);
  TSeekTransition = class(TCheckTransition); // Transition, next char
  TConsumeTransition = class(TSeekTransition);
  // Transition, consume char, next char

  TSwitchHandler = function(SwitchName: string): Boolean of object;

  TTokenizer = class
  private
    FDefaultPos: TScriptPos;
    FHotPos: TScriptPos;
    FMsgs: TMsgs;
    FNextToken: TToken;
    FPos: TScriptPos;
    FStartState: TState;
    FSwitchHandler: TSwitchHandler;
    FText: string;
    FToken: TToken;
    function ConsumeToken: TToken; virtual;
    function NameToType(const name: string): TTokenType; virtual;
    procedure ReadToken;
    procedure ReadNextToken;
  public
    constructor Create(const Text, SourceFile: string; Msgs: TMsgs); virtual;
    destructor Destroy; override;
    function GetToken: TToken;
    function HasTokens: Boolean;
    procedure KillToken;
    function NextTest(t: TTokenType): Boolean;
    function Test(t: TTokenType): Boolean;
    function TestDelete(t: TTokenType): Boolean;
    function NextTestName: Boolean;
    function TestName: Boolean;
    function TestDeleteName: Boolean;
    property DefaultPos: TScriptPos read FDefaultPos;
    property HotPos: TScriptPos read FHotPos;
    property CurrentPos: TScriptPos read FPos;
    property SwitchHandler: TSwitchHandler read FSwitchHandler write
      FSwitchHandler;
  end;

implementation

uses
  dws2Strings;

const ReservedNames : TTokenTypes = [
  ttStrVal, ttSWITCH, ttSEMI, ttDIVIDE, ttTIMES, ttPLUS, ttMINUS, ttSEMI,
  ttBLEFT, ttBRIGHT, ttALEFT, ttARIGHT, ttEQ, ttLESS, ttLESSEQ, ttNOTEQ, ttGTR,
  ttGTREQ, ttCOLON, ttASSIGN, ttCOMMA, ttCRIGHT, ttDOT ];

{ TToken }

constructor TToken.Create;
begin
  FTyp := ttNone;
  FPos := NullPos;
end;

{ TState }

constructor TState.Create;
begin
  FTransitions := TList.Create;
end;

destructor TState.Destroy;
var
  x: Integer;
begin
  for x := 0 to FTransitions.Count - 1 do
    TTransition(FTransitions[x]).Free;
  FTransitions.Free;
  FElseTransition.Free;
  inherited Destroy;
end;

function TState.FindTransition(c: char): TTransition;
var
  x: Integer;
begin
  Result := nil;
  for x := 0 to FTransitions.Count - 1 do
    if c in TTransition(FTransitions[x]).Chars then
    begin
      Result := FTransitions[x];
      exit;
    end;

  if not Assigned(Result) then
    Result := FElseTransition;
end;

procedure TState.AddTransition(o: TTransition);
begin
  FTransitions.Add(o);
end;

procedure TState.SetElse(o: TTransition);
begin
  FElseTransition := o;
end;

{ TTransition }

constructor TTransition.Create;
begin
  Chars := chrs;
  NextState := nstate;
  Start := toStart in opts;
  Final := toFinal in opts;
  Action := actn;
end;

{ TElseTransition }

constructor TElseTransition.Create(actn: TConvertAction);
begin
  Chars := [];
  NextState := nil;
  Start := false;
  Action := actn;
end;

{ TErrorTransition }

constructor TErrorTransition.Create(msg: string);
begin
  ErrorMessage := msg;
end;

var
  sStart, sSpace, sComment, sCommentF, sSlashComment, sSlashComment0: TState;
  sSwitch, sSwitchNameF, sChar0, sCharF, sCharHex, sCharHexF: TState;
  sNameF: TState;
  sIntF, sIntPoint, sIntPointF, sIntExp, sIntExp0, sIntExpF, sHex, sHexF: TState;
  sString0, sStringF, sAssign0: TState;
  sGreaterF, sSmallerF, sDotDot: TState;

  { TTokenizer }

constructor TTokenizer.Create;
begin
{$IFDEF LINUX}
  FText := Text + #10#0;
{$ELSE}
  FText := Text + #13#10#0;
{$ENDIF}
  FToken := nil;
  FMsgs := Msgs;
  FNextToken := nil;
  FDefaultPos := NullPos;
  FDefaultPos.SourceFile := FMsgs.RegisterSourceFile(SourceFile, Text);
  FHotPos := FDefaultPos;
  FPos := FDefaultPos;
  FPos.Pos := 1;
  FPos.Line := 1;
  FPos.Col := 1;
  FStartState := sStart;
end;

destructor TTokenizer.Destroy;
begin
  FToken.Free;
  FNextToken.Free;
  inherited;
end;

procedure TTokenizer.ReadToken;
begin
  KillToken;

  if Assigned(FNextToken) then
  begin
    FToken := FNextToken;
    FNextToken := nil;
  end
  else
    FToken := ConsumeToken;
end;

procedure TTokenizer.ReadNextToken;
begin
  if not Assigned(FNextToken) then
    FNextToken := ConsumeToken;
end;

function TTokenizer.GetToken: TToken;
begin
  Result := FToken;
end;

function TTokenizer.Test(t: TTokenType): Boolean;
begin
  Result := false;
  if not Assigned(FToken) then
    ReadToken;
  if Assigned(FToken) then
  begin
    Result := (FToken.FTyp = t);
    FHotPos := FToken.FPos;
  end;
end;

function TTokenizer.TestDelete(t: TTokenType): Boolean;
begin
  Result := Test(t);
  if Result then
    KillToken;
end;

function TTokenizer.NextTest(t: TTokenType): Boolean;
begin
  Result := false;
  ReadNextToken;
  if Assigned(FNextToken) then
    Result := (FNextToken.FTyp = t);
end;

function TTokenizer.TestName: Boolean;
begin
  Result := false;
  if not Assigned(FToken) then
    ReadToken;
  if Assigned(FToken) then
  begin
    Result := (FToken.FString <> '') and not (FToken.FTyp in ReservedNames);
    FHotPos := FToken.FPos;
  end;
end;

function TTokenizer.TestDeleteName: Boolean;
begin
  Result := TestName;
  if Result then
    KillToken;
end;

function TTokenizer.NextTestName: Boolean;
begin
  Result := false;
  ReadNextToken;
  if Assigned(FNextToken) then
    Result := (FNextToken.FString <> '') and not (FToken.FTyp in ReservedNames);
end;

function TTokenizer.HasTokens: Boolean;
begin
  if not Assigned(FToken) then
    ReadToken;
  Result := FToken <> nil;
end;

function TTokenizer.ConsumeToken: TToken;
var
  state: TState;
  trns: TTransition;
  s: string;
  ch, oldsep: char;
begin
  Result := TToken.Create;
  state := FStartState;
  s := '';

  try
    // Look for the next token in FText
    while (FPos.Pos <= Length(FText)) and Assigned(state) do
    begin

      // Next character
      ch := FText[FPos.Pos];

      // Find next state
      trns := state.FindTransition(ch);

      // Handle Errors
      if trns is TErrorTransition then
        FMsgs.AddCompilerStop(FPos,
          Format('%s ("%s")', [TErrorTransition(trns).ErrorMessage, ch]));

      // A new token begins
      if trns.Start and (Result.FPos.Pos = -1) then
        Result.FPos := FPos;

      // Add actual character to s
      if trns is TConsumeTransition then
        s := s + ch;

      // Proceed to the next character
      if trns is TSeekTransition then
      begin
        Inc(FPos.Pos);
        Inc(FPos.Col);
        if ch = #10 then
        begin
          Inc(FPos.Line);
          FPos.Col := 1;
        end;
      end;

      // The characters in 's' have to be converted
      if trns.Action <> caNone then
      begin
        case trns.Action of
          caClear:
            begin
              s := '';
              Result.FPos := DefaultPos;
            end;

          // Convert name to token
          caName:
            begin
              Result.FTyp := NameToType(s);
              Result.FString := s;
            end;

          // converts ASCII code to character
          caChar:
            begin
              try
                if (StrToInt(s) > $FF) or (StrToInt(s) < 0) then
                  raise Exception.Create('');
                Result.FString := Result.FString + Chr(Byte(StrToInt(s)));
                Result.FTyp := ttStrVal;
              except
                FMsgs.AddCompilerStop(FPos, Format(TOK_InvalidCharConstant, [s]));
              end;
            end;

          // Converts hex constant to character
          caCharHex:
            begin
              try
                if (StrToInt(s) > $FF) or (StrToInt(s) < 0) then
                  raise Exception.Create('');
                Result.FString := Result.FString + Chr(Byte(StrToInt(s)));
                Result.FTyp := ttStrVal;
              except
                FMsgs.AddCompilerStop(FPos, Format(TOK_InvalidCharConstant, [s]));
              end;
            end;

          // Concatenates the parts of a string constant
          caString:
            begin
              Result.FString := Result.FString + s;
              Result.FTyp := ttStrVal;
            end;

          // Converts hexadecimal number to integer
          caHex:
            begin
              try
                Result.FInteger := StrToInt(s);
                Result.FTyp := ttIntVal;
              except
                on e: Exception do
                  FMsgs.AddCompilerStop(FPos, Format(TOK_InvalidHexConstant, [s]));
              end;
            end;

          // Converts integer constants
          caInteger:
            begin
              try
                if s[Length(s)] = '.' then
                  Delete(s, Length(s), 1);
                Result.FInteger := StrToInt(s);
                Result.FTyp := ttIntVal;
              except
                on e: Exception do
                  FMsgs.AddCompilerStop(FPos, Format(TOK_InvalidIntegerConstant,
                    [s]));
              end;
            end;

          // converts Floating Point numbers
          caFloat:
            begin
              try
                oldsep := DecimalSeparator;
                DecimalSeparator := '.';
                Result.FFloat := StrToFloat(s);
                Result.FTyp := ttFloatVal;
                DecimalSeparator := oldsep;
              except
                on e: Exception do
                  FMsgs.AddCompilerStop(FPos, Format(TOK_InvalidFloatConstant, [s]));
              end;
            end;

          caSwitch:
            if Assigned(FSwitchHandler) then
            begin
              FHotPos := Result.FPos;

              // Ask parser if we should create a token or not
              if FSwitchHandler(UpperCase(s)) then
              begin
                Result.FTyp := ttSWITCH;
                Result.FString := UpperCase(s);
              end
              else
              begin
                state := sStart;
                s := '';
                Continue;
              end;
            end;

          caDotDot:
            Result.FTyp := ttDOTDOT;
        end;
        s := '';
      end;

      // If the token is complete then exit
      if trns.Final then
      begin
        FStartState := trns.NextState;
        exit;
      end
      else
        state := trns.NextState;
    end;
  except
    Result.Free;
    raise;
  end;

  // Couldn't read a token (end of FText reached)
  Result.Free;
  Result := nil;
end;

function TTokenizer.NameToType(const name: string): TTokenType;
var
  n: string;
begin
  n := UpperCase(name);
  Result := ttNAME;
  case n[1] of
    '/': Result := ttDIVIDE;
    '*': Result := ttTIMES;
    '+': Result := ttPLUS;
    '-': Result := ttMINUS;
    ';': Result := ttSEMI;
    '(': Result := ttBLEFT;
    ')': Result := ttBRIGHT;
    '[': Result := ttALEFT;
    ']': Result := ttARIGHT;
    '=': Result := ttEQ;
    '<':
      if n = '<' then
        Result := ttLESS
      else if n = '<=' then
        Result := ttLESSEQ
      else if n = '<>' then
        Result := ttNOTEQ;
    '>':
      if n = '>' then
        Result := ttGTR
      else if n = '>=' then
        Result := ttGTREQ;
    ':':
      if n = ':' then
        Result := ttCOLON
      else if n = ':=' then
        Result := ttASSIGN;
    ',': Result := ttCOMMA;
    '}': Result := ttCRIGHT;
    '.':
      if n = '.' then
        Result := ttDOT;
    'A':
      if n = 'AND' then
        Result := ttAND
      else if n = 'ARRAY' then
        Result := ttARRAY
      else if n = 'ABSTRACT' then
        Result := ttABSTRACT
      else if n = 'AS' then
        Result := ttAS;
    'B':
      if n = 'BEGIN' then
        Result := ttBEGIN
      else if n = 'BREAK' then
        Result := ttBREAK;
    'C':
      if n = 'CONST' then
        Result := ttCONST
      else if n = 'CLASS' then
        Result := ttCLASS
      else if n = 'CONSTRUCTOR' then
        Result := ttCONSTRUCTOR
      else if n = 'CONTINUE' then
        Result := ttCONTINUE
      else if n = 'CASE' then
        Result := ttCASE
      else if n = 'CDECL' then
        Result := ttCDECL;
    'D':
      if n = 'DO' then
        Result := ttDO
      else if n = 'DOWNTO' then
        Result := ttDOWNTO
      else if n = 'DIV' then
        Result := ttDIV
      else if n = 'DEFAULT' then
        Result := ttDEFAULT
      else if n = 'DESTRUCTOR' then
        Result := ttDESTRUCTOR;
    'E':
      if n = 'END' then
        Result := ttEND
      else if n = 'ELSE' then
        Result := ttELSE
      else if n = 'EXCEPT' then
        Result := ttEXCEPT
      else if n = 'EXIT' then
        Result := ttEXIT
      else if n = 'EXTERNAL' then
        Result := ttEXTERNAL;
    'F':
      if n = 'FOR' then
        Result := ttFOR
      else if n = 'FALSE' then
        Result := ttFALSE
      else if n = 'FUNCTION' then
        Result := ttFUNCTION
      else if n = 'FINALLY' then
        Result := ttFINALLY
      else if n = 'FASTCALL' then
        Result := ttFASTCALL
      else if n = 'FORWARD' then
        Result := ttFORWARD;
    'I':
      if n = 'IF' then
        Result := ttIF
      else if n = 'IN' then
        Result := ttIN
      else if n = 'IS' then
        Result := ttIS
      else if n = 'INHERITED' then
        Result := ttINHERITED
      else if n = 'INDEX' then
        Result := ttINDEX;
    'M':
      if n = 'MOD' then
        Result := ttMOD;
    'N':
      if n = 'NOT' then
        Result := ttNOT
      else if n = 'NIL' then
        Result := ttNIL;
    'O':
      if n = 'OR' then
        Result := ttOR
      else if n = 'OF' then
        Result := ttOF
      else if n = 'ON' then
        Result := ttON
      else if n = 'OVERRIDE' then
        Result := ttOVERRIDE
      else if n = 'OBJECT' then
        Result := ttOBJECT;
    'P':
      if n = 'PROCEDURE' then
        Result := ttPROCEDURE
      else if n = 'PROPERTY' then
        Result := ttPROPERTY
      else if n = 'PASCAL' then
        Result := ttPASCAL
      else if n = 'PRIVATE' then
        Result := ttPRIVATE
      else if n = 'PROTECTED' then
        Result := ttPROTECTED
      else if n = 'PUBLIC' then
        Result := ttPUBLIC
      else if n = 'PUBLISHED' then
        Result := ttPUBLISHED;
    'R':
      if n = 'REPEAT' then
        Result := ttREPEAT
      else if n = 'RECORD' then
        Result := ttRECORD
      else if n = 'READ' then
        Result := ttREAD
      else if n = 'RAISE' then
        Result := ttRAISE
      else if n = 'REINTRODUCE' then
        Result := ttREINTRODUCE
      else if n = 'REGISTER' then
        Result := ttREGISTER;
    'S':
      if n = 'STDCALL' then
        Result := ttSTDCALL;
    'T':
      if n = 'THEN' then
        Result := ttTHEN
      else if n = 'TO' then
        Result := ttTO
      else if n = 'TRUE' then
        Result := ttTRUE
      else if n = 'TYPE' then
        Result := ttTYPE
      else if n = 'TRY' then
        Result := ttTRY;
    'U':
      if n = 'UNTIL' then
        Result := ttUNTIL
      else if n = 'USES' then
        Result := ttUSES;
    'V':
      if n = 'VAR' then
        Result := ttVAR
      else if n = 'VIRTUAL' then
        Result := ttVIRTUAL;
    'W':
      if n = 'WHILE' then
        Result := ttWHILE
      else if n = 'WRITE' then
        Result := ttWRITE;
    'X':
      if n = 'XOR' then
        Result := ttXOR;
  end;
end;

const
  OPS = ['+', '-', '*', '/', '=', '<', '>'];
  SPACE = [' ', #9, #13, #10, #0];
  SPEC = ['(', ')', ',', ';', '[', ']', '}'];
  STOP = SPEC + OPS + SPACE + [':', '%', '.', '{'];
  STR = [#0..#255];
  NAM = ['A'..'Z', 'a'..'z', '_'];
  INT = ['0'..'9'];
  HEX = INT + ['A'..'F', 'a'..'f'];
  Start = ['''', '#', ':', '$', '.'] + NAM + INT + OPS;

procedure TTokenizer.KillToken;
begin
  FToken.Free;
  FToken := nil;
end;

initialization
  sStart := TState.Create;
  sComment := TState.Create;
  sCommentF := TState.Create;
  sSwitch := TState.Create;
  sSwitchNameF := TState.Create;
  sSlashComment0 := TState.Create;
  sSlashComment := TState.Create;
  sChar0 := TState.Create;
  sCharF := TState.Create;
  sCharHex := TState.Create;
  sCharHexF := TState.Create;
  sNameF := TState.Create;
  sIntF := TState.Create;
  sIntPoint := TState.Create;
  sIntPointF := TState.Create;
  sIntExp := TState.Create;
  sIntExp0 := TState.Create;
  sIntExpF := TState.Create;
  sHex := TState.Create;
  sHexF := TState.Create;
  sString0 := TState.Create;
  sStringF := TState.Create;
  sAssign0 := TState.Create;
  sGreaterF := TState.Create;
  sSmallerF := TState.Create;
  sDotDot := TState.Create;

  sStart.AddTransition(TSeekTransition.Create(SPACE, sStart, [], caNone));
  sStart.AddTransition(TConsumeTransition.Create(NAM, sNameF, [toStart], caNone));
  sStart.AddTransition(TConsumeTransition.Create(INT, sIntF, [toStart], caNone));
  sStart.AddTransition(TSeekTransition.Create([''''], sString0, [toStart],
    caNone));
  sStart.AddTransition(TSeekTransition.Create(['#'], sChar0, [toStart], caNone));
  sStart.AddTransition(TConsumeTransition.Create([':'], sAssign0, [toStart],
    caNone));
  sStart.AddTransition(TConsumeTransition.Create(['+', '-', '*', '='], sStart,
    [toStart, toFinal], caName));
  sStart.AddTransition(TConsumeTransition.Create([']'], sStart, [toStart, toFinal],
    caName));
  sStart.AddTransition(TConsumeTransition.Create(SPEC, sStart, [toStart, toFinal],
    caName));
  sStart.AddTransition(TConsumeTransition.Create(['/'], sSlashComment0, [toStart],
    caNone));
  sStart.AddTransition(TConsumeTransition.Create(['<'], sSmallerF, [toStart],
    caNone));
  sStart.AddTransition(TConsumeTransition.Create(['>'], sGreaterF, [toStart],
    caNone));
  sStart.AddTransition(TConsumeTransition.Create(['.'], sDotDot, [toStart],
    caNone));
  sStart.AddTransition(TSeekTransition.Create(['{'], sComment, [], caNone));
  sStart.AddTransition(TConsumeTransition.Create(['$'], sHex, [toStart], caNone));
  sStart.SetElse(TErrorTransition.Create(TOK_InvalidChar));

  sComment.AddTransition(TSeekTransition.Create(['}'], sStart, [], caClear));
  sComment.AddTransition(TSeekTransition.Create(['$'], sSwitch, [], caNone));
  sComment.SetElse(TSeekTransition.Create([], sCommentF, [], caNone));

  sCommentF.AddTransition(TSeekTransition.Create(['}'], sStart, [], caClear));
  sCommentF.SetElse(TSeekTransition.Create([], sCommentF, [], caNone));

  sSwitch.AddTransition(TConsumeTransition.Create(NAM, sSwitchNameF, [toStart],
    caNone));
  sSwitch.SetElse(TErrorTransition.Create(TOK_NameOfSwitchExpected));

  sSwitchNameF.AddTransition(TConsumeTransition.Create(NAM + INT, sSwitchNameF, [],
    caNone));
  sSwitchNameF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal],
    caSwitch));
  sSwitchNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

  sSlashComment0.AddTransition(TSeekTransition.Create(['/'], sSlashComment, [],
    caNone));
  sSlashComment0.SetElse(TCheckTransition.Create([], sStart, [toFinal], caName));

  sSlashComment.AddTransition(TSeekTransition.Create([#0, #10], sStart, [],
    caClear));
  sSlashComment.SetElse(TSeekTransition.Create([], sSlashComment, [], caNone));

  sChar0.AddTransition(TConsumeTransition.Create(INT, sCharF, [], caNone));
  sChar0.AddTransition(TConsumeTransition.Create(['$'], sCharHex, [], caNone));
  sChar0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

  sCharF.AddTransition(TConsumeTransition.Create(INT, sCharF, [], caNone));
  sCharF.AddTransition(TCheckTransition.Create([''''], sStart, [], caChar));
  sCharF.AddTransition(TCheckTransition.Create(['#'], sStart, [], caChar));
  sCharF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal], caChar));
  sCharF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

  sCharHex.AddTransition(TConsumeTransition.Create(HEX, sCharHexF, [], caNone));
  sCharHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

  sCharHexF.AddTransition(TConsumeTransition.Create(HEX, sCharHexF, [], caNone));
  sCharHexF.AddTransition(TCheckTransition.Create([''''], sStart, [], caCharHex));
  sCharHexF.AddTransition(TCheckTransition.Create(['#'], sStart, [], caCharHex));
  sCharHexF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal],
    caCharHex));
  sCharHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

  sNameF.AddTransition(TConsumeTransition.Create(NAM + INT, sNameF, [], caNone));
  sNameF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal], caName));
  sNameF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

  sIntF.AddTransition(TConsumeTransition.Create(INT, sIntF, [], caNone));
  sIntF.AddTransition(TConsumeTransition.Create(['.'], sIntPoint, [], caNone));
  sIntF.AddTransition(TConsumeTransition.Create(['E', 'e'], sIntExp, [], caNone));
  sIntF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal], caInteger));
  sIntF.SetElse(TErrorTransition.Create(TOK_NumberPointExponentExpected));

  sIntPoint.AddTransition(TCheckTransition.Create(['.'], sDotDot, [toFinal],
    caInteger));
  sIntPoint.AddTransition(TConsumeTransition.Create(INT, sIntPointF, [], caNone));
  sIntPoint.SetElse(TErrorTransition.Create(TOK_NumberExpected));

  sIntPointF.AddTransition(TConsumeTransition.Create(INT, sIntPointF, [], caNone));
  sIntPointF.AddTransition(TConsumeTransition.Create(['E', 'e'], sIntExp, [],
    caNone));
  sIntPointF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal],
    caFloat));
  sIntPointF.SetElse(TErrorTransition.Create(TOK_NumberExponentExpected));

  sIntExp.AddTransition(TConsumeTransition.Create(['+', '-'], sIntExp0, [],
    caNone));
  sIntExp.AddTransition(TConsumeTransition.Create(INT, sIntExpF, [], caNone));
  sIntExp.SetElse(TErrorTransition.Create(TOK_NumberSignExpected));

  sIntExp0.AddTransition(TConsumeTransition.Create(INT, sIntExpF, [], caNone));
  sIntExp0.SetElse(TErrorTransition.Create(TOK_NumberExpected));

  sIntExpF.AddTransition(TConsumeTransition.Create(INT, sIntExpF, [], caNone));
  sIntExpF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal],
    caFloat));
  sIntExpF.SetElse(TErrorTransition.Create(TOK_NumberExpected));

  sHex.AddTransition(TConsumeTransition.Create(HEX, sHexF, [], caNone));
  sHex.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

  sHexF.AddTransition(TConsumeTransition.Create(HEX, sHexF, [], caNone));
  sHexF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal], caHex));
  sHexF.SetElse(TErrorTransition.Create(TOK_HexDigitExpected));

  sString0.AddTransition(TConsumeTransition.Create(STR - ['''', #13, #10],
    sString0,
    [], caNone));
  sString0.AddTransition(TSeekTransition.Create([''''], sStringF, [], caNone));
  sString0.SetElse(TErrorTransition.Create(TOK_StringTerminationError));

  sStringF.AddTransition(TConsumeTransition.Create([''''], sString0, [], caNone));
  sStringF.AddTransition(TCheckTransition.Create(['#'], sStart, [], caString));
  sStringF.AddTransition(TCheckTransition.Create(STOP, sStart, [toFinal],
    caString));
  sStringF.SetElse(TErrorTransition.Create(TOK_InvalidChar));

  sAssign0.AddTransition(TConsumeTransition.Create(['='], sStart, [toFinal],
    caName));
  sAssign0.AddTransition(TCheckTransition.Create(Start + STOP, sStart, [toFinal],
    caName));
  sAssign0.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

  sGreaterF.AddTransition(TConsumeTransition.Create(['='], sStart, [toFinal],
    caName));
  sGreaterF.AddTransition(TCheckTransition.Create(Start + STOP, sStart, [toFinal],
    caName));
  sGreaterF.SetElse(TErrorTransition.Create(TOK_EqualityExpected));

  sSmallerF.AddTransition(TConsumeTransition.Create(['='], sStart, [toFinal],
    caName));
  sSmallerF.AddTransition(TConsumeTransition.Create(['>'], sStart, [toFinal],
    caName));
  sSmallerF.AddTransition(TCheckTransition.Create(Start + STOP, sStart, [toFinal],
    caName));
  sSmallerF.SetElse(TErrorTransition.Create(TOK_GreaterEqualityExpected));

  sDotDot.AddTransition(TConsumeTransition.Create(['.'], sStart, [toFinal],
    caDotDot));
  sDotDot.AddTransition(TCheckTransition.Create(NAM, sStart, [toFinal], caName));
  sDotDot.SetElse(TErrorTransition.Create(TOK_DotExpected));

finalization
  sStart.Free;
  sSpace.Free;
  sComment.Free;
  sCommentF.Free;
  sSwitch.Free;
  sSwitchNameF.Free;
  sSlashComment0.Free;
  sSlashComment.Free;
  sChar0.Free;
  sCharF.Free;
  sCharHex.Free;
  sCharHexF.Free;
  sNameF.Free;
  sIntF.Free;
  sIntPoint.Free;
  sIntPointF.Free;
  sIntExp.Free;
  sIntExp0.Free;
  sIntExpF.Free;
  sHex.Free;
  sHexF.Free;
  sString0.Free;
  sStringF.Free;
  sAssign0.Free;
  sGreaterF.Free;
  sSmallerF.Free;
  sDotDot.Free;
end.
