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
{       [x] D5 (WK)   [x] D6 (WK)    [?] K1 (WK)                       }
{**********************************************************************}

// Note: the documentation will be moved from code into a help file in rel. 1.0!
{: GlobalVariables for DWSII<p>

   This unit implements global variables functions, that allow scripts to read
   and write to variables across a script's context.<br>
   Details:<ul>
   <li>Variables can be declared and read from any script, or from Delphi code
   <li>Read/Write access is thread-safe
   <li>Variables names are <b>case sensitive</b>
   <li>Fast, (reads/writes per second on a 800MHz Duron and a handful of
      variables: 100k/sec, 1000k with optimization using D5)
   </ul><p>

   The global vars can be saved/restored as a whole from Delphi code (delphi
   code only as of now, mainly for security reasons) to a file, string or stream.<br>
   Be aware DWSII will require special care to run in a multi-threaded
   environment.
}

{$I dws2.inc}
unit dws2GlobalVarsFunctions;

interface

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, dws2Functions, dws2Exprs, dws2Symbols, SysUtils;

type

  TReadGlobalVarFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TReadGlobalVarFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TReadGlobalVarDefFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TReadGlobalVarDefFuncExpr = class(TBinaryOpExpr)
    function Eval: Variant; override;
  end;

  TWriteGlobalVarFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TWriteGlobalVarFuncExpr = class(TBinaryOpExpr)
    function Eval: Variant; override;
  end;

  TCleanupGlobalVarsFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  Tdws2GlobalVarsFunctions = class(TComponent)
  end;

  {: Directly write a global var.<p> }
procedure WriteGlobalVar(const aName: string; const aValue: Variant);
{: Directly read a global var.<p> }
function ReadGlobalVar(const aName: string): Variant;
{: Directly read a global var, using a default value if variable does not exists.<p> }
function ReadGlobalVarDef(const aName: string; const aDefault: Variant):
  Variant;
{: Resets all global vars.<p> }
procedure CleanupGlobalVars;

{: Save current global vars and their values to a file. }
function SaveGlobalVarsToString: string;
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromString(const srcString: string);
{: Save current global vars and their values to a file. }
function SaveGlobalVarsToFile(const destFileName: string): Boolean;
{: Load global vars and their values to a file. }
function LoadGlobalVarsFromFile(const srcFileName: string): Boolean;
{: Save current global vars and their values to a file. }
procedure SaveGlobalVarsToStream(destStream: TStream);
{: Load global vars and their values to a file. }
procedure LoadGlobalVarsFromStream(srcStream: TStream);

implementation

var
  vGlobalVars: TThreadList;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';
  cVariant = 'Variant';

  cGlobalVarsFiles = 'GBF 1.0';

type

  TGlobalVar = class(TObject)
    Name: string;
    Value: Variant;

    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
  end;

function GlobalVarsIndex(list: TList; const aName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to list.Count - 1 do
    if TGlobalVar(list[i]).Name = aName then
    begin
      Result := i;
      Break;
    end;
end;

procedure WriteGlobalVar(const aName: string; const aValue: Variant);
var
  gv: TGlobalVar;
  list: TList;
  i: Integer;
begin
  list := vGlobalVars.LockList;
  try
    i := GlobalVarsIndex(list, aName);
    if i < 0 then
    begin
      gv := TGlobalVar.Create;
      gv.Name := aName;
      list.Add(gv);
    end
    else
      gv := TGlobalVar(list[i]);
    gv.Value := aValue;
  finally
    vGlobalVars.UnlockList;
  end;
end;

function ReadGlobalVar(const aName: string): Variant;
begin
  // Result (empty) is our default value when calling...
  Result := ReadGlobalVarDef(aName, Result);
end;

function ReadGlobalVarDef(const aName: string; const aDefault: Variant):
  Variant;
var
  list: TList;
  i: Integer;
begin
  list := vGlobalVars.LockList;
  try
    i := GlobalVarsIndex(list, aName);
    if i < 0 then
      Result := aDefault
    else
      Result := TGlobalVar(list[i]).Value;
  finally
    vGlobalVars.UnlockList;
  end;
end;

procedure CleanupGlobalVars;
var
  i: Integer;
begin
  with vGlobalVars.LockList do
  try
    for i := 0 to Count - 1 do
      TGlobalVar(Items[i]).Free;
    Clear;
  finally
    vGlobalVars.UnlockList;
  end;
end;

function SaveGlobalVarsToString: string;
var
  fs: TStringStream;
begin
  fs := TStringStream.Create('');
  try
    SaveGlobalVarsToStream(fs);
    Result := fs.DataString;
  finally
    fs.Free;
  end;
end;

procedure LoadGlobalVarsFromString(const srcString: string);
var
  fs: TStringStream;
begin
  fs := TStringStream.Create(srcString);
  try
    LoadGlobalVarsFromStream(fs);
  finally
    fs.Free;
  end;
end;

function SaveGlobalVarsToFile(const destFileName: string): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  {$IFDEF LINUX}
  fs := nil;
  {$ENDIF}
  try
    if FileExists(destFileName) then
      fs := TFileStream.Create(destFileName, fmCreate)
    else
      fs := TFileStream.Create(destFileName, fmOpenWrite + fmShareExclusive);
  except
    Exit;
  end;

  try
    SaveGlobalVarsToStream(fs);
    Result := True;
  finally
    fs.Free;
  end;
end;

function LoadGlobalVarsFromFile(const srcFileName: string): Boolean;
var
  fs: TFileStream;
begin
  Result := False;
  {$IFDEF LINUX}
  fs := nil;
  {$ENDIF}
  try
    fs := TFileStream.Create(srcFileName, fmOpenRead + fmShareDenyWrite);
  except
    Exit;
  end;

  try
    LoadGlobalVarsFromStream(fs);
    Result := True;
  finally
    fs.Free;
  end;
end;

procedure SaveGlobalVarsToStream(destStream: TStream);
var
  list: TList;
  i: Integer;
  writer: TWriter;
begin
  list := vGlobalVars.LockList;
  try
    writer := TWriter.Create(destStream, 16384);
    try
      writer.Write(cGlobalVarsFiles[1], Length(cGlobalVarsFiles));
      for i := 0 to list.Count - 1 do
      begin
        writer.WriteListBegin;
        TGlobalVar(list[i]).WriteToFiler(writer);
      end;
      writer.WriteListEnd;
    finally
      writer.Free;
    end;
  finally
    vGlobalVars.UnlockList;
  end;
end;

procedure LoadGlobalVarsFromStream(srcStream: TStream);
var
  list: TList;
  reader: TReader;
  fileTag: string;
  gv: TGlobalVar;
  listFound: Boolean;
begin
  list := vGlobalVars.LockList;
  try
    reader := TReader.Create(srcStream, 16384);
    try
      SetLength(fileTag, Length(cGlobalVarsFiles));
      reader.Read(fileTag[1], Length(cGlobalVarsFiles));
      if fileTag <> cGlobalVarsFiles then
        raise Exception.Create('Invalid file tag');
      CleanupGlobalVars;

      try
        reader.ReadListBegin;
        listFound := True;
      except
        listFound := False;
      end;

      if listFound then
      begin
        while not reader.EndOfList do
        begin
          gv := TGlobalVar.Create;
          gv.ReadFromFiler(reader);
          list.Add(gv);
        end;
        reader.ReadListEnd;
      end;

    finally
      reader.Free;
    end;
  finally
    vGlobalVars.UnlockList;
  end;
end;

procedure WriteVariant(writer: TWriter; const value: Variant);

  procedure WriteValue(const value: TValueType);
  begin
    writer.Write(value, SizeOf(value));
  end;

begin
  // Adapted from VCL's TWriter.WriteProperty
  with writer do
    case VarType(Value) of
      varEmpty: WriteValue(vaNil);
      varNull: WriteValue(vaNull);
      varOleStr: WriteWideString(value);
      varString: WriteString(value);
      varByte, varSmallInt, varInteger:
        WriteInteger(value);
      varSingle: WriteSingle(value);
      varDouble: WriteFloat(value);
      varCurrency: WriteCurrency(value);
      varDate: WriteDate(value);
      varBoolean: WriteBoolean(value);
    else
      try
        WriteString(Value);
      except
        raise EWriteError.Create('Streaming not supported');
      end;
    end;
end;

function ReadVariant(reader: TReader): Variant;

  function ReadValue: TValueType;
  begin
    reader.Read(Result, SizeOf(Result));
  end;

const

  cValTtoVarT: array[TValueType] of Integer = (varNull, varError, varByte,
    varSmallInt, varInteger, varDouble, varString, varError, varBoolean,
    varBoolean, varError, varError, varString, varEmpty, varError, varSingle,
    varCurrency, varDate, varOleStr, varError
{$IFDEF VER140UP}
    , varError // UTF8
{$ENDIF}
    );

var
  valType: TValueType;
begin
  // Adapted from VCL's TReader.ReadPropValue
  valType := reader.NextValue;
  with reader do
    case valType of
      vaNil, vaNull:
        begin
          if ReadValue = vaNil then
            VarClear(Result)
          else
            Result := NULL;
        end;
      vaInt8: TVarData(Result).VByte := Byte(ReadInteger);
      vaInt16: TVarData(Result).VSmallint := Smallint(ReadInteger);
      vaInt32: TVarData(Result).VInteger := ReadInteger;
      vaExtended: TVarData(Result).VDouble := ReadFloat;
      vaSingle: TVarData(Result).VSingle := ReadSingle;
      vaCurrency: TVarData(Result).VCurrency := ReadCurrency;
      vaDate: TVarData(Result).VDate := ReadDate;
      vaString, vaLString:
        Result := ReadString;
      vaWString: Result := ReadWideString;
      vaFalse, vaTrue: TVarData(Result).VBoolean := (ReadValue = vaTrue);
    else
      raise EReadError.Create('Invalid variant stream');
    end;
  TVarData(Result).VType := cValTtoVarT[ValType];
end;

{ TReadGlobalVarFunc }

procedure TReadGlobalVarFunc.Execute;
begin
  Info.Result := ReadGlobalVar(Info['n']);
end;

function TReadGlobalVarFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TReadGlobalVarFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TReadGlobalVarFuncExpr.Eval: Variant;
begin
  Result := ReadGlobalVar(Expr.Eval);
end;

{ TGlobalVar }

procedure TGlobalVar.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteString(Name);
    dws2GlobalVarsFunctions.WriteVariant(writer, Value);
  end;
end;

procedure TGlobalVar.ReadFromFiler(reader: TReader);
begin
  with reader do
  begin
    Name := ReadString;
    Value := dws2GlobalVarsFunctions.ReadVariant(reader);
  end;
end;

{ TReadGlobalVarDefFunc }

procedure TReadGlobalVarDefFunc.Execute;
begin
  Info.Result := ReadGlobalVarDef(Info['n'], Info['d']);
end;

function TReadGlobalVarDefFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TReadGlobalVarDefFuncExpr.Create(Prog, Pos, Args[0], Args[1]);
    Args.Clear;
    Free;
  end;
end;

function TReadGlobalVarDefFuncExpr.Eval: Variant;
begin
  Result := ReadGlobalVarDef(Left.Eval, Right.Eval);
end;

{ TWriteGlobalVarFunc }

procedure TWriteGlobalVarFunc.Execute;
begin
  WriteGlobalVar(Info['n'], Info['v']);
end;

function TWriteGlobalVarFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TWriteGlobalVarFuncExpr.Create(Prog, Pos, Args[0], Args[1]);
    Args.Clear;
    Free;
  end;
end;

function TWriteGlobalVarFuncExpr.Eval: Variant;
begin
  WriteGlobalVar(Left.Eval, Right.Eval);
end;

{ TCleanupGlobalVarsFunc }

procedure TCleanupGlobalVarsFunc.Execute;
begin
  CleanupGlobalVars;
end;

initialization
  vGlobalVars := TThreadList.Create;

  RegisterInternalFunction(TReadGlobalVarFunc, 'ReadGlobalVar', ['n', cString],
    cVariant);
  RegisterInternalFunction(TReadGlobalVarDefFunc, 'ReadGlobalVarDef', ['n',
    cString, 'd', cVariant], cVariant);
  RegisterInternalFunction(TWriteGlobalVarFunc, 'WriteGlobalVar', ['n', cString,
    'v', cVariant], '');
  RegisterInternalFunction(TCleanupGlobalVarsFunc, 'CleanupGlobalVars', [], '');

finalization
  CleanupGlobalVars;
  FreeAndNil(vGlobalVars);
end.
