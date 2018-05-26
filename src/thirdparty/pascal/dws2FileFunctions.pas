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

// dws2FileFunctions - Generated : 27.02.01 12:43:46

{$I dws2.inc}
unit dws2FileFunctions;

interface

uses Classes, dws2Functions, dws2Exprs, dws2Symbols;

type

  TSaveStringToFileFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TLoadStringFromFileFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TAppendStringToFileFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TFileExistsFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TDeleteFileFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRenameFileFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TChDirFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TChDirFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TCreateDirFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TRemoveDirFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TGetCurrentDirFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TGetCurrentDirFuncExpr = class(TExpr)
    function Eval: Variant; override;
  end;

  TSetCurrentDirFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TFileSearchFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  TExtractFileDriveFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExtractFileDriveFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TExtractFileDirFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExtractFileDirFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TExtractFileNameFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExtractFileNameFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TExtractFilePathFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExtractFilePathFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TExtractFileExtFunc = class(TInternalFunction)
    procedure Execute; override;
    function Optimize(FuncExpr: TExprBase): TExprBase; override;
  end;
  TExtractFileExtFuncExpr = class(TUnaryOpExpr)
    function Eval: Variant; override;
  end;

  TChangeFileExtFunc = class(TInternalFunction)
    procedure Execute; override;
  end;

  Tdws2FileFunctions = class(TComponent)
  end;

implementation

uses SysUtils;

const // type constants to make sure strings get reused by the compiler
  cFloat = 'Float';
  cInteger = 'Integer';
  cString = 'String';
  cBoolean = 'Boolean';

  { TSaveStringToFileFunc }

procedure TSaveStringToFileFunc.Execute;
var
  fs: TFileStream;
  FileName,
    buf: string;
begin
  FileName := Info['fileName'];
  fs := TFileStream.Create(FileName, fmCreate);
  buf := Info['data'];
  if Length(buf) > 0 then
    fs.Write(buf[1], Length(buf));
  fs.Free;
end;

{ TLoadStringFromFileFunc }

procedure TLoadStringFromFileFunc.Execute;
var
  fs: TFileStream;
  FileName,
    buf: string;
begin
  FileName := Info['fileName'];
  fs := TFileStream.Create(FileName, fmOpenRead + fmShareDenyNone);
  SetLength(buf, fs.Size);
  if fs.Size > 0 then
    fs.Read(buf[1], Length(buf));
  Info.Result := buf;
  fs.Free;
end;

{ TAppendStringToFileFunc }

procedure TAppendStringToFileFunc.Execute;
var
  fs: TFileStream;
  FileName,
    buf: string;
begin
  buf := Info['data'];
  if Length(buf) > 0 then
  begin
    FileName := Info['fileName'];
    fs := TFileStream.Create(FileName, fmOpenReadWrite);
    fs.Seek(0, soFromEnd);
    fs.Write(buf[1], Length(buf));
    fs.Free;
  end;
end;

{ TFileExistsFunc }

procedure TFileExistsFunc.Execute;
begin
  Info.Result := FileExists(Info['fileName']);
end;

{ TDeleteFileFunc }

procedure TDeleteFileFunc.Execute;
begin
  Info.Result := DeleteFile(Info['fileName']);
end;

{ TRenameFileFunc }

procedure TRenameFileFunc.Execute;
begin
  Info.Result := RenameFile(Info['oldName'], Info['newName']);
end;

{ TChDirFunc }

procedure TChDirFunc.Execute;
begin
  ChDir(Info['s']);
end;

function TChDirFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TChDirFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TChDirFuncExpr.Eval: Variant;
begin
  ChDir(Expr.Eval);
end;

{ TCreateDirFunc }

procedure TCreateDirFunc.Execute;
begin
  Info.Result := CreateDir(Info['dir']);
end;

{ TRemoveDirFunc }

procedure TRemoveDirFunc.Execute;
begin
  Info.Result := RemoveDir(Info['dir']);
end;

{ TGetCurrentDirFunc }

procedure TGetCurrentDirFunc.Execute;
begin
  Info.Result := GetCurrentDir;
end;

function TGetCurrentDirFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    Result := TGetCurrentDirFuncExpr.Create(Prog, Pos);
    Args.Clear;
    Free;
  end;
end;

function TGetCurrentDirFuncExpr.Eval: Variant;
begin
  Result := GetCurrentDir;
end;

{ TSetCurrentDirFunc }

procedure TSetCurrentDirFunc.Execute;
begin
  Info.Result := SetCurrentDir(Info['dir']);
end;

{ TFileSearchFunc }

procedure TFileSearchFunc.Execute;
begin
  Info.Result := FileSearch(Info['name'], Info['dirList']);
end;

{ TExtractFileDriveFunc }

procedure TExtractFileDriveFunc.Execute;
begin
  Info.Result := ExtractFileDrive(Info['fName']);
end;

function TExtractFileDriveFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        ExtractFileDrive(Args[0].Eval))
    else
      Result := TExtractFileDriveFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExtractFileDriveFuncExpr.Eval: Variant;
begin
  Result := ExtractFileDrive(Expr.Eval);
end;

{ TExtractFileDirFunc }

procedure TExtractFileDirFunc.Execute;
begin
  Info.Result := ExtractFileDir(Info['fName']);
end;

function TExtractFileDirFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        ExtractFileDir(Args[0].Eval))
    else
      Result := TExtractFileDirFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExtractFileDirFuncExpr.Eval: Variant;
begin
  Result := ExtractFileDir(Expr.Eval);
end;

{ TExtractFileNameFunc }

procedure TExtractFileNameFunc.Execute;
begin
  Info.Result := ExtractFileName(Info['fName']);
end;

function TExtractFileNameFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        ExtractFileName(Args[0].Eval))
    else
      Result := TExtractFileNameFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExtractFileNameFuncExpr.Eval: Variant;
begin
  Result := ExtractFileName(Expr.Eval);
end;

{ TExtractFilePathFunc }

procedure TExtractFilePathFunc.Execute;
begin
  Info.Result := ExtractFilePath(Info['fName']);
end;

function TExtractFilePathFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        ExtractFilePath(Args[0].Eval))
    else
      Result := TExtractFilePathFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExtractFilePathFuncExpr.Eval: Variant;
begin
  Result := ExtractFilePath(Expr.Eval);
end;

{ TExtractFileExtFunc }

procedure TExtractFileExtFunc.Execute;
begin
  Info.Result := ExtractFileExt(Info['fName']);
end;

function TExtractFileExtFunc.Optimize(FuncExpr: TExprBase): TExprBase;
begin
  with FuncExpr as TFuncExpr do
  begin
    if Args[0] is TConstExpr then
      Result := TConstExpr.Create(Prog, Pos, Prog.TypString,
        ExtractFileExt(Args[0].Eval))
    else
      Result := TExtractFileExtFuncExpr.Create(Prog, Pos, Args[0]);
    Args.Clear;
    Free;
  end;
end;

function TExtractFileExtFuncExpr.Eval: Variant;
begin
  Result := ExtractFileExt(Expr.Eval);
end;

{ TChangeFileExtFunc }

procedure TChangeFileExtFunc.Execute;
begin
  Info.Result := ChangeFileExt(Info['fName'], Info['ext']);
end;

initialization

  RegisterInternalFunction(TSaveStringToFileFunc, 'SaveStringToFile',
    ['fileName', cString, 'data', cString], '');
  RegisterInternalFunction(TLoadStringFromFileFunc, 'LoadStringFromFile',
    ['fileName', cString], cString);
  RegisterInternalFunction(TAppendStringToFileFunc, 'AppendStringToFile',
    ['fileName', cString, 'data', cString], '');
  RegisterInternalFunction(TFileExistsFunc, 'FileExists', ['fileName', cString],
    cBoolean);
  RegisterInternalFunction(TDeleteFileFunc, 'DeleteFile', ['fileName', cString],
    cBoolean);
  RegisterInternalFunction(TRenameFileFunc, 'RenameFile', ['oldName', cString,
    'newName', cString], cBoolean);
  RegisterInternalFunction(TChDirFunc, 'ChDir', ['s', cString], '');
  RegisterInternalFunction(TCreateDirFunc, 'CreateDir', ['dir', cString],
    cBoolean);
  RegisterInternalFunction(TRemoveDirFunc, 'RemoveDir', ['dir', cString],
    cBoolean);
  RegisterInternalFunction(TGetCurrentDirFunc, 'GetCurrentDir', [], cString);
  RegisterInternalFunction(TSetCurrentDirFunc, 'SetCurrentDir', ['dir',
    cString], cBoolean);
  RegisterInternalFunction(TFileSearchFunc, 'FileSearch', ['name', cString,
    'dirList', cString], cString);
  RegisterInternalFunction(TExtractFileDriveFunc, 'ExtractFileDrive', ['fName',
    cString], cString);
  RegisterInternalFunction(TExtractFileDirFunc, 'ExtractFileDir', ['fName',
    cString], cString);
  RegisterInternalFunction(TExtractFileNameFunc, 'ExtractFileName', ['fName',
    cString], cString);
  RegisterInternalFunction(TExtractFilePathFunc, 'ExtractFilePath', ['fName',
    cString], cString);
  RegisterInternalFunction(TExtractFileExtFunc, 'ExtractFileExt', ['fName',
    cString], cString);
  RegisterInternalFunction(TChangeFileExtFunc, 'ChangeFileExt', ['fName',
    cString, 'ext', cString], cString);

end.
