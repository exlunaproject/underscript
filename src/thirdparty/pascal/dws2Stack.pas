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
{    Contributor(s): .                                                 }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2Stack;

interface

type
  TData = array of Variant;

  TStack = class
  private
    FBasePointer: Integer;
    FBpStore: array of Integer;
    FChunkSize: Integer;
    FMaxLevel: Integer;
    FMaxSize: Integer;
    FSize: Integer;
    FStackPointer: Integer;
    function GetFrameSize: Integer;
  public
    Data: TData;
    constructor Create(ChunkSize: Integer; MaxByteSize: Integer);
    function GetSavedBp(Level: Integer): Integer;
    function NextLevel(Level: Integer): Integer;
    procedure Push(Delta: Integer);
    procedure Pop(Delta: Integer);
    procedure WriteData(SourceAddr, DestAddr, Size: Integer; SourceData: TData);
    procedure WriteValue(DestAddr: Integer; const Value: Variant);
    function ReadValue(SourceAddr: Integer): Variant;
    procedure ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
    procedure CopyData(SourceAddr, DestAddr, Size: Integer);
    function SaveBp(Level, Bp: Integer): Integer;
    procedure SwitchFrame(var oldBasePointer: Integer);
    procedure RestoreFrame(oldBasePointer: Integer);
    procedure Reset;
    property BasePointer: Integer read FBasePointer;
    property FrameSize: Integer read GetFrameSize;
    property MaxSize: Integer read FMaxSize write FMaxSize;
    property StackPointer: Integer read FStackPointer;
  end;

  procedure CopyData(SourceData: TData; SourceAddr: Integer; DestData: TData;
    DestAddr: Integer; Size: Integer);

implementation

uses
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  Classes, SysUtils, dws2Errors, dws2Strings;

procedure CopyData(SourceData: TData; SourceAddr: Integer; DestData: TData;
  DestAddr: Integer; Size: Integer);
begin
  while Size > 0 do
  begin
    VarCopy(DestData[DestAddr], SourceData[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;


{ TStack }

procedure TStack.CopyData(SourceAddr, DestAddr, Size: Integer);
begin
  while Size > 0 do
  begin
    VarCopy(Data[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

constructor TStack.Create;
begin
  FChunkSize := ChunkSize;
  FMaxSize := MaxByteSize div SizeOf(Variant);
  FMaxLevel := 1;
end;

function TStack.GetFrameSize: Integer;
begin
  Result := FStackPointer - FBasePointer;
end;

function TStack.GetSavedBp(Level: Integer): Integer;
begin
  Assert(Level >= 0);
  Assert(Level < FMaxLevel);
  Result := FBpStore[Level];
end;

function TStack.NextLevel(Level: Integer): Integer;
begin
  Result := Level + 1;
  if Result > FMaxLevel then
    FMaxLevel := Result;
end;

procedure TStack.Pop(Delta: Integer);
var
  x: Integer;
begin
  // Release ScriptObjs
  for x := FStackPointer - 1 downto FStackPointer - Delta do
    if VarType(Data[x]) = varUnknown then
      VarClear(Data[x]);

  // Release other data
  for x := FStackPointer - 1 downto FStackPointer - Delta do
    if VarType(Data[x]) <> varEmpty then
      VarClear(Data[x]);

  // Free memory
  Dec(FStackPointer, Delta);
end;

procedure TStack.Push(Delta: Integer);
var
  sp : Integer;
begin
  sp := FStackPointer + Delta;

  // Increase stack size if necessary
  if sp > FSize then
  begin
    if sp > FMaxSize then
      raise Exception.CreateFmt(RTE_MaximalDatasizeExceeded, [FMaxSize]);
    FSize := ((sp) div FChunkSize + 1) * FChunkSize;
    if FSize > FMaxSize then
      FSize := FMaxSize;
    SetLength(Data, FSize);
  end;

  FStackPointer := sp;
end;

procedure TStack.ReadData(SourceAddr, DestAddr, Size: Integer; DestData: TData);
begin
  while Size > 0 do
  begin
    VarCopy(DestData[DestAddr], Data[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

function TStack.ReadValue(SourceAddr: Integer): Variant;
begin
  Result := Data[SourceAddr];
end;

procedure TStack.Reset;
begin
  Data := nil;
  FSize := 0;
  FStackPointer := 0;
  FBasePointer := 0;
  SetLength(FBpStore, FMaxLevel + 1);
end;

procedure TStack.RestoreFrame(oldBasePointer: Integer);
begin
  FStackPointer := FBasePointer;
  FBasePointer := oldBasePointer;
end;

function TStack.SaveBp(Level, Bp: Integer): Integer;
begin
  Assert(Level >= 0);
  Assert(Level <= FMaxLevel);
  Result := FBpStore[Level];
  FBpStore[Level] := Bp;
end;

procedure TStack.SwitchFrame(var oldBasePointer: Integer);
begin
  oldBasePointer := FBasePointer;
  FBasePointer := FStackPointer;
end;

procedure TStack.WriteData(SourceAddr, DestAddr, Size: Integer; SourceData: TData);
begin
  while Size > 0 do
  begin
    VarCopy(Data[DestAddr], SourceData[SourceAddr]);
    Inc(SourceAddr);
    Inc(DestAddr);
    Dec(Size);
  end;
end;

procedure TStack.WriteValue(DestAddr: Integer; const Value: Variant);
begin
  VarCopy(Data[DestAddr], Value);
end;

end.
