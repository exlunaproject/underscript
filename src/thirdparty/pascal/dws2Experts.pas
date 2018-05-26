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
{    Contributor(s): Eric Grange.                                      }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

// Design-Time only, do NOT include this unit in runtime packages.
//
unit dws2Experts;

interface

procedure Register;

implementation

uses Classes, Forms, SysUtils,
{$IFDEF NEWDESIGN}
  DesignIntf,
  DesignEditors,
{$ELSE}
  DsgnIntf,
{$ENDIF}
  dws2Strings, dws2Comp, dws2Symbols;

type
  Tdws2DataTypeProperty = class(TStringProperty)
  protected
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  Tdws2AncestorProperty = class(TStringProperty)
  protected
    { Protected Declarations }
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { Tdws2DataTypeProperty }

function Tdws2DataTypeProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure Tdws2DataTypeProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  dws2Unit: Tdws2Unit;
  sl: TStringList;
begin
  if not (GetComponent(0) is Tdws2Symbol) then
    exit;

  dws2Unit := Tdws2Symbol(GetComponent(0)).GetUnit;

  sl := TStringList.Create;
  try
    dws2Unit.GetDataTypes(sl);
    sl.Sorted := True;
    sl.Duplicates := dupIgnore;

    // feed the dropdown list
    for i := 0 to sl.Count - 1 do
      if Length(sl[i]) > 0 then
        Proc(sl[i]);
  finally
    sl.Free;
  end;
end;

{ Tdws2AncestorProperty }

function Tdws2AncestorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList];
end;

procedure Tdws2AncestorProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  sl: TStringList;
  dws2Unit: Tdws2Unit;
begin
  if GetComponent(0) is Tdws2Symbol then
  begin
    dws2Unit := Tdws2Symbol(GetComponent(0)).GetUnit;

    sl := TStringList.Create;
    try
      dws2Unit.GetClassTypes(sl);
      sl.Sorted := True;
      sl.Duplicates := dupIgnore;

      // feed the dropdown list
      for i := 0 to sl.Count - 1 do
        if Length(sl[i]) > 0 then
          Proc(sl[i]);
    finally
      sl.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Function, 'ResultType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Variable, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Property, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Property, 'IndexType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Global, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Parameter, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Field, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Member, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Array, 'DataType',
    Tdws2DataTypeProperty);
  RegisterPropertyEditor(TypeInfo(TDataType), Tdws2Class, 'Ancestor',
    Tdws2AncestorProperty);
end;

end.

