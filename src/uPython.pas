unit uPython;
{
  UnderScript Python Wrapper
  Copyright (c) 2013-2014 Felipe Daragon
  License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, PythonEngine, UndHelper_AS,
  UndImporter, VarPyth, UndConst;

const
  cObjectName = 'RPython';

type
  TUndPython = class
    State: Plua_State;
    PyEngine: TPythonEngine;
    inout: TPythonInputOutput;
    UndModule: TPythonModule;
    constructor Create(L: Plua_State);
    destructor Destroy; override;
    procedure PythonModule1Initialization(Sender: TObject);
    procedure PythonEngine1AfterLoad(Sender: TObject);
    procedure CreateUndModule;
    procedure PythonInputOutput1SendData(Sender: TObject;
      const Data: AnsiString);
  end;

function Python_Run(L: Plua_State): integer; cdecl;

implementation

function Python_Run(L: Plua_State): integer; cdecl;
// Main function for execution
var
  r: TUndScriptResult;
  obj: TUndPython;
  //rv: Variant;
  script: string;
  importer: TUndImporter;
begin
  if plua_validateargs(L, result, [LUA_TSTRING]).OK = false then
    Exit;
  r.success := true;
  obj := TUndPython.Create(L);
  importer := TUndImporter.Create(L);
  importer.EnableDebug:=false;
  importer.FuncReadFormat := '%k = ' + rudLibName + '.GetL("%k")' + crlf;
  importer.FuncWriteFormat := crlf + rudLibName + '.SetL("%k",%k)';
  script := lua_tostring(L, 1);
  script := importer.GetScript(L, script);
  script := 'import ' + rudLibName + crlf + script;
  try
    obj.PyEngine.ExecString(script);
  except
    on E: Exception do begin
      r.success := false;
      r.errormessage := E.Message;
      uConsoleErrorLn(L, -1, 'Python: ' + E.Message);
    end;
  end;
  { obj.PSScript.Script.Text:=script;
    if obj.PSScript.Compile then begin
    obj.success:= obj.PSScript.Execute;
    end else obj.Success:=false; }

  obj.free;
  importer.free;
  Und_PushScriptResult(L, r);
  result := 1;
end;


procedure TUndPython.PythonModule1Initialization(Sender: TObject);
begin
  {
    with Sender as TPythonModule do
    begin
    AddMethod( 'WriteLn', @Und_WriteLn, 'WriteLn' );
    end; }
  // UndModule.AddMethod( 'WriteLn', @Und_WriteLn, 'WriteLn' );
end;

procedure TUndPython.PythonEngine1AfterLoad(Sender: TObject);
begin
  // UndModule.initialize;
end;

procedure TUndPython.CreateUndModule;
  function Und_Debug(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    v: Variant;
  begin
    with GetPythonEngine do
    begin
      if args.ob_refcnt = 1 then
        v := v + PyObjectAsVariant(PyTuple_GetItem(args, 0));
      Undhelper.debug(v);
      result := ReturnNone;
    end;
  end;
  function Und_WriteLn(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    v: Variant;
  begin
    with GetPythonEngine do
    begin
      if args.ob_refcnt = 1 then
        v := v + PyObjectAsVariant(PyTuple_GetItem(args, 0));
      Undhelper.writeln(v);
      result := ReturnNone;
    end;
  end;
  function Und_Write(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    v: Variant;
  begin
    with GetPythonEngine do
    begin
      if args.ob_refcnt = 1 then
        v := PyObjectAsVariant(PyTuple_GetItem(args, 0));
      Undhelper.write(v);
      result := ReturnNone;
    end;
  end;
  function Und_GetL(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    s: string;
    v: Variant;
  begin
    with GetPythonEngine do
    begin
      result := ReturnNone;
      if args.ob_refcnt = 1 then
      begin
        s := PyObjectAsString(PyTuple_GetItem(args, 0));
        try
          v := Undhelper.GetL(s);
        except
        end;
        result := VariantAsPyObject(v);
      end;
    end;
  end;
  function Und_GetG(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    s: string;
    v: Variant;
  begin
    with GetPythonEngine do
    begin
      result := ReturnNone;
      if args.ob_refcnt = 1 then
      begin
        s := PyObjectAsString(PyTuple_GetItem(args, 0));
        try
          v := Undhelper.GetG(s);
        except
        end;
        result := VariantAsPyObject(v);
      end;
    end;
  end;
  function Und_SetL(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    s: string;
    newvalue: Variant;
  begin
    with GetPythonEngine do
    begin
      result := ReturnNone;
      if args.ob_refcnt = 1 then
      begin
        s := PyObjectAsString(PyTuple_GetItem(args, 0));
        newvalue := PyObjectAsVariant(PyTuple_GetItem(args, 1));
        try
          Undhelper.SetL(s, newvalue);
        except
        end;
      end;
    end;
  end;
  function Und_SetG(self, args: PPyObject): PPyObject; cdecl;
  var
    i: integer;
    s: string;
    newvalue: Variant;
  begin
    with GetPythonEngine do
    begin
      result := ReturnNone;
      if args.ob_refcnt = 1 then
      begin
        s := PyObjectAsString(PyTuple_GetItem(args, 0));
        newvalue := PyObjectAsVariant(PyTuple_GetItem(args, 1));
        try
          Undhelper.SetG(s, newvalue);
        except
        end;
      end;
    end;
  end;

{ with GetPythonEngine do begin
  for i := 0 to PyTuple_Size(args)-1 do begin
  s:=s+PyObjectAsString(PyTuple_GetItem(args, i));
  end;
  Result := ReturnNone;
  end; }
// writeln(s);
{ with GetPythonEngine do begin
  PyArg_Parse( args, 's:WriteLn', @s );
  //ShowMessage( 'args of foo: '+PyObjectAsString(args) );
  system.Writeln(PyObjectAsString(args));
  Result := ReturnNone;
  end; }
begin
  UndModule := TPythonModule.Create(nil);
  UndModule.ModuleName := rudLibName;
  UndModule.AddMethod('Debug', @Und_WriteLn, 'Debug');
  UndModule.AddMethod('WriteLn', @Und_WriteLn, 'WriteLn');
  UndModule.AddMethod('Write', @Und_Write, 'Write');
  UndModule.AddMethod('GetL', @Und_GetL, 'GetL');
  UndModule.AddMethod('SetL', @Und_SetL, 'SetL');
  UndModule.AddMethod('GetG', @Und_GetG, 'GetG');
  UndModule.AddMethod('SetG', @Und_SetG, 'SetG');
  UndModule.Engine := PyEngine;
end;

procedure TUndPython.PythonInputOutput1SendData(Sender: TObject;
  const Data: AnsiString);
begin
  Undhelper.writeln(Data);
end;

constructor TUndPython.Create(L: Plua_State);
begin
  PyEngine := TPythonEngine.Create(nil);
  Undhelper.LuaState := L;
  inout := TPythonInputOutput.Create(nil);
  inout.onsenddata := PythonInputOutput1SendData;
  PyEngine.OnAfterLoad := PythonEngine1AfterLoad;
  PyEngine.IO := inout;
  PyEngine.AutoFinalize := false; // false;
  // obj.UseWindowsConsole:=false;
  // obj.RedirectIO:=false;
  // obj.UseLastKnownVersion:=false;
  CreateUndModule;
  PyEngine.LoadDll;
  UndModule.Initialize;
end;

destructor TUndPython.Destroy;
begin
  UndModule.free;
  inout.free;
  try
    PyEngine.free;
  except
    // avoid crash if the app is terminated in the middle of the execution
  end;
  inherited Destroy;
end;

end.
