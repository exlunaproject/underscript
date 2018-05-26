unit uPython;
{
 UnderScript Python Wrapper
 Copyright (c) 2013-2014 Felipe Daragon
 License: MIT (http://opensource.org/licenses/mit-license.php)
}

interface

uses
  Classes, SysUtils, lua, plua, LuaObject, PythonEngine, UndHelperUnit,
  UndImporter, VarPyth, UndConst;

const
 cObjectName='RPython';

type
   TUndPython = class
    State:Plua_State;
    PyEngine:TPythonEngine;
    inout:TPythonInputOutput;
    UndModule: TPythonModule;
    constructor Create(L : PLua_State);
    destructor Destroy; override;
    procedure PythonModule1Initialization(Sender: TObject);
    procedure PythonEngine1AfterLoad(Sender: TObject);
    procedure CreateUndModule;
    procedure PythonInputOutput1SendData(Sender: TObject;const Data: String);
   end;

type
  TUndPythonWrapper = class(TLuaObject)
  private
    obj:TUndPython;
    constructor Create(LuaState : PLua_State; AParent : TLuaObject = nil); overload;
    function  GetPropValue(propName : String): Variant; override;
    function  SetPropValue(PropName : String; const AValue: Variant) : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterUndPythonWrapper(L : Plua_State);
function Python_Run(L: plua_State):integer; cdecl;

implementation

function Python_Run(L: plua_State):integer; cdecl; // Main function for execution
var obj:TUndPython; r:variant; script:string; importer:TUndImporter;
begin
  obj := TUndPython.Create(L);
  importer:=TUndImporter.create(L);
  //importer.EnableDebug:=true;
  importer.FuncReadFormat:='%k = '+rudLibName+'.GetL("%k")'+crlf;
  importer.FuncWriteFormat:=crlf+rudLibName+'.SetL("%k",%k)';
  script:=lua_tostring(L,1);
  script:=importer.GetScript(script); // processa
  script:='import '+rudLibName+crlf+script;
   try
   obj.pyengine.ExecString(script);
   except
    on E : Exception do Und_LogError(L,-1,'Python: '+E.Message);
   end;
  {obj.PSScript.Script.Text:=script;
  if obj.PSScript.Compile then begin
     obj.success:= obj.PSScript.Execute;
  end else obj.Success:=false;}

  obj.free;
  importer.free;
  //plua_pushvariant(L, r);
  result:=1;
end;

function method_evalstring(l : PLua_State) : Integer; cdecl;
var ht : TUndPythonWrapper; r:string;
begin
  ht:=TUndPythonWrapper(LuaToTLuaObject(l, 1));
  r:= ht.obj.pyengine.EvalStringAsStr(pchar(lua_tostring(L,2)));
  lua_pushstring(L, pchar(r));
  result := 1;
end;

function method_run(l : PLua_State) : Integer; cdecl;
var ht : TUndPythonWrapper;
begin
  ht:=TUndPythonWrapper(LuaToTLuaObject(l, 1));
  //ht.obj.EvalString(lua_tostring(L,2));
  try
  ht.obj.pyengine.ExecString(pchar(lua_tostring(L,2)));
  except end;
  result := 1;
end;

function XCL_new(L : PLua_State; AParent : TLuaObject=nil):TLuaObject;
begin
  result := TUndPythonWrapper.Create(L, AParent);
end;

function new_XCL(L : PLua_State) : Integer; cdecl;
var
  p : TLuaObjectNewCallback;
begin
  p := @XCL_new;
  result := new_LuaObject(L,cObjectName, p);
end;

procedure methods_XCL(L : Plua_State; classTable : Integer);
begin
 RegisterMethod(L,'Eval', @method_evalstring, classTable);
 RegisterMethod(L,'Run', @method_run, classTable);
end;

procedure RegisterUndPythonWrapper(L: Plua_State);
begin
  RegisterTLuaObject(L,cObjectName, @new_XCL, @methods_XCL);
end;

procedure TUndPython.PythonModule1Initialization(Sender: TObject);
begin
  {
  with Sender as TPythonModule do
    begin
      AddMethod( 'WriteLn', @Und_WriteLn, 'WriteLn' );
    end; }
    //UndModule.AddMethod( 'WriteLn', @Und_WriteLn, 'WriteLn' );
end;

procedure TUndPython.PythonEngine1AfterLoad(Sender: TObject);
begin
  //UndModule.initialize;
end;

procedure TUndPython.CreateUndModule;
 function Und_WriteLn( self, args : PPyObject ) : PPyObject; cdecl;
 var i : Integer; v:variant;
 begin
  with GetPythonEngine do begin
      if args.ob_refcnt=1 then v:=v+PyObjectAsVariant(PyTuple_GetItem(args, 0));
      Undhelper.writeln(v);
    Result := ReturnNone;
  end;
 end;
 function Und_Write( self, args : PPyObject ) : PPyObject; cdecl;
 var i:integer; v:variant;
 begin
  with GetPythonEngine do begin
      if args.ob_refcnt=1 then v:=PyObjectAsVariant(PyTuple_GetItem(args, 0));
      Undhelper.write(v);
    Result := ReturnNone;
  end;
 end;
 function Und_GetL( self, args : PPyObject ) : PPyObject; cdecl;
 var i:integer; s:string; v:variant;
 begin
  with GetPythonEngine do begin
      Result := ReturnNone;
      if args.ob_refcnt=1 then begin
      s:=PyObjectAsString(PyTuple_GetItem(args, 0));
      try v:=Undhelper.GetL(s); except end;
      result:=VariantAsPyObject(v);
      end;
  end;
 end;
 function Und_GetG( self, args : PPyObject ) : PPyObject; cdecl;
 var i:integer; s:string; v:variant;
 begin
  with GetPythonEngine do begin
      Result := ReturnNone;
      if args.ob_refcnt=1 then begin
      s:=PyObjectAsString(PyTuple_GetItem(args, 0));
      try v:=Undhelper.GetG(s); except end;
      result:=VariantAsPyObject(v);
      end;
  end;
 end;
 function Und_SetL( self, args : PPyObject ) : PPyObject; cdecl;
 var i:integer; s:string; newvalue:variant;
 begin
  with GetPythonEngine do begin
      Result := ReturnNone;
      if args.ob_refcnt=1 then begin
      s:=PyObjectAsString(PyTuple_GetItem(args, 0));
      newvalue:=PyObjectAsVariant(PyTuple_GetItem(args, 1));
      try Undhelper.SetL(s,newvalue); except end;
      end;
  end;
 end;
 function Und_SetG( self, args : PPyObject ) : PPyObject; cdecl;
 var i:integer; s:string; newvalue:variant;
 begin
  with GetPythonEngine do begin
      Result := ReturnNone;
      if args.ob_refcnt=1 then begin
      s:=PyObjectAsString(PyTuple_GetItem(args, 0));
      newvalue:=PyObjectAsVariant(PyTuple_GetItem(args, 1));
      try Undhelper.SetG(s,newvalue); except end;
      end;
  end;
 end;
  {with GetPythonEngine do begin
    for i := 0 to PyTuple_Size(args)-1 do begin
      s:=s+PyObjectAsString(PyTuple_GetItem(args, i));
    end;
    Result := ReturnNone;
  end; }
  //writeln(s);
  {with GetPythonEngine do begin
   PyArg_Parse( args, 's:WriteLn', @s );
   //ShowMessage( 'args of foo: '+PyObjectAsString(args) );
   system.Writeln(PyObjectAsString(args));
   Result := ReturnNone;
  end;  }
begin
    UndModule:= TPythonModule.Create(nil);
    UndModule.ModuleName:=rudLibName;
    UndModule.AddMethod('WriteLn',@Und_WriteLn,'WriteLn');
    UndModule.AddMethod('Write',@Und_Write,'Write');
    UndModule.AddMethod('GetL',@Und_GetL,'GetL');
    UndModule.AddMethod('SetL',@Und_SetL,'SetL');
    UndModule.AddMethod('GetG',@Und_GetG,'GetG');
    UndModule.AddMethod('SetG',@Und_SetG,'SetG');
    UndModule.Engine:=PyEngine;
end;

procedure TUndPython.PythonInputOutput1SendData(Sender: TObject;const Data: String);
begin
 Undhelper.WriteLn(Data);
end;

constructor TUndPython.Create(L : PLua_State);
begin
  pyengine := TPythonEngine.Create(nil);
  UndHelper.LuaState:=L;
  inout:=TPythonInputOutput.Create(nil);
  inout.onsenddata:=PythonInputOutput1SendData;
  pyengine.OnAfterLoad:=PythonEngine1AfterLoad;
  pyengine.IO:=inout;
  pyengine.AutoFinalize:=false;//false;
  //obj.UseWindowsConsole:=false;
  //obj.RedirectIO:=false;
  //obj.UseLastKnownVersion:=false;
  CreateUndModule;
  pyengine.LoadDll;
  UndModule.Initialize;
end;

destructor TUndPython.Destroy;
begin
  Undmodule.Free;
  inout.free;
  try pyengine.Free; except end;  // this causes a crash if the app is terminated in the middle of the execution
  inherited Destroy;
end;

constructor TUndPythonWrapper.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  obj:=TUndPython.create(L);
end;

function TUndPythonWrapper.GetPropValue(propName: String): Variant;
begin
  {if CompareText(propName, 'ErrorMsg') = 0 then result := obj.ErrorInfo else}
  Result:=inherited GetPropValue(propName);
end;

function TUndPythonWrapper.SetPropValue(PropName: String; const AValue: Variant
  ): Boolean;
begin
  result := true;
  //if CompareText(propName, 'Expression') = 0 then obj.Expression.text := AValue else
  Result:=inherited SetPropValue(propName, AValue);
end;

destructor TUndPythonWrapper.Destroy;
begin
  obj.free;
  inherited Destroy;
end;

end.

