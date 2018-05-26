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
{    The Original Code is dws2IDEUtils source code, released           }
{    October 26, 2002                                                  }
{                                                                      }
{    The Initial Developer of the Original Code is Mark Ericksen       }
{    Portions created by Mark Ericksen are                             }
{    Copyright (C) 2002 Mark Ericksen, United States of America.       }
{    All Rights Reserved.                                              }
{                                                                      }
{    Contributors: Andreas Luleich, Willibald Krenn                    }
{                                                                      }
{**********************************************************************}

{$I dws2.inc}

unit dws2IDEUtils;

interface

uses SysUtils, Classes,
{$IFDEF NEWVARIANTS}
  Variants,
{$ENDIF}
  dws2Comp, dws2Exprs, dws2Symbols, dws2Stack, dws2Errors;

type
  { The TdSyn_xxx things are using SynEdit specific formating strings }
  TdSyn_DisplayOption = (doSynStyle,        // uses synEdit markup tags for display
                         doIncludeImage,    // doIncludeImage requires doSynStyle
                         doShortText);      // abbreviate the text where applicable
  TdSyn_DisplayOptions = set of TdSyn_DisplayOption;

  { Content specific options. Separate from SynEdit styling }
  TContentOption = (coIncludeContext,       // list as "record.member", "class.member"
                    coIncludeUnit,          // include unit where declared
                    coMultiLine,            // list members for classes/records
                    coFunctionAsForward,    // create forwards for function symbols
                    coSimpleFunctionImpl    // create function implementations in "simple" format (no params or return type)
                  {, coDebugValue});
  TContentOptions = set of TContentOption;


{ Helper functions }
function GetImageIndex(ImageIndex: Integer; Include: Boolean): string;
function GetNameWithStyle(AName: string; UseStyle: Boolean=True): string;
// return the property that symbol is used as either a read or write access method
function GetPropertyForSymbol(ASym: TSymbol): TPropertySymbol;
function SymbolIsPropertyRead(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
function SymbolIsPropertyWrite(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;

// caller is responsible for freeing the TProgram result
function CompileWithSymbolsAndMap(Script: TDelphiWebScriptII; ScriptText: string): TProgram;

// Complete the class defined in the current 'Context'
function CompleteClassAtCursor(Compiler: TDelphiWebScriptII; AProgram: TProgram;
                               CurrentLine, CurrentRow: Integer;
                               ScriptText: TStrings;
                               BareBonesImpl: Boolean=False): Boolean;
// Complete all incomplete classes
function CompleteAllClasses(Compiler: TDelphiWebScriptII;
                            ScriptText: TStrings;
                            BareBonesImpl: Boolean=False): Boolean;
// Complete the passed in class
function CompleteClass(Compiler: TDelphiWebScriptII; AClassName: string; ScriptText: TStrings;
                       BareBonesImpl: Boolean=False): Boolean;

// Create a method implementation. If Barebones, no function result or params are included (still valid)
procedure CreateMethodImpl(Method: TMethodSymbol; InStrings: TStrings; BareBones: Boolean);

// given a line, find the function symbol that is declared or implemented, return symbol
function FindFuncDeclImplOnLine(ALine: Integer; Dictionary: TSymbolDictionary): TSymbolPosition;

// given a symbol's position, find the toggled Decl/Impl position
function FindFuncTogglePosition(Symbol: TSymbol; FromUsages: TSymbolUsages; Dictionary: TSymbolDictionary): TSymbolPosition;

// given a position, return the Function context it is within. If none, returns nil.
function FindFuncContext(ACol, ALine: Integer; ContextMap: TContextMap): TFuncSymbol;

// Return the position in the script for the alternate method declaration or implementation (opposite of current)
function FindToggledFuncDeclImplPos(ACol, ALine: Integer; AProgram: TProgram): TScriptPos;

//// For Ctrl+<hover> Return if it is a symbol that can be jumped to. (for painting)
//function CanFindSymbolDeclaration(ALine, ACol: Integer): Boolean;
//// find the script position of the symbol at the position for the declaration
//function FindDeclarationForSymbol(ALine, ACol: Integer): TScriptPos; overload;
//// find the script position of the symbol for the declaration
//function FindDeclarationForSymbol(Symbol: TSymbol): TScriptPos; overload;

// Return if a symbol's declaration point is prior to the given positon.
function SymbolDeclBeforePos(ACol, ALine: Integer; AProgram: TProgram; ASymbol: TSymbol): Boolean;

// Return the value of the value symbol for display in an IDE-type environment
function ValueSymbolAsString(ValueSym: TValueSymbol; Stack: TStack): string;

{ Minor 'helper' routines }

{ Convert boolean value to str (needed for multiple Delphi versions }
function BoolToStr(Value: Boolean): string;
{ Convert a variant to a string. Puts quotes around a string type. }
function VariantToStr(Value: Variant): string;


implementation

uses dws2Compiler;

{-----------------------------------------------------------------------------
  Procedure: BoolToStr
  Author:    Mark Ericksen
  Date:      24-Mar-2003
  Arguments: Value: Boolean
  Result:    string
  Purpose:   Give easy ability independent of Delphi version to convert a boolean
             value into a string.
-----------------------------------------------------------------------------}
function BoolToStr(Value: Boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

{-----------------------------------------------------------------------------
  Procedure: VariantToStr
  Author:    Mark Ericksen
  Date:      24-Mar-2003
  Arguments: Value: Variant
  Result:    string
  Purpose:   Convert a variant value to a string. Will add the quotes to string
             types so it is compatible with Delphi and DWS code.
-----------------------------------------------------------------------------}
function VariantToStr(Value: Variant): string;
begin
  if VarIsNull(Value) then
    Result := '<NULL>'
  else if VarIsEmpty(Value) then
    Result := '<Unassigned>'
  else if VarType(Value) = varString then   // NOTE: this is not OLE compatible
    Result := '''' + VarToStr(Value) + ''''
  else if VarType(Value) in [varSmallint..varDate, varBoolean] then
    Result := VarToStr(Value)
  else
    Result := '[other]';
    { TODO : Add support for displaying information about additional types here. }
end;


{-----------------------------------------------------------------------------
  Procedure: GetImageIndex
  Author:    Mark Ericksen
  Date:      03-Oct-2002
  Arguments: ImageIndex: Integer; Include: Boolean; (include text or not)
  Result:    string
  Purpose:   Return the SynCompletionProposal image index for an attached image list.
-----------------------------------------------------------------------------}
function GetImageIndex(ImageIndex: Integer; Include: Boolean): string;
begin
  if Include then
    Result := Format('\image{%d}\column{}', [ImageIndex])  // done as a column
  else
    Result := '';
end;


{-----------------------------------------------------------------------------
  Procedure: GetNameWithStyle
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: AName: string
  Result:    string
  Purpose:   Return the provided name as a column that is 'bold toggled'
-----------------------------------------------------------------------------}
function GetNameWithStyle(AName: string; UseStyle: Boolean): string;
begin
  if UseStyle then
    Result := Format('\column{}\style{~B}%s\column{}\style{~B}', [AName])
  else
    Result := AName;
end;


{-----------------------------------------------------------------------------
  Procedure: CompleteClassAtCursor
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: Compiler: TDelphiWebScriptII; AProgram: TProgram; CurrentLine, CurrentRow: Integer; ScriptText: TStrings; BareBonesImpl: Boolean
  Result:    Boolean
  Purpose:   Convenience wrapper that lets you easily attempt to complete any
             incomplete class that the cursor is inside of. (inside the context)
-----------------------------------------------------------------------------}
function CompleteClassAtCursor(Compiler: TDelphiWebScriptII; AProgram: TProgram;
                               CurrentLine, CurrentRow: Integer;
                               ScriptText: TStrings;
                               BareBonesImpl: Boolean): Boolean;
var
  Context: TContext;
begin
  Result := False;
  if Assigned(AProgram) then
  begin
    { Find the class that is to be completed }
    Context := AProgram.ContextMap.FindContext(CurrentRow, CurrentLine);
    if Assigned(Context) then
      if Context.ParentSym is TClassSymbol then
        Result := CompleteClass(Compiler, Context.ParentSym.Name{TClassSymbol(Context.ParentSym)}, ScriptText, BareBonesImpl);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: CreateMethodImpl
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: Method: TMethodSymbol; InStrings: TStrings; BareBones: Boolean
  Result:    None
  Purpose:   Creates an implementation stub for a method. The BareBones flag
             when true will supress all parameters and return types.
-----------------------------------------------------------------------------}
procedure CreateMethodImpl(Method: TMethodSymbol; InStrings: TStrings; BareBones: Boolean);
var
  p: Integer;
  ParamText: string;
  FuncReturnType: string;
  FuncType: string;
begin
  Assert(Assigned(Method));
  Assert(Assigned(InStrings));
  { Assemble the method text piece-wise. }

  FuncReturnType := '';
  { Get string description of function type }
  case Method.Kind of
  fkFunction    :
    begin
      FuncType  := 'function';
      if (not BareBones) and Assigned(Method.Result) then
        FuncReturnType := ': '+Method.Typ.Caption;
    end;
  fkProcedure   :
    begin
      FuncType := 'procedure';
    end;
  fkConstructor :
    begin
      FuncType := 'constructor';
    end;
  fkDestructor  :
    begin
      FuncType := 'destructor';
    end;
  else
    FuncType  := '<unknown>';
  end;

  // Add 'class' to class methods
  if Method.IsClassMethod then
    FuncType := 'class ' + FuncType;

  ParamText := '';
  // load params for function
  if not BareBones then
  begin
    for p := 0 to Method.Params.Count - 1 do begin
      ParamText := ParamText + Method.Params[p].Description;
      if p < Method.Params.Count - 1 then
        ParamText := ParamText + '; ';
    end;
    // Add parenthesis if there are params
    if ParamText <> '' then
      ParamText := '('+ParamText+')';
  end;

  InStrings.Add('');
  InStrings.Add(Format('%s %s.%s%s%s;', [FuncType, Method.ClassSymbol.Name, Method.Name, ParamText, FuncReturnType]));
  InStrings.Add('begin');
  InStrings.Add('');  //  // auto-generated');
  InStrings.Add('end;');
end;

{-----------------------------------------------------------------------------
  Procedure: CompleteAllClasses
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: Compiler: TDelphiWebScriptII; ScriptText: TStrings; BareBonesImpl: Boolean=False
  Result:    Boolean
  Purpose:   All classes will be completed by default. Only options are for
             how much detail should be added to the implementation (parameters
             and return type -- BareBonesImpl)
-----------------------------------------------------------------------------}
function CompleteAllClasses(Compiler: TDelphiWebScriptII;
                            ScriptText: TStrings;
                            BareBonesImpl: Boolean=False): Boolean;
begin
  // if one or more was built, will return true
  Result := CompleteClass(Compiler, '', ScriptText, BareBonesImpl);
end;

{-----------------------------------------------------------------------------
  Procedure: CompleteClass
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: Compiler: TDelphiWebScriptII; AClassName: string; ScriptText: TStrings; BareBonesImpl: Boolean=False
  Result:    Boolean
  Purpose:   If AClassName has a value then just that class will be completed.
             If AClassName is blank, then any incomplete class will be changed.
             BareBonesImpl means no parameters or return types will be added
             to the implementation. 
-----------------------------------------------------------------------------}
function CompleteClass(Compiler: TDelphiWebScriptII; AClassName: string;
                       ScriptText: TStrings; BareBonesImpl: Boolean=False): Boolean;
var
  i, x: Integer;
  method: TMethodSymbol;
  insertDeclLine: Integer;
  insertDeclCol: Integer;
  ErrorClassName: string;
  fixClassPos: TSymbolPosition;
  fixClass: TClassSymbol;   // class to complete
  locProgram: TProgram;   // local program used for recompiles and completion help
  ChangedInLoop: Boolean; // flag to denote if a change was made this time through the loop
begin
  Result := False;
  if (ScriptText = nil) then
    Exit;

  locProgram := CompileWithSymbolsAndMap(Compiler, ScriptText.Text);
  try
    { Take multiple passes. Recompile as we cycle and keep completing until nothing
      left to complete. Recompiles are needed because we stop compiling on
      error conditions. As the condition is satisfied, need to recompile to get
      further class members checked. }
    repeat
      ChangedInLoop := False;

      // needs to continously recompile the script until all possible changes have been made (re-collect pointers each time)
      for i := 0 to Length(locProgram.ClassCompleteNeeds) - 1 do
      begin
        Assert(Assigned(locProgram.ClassCompleteNeeds[i].ErrorClass));

        ErrorClassName := locProgram.ClassCompleteNeeds[i].ErrorClass.Name;
        { If no specific class is specified, fix errors with any class that has problems. }
        if AClassName = '' then
          fixClassPos := locProgram.SymbolDictionary.FindSymbolUsage(TSymbol(locProgram.ClassCompleteNeeds[i].ErrorClass), suDeclaration)
        else
          fixClassPos := locProgram.SymbolDictionary.FindSymbolUsageOfType(AClassName, TClassSymbol, suDeclaration);

        if not Assigned(fixClassPos) then
          raise Exception.CreateFmt('Class completion failed. Cannot find class "%s".', [ErrorClassName]);

        fixClass := TClassSymbol(fixClassPos.Symbol);        // get a shortcut for easer operation

        { If not completing all and the classes don't match, move to next item.
          The repeat..until will stop if no changes could be made }
        if (AClassName<>'') and (not SameText(AClassName, ErrorClassName)) then
          Continue;

        // property completion needed
        if locProgram.ClassCompleteNeeds[i].ErrorType = ccePropAccessDeclMissing then
        begin
          // default insert position to first line after class declaration
          insertDeclCol := fixClassPos.ScriptPos.Col + 1;
          insertDeclLine := fixClassPos.ScriptPos.Line + 1;

          // cycle the members, find first place fit for new declaration
          for x := 0 to fixClass.Members.Count - 1 do
          begin
            // if a Field, default new position to be AFTER the field.
            if fixClass.Members[x] is TFieldSymbol then
              insertDeclLine := locProgram.SymbolDictionary.FindSymbolUsage(fixClass.Members[x], suDeclaration).ScriptPos.Line + 1;
          end; {for x}
          // insert the completed property information
          ScriptText.Insert(insertDeclLine-1, StringOfChar(' ', insertDeclCol) +
                                              locProgram.ClassCompleteNeeds[i].SuggestedFix);
          Result := True;    // a change was successfully made
          ChangedInLoop := True;
        end; {property completion}

        { Method implementation completion }
        if locProgram.ClassCompleteNeeds[i].ErrorType = cceMethodImplMissing then
        begin
          for x := 0 to fixClass.Members.Count - 1 do
            if fixClass.Members[x] is TMethodSymbol then
            begin
              method := fixClass.Members[x] as TMethodSymbol;
              { If the method is not abstract and is declared here but not implemented }
              if not method.IsAbstract and
                 Assigned(locProgram.SymbolDictionary.FindSymbolUsage(method, suDeclaration)) and
                 (locProgram.SymbolDictionary.FindSymbolUsage(method, suImplementation) = nil) then
              begin
                CreateMethodImpl(method, ScriptText, BareBonesImpl);
                Result := True;    // at least one was built
                ChangedInLoop := True;
              end;
            end;
        end;
      end; {for i}

      { Recompile - free program, compile new one }
      locProgram.Free;
      locProgram := CompileWithSymbolsAndMap(Compiler, ScriptText.Text);

      // if nothing was changed and there are still errors, show message and abort.
      if (not ChangedInLoop) and (Length(locProgram.ClassCompleteNeeds) > 0) then
        if (AClassName = '') or SameText(AClassName, ErrorClassName) then
          raise Exception.CreateFmt('Unable to complete class "%s". Correct other script errors first.', [ErrorClassName]);

    until (Length(locProgram.ClassCompleteNeeds) = 0) or (not ChangedInLoop);

  finally
    locProgram.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: FindFuncDeclImplOnLine
  Author:    Mark Ericksen
  Date:      17-Oct-2002
  Arguments: ALine: Integer; Dictionary: TSymbolDictionary
  Result:    TSymbol
  Purpose:   Return the function symbol that is either declared or implemented
             on the specified line. Returns nil if nothing fits it.
-----------------------------------------------------------------------------}
function FindFuncDeclImplOnLine(ALine: Integer; Dictionary: TSymbolDictionary): TSymbolPosition;
var
  i: Integer;
  Declared, Implement: TSymbolPosition;
begin
  Result := nil;
  for i := 0 to Dictionary.Count - 1 do
  begin
    { If a method, look for Declaration, Implementation }
    if Dictionary[i].Symbol is TMethodSymbol then
    begin
      Declared := Dictionary[i].FindUsage(suDeclaration);
      if Assigned(Declared) then
        if Declared.ScriptPos.Line = ALine then
          Result := Declared;

      Implement:= Dictionary[i].FindUsage(suImplementation);
      if Assigned(Implement) then
        if Implement.ScriptPos.Line = ALine then
          Result := Implement;
    end

    { If a function (not method), look for Forward, Declaration }
    else if Dictionary[i].Symbol is TFuncSymbol then
    begin
      // forwards are the early declarations
      Declared := Dictionary[i].FindUsage(suForward);
      if Assigned(Declared) then
        if Declared.ScriptPos.Line = ALine then
          Result := Declared;

      // declaration is the implementation for functions
      Implement:= Dictionary[i].FindUsage(suDeclaration);
      if Assigned(Implement) then
        if Implement.ScriptPos.Line = ALine then
          Result := Implement;
    end;
    if Assigned(Result) then
      Break;
  end; {for}
end;

{-----------------------------------------------------------------------------
  Procedure: FindFuncTogglePosition
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: Symbol: TSymbol; FromUsages: TSymbolUsages; Dictionary: TSymbolDictionary
  Result:    TSymbolPosition
  Purpose:   Return the symbol position that corresponds to the current function
             or method. This is used to jump from a method declaration to the
             interface and back. A function (non-method) can have a 'forward'
             and this can be used as a jump point.
-----------------------------------------------------------------------------}
function FindFuncTogglePosition(Symbol: TSymbol; FromUsages: TSymbolUsages; Dictionary: TSymbolDictionary): TSymbolPosition;
begin
  Result := nil;
  if not Assigned(Symbol) then Exit;

  { If a method, determine toggled usage }
  if Symbol is TMethodSymbol then
  begin
    if suDeclaration in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suImplementation)
    else if suImplementation in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suDeclaration);
  end
  { If a function, determine toggled usage }
  else if Symbol is TFuncSymbol then
  begin
    if suForward in FromUsages then
      Result := Dictionary.FindSymbolUsage(Symbol, suImplementation)
    else if (suDeclaration in FromUsages) or (suImplementation in FromUsages) then
      Result := Dictionary.FindSymbolUsage(Symbol, suForward);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: FindFuncContext
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: ACol, ALine: Integer; ContextMap: TContextMap
  Result:    TFuncSymbol
  Purpose:   Find the Function symbol associated with the current location in
             the script. Nil is returned if none found.
-----------------------------------------------------------------------------}
function FindFuncContext(ACol, ALine: Integer; ContextMap: TContextMap): TFuncSymbol;
var
  Context: TContext;
begin
  Result := nil;

  Context := ContextMap.FindContext(ACol, ALine);
  if Assigned(Context) then
  begin
    { Determine if context is a function. Go up parent tree until reach top. }
    repeat
      if Context.ParentSym is TFuncSymbol then
        Result := TFuncSymbol(Context.ParentSym)
      else
        Context := Context.Parent;
    until Assigned(Result) or (Context = nil);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: GetPropertyForSymbol
  Author:    Mark Ericksen
  Date:      20-Nov-2002
  Arguments: ASym: TSymbol
  Result:    TPropertySymbol
  Purpose:   Given a symbol, return the property for which it is either
             a read or write access method. Currently only applies to
             TMethodSymbols and TFieldSymbols.
-----------------------------------------------------------------------------}
function GetPropertyForSymbol(ASym: TSymbol): TPropertySymbol;
var
  i: Integer;
  ClassSym: TClassSymbol;
begin
  Result := nil;

  if ASym is TMethodSymbol then
    ClassSym := TMethodSymbol(ASym).ClassSymbol
  else if ASym is TFieldSymbol then
    ClassSym := TFieldSymbol(ASym).ClassSymbol
  else
    ClassSym := nil;

  if Assigned(ClassSym) and Assigned(ASym) then
  begin
    for i := 0 to ClassSym.Members.Count - 1 do
    begin
      if ClassSym.Members[i] is TPropertySymbol then
        if (TPropertySymbol(ClassSym.Members[i]).ReadSym = ASym) or
           (TPropertySymbol(ClassSym.Members[i]).WriteSym = ASym)
        then
        begin
          // Return pointer to the property symbol
          Result := TPropertySymbol(ClassSym.Members[i]);
          Break;
        end;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SymbolIsPropertyRead
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: AProperty: TPropertySymbol; ASym: TSymbol
  Result:    Boolean
  Purpose:   Return if a symbol is used in the 'read' portion of an object property.
-----------------------------------------------------------------------------}
function SymbolIsPropertyRead(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
begin
  // Pointer match is the answer.
  Result := False;
  if Assigned(AProperty) and Assigned(ASym) then
    Result := AProperty.ReadSym = ASym;
end;

{-----------------------------------------------------------------------------
  Procedure: MethodIsWrite
  Author:    Mark Ericksen
  Date:      07-Jan-2003
  Arguments: AProperty: TPropertySymbol; AFunc: TFuncSymbol
  Result:    Boolean
  Purpose:   Return if a method is used in the 'write' portion of an object property.
-----------------------------------------------------------------------------}
function SymbolIsPropertyWrite(AProperty: TPropertySymbol; ASym: TSymbol): Boolean;
begin
  // Pointer match is the answer.
  Result := False;
  if Assigned(AProperty) and Assigned(ASym) then
    Result := AProperty.WriteSym = ASym;
end;

{-----------------------------------------------------------------------------
  Procedure: CompileWithSymbolsAndMap
  Author:    Mark Ericksen
  Date:      09-Oct-2002
  Arguments: Script: TDelphiWebScriptII; ScriptText: string
  Result:    TProgram
  Purpose:   Compile a script using SymbolDictionary and ContentMap. Restores settings immediately.
-----------------------------------------------------------------------------}
function CompileWithSymbolsAndMap(Script: TDelphiWebScriptII; ScriptText: string): TProgram;
var
  OldOpts : TCompilerOptions;
begin
  // compile the script (store original options, turn on needed options)
  OldOpts := Script.Config.CompilerOptions;
  try
    Script.Config.CompilerOptions := Script.Config.CompilerOptions + [coSymbolDictionary, coContextMap];
    Result := Script.Compile(ScriptText);
  finally
    Script.Config.CompilerOptions := OldOpts;
  end;
end;


{-----------------------------------------------------------------------------
  Procedure: FindToggledFuncDeclImplPos
  Author:    Mark Ericksen
  Date:      05-Dec-2002
  Arguments: ACol, ALine: Integer; AProgram: TProgram
  Result:    TScriptPos  (If not found, NullPos is returned (values are -1)
  Purpose:   Return toggled position between Implementation and Declaration
             (procedures/methods)
-----------------------------------------------------------------------------}
function FindToggledFuncDeclImplPos(ACol, ALine: Integer; AProgram: TProgram): TScriptPos;
var
  SymPos,
  TogglePos: TSymbolPosition;
  FuncSym: TFuncSymbol;
  FromUsages: TSymbolUsages;
begin
  SymPos := FindFuncDeclImplOnLine(ALine, AProgram.SymbolDictionary);
  { If assigned, use the function symbol }
  if Assigned(SymPos) then
  begin
    FuncSym := TFuncSymbol(SymPos.Symbol);
    FromUsages := SymPos.SymbolUsages;
  end
  { If nothing found, check context  }
  else
  begin
    FuncSym := FindFuncContext(ACol, ALine, AProgram.ContextMap);
    FromUsages := [suImplementation];
  end;

  { If we found the function symbol in question, find where to jump to. }
  if Assigned(FuncSym) then
  begin
    TogglePos := FindFuncTogglePosition(FuncSym, FromUsages, AProgram.SymbolDictionary);
    if Assigned(TogglePos) then
      Result := TogglePos.ScriptPos  // Return position
    else
      Result := NullPos;             // return invalid position
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: SymbolDeclBeforePos
  Author:    Mark Ericksen
  Date:      21-Mar-2003
  Arguments: ACol, ALine: Integer; AProgram: TProgram; ASymbol: TSymbol
  Result:    Boolean
  Purpose:   Determine if a position (ACol, ALine) is before the declaration of
             a symbol. 
-----------------------------------------------------------------------------}
function SymbolDeclBeforePos(ACol, ALine: Integer; AProgram: TProgram; ASymbol: TSymbol): Boolean;
var
  SymPos: TSymbolPosition;
begin
  Assert(AProgram <> nil);
  Assert(ASymbol <> nil);

  // Check if position is before declaration position
  SymPos := AProgram.SymbolDictionary.FindSymbolUsage(ASymbol, suDeclaration);
  if Assigned(SymPos) then
    // if same line and before column position or on a previous line
    Result := ((ALine = SymPos.ScriptPos.Line) and (SymPos.ScriptPos.Col <= ACol ))
               or (SymPos.ScriptPos.Line < ALine)
  else  // if declaration of symbol not found, then assumed prior to position
    Result := True;
end;

{-----------------------------------------------------------------------------
  Procedure: ValueSymbolAsString
  Author:    Mark Ericksen
  Date:      08-Jun-2003
  Arguments: DataSym: TDataSymbol
  Result:    string
  Purpose:   Return the string representation of the value of the value symbol.
             This will format it as suitable for display in an IDE. Ex: a class
             will display as "()" while a string will be "'my string'"
-----------------------------------------------------------------------------}
function ValueSymbolAsString(ValueSym: TValueSymbol; Stack: TStack): string;
var
  typ: TTypeSymbol;
  Value: Variant;
begin
  Result := '';
  if not Assigned(ValueSym.Typ) then
    EXIT;

//  Value := Stack.ReadValue(ValueSym. StackAddr);
//  typ := ValueSym.Typ as TTypeSymbol;

  if typ is TClassSymbol then
  begin
    if Value = Unassigned then
      Result := 'nil'
    else
      Result := '()';
  end
  else if VarType(Value) = varString then
    Result := '''' + Value + ''''
  else
    Result := Value;
end;


end.
