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
{    The Original Code is DWSUnitEditorUtils source code, released     }
{    January 10, 2003                                                  }
{                                                                      }
{    The Initial Developer of the Original Code is Mark Ericksen       }
{    Portions created by Mark Ericksen are                             }
{    Copyright (C) 2003 Mark Ericksen, United States of America.       }
{    All Rights Reserved.                                              }
{                                                                      }
{**********************************************************************}
{ Known Issues:
-------------------------------------------------------------------------------}
unit dws2SynEditUtils;

interface

uses Windows, SysUtils, Classes, Graphics, SynEdit, SynCompletionProposal,
  SynEditTypes, dws2Comp, dws2Symbols, dws2Exprs;

{ Core functions that are used for Code completion, Parameter display and Symbol hints. }
function PerformCodeCompletion(Editor: TCustomSynEdit; Prg: TProgram;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings;
                             var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;
function PerformParamProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean; overload;
function PerformHintProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer;
                             IncludeImages: Boolean): Boolean; overload;

{ Core functions that are wrapped for convenience }
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings;
                             var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             var x, y: Integer;
                             IncludeImages, IncludePropertyAccessors: Boolean): Boolean; overload;
function PerformParamProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean; overload;
function PerformHintProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer;
                             IncludeImages: Boolean): Boolean; overload;

{ Navigation function }
procedure ToggleFromDecl2Impl(Editor: TCustomSynEdit; AProgram: TProgram);

// if a point is in a comment or a string (as defined by the highlighter)
function IsInCommentOrString(Editor: TCustomSynEdit; APoint: TPoint): Boolean;

implementation

uses dws2IDEUtils, dws2VCLIDEUtils, dws2Errors, SynEditHighlighter;

{-----------------------------------------------------------------------------
  Procedure: CodeProposalProcess
  Author:    Mark Ericksen
  Date:      18-Sep-2002
  Arguments: Editor: TCustomSynEdit;
             Prg: TProgram;            (compiled DelphiWebScript program)
             CodeProposal: TSynCompletionProposal;
             UnitItemsList, UnitInserts: TStrings;  (List of symbols in units compiled into application)
             var x, y: Integer         (position of popup window)
  Result:    Boolean
  Purpose:   Return if the CodeProposal should be executed. Fill the properties
             of the component based on the current position in the editor.
-----------------------------------------------------------------------------}
function PerformCodeCompletion(Editor: TCustomSynEdit; Prg: TProgram;
  CodeProposal: TSynCompletionProposal; UnitItemsList, UnitInserts: TStrings;
  var x, y: Integer; IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
var
  i, SearchStart: Integer;
  SearchSymbol: TSymbol;
  dispOpts: TdSyn_DisplayOptions;
  Context: TContext;
  ThisWordStart: TPoint;  // position where this word begins
  PrevWordStart: TPoint;  // position where previous word start
  PrevWordStop: TPoint;   // position where previous word stops
  tmpLineText: string;
  includeFuncSelf,        // if the function's 'self' member should be included
  includeClassSelf: Boolean;   // if the class' "Self" should be included (used for class methods - "Self" is an alias to the class type)
  showAll: Boolean;
begin
  Assert(Editor <> nil, 'CodeProposalExecute - The Editor parameter must be valued.');
  Assert(Prg <> nil, 'CodeProposalExecute - The Program parameter must be valued.');
  Assert(CodeProposal <> nil, 'CodeProposalExecute - The CodeProposal parameter must be valued.');

  Result := not IsInCommentOrString(Editor, Editor.CaretXY); // don't display if inside a comment or a string
  if not Result then    // if not allowed to popup,
    EXIT;               // don't continue

  showAll := True;      // default to show full list
  { Set display options }
  dispOpts := [doSynStyle, doShortText];
  if IncludeImages then                 // if graphics used,
    Include(dispOpts, doIncludeImage);  // add the graphics

  CodeProposal.ClearList;

  // get the previous symbol
  ThisWordStart := Editor.WordStart;
  // This is the first word, previous is the same
  if (ThisWordStart.X = 1) and (ThisWordStart.Y = 1) then
    PrevWordStop := ThisWordStart
  // There are other words before this one
  else
  begin
    PrevWordStart := Editor.PrevWordPosEx(Point(ThisWordStart.X-1, ThisWordStart.Y));   // find previous word's starting point
    PrevWordStop  := Editor.WordEndEx(PrevWordStart);
  end;

  SearchSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(ThisWordStart.X, ThisWordStart.Y);
  // if no Symbol was found, search to previous symbol for context (MyClass.Somethi - Somethi won't be found, go back to class for context)
  if (SearchSymbol = nil) and (ThisWordStart.Y = PrevWordStop.Y) then  // this word and previous on same line
  begin
    tmpLineText := Editor.LineText;
    SearchStart := Length(tmpLineText)+1;  // +1 because the .X & .Y positions are 1 based. An empty line needs to start at 1
    if ThisWordStart.X < SearchStart then
      SearchStart := ThisWordStart.X;
    // search from current word start to previous word stop (what is in between?)
    if SearchStart > PrevWordStop.X then    // delphi will loop 1 downto 1
      for i := SearchStart downto PrevWordStop.X do
      begin
        // if a '.' separates the words, use the previous word as a context
        if tmpLineText[i] = '.' then
        begin
          showAll := False;    // do NOT show full list. Should be a reduced list
          SearchSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(PrevWordStart.X, PrevWordStart.Y);
          Break;
        end;
      end;{for i}
  end;

  // if a function, look at return type for base symbol
  if SearchSymbol is TFuncSymbol then
  begin
    if TFuncSymbol(SearchSymbol).Kind = fkFunction then
    begin
      if SearchSymbol is TMethodSymbol then
        SearchSymbol := TMethodSymbol(SearchSymbol).Result
      else
        SearchSymbol := TFuncSymbol(SearchSymbol).Result;
    end;
  end;

  // if Symbol found is a value symbol then get the variable type
  if SearchSymbol is TValueSymbol then
    SearchSymbol := SearchSymbol.Typ;

  // if TClassSymbol
  if (SearchSymbol is TClassSymbol) then
    LoadClassSymbolToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TClassSymbol(SearchSymbol), True, IncludePropertyAccessors,
                             False, False, dispOpts)
  // if TRecordSymbol
  else if (SearchSymbol is TRecordSymbol) then
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         TRecordSymbol(SearchSymbol).Members, False, True, False, False, dispOpts)
  // if TUnitSymbol
  else if (SearchSymbol is TUnitSymbol) then begin
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         TUnitSymbol(SearchSymbol).Table, False, True, False, False, dispOpts);
    if TUnitSymbol(SearchSymbol).Table is TLinkedSymbolTable then
      LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                           TLinkedSymbolTable(TUnitSymbol(SearchSymbol).Table).Parent,
                           False, True, False, False, dispOpts);
  end
  { If there is no symbol found, load the full global set including context based ones }
  else if (SearchSymbol = nil) and showAll then begin
    { Start by examining context, if within a procedure, list those first? }
    // Check ContextMap for symbols local to the context
    Context := Prg.ContextMap.FindContext(Editor.CaretX, Editor.CaretY);
    while Assigned(Context) do  // don't stop until no contexts to check
    begin
      // add symbols local to a context
      if Assigned(Context.LocalTable) then
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             Context.LocalTable, False, False, False, False, dispOpts,
                             Prg, Editor.CaretX, Editor.CaretY);

      // add symbols specific to a function
      if Context.ParentSym is TFuncSymbol then
      begin
        // Determine if the "Self" variable should be added.
        // If function is a 'class method' then the "classSelf" should be included
        // as it is an alias to the class type. If the function is just a normal
        // method, then the 'self' is a data variable pointing to the class instance.
        includeFuncSelf := True;
        includeClassSelf := False;
        if Context.ParentSym is TMethodSymbol then
          if TMethodSymbol(Context.ParentSym).IsClassMethod then
          begin
            includeClassSelf := True;    // include alias to class type
            includeFuncSelf := False;    // don't include function's "self" variable to class instance
          end;

        // add function parameters to the list
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TFuncSymbol(Context.ParentSym).Params,
                             False, False, False, False, dispOpts);

        //  add internal variables (result, self, etc) to the list
        LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                             TFuncSymbol(Context.ParentSym).InternalParams,
                             False, False, False, includeFuncSelf, dispOpts, // include variable 'Self'
                             Prg, Editor.CaretX, Editor.CaretY);

        // if a method, add the object's members to the list
        if Context.ParentSym is TMethodSymbol then
          // load this class' members to the lists
          LoadClassSymbolToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                                   TMethodSymbol(Context.ParentSym).ClassSymbol,
                                   True, IncludePropertyAccessors, includeClassSelf, False, dispOpts); 
      end;

      // pop up to the next level context and continue checking
      Context := Context.Parent;
    end;

    // add script specific items (shown at top)
    LoadSymbolsToStrings(CodeProposal.ItemList, CodeProposal.InsertList,
                         Prg.Table, False, False, IncludePropertyAccessors, False, dispOpts,
                         Prg, Editor.CaretX, Editor.CaretY);  // limit to symbols declared before position

    { Add unit global items (shown after script-based items) }

    { if not provided pre-built lists, then fill lists here.}
    if (UnitItemsList = nil) or (UnitInserts = nil) then begin
      LoadUnitDeclaredSymbols(Prg, CodeProposal.ItemList, CodeProposal.InsertList, dispOpts, [], False);
    end
    { Provided pre-built lists of unit declared symbols, Append them now. }
    else begin
      CodeProposal.ItemList.AddStrings(UnitItemsList);
      CodeProposal.InsertList.AddStrings(UnitInserts);
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: PerformParamProposal
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: Editor: TCustomSynEdit; Prg: TProgram; ParamProposal: TSynCompletionProposal; var x, y: Integer
  Result:    Boolean
  Purpose:   Will show the Param Proposal dialog filled with the data for the appropriate symbol.
-----------------------------------------------------------------------------}
function PerformParamProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean;
var
  locLine: String;
  TmpX, TmpY, StartX, StartY,
  TmpLocation: Integer;
  FoundSymbol: TSymbol;
  p: Integer;
  ParamText: string;
  tmpPoint: TPoint;
  ParamDelim: string;

  { Related to paren matching (taken from SynEdit.pas GetMatchingBracketEx() }
  Test: Char;
  NumBrackets: Integer;
  isCommentOrString: Boolean;
  thisParam: string;
begin
  Assert(Assigned(Editor.Highlighter), 'A highligher must be assigned to the editor.');

  with Editor do
  begin
    { We start by decrementing X, by adding a ' ' to then end of the line it makes it so
      we don't lose any meaningful data when a line reads like this "thing," because
      the last character is a ',' (it would have been skipped by first dec(X). }
    locLine := LineText + ' ';

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    TmpY := CaretY;
    if TmpX > Length(locLine) then
      TmpX := Length(locLine)
    else if TmpX = 1 then
      TmpX := 2;     // immediately gets decremented (allows it to work when at beginning of line)

    NumBrackets := 0;
    FoundSymbol := nil;
    TmpLocation := 0;
    dec(TmpX);
    while (TmpX > 0) and (TmpY > 0) and (FoundSymbol = nil) do
    begin
      isCommentOrString := IsInCommentOrString(Editor, Point(TmpX, TmpY));

      if ((LocLine[TmpX] = ')') or (NumBrackets > 0)) and (not isCommentOrString) then
      begin
        // search until start of line
        Inc(NumBrackets);// := 1;
        while (TmpX > 1) and (NumBrackets > 0) do begin
          Dec(TmpX);
          Test := LocLine[TmpX];
          if (Test = ')') or (Test = '(') then
          begin
            isCommentOrString := IsInCommentOrString(Editor, Point(TmpX, TmpY));
            if (Test = ')') and (not isCommentOrString) then
              Inc(NumBrackets)
            else if (Test = '(') and (not isCommentOrString) then
              Dec(NumBrackets);
          end;
        end; {while}
      end
      else if (locLine[TmpX] = '(') and (not isCommentOrString) then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        StartY := TmpY;
        { Set the Popup window to start after the main word on the line after
          the current line. 
          NOTE: The default behavior would be to move the popup window along with
                the cursor as you type. This makes it more like Delphi. }
        // use tmpPoint to get the screen coordinates for the popup
        tmpPoint := ClientToScreen(RowColumnToPixels(Point(StartX+1, CaretY+1)));
        x := tmpPoint.X;
        y := tmpPoint.Y;
        // re-use tmpPoint to get the position of the previous word
        tmpPoint := PrevWordPosEx(Point(StartX, StartY));
        FoundSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(tmpPoint.X, tmpPoint.Y);
        if FoundSymbol = nil then
          dec(TmpX);
      end
      else if (LocLine[TmpX] = ',') and (not isCommentOrString) then
        inc(TmpLocation);

      // if reached the beginning of line, go up a line and begin searching from the end
      if TmpX <= 1 then
      begin
        dec(TmpY);
        locLine := Lines[TmpY-1] + ' ';  //-1 because Lines is 0 based
        TmpX := Length(locLine);
      end;
      dec(TmpX);
    end;
  end;

  Result := False;   // default to NOT execute

  if FoundSymbol <> nil then
  begin
    ParamProposal.ItemList.Clear;

    // needs to keep searching the previous word until reached a TFuncSymbol word, position CurrentIndex at that point
    if FoundSymbol is TFuncSymbol then
    begin
      ParamDelim := ';';    // separate parameters in the list with ';'
      // set current index to highlight. Total param count - commas encountered
      ParamProposal.Form.CurrentIndex := TmpLocation;
      for p := 0 to TFuncSymbol(FoundSymbol).Params.Count - 1 do
      begin
        thisParam := TFuncSymbol(FoundSymbol).Params[p].Description;
        if Length(TParamSymbol(TFuncSymbol(FoundSymbol).Params[p]).DefaultValue) > 0 then
          thisParam := '[' + thisParam + ']';    // has a default value, display appropriately
        ParamText := ParamText + '"' + thisParam + ParamDelim+'"';
        if p < TFuncSymbol(FoundSymbol).Params.Count - 1 then  // if not the last param
          ParamText := ParamText + ', '                        //   separate with a comma
        else                                                   // if the last one
          ParamDelim := '';                                    //   clear the delimeter
      end;
      // if something to add, add to list
      if ParamText <> '' then
        ParamProposal.ItemList.Add(ParamText);
      Result := True;
    end;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: PerformHintProposal
  Author:    Mark Ericksen
  Date:      21-Sep-2002
  Arguments: Editor: TCustomSynEdit; Prg: TProgram; HintProposal: TSynCompletionProposal; var x, y: Integer
  Result:    Boolean
  Purpose:   Shows the pop-up hint information for the focused symbol.
-----------------------------------------------------------------------------}
function PerformHintProposal(Editor: TCustomSynEdit; Prg: TProgram;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer; IncludeImages: Boolean): Boolean;
var
  FoundSymbol: TSymbol;
  SymText: string;    // text to display to represent the symbol
  DispOpts: TdSyn_DisplayOptions;
  MousePos,
  WordStart: TPoint;
  SymPos: TSymbolPosition;      // position in script where symbol is defined
begin
  Result := False;              // default to NOT show it.
  if not Editor.GetPositionOfMouse(MousePos) then
    EXIT;

  HintProposal.ClBackground := clInfoBk;
  HintProposal.Font.Color   := clInfoText;
  HintProposal.ItemList.Clear;
  DispOpts := [doSynStyle];
  if IncludeImages then
    Include(DispOpts, doIncludeImage);

  try
    WordStart := Editor.WordStartEx(MousePos);
    FoundSymbol := Prg.SymbolDictionary.FindSymbolAtPosition(WordStart.X, WordStart.Y);
    if FoundSymbol <> nil then
    begin
      { If program is running, make the hint display the current value }
      if Prg.IsDebugging and (Prg.ProgramState in [psRunning, psRunningStopped]) then
      begin
        if FoundSymbol is TValueSymbol then
        begin
//          HintProposal.ItemList.Add(Format('%s = %s',
//                                           [FoundSymbol.Name,
//                                            DataSymbolAsString(TValueSymbol(FoundSymbol), Prg.Stack)]));
          Result := True;
        end;
      end
      { If not running and okay to display descriptive text }
      else if Prg.ProgramState in [psReadyToRun, psTerminated] then
      begin
        SymText := SymbolToText(Prg.Table, FoundSymbol, DispOpts,  [coIncludeContext, coIncludeUnit], nil);
        if SymText <> '' then begin     // if something to display, show it. ('Integer' is a symbol but is not represented with anything)
          // find the line where symbol is declared, add to end if found
          SymPos := Prg.SymbolDictionary.FindSymbolUsage(FoundSymbol, suDeclaration);
          if Assigned(SymPos) then
            SymText := SymText + Format(' (%d)', [SymPos.ScriptPos.Line]);
          HintProposal.ItemList.Add(SymText);
          Result := True;   // OK to show it
        end;
      end;
    end;

    // position the Hint window to be at the start of the word on the following line
    y := Editor.ClientToScreen(Editor.RowColumnToPixels(Point(WordStart.X, WordStart.Y+1))).Y; // position under the current item
    x := Editor.ClientToScreen(Editor.RowColumnToPixels(WordStart)).X;
  finally
    // if we can't show it, if it IS showing, turn it off (may be from previous time)
    if (not Result) and HintProposal.Form.Visible then
      HintProposal.CancelCompletion;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: PerformCodeCompletion
  Author:    Mark Ericksen
  Date:      23-Sep-2002
  Arguments: Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII; CodeProposal: TSynCompletionProposal; UnitItemsList, UnitInserts: TStrings; var x, y: Integer
  Result:    Boolean
  Purpose:   Simple wrapper for easier use.
-----------------------------------------------------------------------------}
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             UnitItemsList, UnitInserts: TStrings;
                             var x, y: Integer; IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);
  try
    Result := PerformCodeCompletion(Editor, Prg, CodeProposal, UnitItemsList,
                                    UnitInserts, x, y, IncludeImages,
                                    IncludePropertyAccessors);
  finally
    Prg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: PerformCodeCompletion
  Author:    Mark Ericksen
  Date:      23-Sep-2002
  Arguments: Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII; CodeProposal: TSynCompletionProposal; var x, y: Integer
  Result:    Boolean
  Purpose:   Simple wrapper for easier use.
-----------------------------------------------------------------------------}
function PerformCodeCompletion(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             CodeProposal: TSynCompletionProposal;
                             var x, y: Integer; IncludeImages, IncludePropertyAccessors: Boolean): Boolean;
begin
  // Pass nil string lists. Force it to fill them internally.
  Result := PerformCodeCompletion(Editor, DWScript, CodeProposal, nil, nil, x, y,
                                  IncludeImages, IncludePropertyAccessors);
end;

{-----------------------------------------------------------------------------
  Procedure: PerformParamProposal
  Author:    Mark Ericksen
  Date:      23-Sep-2002
  Arguments: Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII; ParamProposal: TSynCompletionProposal; var x, y: Integer
  Result:    Boolean
  Purpose:   Simple wrapper for easier use.
-----------------------------------------------------------------------------}
function PerformParamProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             ParamProposal: TSynCompletionProposal;
                             var x, y: Integer): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);
  try
    Result := PerformParamProposal(Editor, Prg, ParamProposal, x, y);
  finally
    Prg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: PerformHintProposal
  Author:    Mark Ericksen
  Date:      23-Sep-2002
  Arguments: Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII; HintProposal: TSynCompletionProposal; var x, y: Integer
  Result:    Boolean
  Purpose:   Simple wrapper for easier use.
-----------------------------------------------------------------------------}
function PerformHintProposal(Editor: TCustomSynEdit; DWScript: TDelphiWebScriptII;
                             HintProposal: TSynCompletionProposal;
                             var x, y: Integer; IncludeImages: Boolean): Boolean;
var
  Prg: TProgram;
begin
  Prg := CompileWithSymbolsAndMap(DWScript, Editor.Lines.Text);
  try
    Result := PerformHintProposal(Editor, Prg, HintProposal, x, y, IncludeImages);
  finally
    Prg.Free;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: ToggleFromDecl2Impl
  Author:    Mark Ericksen
  Date:      18-Oct-2002
  Arguments: Editor: TCustomSynEdit; AProgram: TProgram
  Result:    None
  Purpose:   Toggle position between Implementation and Declaration (procedures/methods)
-----------------------------------------------------------------------------}
procedure ToggleFromDecl2Impl(Editor: TCustomSynEdit; AProgram: TProgram);
var
  NewPos: TScriptPos;
begin
  NewPos := FindToggledFuncDeclImplPos(Editor.CaretX, Editor.CaretY, AProgram);
  if NewPos.Pos > -1 then    // -1 if NullPos was returned
  begin
    { Place cursor on correct location }
    Editor.CaretX := NewPos.Col;
    Editor.CaretY := NewPos.Line;

    { Try to roughly center the new position in the window }
    Editor.TopLine := Editor.CaretY - (Editor.LinesInWindow div 2);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IsInCommentOrString
  Author:    Mark Ericksen
  Date:      22-Mar-2003
  Arguments: Editor: TCustomSynEdit; APoint: TPoint
  Result:    Boolean
  Purpose:   Return if the specified position (APoint) is in a comment or a
             string as defined by the highlighter.
-----------------------------------------------------------------------------}
function IsInCommentOrString(Editor: TCustomSynEdit; APoint: TPoint): Boolean;
var
  dumbstr: string;
  attr: TSynHighlighterAttributes;
begin
  Result := False;
  if not Assigned(Editor) then
    Exit;

  if Editor.GetHighlighterAttriAtRowCol(APoint, dumbstr, attr) then
    Result := (attr = Editor.Highlighter.StringAttribute) or
              (attr = Editor.Highlighter.CommentAttribute) 
  else
    Result := False;
end;

end.
