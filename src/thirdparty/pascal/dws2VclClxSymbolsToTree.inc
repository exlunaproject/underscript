{ Images used for Tree node graphics are semi-defined in dws2IDEUtils.pas. }

// Format the tree to coincide with the layout of a Tdws2Unit
procedure SymbolsToUnitTree(AProgram: TProgram; Tree: TTreeView);
// Format the tree to work like the default Delphi Code Explorer
procedure SymbolsToDefaultDelphiTree(AProgram: TProgram; Tree: TTreeView);
//
function CreateNewNode(ParentNode: TTreeNode; Text: string; ImgIndex: Integer; Tree: TTreeView; DataPtr: Pointer): TTreeNode;
function AddSymbolToTree(ParentNode: TTreeNode; Tree: TTreeView; Symbol: TSymbol;
                         SymPos: TSymbolPosition): TTreeNode;

// Remove root nodes that have no 'Data' assigned to them.
procedure PruneEmptyNodes(Tree: TTreeView);

// Return if the symbol is declared in a function (may wish to supress)
function IsDeclaredInFunction(ContextMap: TContextMap; SymbolPos: TSymbolPosition): Boolean;

implementation

{-----------------------------------------------------------------------------
  Procedure: CreateFolderNode
  Author:    Mark Ericksen
  Date:      19-Oct-2002
  Arguments: Text: string; ImgIndex: Integer; Tree: TTreeView
  Result:    TTreeNode
  Purpose:   Create a root node on the tree for containing a category of items.
-----------------------------------------------------------------------------}
function CreateNewNode(ParentNode: TTreeNode; Text: string; ImgIndex: Integer; Tree: TTreeView; DataPtr: Pointer): TTreeNode;
begin
  Result := Tree.Items.AddChildObject(ParentNode, Text, DataPtr);
  Result.ImageIndex    := ImgIndex;
  Result.SelectedIndex := ImgIndex;
end;

{-----------------------------------------------------------------------------
  Procedure: AddSymbolToTree
  Author:    Mark Ericksen
  Date:      19-Oct-2002
  Arguments: ParentNode: TTreeNode; Symbol: TSymbol; SymPos: TSymbolPosition
  Result:    TTreeNode
  Purpose:   Add the symbol to the tree (specified node). Link the Symbol
             position to the node for navigation.
-----------------------------------------------------------------------------}
function AddSymbolToTree(ParentNode: TTreeNode; Tree: TTreeView; Symbol: TSymbol;
    SymPos: TSymbolPosition): TTreeNode;
var
  idx: Integer;
  CanAdd: Boolean;
begin
  Result := nil;
  CanAdd := False;
  if (not Assigned(ParentNode)) or (not Assigned(SymPos)) then
    Exit;
  idx := imgFolder;
  { Classes and Forwards }
  if Symbol is TClassSymbol then
  begin
    idx    := imgClass;
    CanAdd := True;
  end
  { Procedures }
  else if Symbol is TFuncSymbol then
  begin
    case TFuncSymbol(Symbol).Kind of
    fkFunction    : idx := imgFunc;
    fkProcedure   : idx := imgProc;
    fkConstructor : idx := imgConstruct;
    fkDestructor  : idx := imgDestruct;
    end;
    CanAdd := True;
  end
  { Types }
  else if Symbol is TTypeSymbol then
  begin
    idx := imgType;
    CanAdd := True;
  end
  { Properties (Also TValueSymbol so check for it first) }
  else if Symbol is TPropertySymbol then
  begin
    idx    := imgProp;
    CanAdd := True;
  end
  { Variables and Constants (not function parameters) }
  else if (Symbol is TValueSymbol) and (not (Symbol is TParamSymbol)) then
  begin
    if Symbol is TConstSymbol then
      idx := imgConst
    else
      idx := imgVar;
    CanAdd := True;
  end
  { Unit symbols }
  else if Symbol is TUnitSymbol then
  begin
    idx    := imgUnit;
    CanAdd := True;
  end;
  //
  { Add symbol under appropriate parent with data attached }
  if CanAdd then
    Result := CreateNewNode(ParentNode, Symbol.Name, idx, Tree, SymPos);
end;

{-----------------------------------------------------------------------------
  Procedure: SymbolsToUnitTree
  Author:    Mark Ericksen
  Date:      04-Oct-2002
  Arguments: AProgram: TProgram; Tree: TTreeView
  Result:    None
  Purpose:   Turn a programs symbols into a browsable TreeView list suitable for
             representing a Tdws2Unit.
-----------------------------------------------------------------------------}
procedure SymbolsToUnitTree(AProgram: TProgram; Tree: TTreeView);
var
  { Nodes that hold sections of items }
  ForwardNode, ConstNode, EnumNode, ArrayNode, RecNode, ClassNode, ProcNode,
  SynonymNode, VarNode: TTreeNode;

  i, x: Integer;
  ForPos,                        // forward
  DeclPos: TSymbolPosition;      // declaration
  PosList,                       // list of symbol positions
  SubSymList: TSymbolPositionList; // sub-Symbol position list (members)
  NewNode: TTreeNode;
begin
  { Requires both the SymbolList and ContextMap compiler directives for the script. }

  { NOTE: Had a Tree.Items.BeginUpdate call with a finally to EndUpdate but it
          never worked properly in Delphi5. Left it out for now. }
  Tree.Items.Clear;
  { Create holders up front. Delete empty ones when done. }
  ForwardNode  := CreateNewNode(nil, 'Forwards', imgFolder, Tree, nil);
  ConstNode    := CreateNewNode(nil, 'Constants', imgFolder, Tree, nil);
  EnumNode     := CreateNewNode(nil, 'Enumerations', imgFolder, Tree, nil);
  ArrayNode    := CreateNewNode(nil, 'Arrays', imgFolder, Tree, nil);
  RecNode      := CreateNewNode(nil, 'Records', imgFolder, Tree, nil);
  ClassNode    := CreateNewNode(nil, 'Classes', imgFolder, Tree, nil);
  ProcNode     := CreateNewNode(nil, 'Procedures', imgFolder, Tree, nil);
  SynonymNode  := CreateNewNode(nil, 'Synonyms', imgFolder, Tree, nil);
  VarNode      := CreateNewNode(nil, 'Variables', imgFolder, Tree, nil);

  { Cycle all symbols. Add select types to appropriate categories }
  for i := 0 to AProgram.SymbolDictionary.Count - 1 do
  begin
    PosList := AProgram.SymbolDictionary[i];
    DeclPos := PosList.FindUsage(suDeclaration);     // find where declared
    ForPos  := PosList.FindUsage(suForward);         // find where forwarded (may not be)
    { If found declaration (not in a unit or another script) }
    if Assigned(DeclPos) then
    begin
      // Only add to tree if the symbol is declared outside of a function or
      // if the symbol is a function symbol (do show inline functions)
      if IsDeclaredInFunction(AProgram.ContextMap, DeclPos) then
        if not (PosList.Symbol is TFuncSymbol) then
          CONTINUE;   // Don't add anything. Loop again.

      { Classes & Forwards - Forwards will be ignored at this point }
      if PosList.Symbol is TClassSymbol then
      begin
        { Add Class forward if applicable }
        if Assigned(ForPos) then
          AddSymbolToTree(ForwardNode, Tree, PosList.Symbol, ForPos);
        { Add Class itself }
        NewNode := AddSymbolToTree(ClassNode, Tree, PosList.Symbol, DeclPos);
        { Add Class members }
        for x := 0 to TClassSymbol(PosList.Symbol).Members.Count - 1 do
        begin
          SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TClassSymbol(PosList.Symbol).Members[x]);
          if Assigned(SubSymList) then
            AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
        end;
      end
      { Constants - not element of enumeration }
      else if (PosList.Symbol is TConstSymbol) and (not (PosList.Symbol is TElementSymbol)) then
        AddSymbolToTree(ConstNode, Tree, PosList.Symbol, DeclPos)
      { Enumerations - Not standard Delphi to include. }
      else if PosList.Symbol is TEnumerationSymbol then
      begin
        NewNode := AddSymbolToTree(EnumNode, Tree, PosList.Symbol, DeclPos);
        for x := 0 to TEnumerationSymbol(PosList.Symbol).Elements.Count - 1 do
        begin
          SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TEnumerationSymbol(PosList.Symbol).Elements[x]);
          if Assigned(SubSymList) then
            AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
        end;
      end
      { Arrays }
      else if PosList.Symbol is TArraySymbol then
        AddSymbolToTree(ArrayNode, Tree, PosList.Symbol, DeclPos)
      { Records }
      else if PosList.Symbol is TRecordSymbol then
      begin
        NewNode := AddSymbolToTree(RecNode, Tree, PosList.Symbol, DeclPos);
        for x := 0 to TRecordSymbol(PosList.Symbol).Members.Count - 1 do
        begin
          SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TRecordSymbol(PosList.Symbol).Members[x]);
          if Assigned(SubSymList) then
            AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
        end;
      end
      { Procedure - Don't catch methods here. }
      else if (PosList.Symbol is TFuncSymbol) and (not (PosList.Symbol is TMethodSymbol)) then
      begin
        { if Function is forwarded, use that position, if not then use declaration }
        if Assigned(ForPos) then
          AddSymbolToTree(ProcNode, Tree, PosList.Symbol, ForPos)
        else
          AddSymbolToTree(ProcNode, Tree, PosList.Symbol, DeclPos);
      end
      { Synonyms (Aliases) }
      else if PosList.Symbol is TAliasSymbol then
        AddSymbolToTree(SynonymNode, Tree, PosList.Symbol, DeclPos)
      { Variables }
      else if PosList.Symbol is TDataSymbol then
        AddSymbolToTree(VarNode, Tree, PosList.Symbol, DeclPos);
    end;
  end;

  { Remove empty sections }
  PruneEmptyNodes(Tree);
end;

{ TODO -oMark : Reselect the previously selected node based on the symbol name and class type? Can't use the pointer because it is invalid after a recompile. }
procedure SymbolsToDefaultDelphiTree(AProgram: TProgram; Tree: TTreeView);
var
  { Nodes that hold sections of items }
  ClassNode, TypesNode, ProcNode, VarConstNode: TTreeNode;

  i, x: Integer;
  ForPos,                        // forward
  DeclPos: TSymbolPosition;      // declaration
  PosList,                       // list of symbol positions
  SubSymList: TSymbolPositionList; // sub-Symbol position list (members)
  NewNode: TTreeNode;
begin
  { Requires both the SymbolList and ContextMap compiler directives for the script. }

  { NOTE: Had a Tree.Items.BeginUpdate call with a finally to EndUpdate but it
          never worked properly in Delphi5. Left it out for now. }
  Tree.Items.Clear;
  { Create holders up front. Delete empty ones when done. }
  ClassNode    := CreateNewNode(nil, 'Classes', imgFolder, Tree, nil);
  ProcNode     := CreateNewNode(nil, 'Procedures', imgFolder, Tree, nil);
  TypesNode    := CreateNewNode(nil, 'Types', imgFolder, Tree, nil);
  VarConstNode := CreateNewNode(nil, 'Variables/Constants', imgFolder, Tree, nil);

  { Cycle all symbols. Add select types to appropriate categories }
  for i := 0 to AProgram.SymbolDictionary.Count - 1 do
  begin
    PosList := AProgram.SymbolDictionary[i];
    DeclPos := PosList.FindUsage(suDeclaration);     // find where declared
    { If found declaration (not in a unit or another script) }
    if Assigned(DeclPos) then
    begin
      // Only add to tree if the symbol is declared outside of a function or
      // if the symbol is a function symbol (do show inline functions)
      if IsDeclaredInFunction(AProgram.ContextMap, DeclPos) then
        if not (PosList.Symbol is TFuncSymbol) then
          CONTINUE;   // Don't add anything. Loop again.

      { Classes & Forwards - Forwards will be ignored at this point }
      if PosList.Symbol is TClassSymbol then
      begin
        { Add Class itself }
        NewNode := AddSymbolToTree(ClassNode, Tree, PosList.Symbol, DeclPos);
        { Add Class members }
        for x := 0 to TClassSymbol(PosList.Symbol).Members.Count - 1 do
        begin
          SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TClassSymbol(PosList.Symbol).Members[x]);
          if Assigned(SubSymList) then
            AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
        end;
      end
      { Variables/Constants }
      { Constants and Variables (not Enumerations)}
      else if ((PosList.Symbol is TConstSymbol) or (PosList.Symbol is TDataSymbol)) and
        (not (PosList.Symbol is TElementSymbol)) then
        AddSymbolToTree(VarConstNode, Tree, PosList.Symbol, DeclPos)
      { Enumerations - Handle first, elements are constants and would otherwise show up there. }
      else if PosList.Symbol is TEnumerationSymbol then
      begin
        NewNode := AddSymbolToTree(VarConstNode, Tree, PosList.Symbol, DeclPos);
        for x := 0 to TEnumerationSymbol(PosList.Symbol).Elements.Count - 1 do
        begin
          SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TEnumerationSymbol(PosList.Symbol).Elements[x]);
          if Assigned(SubSymList) then
            AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
        end;
      end
      { Procedure - Don't catch methods here. [Before "types" because it is a type }
      else if PosList.Symbol is TFuncSymbol then
      begin
        // suppress methods because they are not added here. They are added at the
        // class level. Suppress from getting picked up by the "TTypeSymbol" category.
        if not (PosList.Symbol is TMethodSymbol) then
        begin
          { if Function is forwarded, use that position, if not then use declaration }
          ForPos  := PosList.FindUsage(suForward);         // find where forwarded (may not be)
          if Assigned(ForPos) then
            AddSymbolToTree(ProcNode, Tree, PosList.Symbol, ForPos)
          else
            AddSymbolToTree(ProcNode, Tree, PosList.Symbol, DeclPos);
        end;
      end
      { Types - classes are already handled }
      else if PosList.Symbol is TTypeSymbol then
      begin
        // add type
        NewNode := AddSymbolToTree(TypesNode, Tree, PosList.Symbol, DeclPos);
        { Check for types that have members }
        // (Records - Add members) 
        if PosList.Symbol is TRecordSymbol then
        begin
          for x := 0 to TRecordSymbol(PosList.Symbol).Members.Count - 1 do
          begin
            SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TRecordSymbol(PosList.Symbol).Members[x]);
            if Assigned(SubSymList) then
              AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
          end;
        end
        // (Enumerations - Add elements)
        else if PosList.Symbol is TEnumerationSymbol then
        begin
          for x := 0 to TEnumerationSymbol(PosList.Symbol).Elements.Count - 1 do
          begin
            SubSymList := AProgram.SymbolDictionary.FindSymbolPosList(TEnumerationSymbol(PosList.Symbol).Elements[x]);
            if Assigned(SubSymList) then
              AddSymbolToTree(NewNode, Tree, SubSymList.Symbol, SubSymList.FindUsage(suDeclaration));
          end;
        end;
      end;
    end;
  end;

  { Remove empty sections }
  PruneEmptyNodes(Tree);
end;

{-----------------------------------------------------------------------------
  Procedure: PruneEmptyNodes
  Author:    Mark Ericksen
  Date:      21-Oct-2002
  Arguments: Tree: TTreeView
  Result:    None
  Purpose:   Remove nodes that are 'empty'. Root nodes, no children, no data.
-----------------------------------------------------------------------------}
procedure PruneEmptyNodes(Tree: TTreeView);
var
  i: Integer;
begin
  { Remove empty sections }
  i := 0;
  while i <= Tree.Items.Count - 1 do
  begin
    // if (is a parent node) (with no children) and (has no Data), delete it.
    if (Tree.Items[i].Parent = nil) and (Tree.Items[i].Count = 0) and (Tree.Items[i].Data=nil)then
      Tree.Items.Delete(Tree.Items[i])     // if removed, don't up counter, need to check replacement index
    else
      Inc(i);
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: IsDeclaredInFunction
  Author:    Mark Ericksen
  Date:      25-May-2003
  Arguments: ContextMap: TContextMap; SymbolPos: TSymbolPosition
  Result:    Boolean
  Purpose:   Helper function to return if the symbol declaration position is
             within the context of a function or method. The caller may wish
             to suppress the symbol if it is.
-----------------------------------------------------------------------------}
function IsDeclaredInFunction(ContextMap: TContextMap; SymbolPos: TSymbolPosition): Boolean;
var
  declContext: TContext;
begin
  Result := False;
  declContext := ContextMap.FindContext(SymbolPos.ScriptPos);
  if Assigned(declContext) then
    Result := declContext.HasParentSymbolOfClass(TFuncSymbol, True);
end;
