//
module PrettyPrinter

open AST

let internal nl = System.Environment.NewLine

type Tree = 
    | TreeNode of name: string * subtrees: List<string * Tree>
     with
        member this.Format (indent: string): string =
            match this with
            | TreeNode(descr, subtrees) ->
                let rec formatChildren (children: List<string * Tree>) (indent: string): string =
                    match children with
                    | [] -> ""
                    | [(descr, tree)] -> // Last child
                        let nameStr = if descr <> "" then (descr + ": ") else ""
                        let childIndent = indent + " " + (String.replicate (nameStr.Length + 1) " ")
                        indent + "┗╾" + nameStr + (tree.Format childIndent)
                    | (name, tree) :: rest ->
                        let nameStr = if name <> "" then (name + ": ") else ""
                        let childIndent = indent + "┃" + (String.replicate (nameStr.Length + 1) " ")
                        indent + "┣╾" + nameStr + (tree.Format childIndent)
                            + (formatChildren rest indent)
                descr + nl + (formatChildren subtrees indent)
        
        override this.ToString(): string =
            this.Format ""

let makeTree (name: string) (node: AST.Node<_>) (subtrees: List<string * Tree>): Tree = 
    TreeNode($"%s{name} %s{node.Pos.Format}", subtrees)


let rec internal printSchemeDecl (schemeDecl: AST.Node<AST.SchemeDecl>): Tree =
    // printfn "SchemeDeclNode" 
    let schemeDefNode = printSchemeDef schemeDecl.NodeCategory.schemeDef
    makeTree "SchemeDeclNode" schemeDecl [("", schemeDefNode)]

and internal printSchemeDef (schemeDef: AST.Node<AST.SchemeDef>): Tree =
    // printfn "SchemeDef" 
    let classExprNode = printClassExpr schemeDef.NodeCategory.classExpr
    makeTree "SchemeDefNode" schemeDef [("Name", TreeNode(schemeDef.NodeCategory.name, [])); ("", classExprNode)]

and internal printClassExpr (classExpr: AST.Node<AST.ClassExpr>): Tree =
    makeTree "ClassExprNode" classExpr [("OptDecl", printDecl classExpr.NodeCategory.optDecl)]
            

and internal printDecl (decl: List<AST.Decl>): Tree =
    // printfn "Decl" 
    TreeNode("", List.map (fun (decl) -> 
        match decl with
        | AST.TypeDecl typeDecl -> 
            ("TypeDecl", printTypeDecl typeDecl)
        | AST.ValueDecl valueDecl -> 
            ("ValueDecl", printValueDecl valueDecl)
        | AST.AxiomDecl axiomDecl -> 
            ("AxiomDecl", printAxiomDecl axiomDecl
        ) )decl)  

and internal printTypeDecl (typeDecl: AST.Node<AST.TypeDecl>): Tree =
    // printfn "TypeDecl" 
    let typeDefList = printTypeDef typeDecl.NodeCategory.typeDefList
    makeTree "TypeDeclNode" typeDecl [("TypeDefList", TreeNode("", typeDefList))]

and internal printTypeDef (typeDefList: List<AST.TypeDef>): List<string * Tree> =
    // printfn "TypeDef" 
    List.map (fun typeDef -> 
        match typeDef with 
        | AST.SortDef sortDef -> 
            ("SortDef", printSortDef sortDef)
        | AST.VariantDef variantDef -> 
            ("VariantDef", printVariantDef variantDef)
        | AST.AbbrevDef abbrevDef -> 
            ("AbbrevDef", printAbbrevDef abbrevDef)) typeDefList
   
and internal printValueDecl (valueDecl: AST.Node<AST.ValueDecl>): Tree =
    // printfn "ValueDecl" 
    let valueDefList =  printValueDef valueDecl.NodeCategory.valueDefList
    makeTree "ValueDeclNode" valueDecl [("ValueDefList", TreeNode("", valueDefList))]

and internal printValueDef (valueDefList: List<AST.ValueDef>): List<string * Tree> =
    // printfn "ValueDef" 
    List.map (fun valueDef -> 
        match valueDef with
        | AST.ExplicitValueDef explicitValueDef -> 
            ("ExplicitValueDef", printExplicitValueDef explicitValueDef)
        | AST.ExplicitFunctionDef explicitFunctionDef -> 
            ("ExplicitFunctionDef", printExplicitFunctionDef explicitFunctionDef)
        | AST.ValueSignature valueSignature -> 
            ("ValueSignature", printValueSignature valueSignature)) valueDefList

and internal printAxiomDecl (axiomDecl: AST.Node<AST.AxiomDecl>): Tree =
    // printfn "AxiomDecl" 
    let axiomDefList = List.map (fun axiomDef ->
        ("", printAxiomDef axiomDef)) axiomDecl.NodeCategory.axiomDefList
    makeTree "AxiomDeclNode" axiomDecl [("AxiomDefList", TreeNode("", axiomDefList))]

and internal printAxiomDef (axiomDef: AST.Node<AST.AxiomDef>): Tree =
    // printfn "AxiomDef" 
    let name = axiomDef.NodeCategory.name
    let logicalValueExpr = printValueExpr axiomDef.NodeCategory.logicalValueExpr
    makeTree "AxiomDefNode" axiomDef [("Name", TreeNode(name, [])); ("LogicalValueExpr", logicalValueExpr)]

and internal printSortDef (sortDef: AST.Node<AST.SortDef>): Tree =
    // printfn "SortDef" 
    makeTree "SortDefNode" sortDef [("Name", TreeNode(sortDef.NodeCategory.name, []))]

and internal printVariantDef (variantDef: AST.Node<AST.VariantDef>): Tree =
    // printfn "VariantDef" 
    let choice = List.map (fun (i, choice) -> 
        ($"Choice %d{i+1}", TreeNode(choice, []))) (List.indexed variantDef.NodeCategory.choice)
        
    makeTree "VariantDefNode" variantDef [("Name", TreeNode(variantDef.NodeCategory.name, [])); ("Choice", TreeNode("", choice))]

and internal printAbbrevDef (abbrevDef: AST.Node<AST.AbbrevDef>): Tree =
    // printfn "AbbrevDef" 
    let typeExprTree = printTypeExpr abbrevDef.NodeCategory.typeExpr
    makeTree "AbbrevDefNode" abbrevDef [("Name", TreeNode(abbrevDef.NodeCategory.name, [])); ("TypeExpr", typeExprTree)]

and internal printExplicitValueDef (explicitValueDef: AST.Node<AST.ExplicitValueDef>): Tree =
    // printfn "ExplicitValueDef" 
    let typeExprTree = printTypeExpr explicitValueDef.NodeCategory.typeExpr
    let valueExprTree = printValueExpr explicitValueDef.NodeCategory.valueExpr
    makeTree "ExplicitValueDefNode" explicitValueDef [("Name", TreeNode(explicitValueDef.NodeCategory.name, [])); ("TypeExpr", typeExprTree); ("ValueExpr", valueExprTree)]


and internal printExplicitFunctionDef (explicitFunctionDef: AST.Node<AST.ExplicitFunctionDef>): Tree =
    // printfn "ExplicitFunctionDef" 
    let args = List.map (fun (i,arg) -> 
        ($"arg %d{i + 1}", printTypeExpr arg)) (List.indexed explicitFunctionDef.NodeCategory.args)
    let returnTypeExpr = printTypeExpr explicitFunctionDef.NodeCategory.returnTypeExpr
    let bodyExpr = printBodyExpr explicitFunctionDef.NodeCategory.bodyExpr
    makeTree "ExplicitFunctionDefNode" explicitFunctionDef [("Name", TreeNode(explicitFunctionDef.NodeCategory.name, [])); ("Args", TreeNode("", args)); ("ReturnTypeExpr", returnTypeExpr); ("BodyExpr", bodyExpr)]
   
and internal printValueSignature (valueSignature: AST.Node<AST.ValueSignature>): Tree =
    // printfn "ValueSignature" 
    let typeExprTree = printTypeExpr valueSignature.NodeCategory.typeExpr
    makeTree "ValueSignatureNode" valueSignature [("Name", TreeNode(valueSignature.NodeCategory.name, [])); ("TypeExpr", typeExprTree)]
   
and internal printTypeExpr (typeExpr: AST.Node<AST.TypeExpr>): Tree =
    // printfn "TypeExpr" 
    let typeExprTree = 
        match typeExpr.NodeCategory with
        | AST.TypeLiteral typeLiteral -> TreeNode("TypeLiteral", ["", printTypeLiteral typeLiteral])
        | AST.TypeName typeName -> TreeNode("TypeName", ["", printTypeName typeName])
        | AST.SubtypeExpr subTypeExpr -> TreeNode("SubtypeExpr", ["", printSubtypeExpr subTypeExpr])
        | AST.BracketedTypeExpr bracketedExpr -> TreeNode("BracketedTypeExpr", ["", printBracketedTypeExpr bracketedExpr])
    
    makeTree "TypeExprNode" typeExpr [("TypeExpr", typeExprTree)]

and internal printValueExpr (valueExpr: AST.Node<AST.ValueExpr>): Tree =
    // printfn "ValueExpr" 
    let valueExprTree = 
        match valueExpr.NodeCategory with
        | AST.ValueLiteral valueLiteral -> TreeNode("ValueLiteral", ["", printValueLiteral valueLiteral])
        | AST.ApplicationExpr applicationExpr -> TreeNode("ApplicationExpr", ["", printApplicationExpr applicationExpr])
        | AST.BracketedExpr bracketedExpr -> TreeNode("BracketedExpr", ["", printBracketedExpr bracketedExpr])
        | AST.QuantifiedExpr quantifiedExpr -> TreeNode("QuantifiedExpr", ["", printQuantifiedExpr quantifiedExpr])
        | AST.AxiomInfixExpr axiomInfixExpr -> TreeNode("AxiomInfixExpr", ["", printAxiomInfixExpr axiomInfixExpr])
        | AST.AxiomPrefixExpr axiomPrefixExpr -> TreeNode("AxiomPrefixExpr", ["", printAxiomPrefixExpr axiomPrefixExpr])
        | AST.ValueInfixExpr valueInfixExpr -> TreeNode("ValueInfixExpr", ["", printValueInfixExpr valueInfixExpr])
        | AST.ValuePrefixExpr valuePrefixExpr -> TreeNode("ValuePrefixExpr", ["", printValuePrefixExpr valuePrefixExpr])
        | AST.LetExpr letExpr -> TreeNode("LetExpr", ["", printLetExpr letExpr])
        | AST.IfExpr ifExpr -> TreeNode("IfExpr", ["", printIfExpr ifExpr])
        | AST.ValueName valueName -> TreeNode("ValueName", ["", printValueName valueName])
    
    makeTree "ValueExprNode" valueExpr [("ValueExpr", valueExprTree)]
   
and internal printBodyExpr (bodyExpr: AST.Node<AST.BodyExpr>): Tree =
    // printfn "BodyExpr" 
    let valueExprTree = printValueExpr bodyExpr.NodeCategory.valueExpr
    let args = TreeNode("", List.map (fun (i, arg) -> 
        ($"arg %d{i + 1}", TreeNode(arg, []))) (List.indexed bodyExpr.NodeCategory.args))
    let funcDefOpTree = printFunctionDefOperator(bodyExpr.NodeCategory.functionDefOperator)

    makeTree "BodyExprNode" bodyExpr [("Name", TreeNode(bodyExpr.NodeCategory.name, [])); ("Args", args); ("FunctionDefOperator", funcDefOpTree); ("ValueExpr", valueExprTree)]

and internal printFunctionDefOperator (functionDefOperator: AST.FunctionDefOperator): Tree =
    match functionDefOperator with
    | Is -> TreeNode("Is", [])

and internal printApplicationExpr (applicationExpr: AST.Node<AST.ApplicationExpr>): Tree =
    // printfn "ApplicationExpr" 
    let args = TreeNode("", List.map (fun (i, arg) -> 
        ($"arg %d{i + 1}", printValueExpr arg)) (List.indexed applicationExpr.NodeCategory.args))
    makeTree "ApplicationExprNode" applicationExpr [("Name", TreeNode(applicationExpr.NodeCategory.name, [])); ("Args", args)]

and internal printBracketedExpr (bracketedExpr: AST.Node<AST.BracketedExpr>): Tree =
    // printfn "BracketedExpr" 
    let valueExprTree = printValueExpr bracketedExpr.NodeCategory.valueExpr
    makeTree "BracketedExprNode" bracketedExpr [("ValueExpr", valueExprTree)]

and internal printQuantifiedExpr (quantifiedExpr: AST.Node<AST.QuantifiedExpr>): Tree =
    // printfn "QuantifiedExpr" 

    let quantifierTree = printQuantifiers quantifiedExpr.NodeCategory.quantifier
    let singleTypingList = TreeNode("", List.map (fun (i, singleTyping) -> 
        ($"SingleTyping %d{i + 1}", printSingleTyping singleTyping)) (List.indexed quantifiedExpr.NodeCategory.singleTypingList))
    let valueExprTree = printValueExpr quantifiedExpr.NodeCategory.valueExpr

    makeTree "QuantifiedExprNode" quantifiedExpr [("Quantifier", quantifierTree); ("SingleTypingList", singleTypingList); ("ValueExpr", valueExprTree)]

and internal printQuantifiers (quantifier: AST.Quantifiers): Tree =
    // printfn "Quantifiers" 
    match quantifier with
    | Forall -> TreeNode("Forall", [])
    | Exists -> TreeNode("Exists", [])

and internal printAxiomInfixExpr (axiomInfixExpr: AST.Node<AST.AxiomInfixExpr>): Tree =
    // printfn "AxiomInfixExpr" 
    let leftExpr = printValueExpr axiomInfixExpr.NodeCategory.leftExpr
    let infixConnective = printInfixConnective axiomInfixExpr.NodeCategory.infixConnective
    let rightExpr = printValueExpr axiomInfixExpr.NodeCategory.rightExpr
    
    makeTree "AxiomInfixExprNode" axiomInfixExpr [("LeftExpr", leftExpr); ("InfixConnective", infixConnective); ("RightExpr", rightExpr)]

and internal printInfixConnective (infixConnective: AST.InfixConnective): Tree =
    match infixConnective with
    | And -> TreeNode("And", [])
    | Or -> TreeNode("Or", [])
    | Implies -> TreeNode("Implies", [])

and internal printValueInfixExpr (valueInfixExpr: AST.Node<AST.ValueInfixExpr>): Tree =
    // printfn "ValueInfixExpr" 
    let leftExpr = printValueExpr valueInfixExpr.NodeCategory.leftExpr
    let infixOperator = printInfixOperator valueInfixExpr.NodeCategory.infixOperator
    let rightExpr = printValueExpr valueInfixExpr.NodeCategory.rightExpr

    makeTree "ValueInfixExprNode" valueInfixExpr [("LeftExpr", leftExpr); ("InfixOperator", infixOperator); ("RightExpr", rightExpr)]

and internal printInfixOperator (infixOperator: AST.InfixOperator): Tree =
    match infixOperator with
    | Add -> TreeNode("Add", [])
    | Subtract -> TreeNode("Subtract", [])
    | Multiply -> TreeNode("Multiply", [])
    | Divide -> TreeNode("Divide", [])
    | Equals -> TreeNode("Equals", [])
    | NotEquals -> TreeNode("NNotEqualseq", [])
    | LessThan -> TreeNode("LessThan", [])
    | LessOrEqual -> TreeNode("LessOrEqual", [])
    | GreaterThan -> TreeNode("GreaterThan", [])
    | GreaterOrEqual -> TreeNode("GreaterOrEqual", [])

and internal printAxiomPrefixExpr (axiomPrefixExpr: AST.Node<AST.AxiomPrefixExpr>): Tree =
    // printfn "AxiomPrefixExpr" 
    let valueExpr = printValueExpr axiomPrefixExpr.NodeCategory.valueExpr
    let prefixConnective = printPrefixConnective axiomPrefixExpr.NodeCategory.prefixConnective

    makeTree "AxiomPrefixExprNode" axiomPrefixExpr [("ValueExpr", valueExpr); ("PrefixConnective", prefixConnective)]

and internal printPrefixConnective (prefixConnective: AST.PrefixConnective): Tree =
    match prefixConnective with
    | Not -> TreeNode("Not", [])

and internal printValuePrefixExpr (valuePrefixExpr: AST.Node<AST.ValuePrefixExpr>): Tree =
    // printfn "ValuePrefixExpr" 
    let valueExpr = printValueExpr valuePrefixExpr.NodeCategory.valueExpr
    let prefixOperator = printPrefixOperator valuePrefixExpr.NodeCategory.prefixOperator

    makeTree "ValuePrefixExprNode" valuePrefixExpr [("ValueExpr", valueExpr); ("PrefixOperator", prefixOperator)]

and internal printPrefixOperator (prefixOperator: AST.PrefixOperator): Tree =
    match prefixOperator with
    | Abs -> TreeNode("Abs", [])

and internal printLetExpr (letExpr: AST.Node<AST.LetExpr>): Tree =
    // printfn "LetExpr" 
    let initExpr = printValueExpr letExpr.NodeCategory.initExpr
    let scopeExpr = printValueExpr letExpr.NodeCategory.scopeExpr

    makeTree "LetExprNode" letExpr [("Name", TreeNode(letExpr.NodeCategory.name, [])); ("InitExpr", initExpr); ("ScopeExpr", scopeExpr)]

and internal printIfExpr (ifExpr: AST.Node<AST.IfExpr>): Tree =
    // printfn "IfExpr" 
    let condExpr = printValueExpr ifExpr.NodeCategory.condExpr
    let thenExpr = printValueExpr ifExpr.NodeCategory.thenExpr
    let elseExpr = printValueExpr ifExpr.NodeCategory.elseExpr

    makeTree "IfExprNode" ifExpr [("CondExpr", condExpr); ("ThenExpr", thenExpr); ("ElseExpr", elseExpr)]
   
and internal printTypeLiteral (typeLiteral: AST.Node<AST.TypeLiteral>): Tree  =
    // printfn "TypeLiteral" 
    let literalType = 
        match typeLiteral.NodeCategory with
        | Bool -> "Bool"
        | int -> "Int"
    
    makeTree "TypeLiteralNode" typeLiteral [("", TreeNode(literalType, []))]


and internal printTypeName (typeName: AST.Node<AST.TypeName>): Tree =
    // printfn "TypeName" 
    
    makeTree "TypeNameNode" typeName [("", TreeNode(typeName.NodeCategory.name, []))]

and internal printSubtypeExpr (subTypeExpr: AST.Node<AST.SubtypeExpr>): Tree =
    // printfn "SubtypeExpr" 
    let singleTyping = printSingleTyping subTypeExpr.NodeCategory.singleTyping
    let valueExpr = printValueExpr subTypeExpr.NodeCategory.valueExpr

    makeTree "SubtypeExprNode" subTypeExpr [("SingleTyping", singleTyping); ("ValueExpr", valueExpr)]
    
and internal printSingleTyping (singleTyping: AST.Node<AST.SingleTyping>): Tree =
    // printfn "SingleTyping" 
    let name = TreeNode(singleTyping.NodeCategory.name, [])
    let typeExpr = printTypeExpr singleTyping.NodeCategory.typeExpr

    makeTree "SingleTypingNode" singleTyping [("Name", name); ("TypeExpr", typeExpr)]
    
and internal printBracketedTypeExpr (bracketedTypeExpr: AST.Node<AST.BracketedTypeExpr>): Tree =
    // printfn "BracketedTypeExpr" 
    let typeExpr = printTypeExpr bracketedTypeExpr.NodeCategory.typeExpr

    makeTree "BracketedTypeExprNode" bracketedTypeExpr [("TypeExpr", typeExpr)]

and internal printValueLiteral (valueLiteral: AST.Node<AST.ValueLiteral>): Tree =
    // // printfn "ValueLiteral" 
    let literalValue = 
        match valueLiteral.NodeCategory with
        | AST.Bool(boolValue) -> boolValue.ToString()
        | AST.Int(intValue) -> intValue.ToString()
    
    makeTree "ValueLiteralNode" valueLiteral [("", TreeNode(literalValue, []))]
            

and internal printValueName (valueName: string): Tree =
    // // printfn "ValueName" 
    TreeNode(valueName, [])

let prettPrint (ast: AST.Node<AST.SchemeDecl>): string =
    (printSchemeDecl ast).ToString()