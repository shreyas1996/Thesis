//
module PrettyPrinter

open AST
open Type

let internal printPosition (pos: AST.Position) (indent: int) =
    printfn "%sPosition: %s" (String.replicate indent "  ") pos.Format

let rec internal printNode (node: AST.Node<_>) (indent: int) =
    printPosition node.Pos (indent)
    printNodeCategory node.NodeCategory (indent + 1)

and internal printNodeCategory (nodeCategory: obj) (indent: int): Type.NodeTypes =
    match nodeCategory with
        | :? AST.SchemeDecl as schemeDecl -> SchemeDeclResult (printSchemeDecl schemeDecl indent)
        | :? AST.SchemeDef as schemeDef -> SchemeDefResult (printSchemeDef schemeDef indent)
        | :? AST.ClassExpr as classExpr -> ClassExprResult (printClassExpr classExpr indent)
        | :? AST.Decl as decl -> DeclResult (printDecl decl indent)
        | :? AST.TypeDecl as typeDecl -> TypeDeclResult (printTypeDecl typeDecl indent)
        | :? AST.ValueDecl as valueDecl -> ValueDeclResult (printValueDecl valueDecl indent)
        | :? AST.AxiomDecl as axiomDecl -> AxiomDeclResult (printAxiomDecl axiomDecl indent)
        | :? AST.AxiomDef as axiomDef -> AxiomDefResult (printAxiomDef axiomDef indent)
        | :? AST.SortDef as sortDef -> SortDefResult (printSortDef sortDef indent)
        | :? AST.VariantDef as variantDef -> VariantDefResult (printVariantDef variantDef indent)
        | :? AST.AbbrevDef as abbrevDef -> AbbrevDefResult (printAbbrevDef abbrevDef indent)
        | :? AST.ExplicitValueDef as explicitValueDef -> ExplicitValueDefResult (printExplicitValueDef explicitValueDef indent)
        | :? AST.ExplicitFunctionDef as explicitFunctionDef -> ExplicitFunctionDefResult (printExplicitFunctionDef explicitFunctionDef indent)
        | :? AST.ValueSignature as valueSignature -> ValueSignatureResult (printValueSignature valueSignature indent)
        | :? AST.TypeExpr as typeExpr -> TypeExprResult (printTypeExpr typeExpr indent)
        | :? AST.ValueExpr as valueExpr -> ValueExprResult (printValueExpr valueExpr indent)
        | :? AST.BodyExpr as bodyExpr -> BodyExprResult (printBodyExpr bodyExpr indent)
        | :? AST.ApplicationExpr as applicationExpr -> ApplicationExprResult (printApplicationExpr applicationExpr indent)
        | :? AST.BracketedExpr as bracketedExpr -> BracketedExprResult (printBracketedExpr bracketedExpr indent)
        | :? AST.QuantifiedExpr as quantifiedExpr -> QuantifiedExprResult (printQuantifiedExpr quantifiedExpr indent)
        | :? AST.AxiomInfixExpr as axiomInfixExpr -> AxiomInfixExprResult (printAxiomInfixExpr axiomInfixExpr indent)
        | :? AST.AxiomPrefixExpr as axiomPrefixExpr -> AxiomPrefixExprResult (printAxiomPrefixExpr axiomPrefixExpr indent)
        | :? AST.ValueInfixExpr as valueInfixExpr -> ValueInfixExprResult (printValueInfixExpr valueInfixExpr indent)
        | :? AST.ValuePrefixExpr as valuePrefixExpr -> ValuePrefixExprResult (printValuePrefixExpr valuePrefixExpr indent)
        | :? AST.LetExpr as letExpr -> LetExprResult (printLetExpr letExpr indent)
        | :? AST.IfExpr as ifExpr -> IfExprResult (printIfExpr ifExpr indent)
        | :? AST.TypeLiteral as typeLiteral -> TypeLiteralResult (printTypeLiteral typeLiteral indent)
        | :? AST.TypeName as typeName -> TypeNameResult (printTypeName typeName indent)
        | :? AST.SubtypeExpr as subTypeExpr -> SubtypeExprResult (printSubtypeExpr subTypeExpr indent)
        | :? AST.SingleTyping as singleTyping -> SingleTypingResult (printSingleTyping singleTyping indent)
        | :? AST.BracketedTypeExpr as bracketedTypeExpr -> BracketedTypeExprResult (printBracketedTypeExpr bracketedTypeExpr indent)
        | :? AST.ValueLiteral as valueLiteral -> ValueLiteralResult (printValueLiteral valueLiteral indent)


and internal printSchemeDecl (schemeDecl: AST.SchemeDecl) (indent: int): Type.SchemeDecl =
    printfn "%sSchemeDeclNode" (String.replicate indent "  ")
    match printNode schemeDecl.schemeDef (indent + 1 ) with
        | SchemeDefResult schemeDef -> { schemeDef = schemeDef }
        | _ -> failwith "Unexpected node type"

and internal printSchemeDef (schemeDef: AST.SchemeDef) (indent: int): Type.SchemeDef =
    printfn "%sSchemeDef" (String.replicate indent "  ")
    match printNode schemeDef.classExpr (indent + 1) with
        | ClassExprResult classExpr -> { name = schemeDef.name; classExpr = classExpr }
        | _ -> failwith "Unexpected node type"

and internal printClassExpr (classExpr: AST.ClassExpr) (indent: int): Type.ClassExpr =
    printfn "%sClassExpr" (String.replicate indent "  ")
    match classExpr.optDecl with
        | None -> { optDecl = None }
        | Some x -> 
            match printNode x (indent + 1) with
            | DeclResult decl -> { optDecl = Some decl }
            | _ -> failwith "Unexpected node type"

and internal printDecl (decl: AST.Decl) (indent: int): Type.Decl =
    printfn "%sDecl" (String.replicate indent "  ")
    let newTypeDecl = match decl.TypeDecl with
        | None -> None
        | Some x -> 
            match printNode x (indent + 1) with
            | TypeDeclResult typeDecl -> Some typeDecl
            | _ -> failwith "Unexpected node type"
    let newValueDecl = match decl.ValueDecl with
        | None -> None
        | Some x -> 
            match printNode x (indent + 1) with
            | ValueDeclResult valueDecl -> Some valueDecl
            | _ -> failwith "Unexpected node type"
    let newAxiomDecl = match decl.AxiomDecl with
        | None -> None
        | Some x -> 
            match printNode x (indent + 1) with
            | AxiomDeclResult axiomDecl -> Some axiomDecl
            | _ -> failwith "Unexpected node type"
    { TypeDecl = newTypeDecl; ValueDecl = newValueDecl; AxiomDecl = newAxiomDecl }

and internal printTypeDecl (typeDecl: AST.TypeDecl) (indent: int): Type.TypeDecl =
    printfn "%sTypeDecl" (String.replicate indent "  ")
    let typeDefList = List.map (fun node -> printTypeDef node (indent + 1)) typeDecl.typeDefList
    { typeDefList = typeDefList }

and internal printTypeDef (typeDef: AST.TypeDef) (indent: int): Type.TypeDef =
    printfn "%sTypeDef" (String.replicate indent "  ")
    match typeDef with 
        | AST.SortDef sortDef -> 
            match printNode sortDef (indent + 1) with
                | SortDefResult sortDef -> SortDef sortDef
                | _ -> failwith "Unexpected node type"
        | AST.VariantDef variantDef -> 
            match printNode variantDef (indent + 1) with
                | VariantDefResult variantDef -> VariantDef variantDef
                | _ -> failwith "Unexpected node type"
        | AST.AbbrevDef abbrevDef -> 
            match printNode abbrevDef (indent + 1) with
                | AbbrevDefResult abbrevDef -> AbbrevDef abbrevDef
                | _ -> failwith "Unexpected node type"

and internal printValueDecl (valueDecl: AST.ValueDecl) (indent: int): Type.ValueDecl =
    printfn "%sValueDecl" (String.replicate indent "  ")
    let valueDefList = List.map (fun valueDef -> printValueDef valueDef (indent + 1)) valueDecl.valueDefList
    { valueDefList = valueDefList }

and internal printValueDef (valueDef: AST.ValueDef) (indent: int): Type.ValueDef =
    printfn "%sValueDef" (String.replicate indent "  ")
    match valueDef with
        | AST.ExplicitValueDef explicitValueDef -> 
            match printNode explicitValueDef (indent + 1) with
            | ExplicitValueDefResult explicitValueDef -> ExplicitValueDef explicitValueDef
            | _ -> failwith "Unexpected node type"
        | AST.ExplicitFunctionDef explicitFunctionDef -> 
            match printNode explicitFunctionDef (indent + 1) with
            | ExplicitFunctionDefResult explicitFunctionDef -> ExplicitFunctionDef explicitFunctionDef
            | _ -> failwith "Unexpected node type"
        | AST.ValueSignature valueSignature -> 
            match printNode valueSignature (indent + 1) with
            | ValueSignatureResult valueSignature -> ValueSignature valueSignature
            | _ -> failwith "Unexpected node type"

and internal printAxiomDecl (axiomDecl: AST.AxiomDecl) (indent: int): Type.AxiomDecl =
    printfn "%sAxiomDecl" (String.replicate indent "  ")
    let axiomDefList = List.map (fun axiomDef -> 
        match printNode axiomDef (indent + 1) with
        | AxiomDefResult axiomDef -> axiomDef
        | _ -> failwith "Unexpected node type") axiomDecl.axiomDefList
    { axiomDefList = axiomDefList }

and internal printAxiomDef (axiomDef: AST.AxiomDef) (indent: int): Type.AxiomDef =
    printfn "%sAxiomDef" (String.replicate indent "  ")
    match axiomDef.name with
        | None -> { name = None; logicalValueExpr = 
            match printNode axiomDef.logicalValueExpr (indent + 1) with
            | ValueExprResult logicalValueExpr -> logicalValueExpr
            | _ -> failwith "Unexpected node type" }
        | Some _ -> { name = axiomDef.name; logicalValueExpr = 
            match printNode axiomDef.logicalValueExpr (indent + 1) with
            | ValueExprResult logicalValueExpr -> logicalValueExpr
            | _ -> failwith "Unexpected node type" }

and internal printSortDef (sortDef: AST.SortDef) (indent: int): Type.SortDef =
    printfn "%sSortDef" (String.replicate indent "  ")
    { name = sortDef.name }

and internal printVariantDef (variantDef: AST.VariantDef) (indent: int): Type.VariantDef =
    printfn "%sVariantDef" (String.replicate indent "  ")
    { name = variantDef.name; choice = variantDef.choice }

and internal printAbbrevDef (abbrevDef: AST.AbbrevDef) (indent: int): Type.AbbrevDef =
    printfn "%sAbbrevDef" (String.replicate indent "  ")
    { name = abbrevDef.name; typeExpr = 
        match printNode abbrevDef.typeExpr (indent + 1) with
        | TypeExprResult typeExpr -> typeExpr
        | _ -> failwith "Unexpected node type" }

and internal printExplicitValueDef (explicitValueDef: AST.ExplicitValueDef) (indent: int): Type.ExplicitValueDef =
    printfn "%sExplicitValueDef" (String.replicate indent "  ")
    { 
        name = explicitValueDef.name; 
        typeExpr = 
            match printNode explicitValueDef.typeExpr (indent + 1) with
                | TypeExprResult typeExpr -> typeExpr
                | _ -> failwith "Unexpected node type"; 
        valueExpr = 
            match printNode explicitValueDef.valueExpr (indent + 1) with
                | ValueExprResult valueExpr -> valueExpr
                | _ -> failwith "Unexpected node type" 
    }

and internal printExplicitFunctionDef (explicitFunctionDef: AST.ExplicitFunctionDef) (indent: int): Type.ExplicitFunctionDef =
    printfn "%sExplicitFunctionDef" (String.replicate indent "  ")
    { 
        name = explicitFunctionDef.name; 
        args = List.map (fun arg -> 
            match printNode arg (indent + 1) with
                | TypeExprResult arg -> arg
                | _ -> failwith "Unexpected node type") explicitFunctionDef.args; 
        functionDeclOperator = explicitFunctionDef.functionDeclOperator; 
        returnTypeExpr = 
            match printNode explicitFunctionDef.returnTypeExpr (indent + 1) with
                | TypeExprResult returnTypeExpr -> returnTypeExpr
                | _ -> failwith "Unexpected node type";
        bodyExpr = 
            match printNode explicitFunctionDef.bodyExpr (indent + 1) with
                | BodyExprResult bodyExpr -> bodyExpr
                | _ -> failwith "Unexpected node type" }

and internal printValueSignature (valueSignature: AST.ValueSignature) (indent: int): Type.ValueSignature =
    printfn "%sValueSignature" (String.replicate indent "  ")
    { name = valueSignature.name; typeExpr = 
        match printNode valueSignature.typeExpr (indent + 1) with
        | TypeExprResult typeExpr -> typeExpr
        | _ -> failwith "Unexpected node type" }

and internal printTypeExpr (typeExpr: AST.TypeExpr) (indent: int): Type.TypeExpr =
    printfn "%sTypeExpr" (String.replicate indent "  ")
    match typeExpr with
        | AST.TypeLiteral typeLiteral -> 
            match printNode typeLiteral (indent + 1) with
            | TypeLiteralResult result -> Type.TypeLiteral result
            | _ -> failwith "Unexpected node type"
        | AST.TypeName typeName -> 
            match printNode typeName (indent + 1) with
            | TypeNameResult result -> Type.TypeName result
            | _ -> failwith "Unexpected node type"
        | AST.SubtypeExpr subTypeExpr -> 
            match printNode subTypeExpr (indent + 1) with
            | SubtypeExprResult result -> Type.SubtypeExpr result
            | _ -> failwith "Unexpected node type"
        | AST.BracketedTypeExpr bracketedExpr -> 
            match printNode bracketedExpr (indent + 1) with
            | BracketedTypeExprResult result -> Type.BracketedTypeExpr result
            | _ -> failwith "Unexpected node type"

and internal printValueExpr (valueExpr: AST.ValueExpr) (indent: int): Type.ValueExpr =
    printfn "%sValueExpr" (String.replicate indent "  ")
    match valueExpr with
        | AST.ValueLiteral valueLiteral -> 
            match printNode valueLiteral (indent + 1) with
            | ValueLiteralResult result -> Type.ValueLiteral result
            | _ -> failwith "Unexpected node type"
        | AST.ApplicationExpr applicationExpr -> 
            match printNode applicationExpr (indent + 1) with
            | ApplicationExprResult result -> Type.ApplicationExpr result
            | _ -> failwith "Unexpected node type"
        | AST.BracketedExpr bracketedExpr -> 
            match printNode bracketedExpr (indent + 1) with
            | BracketedExprResult result -> Type.BracketedExpr result
            | _ -> failwith "Unexpected node type"
        | AST.QuantifiedExpr quantifiedExpr -> 
            match printNode quantifiedExpr (indent + 1) with
            | QuantifiedExprResult result -> Type.QuantifiedExpr result
            | _ -> failwith "Unexpected node type"
        | AST.AxiomInfixExpr axiomInfixExpr -> 
            match printNode axiomInfixExpr (indent + 1) with
            | AxiomInfixExprResult result -> Type.AxiomInfixExpr result
            | _ -> failwith "Unexpected node type"
        | AST.AxiomPrefixExpr axiomPrefixExpr -> 
            match printNode axiomPrefixExpr (indent + 1) with
            | AxiomPrefixExprResult result -> Type.AxiomPrefixExpr result
            | _ -> failwith "Unexpected node type"
        | AST.ValueInfixExpr valueInfixExpr -> 
            match printNode valueInfixExpr (indent + 1) with
            | ValueInfixExprResult result -> Type.ValueInfixExpr result
            | _ -> failwith "Unexpected node type"
        | AST.ValuePrefixExpr valuePrefixExpr -> 
            match printNode valuePrefixExpr (indent + 1) with
            | ValuePrefixExprResult result -> Type.ValuePrefixExpr result
            | _ -> failwith "Unexpected node type"
        | AST.LetExpr letExpr -> 
            match printNode letExpr (indent + 1) with
            | LetExprResult result -> Type.LetExpr result
            | _ -> failwith "Unexpected node type"
        | AST.IfExpr ifExpr -> 
            match printNode ifExpr (indent + 1) with
            | IfExprResult result -> Type.IfExpr result
            | _ -> failwith "Unexpected node type"
        | AST.ValueName valueName -> Type.ValueName(printValueName valueName (indent + 1))

and internal printBodyExpr (bodyExpr: AST.BodyExpr) (indent: int): Type.BodyExpr =
    printfn "%sBodyExpr" (String.replicate indent "  ")
    { name = bodyExpr.name; args = bodyExpr.args; functionDefOperator = bodyExpr.functionDefOperator; 
        valueExpr = match printNode bodyExpr.valueExpr (indent + 1) with 
            | ValueExprResult valueExpr -> valueExpr
            | _ -> failwith "Unexpected node type"}

and internal printApplicationExpr (applicationExpr: AST.ApplicationExpr) (indent: int): Type.ApplicationExpr =
    printfn "%sApplicationExpr" (String.replicate indent "  ")
    { name = applicationExpr.name; args = List.map (fun arg -> 
        match printNode arg (indent + 1) with 
            | ValueExprResult valueExpr -> valueExpr
            | _ -> failwith "Unexpected node type"
        ) applicationExpr.args }

and internal printBracketedExpr (bracketedExpr: AST.BracketedExpr) (indent: int): Type.BracketedExpr =
    printfn "%sBracketedExpr" (String.replicate indent "  ")
    { valueExpr = match printNode bracketedExpr.valueExpr (indent + 1) with
        | ValueExprResult valueExpr -> valueExpr
        | _ -> failwith "Unexpected node type"}

and internal printQuantifiedExpr (quantifiedExpr: AST.QuantifiedExpr) (indent: int): Type.QuantifiedExpr =
    printfn "%sQuantifiedExpr" (String.replicate indent "  ")
    let singleTypingList = List.map (fun singleTyping -> 
        match printNode singleTyping (indent + 1) with
            | SingleTypingResult singleTyping -> singleTyping
            | _ -> failwith "Unexpected node type") quantifiedExpr.singleTypingList
    let valueExpr = match printNode quantifiedExpr.valueExpr (indent + 1) with
        | ValueExprResult valueExpr -> valueExpr
        | _ -> failwith "Unexpected node type"
    { quantifier = quantifiedExpr.quantifier; singleTypingList = singleTypingList; valueExpr = valueExpr }

and internal printAxiomInfixExpr (axiomInfixExpr: AST.AxiomInfixExpr) (indent: int): Type.AxiomInfixExpr =
    printfn "%sAxiomInfixExpr" (String.replicate indent "  ")
    let leftExpr = match printNode axiomInfixExpr.leftExpr (indent + 1) with
        | ValueExprResult leftExpr -> leftExpr
        | _ -> failwith "Unexpected node type"
    let rightExpr = match printNode axiomInfixExpr.rightExpr (indent + 1) with
        | ValueExprResult rightExpr -> rightExpr
        | _ -> failwith "Unexpected node type"
    { leftExpr = leftExpr; infixConnective = axiomInfixExpr.infixConnective; rightExpr = rightExpr }

and internal printValueInfixExpr (valueInfixExpr: AST.ValueInfixExpr) (indent: int): Type.ValueInfixExpr =
    printfn "%sValueInfixExpr" (String.replicate indent "  ")
    let leftExpr = match printNode valueInfixExpr.leftExpr (indent + 1) with
        | ValueExprResult leftExpr -> leftExpr
        | _ -> failwith "Unexpected node type"
    let rightExpr = match printNode valueInfixExpr.rightExpr (indent + 1) with
        | ValueExprResult rightExpr -> rightExpr
        | _ -> failwith "Unexpected node type"
    { leftExpr = leftExpr; infixOperator = valueInfixExpr.infixOperator; rightExpr = rightExpr }

and internal printAxiomPrefixExpr (axiomPrefixExpr: AST.AxiomPrefixExpr) (indent: int): Type.AxiomPrefixExpr =
    printfn "%sAxiomPrefixExpr" (String.replicate indent "  ")
    let valueExpr = match printNode axiomPrefixExpr.valueExpr (indent + 1) with
        | ValueExprResult valueExpr -> valueExpr
        | _ -> failwith "Unexpected node type"
    { prefixConnective = axiomPrefixExpr.prefixConnective; valueExpr = valueExpr }

and internal printValuePrefixExpr (valuePrefixExpr: AST.ValuePrefixExpr) (indent: int): Type.ValuePrefixExpr =
    printfn "%sValuePrefixExpr" (String.replicate indent "  ")
    let valueExpr = match printNode valuePrefixExpr.valueExpr (indent + 1) with
        | ValueExprResult valueExpr -> valueExpr
        | _ -> failwith "Unexpected node type"
    { prefixOperator = valuePrefixExpr.prefixOperator; valueExpr = valueExpr }

and internal printLetExpr (letExpr: AST.LetExpr) (indent: int): Type.LetExpr =
    printfn "%sLetExpr" (String.replicate indent "  ")
    let initExpr = match printNode letExpr.initExpr (indent + 1) with
        | ValueExprResult initExpr -> initExpr
        | _ -> failwith "Unexpected node type"
    let scopeExpr = match printNode letExpr.scopeExpr (indent + 1) with
        | ValueExprResult scopeExpr -> scopeExpr
        | _ -> failwith "Unexpected node type"
    { name = letExpr.name; initExpr = initExpr; scopeExpr = scopeExpr }

and internal printIfExpr (ifExpr: AST.IfExpr) (indent: int): Type.IfExpr =
    printfn "%sIfExpr" (String.replicate indent "  ")
    let condExpr = match printNode ifExpr.condExpr (indent + 1) with
        | ValueExprResult condExpr -> condExpr
        | _ -> failwith "Unexpected node type"
    let thenExpr = match printNode ifExpr.thenExpr (indent + 1) with
        | ValueExprResult thenExpr -> thenExpr
        | _ -> failwith "Unexpected node type"
    let elseExpr = match printNode ifExpr.elseExpr (indent + 1) with
        | ValueExprResult elseExpr -> elseExpr
        | _ -> failwith "Unexpected node type"

    { condExpr = condExpr; thenExpr = thenExpr; elseExpr = elseExpr }

and internal printTypeLiteral (typeLiteral: AST.TypeLiteral) (indent: int): AST.TypeLiteral  =
    printfn "%sTypeLiteral" (String.replicate indent "  ")
    match typeLiteral with
        | Bool -> Bool
        | Int -> Int

and internal printTypeName (typeName: AST.TypeName) (indent: int) =
    printfn "%sTypeName" (String.replicate indent "  ")
    { name = typeName.name }

and internal printSubtypeExpr (subTypeExpr: AST.SubtypeExpr) (indent: int): Type.SubtypeExpr =
    printfn "%sSubtypeExpr" (String.replicate indent "  ")
    let singleTyping = match printNode subTypeExpr.singleTyping (indent + 1) with
        | SingleTypingResult singleTyping -> singleTyping
        | _ -> failwith "Unexpected node type"
    let valueExpr = match printNode subTypeExpr.valueExpr (indent + 1) with
        | ValueExprResult valueExpr -> valueExpr
        | _ -> failwith "Unexpected node type"

    { singleTyping = singleTyping; valueExpr = valueExpr }

and internal printSingleTyping (singleTyping: AST.SingleTyping) (indent: int): Type.SingleTyping =
    printfn "%sSingleTyping" (String.replicate indent "  ")
    let typeExpr = match printNode singleTyping.typeExpr (indent + 1) with
        | TypeExprResult typeExpr -> typeExpr
        | _ -> failwith "Unexpected node type"
    { name = singleTyping.name; typeExpr = typeExpr }

and internal printBracketedTypeExpr (bracketedTypeExpr: AST.BracketedTypeExpr) (indent: int): Type.BracketedTypeExpr =
    printfn "%sBracketedTypeExpr" (String.replicate indent "  ")
    let typeExpr = match printNode bracketedTypeExpr.typeExpr (indent + 1) with
        | TypeExprResult typeExpr -> typeExpr
        | _ -> failwith "Unexpected node type"
    { typeExpr = typeExpr }

and internal printValueLiteral (valueLiteral: AST.ValueLiteral) (indent: int): AST.ValueLiteral =
    printfn "%sValueLiteral" (String.replicate indent "  ")
    match valueLiteral with
        | AST.Bool(boolValue) -> 
            printfn "%sBool: %b" (String.replicate (indent + 1) "  ") boolValue
            valueLiteral
        | AST.Int(intValue) -> 
            printfn "%sInt: %d" (String.replicate (indent + 1) "  ") intValue
            valueLiteral

and internal printValueName (valueName: string) (indent: int): string =
    printfn "%sValueName" (String.replicate indent "  ")
    valueName

let prettPrint (ast: AST.Node<AST.SchemeDecl>): string =
    (printNode ast 0).ToString()