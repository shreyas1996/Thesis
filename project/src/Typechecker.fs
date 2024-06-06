module Typechecker

open System.Collections.Generic
open AST

type TypeErrors = list<Position * string>

type TypeInfo =
    | TAbbrevDef of TTypeExpr
    | TSortDef of string
    | TVariantDef of string list

and TTypeExpr =
    | TLit of TTypeLiteral
    | TName of string
    | TSubtype of TSingleTyping
    | TBracketed of TTypeExpr

and TTypeLiteral =
    | TInt
    | TBool

and TSingleTyping = {
    Name: string
    Type: TTypeExpr
}

type VarTypeInfo =
    | TFunction of TTypeExpr list * TTypeExpr
    | TType of TTypeExpr

type TypeEnvironment = {
    Variables: Map<string, VarTypeInfo>
    Types: Map<string, TypeInfo>
}

let emptyEnvironment = {
    Variables = Map.empty
    Types = Map.empty
}

let memo = Dictionary<string, TypeInfo>()

let addVariable (env: TypeEnvironment) (name: string) (typ: VarTypeInfo) = { env with Variables = Map.add name typ env.Variables }
let addType (env: TypeEnvironment) (name: string) (typ: TypeInfo) = { env with Types = Map.add name typ env.Types }

let mergeMaps (map1: Map<string, TypeInfo>) (map2: Map<string, TypeInfo>) =
    map2
    |> Map.fold (fun acc key value -> Map.add key value acc) map1

let rec lookupVariable (env: TypeEnvironment) (name: string) =
    match Map.tryFind name env.Variables with
    | Some typ -> Some typ
    | None -> None

let lookupType (env: TypeEnvironment) (name: string) = Map.tryFind name env.Types

let rec collectTypeExpr (env: TypeEnvironment) (expr: Node<TypeExpr>) =
    match expr.NodeCategory with
    | TypeExpr.TypeLiteral t -> 
        match t.NodeCategory with
        | TypeLiteral.Bool -> TLit TBool
        | TypeLiteral.Int -> TLit TInt
    | TypeExpr.TypeName t -> TName(t.NodeCategory.name)
    | TypeExpr.SubtypeExpr subTypeExpr -> 
        let singleTyping = subTypeExpr.NodeCategory.singleTyping
        let singleTypingType = collectTypeExpr env singleTyping.NodeCategory.typeExpr
        TSubtype({ Name = singleTyping.NodeCategory.name; Type = singleTypingType })
    | TypeExpr.BracketedTypeExpr t -> TBracketed(collectTypeExpr env t.NodeCategory.typeExpr)

let collectDeclarations (env: TypeEnvironment) (decl: Decl): TypeEnvironment =
    match decl with
    | Decl.TypeDecl typeDecl ->
        typeDecl.NodeCategory.typeDefList |> List.fold (fun env typeDef ->
            match typeDef with
            | TypeDef.SortDef sortDef -> addType env sortDef.NodeCategory.name (TSortDef sortDef.NodeCategory.name)
            | TypeDef.AbbrevDef abbrevDef -> 
                let typ = collectTypeExpr env abbrevDef.NodeCategory.typeExpr
                addType env abbrevDef.NodeCategory.name (TAbbrevDef typ)
            | TypeDef.VariantDef variantDef -> 
                let newEnv = addType env variantDef.NodeCategory.name (TVariantDef variantDef.NodeCategory.choice)
                variantDef.NodeCategory.choice |> List.fold (fun env choice -> addVariable env choice (TType (TName variantDef.NodeCategory.name))) newEnv
        ) env
    | Decl.ValueDecl valueDecl ->
        valueDecl.NodeCategory.valueDefList |> List.fold (fun env valueDef ->
            match valueDef with
            | ValueDef.ValueSignature valueSig -> 
                let typ = collectTypeExpr env valueSig.NodeCategory.typeExpr
                addVariable env valueSig.NodeCategory.name (TType typ)
            | ValueDef.ExplicitValueDef explicitValueDef -> 
                let typ = collectTypeExpr env explicitValueDef.NodeCategory.typeExpr
                addVariable env explicitValueDef.NodeCategory.name (TType typ)
            | ValueDef.ExplicitFunctionDef funcDef ->
                let paramTypes = funcDef.NodeCategory.args |> List.map (fun arg -> collectTypeExpr env arg)
                let returnType = collectTypeExpr env funcDef.NodeCategory.returnTypeExpr
                addVariable env funcDef.NodeCategory.name (TFunction (paramTypes, returnType))
        ) env
    | Decl.AxiomDecl _ -> env

let collectSchemeDeclarations (env: TypeEnvironment) (scheme: Node<SchemeDecl>): TypeEnvironment =
    scheme.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.fold collectDeclarations env

let rec isSubtype env (subType: TTypeExpr) (superType: TTypeExpr): bool =
    match (subType, superType) with
    | (TLit subType, TLit superType) -> subType = superType
    | (TName subTypeName, TName superTypeName) -> 
        let typComparison = subTypeName = superTypeName
        if typComparison then true
        else
            match lookupType env subTypeName, lookupType env superTypeName with
            | Some (TAbbrevDef subType), Some (TAbbrevDef superType) -> isSubtype env subType superType
            | _ -> false
    | (TSubtype subType, TSubtype superType) -> isSubtype env subType.Type superType.Type
    | (TName subTypeName, TLit superType) ->
        match lookupType env subTypeName with
        | Some (TAbbrevDef subType) -> isSubtype env subType (TLit superType)
        | _ -> false
    | (TLit subType, TName superTypeName) ->
        match lookupType env superTypeName with
        | Some (TAbbrevDef superType) -> isSubtype env (TLit subType) superType
        | _ -> false
    | (TSubtype subType, TLit superType) -> isSubtype env subType.Type (TLit superType)
    | (TLit subType, TSubtype superType) -> isSubtype env (TLit subType) superType.Type
    | (TSubtype subType, TName superTypeName) ->
        match lookupType env superTypeName with
        | Some (TAbbrevDef superType) -> isSubtype env subType.Type superType
        | _ -> false
    | (TName subTypeName, TSubtype superType) ->
        match lookupType env subTypeName with
        | Some (TAbbrevDef subType) -> isSubtype env subType superType.Type
        | _ -> false
    | _ -> false

let checkRelationalExprTypes (env: TypeEnvironment) (leftType: TTypeExpr) (rightType: TTypeExpr) =
    isSubtype env leftType rightType || isSubtype env rightType leftType

let rec resolveTypeExpr (env: TypeEnvironment) (expr: Node<TypeExpr>) (visited: Set<string>): Result<TTypeExpr, TypeErrors> =
    let rec loop typ visited =
        match typ with
        | TName name when Set.contains name visited -> Error([expr.Pos, sprintf "Cyclic type definition detected: %s" name])
        | TName name -> 
            match lookupType env name with
            | Some typ -> 
                match typ with
                | TAbbrevDef abbTyp -> loop abbTyp (Set.add name visited)
                | TSortDef _ -> Ok(TName name)
                | TVariantDef _ -> Ok(TName name)
            | _ -> Error([expr.Pos, sprintf "Type not found or unknown: %s" name])
        | _ -> Ok(typ)
    match expr.NodeCategory with
    | TypeExpr.TypeLiteral t -> 
        match t.NodeCategory with
        | TypeLiteral.Bool -> Ok(TLit TBool)
        | TypeLiteral.Int -> Ok(TLit TInt)
    | TypeExpr.TypeName t -> 
        match lookupType env t.NodeCategory.name with
        | Some typ -> 
            match typ with
            | TAbbrevDef abbTyp ->
                match loop abbTyp (Set.add t.NodeCategory.name visited) with
                | Ok typ -> Ok(TName(t.NodeCategory.name))
                | Error e -> Error e
            | TSortDef _ -> Ok(TName(t.NodeCategory.name))
            | TVariantDef _ -> Ok(TName(t.NodeCategory.name))
        | _ -> Error([expr.Pos, sprintf "Type not found or unknown: %s" t.NodeCategory.name])
    | TypeExpr.SubtypeExpr subTypeExpr -> 
        let singleTypingExpr = subTypeExpr.NodeCategory.singleTyping
        let resolveSingleTyping = resolveTypeExpr env singleTypingExpr.NodeCategory.typeExpr visited
        match resolveSingleTyping with
        | Ok singleTyping -> 
            let newEnv = addVariable env singleTypingExpr.NodeCategory.name (TType singleTyping)
            let valueType = resolveAndCheckValueExpr newEnv subTypeExpr.NodeCategory.valueExpr
            match valueType with
            | Ok valueType -> 
                if isSubtype env valueType (TLit TBool) then 
                    Ok(TSubtype({ Name = singleTypingExpr.NodeCategory.name; Type = singleTyping }))
                else 
                    Error([subTypeExpr.NodeCategory.valueExpr.Pos, sprintf "Type mismatch in subtype expression at Pos: %A" expr.Pos.Format])
            | Error e -> Error e
        | Error e -> Error e
    | TypeExpr.BracketedTypeExpr t -> resolveTypeExpr env t.NodeCategory.typeExpr visited


and resolveAndCheckValueExpr (env: TypeEnvironment) (expr: Node<ValueExpr>): Result<TTypeExpr, TypeErrors> =
    match expr.NodeCategory with
    | ValueLiteral litNode ->
        match litNode.NodeCategory with
        | ValueLiteral.Bool _ -> Ok(TLit TBool)
        | ValueLiteral.Int _ -> Ok(TLit TInt)
    | ValueName name -> 
        match lookupVariable env name with
        | Some t -> 
            match t with
            | TType t -> Ok(t)
            | TFunction _ -> Error([expr.Pos, sprintf "Variable %s is a function at Pos: %A" name expr.Pos.Format])
        | _ -> Error([expr.Pos, sprintf "Variable %s not found or unknown at Pos: %A" name expr.Pos.Format])
    | ValueInfixExpr infixExpr ->
        let leftType = resolveAndCheckValueExpr env infixExpr.NodeCategory.leftExpr
        let rightType = resolveAndCheckValueExpr env infixExpr.NodeCategory.rightExpr
        let infixTypeErrors = [leftType; rightType] |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
        match leftType, rightType with
        | Ok resolvedLeftType, Ok resolvedRightType ->
            match infixExpr.NodeCategory.infixOperator with
            | InfixOperator.Equals 
            | InfixOperator.NotEquals 
            | InfixOperator.LessThan 
            | InfixOperator.GreaterThan 
            | InfixOperator.LessOrEqual 
            | InfixOperator.GreaterOrEqual ->
                if checkRelationalExprTypes env resolvedLeftType resolvedRightType then Ok(TLit TBool)
                else Error([expr.Pos, sprintf "Type mismatch in relational expression at Pos: %A" expr.Pos.Format])
            | InfixOperator.Add 
            | InfixOperator.Subtract 
            | InfixOperator.Multiply 
            | InfixOperator.Divide ->
                if isSubtype env resolvedLeftType (TLit TInt) && isSubtype env resolvedRightType (TLit TInt) then Ok(TLit TInt)
                else Error([expr.Pos, sprintf "Type mismatch in arithmetic expression at Pos: %A" expr.Pos.Format])
        | _ -> Error(infixTypeErrors)
    | ApplicationExpr appExpr ->
        match lookupVariable env appExpr.NodeCategory.name with
        | Some (TFunction (paramTypes, returnType)) ->
            let argTypes = appExpr.NodeCategory.args |> List.map (fun arg -> resolveAndCheckValueExpr env arg)
            let argTypeErrors = argTypes |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
            if List.isEmpty argTypeErrors then
                let resolvedArgTypes = argTypes |> List.choose (function | Ok t -> Some t | _ -> None)
                // printfn "Resolved arg types: %A" resolvedArgTypes
                // printfn "Param types: %A" paramTypes
                // printfn "Env: %A" env 
                if List.length resolvedArgTypes = List.length paramTypes then
                    if List.forall2 (isSubtype env) resolvedArgTypes paramTypes then Ok(returnType)
                    else Error([expr.Pos, sprintf "Parameter Type mismatch in function application at Pos: %A" expr.Pos.Format])
                else Error([expr.Pos, sprintf "Number of Parameters mismatch in function application at Pos: %A" expr.Pos.Format])
            else Error(argTypeErrors)
        | _ -> Error([expr.Pos, sprintf "Function %s not found or unknown at Pos: %A" appExpr.NodeCategory.name appExpr.Pos.Format])
    | IfExpr ifExpr ->
        let condType = resolveAndCheckValueExpr env ifExpr.NodeCategory.condExpr
        let condTypeErrors = [condType] |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
        let thenType = resolveAndCheckValueExpr env ifExpr.NodeCategory.thenExpr
        let elseType = resolveAndCheckValueExpr env ifExpr.NodeCategory.elseExpr
        let thenTypeErrors = [thenType; elseType] |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
        let concatErrors = List.concat [condTypeErrors; thenTypeErrors]
        match condType with
        | Ok resolvedCondType ->
                let subTypeCondType = isSubtype env resolvedCondType (TLit TBool)
                let subTypeCondError = if not subTypeCondType then [expr.Pos, sprintf "Condition must be of type Bool at Pos: %A" expr.Pos.Format] else []
                match thenType, elseType with
                | Ok resolvedThenType, Ok resolvedElseType -> 
                    let subTypeThenType = isSubtype env resolvedThenType resolvedElseType
                    if subTypeCondType && subTypeThenType then Ok(resolvedThenType)
                    else
                        let subTypeThenError = if not subTypeThenType then [ifExpr.NodeCategory.elseExpr.Pos, sprintf "Type mismatch in then/else branches at Pos: %A" ifExpr.NodeCategory.elseExpr.Pos.Format] else []
                        Error(List.concat [subTypeCondError; subTypeThenError])
                | _ -> Error(List.concat [subTypeCondError; thenTypeErrors])
        | _ -> Error(concatErrors)
    | LetExpr letExpr ->
        let initType = resolveAndCheckValueExpr env letExpr.NodeCategory.initExpr
        match initType with
        | Ok resolvedInitType ->
            let newEnv = addVariable env letExpr.NodeCategory.name (TType resolvedInitType)
            resolveAndCheckValueExpr newEnv letExpr.NodeCategory.scopeExpr
        | Error e -> Error e
    | BracketedExpr brExpr -> resolveAndCheckValueExpr env brExpr.NodeCategory.valueExpr
    | QuantifiedExpr qExpr ->
        let singleTypingErrors, newEnv =
            qExpr.NodeCategory.singleTypingList
            |> List.fold (fun (errors, env) singleTyping ->
                match resolveTypeExpr env singleTyping.NodeCategory.typeExpr Set.empty with
                | Ok tTypeExpr -> 
                    let updatedEnv = addVariable env singleTyping.NodeCategory.name (TType tTypeExpr)
                    (errors, updatedEnv)
                | Error e -> (errors @ e, env)
            ) ([], env)
        
        if List.isEmpty singleTypingErrors then
            resolveAndCheckValueExpr newEnv qExpr.NodeCategory.valueExpr
        else
            Error(singleTypingErrors)
    | AxiomInfixExpr infixExpr ->
        let leftType = resolveAndCheckValueExpr env infixExpr.NodeCategory.leftExpr
        let rightType = resolveAndCheckValueExpr env infixExpr.NodeCategory.rightExpr
        let axiomInfixTypeErrors = [leftType; rightType] |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
        match leftType, rightType with
        | Ok resolvedLeftType, Ok resolvedRightType ->
            match infixExpr.NodeCategory.infixConnective with
            | InfixConnective.And
            | InfixConnective.Or
            | InfixConnective.Implies ->
                if isSubtype env resolvedLeftType (TLit TBool) && isSubtype env resolvedRightType (TLit TBool) then Ok(TLit TBool)
                else Error([expr.Pos, sprintf "Type mismatch in logical expression at Pos: %A" expr.Pos.Format])
        | _ -> Error(axiomInfixTypeErrors)
    | AxiomPrefixExpr prefixExpr ->
        let exprType = resolveAndCheckValueExpr env prefixExpr.NodeCategory.valueExpr
        match exprType with
        | Ok resolvedExprType ->
            match prefixExpr.NodeCategory.prefixConnective with
            | PrefixConnective.Not ->
                if isSubtype env resolvedExprType (TLit TBool) then Ok(TLit TBool)
                else Error([expr.Pos, sprintf "Type mismatch in logical expression at Pos: %A" expr.Pos.Format])
        | Error e -> Error e
    | ValuePrefixExpr prefixExpr ->
        let exprType = resolveAndCheckValueExpr env prefixExpr.NodeCategory.valueExpr
        match exprType with
        | Ok resolvedExprType ->
            match prefixExpr.NodeCategory.prefixOperator with
            | PrefixOperator.Abs ->
                if isSubtype env resolvedExprType (TLit TInt) then Ok(TLit TInt) else Error([expr.Pos, sprintf "Type mismatch in absolute value expression at Pos: %A" expr.Pos.Format])
        | Error e -> Error e

let resolveAndCheckFunction env (funcDef: Node<ExplicitFunctionDef>): Result<TTypeExpr, TypeErrors> =
    // Resolve parameter types and add to the function scope
    let paramTypes = funcDef.NodeCategory.args |> List.map (fun arg -> resolveTypeExpr env arg Set.empty)
    let paramTypeErrors = paramTypes |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
    if (List.isEmpty paramTypeErrors) then
        let resolvedParamTypes = paramTypes |> List.choose (function | Ok t -> Some t | _ -> None)
        // Resolve return type
        let returnType = resolveTypeExpr env funcDef.NodeCategory.returnTypeExpr Set.empty
        match returnType with
        | Ok resolvedReturnType ->
            if List.length funcDef.NodeCategory.bodyExpr.NodeCategory.args = List.length resolvedParamTypes then
                let newEnv = List.fold2 (fun env paramName paramType -> addVariable env paramName (TType paramType)) env funcDef.NodeCategory.bodyExpr.NodeCategory.args resolvedParamTypes
                // Resolve and typecheck the body expression in the function scope
                let bodyType = resolveAndCheckValueExpr newEnv funcDef.NodeCategory.bodyExpr.NodeCategory.valueExpr
                match bodyType with
                | Ok resolvedBodyType -> if not (isSubtype env resolvedBodyType resolvedReturnType) then Error([funcDef.Pos, sprintf "Return type mismatch in function %s at Pos: %A" funcDef.NodeCategory.name funcDef.Pos.Format]) else Ok(resolvedReturnType)
                | Error e -> Error e
            else
                Error([funcDef.Pos, sprintf "Number of Parameters mismatch in function Definition %s at Pos: %A" funcDef.NodeCategory.name funcDef.Pos.Format])
        | Error e -> Error e
    else Error(paramTypeErrors)

let rec resolveAndCheckDecl env (decl: Decl): Result<bool, TypeErrors> =
    let results = 
        match decl with
        | Decl.TypeDecl typeDecl ->
            typeDecl.NodeCategory.typeDefList |> List.map (fun typeDef ->
                match typeDef with
                | TypeDef.AbbrevDef abbrevDef -> 
                    resolveTypeExpr env abbrevDef.NodeCategory.typeExpr Set.empty
                | TypeDef.SortDef sortDef -> 
                    match lookupType env sortDef.NodeCategory.name with
                    | Some (TSortDef sortDefName) -> Ok(TName sortDefName)
                    | _ -> Error([sortDef.Pos, sprintf "Sort not found or unknown: %s at Pos: %A" sortDef.NodeCategory.name sortDef.Pos.Format])
                | TypeDef.VariantDef variantDef ->
                    match lookupType env variantDef.NodeCategory.name with
                    | Some (TVariantDef _) -> Ok(TName variantDef.NodeCategory.name)
                    | _ -> Error([variantDef.Pos, sprintf "Variant not found or unknown: %s at Pos: %A" variantDef.NodeCategory.name variantDef.Pos.Format])
            )
        | Decl.ValueDecl valueDecl ->
            valueDecl.NodeCategory.valueDefList |> List.map (fun valueDef ->
                match valueDef with
                | ValueDef.ExplicitFunctionDef funcDef -> 
                    resolveAndCheckFunction env funcDef
                | ValueDef.ExplicitValueDef valueDef ->
                    let valueType = resolveAndCheckValueExpr env valueDef.NodeCategory.valueExpr
                    let expectedType = resolveTypeExpr env valueDef.NodeCategory.typeExpr Set.empty
                    let typeErrors = [valueType; expectedType] |> List.choose (function | Ok _ -> None | Error e -> Some e) |> List.concat
                    match valueType, expectedType with
                    | Ok resolvedValueType, Ok resolvedExpectedType ->
                        if not (isSubtype env resolvedValueType resolvedExpectedType) then Error([valueDef.Pos, sprintf "Type mismatch in value definition %s at Pos: %A" valueDef.NodeCategory.name valueDef.Pos.Format])
                        else Ok resolvedExpectedType
                    | _ -> Error(typeErrors)
                | ValueDef.ValueSignature valSig ->
                    resolveTypeExpr env valSig.NodeCategory.typeExpr Set.empty
            )
        | Decl.AxiomDecl axiomDecl ->
            axiomDecl.NodeCategory.axiomDefList |> List.map (fun axiomDef ->
                let axiomType = resolveAndCheckValueExpr env axiomDef.NodeCategory.logicalValueExpr
                match axiomType with
                | Ok resolvedAxiomType ->
                    if not (isSubtype env resolvedAxiomType (TLit TBool)) then Error([axiomDef.Pos, sprintf "Axiom must result in Bool at Pos: %A" axiomDef.Pos.Format])
                    else Ok resolvedAxiomType
                | Error e -> Error e
            )
    let errors = results |> List.choose (function | Error e -> Some e | _ -> None)
    match errors with
    | [] -> Ok true
    | _ -> Error (List.concat errors)

let resolveAndCheckScheme env (scheme: Node<SchemeDecl>) =
    let results = scheme.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.map (resolveAndCheckDecl env)
    let errors = results |> List.choose (function | Error e -> Some e | _ -> None)
    match errors with
    | [] -> Ok true
    | _ -> Error (List.concat errors)

let typeCheckAST (program: Node<SchemeDecl>): Result<TypeEnvironment, TypeErrors> =
    let initialEnv = emptyEnvironment
    let env = collectSchemeDeclarations initialEnv program
    let result = resolveAndCheckScheme env program
    // printfn $"Typechecking Result: %A{env}"
    match result with
    | Ok _ -> Ok env
    | Error e -> Error e
