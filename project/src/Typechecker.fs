module Typechecker

open System.Collections.Generic
open AST

type TypeInfo =
    | TInt
    | TBool
    | TFunction of TypeInfo list * TypeInfo
    | TSortDefined
    | TUserDefined of string
    | TVariant of string list
    | UnknownType


type TypeEnvironment = {
    Variables: Map<string, TypeInfo>
    Types: Map<string, TypeInfo>
}

let emptyEnvironment = {
    Variables = Map.empty
    Types = Map.empty
}

let memo = Dictionary<string, TypeInfo>()

let addVariable (env: TypeEnvironment) (name: string) (typ: TypeInfo) = { env with Variables = Map.add name typ env.Variables }
let addType (env: TypeEnvironment) (name: string) (typ: TypeInfo) = { env with Types = Map.add name typ env.Types }

let mergeMaps (map1: Map<string, TypeInfo>) (map2: Map<string, TypeInfo>) =
    map2
    |> Map.fold (fun acc key value -> Map.add key value acc) map1

let rec lookupVariable (env: TypeEnvironment) (name: string) =
    match Map.tryFind name env.Variables with
    | Some typ -> Some typ
    | None -> None

let lookupType (env: TypeEnvironment) (name: string) = Map.tryFind name env.Types

let rec isSubtype env (subType: TypeInfo) (superType: TypeInfo) =
    match (subType, superType) with
    | (TInt, TInt) | (TBool, TBool) -> true
    | (TUserDefined subName, TUserDefined superName) when subName = superName -> true
    | (TUserDefined subName, superType) ->
        match lookupType env subName with
        | Some baseType -> isSubtype env baseType superType
        | None -> false
    | (TFunction (subParams, subReturn), TFunction (superParams, superReturn)) ->
        List.length subParams = List.length superParams &&
        List.forall2 (isSubtype env) subParams superParams &&
        isSubtype env subReturn superReturn
    | (TVariant subChoices, TVariant superChoices) ->
        List.length subChoices = List.length superChoices &&
        List.forall2 (fun subChoice superChoice -> subChoice = superChoice) subChoices superChoices
    | _ -> false

let collectDeclarations (env: TypeEnvironment) (decl: Decl): TypeEnvironment =
    match decl with
    | Decl.TypeDecl typeDecl ->
        typeDecl.NodeCategory.typeDefList |> List.fold (fun env typeDef ->
            match typeDef with
            | TypeDef.SortDef sortDef -> addType env sortDef.NodeCategory.name TSortDefined
            | TypeDef.AbbrevDef abbrevDef -> 
                let typ = match abbrevDef.NodeCategory.typeExpr.NodeCategory with
                          | TypeExpr.TypeLiteral t -> 
                                match t.NodeCategory with
                                | TypeLiteral.Bool -> TBool
                                | TypeLiteral.Int -> TInt
                          | TypeExpr.TypeName t -> TUserDefined t.NodeCategory.name
                          | _ -> UnknownType

                addType env abbrevDef.NodeCategory.name typ
            | TypeDef.VariantDef variantDef -> 
                let newEnv = addType env variantDef.NodeCategory.name (TVariant variantDef.NodeCategory.choice)
                variantDef.NodeCategory.choice |> List.fold (fun env choice -> addVariable env choice (TUserDefined variantDef.NodeCategory.name)) newEnv
        ) env
    | Decl.ValueDecl valueDecl ->
        valueDecl.NodeCategory.valueDefList |> List.fold (fun env valueDef ->
            match valueDef with
            | ValueDef.ValueSignature valueSig -> addVariable env valueSig.NodeCategory.name UnknownType
            | ValueDef.ExplicitValueDef valueDef -> addVariable env valueDef.NodeCategory.name UnknownType
            | ValueDef.ExplicitFunctionDef funcDef ->
                let paramTypes = funcDef.NodeCategory.args |> List.map (fun arg -> UnknownType)
                addVariable env funcDef.NodeCategory.name (TFunction (paramTypes, UnknownType))
        ) env
    | Decl.AxiomDecl _ -> env

let collectSchemeDeclarations (env: TypeEnvironment) (scheme: Node<SchemeDecl>): TypeEnvironment =
    scheme.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.fold collectDeclarations env

let checkRelationalExprTypes (env: TypeEnvironment) (leftType: TypeInfo) (rightType: TypeInfo) =
    isSubtype env leftType rightType || isSubtype env rightType leftType

let rec resolveTypeExpr (env: TypeEnvironment) (expr: Node<TypeExpr>) (visited: Set<string>) =
    let rec loop typ visited =
        match typ with
        | TUserDefined name ->
            if Set.contains name visited then
                failwithf "Circular type definition detected: %s" name
            else
                match lookupType env name with
                | Some typ -> 
                    if memo.ContainsKey(name) then
                        memo.[name]
                    else
                        let resolved = loop typ (Set.add name visited)
                        memo.[name] <- resolved
                        resolved
                | _ -> failwithf "Type not found or unknown: %s" name
        | _ -> typ

    match expr.NodeCategory with
    | TypeExpr.TypeLiteral t -> 
        match t.NodeCategory with
        | TypeLiteral.Bool -> TBool
        | TypeLiteral.Int -> TInt
    | TypeExpr.TypeName t -> 
        match lookupType env t.NodeCategory.name with
        | Some typ -> loop typ (Set.add t.NodeCategory.name visited)
        | _ -> failwithf "Type not found or unknown: %s" t.NodeCategory.name
    | TypeExpr.SubtypeExpr subTypeExpr -> 
        let singleTypingExpr = subTypeExpr.NodeCategory.singleTyping
        let singleTyping = resolveTypeExpr env singleTypingExpr.NodeCategory.typeExpr visited
        let newEnv = addVariable env singleTypingExpr.NodeCategory.name singleTyping
        let valueType = resolveAndCheckValueExpr newEnv subTypeExpr.NodeCategory.valueExpr

        if isSubtype newEnv valueType TBool then singleTyping else failwith "Subtype expression must result in Bool"
    | TypeExpr.BracketedTypeExpr t -> resolveTypeExpr env t.NodeCategory.typeExpr visited


and resolveAndCheckValueExpr (env: TypeEnvironment) (expr: Node<ValueExpr>) =
    match expr.NodeCategory with
    | ValueLiteral litNode ->
        match litNode.NodeCategory with
        | ValueLiteral.Bool _ -> TBool
        | ValueLiteral.Int _ -> TInt
    | ValueName name -> 
        match lookupVariable env name with
        | Some t -> t
        | _ -> failwithf "Variable not found or unknown: %s" name
    | ValueInfixExpr infixExpr ->
        let leftType = resolveAndCheckValueExpr env infixExpr.NodeCategory.leftExpr
        let rightType = resolveAndCheckValueExpr env infixExpr.NodeCategory.rightExpr
        match infixExpr.NodeCategory.infixOperator with
        | InfixOperator.Equals 
        | InfixOperator.NotEquals 
        | InfixOperator.LessThan 
        | InfixOperator.GreaterThan 
        | InfixOperator.LessOrEqual 
        | InfixOperator.GreaterOrEqual ->
            if checkRelationalExprTypes env leftType rightType then TBool
            else failwith "Type mismatch in relational expression"
        | InfixOperator.Add 
        | InfixOperator.Subtract 
        | InfixOperator.Multiply 
        | InfixOperator.Divide ->
            if isSubtype env leftType TInt && isSubtype env rightType TInt then TInt
            else failwith "Type mismatch in arithmetic expression"
    | ApplicationExpr appExpr ->
        match lookupVariable env appExpr.NodeCategory.name with
        | Some (TFunction (paramTypes, returnType)) ->
            let argTypes = appExpr.NodeCategory.args |> List.map (fun arg -> resolveAndCheckValueExpr env arg)
            if List.forall2 (isSubtype env) argTypes paramTypes then returnType
            else failwith "Type mismatch in function application"
        | _ -> failwithf "Function %s not found or not a function" appExpr.NodeCategory.name
    | IfExpr ifExpr ->
        let condType = resolveAndCheckValueExpr env ifExpr.NodeCategory.condExpr
        if not (isSubtype env condType TBool) then failwith "Condition of if expression must be Bool"
        let thenType = resolveAndCheckValueExpr env ifExpr.NodeCategory.thenExpr
        let elseType = resolveAndCheckValueExpr env ifExpr.NodeCategory.elseExpr
        if isSubtype env thenType elseType then thenType
        else failwith "Type mismatch in branches of if expression"
    | LetExpr letExpr ->
        let initType = resolveAndCheckValueExpr env letExpr.NodeCategory.initExpr
        let newEnv = addVariable env letExpr.NodeCategory.name initType
        resolveAndCheckValueExpr newEnv letExpr.NodeCategory.scopeExpr
    | BracketedExpr brExpr -> resolveAndCheckValueExpr env brExpr.NodeCategory.valueExpr
    | QuantifiedExpr qExpr ->
        let newEnv = qExpr.NodeCategory.singleTypingList |> List.fold (fun env singleTyping ->
            let typ = resolveTypeExpr env singleTyping.NodeCategory.typeExpr Set.empty
            addVariable env singleTyping.NodeCategory.name typ) env
        let bodyType = resolveAndCheckValueExpr newEnv qExpr.NodeCategory.valueExpr
        if isSubtype env bodyType TBool then TBool else failwith "Quantified expression must result in Bool"
    | AxiomInfixExpr infixExpr ->
        let leftType = resolveAndCheckValueExpr env infixExpr.NodeCategory.leftExpr
        let rightType = resolveAndCheckValueExpr env infixExpr.NodeCategory.rightExpr
        match infixExpr.NodeCategory.infixConnective with
        | InfixConnective.And
        | InfixConnective.Or
        | InfixConnective.Implies ->
            if isSubtype env leftType TBool && isSubtype env rightType TBool then TBool
            else failwith "Type mismatch in logical expression"
    | AxiomPrefixExpr prefixExpr ->
        let exprType = resolveAndCheckValueExpr env prefixExpr.NodeCategory.valueExpr
        match prefixExpr.NodeCategory.prefixConnective with
        | PrefixConnective.Not ->
            if isSubtype env exprType TBool then TBool else failwith "Type mismatch in logical expression"
    | ValuePrefixExpr prefixExpr ->
        let exprType = resolveAndCheckValueExpr env prefixExpr.NodeCategory.valueExpr
        match prefixExpr.NodeCategory.prefixOperator with
        | PrefixOperator.Abs ->
            if isSubtype env exprType TInt then TInt else failwith "Type mismatch in prefix expression"

let resolveAndCheckFunction env (funcDef: Node<ExplicitFunctionDef>) =
    // Resolve parameter types and add to the function scope
    let paramTypes = funcDef.NodeCategory.args |> List.map (fun arg -> resolveTypeExpr env arg Set.empty)
   
    // Resolve return type
    let returnType = resolveTypeExpr env funcDef.NodeCategory.returnTypeExpr Set.empty
    
    // Update the function type in the original environment
    let updatedFuncType = TFunction (paramTypes, returnType)
    let updatedEnv = addVariable env funcDef.NodeCategory.name updatedFuncType

    let newEnv = List.fold2 (fun env paramName paramType -> addVariable env paramName paramType) updatedEnv funcDef.NodeCategory.bodyExpr.NodeCategory.args paramTypes

    // Resolve and typecheck the body expression in the function scope
    let bodyType = resolveAndCheckValueExpr newEnv funcDef.NodeCategory.bodyExpr.NodeCategory.valueExpr
    
    // Ensure the return type of the body matches the declared return type
    if not (isSubtype env bodyType returnType) then failwithf "Return type mismatch in function %s" funcDef.NodeCategory.name
    
    // Return the updated environment
    updatedEnv
    

let rec resolveAndCheckDecl env (decl: Decl) =
    match decl with
    | Decl.TypeDecl typeDecl ->
        typeDecl.NodeCategory.typeDefList |> List.fold (fun env typeDef ->
            match typeDef with
            | TypeDef.AbbrevDef abbrevDef -> 
                let typ = resolveTypeExpr env abbrevDef.NodeCategory.typeExpr Set.empty
                addType env abbrevDef.NodeCategory.name typ
            | _ -> env
        ) env
    | Decl.ValueDecl valueDecl ->
        valueDecl.NodeCategory.valueDefList |> List.fold (fun env valueDef ->
            match valueDef with
            | ValueDef.ExplicitFunctionDef funcDef -> resolveAndCheckFunction env funcDef
            | ValueDef.ExplicitValueDef valueDef ->
                let valueType = resolveAndCheckValueExpr env valueDef.NodeCategory.valueExpr
                let expectedType = resolveTypeExpr env valueDef.NodeCategory.typeExpr Set.empty
                if not (isSubtype env valueType expectedType) then failwithf "Type mismatch in value definition %s" valueDef.NodeCategory.name
                env
            | ValueDef.ValueSignature valSig -> 
                let expectedType = resolveTypeExpr env valSig.NodeCategory.typeExpr Set.empty
                addVariable env valSig.NodeCategory.name expectedType

        ) env
    | Decl.AxiomDecl axiomDecl ->
        axiomDecl.NodeCategory.axiomDefList |> List.fold (fun env axiomDef ->
            let axiomType = resolveAndCheckValueExpr env axiomDef.NodeCategory.logicalValueExpr
            if not (isSubtype env axiomType TBool) then failwithf "Axiom %s must be of type Bool" axiomDef.NodeCategory.name
            env
        ) env

let resolveAndCheckScheme env (scheme: Node<SchemeDecl>) =
    scheme.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.fold resolveAndCheckDecl env

let typeCheckAST (program: Node<SchemeDecl>) =
    let initialEnv = emptyEnvironment
    let env = collectSchemeDeclarations initialEnv program
    let finalEnv = resolveAndCheckScheme env program
    finalEnv
