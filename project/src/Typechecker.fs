module TypeChecker

open AST

type TypeEnv = Map<string, TypeInfo>

and TypeInfo =
    | BasicType of string
    | FunctionType of string list * string
    | VariantType of string list

let initializeTypeEnv () : TypeEnv =
    Map.empty
    |> Map.add "Int" (BasicType "Int")
    |> Map.add "Bool" (BasicType "Bool")

let extendEnv (env: TypeEnv) (name: string) (typ: TypeInfo) =
    Map.add name typ env

let lookupEnv (env: TypeEnv) (name: string) =
    Map.tryFind name env

// Utility to extract type from a type expression node
let rec lookupTypeExpr (env: TypeEnv) (expr: Node<TypeExpr>) : TypeInfo =
    match expr.NodeCategory with
    | TypeLiteral tl -> BasicType (match tl with | Bool -> "Bool" | Int -> "Int")
    | TypeName tn ->
        match lookupEnv env tn.name with
        | Some typ -> typ
        | None -> failwith $"Unknown type {tn.name}"
    | SubtypeExpr se -> failwith "Subtype expressions not supported in type definitions"
    | BracketedTypeExpr be -> lookupTypeExpr env be.NodeCategory.typeExpr

// Type checking for value expressions
let rec typeCheck (env: TypeEnv) (expr: Node<ValueExpr>) : Result<TypeInfo, string> =
    match expr.NodeCategory with
    | ValueLiteral vl ->
        Ok (match vl with
            | Bool _ -> BasicType "Bool"
            | Int _ -> BasicType "Int")
    | ValueName name ->
        match lookupEnv env name with
        | Some typ -> Ok typ
        | None -> Error $"Undefined variable: {name}"
    | ApplicationExpr ae ->
        match lookupEnv env ae.NodeCategory.name with
        | Some (FunctionType (argTypes, returnType)) ->
            if List.length ae.NodeCategory.args = List.length argTypes then
                let argResults = List.map2 (typeCheck env) ae.NodeCategory.args (List.map (fun t -> BasicType t) argTypes)
                if List.exists (Result.isError) argResults then
                    Error "Type mismatch in function arguments"
                else
                    Ok (BasicType returnType)
            else
                Error "Argument count mismatch"
        | _ -> Error $"Function {ae.NodeCategory.name} not found or is not a function"
    | IfExpr ie ->
        match (typeCheck env ie.NodeCategory.condExpr, typeCheck env ie.NodeCategory.thenExpr, typeCheck env ie.NodeCategory.elseExpr) with
        | (Ok (BasicType "Bool"), Ok t1, Ok t2) when t1 = t2 -> Ok t1
        | _ -> Error "If expression type mismatch or condition is not boolean"
    | _ -> Error "Expression type not supported yet"

// Integrating type definitions
let integrateTypeDefs (env: TypeEnv) (typeDefs: list<TypeDef>) : TypeEnv =
    List.fold (fun env def ->
        match def with
        | SortDef sd -> extendEnv env sd.NodeCategory.name (BasicType sd.NodeCategory.name)
        | AbbrevDef ad -> extendEnv env ad.NodeCategory.name (lookupTypeExpr env ad.NodeCategory.typeExpr)
        | VariantDef vd -> extendEnv env vd.NodeCategory.name (VariantType vd.NodeCategory.choice)
    ) env typeDefs

// Integrating value definitions
let integrateValueDefs (env: TypeEnv) (valueDefs: list<ValueDef>) : TypeEnv =
    List.fold (fun env def ->
        match def with
        | ValueSignature vs ->
            extendEnv env vs.NodeCategory.name (lookupTypeExpr env vs.NodeCategory.typeExpr)
        | ExplicitValueDef evd ->
            let expectedType = lookupTypeExpr env evd.NodeCategory.typeExpr
            match typeCheck env evd.NodeCategory.valueExpr with
            | Ok actualType when actualType = expectedType -> extendEnv env evd.NodeCategory.name expectedType
            | _ -> failwith "Type mismatch in explicit value definition"
        | ExplicitFunctionDef efd ->
            let argTypes = List.map (lookupTypeExpr env) efd.NodeCategory.args
            let returnType = lookupTypeExpr env efd.NodeCategory.returnTypeExpr
            extendEnv env efd.NodeCategory.name (FunctionType (argTypes, returnType))
    ) env valueDefs

// Check axioms which must be logical expressions
let typeCheckAxioms (env: TypeEnv) (axiomDefs: list<Node<AxiomDef>>) : Result<unit, string> =
    List.fold (fun result axiomDef ->
        match result with
        | Error _ as e -> e
        | Ok () ->
            match typeCheck env axiomDef.NodeCategory.logicalValueExpr with
            | Ok (BasicType "Bool") -> Ok ()
            | _ -> Error "Axiom expressions must be of type Bool"
    ) (Ok ()) axiomDefs

// Example usage in main module
let typeCheckAST (ast: Node<SchemeDecl>) =
    let env = initializeTypeEnv ()
    let env = integrateTypeDefs env ast.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.collect (function | TypeDecl td -> td.NodeCategory.typeDefList | _ -> [])
    let env = integrateValueDefs env ast.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.collect (function | ValueDecl vd -> vd.NodeCategory.valueDefList | _ -> [])
    let result = typeCheckAxioms env ast.NodeCategory.schemeDef.NodeCategory.classExpr.NodeCategory.optDecl |> List.collect (function | AxiomDecl ad -> ad.NodeCategory.axiomDefList | _ -> [])
    result
