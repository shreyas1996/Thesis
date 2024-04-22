module TypeCollector

open System.Collections.Generic

open AST
open Type

type TypingEnv = {
    TypeVars: Map<string, Type>;
    Vars: Map<string, Type>;
}

type TypeErrors = list<Position * string>

let emptyEnv = { TypeVars = Map.empty; Vars = Map.empty }

let rec collectTypeDefinitions env (node: UntypedAST) =
    match node.Expr with
    | SortDef(name, pretype) ->
        { env with TypeVars = env.TypeVars.Add(name, TVar(name)) }
    | AbbrevDef(name, pretype) ->
        match resolvePretype env pretype with
        | Ok(t) -> { env with TypeVars = env.TypeVars.Add(name, t) }
        | Error(_) -> env
    | VariantDef(name, variants) ->
        let results = variants |> List.map (resolvePretype env)
        match List.sequence results with
        | Ok(types) -> { env with TypeVars = env.TypeVars.Add(name, TProduct(types)) } // This is incorrect for a true union but is a placeholder
        | Error(_) -> env
    | _ -> env

and resolvePretype env (pretype: AST.PretypeNode) : Result<Type, TypeErrors> =
    match pretype.Pretype with
    | Pretype.TId(name) -> 
        match (lookupTypeVar env name) with
        | Some(t) -> Ok(t)
        | None -> Error([(pretype.Pos, $"reference to undefined type: %s{name}")])
    | Pretype.TFun(args, ret) ->
        let resolvedArgs = List.map (resolvePretype env) args |> List.sequence
        let resolvedRet = resolvePretype env ret
        Result.bind resolvedArgs (fun args ->
            Result.map (fun ret -> TFun(args, ret)) resolvedRet)
    | Pretype.TSet(elementType, isInf) -> resolvePretype env elementType |> Result.map TSet
    | Pretype.TProduct(types) -> List.map (resolvePretype env) types |> List.sequence |> Result.map TProduct
    | _ -> Error([pretype.Pos, "Unsupported or unknown pretype"])

and internal lookupTypeVar (env: TypingEnv) (name: string): Option<Type> =
    // Mapping between type names and known basic types
    let btmap = Map (List.map (fun t -> (t.ToString(), t)) Type.basicTypes)
    match (btmap.TryFind name) with
    | Some(t) -> Some(t)
    | None ->
        // Let's check whether we are dealing with a type alias.  Note that we
        // do *not* recursively resolve the type alias with its definition
        match (env.TypeVars.TryFind(name)) with
        | Some(_) -> Some(TVar(name))
        | None -> None



let runFirstPass (nodes: UntypedAST) =
    collectTypeDefinitions emptyEnv nodes
