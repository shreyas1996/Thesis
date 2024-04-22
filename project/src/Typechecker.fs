module TypeChecker

open AST
open Type
open TypeCollector

type TypingError = string * Position
type TypingResult<'T> = Result<'T, TypingError list>

/// Attempt to match two types, considering potential aliases and base types.
// let matchTypes (env: TypingEnv) expected actual =
//     if expected = actual then Ok ()
//     else Error [("Expected type " + expected.ToString() + " but got " + actual.ToString(), actual.Position)]

/// Resolve types for SetDef, Application, and operations
let rec typeCheckExpr (env: TypingEnv) (node: Node<'E, 'T>) : TypingResult<Type> =
    match node.Expr with
    | Var(name) ->
        match Map.tryFind name env.Vars with
        | Some t -> Ok t
        | None -> Error [("Variable not defined: " + name, node.Pos)]
    | ValueDef(value) ->
        typeCheckExpr env value
    | Add(lhs, rhs) | Sub(lhs, rhs) | Mul(lhs, rhs) | Div(lhs, rhs) ->
        checkBinaryOperation env lhs rhs node.Pos
    | Application(func, args) ->
        typeCheckExpr env func |> Result.bind (fun funcType ->
            match funcType with
            | TFun(paramTypes, returnType) when List.length paramTypes = List.length args ->
                args |> List.map (typeCheckExpr env) |> Result.sequence |> Result.map (fun _ -> returnType)
            | TFun(_, _) -> Error [("Function argument count mismatch", node.Pos)]
            | _ -> Error [("Expression is not a function and cannot be applied", node.Pos)])
    | SetDef(elements) ->
        elements |> List.map (typeCheckExpr env) |> Result.sequence |> Result.map (fun types ->
            let setType = List.head types
            if List.forall ((=) setType) types then TSet(setType)
            else Error [("Set elements types mismatch", node.Pos)])

and checkBinaryOperation env lhs rhs pos =
    let typeLhs = typeCheckExpr env lhs
    let typeRhs = typeCheckExpr env rhs
    Result.bind typeLhs (fun tLhs ->
        Result.bind typeRhs (fun tRhs ->
            if tLhs = tRhs then Ok tLhs
            else Error [("Type mismatch in binary operation", pos)]))

/// Entry point for type checking which should process nodes sequentially
let typeCheckAST (nodes: Expr list) : TypingResult<Expr list> =
    let env = runFirstPass nodes
    nodes |> List.map (typeCheckExpr env) |> Result.sequence
