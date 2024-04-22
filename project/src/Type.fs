//
module Type

type Type =
    | TBool
    | TInt
    | TReal
    | TFloat
    | TString
    | TUnit
    | TVar of name: string
    | TFun of args: List<Type> * ret: Type
    | TProduct of types: List<Type>
    | TSet of elementType: Type

     override this.ToString(): string =
        match this with
        | TBool -> "Bool"
        | TInt -> "Int"
        | TReal -> "Real"
        | TNat -> "Nat"
        | TFloat -> "Float"
        | TString -> "Text"
        | TUnit -> "Unit"
        | TVar(name) -> name
        | TFun(args, ret) ->
            let fmtArg (t: Type) = $"%O{t}"
            let argsStr = List.map fmtArg args
            "(" + System.String.Join(", ", argsStr) + $") -> %O{ret}"
        | TProduct(types) ->
            let fmtType (t: Type) = $"%O{t}"
            let typesStr = List.map fmtType types
            "(" + System.String.Join(", ", typesStr) + ")"
        | TSet(elementType) -> $"Set(%O{elementType})"

let basicTypes = [TBool; TInt; TReal; TString; TUnit]
