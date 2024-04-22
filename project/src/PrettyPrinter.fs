//
module PrettyPrinter

open AST

/// Newline symbol for the current operating system.
let internal nl = System.Environment.NewLine

/// Generic hierarchical representation of a tree for pretty-printing.
type internal Tree =
    | Node of descr: string * subtrees: List<string * Tree>

    with
        /// Return a nice, indenter representation of the tree.  The argument
        /// 'indent' is a string (expected to only contain spaces) providing the
        /// visual indentation from the left.
        member this.Format (indent: string): string =
            match this with
            | Node(descr, subtrees) ->
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

        /// Return a nice, indented representation of the tree.
        override this.ToString(): string =
            this.Format ""

let rec internal formatType (t: Type.Type): Tree =
    match t with
    | Type.TBool -> Node("bool", [])
    | Type.TInt -> Node("int", [])
    | Type.TFloat -> Node("float", [])
    | Type.TString -> Node("string", [])
    | Type.TUnit -> Node("unit", [])
    | Type.TVar(name) -> Node(name, [])
    | Type.TFun(args, ret) ->
        /// Formatted argument types with their respective positions
        let argChildren =
            List.map (fun (i, t) -> ($"arg %d{i+1}", formatType t))
                     (List.indexed args)
        Node("fun", (argChildren @
                     [("return", formatType ret)]))
    | Type.TProduct(types) ->
        let typeChildren =
            List.map (fun (i, t) -> ($"type %d{i+1}", formatType t))
                     (List.indexed types)
        Node("product", typeChildren)
    | Type.TSet(elementType) -> Node("set", [("", formatType elementType)])

// let rec internal formatTypingEnv (env: Typechecker.TypingEnv): List<string * Tree> =
//     let formatMap (m: Map<string, Type.Type>): List<string * Tree> =
//         List.map (fun (name, tpe) -> (name, formatType tpe)) (Map.toList m)
//     let formatSet (s: Set<string>): string =
//         if s.IsEmpty then "∅" else Util.formatAsSet s
//     let vars = formatMap env.Vars
//     let typeVars = formatMap env.TypeVars
//     let varsNode = Node((if vars.IsEmpty then "∅" else "Map"), vars)
//     let typeVarsNode = Node((if typeVars.IsEmpty then "∅" else "Map"), typeVars)
//     [("Env.Vars", varsNode); ("Env.TypeVars", typeVarsNode)]

/// Traverse an RSL AST node and return its hierarchical representation.
let rec internal formatASTRec (node: AST.Node<'E,'T>): Tree =
    // (typeInfo: 't option)
    // let typeDescr = typeInfo |> Option.map (fun t -> "Type: " + t.ToString()) |> Option.defaultValue ""
    let mkTree (descr: string) (node: AST.Node<'E,'T>)
               (children: List<string * Tree>): Tree =
        Node($"%s{descr} %s{node.Pos.Format}",
             (formatNodeTypingInfo node) @ children)
    match node.Expr with
        | UnitVal -> mkTree "UnitVal ()" node []
        | IntVal(value) -> mkTree $"IntVal %d{value}" node []
        | BoolVal(value) -> mkTree $"BoolVal %b{value}" node []
        | FloatVal(value) -> mkTree $"FloatVal %f{value}" node []
        | StringVal(value) -> mkTree $"StringVal \"%s{value}\"" node []
        | Var(name) -> mkTree $"Var %s{name}" node []
        | ValList(exprs) ->
            let exprNodes = List.map (fun node -> ("", formatASTRec node)) exprs
            Node("ValList", [("Exprs", Node("", exprNodes))])
            // mkTree "ValList" node [("Exprs", Node("", exprNodes))]
        | Add(lhs, rhs) -> 
            Node("Addition", [("LHS", formatASTRec lhs); ("RHS", formatASTRec rhs)])
            // mkTree "Add" node [("lhs", formatASTRec lhs)
            //                ("rhs", formatASTRec rhs)]
        | Sub(lhs, rhs) -> 
            Node("Subtraction", [("LHS", formatASTRec lhs); ("RHS", formatASTRec rhs)])
            // mkTree "Subtraction" node [("lhs", formatASTRec lhs)
            //                ("rhs", formatASTRec rhs)]
        | Mul(lhs, rhs) -> 
            Node("Multiplication", [("LHS", formatASTRec lhs); ("RHS", formatASTRec rhs)])
            // mkTree "Multiplication" node [("lhs", formatASTRec lhs)
            //                ("rhs", formatASTRec rhs)]
        | Div(lhs, rhs) -> 
            Node("Division", [("LHS", formatASTRec lhs); ("RHS", formatASTRec rhs)])
            // mkTree "Division" node [("lhs", formatASTRec lhs)
            //                ("rhs", formatASTRec rhs)]
        | Exponent(expr1, expr2) -> 
            Node("Exponent", [("Base", formatASTRec expr1); ("Power", formatASTRec expr2)])
            // mkTree "Exponent" node [("Base", formatASTRec expr1)
            //                ("Power", formatASTRec expr2)]
        | TP(expr) -> 
            // let tpExprs = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) expr
            Node("TP", [("", formatASTRec expr)])
            // mkTree "TP" node [("", formatASTRec expr)]
        | Yenda(expr) -> 
            let tpExprs = List.map (fun node -> ("", formatPretypeNode node)) expr
            Node("Yenda", [("", Node("", tpExprs))])
        | Yenda2(expr) -> 
            // let tpExprs = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) expr
            Node("Yenda2", [("Yenda2", formatPretypeNode expr)])
        | TypeDef(name, tpe) -> Node("TypeDef", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe)])
        | SortDef(name, tpe) -> 
            Node("SortDef", [("Pretype", formatPretypeNode tpe)])
            // mkTree "SortDef" node [("Pretype", formatPretypeNode tpe)]
        | VariantDef(name, tpe) -> 
            let tpUnzipped = List.map (fun node -> ("", formatPretypeNode node)) tpe
            Node("VariantDef", [("Name", Node(name, [])); ("Pretype", Node("", tpUnzipped))])
            // mkTree "VariantDef" node [("Name", Node(name, [])); ("Pretype", Node("", tpUnzipped))]
        | AbbrevDef(name, tpe) -> Node("AbbrevDef", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe)])
        | ClassDef(name, decList) ->
            // let typeDefNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) typeDefs
            // let valueDefNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) valueDefs
            let decListNodes = List.map (fun node -> ("", formatASTRec node)) decList
            // Node("ClassDef", [("Name", Node(name, [])); ("TypeDefs", Node("", typeDefNodes)); ("ValueDefs", Node("", valueDefNodes))])
            // let decListNodes = List.map (fun nodes -> ("", Node("", nodes))) decListNodes
            Node("ClassDef", [("Name", Node(name, [])); ("Declarations", Node("", decListNodes))])
        | ValueDef(expr) -> Node("ValueDef", [("", formatASTRec expr)])
        | IsEqual(expr1, expr2) -> Node("IsEqual", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | UnionDef(expr1, expr2) -> Node("UnionDef", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | AxiomDef(exprs, expr) -> 
            let exprNodes = List.map (fun node -> ("", formatASTRec node)) exprs
            Node("AxiomDef", [("For All", Node("", exprNodes)); ("Post", formatASTRec expr)])
        | Belongs(expr1, expr2) -> Node("Belongs", [("LHS", formatASTRec expr1); ("RHS", formatASTRec expr2)])
        | SetDef(exprs) ->
            let exprNodes = List.map (fun node -> ("", formatASTRec node)) exprs
            Node("SetDef", [("Exprs", Node("", exprNodes))])
        | Application(expr, args) ->
            /// Formatted arguments with their respective positions
            let argChildren =
                List.map (fun (i, n) -> ($"arg %d{i+1}", formatASTRec n))
                        (List.indexed args)
            Node("Application", [("Expr",formatASTRec expr); ("Args", Node("", argChildren))] )
            // mkTree "Application" node (("Expr", formatASTRec expr) ::
            //                        argChildren)
        | Tuple(exprs) ->
            let exprNodes = List.map (fun node -> ("", formatASTRec node)) exprs
            Node("Tuple", [("Args", Node("", exprNodes))])
            // mkTree "Tuple" node [("Args", exprNodes)]
        | Assign(assignNode) -> 
            match assignNode with 
                | WithBody(name, tpe, body) -> Node("Assign", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe); ("Body", formatASTRec body)])
                | WithoutBody(name, tpe) -> Node("Assign", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe)])
        | ClasseDecl(classNode) ->
            match classNode with
                | TypeNode(nodeArray) -> 
                    let nodeArrayNodes = List.map (fun node -> ("", formatASTRec node)) nodeArray
                    Node("TypeDeclarations", [("TypeDefinitions", Node("", nodeArrayNodes))])
                | ValueNode(nodeArray) -> 
                    let nodeArrayNodes = List.map (fun node -> ("", formatASTRec node)) nodeArray
                    Node("ValueDeclarations", [("ValueDefinitions", Node("", nodeArrayNodes))])
                | AxiomNode(nodeArray) -> 
                    let nodeArrayNodes = List.map (fun node -> ("", formatASTRec node)) nodeArray
                    Node("AxiomDeclarations", [("AxiomDefinitions", Node("", nodeArrayNodes))])
        | TestCase(name, expr) -> Node("TestCase", [("Name", Node(name, [])); ("Expr", formatASTRec expr)])
        | Scheme(name, exprs) ->
            let exprNodes = List.map (fun node -> ("", formatASTRec node)) exprs
            Node("Scheme", [("Name", Node(name, [])); ("Exprs", Node("", exprNodes))])
        | Product(expr1, expr2) -> Node("Product", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | Equate(expr1, expr2) -> 
            let pretypeExprNodes = List.map(fun node -> ("", formatPretypeNode node)) expr1
            Node("Equate", [("Expr1", Node("",pretypeExprNodes)); ("Expr2", formatASTRec expr2)])

and internal formatPretypeNode (node: PretypeNode): Tree =
    match node.Pretype with
    | Pretype.TId(id) ->
        Node((formatPretypeDescr node $"Pretype Id \"%s{id}\""), [])
    | Pretype.TText(text) -> Node("Text", [("", Node(text, []))])
    | Pretype.TSet(elementType, isInf) -> Node("Set", [("", formatPretypeNode elementType); ("IsInf", Node(string isInf, []))])
    | Pretype.TProduct(elementType) ->
        let nodeArgs =
            List.map (fun (i, t) -> ((formatPretypeDescr t $"Type %d{i+1}"),
                                     formatPretypeNode t))
                     (List.indexed elementType)
        Node((formatPretypeDescr node "Product pretype"),nodeArgs)
    | Pretype.TFun(args, ret) ->
        /// Formatted argument pretypes with their respective position
        let argChildren =
            List.map (fun (i, t) -> ((formatPretypeDescr t $"arg %d{i+1}"),
                                     formatPretypeNode t))
                     (List.indexed args)
        Node((formatPretypeDescr node "Function pretype"),
             argChildren @
             [("return", formatPretypeNode ret)])
        // Node((formatPretypeDescr node "Function pretype"),
        //      [ ("", Node("Arg Types", argChildren));
        //        ("", Node("Return types", retChildren))]
        //      )

and internal formatPretypeDescr (node: PretypeNode) (descr: string) : string =
    $"%s{descr}; pos: %s{node.Pos.Format}"

/// Return a description of an AST node, and possibly some subtrees.
and internal formatNodeTypingInfo (node: Node<'E,'T>): List<string * Tree> =
    // let envChildren =
    //     match typeof<'E> with
    //     | t when t = typeof<unit> -> [] // Nothing to show
    //     | t when t = typeof<Typechecker.TypingEnv> ->
    //         formatTypingEnv ((node.Env :> obj) :?> Typechecker.TypingEnv)
    //     | t -> failwith $"BUG: unsupported AST environment type for pretty-printing: %O{t}"
    // let typeChildren =
    //     match typeof<'T> with
    //     | t when t = typeof<unit> -> [] // Nothing to show
    //     | t when t = typeof<Type.Type> ->
    //         [("Type", formatType ((node.Type :> obj) :?> Type.Type))]
    //     | t -> failwith $"BUG: unsupported AST type argument for pretty-printing: %O{t}"
    // envChildren @ typeChildren
    []


/// Return a compact but readable representation of the AST.
let prettyPrintAST<'E,'T> (node: Node<'E,'T>): string =
    let output = (formatASTRec node).ToString()
    let uniqueId = System.Guid.NewGuid().ToString()
    System.IO.File.WriteAllText($"outputs/parser/AST_{uniqueId}.txt", output)
    output
