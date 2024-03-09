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

/// Traverse an RSL AST node and return its hierarchical representation.
let rec internal formatASTRec (node: UntypedAST): Tree =
    match node.Expr with
        | UnitVal -> Node("UnitVal", [])
        | BoolVal(value) -> Node("BoolVal", [("", Node(string value, []))])
        | IntVal(value) -> Node("IntVal", [("", Node(string value, []))])
        | FloatVal(value) -> Node("FloatVal", [("", Node(string value, []))])
        | StringVal(value) -> Node("StringVal", [("", Node(value, []))])
        | Var(name) -> Node("Var", [("", Node(name, []))])
        | ValList(exprs) ->
            let exprNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) exprs
            Node("ValList", [("Exprs", Node("", exprNodes))])
        | Add(expr1, expr2) -> Node("Add", [("LHS", formatASTRec expr1); ("RHS", formatASTRec expr2)])
        | Sub(expr1, expr2) -> Node("Sub", [("LHS", formatASTRec expr1); ("RHS", formatASTRec expr2)])
        | Exponent(expr1, expr2) -> Node("Exponent", [("Base", formatASTRec expr1); ("Power", formatASTRec expr2)])
        | TP(expr) -> 
            // let tpExprs = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) expr
            Node("TP", [("", formatASTRec expr)])
        | Yenda(expr) -> 
            let tpExprs = List.map (fun (node: PretypeNode) -> ("", formatPretypeNode node)) expr
            Node("Yenda", [("", Node("", tpExprs))])
        | Yenda2(expr) -> 
            // let tpExprs = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) expr
            Node("Yenda2", [("Yenda2", formatPretypeNode expr)])
        | TypeDef(name, tpe) -> Node("TypeDef", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe)])
        | ClassDef(name, typeDefs, valueDefs) ->
            let typeDefNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) typeDefs
            let valueDefNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) valueDefs
            Node("ClassDef", [("Name", Node(name, [])); ("TypeDefs", Node("", typeDefNodes)); ("ValueDefs", Node("", valueDefNodes))])
        | ValueDef(expr) -> Node("ValueDef", [("", formatASTRec expr)])
        | IsEqual(expr1, expr2) -> Node("IsEqual", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | UnionDef(expr1, expr2) -> Node("UnionDef", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | SetDef(exprs) ->
            let exprNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) exprs
            Node("SetDef", [("Exprs", Node("", exprNodes))])
        | Application(expr, args) ->
            /// Formatted arguments with their respective positions
            let argChildren =
                List.map (fun (i, n) -> ($"arg %d{i+1}", formatASTRec n))
                        (List.indexed args)
            Node("Application", [("Expr",formatASTRec expr); ("Args", Node("", argChildren))] )
        | Tuple(exprs) ->
            let exprNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) exprs
            Node("Tuple", [("Args", Node("", exprNodes))])
        | Assign(name, tpe, body) -> Node("Assign", [("Name", Node(name, [])); ("Pretype", formatPretypeNode tpe); ("Body", formatASTRec body)])
        | TestCase(name, expr) -> Node("TestCase", [("Name", Node(name, [])); ("Expr", formatASTRec expr)])
        | Scheme(name, exprs) ->
            let exprNodes = List.map (fun (node: UntypedAST) -> ("", formatASTRec node)) exprs
            Node("Scheme", [("Name", Node(name, [])); ("Exprs", Node("", exprNodes))])
        | Product(expr1, expr2) -> Node("Product", [("Expr1", formatASTRec expr1); ("Expr2", formatASTRec expr2)])
        | Equate(expr1, expr2) -> 
            let pretypeExprNodes = List.map(fun (node: PretypeNode) -> ("", formatPretypeNode node)) expr1
            Node("Equate", [("Expr1", Node("",pretypeExprNodes)); ("Expr2", formatASTRec expr2)])

and internal formatPretypeNode (node: PretypeNode): Tree =
    match node.Pretype with
    | Pretype.TId(id) ->
        Node((formatPretypeDescr node $"Pretype Id \"%s{id}\""), [])
    | Pretype.TText(text) -> Node("Text", [("", Node(text, []))])
    | Pretype.TSet(elementType) -> Node("Set", [("", formatPretypeNode elementType)])
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

and internal formatPretypeDescr (node: PretypeNode) (descr: string) : string =
    $"%s{descr}; pos: %s{node.Pos.Format}"

/// Return a description of an AST node, and possibly some subtrees.
and internal formatNodeTypingInfo (node: UntypedAST): List<string * Tree> =
    // No typing information in RSL AST
    []

/// Return a compact but readable representation of the AST.
let prettyPrintAST (node: UntypedAST): string =
    let output = (formatASTRec node).ToString()
    let uniqueId = System.Guid.NewGuid().ToString()
    System.IO.File.WriteAllText($"outputs/AST_{uniqueId}.txt", output)
    output
