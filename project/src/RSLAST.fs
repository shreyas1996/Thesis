//
module AST

[<RequireQualifiedAccess>]
type Position =
    { /// The name of the file being parsed.
      FileName: string
      /// "Main" line of the AST elements, used e.g. to report typing errors.
      Line: int
      /// "Main" column of the AST elements, used e.g. to report typing errors.
      Col: int
      /// Line where the AST element starts.
      LineStart: int
      /// Column where the AST element starts.
      ColStart: int
      /// Line where the AST element ends.
      LineEnd: int
      /// Column where the AST element ends.
      ColEnd: int
    }
    with
        /// Return a comoact string representation of a position in the input
        /// source file.
        member this.Format =
           $"(%d{this.LineStart}:%d{this.ColStart}-%d{this.LineEnd}:%d{this.ColEnd})"

[<RequireQualifiedAccess>]
type PretypeNode = {
    Pos: Position
    Pretype: Pretype
}

and Pretype =
    | TId of id: string
    | TText of string
    | TSet of elementType: PretypeNode
    | TProduct of elementType: List<PretypeNode>
    /// A function pretype, with argument pretypes and return pretype.
    | TFun of args: List<PretypeNode>
        * ret: PretypeNode

[<RequireQualifiedAccess>]
type Node<'E, 'T> = 
    { /// Hygge expression contained in the AST node.
      Expr: Expr<'E,'T>
      /// Position in the source file of the expression in this AST node.
      Pos: Position
      /// Typing environment used to type-check the expression in this AST node.
      Env: 'E
      /// Type assigned to the expression in this AST node.
      Type: 'T
    }

and Expr<'E,'T> =
     /// Unit value.
    | UnitVal

    /// Integer value.
    | BoolVal of value: bool

    /// Integer value.
    | IntVal of value: int

    /// Floating-point constant (single-precision, a.k.a. float32).
    | FloatVal of value: single

    /// String value.
    | StringVal of value: string

    /// Variable name.
    | Var of name: string

    // Value List

    | ValList of List<Node<'E, 'T>>
    
    // | TSet of Node<'E, 'T>
    | TypeDef of string 
        * tpe: PretypeNode
        // * scope: Node<'E, 'T>
        // * scope: string

    | ClassDef of string 
        * List<Node<'E, 'T>>
        * List<Node<'E, 'T>> 
        // * Option<List<Node<'E, 'T>>>

    | ValueDef of Node<'E, 'T> 

    | SetDef of List<Node<'E, 'T>>

    | UnionDef of Node<'E, 'T> 
        * Node<'E, 'T>
    
    | Add of 
        lhs: Node<'E, 'T> 
        * rhs: Node<'E, 'T>
    
    | Sub of 
        lhs: Node<'E, 'T> 
        * rhs: Node<'E, 'T>

    | Exponent of 
        lhs: Node<'E, 'T> 
        * rhs: Node<'E, 'T>

    | TP of Node<'E, 'T>
    | Yenda of List<PretypeNode>
    | Yenda2 of PretypeNode

    | Assign of string
        * tpe: PretypeNode
        // * tpe: Node<'E, 'T>
        * Body: Node<'E,'T>
    
    // | Let of name: string
    //     * tpe: PretypeNode
    //     * init: Node<'E,'T>
    //     * scope: Node<'E,'T>
    | Application of expr: Node<'E,'T>
        * args: List<Node<'E,'T>>
    | Tuple of args: List<Node<'E,'T>>

    | IsEqual of 
        lhs: Node<'E,'T>
        * rhs: Node<'E,'T>

    | TestCase of string 
        * Node<'E, 'T>

    | Scheme of string 
        * List<Node<'E, 'T>>

    | Product of Node<'E, 'T> * Node<'E, 'T>

    | Equate of List<PretypeNode>  * Node<'E, 'T>
    // Add other expressions and definitions as necessary
// Adapt additional expressions and constructs as needed for RSL

/// A type alias for an untyped AST, where there is no typing environment nor
/// typing information (unit).
type UntypedAST = Node<unit, unit>


/// A type alias for an untyped expression within an untyped AST, where there is
/// no typing environment nor typing information (unit).
type UntypedExpr = Expr<unit, unit>
