// AST.fs
module AST

type Position =
    {
        FileName: string
        Line: int
        Col: int
        LineStart: int
        ColStart: int
        LineEnd: int
        ColEnd: int
    }
    with
        member this.Format =
           $"(%d{this.LineStart}:%d{this.ColStart}-%d{this.LineEnd}:%d{this.ColEnd})"

type Node<'C> =
    {
        Pos: Position
        NodeCategory: 'C
    }

type FunctionDefOperator =
    | Is

type FunctionDeclOperator =
    | Arrow
    | PartialArrow

type PrefixOperator =
    | Abs

type PrefixConnective =
    | Not

type InfixOperator =
    | Equals
    | NotEquals
    | LessThan
    | GreaterThan
    | LessOrEqual
    | GreaterOrEqual
    | Add
    | Subtract
    | Multiply
    | Divide

type InfixConnective =
    | And
    | Or
    | Implies

type Quantifiers =
    | Forall
    | Exists

type ValueLiteral =
    | Bool of bool
    | Int of int

type TypeLiteral =
    | Bool
    | Int

type TypeName =
    {
        name: string

    }

type TypeExpr =
    | TypeLiteral of Node<TypeLiteral>
    | TypeName of Node<TypeName>
    | SubtypeExpr of Node<SubtypeExpr>
    | BracketedTypeExpr of Node<BracketedTypeExpr>

and SubtypeExpr =
    {
        singleTyping: Node<SingleTyping>
        valueExpr: Node<ValueExpr>

    }

and SingleTyping =
    {
        name: string
        typeExpr: Node<TypeExpr>

    }

and BracketedTypeExpr =
    {
        typeExpr: Node<TypeExpr>

    }

and ValueExpr =
    | ValueLiteral of Node<ValueLiteral>
    | ValueName of string
    | ApplicationExpr of Node<ApplicationExpr>
    | BracketedExpr of Node<BracketedExpr>
    | QuantifiedExpr of Node<QuantifiedExpr>
    | AxiomInfixExpr of Node<AxiomInfixExpr>
    | ValueInfixExpr of Node<ValueInfixExpr>
    | AxiomPrefixExpr of Node<AxiomPrefixExpr>
    | ValuePrefixExpr of Node<ValuePrefixExpr>
    | LetExpr of Node<LetExpr>
    | IfExpr of Node<IfExpr>

and ApplicationExpr =
    {
        name: string
        args: List<Node<ValueExpr>>

    }

and BracketedExpr =
    {
        valueExpr: Node<ValueExpr>

    }

and QuantifiedExpr =
    {
        quantifier: Quantifiers
        singleTypingList: List<Node<SingleTyping>>
        valueExpr: Node<ValueExpr>

    }

and AxiomInfixExpr =
    {
        leftExpr: Node<ValueExpr>
        infixConnective: InfixConnective
        rightExpr: Node<ValueExpr>

    }

and ValueInfixExpr =
    {
        leftExpr: Node<ValueExpr>
        infixOperator: InfixOperator
        rightExpr: Node<ValueExpr>

    }

and AxiomPrefixExpr =
    {
        prefixConnective: PrefixConnective
        valueExpr: Node<ValueExpr>

    }

and ValuePrefixExpr =
    {
        prefixOperator: PrefixOperator
        valueExpr: Node<ValueExpr>

    }

and LetExpr =
    {
        name: string
        initExpr: Node<ValueExpr>
        scopeExpr: Node<ValueExpr>

    }

and IfExpr =
    {
        condExpr: Node<ValueExpr>
        thenExpr: Node<ValueExpr>
        elseExpr: Node<ValueExpr>

    }

and TypeDef =
    | SortDef of Node<SortDef>
    | AbbrevDef of Node<AbbrevDef>
    | VariantDef of Node<VariantDef>

and VariantDef =
    {
        name: string
        choice: List<string>

    }
and AbbrevDef =
    {
        name: string
        typeExpr: Node<TypeExpr>

    }
and SortDef =
    {
        name: string

    }
and TypeDecl =
    {
        typeDefList: List<TypeDef>

    }
and BodyExpr =
    {
        name: string
        args: List<string>
        functionDefOperator: FunctionDefOperator
        valueExpr: Node<ValueExpr>

    }
and ValueDef =
    | ValueSignature of Node<ValueSignature>
    | ExplicitValueDef of Node<ExplicitValueDef>
    | ExplicitFunctionDef of Node<ExplicitFunctionDef>
and ValueSignature =
    {
        name: string
        typeExpr: Node<TypeExpr>

    }
and ExplicitValueDef =
    {
        name: string
        typeExpr: Node<TypeExpr>
        valueExpr: Node<ValueExpr>

    }
and ExplicitFunctionDef =
    {
        name: string
        args: List<Node<TypeExpr>>
        functionDeclOperator: FunctionDeclOperator
        returnTypeExpr: Node<TypeExpr>
        bodyExpr: Node<BodyExpr>

    }
and ValueDecl =
    {
        valueDefList: List<ValueDef>

    }
and AxiomDef =
    {
        name: string
        logicalValueExpr: Node<ValueExpr>

    }
and AxiomDecl =
    {
        axiomDefList: List<Node<AxiomDef>>

    }

and Decl =
    | TypeDecl of Node<TypeDecl>
    | ValueDecl of Node<ValueDecl>
    | AxiomDecl of Node<AxiomDecl>

and ClassExpr = 
    {
        optDecl: List<Decl>

    }
and SchemeDef = 
    {
        name: string
        classExpr: Node<ClassExpr>

    }
and SchemeDecl = 
    {
        schemeDef: Node<SchemeDef>

    }

// type UntypedExpr = NodeCategories

// type UntypedAST = Node
