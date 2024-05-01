//
module Type

open AST


type TypeExpr =
    | TypeLiteral of TypeLiteral
    | TypeName of TypeName
    | SubtypeExpr of SubtypeExpr
    | BracketedTypeExpr of BracketedTypeExpr

and SubtypeExpr =
    {
        singleTyping: SingleTyping
        valueExpr: ValueExpr
    }

and SingleTyping =
    {
        name: string
        typeExpr: TypeExpr
    }

and BracketedTypeExpr =
    {
        typeExpr: TypeExpr
    }

and ValueExpr =
    | ValueLiteral of ValueLiteral
    | ValueName of string
    | ApplicationExpr of ApplicationExpr
    | BracketedExpr of BracketedExpr
    | QuantifiedExpr of QuantifiedExpr
    | AxiomInfixExpr of AxiomInfixExpr
    | ValueInfixExpr of ValueInfixExpr
    | AxiomPrefixExpr of AxiomPrefixExpr
    | ValuePrefixExpr of ValuePrefixExpr
    | LetExpr of LetExpr
    | IfExpr of IfExpr

and ApplicationExpr =
    {
        name: string
        args: List<ValueExpr>
    }

and BracketedExpr =
    {
        valueExpr: ValueExpr
    }

and QuantifiedExpr =
    {
        quantifier: Quantifiers
        singleTypingList: List<SingleTyping>
        valueExpr: ValueExpr
    }

and AxiomInfixExpr =
    {
        leftExpr: ValueExpr
        infixConnective: InfixConnective
        rightExpr: ValueExpr
    }

and ValueInfixExpr =
    {
        leftExpr: ValueExpr
        infixOperator: InfixOperator
        rightExpr: ValueExpr
    }

and AxiomPrefixExpr =
    {
        prefixConnective: PrefixConnective
        valueExpr: ValueExpr
    }

and ValuePrefixExpr =
    {
        prefixOperator: PrefixOperator
        valueExpr: ValueExpr
    }

and LetExpr =
    {
        name: string
        initExpr: ValueExpr
        scopeExpr: ValueExpr
    }

and IfExpr =
    {
        condExpr: ValueExpr
        thenExpr: ValueExpr
        elseExpr: ValueExpr
    }

and TypeDef =
    | SortDef of SortDef
    | AbbrevDef of AbbrevDef
    | VariantDef of VariantDef

and VariantDef =
    {
        name: string
        choice: List<string>
    }

and AbbrevDef =
    {
        name: string
        typeExpr: TypeExpr
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
        valueExpr: ValueExpr
    }

and ValueDef =
    | ValueSignature of ValueSignature
    | ExplicitValueDef of ExplicitValueDef
    | ExplicitFunctionDef of ExplicitFunctionDef

and ValueSignature =
    {
        name: string
        typeExpr: TypeExpr
    }

and ExplicitValueDef =
    {
        name: string
        typeExpr: TypeExpr
        valueExpr: ValueExpr
    }

and ExplicitFunctionDef =
    {
        name: string
        args: List<TypeExpr>
        functionDeclOperator: FunctionDeclOperator
        returnTypeExpr: TypeExpr
        bodyExpr: BodyExpr
    }

and ValueDecl =
    {
        valueDefList: List<ValueDef>
    }

and AxiomDef =
    {
        name: Option<string>
        logicalValueExpr: ValueExpr
    }

and AxiomDecl =
    {
        axiomDefList: List<AxiomDef>
    }

and Decl =
    {
        TypeDecl: Option<TypeDecl>
        ValueDecl: Option<ValueDecl>
        AxiomDecl: Option<AxiomDecl>
    }

and ClassExpr = 
    {
        optDecl: Option<Decl>
    }

and SchemeDef = 
    {
        name: string
        classExpr: ClassExpr
    }

and SchemeDecl = 
    {
        schemeDef: SchemeDef
    }

type NodeTypes =
    | SchemeDeclResult of SchemeDecl
    | SchemeDefResult of SchemeDef
    | ClassExprResult of ClassExpr
    | DeclResult of Decl
    | TypeDeclResult of TypeDecl
    | ValueDeclResult of ValueDecl
    | AxiomDeclResult of AxiomDecl
    | AxiomDefResult of AxiomDef
    | ValueDefResult of ValueDef
    | ExplicitFunctionDefResult of ExplicitFunctionDef
    | ExplicitValueDefResult of ExplicitValueDef
    | ValueSignatureResult of ValueSignature
    | BodyExprResult of BodyExpr
    | SortDefResult of SortDef
    | AbbrevDefResult of AbbrevDef
    | VariantDefResult of VariantDef
    | TypeDefResult of TypeDef
    | TypeExprResult of TypeExpr
    | SubtypeExprResult of SubtypeExpr
    | SingleTypingResult of SingleTyping
    | BracketedTypeExprResult of BracketedTypeExpr
    | ValueExprResult of ValueExpr
    | ApplicationExprResult of ApplicationExpr
    | BracketedExprResult of BracketedExpr
    | QuantifiedExprResult of QuantifiedExpr
    | AxiomInfixExprResult of AxiomInfixExpr
    | ValueInfixExprResult of ValueInfixExpr
    | AxiomPrefixExprResult of AxiomPrefixExpr
    | ValuePrefixExprResult of ValuePrefixExpr
    | LetExprResult of LetExpr
    | IfExprResult of IfExpr
    | TypeLiteralResult of AST.TypeLiteral
    | TypeNameResult of AST.TypeName
    | ValueLiteralResult of AST.ValueLiteral

