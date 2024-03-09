
// rslc - The compiler for the RSL programming language.
// Copyright (C) [Year] [Your Name]
// Released under the MIT license (see LICENSE.md for details)

/// Definition of a type in the rslc compiler.
module Type

/// Representation of a type. This is essentially the abstract syntax tree of a
/// type as it appears in an RSL program.
type Type =
    /// Boolean type.
    | BoolType
    /// Integer type.
    | IntType
    /// Floating-point type (single-precision).
    | FloatType
    /// String type.
    | StringType
    /// Unit type.
    | UnitType
    /// Type variable.
    | VarType of name: string
    /// Function type, with argument types and return type.
    /// Set type, representing a collection of elements of a certain type.
    | SetType of elementType: Type
    /// Class type, representing a class definition.
    | ClassType of className: string
    /// Test case type, representing a test case definition.
    | TestCaseType of testCaseName: string
    // Add other types as needed

    /// Returns a human-readable string describing the type.
    override this.ToString(): string =
        match this with
        | BoolType -> "bool"
        | IntType -> "int"
        | FloatType -> "float"
        | StringType -> "string"
        | UnitType -> "unit"
        | VarType(name) -> name
        | SetType(elementType) -> sprintf "set of %s" (string elementType)
        | ClassType(className) -> sprintf "class %s" className
        | TestCaseType(testCaseName) -> sprintf "test case %s" testCaseName
        // Add cases for other types

/// List of basic types known by the compiler.
let basicTypes = [BoolType; IntType; FloatType; StringType; UnitType]

/// Set of free type variables in a type.
let rec freeTypeVars (t: Type): Set<string> =
    match t with
    | BoolType
    | IntType
    | FloatType
    | StringType
    | UnitType -> Set.empty
    | VarType(name) -> Set.singleton name
    | SetType(elementType) -> freeTypeVars elementType
    | ClassType _
    | TestCaseType _ -> Set.empty // Assuming class and test case names are not type variables

