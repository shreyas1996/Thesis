/// Entry point of the RSL TypeChecker program, including the main function.
module Main

open System.IO
/// Tokenize the given file with the given options, and print the result on the
/// terminal. Return 0 in case of success, non-zero otherwise.
let internal tokenize (opt: CmdLine.TokenizerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug $"Parsed command line options:%s{Util.nl}%O{opt}"
    match (Util.lexFile opt.File) with
    | Error(msg) ->
        Log.error $"%s{msg}"; 1 // Non-zero exit code
    | Ok(tokens) ->
        Log.info "Lexing succeeded."
        printfn $"%A{tokens}"
        0 // Success!


/// Parse the given file with the given options, and print the result on the
/// terminal. Return 0 in case of success, non-zero otherwise.
let internal parse (opt: CmdLine.ParserOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug $"Parsed command line options:%s{Util.nl}%O{opt}"
    match (Util.parseFile opt.File) with
    | Error(msg) ->
        let errorPath = Path.Combine("outputs", "error", "parser", Path.GetFileNameWithoutExtension(opt.File) + ".txt")
        System.IO.File.WriteAllText(errorPath, msg.ToString())
        Log.error $"%s{msg}"; 1 // Non-zero exit code
    | Ok(ast) ->
        Log.info "Lexing and parsing succeeded."
        let prettyAst = PrettyPrinter.prettPrint ast
        let outputPath = Path.Combine("outputs", "parser", Path.GetFileNameWithoutExtension(opt.File) + ".txt")
        System.IO.File.WriteAllText(outputPath, prettyAst.ToString())
        // printfn $"%s{prettyAst.ToString()}"
        Log.info $"AST saved to {outputPath}"
        0 // Success!

let internal typecheck (opt: CmdLine.TypecheckerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug $"Parsed command line options:%s{Util.nl}%O{opt}"
    match (Util.parseFile opt.File) with
    | Error(msg) ->
        Log.error $"%s{msg}"; 1 // Non-zero exit code
    | Ok(ast) ->
        Log.info "Lexing and parsing succeeded."
        // match (Typechecker.typeCheckAST ast) with
        // | Error(typErrs) ->
        //     // for posErr in typErrs do
        //     //     Log.error (Util.formatMsg posErr)
        //     Log.error $"%s{typErrs}"; 1 // Non-zero exit code
        //     1 // Non-zero exit code
        // | Ok(tast) ->
        //     Log.info "Type checking succeeded."
        //     0 // Success!
        let typecheckerResult = Typechecker.typeCheckAST ast
        printfn $"Typechecking Result: %A{typecheckerResult}"
        0 // Success!


[<EntryPoint>]
let main (args: string[]): int =
    match (CmdLine.parse args) with
    | CmdLine.ParseResult.Error(exitCode) -> exitCode // Non-zero exit code
    | CmdLine.ParseResult.Tokenize(opts) -> tokenize opts
    | CmdLine.ParseResult.Parse(opts) -> parse opts
    | CmdLine.ParseResult.Typecheck(opts) -> typecheck opts