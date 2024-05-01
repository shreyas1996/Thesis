/// Entry point of the RSL TypeChecker program, including the main function.
module Main

/// Tokenize the given file with the given options, and print the result on the
/// terminal. Return 0 in case of success, non-zero otherwise.
let internal tokenize (opt: CmdLine.TokenizerOptions): int =
    Log.setLogLevel opt.LogLevel
    if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
    Log.debug $"Parsed command line options:%s{RSLUtil.nl}%O{opt}"
    match (RSLUtil.lexFile opt.File) with
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
    Log.debug $"Parsed command line options:%s{RSLUtil.nl}%O{opt}"
    match (RSLUtil.parseFile opt.File) with
    | Error(msg) ->
        Log.error $"%s{msg}"; 1 // Non-zero exit code
    | Ok(ast) ->
        Log.info "Lexing and parsing succeeded."
        printf $"%s{RSLPrettyPrinter.prettyPrintAST ast}"
        0 // Success!

// let internal typecheck (opt: CmdLine.TypecheckerOptions): int =
//     Log.setLogLevel opt.LogLevel
//     if opt.Verbose then Log.setLogLevel Log.LogLevel.debug
//     Log.debug $"Parsed command line options:%s{RSLUtil.nl}%O{opt}"
//     match (RSLUtil.parseFile opt.File) with
//     | Error(msg) ->
//         Log.error $"%s{msg}"; 1 // Non-zero exit code
//     | Ok(ast) ->
//         Log.info "Lexing and parsing succeeded."
//         match (Typechecker.typeCheckAST ast) with
//         | Error(typErrs) ->
//             for posErr in typErrs do
//                 Log.error (RSLUtil.formatMsg posErr)
//             1 // Non-zero exit code
//         | Ok(tast) ->
//             Log.info "Type checking succeeded."
//             printf $"%s{RSLPrettyPrinter.prettyPrint tast}"
//             0 // Success!


[<EntryPoint>]
let main (args: string[]): int =
    match (CmdLine.parse args) with
    | CmdLine.ParseResult.Error(exitCode) -> exitCode // Non-zero exit code
    | CmdLine.ParseResult.Tokenize(opts) -> tokenize opts
    | CmdLine.ParseResult.Parse(opts) -> parse opts