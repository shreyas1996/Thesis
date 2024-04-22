module CmdLine

open CommandLine

/// Command line options for tokenization.
[<Verb("tokenize", HelpText="Tokenize the given input source file.")>]
type TokenizerOptions = {
    [<Value(0, Required=true, MetaName="input file", HelpText="Source code file to be tokenized.")>]
    File: string;

    [<Option('l', "log-level", HelpText="Set the log level. Valid values: debug, info, warning, error. (Default: warning)")>]
    LogLevel: Log.LogLevel;

    [<Option('v', "verbose", HelpText="Enable verbose output. (Same effect of using option '--log-level debug')")>]
    Verbose: bool;
}


/// Command line options for parsing.
[<Verb("parse", HelpText="Parse the given input source file.")>]
type ParserOptions = {
    [<Value(0, Required=true, MetaName="input file", HelpText="Source code file to be parsed.")>]
    File: string;

    [<Option('l', "log-level", HelpText="Set the log level. Valid values: debug, info, warning, error. (Default: warning)")>]
    LogLevel: Log.LogLevel;

    [<Option('v', "verbose", HelpText="Enable verbose output. (Same effect of using option '--log-level debug')")>]
    Verbose: bool;
}

[<Verb("typecheck", HelpText="Typecheck the given input source file.")>]
type TypecheckerOptions = {
    [<Value(0, Required=true, MetaName="input file", HelpText="Source code file to be typechecked.")>]
    File: string;

    [<Option('l', "log-level", HelpText="Set the log level. Valid values: debug, info, warning, error. (Default: warning)")>]
    LogLevel: Log.LogLevel;

    [<Option('v', "verbose", HelpText="Enable verbose output. (Same effect of using option '--log-level debug')")>]
    Verbose: bool;
}

/// Possible result of command line parsing.
[<RequireQualifiedAccess>]
type ParseResult =
    | Error of int
    | Tokenize of TokenizerOptions
    | Parse of ParserOptions
    | Typecheck of TypecheckerOptions


/// Parse the command line.  If successful, return the parsed options; otherwise,
/// return a non-zero integer usable as exit code.
let parse (args: string[]): ParseResult =
    let res = CommandLine.Parser.Default.ParseArguments<TokenizerOptions,
                                                        ParserOptions>(args);

    match res with
    | :? NotParsed<obj> ->
        ParseResult.Error(1) // Non-zero exit code
    | :? Parsed<obj> as parsed ->
        match parsed.Value with
        | :? TokenizerOptions as opt -> ParseResult.Tokenize(opt)
        | :? ParserOptions as opt -> ParseResult.Parse(opt)
         | :? TypecheckerOptions as opt -> ParseResult.Typecheck(opt)
        | x -> failwith $"BUG: unexpected command line parsed value: %O{x}"
    | x -> failwith $"BUG: unexpected command line parsing result: %O{x}"