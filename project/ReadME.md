# RSL TypeChecker

This project is an RSL (Relational Specification Language) TypeChecker. It provides command-line utilities to tokenize, parse, and typecheck RSL source files.

## Prerequisites

- .NET 6.0 SDK
- Bash shell (for running the `run_tests.sh` script)

## Building the Project

To build the project, run the following command:

```sh
dotnet build
```
## Running the TypeChecker
The entry point for executing commands is the ./myrsl script. Below are the available commands and their usage.

### Tokenize
To tokenize a given input source file, use the tokenize command:
``` sh
./myrsl tokenize <input file> [-l <log-level>] [-v]
```
- <input file>: Source code file to be tokenized (required).
- -l, --log-level: Set the log level. Valid values: debug, info, warning, error. Default is warning.
- -v, --verbose: Enable verbose output. This has the same effect as using the --log-level debug option.

### Parse
To parse a given input source file, use the parse command:
```sh
./myrsl parse <input file> [-l <log-level>] [-v]
```
- <input file>: Source code file to be parsed (required).
- -l, --log-level: Set the log level. Valid values: debug, info, warning, error. Default is warning.
- -v, --verbose: Enable verbose output. This has the same effect as using the --log-level debug option.

### Typecheck
To typecheck a given input source file, use the typecheck command:
```sh
./myrsl typecheck <input file> [-l <log-level>] [-v]
```
- <input file>: Source code file to be typechecked (required).
- -l, --log-level: Set the log level. Valid values: debug, info, warning, error. Default is warning.
- -v, --verbose: Enable verbose output. This has the same effect as using the --log-level debug option.

## Running Tests
To run tests on .rsl files in a directory, use the run_tests.sh script:
```sh
./run_tests.sh <directory>
```
- <directory>: Directory containing .rsl files to be tested.

The script will iterate over all .rsl files in the specified directory and execute the ./myrsl parse command on each file.

License
This project is licensed under the terms of the [LICENSE](./LICENSE) file.