open Core
open Lex_and_parser

let maybe_pprint_ast should_pprint_ast pprintfun ast =
    if should_pprint_ast then (
        pprintfun Fmt.stdout ast ;
        Error (Error.of_string ""))
    else Ok ast

let compile_program_ir ?(should_pprint_past = false) lexbuf =
    let open Result in
    parse_program lexbuf
    >>= maybe_pprint_ast should_pprint_past pprint_parsed_ast
    |> function
    | Ok program -> ()
    | Error e    -> eprintf "%s" (Core.Error.to_string_hum e)
