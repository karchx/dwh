open Core
open Core_unix
open Compile_program_ir

let dwh_file =
    Command.Arg_type.create (fun filename ->
        if not (String.is_suffix filename ~suffix: ".dwh") then begin
            eprintf "'%s' is not a dwh file. Hint: use the .dwh extension\n%!" filename;
            exit 1
        end;

        match Sys_unix.is_file ~follow_symlinks:true filename with
        | `Yes -> filename
        | `No ->
            eprintf "File '%s' not found\n%!" filename;
            exit 1
        | `Unknown ->
            eprintf "Unknow error file: '%s'%!" filename;
            exit 1
    )

let command =
    Command.basic ~summary:"Run dwh pipelines data"
        ~readme:(fun () -> "A list of execution options")
        Command.Let_syntax.(
            let%map_open should_pprint_past =
                flag "-print-parsed-ast" no_arg ~doc:"Pretty print the parsed AST of the program"
            and filename = anon (maybe_with_default "-" ("filename" %: dwh_file)) in
            fun () ->
                In_channel.with_file filename ~f:(fun file_ic ->
                    let lexbuf =
                        Lexing.from_channel file_ic in
                    compile_program_ir lexbuf ~should_pprint_past))

let () = Command_unix.run ~version:"1.0" ~build_info:"RWO" command

