(** This is the entry point for execution of Dwh programs. *)

val compile_program_ir :
    ?should_pprint_past:bool
    -> Lexing.lexbuf
    -> unit
