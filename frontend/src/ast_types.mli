type loc = Lexing.position

(** An abstract type for identifiers *)
module type ID = sig
    type t
    
    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
end

module Var_name : ID

type modifier = MConst (** Immutable *) | MVar (** Mutable *)


(** Define types of expressions in Dwh programs *)
type type_expr =
    | TEInt
    | TEVoid
    | TEBool

type bin_op =
    | BinOpPlus
    | BinOpMinus
    | BinOpMult
    | BinOpDiv

