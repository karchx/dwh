open Base

type loc = Lexing.position

module type ID = sig
    type t

    val of_string : string -> t
    val to_string : t -> string
    val ( = ) : t -> t -> bool
end

module String_id = struct
    type t = string

    let of_string x = x
    let to_string x = x

    let ( = ) = String.(=)
end

module Var_name : ID = String_id

type modifier = MConst | MVar

type type_expr =
    | TEInt
    | TEVoid
    | TEBool

type bin_op =
    | BinOpPlus
    | BinOpMinus
    | BinOpMult
    | BinOpDiv

