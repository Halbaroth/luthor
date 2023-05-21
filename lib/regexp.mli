type unary_op = Kleene | Pos | Opt [@@deriving compare, sexp_of]
type binary_op = Union | Concat [@@deriving compare, sexp_of]

type t =
  | Symbol of char
  | BinaryOp of t * binary_op * t
  | UnaryOp of t * unary_op
  [@@deriving compare, sexp_of]

val pp : Format.formatter -> t -> unit

type 'a loc = { data : 'a; pos : int }

module Lexer : sig
  type token =
    | CHAR of char
    | STAR
    | QMARK
    | BAR
    | PLUS
    | LPAR
    | RPAR
    [@@deriving show, compare, sexp_of]

  exception SyntaxError of int

  val scan : string -> token loc Seq.t
end

module Parser : sig
  exception ParsingError of int option * string

  val emit : Lexer.token loc Seq.t -> t
end
