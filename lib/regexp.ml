open Ppx_sexp_conv_lib
open Conv

type unary_op = Kleene | Pos | Opt [@@deriving compare, sexp_of]
type binary_op = Union | Concat [@@deriving compare, sexp_of]

let pp_unary_op fmt = function
  | Kleene -> Format.fprintf fmt "*"
  | Pos -> Format.fprintf fmt "+"
  | Opt -> Format.fprintf fmt "?"

let pp_binary_op fmt = function
  | Union -> Format.fprintf fmt "|"
  | Concat -> ()

module Char = struct
  include Char
  let sexp_of_t = sexp_of_char
  let pp fmt = Format.fprintf fmt "%c"
end

type t =
  | Symbol of Char.t
  | BinaryOp of t * binary_op * t
  | UnaryOp of t * unary_op
  [@@deriving compare, sexp_of]

let rec pp fmt = function
  | Symbol c -> Char.pp fmt c
  | BinaryOp (t1, op, t2) ->
      Format.fprintf fmt "(%a)%a(%a)" pp t1 pp_binary_op op pp t2
  | UnaryOp (t, op) ->
      Format.fprintf fmt "(%a)%a" pp t pp_unary_op op

type 'a loc = { data : 'a; pos : int }

module Lexer = struct
  type token =
    | CHAR of Char.t
    | STAR
    | QMARK
    | BAR
    | PLUS
    | LPAR
    | RPAR
    [@@deriving show, compare, sexp_of]

  exception SyntaxError of int

  let simple_tokens = [
    ('*',  STAR);
    ('?',  QMARK);
    ('|',  BAR);
    ('+',  PLUS);
    ('(',  LPAR);
    (')',  RPAR)
  ]

  let is_simple_lexeme c =
    List.find_opt (fun (l, _) -> c = l) simple_tokens
    |> Option.is_some

  let scan str =
    let seq = String.to_seqi str in
    let rec scan ~escaped seq () =
      match Seq.uncons seq with
      | Some (lc, tl) ->
          begin match lc with
          | _, ' ' -> scan ~escaped:false tl ()
          | pos, c when escaped ->
              Seq.cons { data = CHAR c; pos } (scan ~escaped:false tl) ()
          | pos, c when is_simple_lexeme c ->
              let token = List.assoc c simple_tokens in
              Seq.cons { data = token; pos } (scan ~escaped:false tl) ()
          | _, '\\' -> scan ~escaped:true tl ()
          | pos, c ->
              Seq.cons { data = CHAR c; pos } (scan ~escaped tl) ()
          end
      | None -> Seq.empty ()
    in
    scan ~escaped:false seq
end

(* Predictive parser based on the following grammar:
    expr   -> term R
    R      -> '|' term R | term R | e
    term   -> group '*' | group '+' | group '?' | group
    group  -> alpha | '(' expr ')'
  where e stands for the empty string and
  alpha stands for printable characters class. *)
module Parser = struct
  exception ParsingError of int option * string

  (* let raise_error ?pos msg =
    Format.kasprintf (fun str -> raise @@ ParsingError (pos, str)) msg
*)
  (* let expect_token token tokens =
    match Seq.uncons tokens with
    | Some ({ data; _ }, tl) when Lexer.compare_token token data = 0 -> tl
    | Some ({ data; pos }, _) ->
        raise_error ~pos "Token %a expected"
          Lexer.pp_token token pos Lexer.pp_token data
    | None ->
        raise_error "Token %a expected, got nothing"
          Lexer.pp_token token
*)
  let rec emit_r acc tokens =
    match Seq.uncons tokens with
    | Some ({ data = Lexer.BAR; _ }, tl) ->
      let term, tl = emit_term tl in
      emit_r (BinaryOp (acc, Union, term)) tl
    | None -> acc, Seq.empty
    | Some ({ data = Lexer.RPAR; _ }, tl) ->
        acc, tl
    | _ ->
      let term, tl = emit_term tokens in
      emit_r (BinaryOp (acc, Concat, term)) tl

  and emit_group tokens =
    match Seq.uncons tokens with
    | Some ({ data = Lexer.CHAR c; _ }, tl) ->
        Symbol c, tl
    | Some ({ data = LPAR; _ }, tl) ->
        emit_expr tl
(*         expr, expect_token RPAR tl *)
    | _ -> assert false

  and emit_term tokens =
    let group, tokens = emit_group tokens in
    match Seq.uncons tokens with
    | Some ({ data = Lexer.PLUS; _ }, tl) ->
        UnaryOp (group, Pos), tl
    | Some ({ data = QMARK; _ }, tl) ->
        UnaryOp (group, Opt), tl
    | Some ({ data = STAR; _ }, tl) ->
        UnaryOp (group, Kleene), tl
    | _ -> group, tokens

  and emit_expr tokens =
    let term, tokens = emit_term tokens in
    emit_r term tokens

  let emit tokens =
    let res, tokens = emit_expr tokens in
    assert (Seq.is_empty tokens);
    res
end
