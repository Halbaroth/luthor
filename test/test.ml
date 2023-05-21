let printf = Format.printf

open Base
open Luthor
module Lexer = Regexp.Lexer
module Parser = Regexp.Parser

(* let eval str = emit str |> Ast.eval *)

let scan str =
  let open Lexer in
  scan str
  |> Stdlib.Seq.map (fun { Regexp.data; _ } -> data)
  |> Stdlib.List.of_seq

let emit str = Lexer.scan str |> Parser.emit

let%test_unit "escaped" =
  [%test_eq: Lexer.token list] (scan {|\?|}) [CHAR '?']

let%test_unit "escaped" =
  [%test_eq: Lexer.token list] (scan {|a\b|}) [CHAR 'a'; CHAR 'b']

let%test_unit "escaped" =
  [%test_eq: Lexer.token list] (scan {|a\\b|})
    [CHAR 'a'; CHAR '\\'; CHAR 'b']

let%test_unit "escaped" =
  [%test_eq: Lexer.token list] (scan {|a\??b|})
    [CHAR 'a'; CHAR '?'; QMARK; CHAR 'b']

let%test_unit "escaped" =
  [%test_eq: Lexer.token list] (scan {|a\?c?b|})
    [CHAR 'a'; CHAR '?'; CHAR 'c'; QMARK; CHAR 'b']

let%test_unit "scan" =
  [%test_eq: Lexer.token list] (scan {|ab*(a\\a)?|(abb)|})
  [CHAR 'a'; CHAR 'b'; STAR; LPAR; CHAR 'a'; CHAR '\\'; CHAR 'a'; RPAR;
  QMARK; BAR; LPAR; CHAR 'a'; CHAR 'b'; CHAR 'b'; RPAR]

let%test_unit "parse a|c" =
  [%test_eq: Regexp.t] (emit "a|c")
  (BinaryOp (
    Symbol 'a',
    Union,
    Symbol 'c'))

let%test_unit "parse a+|c" =
  [%test_eq: Regexp.t] (emit "a+|c")
  (BinaryOp
    (UnaryOp (
      Symbol 'a',
      Pos),
    Union,
    Symbol 'c'))

let%test_unit "parse (a)|c" =
  [%test_eq: Regexp.t] (emit "(a)|c")
  (BinaryOp (
    Symbol 'a',
    Union,
    Symbol 'c'))

let%test_unit "parse (ab)+b*|c" =
 [%test_eq: Regexp.t] (emit "(ab)+b*|c")
 (BinaryOp
   (BinaryOp (
     (UnaryOp (
       BinaryOp (
         Symbol 'a',
         Concat,
         Symbol 'b'
       ),
       Pos)),
     Concat,
     (UnaryOp (
       Symbol 'b',
       Kleene))),
   Union,
   (Symbol 'c')))

let compile str = Nfa.compile (Lexer.scan str |> Parser.emit)

let%test_unit "match a+b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "a+b|c") "ab") true

let%test_unit "match a+b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "a+b|c") "c") true

let%test_unit "match a+b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "a+b|c") "bc") false

let%test_unit "match a*b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "a*b|c") "b") true

let%test_unit "match a+b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "a+b|c") "ab") true

let%test_unit "match a+b|c" =
 [%test_eq: bool] (Nfa.match_string (compile "foo|(bar)*") "barbar") true
