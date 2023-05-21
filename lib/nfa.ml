open Ppx_sexp_conv_lib
open Conv

module Map = Map.Make (struct
  type t = int

  let compare = Int.compare
end)

module Set = Set.Make(struct
  type t = int * char option

  let compare (i, _) (j, _) = Int.compare i j
end)

module Tbl = struct
  type t = Set.t Map.t [@@deriving compare]

  let pp_set fmt set =
    let pp_sep fmt () = Format.fprintf fmt "," in
    let pp_elt fmt (state, sy) =
      match sy with
      | Some sy -> Format.fprintf fmt "(%i, %c)" state sy
      | None -> Format.fprintf fmt "(%i, <None>)" state
    in
    Format.pp_print_list ~pp_sep pp_elt fmt (Set.to_seq set |> List.of_seq)

  let pp fmt tbl =
    let pp_sep fmt () = Format.fprintf fmt "," in
    let pp_elt fmt (state, moves) =
      Format.fprintf fmt "%i -> {@[<v 2>%a@]}" state pp_set moves
    in
    Format.pp_print_list ~pp_sep pp_elt fmt (Map.to_seq tbl |> List.of_seq)
end

let compare_int = Int.compare

type t = {
  tbl : Tbl.t;
  start : int;
  final : int;
} [@@deriving compare, show]

let epsilon_closure nfa states =
  let rec aux visited acc state =
    match Map.find_opt state nfa.tbl with
    | Some states ->
        let visited = state :: visited in
        let states =
          Set.filter (fun (state, lbl) ->
            not (List.mem state visited) && Option.is_none lbl) states
        in
        Set.fold (fun (state, _) (visited, acc) -> aux visited acc state)
          states (visited, Set.union states acc)
    | None -> (visited, acc)
  in
  Set.fold (fun (state, _) (visited, acc) -> aux visited acc state) states
    ([], states)
  |> snd

let move nfa states c = Set.fold (fun (state, _) acc ->
    match Map.find_opt state nfa.tbl with
    | Some states_ ->
        begin
          Set.filter (fun (_, sy) ->
            match sy with
            | Some d -> Char.equal c d
            | None -> false) states_
          |> Set.union acc
        end
    | None -> acc
  ) states Set.empty

let match_string nfa str =
  let s = epsilon_closure nfa (Set.singleton (nfa.start, None)) in
  String.to_seq str
  |> List.of_seq
  |> List.fold_left (fun states c ->
      epsilon_closure nfa (move nfa states c)) s
  |> Set.find_first_opt (fun (s, _) -> s = nfa.final)
  |> Option.is_some

let compile regexp =
  let rec aux acc = function
    | Regexp.BinaryOp (e1, Union, e2) ->
        let t1, acc = aux acc e1 in
        let t2, start = aux acc e2 in
        let final = start + 1 in
        let tbl = Map.(
          union (fun _ _ _ -> assert false) t1.tbl t2.tbl
          |> add start (Set.of_list [t1.start, None; t2.start, None])
          |> add t1.final (Set.singleton (final, None))
          |> add t2.final (Set.singleton (final, None)))
        in
        { tbl; start; final }, final + 1
    | BinaryOp (e1, Concat, e2) ->
        let t1, acc = aux acc e1 in
        let t2, acc = aux acc e2 in
        let tbl = Map.(
          union (fun _ _ _ -> assert false) t1.tbl t2.tbl
          |> add t1.final (Set.singleton (t2.start, None)))
        in
        { tbl; start = t1.start; final = t2.final }, acc + 1
    | UnaryOp (e, Kleene) ->
        let t, start = aux acc e in
        let final = start + 1 in
        let tbl = Map.(
          add start (Set.of_list [t.start, None; final, None]) t.tbl
          |> add t.final (Set.of_list [t.start, None; final, None]))
        in
        { tbl; start; final }, final + 1
    | UnaryOp (e, Pos) ->
        let t, start = aux acc e in
        let final = start + 1 in
        let tbl = Map.(
          add start (Set.singleton (t.start, None)) t.tbl
          |> add t.final (Set.of_list [t.start, None; final, None]))
        in
        { tbl; start; final }, final + 1
    | UnaryOp (e, Opt) ->
        let t, start = aux acc e in
        let final = start + 1 in
        let tbl = Map.(
          add start (Set.of_list [t.start, None; t.final, None]) t.tbl
          |> add t.final (Set.singleton (final, None)))
        in
        { tbl; start; final }, final + 1
    | Symbol c ->
        let start = acc and final = acc + 1 in
        let tbl = Map.(
          empty
          |> add start (Set.singleton (final, Some c)))
        in
        { tbl; start; final }, final + 1
  in
  aux 0 regexp |> fst
