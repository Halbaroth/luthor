type t [@@deriving compare, show]

val compile : Regexp.t -> t
val match_string : t -> string -> bool
