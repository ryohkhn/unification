type t
[@@deriving eq, ord, show]

val fresh : unit -> t

val to_string : t -> string
