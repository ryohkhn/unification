type t =
  | Var of Identifier.t
  | Int
  | Product of t * t
  | Arrow of t * t
[@@deriving eq, ord, show]
