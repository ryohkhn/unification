type binop =
  | Plus
  | Minus
  | Times
  | Div
[@@deriving eq, ord, show]

type projection =
  | First
  | Second
[@@deriving eq, ord, show]

type t =
  | Var of Identifier.t
  | IntConst of int
  | Binop of t * binop * t
  | Pair of t * t
  | Proj of projection * t
  | Fun of Identifier.t * t
  | App of t * t
[@@deriving eq, ord, show]
