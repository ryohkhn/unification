module SubstitutionMap : Map.S with type key = Identifier.t

type t = Type.t SubstitutionMap.t
[@@deriving eq, ord]

val apply : t -> Type.t -> Type.t

val compose : t -> t -> t

val empty : t

val singleton : Identifier.t -> Type.t -> t

val of_seq : (Identifier.t * Type.t) list -> t

val find : Identifier.t -> t -> Type.t option

val pp : Format.formatter -> t -> unit

val show : t -> string
