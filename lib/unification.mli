(*
   The function `unify` must compute the substitution
   `s` such that if  `unify t1 t2 = Some s` then
   `apply s t1 = apply s t2`.
 *)
val unify : Type.t -> Type.t -> TypeSubstitution.t option

val occurs : Identifier.t -> Type.t -> bool
