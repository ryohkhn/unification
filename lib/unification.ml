(*
  This function checks if the type variable with the given identifier `id`
  occurs in the given type `ty`. It recursively traverses the type structure
  and returns true if the type variable is found, false otherwise.

  Parameters:
    - id: Identifier of the type variable to search for.
    - ty: Type structure to search within.

  Returns:
    - true if the type variable `id` occurs in `ty`, false otherwise.
*)
let rec occurs id ty =
  match ty with
  | Type.Var id' -> id = id'
  | Type.Int -> false
  | Type.Product (ty1, ty2) -> occurs id ty1 || occurs id ty2
  | Type.Arrow (ty1, ty2) -> occurs id ty1 || occurs id ty2

(*
  This function performs type unification between two given types `t1` and `t2`.
  It recursively traverses the type structures and attempts to find a substitution
  that makes the two types equal. If successful, it returns Some substitution,
  otherwise it returns None.

  Parameters:
    - t1: First type to unify.
    - t2: Second type to unify.

  Returns:
    - Some substitution if unification is successful, None otherwise.
*)
let rec unify t1 t2 =
  match t1, t2 with
  | Type.Int, Type.Int -> Some (TypeSubstitution.empty)
  | Type.Var id1, Type.Var id2 when id1 = id2 -> Some (TypeSubstitution.empty)
  | Type.Var id, t | t, Type.Var id ->
    if occurs id t then
      None (* Occur Check *)
    else
      Some (TypeSubstitution.singleton id t)
  | Type.Product (ty1, ty2), Type.Product (ty1', ty2') 
  | Type.Arrow (ty1, ty2), Type.Arrow (ty1', ty2') -> (
      match unify ty1 ty1' with
      | Some s1 -> (
          match unify (TypeSubstitution.apply s1 ty2) (TypeSubstitution.apply s1 ty2') with
          | Some s2 -> Some (TypeSubstitution.compose s2 s1)
          | None -> None
        )
      | None -> None
    )
  | _, _ -> None
