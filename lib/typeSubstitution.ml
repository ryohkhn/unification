module SubstitutionMap = Map.Make(Identifier)

type t = Type.t SubstitutionMap.t
[@@deriving eq, ord]

let rec apply subs ty = match ty with
  | Type.Var id -> (
      match SubstitutionMap.find_opt id subs with
     | Some ty' -> apply subs ty' (* recursive call since a Var can be a substitution *)
     | None -> ty
    )
  | Type.Product (ty1, ty2) -> Type.Product (apply subs ty1, apply subs ty2)
  | Type.Arrow (ty1, ty2) -> Type.Arrow (apply subs ty1, apply subs ty2)
  | _ -> ty

(* compose s2 s1 : first s1, then s2 *)
let compose s2 s1 =
  SubstitutionMap.merge (fun _ ty2 ty1 ->
      match ty2, ty1 with
      | Some _, Some _ -> ty1 (* when there is two subs for the same var, s1 is used *)
      | Some ty, None | None, Some ty -> Some (ty)
      | None, None -> None
    ) s2 s1
  |> SubstitutionMap.map (fun ty -> apply s2 ty)

let empty = SubstitutionMap.empty

let singleton var_ty ty = SubstitutionMap.singleton var_ty ty

let of_seq s = SubstitutionMap.of_seq (List.to_seq s)

let find id env = SubstitutionMap.find_opt id env

let pp ppf map =
  let pp_item ppf (key, value) =
    Format.fprintf ppf "(%a, %a)" Identifier.pp key Type.pp value
  in
  SubstitutionMap.iter (fun key value -> pp_item ppf (key, value)) map

let show t = Format.asprintf "%a" pp t
