let comp_apply ty1_env ty2_env ty1 ty2 =
  let env = TypeSubstitution.compose ty1_env ty2_env in
  let ty1 = TypeSubstitution.apply env ty1 in
  let ty2 = TypeSubstitution.apply env ty2 in
  env, ty1, ty2

let rec infer env = function
  | Term.IntConst _ -> Some Type.Int, env
  | Term.Var id -> infer_var env id
  | Term.Binop (t1, _, t2) -> infer_binop env t1 t2
  | Term.Pair (t1, t2) -> infer_pair env t1 t2
  | Term.Proj (proj, t) -> infer_proj env proj t
  | Term.Fun (id, body) -> infer_fun env id body
  | Term.App (t1, t2) -> infer_app env t1 t2

and infer_var env id = (
  match (TypeSubstitution.find id env) with
  | Some ty -> Some ty
  | None -> Some (Type.Var id)
), env

and infer_binop env t1 t2 =
  match infer env t1, infer env t2 with
  | (Some ty1, ty1_env), (Some ty2, ty2_env) -> (
      let env', ty1, ty2 = comp_apply ty1_env ty2_env ty1 ty2 in
      (* Binop operations only applies on Type.Int *)
      match Unification.unify ty1 Type.Int, Unification.unify ty2 Type.Int with
      | Some ty1_env, Some ty2_env ->
        let binop_env = TypeSubstitution.compose ty1_env ty2_env in
        Some Type.Int, TypeSubstitution.compose env' binop_env
      | _ -> None, env
    )
  | _ -> None, env

and infer_pair env t1 t2 =
  match infer env t1, infer env t2 with
  | (Some ty1, ty1_env), (Some ty2, ty2_env) ->
    let env', ty1, ty2 = comp_apply ty1_env ty2_env ty1 ty2 in
    Some (Type.Product (ty1, ty2)), env'
  | _ -> None, env

and infer_proj env proj t =
  match proj, infer env t with
  | First, (Some (Type.Product (ty, _)), ty_env)
  | Second, (Some (Type.Product (_, ty)), ty_env) ->
    let ty = TypeSubstitution.apply ty_env ty in
    Some ty, ty_env
  | _ -> None, env

and infer_fun env id body =
  match infer env body with
  | Some body_ty, env' -> (
      (* The anonymous variable is infered in the body env to determine the type *)
      let id_ty = TypeSubstitution.apply env' (Type.Var id) in
      Some (Type.Arrow (id_ty, body_ty)), env'
    )
  | _ -> None, env

and infer_app env t1 t2 =
  match infer env t1, infer env t2 with
  | (Some (Type.Arrow (dom, cod)), t1_env), (Some ty, t2_env) -> (
      let env, dom, ty = comp_apply t1_env t2_env dom ty in
      (* After composing and applying subs, the argument needs to be typed and not a Var *)
      match ty with
      | Type.Var _ -> None, env
      | _ -> (
          (* We unify the domain and the applied type to check if it has a substitution *)
          match Unification.unify dom ty with
          | Some env' ->
            let app_env = TypeSubstitution.compose env env' in
            let res_ty = TypeSubstitution.apply app_env cod in
            Some res_ty, app_env
          | None ->
            None, env
        )
    )
  | (Some (Type.Var id as f), t1_env), (Some ty, t2_env) -> (
      let env, dom, ty = comp_apply t1_env t2_env f ty in
      match Unification.unify dom ty with
      | Some env' ->
        (* A Var applied is a new Var that will be substituted into a Type.Arrow *)
        let new_id = Identifier.fresh () in
        let var_fun = Type.Arrow (ty, Type.Var new_id) in
        let app_env = TypeSubstitution.compose env env' in
        let var_fun_env =
          TypeSubstitution.singleton id var_fun
          |> TypeSubstitution.compose app_env
        in
        Some (Type.Var new_id), var_fun_env
      | None -> None, env
    )
  | _ -> None, env

let typeof term =
  fst (infer (TypeSubstitution.empty) term)
