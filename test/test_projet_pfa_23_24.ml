open TypeInference

let tests_typeof =
  let x = Identifier.fresh () in
  let y = Identifier.fresh () in
  let f = Identifier.fresh () in
  [
    ("Variable: x",
     Term.Var x,
     Some (Type.Var x)
    );

    ("Int: 42",
     Term.IntConst 42, 
     Some Type.Int
    );

    ("Binop: 10 + 10",
     Term.Binop (Term.IntConst 10, Term.Plus, Term.IntConst 5), 
     Some Type.Int
    );

    ("Binop: x - x",
     Term.Binop (Term.Var x, Term.Minus, Term.Var y),
     Some Type.Int
    );

    ("Binop: 42 + y",
     Term.Binop (Term.IntConst 42, Term.Plus, Term.Var y),
     Some Type.Int
    );

    ("Binop: 1 + (fun x -> x + 1) 2",
     Term.Binop (Term.IntConst 1, Term.Plus,
                 Term.App (
                   Term.Fun (x, Term.Binop (Term.Var x, Term.Plus, Term.IntConst 1)),
                   Term.IntConst 2
                 )),
     Some Type.Int
    );

    ("Binop: 10 * (x, y)",
     Term.Binop (Term.IntConst 10, Term.Times, Term.Pair (Term.Var x, Term.Var y)),
     None
    );

    ("Pair: (x, y)",
     Term.Pair (Term.Var x, Term.Var y),
     Some (Type.Product (Type.Var x, Type.Var y))
    );

    ("Projection: fst (1, 2)",
     Term.Proj (First, Term.Pair (Term.IntConst 1, Term.IntConst 2)),
     Some Type.Int
    );

    ("Projection: snd (1, x)",
     Term.Proj (Second, Term.Pair (Term.IntConst 1, Term.Var x)),
     Some (Type.Var x)
    );

    ("Projection: snd 1",
     Term.Proj (Second, Term.IntConst 1),
     None
    );

    ("Projection: fst (snd ((1, 2), (3, 4)))",
     Term.Proj (First, Term.Proj (Second, Term.Pair (Term.Pair (Term.IntConst 1, Term.IntConst 2), Term.Pair (Term.IntConst 3, Term.IntConst 4)))),
     Some Type.Int
    );

    ( "Fun: (fun x -> x)",
      Term.Fun (x, Term.Var x),
      Some (Type.Arrow (Type.Var x, Type.Var x))
    );

    ("Fun: (fun x -> 42)",
      Term.Fun (x, Term.IntConst 42),
      Some (Type.Arrow (Type.Var x, Type.Int))
    );

    ("Fun: (fun x -> x + 42)",
      Term.Fun (x, Term.Binop(Term.Var x, Term.Plus, Term.IntConst 42)),
      Some (Type.Arrow (Type.Int, Type.Int))
    );

    ("Fun: (fun x -> x + 42)",
      Term.Fun (x, Term.Binop(Term.Var x, Term.Plus, Term.IntConst 42)),
      Some (Type.Arrow (Type.Int, Type.Int))
    );

    ("Fun: (fun x y -> x + y)",
     Term.(Fun (x, Fun (y, Binop (Var x, Plus, Var y)))),
     Some Type.(Arrow (Int, Arrow (Int, Int)))
    );

    ("Application: (fun x -> x) 42",
     Term.App (Term.Fun (x, Term.Var x), Term.IntConst 42),
     Some Type.Int
    );

    ( "Application: (fun x -> x + 1) 5",
      Term.App (Term.Fun (x, Term.Binop (Term.Var x, Term.Plus, Term.IntConst 1)), Term.IntConst 5),
      Some Type.Int
    );

    ( "Application: (fun y -> 1) (fun x -> x + 10)",
      Term.App ((Term.Fun (y, Term.IntConst 1), Term.Fun (x, Term.IntConst 10))),
      Some Type.Int
    );

    ( "Application: (fun x -> x * 2) y",
      Term.App (Term.Fun (x, Term.Binop (Term.Var x, Term.Times, Term.IntConst 2)), Term.Var y),
      None
    );

    ("Partial Application: (fun x y -> x * y) 2",
     Term.App (Term.Fun (x, Term.Fun (y, Term.Binop (Term.Var x, Term.Times, Term.Var y))), Term.IntConst 2),
     Some (Type.Arrow (Type.Int, Type.Int))
    );

    ("Application of a var: (fun f -> f 2) (fun x -> x + 1)",
     Term.App (Term.Fun (f, Term.App (Term.Var f, Term.IntConst 2)),
               Term.Fun (y, Term.Binop (Term.Var y, Term.Plus, Term.IntConst 1))),
     Some Type.Int
    );

    ("Application: Nested fun: (fun x -> (fun y -> x + y)) 5",
     Term.App (Term.Fun (x, Term.Fun (y, Term.Binop (Term.Var x, Term.Plus, Term.Var y))),
               Term.IntConst 5),
     Some (Type.Arrow (Type.Int, Type.Int))
    );

    ("Application of a var: Nested fun: (fun f -> (fun x -> f (x + 1))) (fun y -> y * 2)",
     Term.App (Term.Fun (f, Term.Fun (x, Term.App (Term.Var f, Term.Binop (Term.Var x, Term.Plus, Term.IntConst 1)))),
               Term.Fun (y, Term.Binop (Term.Var y, Term.Times, Term.IntConst 2))),
     Some (Type.Arrow (Type.Int, Type.Int))
    );
  ]

let tests_unify =
  let t1 = Identifier.fresh () in
  let t2 = Identifier.fresh () in
  [
    ("Unify: a =? a",
     Type.Int, Type.Int,
     Some (TypeSubstitution.empty)
    );

    ("Unify: x =? y",
     Type.Var t1, Type.Var t2,
     Some (TypeSubstitution.singleton t1 (Type.Var t2))
    );

    ("Unify: x =? x",
     Type.Var t1, Type.Var t1,
     Some (TypeSubstitution.empty)
    );

    ("Unify: x =? a",
     Type.Var t1, Type.Int,
     Some (TypeSubstitution.singleton t1 Type.Int)
    );

    ("Unify: a =? x",
     Type.Int, Type.Var t1,
     Some (TypeSubstitution.singleton t1 Type.Int)
    );

    ("Unify: (a, x) =? (a, x)",
     Type.Product (Type.Int, Type.Var t1), Type.Product (Type.Int, Type.Var t1),
     Some TypeSubstitution.empty
    );

    ("Unify: (fun x -> a) =? (fun x -> a)",
     Type.Arrow (Type.Var t2, Type.Int), Type.Arrow (Type.Var t2, Type.Int),
     Some TypeSubstitution.empty
    );

    ("Unify: (fun x -> y) =? (fun a -> a)",
      Type.Arrow (Type.Var t1, Type.Var t2),
      Type.Arrow (Type.Int, Type.Int),
      Some (TypeSubstitution.of_seq [(t1, Type.Int); (t2, Type.Int)])
    );

    ("Unify: (fun a -> x) =? (fun a -> y)", 
      Type.Arrow (Type.Int, Type.Var t1),
      Type.Arrow (Type.Int, Type.Var t2),
      Some (TypeSubstitution.singleton t1 (Type.Var t2))
    );

    ("Unify: x =? (fun x -> a)",
      Type.Var t1,
      Type.Arrow (Type.Var t1, Type.Int),
      None
    );
    
    ("Unify: {X = Y, Y = a}",
      Type.Arrow (Type.Var t1, Type.Var t2),
      Type.Arrow (Type.Var t2, Type.Int),
      Some (TypeSubstitution.of_seq [(t1, Type.Int); (t2, Type.Int)])
    );

    ("Unify: {a = Y, X = Y}",
      Type.Arrow (Type.Int, Type.Var t2),
      Type.Arrow (Type.Var t1, Type.Var t2),
      Some (TypeSubstitution.of_seq [(t1, Type.Int)])
    );

    ("Unify: {f(x) = f(y,z)}",
     Type.Arrow(Type.Var t1, Type.Int),
     Type.Arrow(Type.Var t1, Type.Arrow(Type.Var t2, Type.Int)),
     None
    );
  ]


let tests_compose =
  let y = Identifier.fresh () in
  let y' = Type.Var y in
  let b = Identifier.fresh () in
  let b' = Type.Var b in
  let c = Identifier.fresh () in
  let c' = Type.Var c in
  let z = Identifier.fresh () in
  let x = Identifier.fresh () in
  let s1 = [(y, Type.Var b); (z, Type.Arrow (c', c'))] in
  let s2 = [(x, Type.Arrow (y', y')); (y, Type.Var z)] in
  let composed = [(x, Type.Arrow (b', b')); (y, Type.Arrow (c', c')); (z, Type.Arrow (c', c'))] in
  [("{y/b, z/h(c)} ◦ {x/f(y), y/z}", TypeSubstitution.of_seq s1, TypeSubstitution.of_seq s2, TypeSubstitution.of_seq composed)]


let typeModule = (module Type : Alcotest.TESTABLE with type t = Type.t)

let typeSubstitutionModule = (module TypeSubstitution : Alcotest.TESTABLE with type t = TypeSubstitution.t)

let check_compose name s1 s2 expected =
  let open Alcotest in
  test_case name `Quick (fun () -> check typeSubstitutionModule "Composed" expected (TypeSubstitution.compose s1 s2))

let check_typeof term_text term expected_type =
  let open Alcotest in
  test_case term_text `Quick (fun () -> check (option typeModule) "Same type" expected_type (Inference.typeof term))

let check_unify term_text term1 term2 expected_map =
  let open Alcotest in
  test_case term_text `Quick (fun () -> check (option typeSubstitutionModule) "Unified" expected_map (Unification.unify term1 term2))

let () =
  let open Alcotest in
  run "Inference" [
      "typeof", List.map (fun (term_text, term, expected_type) -> check_typeof term_text term expected_type) tests_typeof ;
      "unify", List.map (fun (term_text, term1, term2, expected_map) -> check_unify term_text term1 term2 expected_map) tests_unify ;
      "compose", List.map (fun (term, sub1, sub2, expected) -> check_compose term sub1 sub2 expected) tests_compose;
  ]
