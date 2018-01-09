(* Test the standard overloadings coming with the package. *)
open Camlp4.PreCast
open Test

let std_int var =
  [[Var.int "x"], Compare_val.int(), "to_int(of_int x)", <:expr@here< x >>;
   [Var.int32 "x"], Compare_val.int32(),
   "to_int32(of_int32 x)", <:expr@here< x >>;
   [Var.nativeint "x"], Compare_val.nativeint(),
   "to_nativeint(of_nativeint x)", <:expr@here< x >>;
   [Var.float "x"], Compare_val.float(),
   "float(truncate x)", <:expr@here< floor x >>;
   [], Compare_val.bool(), "max_int + min_int = -1", <:expr@here< true >>;
   [var "x"], Compare_val.bool(),
   "(x lsr 1) lsl 1 = x land (lnot 1)", <:expr@here< true >>;
  ]

(* Overload the expressions with the module [m]. *)
let std_int_for m var =
  let o (var, cmp, e0, e1) = (var, cmp, m ^ ".(" ^ e0 ^ ")", e1) in
  `Compare_val(List.map o (std_int var))

let standard =
  List.map (fun (m, var) -> concrete_syntax m "" (std_int_for m var))
    ["Int", (fun x -> Var.int x);
     "Int32", (fun x -> Var.int32 x);
     "Int64", (fun x -> Var.int64 x);
     "Nativeint", (fun x -> Var.nativeint x);
    ]

let optim =
  [concrete_syntax "Int optimizations" ""
     (`Compare_expr ["Int.(of_int x)", <:expr@here< x >>;
                     "Int.(to_int x)", <:expr@here< x >>]);
   concrete_syntax "Int optimizations" ""
     (`Compare_expr ["Int.(123 * 2 - 1)", <:expr@here< 245 >>;
                     "Int.(x + 0)", <:expr@here< x >>;
                     "Int.(0 + x)", <:expr@here< x >>;
                     "Int.((0 + x) + 0)", <:expr@here< x >>;
                     "Int.((0 + 0) + x)", <:expr@here< x >>;
                     "Int.(1 * x)", <:expr@here< x >>;
                     "Int.(x * 1)", <:expr@here< x >>;
                     "Int.((1 + 0) * x)", <:expr@here< x >>;
                     "Int.(0 * x)", <:expr@here< 0 >>;
                     "Int.(x * 0)", <:expr@here< 0 >>;
                     "Int.((4 - 3) * x)", <:expr@here< x >>;
                    ]);
   concrete_syntax "Int optimizations (macros)"
     "DEFINE F(a) = Int.(a * x)\n\
      DEFINE G(a, M) = M.(a * x)"
     (`Compare_expr [(* "F(0)", <:expr@here< 0 >>; *)
                     (* "F(1)", <:expr@here< x >>; *)
                     "G(0, Int)", <:expr@here< 0 >>;
                     "G(1, Int)", <:expr@here< x >>; ]);
   concrete_syntax "Int32 optimizations" ""
     (`Compare_expr ["Int32.(of_int32 x)",
                     <:expr@here< let open Int32 in x >>;
                     "Int32.(to_int32 x)",
                     <:expr@here< let open Int32 in x >>]);
   concrete_syntax "Int64 optimizations" ""
     (`Compare_expr ["Int64.(of_int64 x)",
                     <:expr@here< let open Int64 in x >>;
                     "Int64.(to_int64 x)",
                     <:expr@here< let open Int64 in x >>]);
   concrete_syntax "Nativeint optimizations" ""
     (`Compare_expr ["Nativeint.(of_nativeint x)",
                     <:expr@here< let open Nativeint in x >>;
                     "Nativeint.(to_nativeint x)",
                     <:expr@here< let open Nativeint in x >>]);
  ]

let float =
  [concrete_syntax "float max nan (inline)" ""
     (`Compare_val [[Var.float "x"], Compare_val.float(),
                    "Float.(max x nan)", <:expr@here< x >>;
                    [Var.float "x"], Compare_val.float(),
                    "Float.(max nan x)", <:expr@here< x >>; ]);
   concrete_syntax "float max nan"
     "let f = Float.(max)"
     (`Compare_val [[Var.float "x"], Compare_val.float(),
                    "f x nan", <:expr@here< x >>;
                    [Var.float "x"], Compare_val.float(),
                    "f nan x", <:expr@here< x >>; ]);
   concrete_syntax "float min nan (inline)" ""
     (`Compare_val [[Var.float "x"], Compare_val.float(),
                    "Float.(min x nan)", <:expr@here< x >>;
                    [Var.float "x"], Compare_val.float(),
                    "Float.(min nan x)", <:expr@here< x >>; ]);
   concrete_syntax "float min nan"
     "let f = Float.(min)"
     (`Compare_val [[Var.float "x"], Compare_val.float(),
                    "f x nan", <:expr@here< x >>;
                    [Var.float "x"], Compare_val.float(),
                    "f nan x", <:expr@here< x >>; ]);
   concrete_syntax "float min(f1, f2, f3)"  ""
     (`Compare_val [[Var.float "x"; Var.float "y"; Var.float "z"],
                    Compare_val.float(),
                    "Float.(min(x, y, z))", <:expr@here< min x (min y z) >> ]);
   concrete_syntax "float max(f1, f2, f3)"  ""
     (`Compare_val [[Var.float "x"; Var.float "y"; Var.float "z"],
                    Compare_val.float(),
                    "Float.(max(x, y, z))", <:expr@here< max x (max y z) >> ]);
   concrete_syntax "is_nan (inline)" ""
     (`Compare_val [[Var.float "x"], Compare_val.bool(),
                    "Float.(is_nan x)", <:expr@here< false >>;
                    [], Compare_val.bool() ~comp_no:1,
                    "Float.(is_nan nan)", <:expr@here< true >>; ]);
   concrete_syntax "is_nan"
     "let f = Float.(is_nan)"
     (`Compare_val [[Var.float "x"], Compare_val.bool(),
                    "f x", <:expr@here< false >>;
                    [], Compare_val.bool() ~comp_no:1,
                    "f nan", <:expr@here< true >>; ]);
   concrete_syntax "sci notation" ""
     (`Compare_val [[], Compare_val.float() ~comp_no:1,
                    "Float.(1.)", <:expr@here< 1. >>;
                    [], Compare_val.float() ~comp_no:1,
                    "Float.(1e-6)", <:expr@here< 1e-6 >>; ]);
   concrete_syntax "Float optimizations" ""
     (`Compare_expr ["Float.(234. * 3 - 4)", <:expr@here< 698. >>;
                     "Float.(x + 0.)", <:expr@here< x >>;
                     "Float.(0. +. x)", <:expr@here< x >>;
                     "Float.((0. +. x) + 0)", <:expr@here< x >>;
                     "Float.((0. +. 0) + x)", <:expr@here< x >>;
                     "Float.(1. *. x)", <:expr@here< x >>;
                     "Float.(x *. 1)", <:expr@here< x >>;
                     "Float.((1. +. 0.) *. x)", <:expr@here< x >>;
                     "Float.(0. *. x)", <:expr@here< 0. >>;
                     "Float.(x *. 0)", <:expr@here< 0. >>;
                     "Float.(x *. (5. - 5))", <:expr@here< 0. >>;
                     "Float.((6 - 5) * x)", <:expr@here< x >>  ]);
   concrete_syntax "Float optimizations (macros)"
     "DEFINE F(a) = Float.(a * x)\n\
      DEFINE G(a, M) = M.(a * x)"
     (`Compare_expr [(* "F(0)", <:expr@here< 0. >>; *)
                     (* "F(1)", <:expr@here< x >>; *)
                     "G(0, Float)", <:expr@here< 0. >>;
                     "G(1, Float)", <:expr@here< x >>; ]);
   concrete_syntax "pi" ""
     (`Compare_val [[], Compare_val.float() ~comp_no:1,
                    "Float.(pi)", <:expr@here< 4. *. atan 1. >>; ]);
   concrete_syntax "hypot" ""
     (`Compare_val [[], Compare_val.float() ~comp_no:1,
                    "Float.(hypot 3. 4.)", <:expr@here< 5. >>;
                    [], Compare_val.float() ~comp_no:1,
                    "Float.(hypot 0. (-7.))", <:expr@here< 7. >>;
                    [], Compare_val.float() ~comp_no:1,
                    "Float.(hypot (-7.) 0.)", <:expr@here< 7. >>;
                    let bound = 10000. in
                    [Var.float "x" ~bound; Var.float "y" ~bound],
                    Compare_val.float() ~eps:1e-10,
                    (* Occasional test failures are possible since the
                       expression one compares to does not avoid the
                       overflow and underflow. *)
                    "Float.(hypot x y)", <:expr@here< sqrt(x *. x +. y *. y) >>;
                   ]);
  ]

let () =
  run (group "Standard overloadings" ~libs:["pa_do.cmo"]
         ~header:<:str_item@here< >> ~comp_no:10
         (standard @ optim @ float))


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
