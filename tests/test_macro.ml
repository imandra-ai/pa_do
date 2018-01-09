open Printf
open Camlp4.PreCast
open Test

let simple =
  group "Simple macro tests"
    [concrete_syntax "Float"
       "DEFINE F(x) = Float.(x + 1)"
       (`Compare_val [[Var.float "x"], Compare_val.float(),
                     "F(x)", <:expr@here< x +. 1. >>; ]);
     concrete_syntax "Module param"
       "DEFINE F(M, x) = M.of_float x"
       (`Compare_val [[Var.float "x"], Compare_val.int32(),
                     "F(Int32, x)", <:expr@here< Int32.of_float x >>;
                      [Var.float "x"], Compare_val.int64(),
                     "F(Int64, x)", <:expr@here< Int64.of_float x >>;]);
     concrete_syntax "Module param (functor)"
       "DEFINE F(M) = \
         let module Z = Set.Make(M) in Z.elements(Z.singleton x)"
       (`Compare_val [[Var.int32 "x"],
                     Compare_val.list (Compare_val.int32()),
                     "F(Int32)", <:expr@here< [x] >>;
                      [Var.string "x"],
                     Compare_val.list (Compare_val.string()),
                     "F(String)", <:expr@here< [x] >>; ]);
     concrete_syntax "Module param (type annotation)"
       "DEFINE F(M, y) = M.add (x : M.t) y"
       (`Compare_val [[Var.int32 "x"], Compare_val.int32(),
                     "F(Int32, 1l)", <:expr@here< Int32.add x 1l >>;
                      [Var.int64 "x"], Compare_val.int64(),
                     "F(Int64, 1L)", <:expr@here< Int64.add x 1L >>; ]);
     concrete_syntax "Parameters values are evaluated first"
       "DEFINE C(x) = (x,x+1)
        DEFINE X(a,b) = a * b"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "X(C(x))", <:expr@here< x * (x + 1) >>]);

     concrete_syntax "Overloading suspension"
       "DEFINE F(M) = M.(1 + 44)"
       (`Compare_val [[], Compare_val.int(),
                     "F(Int)", <:expr@here< 45 >>;
                      [], Compare_val.float(),
                     "F(Float)", <:expr@here< 45.0 >>; ]);
     concrete_syntax "Suspended overloading protected"
       "DEFINE F(M) = Float.(1 + M.(float(1 + y)))"
       (* [1 + y] should not be touched by Float *)
       (`Compare_val [[Var.int32 "y"], Compare_val.float(),
                     "F(Int32)", <:expr@here< 2. +. (Int32.to_float y) >>;
                      [Var.int64 "y"], Compare_val.float(),
                     "F(Int64)", <:expr@here< 2. +. (Int64.to_float y) >>; ]);
     concrete_syntax "Symbol parameters"
       "DEFINE F((<=), a, b) = if a <= b then a else b"
       (`Compare_expr ["F(le, x, y)",
                      <:expr@here< if le x y then x else y >> ]);
     concrete_syntax "DEFINE params mask macros"
       "DEFINE X = 1
        DEFINE F(X,x) = let module M = Set.Make(X) in M.elements(M.singleton x)"
       (`Compare_val [[Var.string "x"],
                     Compare_val.list (Compare_val.string()),
                     "F(String, x)", <:expr@here< [x] >> ;
                      [Var.int32 "x"],
                     Compare_val.list (Compare_val.int32()),
                     "F(Int32, x)", <:expr@here< [x] >>; ]);

     concrete_syntax "Macro taking other macros as params"
       "DEFINE F(DO(), x) = DO(x) + x
        DEFINE G(y) = 2 * y"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "F(G, x)", <:expr@here< 2 * x + x >> ]);

     concrete_syntax "Parameter macros with several arguments"
       "DEFINE F(DO(), x) = DO(x, x + 1) + x
        DEFINE G(y, z) = y * z"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "F(G, x)", <:expr@here< (x * (x + 1)) + x >> ]);

     concrete_syntax "Macro ignoring some parameters"
       "DEFINE A(_, x, _) = x"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "A(x, x+1, x+2)", <:expr@here< x + 1 >> ]);

     concrete_syntax "Macro passing a macro argument to another macro"
       "DEFINE F(DO(), x) = DO(x) + x
        DEFINE G(DO(), y) = F(DO, y)
        DEFINE H(x) = 2 * x"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "G(H, x)", <:expr@here< 2 * x + x >> ]);

     concrete_syntax "Macro passing a macro argument to another macro \
       (different name)"
       "DEFINE F(DO(), x) = DO(x) + x
        DEFINE G(AH(), y) = F(AH, y)
        DEFINE H(x) = 2 * x"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "G(H, x)", <:expr@here< 2 * x + x >> ]);
    ]
;;

let ifdef =
  group "IFDEF macro tests"
    [concrete_syntax "Substitution in IFDEF bodies"
       (* This is different from the macro package coming with camlp4
          where [y] is not substituted for [y].  The fact that [Y] is
          substituted is believed to be more sane. *)
       "DEFINE X
        IFDEF X THEN
          DEFINE Y = 1
          let y = Y
        END
        let z = Y"
       (`Compare_val [[], Compare_val.int(), "y", <:expr@here< 1 >>;
                      [], Compare_val.int(), "z", <:expr@here< 1 >>; ]);
     concrete_syntax "No backward substitution"
       (* If [Z] is substituted while reading the branch body and
          [Y] is then replaced later, the expression [Y] coming
          from [Z] expansion will be replaced by [1].  This is
          not expected because the behavior should be the same
          whether IFDEF is present or not. *)
       "DEFINE X
        type t = Y of int
        let x = Y 1 (* *)
        DEFINE Z = Y
        IFDEF X THEN
          DEFINE Y = 1
          let y = Z Y
        END"
       (`Compare_val [[], (Compare_val.make ~comp_no:1
                            <:expr@here< Pervasives.(=) >>),
                     "y", <:expr@here< x >> ]);
     concrete_syntax "Param substituted before macro expansion"
       "DEFINE F(x) = x + 1
        DEFINE X
        IFDEF X THEN
          DEFINE Y = 1
          let y = F(Y)
          let y' = F(F(Y))
        END
        let z = F(Y)
        let z' = F(F(Y))"
       (`Compare_val [[], Compare_val.int(),
                     "y", <:expr@here< 1 + 1 >>;
                      [], Compare_val.int(),
                     "z", <:expr@here< 1 + 1 >>;
                      [], Compare_val.int(),
                     "y'", <:expr@here< 1 + 1 + 1 >>;
                      [], Compare_val.int(),
                     "z'", <:expr@here< 1 + 1 + 1 >>; ]);
     concrete_syntax "DEFINE params mask macros (even when suspended)"
       "DEFINE X = 1
        IFDEF X THEN
          DEFINE F(X,x) =
            let module M = Set.Make(X) in M.elements(M.singleton x)
        END"
       (`Compare_val [[Var.string "x"],
                      Compare_val.list (Compare_val.string()),
                      "F(String, x)", <:expr@here< [x] >> ;
                      [Var.int32 "x"],
                      Compare_val.list (Compare_val.int32()),
                      "F(Int32, x)", <:expr@here< [x] >> ]);

     concrete_syntax "Suspended use of macros with >1 arguments \
                      (currified in Camlp4)"
       "DEFINE A
        IFDEF A THEN
          DEFINE F(x, y) = x * y
          let f x = F(x, x + x)
        END"
       (`Compare_val [[Var.int "x"], Compare_val.int(),
                      "f x", <:expr@here< x * (x + x) >> ]);

     concrete_syntax "Suspended use of macros with no arguments (constructor)"
       "DEFINE A
        type t = U of int * int
        IFDEF A THEN
          DEFINE B = U
          let g y z = B (y, z)
        END"
       (`Compare_val [[Var.int "x"; Var.int "y"; Var.int "z"],
                      Compare_val.int(),
                      "match g y z with U(a,b) -> a+b",
                      <:expr@here< y + z >> ]);

     concrete_syntax "Suspended use of macros with no arguments (general subst)"
       "DEFINE A
        let f x (y,z) = x + y * z
        IFDEF A THEN
          DEFINE B = f 1
          let g x y = B (x, y)
        END"
       (`Compare_val [[Var.int "x"; Var.int "y"],
                      Compare_val.int(),
                      "g x y", <:expr@here< 1 + x * y >> ]);
    ]
;;

let () =
  run (group "Macros tests" ~libs:["pa_do.cmo"] ~comp_no:10
         [simple; ifdef])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
