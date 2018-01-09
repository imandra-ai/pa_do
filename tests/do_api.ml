open Printf
open Camlp4.PreCast
open Test

let simple =
  group "Simple api tests for pa_do"
    [api "Int32"
      <:expr@here<
      let std_funs =
        ["mod", "rem";  "succ", "succ"; "pred", "pred"; "abs", "abs";
         "land", "logand"; "lor", "logor"; "lxor", "logxor";
         "lnot", "lognot"; "lsl", "shift_left"; "asr", "shift_right";
         "lsr", "shift_right_logical";
         "truncate", "of_float";  "float", "to_float";
         "of_int", "of_int";  "to_int", "to_int";
         "of_string", "of_string";  "to_string", "to_string";
        ] in
      let overloading_int32 =
        let t = (int empty ~cache:false
                    (fun i _ _loc -> <:expr< $`int32:Int32.of_int(i)$ >>)) in
        let t = std_arithmetic t in
        let t = comparison t in
        let t = lid t "incr"
          (fun _ _loc -> <:expr< fun x -> x := Int32.add x 1l >>) in
        let t = lid t "decr"
          (fun _ _loc -> <:expr< fun x -> x := Int32.sub x 1l >>) in
        lid_subst t std_funs in
      associate overloading_int32 "Int32"; >>
        (`Compare_val [[Var.int32 "x";
                        Var.int32 "y"],
                      Compare_val.int32(),
                      "Int32.(if x < y then x + y else x - y)",
                      (<:expr@here< if Int32.compare x y < 0
                       then Int32.add x y else Int32.sub x y >>); ]);
    ]

let () =
  run (group "Api tests" ~libs:["pa_do.cmo"]
          ~header:<:str_item@here< open Pa_do.Delimited_overloading;;
          open Camlp4.PreCast >> ~comp_no:10
          [simple])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
