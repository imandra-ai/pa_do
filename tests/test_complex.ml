(* File: test_complex.ml

   Copyright (C) 2008

     Dany Maslowski <dan_mski@hotmail.com>

     Julie De Pril <julie.depril@umons.ac.be>

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Camlp4.PreCast
open Printf
open Test

(* We do not need funny names for the complex variables because there
   won't be any other variables in our expressions (so no risk of name
   clash). *)
let new_var =
  let no = ref 0 in
  fun () ->
    incr no;
    (* FIXME: the name starts with an underscore so that there is no
       warning if the name is not used -- but it would be better to
       ensure all bindings are indeed used... *)
    "_z" ^ string_of_int !no

let new_float =
  Random.self_init();
  fun () ->
    let r = float(Random.int 100) in
    if Random.bool() then r else -. r (* +0. and -0. not unlikely *)


(* Returns a string and an ast that correspond to a complex expression. *)
let expressions ~complex max_depth =
  let constant =
    if complex then
      (fun () ->
         let r = new_float() in
         let i = new_float() in
         (sprintf "(%g)+(%g)I" r i,
          <:expr@here< { Complex.re = $`flo:r$; Complex.im = $`flo:i$ } >>))
    else
      (fun () ->
         let r = new_float() in
         (sprintf "%g" r,  <:expr@here< $`flo:r$ >>))
  in
  let rec create var current_depth =
    if current_depth = max_depth then (* No more recursion *)
      constant()
    else
      let new_expr var = create var (current_depth + 1) in
      match Random.int 14 with
      | 0 -> constant()
      | 1 ->
          if var = [] then (* no defined variable, so add a constant *)
            constant()
          else
            let i_var = Random.int (List.length var) in
            let v = List.nth var i_var in
            v, <:expr@here< $lid:v$ >>
      | 2 ->
          let v = new_var() in
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr (v::var) in
          "let " ^ v ^ " = " ^ ast1_s ^ " in " ^ ast2_s,
          <:expr@here< let $lid:v$ = $ast1_e$ in $ast2_e$ >>
      | 3 ->
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr var in
          "(" ^ ast1_s ^ ") + (" ^ ast2_s ^ ")",
          (if complex then <:expr@here< Complex.add $ast1_e$ $ast2_e$ >>
           else <:expr@here< $ast1_e$ +. $ast2_e$ >> )
      | 4 ->
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr var in
          "(" ^ ast1_s ^ ") - (" ^ ast2_s ^ ")",
          (if complex then <:expr@here< Complex.sub $ast1_e$ $ast2_e$ >>
           else <:expr@here< $ast1_e$ -. $ast2_e$ >> )
      | 5 ->
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr var in
          "(" ^ ast1_s ^ ") * (" ^ ast2_s ^ ")",
          (if complex then <:expr@here< Complex.mul $ast1_e$ $ast2_e$ >>
           else <:expr@here< $ast1_e$ *. $ast2_e$ >> )
      | 6 ->
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr var in
          "(" ^ ast1_s ^ ") / (" ^ ast2_s ^ ")",
          (if complex then <:expr@here< Complex.div $ast1_e$ $ast2_e$ >>
           else <:expr@here< $ast1_e$ /. $ast2_e$ >> )
      | 7 ->
          let ast1_s, ast1_e = new_expr var in
          let ast2_s, ast2_e = new_expr var in
          "(" ^ ast1_s ^ ") ** (" ^ ast2_s ^ ")",
          (if complex then <:expr@here< Complex.pow $ast1_e$ $ast2_e$ >>
           else <:expr@here< $ast1_e$ ** $ast2_e$ >> )
      | 8 ->
          let ast_s, ast_e = new_expr var in
          "conj(" ^ ast_s ^ ")",
          (if complex then <:expr@here< Complex.conj $ast_e$ >>
           else ast_e (* do nothing *) )
      | 9 ->
          let ast_s, ast_e = new_expr var in
          "exp(" ^ ast_s ^ ")",
          (if complex then <:expr@here< Complex.exp $ast_e$ >>
           else <:expr@here< exp $ast_e$ >> )
      | 10 ->
          let ast_s, ast_e = new_expr var in
          if complex then
            ("log(" ^ ast_s ^ ")", <:expr@here< Complex.log $ast_e$ >>)
          else
            ("(log(" ^ ast_s ^ ")).re",
             <:expr@here< log(abs_float $ast_e$) >> )

      (* [Complex.sin] and [Complex.cos] do not exist => no possible
         comparison. *)
      | 11 ->
          let ast_s, ast_e = new_expr var in
          if complex then
            ("arg(" ^ ast_s ^ ") + 0I", (* +0I: make sure the result is
                                           considered a complex. *)
             <:expr@here< {Complex.re = Complex.arg $ast_e$; im = 0.0 } >>)
          else
            ("arg(" ^ ast_s ^ ")",
             <:expr@here< (if $ast_e$ > 0. || 1. /. $ast_e$ > 0.
                           then 0. else 4. *. atan 1.) >>)
      | 12 ->
          let ast_s, ast_e = new_expr var in
          if complex then
            ("norm(" ^ ast_s ^ ") + 0I",
             <:expr@here< {Complex.re = Complex.norm $ast_e$; im = 0.0 } >>)
          else
            ("norm(" ^ ast_s ^ ")", <:expr@here< abs_float $ast_e$ >>)
      | 13 ->
          let ast_s, ast_e = new_expr var in
          if complex then
            ("norm2(" ^ ast_s ^ ") + 0I",
             <:expr@here< {Complex.re = Complex.norm2 $ast_e$; im = 0.0 } >>)
          else
            ("norm2(" ^ ast_s ^ ")", <:expr@here< $ast_e$**2. >>)
      | _ -> assert false
  in
  let s, e = create [] 0 in
  "Complex.(" ^ s ^ ")",
  (if complex then e
   else <:expr@here< { Complex.re = $e$; Complex.im = 0. } >> )


let cmp_complex =
  Compare_val.make
    (<:expr@here< fun w z ->
       let re = abs_float(w.Complex.re -. z.Complex.re)
       and im = abs_float(w.Complex.im -. z.Complex.im) in
       let re_max = max (abs_float w.Complex.re) (abs_float z.Complex.re)
       and im_max = max (abs_float w.Complex.im) (abs_float z.Complex.im) in
       (* Ignore nan results *)
       (re <> re || re <= 1e-12 *. re_max)
       && (im <> im || im <= 1e-12 *. im_max)
       >>)

let rec list_init n f =
  if n = 0 then [] else f() :: list_init (n - 1) f

let complex_literals =
  [] (* FIXME: TODO *)

let complex =
  let test title s e =
    concrete_syntax title ""
      (`Compare_val [([], cmp_complex, s, e)]) in
  [test "arg(neg real)" "Complex.(arg(arg(653-471I)))"
     (<:expr@here<
        {Complex.re = Complex.arg{Complex.re=Complex.arg{Complex.re=653.;
                                                         Complex.im=(-471.)};
                                  Complex.im=0.0};
         Complex.im = 0.0} >>);

   test "arg(-1+0I)" "Complex.(arg(-1+0I))"
     (<:expr@here<
        {Complex.re = Complex.arg{Complex.re=(-1.); Complex.im=0.};
         Complex.im = 0.0} >>);

   test "arg(-1-0I)" "Complex.(arg(-1-0I))"
     (<:expr@here<
        {Complex.re = Complex.arg{Complex.re=(-1.); Complex.im=(-0.)};
         Complex.im = 0.0} >>);

   test "log(neg real)" "Complex.(log(-12))"
     (<:expr@here< Complex.log {Complex.re=(-12.); Complex.im=0.0} >>);

   test "Conj and sign of 0." "Complex.(arg(conj(0)))"
     (<:expr@here< {Complex.re = Complex.arg(Complex.conj Complex.zero);
                    Complex.im = 0.0 } >>);

   test "arg(-0+0I)" "Complex.(arg(-0+0I))"
     (<:expr@here<
        {Complex.re = Complex.arg {Complex.re=(-0.); Complex.im=0.};
         Complex.im = 0.0} >>);

   (* [-0-0I] means that the real and imaginary parts are [-0.].  This
      is to be close to what the person writes and is not the same as
      [Complex.add {re=(-0.); im=0.} {re=0.; im=(-0.)}]. *)
   test "-0-0I" "Complex.(arg(-0-0I))"
     (<:expr@here<
        {Complex.re = Complex.arg {Complex.re=(-0.); Complex.im=(-0.)};
         Complex.im = 0.0} >>);

   test "-0I = 0 + (-0)I" "Complex.(arg(-0I))"
     (<:expr@here<
        {Complex.re = Complex.arg {Complex.re=0.; Complex.im=(-0.)};
         Complex.im = 0.0} >>);

   (* We mean here that only the real part is touched -- there is
      no imaginary part, so no computation is performed on it. *)
   test "1 * (-1) = -1 + (+0)I" "Complex.(arg(1 * (-1)))"
     (<:expr@here<
        let z = {Complex.re=(-1.); Complex.im = 0.} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   (* If an explicit imaginary part is added, then the computations
      must not ignore it just because it is [0.]. *)
   test "(1+0I) * (-1) = -1 + (-0)I" "Complex.(arg((1+0I) * (-1)))"
     (<:expr@here<
        let z = {Complex.re=(-1.); Complex.im = (-0.)} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   test "1 * I = 0 + I" "Complex.(arg(1 * I))"
     (<:expr@here<
        let z = {Complex.re=0.0; Complex.im=1.} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   test "(1+0I) * I = (-0) + I" "Complex.(arg((1+0I) * I))"
     (<:expr@here<
        let z = {Complex.re=(-0.); Complex.im=1.} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   test "(1+0I) * (-I) = 0 + (-1)I" "Complex.(arg((1+0I) * (-I)))"
     (<:expr@here<
        let z = {Complex.re=0.; Complex.im=(-1.)} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   (* No explicit imaginary part in the left hand side expression =>
      one does not introduce one during the computations (so they
      amount to float ones). *)
   test "1 / (-1) = (-1) + (+0)I" "Complex.(arg(1 / (-1)))"
     (<:expr@here<
        let z = {Complex.re=(-1.); Complex.im = 0.0} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   test "(1+0I) / (-1) = (-1) + (-0)I" "Complex.(arg((1+0I) / (-1)))"
     (<:expr@here<
        let z = {Complex.re=(-1.); Complex.im = (-0.)} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   (* No explicit imaginary part. *)
   test "1 / (-I) = (+0) + (+1)I" "Complex.(arg(1 / (-I)))"
     (<:expr@here<
        let z = {Complex.re=0.0; Complex.im = 1.} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   test "(1+0I) / (-I) = (-0) + (+1)I" "Complex.(arg((1+0I) / (-I)))"
     (<:expr@here<
        let z = {Complex.re=(-0.); Complex.im = 1.} in
        { Complex.re = Complex.arg z;  Complex.im = 0.0 } >>);

   (* Because [Complex.arg : Complex.t -> float], we take [arg] are a
      literal evidence that we are back to pure real numbers and so
      [conj] should not change the imaginary part. *)
   test "Arg Conj Arg" "Complex.(arg(conj(arg(1-I))))"
     (<:expr@here<
        {Complex.re = Complex.arg({Complex.re = Complex.arg
                                      {Complex.re = 1.0; Complex.im =(-1.0)};
                                   Complex.im = 0.0 });
         Complex.im = 0.0 } >>);

   concrete_syntax "to_string is recognized" ""
     (`Compare_val [([], Compare_val.string ~comp_no:1 (),
                     "Complex.(to_string(1+I))",
                     <:expr@here< "1+1I" >>)
      ]);

   (* When binding a float expression, it will not be converted to a
      complex one when reaching an unkown construction -- just as it
      would be if it was inlined. *)
   concrete_syntax "Binding a float" ""
     (`Compare_val [([], Compare_val.bool ~comp_no:1 (),
                     "let f x = x = Complex.one in
		      Complex.(let z = 1 in f z)",
                     <:expr@here< true >>);
                    ([Var.float "u"], Compare_val.bool ~comp_no:10 (),
                     "let f x = x = { Complex.re = u; Complex.im = 0. } in
		      Complex.(let z = re u in f z)",
                     <:expr@here< true >>);
                   ]);

   test "Binding complex" "Complex.(let z = 1 + 2I in sin z)"
     (<:expr@here< { Complex.re = sin 1. *. cosh 2.;
                     Complex.im = cos 1. *. sinh 2. } >>);

   test "Binding imaginary" "Complex.(let z = 2I in conj z)"
     (<:expr@here< { Complex.re = 0.; Complex.im = (-2.) } >>);

   (* Simple case where the binding is completely eliminated and float
      operations on literals are statically performed. *)
   concrete_syntax "Binding optimization" ""
     (`Compare_expr ["Complex.(let z = 1+2I in z)",
                     <:expr@here< let open Complex in
                                  { Complex.re = 1.; Complex.im = 2.; } >> ;
                     "Complex.(let z = 1+2I in z + 3)",
                     <:expr@here< let open Complex in
                                  { Complex.re = 4.; Complex.im = 2.; } >> ;
                     "Complex.(let z = 1+2I in z + 3I)",
                     <:expr@here< let open Complex in
                                  { Complex.re = 1.; Complex.im = 5.; } >> ]);

   (* Make sure bindings are not duplicated.  Beware that the local
      [open Complex] makes [i] the unit complex number. *)
   concrete_syntax "Binding with side effect" ""
     (`Compare_val [([], Compare_val.int (),
                     "let j = ref 1 in \
                      Complex.(let z = incr j; 2 in ignore(z + z); !j)",
                     <:expr@here< 2 >>);
                    ([], Compare_val.int (),
                     "let j = ref 1 in \
		      ignore(Complex.(let z = incr j; 1 in z)); \
                      !j",
                     <:expr@here< 2 >>);
                    ([], Compare_val.int (),
                     "let i = ref 1 in
		      let f() = incr i; 1. in
		      ignore(Complex.(let a = sin(re(f())) in a));
		      !i",
                     <:expr@here< 2 >>);
                   ]);

   (* A binding may be masked by another *)
   test "Binding scoping" "Complex.(let z = 1 in let z = 2 + z in z)"
     (<:expr@here< { Complex.re = 3.; Complex.im = 0. } >>);

   concrete_syntax "Removal of overloading protections" ""
     (`Compare_val [([], Compare_val.float (),
                     "Complex.(let a = 1 in Float.(1))",
                     <:expr@here< 1. >> )]);

  ]

let random_tests ~complex =
  list_init 200 begin fun () ->
    let s, e = expressions ~complex (Random.int 10) in
    concrete_syntax ("Random expression: " ^ s) ""
      (`Compare_val [([], cmp_complex, s, e)])
  end

let () =
  (* Compare results only 1 time because they are just numbers *)
  run (group "Complex optimization tests" ~libs:["pa_do.cmo"] ~comp_no:1
         [group "Checking various corner cases" complex;
          group "Random tests on complex numbers" (random_tests ~complex:true);
          group "Random tests on floats" (random_tests ~complex:false);
         ])


(* Local Variables: *)
(* compile-command: "omake --no--progress complex" *)
(* End: *)
