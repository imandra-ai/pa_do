(* Simple example showing how to define a generic function on various
   types.  Inspired from a discussion on the OCaml list; see:
   http://caml.inria.fr/pub/ml-archives/caml-list/2009/03/c73848e9143d0e7753fc3bbda1aacfc5.en.html *)

open Printf

DEFINE NEWTON(M) = M.(
  let rec newton epsilon f df x =
    let x' = x - f x / df x in
    if abs(x - x') < epsilon then x'
    else newton epsilon f df x' in
  newton)
;;

let float_newton = NEWTON(Float)

let ratio_newton = NEWTON(Ratio)

(* Note that the constants will also be overloaded according to the
   module name [M] passed in. *)
DEFINE F(M) = M.(fun x -> x * x - 2)
DEFINE DF(M) = M.(fun x -> 2 * x)

let () =
  let f = F(Float) and df = DF(Float) in
  printf "Solution (Float): %.14f\n" (float_newton 1e-8 f df 2.);
  let f = F(Ratio) and df = DF(Ratio) in
  let x = Ratio.(ratio_newton (1/10_000_000) f df 2) in
  printf "Solution (Ratio): %s ~= %.14f\n" Ratio.(to_string x) Ratio.(float x)



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
