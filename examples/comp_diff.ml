(* Simple Computational Differentiation (aka Automatic
   Differentiation) illustration inspired by the paper:

   Functional Differentiation of Computer Programs,
   Jerzy Karczmarczuk (karczma@info.unicaen.fr)
   Dept. of Computer Science, University of Caen,
   Sciences III, Bd. Maréchal Juin, 14032 Caen, France
*)
open Printf

module CD =
struct
  type t = { v: float;                  (* value *)
             d: float }                 (* deriv *)

  let to_string x = sprintf "(%f, %f)" x.v x.d

  (* Constants have derivative 0. *)
  let cst x = { v = x;  d = 0. }
  let var x = { v = x;  d = 1. }

  (* Simple arithmetic operations *)
  let neg x = { v = -. x.v;  d = -. x.d }
  let add x y = { v = x.v +. y.v;  d = x.d +. y.d }
  let sub x y = { v = x.v -. y.v;  d = x.d -. y.d }
  let mul x y = { v = x.v *. y.v;  d = x.d *. y.v +. x.v *. y.d }
  let div x y = { v = x.v /. y.v;
                  d = (x.d *. y.v -. x.v *. y.d) /. (y.v *. y.v) }

  (* Standard functions *)
  let dlift f f' = fun x -> { v = f x.v;  d = x.d *. f' x.v }

  let exp  = dlift exp exp
  let sin  = dlift sin cos
  and cos  = dlift cos (fun x -> -. sin x)
  let sqrt = dlift sqrt (fun x -> 0.5 /. sqrt x)
  let log  = dlift log (fun x -> 1. /. x)
end;;
OVERLOAD_FLOAT CD (cst);;
OVERLOAD_ARITHMETIC CD;;
OVERLOAD CD (exp; sin; cos; sqrt; log);;

let f x = CD.(
  let z = -x*(2.*x*x + x) in
  (z + 3.0*x)/(z - 1.0)
)

let ch z =
  CD.(let e = exp z in (e + 1.0/e)/2.0)

let () =
  let x = 2.5 in
  printf "(f %g, f' %g) = %s\n" x x (CD.to_string (f (CD.var x)));
  let x = 0.5 in
  printf "(ch %g, ch' %g) = %s\n" x x (CD.to_string (ch (CD.var 0.5)))



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)

