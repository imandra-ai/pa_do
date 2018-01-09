(* Interval arithmetic implemented following the description given in

   Hans Koch, Alain Schenkel, Peter Wittwer, Computer-Assisted Proofs
   in Analysis and Programming in Logic: A Case Study, SIAM Review,
   Volume 38, Issue 4 (December 1996), pages: 565-604.
*)
open Printf

(* Bounds on the positive safe range *)
let lower_bound = ldexp 1. (-500)
let upper_bound = ldexp 1. 500
let up = 1. +. epsilon_float
let down = 1. -. epsilon_float

type t = { lo: float; up: float }
  (* invriant: [lo <= up] and [lo], [up] are in the safe range. *)

exception Overflow of t

exception Division_by_zero of t

(* Return an interval with safe lower and upper bounds on the interval
   [x .. y].  Assume [x <= y].  @raise Failure if it is not possible. *)
let safe_bounds x y =
  if x > 0. then
    (* Disallow falling under the safe threshold which may lead to
       underflow and unwanted rounding to 0. *)
    let i = { lo = Float.(max (down *. x) lower_bound);  up = up *. y } in
    if i.up < upper_bound then i else raise(Overflow i)
  else if y < 0. then
    let i = { lo = up *. x;  up = Float.(min (down *. y) (-. lower_bound)) } in
    if i.lo > -. upper_bound then i else raise(Overflow i)
  else (* x <= ±0 <= y or nan *)
    let i = { (* preserve the sign of [x] and [y] *)
      lo = if x = 0. then x else Float.(min (up *. x) (-. lower_bound));
      up = if y = 0. then y else Float.(max (down *. y) lower_bound) } in
    if i.lo > -.upper_bound && i.up < upper_bound then i else raise(Overflow i)

let in_safe_range i x =
  let absx = abs_float x in
  if not(x = 0. || (lower_bound < absx && absx < upper_bound)) then
    raise(Overflow i)

(* No checks needed (even on 64 bit machines). *)
let of_int x = { lo = float x; up = float x }

(* No need to use an surrounding interval, the number is considered exact.
   We must only verify that [x] is in the safe range. *)
let of_float x = let i = { lo = x;  up = x } in in_safe_range i x; i

let make lo up =
  if lo > up then failwith(sprintf "Interval.make: %g > %g" lo up);
  let i = { lo = lo;  up = up } in
  in_safe_range i lo;  in_safe_range i up;  i

let lower i = { lo = 0.0;  up = i.up }

let to_string i = sprintf "[%g, %g]" i.lo i.up

let neg i = { lo = -. i.up;  up = -. i.lo }

let abs i =
  Float.({ lo = max(0., i.lo, -. i.up); up = max(0., -. i.lo, i.up) })

let add i j = safe_bounds (i.lo +. j.lo) (i.up +. j.up)

let sub i j = add i (neg j)

let mul i j = Float.(
  let lolo = i.lo * j.lo
  and loup = i.lo * j.up
  and uplo = i.up * j.lo
  and upup = i.up * j.up in
  safe_bounds (min(lolo, loup, uplo, upup)) (max(lolo, loup, uplo, upup))
)

let inv i =
  if i.lo *. i.up <= 0. then raise(Division_by_zero i);
  safe_bounds (1. /. i.up) (1. /. i.lo)

let div i j = mul i (inv j)

let rec pow i n =
  if n = 0 then { lo = 1.; up = 1. }
  else if n = 1 then i
  else if n mod 2 = 0 then pow (mul i i) (n / 2)
  else mul i (pow (mul i i) (n / 2))

let lt i j =  i.up < j.lo

let gt i j =  i.lo > j.up
