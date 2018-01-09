(* Runge Kutta method of order 4 *)

(* Real vector space.  (With this interface we cannot provide
   optimizations such as using a simgle temporary variable to
   accumulate a sum.) *)
module type Vec =
sig
  type t
  val add : t -> t -> t
  val mul : float -> t -> t
end

module Make(V: Vec) =
struct
  (* Unfortunately the overloading will have effect outside the module
     boundaries. *)
  OVERLOAD V (( + ) -> add; ( * ) -> mul);;

  let onesixth = 1. /. 6.
  let onethird = 1. /. 3.

  let rk4 ?(n=200) f t0 t1 x0 =
    let h = (t1 -. t0) /. float n in
    let xt = Array.make (n + 1) x0 in
    for i = 0 to n-1 do
      V.(
        let ti = t0 +. float i *. h
        and xi = xt.(i) in
        let ti5 = ti +. 0.5 *. h in
        let k1 = h * f ti xi in
        let k2 = h * f ti5 (xi + 0.5 * k1) in
        let k3 = h * f ti5 (xi + 0.5 * k2) in
        let k4 = h * f (ti +. h) (xi + k3) in
        xt.(Int.(i + 1)) <- xi + onesixth * (k1 + k4) + onethird * (k2 + k3)
      )
    done;
    xt
end



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
