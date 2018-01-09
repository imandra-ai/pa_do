(* Feature request #46.  Simple example inspired by the post
   http://camltastic.blogspot.com/2008/05/phantom-types.html
*)

type 'a t
val meters : float -> [`Meters] t
val feet : float -> [`Feet] t

(** Arithmetic operations. *)
val neg : 'a t -> 'a t
val add : 'a t -> 'a t -> 'a t
val sub : 'a t -> 'a t -> 'a t
val mul : 'a t -> 'a t -> 'a t
val div : 'a t -> 'a t -> 'a t

val to_float : 'a t -> float
  (** Naive function to be able to print. *)
