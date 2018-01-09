
(** v <- a1 * v1 + ... + aN * vN ai may be of the form ai1 * ... * aik
    which will be interpreted as a multiplication of floats.
    v will generally not be allocated, but one must take care it does
    not occur in the right hand side, or if it does, only under a
    restricted form.

    v <- v + cos v will need to allocate a temporary vector.

    v <- v ± expr : optimized, does not allocate a temporary vector,
    provided v is not used by [expr].

    v <- a * v : optimized, use [scal].
    v <- w : copy v into w
    v <- a * w : copy w into v and scale it.


    Literals for bigarrays:

    {| a1, a2, ..., aN |}
    {| a11, a12, ..., a1N;
       a21, a22, ..., a2N;
       ...
       aM1, aM2, ..., aMN |}

    Uses [Vec.create] and [Mat.create], so the type of the vector and
    matrix are given by these functions.

    Slices for bigarrays:
*)

(* Define a type and provide a function so that transformations of
   that type can be "pushed" into the system (associated with a name?). *)
