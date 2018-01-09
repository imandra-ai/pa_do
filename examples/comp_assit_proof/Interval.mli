(** Interval arithmetic.  The implementation follows the description
    given in the paper:

    Hans Koch, Alain Schenkel, Peter Wittwer, Computer-Assisted Proofs
    in Analysis and Programming in Logic: A Case Study, SIAM Review,
    Volume 38, Issue 4 (December 1996), pages: 565-604.
*)

type t = private { lo: float; up: float }
    (** An interval with bounds in the "safe range" (defined as the
        set of machine representable numbers [r] s.t. [r = 0.] or
        [2**(-500) < abs(r) < 2**500].  See p. 15 of the paper.  This
        is the "standard set" for the reals (p. 19). *)

exception Overflow of t
  (** Raised when the interval overflows the safe range. *)

exception Division_by_zero of t
  (** Raised when dividing by an interval containing [0.0]. *)

val of_int : int -> t
  (** [of_int x] interval corresponding to an integer [x]. *)

val of_float : float -> t
  (** [of_float x] interval containing the single float [x].  Beware
      that if [x] is a literal, its computer representation may be
      truncated.  @raise Failure if an "safe" interval cannot be
      constructed. *)

val make : float -> float -> t
  (** [make lo up] make an interval with lower bound [lo] and upper
      bound [up].  @raise Failure if [lo > up] or [lo] or [up] are too
      large. *)

val lower : t -> t
  (** [lower i] returns the interval [0.0 .. i.up]. *)

val to_string : t -> string
  (** Textual representation of an interval. *)


(** {2 Arithmetic operators} *)

val neg : t -> t
  (** Unary negation. *)

val abs : t -> t
  (** Absolute value. *)

val add : t -> t -> t
  (** Addition. *)

val sub : t -> t -> t
  (** Substraction. *)

val mul : t -> t -> t
  (** Multiplication. *)

val inv : t -> t
  (** [inv i] is the inverse of [i], i.e. [1 / i].  @raise
      {!Interval.Division_by_zero} if one divides by an interval
      containing [0.0]. *)

val div : t -> t -> t
  (** Division.  @raise {!Interval.Division_by_zero} if one divides by
      an interval containing [0.0]. *)

val pow : t -> int -> t
  (** [pow i n] returns the interval representing [i**n]. *)


(** {2 Comparison operators} *)

val lt : t -> t -> bool
  (** [lt i j] returns [true] if the interval [i] is completely below
      the interval [j].  Beware that this is not a total order. *)

val gt : t -> t -> bool
  (** [lt i j] returns [true] if the interval [i] is completely above
      the interval [j].  Beware that this is not a total order. *)
