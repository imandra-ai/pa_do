(* FIXME: How to add an operator to an existing module.  E.g. suppose
   one defines Int64.pow, how do we bind it to ** ?
*)

(** (submodule of [Pa_do]) Overloadings for the modules in the
    standard library.

    For the modules [Int32], [Int64], [Nativeint], [Complex] and the
    two virtual modules [Int] and [Float], the common features are:
    - Integer constants are reinterpreted as constants of the module.
    - The standard arithmetic operations [+], [-] (binary and unary),
    [*], and [/] are overloaded.
    - Comparison operators are overloaded and specialized to the
    module types for greater efficiency.
    - [to_string] and [of_string] are aliased in all modules to the
    proper conversion functions.
    - [min], [max], [incr], [decr] are defined for all modules except
    [Complex].  [min] and [max] are also allowed to take a literal
    tuple of values: [min(x1,...,xN)] will inline an expression
    computing the minimum of the values [x1],...,[xN].

    In addition, all integer modules share the following overloadings
    (which allows to easily replace one by another):
    - [abs], [mod], [pred], [succ],
    - [lsl], [lsr], [asr] (since the second argument of these functions is
    an integer, integer literals at this position will not be transformed),
    - [land], [lor], [lxor], [lnot],
    - [float], [truncate],
    - [of_int], [to_int], [of_int32], [to_int32], [of_int64], [to_int64],
    [of_nativeint], [to_nativeint],
    - [max_int], [min_int].
*)

open Camlp4.PreCast.Syntax.Ast

(** Int overloadings and optimized functions. *)
module Int :
sig
  val overloading : Delimited_overloading.t
    (** The main purpose of this overloading set is to escape
        subexpressions from a surrounding overloading.  Also, it makes
        sure that common integer operations [abs], [float], [lsl],
        [lsr], [incr], [max],... have their standard ([Pervasives])
        meaning, even if they have been redefined.  [max] and
        [min] are optimized for integers. *)

  val eq : Loc.t -> expr
    (** An expression for the equality function [( = ) : int -> int ->
        bool], optimized for integers. *)
  val ne : Loc.t -> expr  (** same as [eq] but for [<>] *)
  val lt : Loc.t -> expr  (** same as [eq] but for [<] *)
  val le : Loc.t -> expr  (** same as [eq] but for [<=] *)
  val gt : Loc.t -> expr  (** same as [eq] but for [>] *)
  val ge : Loc.t -> expr  (** same as [eq] but for [>=] *)
  val compare : Loc.t -> expr
    (** An expression for a specialized comparison function. *)
end

(** Int32 overloadings and optimized functions. *)
module Int32 :
sig
  val overloading : Delimited_overloading.t

  val eq : Loc.t -> expr
    (** An expression for the equality function [( = ) : int32 ->
        int32 -> bool], optimized. *)
  val ne : Loc.t -> expr  (** same as [eq] but for [<>] *)
  val lt : Loc.t -> expr  (** same as [eq] but for [<] *)
  val le : Loc.t -> expr  (** same as [eq] but for [<=] *)
  val gt : Loc.t -> expr  (** same as [eq] but for [>] *)
  val ge : Loc.t -> expr  (** same as [eq] but for [>=] *)
end

(** Int64 overloadings and optimized functions. *)
module Int64 :
sig
  val overloading : Delimited_overloading.t

  val eq : Loc.t -> expr
    (** An expression for the equality function [( = ) : int64 ->
        int64 -> bool], optimized. *)
  val ne : Loc.t -> expr  (** same as [eq] but for [<>] *)
  val lt : Loc.t -> expr  (** same as [eq] but for [<] *)
  val le : Loc.t -> expr  (** same as [eq] but for [<=] *)
  val gt : Loc.t -> expr  (** same as [eq] but for [>] *)
  val ge : Loc.t -> expr  (** same as [eq] but for [>=] *)
end

(** Nativeint overloadings and optimized functions. *)
module Nativeint :
sig
  val overloading : Delimited_overloading.t

  val eq : Loc.t -> expr
    (** An expression for the equality function [( = ) : nativeint ->
        nativeint -> bool], optimized. *)
  val ne : Loc.t -> expr  (** same as [eq] but for [<>] *)
  val lt : Loc.t -> expr  (** same as [eq] but for [<] *)
  val le : Loc.t -> expr  (** same as [eq] but for [<=] *)
  val gt : Loc.t -> expr  (** same as [eq] but for [>] *)
  val ge : Loc.t -> expr  (** same as [eq] but for [>=] *)
end


(** Float overloadings and optimized functions *)
module Float :
sig
  val overloading : Delimited_overloading.t
    (** In addition to the above overloadings, the usual float
        functions like [sqrt], [exp], [cos], [sin], [acos],
        [floor],... are the [Pervasives] ones.  Also [abs] is an alias
        for [abs_float].  [max] and [min] are optimized for floats and
        ignore [nan] (i.e.  [max x nan = max nan x = x], [min x nan =
        min nan x = x]) as mandated by
        {{:http://en.wikipedia.org/wiki/IEEE_754r}IEEE Std 754-2008}.
        The function [is_nan] is available.  The power operator is
        specialized so that writing e.g. [x**2] is translated into [x
        *. x].  The constant [pi] and function [hypot] are available. *)

  val eq : Loc.t -> expr
    (** An expression for the equality function [( = ) : float ->
        float -> bool], optimized for floats.  *)
  val ne : Loc.t -> expr  (** same as [eq] but for [<>] *)
  val lt : Loc.t -> expr  (** same as [eq] but for [<] *)
  val le : Loc.t -> expr  (** same as [eq] but for [<=] *)
  val gt : Loc.t -> expr  (** same as [eq] but for [>] *)
  val ge : Loc.t -> expr  (** same as [eq] but for [>=] *)
  val compare : Loc.t -> expr
    (** Relocatable expression for a specialized comparison function. *)

  val neg : Loc.t -> expr
    (** An alternate identifier for [~-.] that performs float unary
        negation even if the identifier [~-.] has been redefined. *)
  val add : Loc.t -> expr
    (** An alternate identifier for [+.] that performs float addition
        even if the identifier [+.] has been redefined. *)
  val sub : Loc.t -> expr  (** Similar to [add] but for [-.] *)
  val mul : Loc.t -> expr  (** Similar to [add] but for [*.] *)
  val div : Loc.t -> expr  (** Similar to [add] but for [/.] *)
  val pow : Loc.t -> expr  (** Similar to [add] but for [**] *)
  val abs : Loc.t -> expr  (** Similar to [add] but for [abs_float] *)

  val max : Loc.t -> expr  (** Optimized implementation for [max] *)
  val min : Loc.t -> expr  (** Optimized implementation for [min] *)

  val pi :  Loc.t -> expr  (** Expression for the constant pi *)
  val hypot : Loc.t -> expr (** Expression for [hypot x y] which
                               computes [sqrt(x *. x +. y *. y)] without
                               undue underflow or overflow. *)
end

(** Overloading and optimizations for the Complex module.  *)
module Complex :
sig
  val overloading : Delimited_overloading.t
    (** In addition to the features listed at the top of this module,
        [overloading_complex] allows the notation [I] to designate the
        pure imaginary complex number and [x I], where [x] is a
        literal integer or float, to mean [x] times [I].  Expressions
        like
        {[
        a + bI
        ]}
        where [a] and [b] are float of integer literals, are
        transformed into [{ Complex.re = a; Complex.im = b }].  The
        functions [**], [conj], [norm], [norm2], [arg], [sin], [cos],
        [exp], [log] are also overloaded and can be used without
        prefixing them by [Complex.].  [abs] (resp. [abs2]) can be
        used as an alias for [norm] (resp. [norm2]).  Comparison
        operators [<=], [<], [>=], [>] can be used (it is assumed
        their operand is real; if it can be detected that one is
        complex, a parsing error is raised).

        You can use [z.re] and [z.im], where [z] is a complex
        expression, to take the real and imaginary parts of [z].  Two
        "inverse" functions are provided: [re : float -> Complex.t]
        (resp. [im : float -> Complex.t]) that takes a float [x] to a
        complex with real (resp. imginary) part [x].  Operations are
        optimized so that writing [re x + im y] or [re x + re y * I]
        is equivalent to [{Complex.re = x; im = y}].

        The expression [float z] will return the float contained in
        the complex [z].  Expressions like [arg z] or [norm z] return
        real numbers embedded in the complex type, you must write
        [float(arg z)] to "convert" them to float.  If it cannot be
        determined from the expression that it is real (like [float
        z.re] or [float(arg ...)]), a parsing exception will be
        raised.  Note that inequalities have the same behavior, so you
        can write [norm z <= 3] and the right code will be generated.

        Complex expressions written with these overloadings are never
        be less efficient that the ones written with the complex module.

        Remark: The literal [-0-0I] means [(-0)+(-0)I] and so is the
        complex number [{ Complex.re = (-0.); im = (-0.)}].  The sign
        of zeros is important for some complex operations. *)

end


val overloading_hashtbl : Delimited_overloading.t
  (** Allows to use [h.(k)] for [Hashtbl.find h k] and [h.(k) <- v]
      for [Hashtbl.replace h k v].  You can also use [clear],
      [create], [fold], [iter] and [length] without prefix. *)

val overloading_string : Delimited_overloading.t
  (** Trivial overloading to restore the usual meaning of string
      operations.  Specializes [compare]. *)
