(* File: delimited_overloading.mli

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

(** (submodule of [Pa_do]) Syntax extension module to enable easy {i
    local} overloading of operators and functions.  The principle is
    simple: expressions like [X1.X2...Xk.(expr)] are automatically
    transformed, substituting constants, operators, and functions in
    [expr] and possibly optimizing the resulting expression.  For
    example, you can write [Float.(1 + x / 3)] and it will be just as
    if you wrote [1.0 +. x /. 3.0].

    This library comes with overloadings of arithmetic and comparison
    operators for all standard OCaml numeric types, namely [Int],
    [Float], [Complex], [Int32], [Int64], [Nativeint], [Num],
    [Big_int], and [Ratio] (the latter three are in {!Pa_do_nums}
    since they require to load [nums.cma] for compile time checks).
    Where applicable the functions [lsl], [lsr], [land], [lor],
    [succ], [pred], [floor], [max], [min], [abs], [truncate],... are
    also overloaded.

    There are two ways of using this syntax extension to define your
    own overloadings.

    {b 1. Creating a camlp4 syntax module}

    Here you create a file, say my_syntax.ml, and use the API below to
    define your overloadings.  This is the recommended and more
    powerful way.

    {b 2. Using the concrete syntax}

    From time to time, you'll find that you need a quick way to
    overload operators for a module you just defined.  In this case,
    you can the concrete syntax which allows simple overloading to be
    defined.  Given a module longident [X] (i.e. [X] has the form
    [X1.X2...Xk] for some modules [X1], [X2],..., [Xk]), you can use
    {[
    OVERLOAD X (op1 -> f1;  op2 -> f2; ...)
    ]}
    to mean that [op1] should be replaced by [X.f1], etc.  Note that,
    if unqualified, the functions [f1], [f2],... will be automatically
    qualified by the module [X].  They are untouched if they are
    prefixed by a module (hence the complete module path must then be
    provided).  For example, if you set [OVERLOAD X((+) -> add; succ ->
    succ)], then the expression [X.(x + succ y)] will be transformed
    into [A.add x (X.succ y)].  The shortcut [OVERLOAD X (f)] means
    [OVERLOAD X (f -> f)]. You can overload literal values using
    {[
    OVERLOAD_INT X(f)        (* the argument of f is an int *)
    OVERLOAD_FLOAT X(f)      (* the argument of f is a float *)
    OVERLOAD_INT32 X(f)      (* the argument of f is an int32 *)
    OVERLOAD_INT64 X(f)      (* the argument of f is an int64 *)
    OVERLOAD_NATIVEINT X(f)  (* the argument of f is a nativeint *)
    OVERLOAD_STRING X(f)     (* the argument of f is a string *)
    OVERLOAD_POLY_VAR X(f)   (* the argument of f is a string *)
    ]}
    which will replace any constant [x] with [f x].  The meaning of
    common constructions can also be changed:
    {[
    OVERLOAD_ARRAY_GET X(f)  (* [a.(i)] is transformed into [X.f a i] *)
    OVERLOAD_ARRAY_SET X(f)  (* [a.(i) <- x] is transformed into [X.f a i x] *)
    OVERLOAD_BIGARRAY_GET X(f) (* [a.{i1,...,iN})] is transformed into
                                  [X.f a [| i1;...; iN |]] *)
    OVERLOAD_BIGARRAY_SET X(f) (* [a.{i1,...,iN} <- x] is transformed into
                                  [X.f a [| i1;...; iN |] x] *)
    OVERLOAD_STRING_GET X(f) (* [a.[i]] is transformed into [X.f a i] *)
    OVERLOAD_STRING_SET X(f) (* [a.[i] <- x] is transformed into [X.f a i x] *)
    OVERLOAD_ASSIGN_REF X(f) (* [a := x] is transformed into [X.f a x] *)
    OVERLOAD_RECORD_FIELD X(field1; ...; fieldN) (* qualifies the fields *)
    ]}
    As a convenience, you can use [OVERLOAD_ARITHMETIC X] to overload
    the usual arithmetic operators (see
    {!Delimited_overloading.std_arithmetic}) and [OVERLOAD_COMPARISON
    X(cmp)] to interpret comparison operators using the compare
    function [cmp] (see {!Delimited_overloading.comparison} for more
    details).  As it is a common case, you can use the shortcut
    [OVERLOAD_COMPARISON X] for [OVERLOAD_COMPARISON X(compare)].

    You can add to [X] the overloadings defined in another module
    longident, say [Y], by doing [OVERLOAD X inherit Y].  The
    functions of [Y] will be re-qualified by the module [X]
    (i.e. [Y.f] will become [X.f]).  To avoid that requalification,
    use [OVERLOAD X = Y] (further overloads added to [X] will also be
    qualified by the same module used to qualify the overloadings of
    [Y]).  This latter form is for example useful to use a shorter
    name for a given overloading.  Finally, you can use
    {[
    OVERLOAD_OPENIN X
    ]}
    (resp. [OVERLOAD_OPENIN X(false)]) to enable (resp. disable) the
    opening of the module [M] in [e] for an expression like [M.(e)].

    In all the above directives [X] can have the special value
    [DEFAULT].  Setting overloadings for [DEFAULT] will apply these
    overloadings to all modules for which no specific overloadings
    have been set.

    {i Remark:} Saving constant expressions to avoid evaluating them
    more than once (see {!Delimited_overloading.int}) is not enabled
    for the concrete syntax.  This is because the module to be
    overloaded is usually defined in the same source file and
    therefore will not be known by the bindings put at the beginning
    of the source file.


    @author Dany Maslowski, Julie De Pril, Christophe Troestler
*)

open Camlp4.PreCast.Syntax.Ast


type t
  (** Type representing an immutable "set of overloadings". *)

type module_longident = Macro.Module_longident.t
  (** Type representing a module longident, i.e. a "sequence" of
      modules like [A.B.C].  *)

type reloc_expr = module_longident -> Loc.t -> expr
  (** A relocatable expression, i.e. an expression parametrized by a
      module name and a location.

      The purpose is as follows.  Suppose we want to overload ["+"]
      with [add].  [add] must be qualified with the module to which it
      belongs.  However we do not know yet what that module will be
      because we want to use this overloading with several modules.
      The same goes for the location: for correct error reporting, the
      location of the subtituted expression must be the location of
      ["+"] wich is currently unknown.  Thus the right way to declare
      the overloading of [+] is using [fun m _loc -> qualify_lid add m
      _loc], where [qualify_lid] is explained below.  *)

exception Invalid_identifier of string
  (** [Invalid_identifier m] is raised to indicate that the
      string [m] is not a valid identifier (lowercase or operator).  *)

val empty : t
  (** Empty set of overloadings. *)

val concat : t -> t -> t
  (** [concat t1 t2] "concatenates" the overloadings in [t1] and [t2].
      If a constant, string, identifier,... has been overloaded in
      [t1] and in [t2], then the overloading of [t2] is the one
      present in the result of [concat].  *)

val qualify_lid : string -> reloc_expr
  (** [qualify_lid lid m _loc] returns the expression of the operator
      or function name [lid] qualified with the module longident [m].
      See [reloc_expr] for a use case of this function.  *)


(** {2 Overloading constant expressions} *)

val int : t -> ?cache:bool -> (int -> reloc_expr) -> t
  (** [int t f] return the same "set of overloadings" that [t] with the new
      function [f] that specifies how integer constants are overloaded for [t].

      @param cache if [true] (the default), for any literal [i], binds
      the expression transforming [i] at the beginning of the source
      file so it is only evaluated once.  The overloaded module must
      therefore be know at for the whole source (this is generally the
      case for a library) and it is advised to use fully qualified names.  *)

val float : t -> ?cache:bool -> (float -> reloc_expr) -> t
  (** [float t f]  return the same "set of overloadings" that [t] with the new
      function [f] that specifies how float constants are overloaded for [t].
      @param cache see {!Delimited_overloading.int}.
  *)

val nativeint : t -> ?cache:bool -> (nativeint -> reloc_expr) -> t
  (** [nativeint t f] return the "set of overloadings" [t] augmented with the
      new function [f] that specifies how nativeint constants are overloaded
      for [t].
      @param cache see {!Delimited_overloading.int}.
  *)

val int32 : t -> ?cache:bool -> (int32 -> reloc_expr) -> t
  (** [int32 t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how int32 constants are overloaded for [t].
      @param cache see {!Delimited_overloading.int}.
  *)

val int64 : t -> ?cache:bool -> (int64 -> reloc_expr) -> t
  (** [int64 t f]  return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how int64 constants are overloaded for [t].
      @param cache see {!Delimited_overloading.int}.
  *)

val string : t -> ?cache:bool -> check:(string -> 'a) ->
  ?to_type:(string -> string) -> (string -> reloc_expr) -> t
  (** [string t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how string constants are overloaded for [t].

      @param cache see {!Delimited_overloading.int}.
      @param check a function that checks that the string represent a
      valid value.  If it does not, it is expected that [check] raise
      an exception (which one is unimportant).
      @param to_type Allows to customize the error message in case
      [check] fails.  [to_type] receive the module name and is
      expected to return a string characterizing the type of the
      expected value.  Default: identity. *)

val poly_var : t -> ?cache:bool -> check:(string -> 'a) ->
  ?to_type:(string -> string) -> (string -> reloc_expr) -> t
  (** [poly_var t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how the {i lowercase} polymorphic variants are
      overloaded for [t].  The string argument of [f] is the name of the
      polymorphic variant (for example [abc] is the name of [`abc]).

      @param cache see {!Delimited_overloading.int}.
  *)

val list: t -> (expr list -> reloc_expr) -> t
  (** [list t f]  return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how list constants are overloaded for [t].  *)

val array : t -> (expr list -> reloc_expr) -> t
  (** [array t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how array constants are overloaded for [t].
      Note that [Ast.exSem_of_list] may be useful to group the expressions into
      a single one.  *)

val array_get : t -> (expr -> expr -> reloc_expr) -> t
  (** [array_get t f] return the "set of overloadings" [t] augmented with the
      new function [f] that specifies how the construction [a.(i)] is overloaded
      for [t].  Expressions like [a.(i)], where [a] and [i] are expressions,
      will be transformed into [f a i _loc] where [_loc] is the location of
      [a.(i)].  *)

val array_set : t -> (expr -> expr -> expr -> reloc_expr) -> t
  (** [array_set t f] return the "set of overloadings" [t] augmented with the
      new function [f] that specifies how the construction [a.(i) <- x] is
      overloaded for [t].  Expressions like [a.(i) <- x], where [a], [i], and
      [x] are expressions, will be transformed into [f a i x _loc] where [_loc]
      is the location of [a.(i) <- x].  *)

val string_get : t -> (expr -> expr -> reloc_expr) -> t
  (** Same as [array_get] but for [a.[i]]. *)

val string_set : t -> (expr -> expr -> expr -> reloc_expr) -> t
  (** Same as [array_set] but for [a.[i] <- x]. *)

val bigarray_get : t -> (expr -> expr list -> reloc_expr) -> t
  (** Same as [array_get] but for [a.{i1,...,iK}]. *)

val bigarray_set : t -> (expr -> expr list -> expr -> reloc_expr) -> t
  (** Same as [array_set] but for [a.{i1,...,iK} <- x]. *)


val assign : t -> (expr -> expr -> reloc_expr) -> t
  (** [assign t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how the assignment is overloaded for [t].
      More precisely, expressions [a <- x] will be replaced by [f a x _loc]
      where [_loc] is the location of [a <- x]. *)

val assign_ref : t -> (expr -> expr -> reloc_expr) -> t
  (** [assign t f] return the "set of overloadings" [t] augmented with the new
      function [f] that specifies how the reference assignment is overloaded
      for [t].  More precisely, expressions [a := x] will be replaced by
      [f a x _loc] where [_loc] is the location of [a := x]. *)


(** {2 Simple operator overloading} *)

val lid : t -> ?cache:bool -> string -> reloc_expr -> t
  (** [lid t op e] return the set of overloadings [t] augmented with
      the transformation of the identifier [op] in the relocatable
      expression [e].

      @param cache if [false], camlp4 will just replace [op] with the
      expression [a].  If [true], camlp4 will be bind the expression
      [e] to an identifier and use that identifier for all occurrences
      of [e] (this is interesting if [e] is a complicated function for
      example).  Default: [true] as a very small benefit can be seen
      on [Int32] and [Int64] benchmarks.  *)

val lid_subst : t -> (string * string) list -> t
  (** [lid_subst t ov] return the set of overloadings [t] augmented
      with the transformations of the identifier [op] in the
      identifier [op'] for all [(op, op')] in the list [ov].  The
      identifiers [op'] will be qualified by the module to which the
      result is linked.  In other words, if you set [let t' =
      lid_subst t ["+", "add"]] and [t'] is linked with the module
      longident [M], then [M.(a + b)] will be transformed into [M.add
      a b]. *)

val std_arithmetic : t -> t
  (** [std_arithmetic t] is just a shortcut for [lid_subst t
      [("+","add"); ("-","sub"); ("~-", "neg"); ("*","mul");
      ("/","div")]].  Note that the unary negation, [~-], is
      overloaded as well. *)

val comparison : ?cache: bool -> ?cmp:string -> t -> t
  (** [comparison ?cmp t] return the set of overloadings [t] augmented
      with the transformations for comparison operators [=], [<>],
      [<], [>], [<=], and [>=] using the function name [cmp] to
      overload them.  In other words, the expression [M.(a = b)] will
      be substituted by [M.cmp a b = 0] (where the last equality is
      the standard one on integers), [M.(a < b)] will be transformed
      into [M.cmp a b < 0], etc.

      Associated [min] and [max] functions are also defined.

      @param cmp the name of the comparison function to use.  It will
      be automatically qualified by the module to which the result is
      linked.  Default: [compare].

      @param cache whether to cache the comparison functions built
      from [cmp].  It is generally a good idea to do so
      (e.g. speed-wise).  *)

val lid_remove : t -> string list -> t
  (** [lid_remove t l] return the set of overloadings [t] with all
      overloadings for a lowercase identifier or an operator in the
      list [l] removed.  *)


(** {2 General substitution functions} *)

type transf
  (** Represents a "stack" of expression transformation functions. *)

type transf_fun = transf -> expr -> expr

val transf_module : transf -> module_longident

val transf_qualify : transf -> string -> Loc.t -> expr
  (** [transf_qualify tr lid _loc] qualify [lid] by the module
      longident used to apply the overloadings.  This is a shortcut
      for [qualify_lid lid (transf_module tr) _loc]. *)

val self : transf -> Camlp4.PreCast.Syntax.Ast.map
  (** [self tr] returns the complete transformation object (closed
      complete when using [apply]) allowing to apply it to
      sub-formulas.  For example, to recurse on a subexpression [e],
      use [(self tr)#expr e]. *)

val super : transf_fun
  (** When adding a new transformation, [super tr] is the previous
      transformation that the new one may override. *)

val expr : t -> transf_fun -> t
  (** [expr t f] returns a new set of overloadings which executes
      the transformation [f] first.  The location of [f tr e m] should
      be the same as [e]; use the function [Ast.loc_of_expr] to
      retrieve the location from [e].  A typical function [f] is as
      follows:
      {[
      let f tr e =
        let _loc = Ast.loc_of_expr e in
        match e with
        | ... e ... -> ... (self tr)#expr e ... (* recurse on expression [e] *)
        | ...
        | _ -> super tr e
      ]}
      Note that the recusion on sub-expressions of [e] will apply [f]
      to these subexpressions too.
      The fact that [self tr] is a transversal on the AST allows to
      apply it to various kind of subformulas (e.g. for a recursive
      binding, use [(self tr)#rec_binding]).  *)

val expr_fold : t -> (self:('a -> transf -> map) -> super:('a -> transf_fun) ->
                       'a -> transf_fun) -> 'a -> t
  (** [expr_fold f a0] is the same as {!Delimited_overloading.expr}
      except that it allows to carry down information while parsing
      the AST.  More precisely, the first time [f ~self ~super a tr e
      m] is called, [a = a0] and, when you recurse down the AST with
      [self a tr] or [super a tr], you state that the new value [a]
      must be used for subsequent calls of [f] on the subtree.  The
      preceding [self] and [super] may also be used in cases one wants
      to recurse without changing the value of [a]. *)

val before : t -> (unit -> unit) -> t
(** [before t f] add [f] to the list of functions to execute before
    performing an overloading.  The functions given last are executed
    first.  This is executed once for each construction [M.(e)] where
    [M] is the module name to which [t] is bound.  Recall that
    an overloading may occur inside another one. *)

val after : t -> (expr -> expr) -> t
(** [after t f] perform the transformation [f e] after the overloading
    have been performed -- i.e. [e] is the final expression without
    overloaded expression.  The functions given last are executed
    last.  This is useful for example to prefix the expression with
    some declarations.  *)


(** {2 Miscellaneous} *)

val openin : ?remove:bool -> t -> t
  (** Set (default) or disable (with [~remove:true]) the opening of
      the module [M] for the expression [e] when using [M.(e)].
      Since version 0.8.13, modules are by default opened to be
      compatible with OCaml 3.12 notation [M.(e)]. *)


(** {2 Applying overloadings} *)

val apply : t -> module_longident -> (expr -> expr)
  (** [apply t m e] apply the overloadings in [t] to the expression
      [e].  The overloadings will be resolved for sub-expressions [e']
      of [e] appearing in [e] as [M.(e')] where [M] is the module name
      contained in [m]. *)


(** {2 Attaching overloadings to modules} *)

val associate : t -> ?qualify:string -> string -> unit
  (** [associate_t t m] links the "set of overloadings" [t] to the
      module longident [m].  For example [let t = std_arithmetic empty
      in associate t "I"] implies that [I.(x + y * z)] will be
      transformed into [I.add x (I.mul y z)].

      @param qualify allows to specify a module different from [m] for
      the qualidifcation of the relocatable expressions in the
      overloading [t] (this is interesting if several overloadings are
      defined for a given library).

      @raise Invalid_module_longident if [m] does not have the form of
      a module longident.  *)

(** Default overloadings (initially not set).  When an expression
    [M.(...)] and that no overloadings have been specifically
    associated with the module [M] (using
    {!Delimited_overloading.associate}), the default overloadings are
    used.  If no default overloadings are set, a parsing error is
    raised. *)
module Default :
sig
  val update : (t -> t) -> unit
    (** [update f] updates the set of default overloadings to [f t]
        where [t] is its current value.  If default overloadings were
        not set, [t] is [empty].  *)

  val unset : unit -> unit
    (** Remove all default overloadings.  Trying to use [M.(e)] when
        [M] has no associated overloadings is then an error. *)

  val apply : module_longident -> (expr -> expr)
    (** [apply m e] apply the default overloadings qualified with the
        module [m] to the expression [e].
        @raise Not_found if no default overloadings have been set. *)
end


(** Suspension of overloading for certain modules. *)
module Suspend :
sig
  val add : string -> unit
    (** [add m] suspend overloadings of the module [m].  An internal
        counter is kept that remembers how many time you suspended [m].

        @raise Invalid_module_longident if [m] does not have the form of a
        module longident.  *)

  val remove : string -> unit
    (** [remove m] remove the suspension of overloadings for the
        module [m].  Note that, if you added k times a suspension for
        [m], you must [remove] it k times for the overloading to
        becomes active again.

        @raise Invalid_module_longident if [m] does not have the form of a
        module longident.  *)

  val resolve : expr -> expr
    (** [resolve e] removed all the suspended overloading marks from
        the expression [e], applying the module overloadings
        (according to the currently [associate]d modules). *)
end


(** {2 Utilities} *)

val new_lid : unit -> string
  (** [new_lid()] returns the name of a new lowercase identifier.  *)

val is_immediate : expr -> bool
(** [is_immediate e] says whether the expression [e] is "immediate",
    such as a literal number or string, a variable name, a variable
    name with type annotation, the dereference of a name,... (assuming
    expression is a standard OCaml one).  It is an interesting
    information in order to decide whether the expression needs to be
    bound to a variable to avoid to evaluate it several times.  *)

val add_to_beginning_of_file : str_item -> unit
(** [add_to_beginning_of_file decl] add [decl] (a structure item) to
    the beginning of the parsed file. *)

val overloaded : string
  (** The function name that serves to protect already overloaded
      expressions.  May be removed at any time. *)

val suspended : string
  (** For internal purposes only *)


(** {2 Other similar constructions} *)

val module_square_brackets : (module_longident -> Loc.t -> expr -> expr) ref
  (** Function to be performed on [M.[ e ]]. *)

val module_curly_brackets : (module_longident -> Loc.t -> expr -> expr) ref
  (** Function to be performed on [M.{ e }]. *)


(**/**)

(** {2 Internals} *)

module Overloading :
sig
  val toplevel_reset : unit -> unit
end

(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
