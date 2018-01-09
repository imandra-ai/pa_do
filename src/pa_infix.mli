(* File: pa_infix.mli

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

(** Syntax extension module to change the priority or associativity of
    unary and binary operators.  As these characteristics of operators
    are global for a source file, the API also checks that no
    conflicts occur.

    There are two ways of using this syntax extension.

    {6 1. Using the API}

    Create a file, say my_syntax.ml, in which you [open Pa_infix] and
    use the API below to set the operators you need, compile it to an
    object file and pass the latter to camlp4.  The advantage of this
    approach is that the created module can be reused with various
    source files.  This is the recommended way of proceeding if you
    want to define new operators to ship with your OCaml library.

    {6:concrete 2. Using the concrete syntax}

    This approach is particularly useful when you quickly need to
    define prefix/postfix/infix operators in a given source file.  The
    syntax is [INFIX ( op )], [PREFIX ( op )], and [POSTFIX ( op )] to
    respectively define binary infix, unary prefix, and unary postfix
    operators.  These can be followed by a precedence specification:
    [HIGHEST], [HIGHER ( op )], [LEVEL ( op )], [LOWER ( op )], or
    [LOWEST].  Finally the associativity of infix operators can be set
    with [LEFTA] (left associative), [RIGHTA] (right associative), or
    [NONA] (non-associative).  Operators at the same level must have
    the same associativity so associativity is inherited from [op]
    when you use [LEVEL ( op )] (you cannot specify it).

    Examples:
    {[
    INFIX ( %+ ) RIGHTA HIGHER (+)
    INFIX ( ^* ) LEVEL (+)
    PREFIX ( /+/ )
    POSTFIX ( /// ) LEVEL ( ! )
    ]}

    In case the concrete syntax conflicts with keywords in your code,
    it can be disabled by passing the option [-no-pa-infix] to camlp4
    (in which case only the first approach is possible).

    {b Remarks}

    - Standard OCaml operators ([+], [-],..., [+.],...) cannot be
    changed.  This is to prevent strange bugs from occurring by
    changing the meaning of code that does not use new operators
    (e.g. one certainly does not want [+] to bind more tightly than
    [*] or [-] to be right associative).

    - Alphanetic operators are possible.  Beware that setting an
    alphabetic operator makes it a keyword, thus preventing it to be
    used as a variable name anywhere else.  To define an alphabetic
    operator, say [op], use the syntax [let ( op ) args = expr] --
    just as you would do to redefine existing alphabetic operators
    such as [lsl],...  Beware that prefix and postfxix operators bind
    more tightly than function evaluation (think of the dereference
    operator [!]).  Use with care.

    - If an operator is added even with its default level, the
    operator will be considered as being set and it will not be
    possible to change it further. *)

open Camlp4.PreCast.Syntax.Ast

(** {1:api Pa_infix API} *)

type operator = string
    (** Operators represented as their concrete string syntax. *)

exception Not_operator of operator
  (** [Not_operator op] is raised if the string [op] does not
      designate a valid operator name.  *)

exception Forbidden of operator
  (** [Forbidden op] is raised when the operator [op] belongs to the
      operators which precedence or associativity one is not allowed
      to change.  *)

(** Level of precedence of operators (also specifies their
    associativity).  *)
module Level :
sig
  type unary
  type binary
  type 'a t
    (** A precedence level.  Can be for unary or binary operators.
        Precedence levels know their associativity. *)

  (** Arity of a level.  Levels for unary and binary operators are
      different (unary operators always bind more tightly than
      binary ones). *)
  type arity = Unary | Binary

  (** Possible associativity values for binary infix operators. *)
  type assoc = Camlp4.Sig.Grammar.assoc = NonA | RightA | LeftA

  (** Relative precedence of new levels. *)
  type 'a precedence =
    | Highest        (** Binds tighter than all previously defined levels
                         of the same arity. *)
    | Higher of 'a t (** [Higher l] binds tighter than the level [l]. *)
    | Lower of 'a t  (** [Lower l] binds less tight than [l] *)
    | Lowest         (** Binds less tight than all previously defined levels
                         of the same arity. *)

  exception Bad_arity
    (** [Bad_arity] is raised if the type of level required by a function
        is not respected. *)

  val compare : 'a t -> 'b t -> int
    (** Total ordering on Level.t *)

  val binary : ?name:string -> ?assoc:assoc -> binary precedence -> binary t
    (** [binary prec] creates a new level for infix operators at the
        level specified by [prec].

        @param name the name of the new level (only used for error reporting).
        @param assoc the associativity of the new level.  By default,
        the associativity is inherited from the the level given in
        [prec] or is [NonA] for [Highest] and [Lowest].
        @raise Bad_arity if a ['_a precedence] is given which does not
        turn out to be a [binary precedence]. *)

  val unary : ?name:string -> unary precedence -> unary t
    (** [unnary prec] creates a new level for prefix or postfix
        operators at the level specified by [prec].

        @param name the name of the new level.
        @raise Bad_arity if a ['_a precedence] is given which does not
        turn out to be a [unary precedence].  *)

  val name : 'a t -> string
    (** [name l] returns the possible name given to the level [l]. *)

  val arity : 'a t -> arity
    (** [arity l] returns the arity of that level. *)

  val assoc : 'a t -> assoc
    (** [assoc l] returns the associativity of that level. *)

  val assignment :  binary t    (** Level of ":=" *)
  val disjunction : binary t    (** Level of "||" *)
  val conjunction : binary t    (** Level of "&&" *)
  val comparison : binary t     (** Level of "<", "<=", "=",... *)
  val concatenation : binary t  (** Level of "^" *)
  val addition : binary t       (** Level of "+", "-",... *)
  val multiplication : binary t (** Level of "*", "/", "land",...
                                and it is also the default level of
                                alphabetic operators. *)
  val exponentiation : binary t (** Level of "**", "lsl",... *)
  val default_unary : unary t   (** Level of "!" *)
end

(** Possible kinds of operators. *)
type kind = Prefix | Infix | Postfix

exception Conflicting_kind of operator * kind * kind
  (** [Conflicting_kind op k0 k] is raised when the kind of the operator
      [op] was [k0] but now trying to set it to [k].*)

exception Conflicting_level of operator * string * string
  (** [Conflicting_level op l0 l] is raised when the level of the operator
      [op] was [l0] but now trying to set it to [l].*)

val infix : operator -> ?expr:(expr -> expr -> Loc.t -> expr) ->
  Level.binary Level.t -> unit
  (** [infix op l] defines the operator [op] as a binary infix
      operator at level [l].

      @param expr the function performing the operator subsititution:
      the expression [x op y] is transformed into [expr x y _loc].  It
      is the rewriting rule associated to [x op y].  Note that this
      can be used to simplify expressions [x op y] but defines in no
      way the {i function} [op] ; if you want [op] also to work in
      context where no arguments are provided immediately, you need to
      bind [op] to a compatible function: [let (op) = ...].  The
      default is to keep the function application: [fun x y _loc ->
      <:expr< $lid:op$ $x$ $y$ >>].

      @raise Not_operator if [op] is not a valid operator name.
      @raise Forbidden if the operator [op] is not allowed.
      @raise Conflicting_kind if there is a conflict with the kind of [op].
      @raise Conflicting_level if there is a conflict with the level of [op].
      @raise Level.Bad_arity if a ['_a Level.t] is given which is not
      a [Level.binary Level.t].  *)

val prefix : ?expr:(expr -> Loc.t -> expr) ->
  ?level:Level.unary Level.t -> operator -> unit
  (** [prefix op l] define the operator [op] as a prefix operator at
      level [l].  Remark: if you use a prefix operator between braces,
      make sure to put spaces between around the expressions.  For
      example, assuming you set [/] as prefix, [(/4)] is parsed as
      "(/", "4" and ")", you have to write [( /4 )] or [( / 4 )] for
      [/] to be considered as a prefix operator.  Note that, usually,
      you will not need any braces as prefix operators bind more
      tightly than evaluation: [f /4 5] will be interpreted as [f ( /4 ) 5].

      @param expr the function performing the operator subsititution:
      the expression [op x] is transformed into [expr x _loc].
      Default: [fun x _loc -> <:expr< $lid:op$ $x$ >>].

      @raise Not_operator if [op] is not a valid operator name.
      @raise Forbidden if the operator [op] is not allowed.
      @raise Conflicting_kind if there is a conflict with the kind of [op].
      @raise Conflicting_level if there is a conflict with the level of [op].
      @raise Level.Bad_arity if a ['_a Level.t] is given which is not
      a [Level.unary Level.t].  *)

val postfix :  ?expr:(expr -> Loc.t -> expr) ->
  ?level:Level.unary Level.t -> operator -> unit
  (** [postfix op l] define the operator [op] as a postfix operator at
      level [l].  The same remarks as for [prefix] apply.

      @param expr the function performing the operator subsititution:
      the expression [x op] is transformed into [expr x _loc].
      Default: [fun x _loc -> <:expr< $lid:op$ $x$ >>].

      @raise Not_operator if [op] is not a valid operator name.
      @raise Forbidden if the operator [op] is not allowed.
      @raise Conflicting_kind if there is a conflict with the kind of [op].
      @raise Conflicting_level if there is a conflict with the level of [op].
      @raise Level.Bad_arity if a ['_a Level.t] is given which is not a
      [Level.unary Level.t].  *)

val assoc : operator -> Level.assoc
  (** [assoc op] returns the associativity of the operator [op].
      @raise Not_operator if [op] is not a valid operator name. *)

val level : operator -> 'a Level.t
  (** [pos op] returns the (default or assigned) level of the operator [op].
      @raise Not_operator if [op] is not a valid operator name. *)

val kind : operator -> kind
  (** [kind op] returns the (default or assigned) kind of the operator [op].
      @raise Not_operator if [op] is not a valid operator name. *)

val is_operator : operator -> bool
  (** [is_operator op] tells whether the string [op] can be used as an
      operator. *)

val is_set : operator -> bool
  (** [is_set op] tells whether the operator [op] has been set using
      one of the functions of this API.
      @raise Not_operator if [op] is not a valid operator name. *)

val handle_error : ('a -> 'b) -> 'a -> 'b
  (** [handle_error f a] applies [f] to [x] and returns the result.
   If an exception of this library is raised, it prints a message
   describing the error and exits with code 2.  *)
