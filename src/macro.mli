(* File: delimited_overloading.mli

   Copyright (C) 2008

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

(** (sub-module of [Pa_do]) Improved version of macros that
    play well with overloading.  You can use the following:

    At toplevel (structure item):
    {[
    DEFINE <uident>
    DEFINE <uident> = <expression>
    DEFINE <uident> (<parameters>) = <expression>
    IFDEF <uident> THEN <structure_items> [ELSE <structure_items>] (END|ENDIF)
    IFNDEF <uident> THEN <structure_items> [ELSE <structure_items>] (END|ENDIF)
    INCLUDE <string>
    ]}

    In expressions:
    {[
    IFDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
    IFNDEF <uident> THEN <expression> [ ELSE <expression> ] (END | ENDIF)
    DEFINE <lident> = <expression> in <expression> (* in discussion *)
    INCLUDE_AS_STRING <string> (* turn the file content into a string *)
    __FILE__
    __LOCATION__
    ]}

    Macros can take parameters starting with a lowercase -- to to be
    substituted by expressions -- and parameters starting with an
    uppercase -- to be substituted by module names.  The latter
    allows, for example, to delay overloadings until macro
    application.  Thus you can write:
    {[
    DEFINE F(M, x) = let y = M.(1 + x) in M.float y
    ]}
    and then use is as [F(Int64, 1L)].  You can also use this feature
    to parametrise code to be instantiated with different modules
    (i.e. use it as a poor man defunctorizer): for example, after
    after [DEFINE F(M, y) = M.add (x : M.t) y], [F(Int32, 1l)]
    becomes [Int32.add (x : Int32.t) 1l].

    Macros are lexically scoped: previously defined macros will be
    expanded in the body of the current macro definition but only the
    macro parameters will be substituted at the point of use of the
    macro (also free variables in the body of the macro will be
    resolved at the point of use of the macro).  If you want to change
    what the body of a macro does and passing a function as a macro
    parameter is not enough, you can pass another macro as a parameter
    by using the following syntax :
    {[
    DEFINE F(..., X(),...) = ... X(a1,...,aN) ...
    DEFINE G(b1,...,bN) = ...
    ... F(..., G,...) ... (* use F passing the macro G as a parameter *)
    ]}
    The interesting point is that [G] may not use some of its
    arguments (which will therefore not be evaluated), so it is
    possible to setup a macro for a general computation and perform
    only parts of it if needed through a good choice of the macro
    passed as parameter.

    The toplevel statement INCLUDE <string> can be used to include a
    file containing macro definitions and also any other toplevel items.
    The included files are looked up in directories passed in via the -I
    option, falling back to the current directory.


    Strong points of [Macro]:

    - Special attention is payed to the locations with a trick to ease
    the debugging of code using macros.

    - Proper handling of lazy evaluation of macros in [IFDEF] constructs.

    - It has an API enabling libraries to be bundled with syntax
    extensions that provides new macros.  The hooks also allow other
    syntax extensions to cooperate with macros (e.g. for macros
    conditionals).

    Altough [Macro] does not provide type arguments (e.g. for type
    annotations), it is easy to work around this limitation by using
    the parametrisation by a module.
*)

open Camlp4.PreCast.Syntax.Ast

exception Invalid of string

(** Types of parameters accepted by macros. *)
type param =
  | Lid of string (** Lowercase parameters to be substituted by expressions. *)
  | Uid of string (** Capitalized parameters to be substituted by module
                      longidents. *)
  | Macro of string (** Uppercase macros names to be expanded in the current
                        macro body. *)
  | Unused (** Unused parameter.  Interesting for example for macros
               with several definitions selected defined conditionally
               which may not use all their parameters. *)

val define : ?expr:(param list * expr) -> string -> unit
  (** [define name] defines the new macro [name] which has no body.
      [define name ~expr:(params, body)] defines the new macro [name]
      with the formal parameters [params] and the body [expr].

      @raise Invalid if [name] is not made of uppercase letters. *)

val undef : string -> unit
  (** [undef name] undefines the macro [name].
      @raise Invalid if [name] is not made of uppercase letters. *)

val is_defined : string -> bool
  (** [is_defined name] returns [true] iff the macro [name] is defined.
      @raise Invalid if [name] is not made of uppercase letters. *)

(** Various callbacks for syntax extensions to cooperate nicely with
    macros.  Hooks are run in the same order they were declared.  *)
module Hook :
sig

  val define_params : (param list -> unit) -> unit
    (** Register a hook to be run after the parameters of a macro have
        been read. *)

  val define : (param list -> expr -> expr) -> unit
    (** Register hook [f] to be run after the body of a macro has been
        read; [f params boby] returns the new body of the macro.  If
        several hooks are registered, they are applied in the order of
        declaration (each receiving the body transformed by the
        previous one). *)

  val macro_expansion : (expr -> expr) -> unit
    (** At a macro call site, we substitute the macros parameters in
        the body [e] of the macro and then apply all functions
        registered with [macro_expansion] in the order they were given
        to obtain the final expression. *)
end


(** {2 Utilities} *)

exception Invalid_identifier of string

val is_lowercase_identifier : string -> bool
  (** [is_lowercase_identifier lid] returns true if the string [lid]
      is a lowercase identifier. *)

val is_capitalized_identifier : string -> bool
  (** Returns true if the string is a capitalized (i.e. module)
      identifier. *)

val is_uppercase_identifier : string -> bool
  (** Returns true if the string is an uppercase (i.e. macro)
      identifier. *)

(** Management of module longidents (aka module paths). *)
module Module_longident :
sig
  type t

  exception Invalid of string
    (** [Invalid m] is raised to indicate that the string [m] is not a
        valid module longident.  *)

  val of_string : string -> t
    (** Return the module longident associated with the given string.
        @raise Module_longident.Invalid if the string has not the correct
        format. *)

  val to_string : t -> string
    (** Return a string representing the module longident.
        @raise Module_longident.Invalid if the string has not the correct
        format. *)

  val is : string -> bool
    (** [is s] returns [true] iff [s] represents a module longident. *)

  val to_id : t -> Loc.t -> ident
    (** [to_id m loc] converts [m] to an identifier like [<:ident<
        A.B.C >>]. *)

  val of_id : ident -> t
    (** [of_id] returns the module longident corresponding to an
        identifier like [<:ident< A.B.C >>].  It is the inverse of
        [to_id].
        @raise Invalid_argument if the identifier does not have the right
        shape. *)

  val of_list : string list -> t
    (** Return the longident from the list of components. *)

  val qualify_lid : string -> t -> Loc.t -> expr
    (** [qualify_lid lid m _loc] returns the expression of the operator
        or function name [lid] qualified with the module longident [m].  *)

  val qualify_uid : string -> t -> Loc.t -> expr
    (** [qualify_uid uid m _loc] returns the expression of the
        constructor [uid] qualified with the module longident [m].  *)

  val qualify : ident -> t -> ident
    (** [qualify i] returns the identifier [i], e.g. [<:ident<
        A.B.c >>], qualified with the module longident [m].  The
        localization is taken from the identifier. *)


  val compare : t -> t -> int
    (** Total comparison function on longidents. *)
end
