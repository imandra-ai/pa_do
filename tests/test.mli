(* File: test.mli

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


(** Module to allow automatic tests of this syntax extension.  *)

open Camlp4.PreCast.Syntax.Ast

module Var:
sig
  type t

  val make : ?libs:string list -> ?to_string:('a -> string) ->
    ?input_val:expr -> (unit -> 'a) -> string -> t
    (** [make random var] creates a new variable named [var] and the
        function to generate random values for this variable is given
        by [random].
        @param libs the list of useful libraries.
        Default: [].
        @param to_string the function that converts your value into a string
        Default: fun v -> Marshal.to_string v [Marshal.Closures].
        @param input_val the expression of the function that reads your
        value from a channel.
        Default: [<:expr\@here< Marshal.from_channel>>] *)

  val int : ?bound:int -> ?random:(unit -> int) -> string -> t
    (** [int var] creates a new variable of type int named [var].
        @param bound the bound for random values.
        Default: 10_000.
        @param random the function that returns random values.
        Default: fun () -> Random.int bound. *)

  val int32 : ?bound:int32 -> ?random:(unit -> int32) -> string -> t
    (** [int32 var] creates a new variable of type int32 named [var].
        @param bound the bound for random values.
        Default: 10_000l.
        @param random the function that returns random values.
        Default: fun () -> Random.int32 bound. *)

  val int64 : ?bound:int64 -> ?random:(unit -> int64) -> string -> t
    (** [int64 var] creates a new variable of type int64 named [var].
        @param bound the bound for random values.
        Default: 10_000L.
        @param random the function that returns random values.
        Default: fun () -> Random.int64 bound. *)

  val nativeint : ?bound:nativeint -> ?random:(unit -> nativeint) -> string -> t
    (** [nativeint var] creates a new variable of type nativeint named
        [var]. @param bound the bound for random values.
        Default: 10_000n.
        @param random the function that returns random values.
        Default: fun () -> Random.nativeint bound. *)

  val float : ?bound:float -> ?random:(unit -> float) -> string -> t
    (** [float var] creates a new variable of type float named [var].
        @param bound the bound for random values.
        Default: 10_000..
        @param random the function that returns random values.
        Default: fun () -> Random.float bound.*)

  val string : ?bound:int -> ?random:(unit -> string) -> string -> t
    (** [string var] creates a new variable of type string named [var].
        @param bound the maximum length for random values.
        Default: 500.
        @param random the function that returns random values.
        Default: begin fun () ->
        let len = Random.int bound in
        let s = ref "" in
        for i = 1 to len do
          let char = Char.chr (Random.int 256) in
          s := !s ^ (Char.escaped char)
        done;
        !s end. *)
end

module Compare_val :
sig
  type t

  val make : ?libs:string list -> ?comp_no:int -> expr -> t
    (** [make equal] return a new way to compare values of tests that
        uses the expression [equal] to perform the test.  Accordingly,
        the [equal] expression must be a function that takes two
        arguments and tells whether they are equal or not.

        @param comp_no the number of comparisons that will be made,
        i.e. on how many random values the equality will be tested
        (default: inherited from the group one puts the test in).

        @param libs the possible libraries that are needed to perform
        the test. *)

  val bool : ?comp_no:int -> unit -> t
    (** [bool()] standard boolean comparison.
        @param comp_no the number of comparisons that will be made.  *)

  val int : ?comp_no:int -> unit -> t
    (** [int()] enables to compare the int values of two expressions.
        @param comp_no the number of comparisons that will be made.
        Equality function: [Pervasives.(=)]. *)

  val int32 : ?comp_no:int -> unit -> t
    (** [int32()] enables to compare the int32 values of two expressions.
        @param comp_no the number of comparisons that will be made.
        Equality function: use of [Int32.compare]. *)

  val int64 : ?comp_no:int -> unit -> t
    (** [int64()] enables to compare the int64 values of two
        expressions.
        @param comp_no the number of comparisons that will be made.
        Equality function: use of [Int64.compare]. *)

  val nativeint : ?comp_no:int -> unit -> t
    (** [nativeint()] enables to compare the nativeint values
        of two expressions.
        @param comp_no the number of comparisons that will be made.
        Equality function: use of [Nativeint.compare]. *)

  val float : ?comp_no:int -> ?eps:float -> unit -> t
    (** [float()] enables to compare the float values of two
        expressions.
        @param comp_no the number of comparisons that will be made.
        @param eps the precision between two floats. (Default: 1e-12)
        Equality function: [fun a b -> abs_float (a -. b) < eps]. *)

  val complex : ?comp_no:int -> ?eps:float -> unit -> t
    (** [complex()] enables to compare the complex values of two
        expressions.
        @param comp_no the number of comparisons that will be made.
        @param eps the precision between two real parts and two imaginary
        parts. (Default: 1e-12)
        Equality function: equality of the two float real parts and the two
        float imaginary parts. *)

  val string : ?comp_no:int -> unit -> t
    (** [string()] enables to compare the string values of two
        expressions.
        @param comp_no the number of comparisons that will be made.
        Equality function: [Pervasives.(=)]. *)

  val array : ?comp_no:int -> t -> t
    (** [array t] enables to compare the array values of two
        expressions thanks to [t], the equality function of
        elements of the arrays.
        @param comp_no the number of comparisons that will be made.
        Equality function: [fun a b ->
        Array.length a = Array.length b &&
          (let is_eq = ref true in
          Array.iteri (fun i v -> if not(($eq$) v b.(i)) then is_eq := false) a;
          !is_eq)] where [eq] is the equality function between componants
        given by [t]. *)

  val list : ?comp_no:int -> t -> t
    (** [list t] enables to compare the list values of two
        expressions thanks to [t], the equality function of
        elements of the lists.
        @param comp_no the number of comparisons that will be made.
        Equality function: [fun a b ->
        List.length a = List.length b &&
          (let is_eq = ref true in
          List.iter2 (fun v_a v_b -> if not(($eq$) v_a v_b)
             then is_eq := false) a b;
          !is_eq)] where [eq] is the equality function between componants
        given by [t]. *)
end

type test
  (** Abstract type standing for a single test or a group of tests. *)

val group : string -> ?libs:string list -> ?header:str_item ->
  ?comp_no:int -> test list -> test
  (** [group name l]

      @param libs the libraries needed to compile the tests in [l].
      @param header code inserted at the beginning of all tests in [l].
      @param comp_no the number of comparisons made to compare the values
      of two expressions. It is used if the argument [comp_no] is not
      given to functions [Compare_val.make], [Compare_val.int],...
      Default: 100 comparisons. *)

(** Possible test cases.  See {!Test.api} for their meaning. *)
type api_test_case =
  [ `Raise of patt
  | `Compare_expr of (string * expr) list
  | `Compare_val of (Var.t list * Compare_val.t * string * expr) list ]

val api : string -> expr -> api_test_case -> test
  (** [api name stm cases] is a test consiting in running camlp4
      with the loaded module consisting in the statements [stm] on
      each of the test [cases].  The module will inherit the header of
      the groups the test is put in.  The tests cases may consist
      either in

      - [`Raise el] meaning it is expected that [stm] raises one of the
      exceptions in the list [el];
      - [`Compare_expr l], where [l] is a list of couples [(e1, e2)],
      meaning that camlp4 will be used to parse the expressions [e1] and
      must return the AST [e2] to succeed.
      - [`Compare_val l], where [l] is a list of tuples [(var,compare,
      e0, e1)], meaning that random values will be given to variables
      in the list [var] and equality of the expressions [e0] (with
      overloadings) and [e1] (a expression supposedly equivalent to
      [e0] with no overloadings) given by [compare] will be tested.

      The [stm] can easily be written with an antiquotation.  The
      expression [stm] must be of type [unit].  We advise you to use
      the following syntax:
      {[
      <:expr\@here< (stm1;
                     stm2;
                    ) >>
      ]}
      The location [\@here] will avoid the error "Unbound value _loc"
      and, more importantly, allow the system to report a correct
      location in case you made an error in the expression.  Strictly
      speaking the braces are not needed but they will allow statements
      to be nicely aligned and will prevent to be bitten by a
      final colon (as the one after [stm2] above).

      Remember that the expression will be transformed into an AST
      while compiling the test module, so it will be parsed with the
      OCaml syntax -- in particular, the tested syntax extension will
      not be active. *)

type concrete_test_case =
    [ `Camlp4_error of
      (string * int * int * int * int * int * int * bool) option
      * bool option * string
    | `Compare_expr of (string * expr) list
    | `Compare_val of (Var.t list * Compare_val.t * string * expr) list ]

val concrete_syntax : string -> string -> concrete_test_case -> test
  (** [concrete_syntax name stm cases] build a test named [name]
      consisting of running the code [stm] (e.g. for defining some
      macros) and performing all the test cases.  The tests may be:

      - [`Camlp4_error loc with_loc msg] meaning it is expected that
      Camlp4 exits on running [stm] with an error message that is
      matched by the regular expression given by [msg].  The boolean
      [with_loc] specifies that the error message starts with a
      location, or not. [loc] is the location of this test case, it is
      of the form [(file_name, start_line, start_bol, start_off,
      stop_line, stop_bol, stop_off, ghost)].
      - [`Compare_expr(concrete, expr)] checks that the value
      [concrete] expands (under the effect the syntax extension
      possibly influenced by [stm]) to [expr].
      - [`Compare_val(vars, cmp_fn, concrete, expr)] compares the
      value defined by the [concrete] expression and the one given by
      [expr].  The function [cmp_fn] is used to determine whether two
      values must be considered equal.  [concrete] and [expr] may both
      contain the variables [vars] in which case the two values will
      be compared for various random values given to the variables
      (the number of comparisons is given by the parameter [comp_no]
      of {!Test.group}).

      See {!Test.api} for the meaning of [`Compare_expr] and
      [`Compare_val]. *)

val run : ?ocamlc:string -> ?camlp4:string -> test -> unit
  (** [run t] runs all the tests contained in [t]. It can abort without
      running the tests if the needed programs ocamlc and camlp4 are
      not found.
      @param ocamlc the full path to ocamlc. By default, it is "ocamlc".
      @param camlp4 the full path to camlp4. By default, it is "camlp4". *)



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
