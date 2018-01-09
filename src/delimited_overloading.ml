(* File: delimited_overloading.ml

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

open Camlp4.PreCast
open Syntax.Ast
open Printf
module Longident = Macro.Module_longident

(* debug *)
let print =
  let module P = Camlp4.Printers.OCaml.Make(Syntax)
  in (new P.printer ())#expr Format.err_formatter

(***********************************************************************
 *                            Utilities
 ***********************************************************************)

let not_interactive = not(!Sys.interactive)

(* Generating new identifiers *)
let new_lid =
  Random.self_init();
  let prefix = sprintf "__pa_do_%x_" (Random.int 100_000_000) in
  let no = ref 0 in
  fun () ->
    incr no;
    prefix ^ string_of_int !no

(* Generating new module names *)
let new_uid =
  let prefix = sprintf "Pa_do_%x_" (Random.int 100_000_000) in
  let no = ref 0 in
  fun () ->
    incr no;
    prefix ^ string_of_int !no

let is_immediate = function
  | <:expr< $id:_$ >>  (* already a name *)
  | <:expr< ($id:_$ : $_$) >>  (* name with type annotation *)
  | <:expr< $int:_$ >>
  | <:expr< $flo:_$ >> (* constants, no harm in duplicating them *)
  | <:expr< $str:_$ >> (* string *)
  | <:expr< ! $id:_$ >> (* dereference of a name *)
  | <:expr< ! ($id:_$ : $_$) >>  (* dereference of a name *)
    (* FIXME: may want to add record access x.y.z (but binding it may
       avoid an indirection, so one may limit ourselves to x.f and
       x.A.B...Z.f (field in a module) *)
    -> true
  | _ -> false

let is_symbol c = List.mem c ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/';
                              ':'; '<'; '='; '>'; '?';'@'; '^'; '|'; '~'; '\\']

let is_operator op =
  let l = String.length op in
  let rec loop i = (i = l) || (is_symbol op.[i] && loop (i+1)) in
  l > 0 && loop 0

(* Re-exporting "qualify" functions for the user convenience. *)

let qualify = Longident.qualify
let qualify_lid = Longident.qualify_lid


(***********************************************************************
 *                            Global flag
 *                 "Protecting" M.() and bigarrays
 ***********************************************************************)

(* lid of the "overloaded" function that will be left as a trace that
   an expression has been overloaded: if [e] has already been
   overloaded, [$overloaded$ e] will indicate to surrounding
   overloadings that one does not have to touch to [e] anymore.  None
   of the [$overloaded$] will appear in the final code.  (We allowed
   this before, defining [external $overloaded$ : 'a -> 'a =
   "%identity"] but this may prevent (e.g. float) optimizations to
   take place so now we are careful to remove them.) *)
let overloaded = "__pa_do_overloaded_"

(* There is the possibility of suspending (not acting immediately on)
   an overloading.  Since [M.(e)] cannot be represented in the AST, we
   will transform it into [__pa_do_suspended_ M e] so one can
   transform it later.  Note that we do not define
   [__pa_do_suspended_], leaving it in the AST is an error.  *)
let suspended = "__pa_do_suspended_"
;;

(* Bigarrays are transformed by Camlp4 into the Bigarray.<Array>.get
   (so e.g. [Bigarray.Array1.get a i <- x] will work under camlp4!).
   Nonetheless we want to distinguish whether the user actually wrote,
   say, [a.{i}] or [Bigarray.Array1.get a i] because overloading the
   latter is not what the user expects.  So we use the same trick as
   above, replacing the default rule by one that wraps the call with a
   special named funtion (as an added bonus [Bigarray.Array1.get a i <-
   x] does not work anymore).  We will do this only when inside an
   expression to be overloaded so this protection can be removed in
   the final code. *)
let ba_syntax = "__pa_do__bigarray_"


(* GLOBAL FLAG.  Whether we are inside an expression [e] being
   overloaded (because [M.(e)] was written).  This is important to
   decide whether to protect the expression after the overloadings are
   resolved. *)
module Overloading =
struct
  let ov_level = ref 0 (* Can be global because no concurrency while scanning.
                          Overloadings can be nested. *)

  let enter() = incr ov_level
  let leave() = decr ov_level

  (* For the toploop, the global variable might not be reset properly
     if there is a parsing error (e.g. for [M.(e]).  So, make sure we
     start in a clean state at the beginning of all toplevel inputs.
     This allows to connect the toploop to a reset function. *)
  let toplevel_reset =
    if not_interactive then (fun () -> ()) (* do nothing *)
    else (fun () -> ov_level := 0)

  (* Whether we are inside an overloading. *)
  let inside() = !ov_level > 0

  let protect e =
    if inside() then
      let _loc = Ast.loc_of_expr e in <:expr< $lid:overloaded$ $e$ >>
    else e

  let protect_ba _loc e =
    if inside() then <:expr< $lid:ba_syntax$ ($e$) >> else e

  (* Based on the same function in Camlp4OCamlRevisedParser.ml *)
  let bigarray_get _loc arr arg =
    let coords =
      match arg with
        (* The subtle distinction that, in the revised syntax, the
           parenthesis add an [Ast.ExTup] constructor is not present
           in the original syntax, thus one matches for [Ast.ExCom]
           explicitely. *)
      | <:expr< ($e1$, $e2$) >> | Ast.ExCom(_, e1, e2) ->
        Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
      | _ -> [arg] in
    match coords with
    | [c1] -> protect_ba _loc <:expr< Bigarray.Array1.get $arr$ $c1$ >>
    | [c1; c2] -> protect_ba _loc <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >>
    | [c1; c2; c3] ->
        protect_ba _loc <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >>
    | _ -> protect_ba _loc (<:expr< Bigarray.Genarray.get $arr$
                              [| $Ast.exSem_of_list coords$ |] >>)

  (* Based on the same function in "Camlp4OCamlParser.ml".  If the
     array is unprotected, it is sthe standard transformation; if it
     is, keep the protection. *)
  let bigarray_set _loc var newval =
    match var with
    (* 1D *)
    | <:expr< $lid:id$ (Bigarray.Array1.get $arr$ $c1$) >>
        when id = ba_syntax ->
      Some <:expr< $lid:ba_syntax$ (Bigarray.Array1.set $arr$ $c1$ $newval$) >>
    | <:expr< Bigarray.Array1.get $arr$ $c1$ >> ->
      Some <:expr< Bigarray.Array1.set $arr$ $c1$ $newval$ >>
    (* 2D *)
    | <:expr< $lid:id$ (Bigarray.Array2.get $arr$ $c1$ $c2$) >>
        when id=ba_syntax ->
      Some <:expr< $lid:ba_syntax$
        (Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$) >>
    | <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >> ->
      Some <:expr< Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$ >>
    (* 3D *)
    | <:expr< $lid:id$ (Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$) >>
        when id = ba_syntax ->
      Some <:expr< $lid:ba_syntax$
        (Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$) >>
    | <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >> ->
      Some <:expr< Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$ >>
    (* N-D *)
    | <:expr< $lid:id$ (Bigarray.Genarray.get $arr$ [| $coords$ |]) >>
        when id = ba_syntax ->
      Some <:expr< $lid:ba_syntax$
        (Bigarray.Genarray.set $arr$ [| $coords$ |] $newval$) >>
    | <:expr< Bigarray.Genarray.get $arr$ [| $coords$ |] >> ->
      Some <:expr< Bigarray.Genarray.set $arr$ [| $coords$ |] $newval$ >>
    | _ -> None
end


let () =
  let module S = Syntax in
  (try
     DELETE_RULE Gram S.expr: SELF; "."; "{"; S.comma_expr; "}" END;
     DELETE_RULE Gram S.expr: SELF; "<-"; S.expr LEVEL "top" END;
   with _ ->
     (* FIXME: it seems that OCaml 3.11 does not raise an exception
        but abort itself the program.  This is unfortunate. *)
     eprintf "Could not change the bigarray rules.  Did you load \
              a grammar?\n";
     exit 2);

  EXTEND Gram
    GLOBAL: S.expr S.comma_expr;

  S.expr: LEVEL "."
    [ [ e1 = SELF; "."; "{"; e2 = S.comma_expr; "}" ->
          Overloading.bigarray_get _loc e1 e2 ] ];
  S.expr: LEVEL ":="
    [ [ e1 = SELF; "<-"; e2 = S.expr LEVEL "top" ->
          match Overloading.bigarray_set _loc e1 e2 with
          | Some e -> e
          | None -> <:expr< $e1$ <- $e2$ >>
      ] ];
  END;;



(***********************************************************************
 *                      Overloading structure
 ***********************************************************************)

module Ops_map = Map.Make(String)

type module_longident = Longident.t

exception Invalid_identifier = Macro.Invalid_identifier

type reloc_expr = module_longident -> Loc.t -> expr

(* Extensible transformations.  This define the runtime information
   that accompanies the execution of general transformations.  This is
   open on both sides from above through [self] (many more
   transformations may be staked up) and below through [after] (what
   we do in non handled cases not yet specified). *)
type transf = {
  t: t; (* The structure containing the overloadings. *)
  m: module_longident; (* The module used to qualify the overloadings. *)
  self : Ast.map;
  (* the full version of the transf., at the end of the enrichment. *)
  before : transf_fun list;
  (* The transformations already performed, in reverse order.  This is
     needed for [expr_fold] which may need to insert a new function
     (closure) in the stack if the information the transformation
     carries changes. *)
  after : transf_fun list;
  (* The "old" version of the transformation, what is done "after" the
     current function.  When [t] is linked to a module, this will
     contain the "initial" transformation generated from the "simple"
     overloadings. *)
  transversal: expr -> expr;
  (* Basic AST transversal (to apply when [super = []]). *)
}
and transf_fun = transf -> expr -> expr

and t = {
  (* Constant values.  Substituted by relocatable expressions. *)
  int : int -> reloc_expr;
  int_is_set : bool;
  float : float -> reloc_expr;
  float_is_set : bool;
  int32 : int32 -> reloc_expr;
  int32_is_set : bool;
  int64 : int64 -> reloc_expr;
  int64_is_set : bool;
  nativeint : nativeint -> reloc_expr;
  nativeint_is_set : bool;
  string : string -> reloc_expr;
  string_is_set : bool;
  poly_var : string -> reloc_expr;
  poly_var_is_set : bool;
  (* Constant containers.  *)
  array : expr list -> reloc_expr;
  array_is_set : bool;
  list : expr list -> reloc_expr;
  list_is_set : bool;
  array_get : expr -> expr -> reloc_expr;
  array_get_is_set : bool;
  array_set : expr -> expr -> expr -> reloc_expr;
  array_set_is_set : bool;
  string_get : expr -> expr -> reloc_expr;
  string_get_is_set : bool;
  string_set : expr -> expr -> expr -> reloc_expr;
  string_set_is_set : bool;
  ba_get : expr -> expr list -> reloc_expr;
  ba_get_is_set : bool;
  ba_set : expr -> expr list -> expr -> reloc_expr;
  ba_set_is_set : bool;
  assign_ref : expr -> expr -> reloc_expr;
  assign_ref_is_set : bool;
  assign : expr -> expr -> reloc_expr;
  assign_is_set : bool;
  (* FIXME: a.b and a.b <- x *)
  openin : bool; (* whether to locally open the module *)
  (* Simple operator overloading.  To a lident, associate a
     relocatable expression. *)
  ops : reloc_expr Ops_map.t;
  (* General substitution functions for the "simple substitution stack": *)
  ops_transf : transf_fun list;
  (* Functions to execute before the overloading is performed, in the
     order of execution. *)
  pre_process : (unit -> unit) list;
  (* Functions to execute after the overloading (can transform the final
     expression), in reverse order of execution. *)
  post_process : (expr -> expr) list;
}


let empty = {
  int = (fun i _ _loc -> <:expr< $`int:i$ >>);
  int_is_set = false;
  float = (fun f _ _loc -> <:expr< $`flo:f$ >>);
  float_is_set = false;
  int32 = (fun i _ _loc -> <:expr< $`int32:i$ >>);
  int32_is_set = false;
  int64 = (fun i _ _loc -> <:expr< $`int64:i$ >>);
  int64_is_set = false;
  nativeint = (fun n _ _loc -> <:expr< $`nativeint:n$ >>);
  nativeint_is_set = false;
  string = (fun s _ _loc -> <:expr< $str:s$ >>);
  string_is_set = false;
  array = (fun l _ _loc -> <:expr< [| $exSem_of_list l$ |] >>);
  array_is_set = false;
  list = (fun l _ _loc ->
    let cons e tl = <:expr< $e$ :: $tl$ >> in
    List.fold_right cons l <:expr< [] >>);
  list_is_set = false;

  poly_var = (fun s _ _loc -> <:expr< `$s$ >>);
  poly_var_is_set = false;
  array_get = (fun a i _ _loc -> <:expr< $a$ .( $i$ ) >>);
  array_get_is_set = false;
  array_set = (fun a i x _ _loc -> <:expr< $a$ .( $i$ )  <- $x$ >>);
  array_set_is_set = false;
  string_get = (fun a i _ _loc -> <:expr< $a$ .[ $i$ ] >>);
  string_get_is_set = false;
  string_set = (fun a i x _ _loc -> <:expr< $a$ .[ $i$ ]  <- $x$ >>);
  string_set_is_set = false;
  ba_get = (fun a i _ _loc -> assert false);
  ba_get_is_set = false;
  ba_set = (fun a i x _ _loc -> assert false);
  ba_set_is_set = false;

  assign_ref = (fun a b _ _loc -> <:expr< $a$ := $b$ >>);
  assign_ref_is_set = false;
  assign = (fun a b _ _loc -> <:expr< $a$ <- $b$ >>);
  assign_is_set = false;
  openin = true; (* since OCaml 3.12.0, the syntax M.(e) is used for
                    locally opening M, enable it by default. *)
  ops = Ops_map.empty;
  ops_transf = []; (* [basic_subst] (see below) will implicitly be at
                      the bottom of this list. *)
  pre_process = [];
  post_process = [];
}

let concat t1 t2 ={
  int = (if t2.int_is_set then t2.int else t1.int);
  int_is_set = t1.int_is_set || t2.int_is_set;
  float = (if t2.float_is_set then t2.float else t1.float);
  float_is_set = t1.float_is_set || t2.float_is_set;
  int32 = (if t2.int32_is_set then t2.int32 else t1.int32);
  int32_is_set = t1.int32_is_set || t2.int32_is_set;
  int64 = (if t2.int64_is_set then t2.int64 else t1.int64);
  int64_is_set = t1.int64_is_set || t2.int64_is_set;
  nativeint = (if t2.nativeint_is_set then t2.nativeint else t1.nativeint);
  nativeint_is_set = t1.nativeint_is_set || t2.nativeint_is_set;
  string = (if t2.string_is_set then t2.string else t1.string);
  string_is_set = t1.string_is_set || t2.string_is_set;
  array = (if t2.array_is_set then t2.array else t1.array);
  array_is_set = t1.array_is_set || t2.array_is_set;
  list = (if t2.list_is_set then t2.list else t1.list);
  list_is_set = t1.list_is_set || t2.list_is_set;

  poly_var = (if t2.poly_var_is_set then t2.poly_var else t1.poly_var);
  poly_var_is_set = t1.poly_var_is_set || t2.poly_var_is_set;
  array_get = (if t2.array_get_is_set then t2.array_get else t1.array_get);
  array_get_is_set = t1.array_get_is_set || t2.array_get_is_set;
  array_set = (if t2.array_set_is_set then t2.array_set else t1.array_set);
  array_set_is_set = t1.array_set_is_set || t2.array_set_is_set;
  string_get = (if t2.string_get_is_set then t2.string_get else t1.string_get);
  string_get_is_set = t1.string_get_is_set || t2.string_get_is_set;
  string_set = (if t2.string_set_is_set then t2.string_set else t1.string_set);
  string_set_is_set = t1.string_set_is_set || t2.string_set_is_set;
  ba_get = (if t2.ba_get_is_set then t2.ba_get else t1.ba_get);
  ba_get_is_set = t1.ba_get_is_set || t2.ba_get_is_set;
  ba_set = (if t2.ba_set_is_set then t2.ba_set else t1.ba_set);
  ba_set_is_set = t1.ba_set_is_set || t2.ba_set_is_set;

  assign_ref = (if t2.assign_ref_is_set then t2.assign_ref else t1.assign_ref);
  assign_ref_is_set = t1.assign_ref_is_set || t2.assign_ref_is_set;
  assign = (if t2.assign_is_set then t2.assign else t1.assign);
  assign_is_set = t1.assign_is_set || t2.assign_is_set;
  openin = t1.openin || t2.openin;
  (* Beware of the order: t2 keys must mask t1 keys *)
  ops = Ops_map.fold (fun k a m -> Ops_map.add k a m) t2.ops t1.ops;
  (* [t2] supersedes [t1], its transformations must come first: *)
  ops_transf = t2.ops_transf @ t1.ops_transf;
  pre_process = t2.pre_process @ t1.pre_process; (* t1 executed after t2 *)
  post_process = t2.post_process @ t1.post_process;
}


(***********************************************************************
 *                       Saving substitutions
 ***********************************************************************)

(* Certain constant conversions, for example from an integer to a
   Big_int, will need to be done at runtime.  In order to do these
   conversions one time only, we can hold the result in a variable.
   The functions below help to do that.  This is also interesting when
   one subtitutes a lowercase identifier by a function, the function
   will be bound to a variable -- which will avoid code bloat -- and
   the compiler can decide whether or not to inline it. *)

let int_constants = Hashtbl.create 10
let float_constants = Hashtbl.create 10
let int32_constants = Hashtbl.create 10
let int64_constants = Hashtbl.create 10
let nativeint_constants = Hashtbl.create 10
let string_constants = Hashtbl.create 10
let poly_var_constants = Hashtbl.create 10
let lid_constants = Hashtbl.create 20

(* The concrete syntax allows to change, in the middle of a file, the
   overloadings for a module [m].  In such an event, the associations
   stored for [m] may not be valid but need to be kept because their
   presence in the *_constants means they have been used.  So we save
   them away and delete them from the main hashtables.  *)
let saved_associations = ref []

let save_cached_values (m: module_longident) =
  let save constants =
    let gather ((_,m') as k) v l =
      if m' = m then (
        saved_associations := v :: !saved_associations;
        k :: l
      ) else l in
    let keys = Hashtbl.fold gather constants [] in
    List.iter (fun k -> Hashtbl.remove constants k) keys in
  save int_constants;
  save float_constants;
  save int32_constants;
  save int64_constants;
  save nativeint_constants;
  save string_constants;
  save poly_var_constants;
  save lid_constants
;;

(* The filter are evaluated after the whole source is read, thus all
   constants will have been collected. *)
let () =
  if not_interactive then begin
    let define constants str_item =
      let _loc = Ast.loc_of_str_item str_item in
      let add _ (id, expr) c = (<:str_item< $c$  let $lid:id$ = $expr$ >>) in
      Hashtbl.fold add constants str_item in
    let define_constants str_item =
      let _loc = Ast.loc_of_str_item str_item in
      let add c (id, expr) = (<:str_item< $c$  let $lid:id$ = $expr$ >>) in
      let cst = List.fold_left add (<:str_item< >>) !saved_associations in
      let cst = define int_constants cst in
      let cst = define float_constants cst in
      let cst = define int32_constants cst in
      let cst = define int64_constants cst in
      let cst = define nativeint_constants cst in
      let cst = define string_constants cst in
      let cst = define poly_var_constants cst in
      let cst = define lid_constants cst in
      (<:str_item< $cst$  $str_item$ >>)  in
    AstFilters.register_str_item_filter define_constants
  end

(* The localisation of the lid we substitute for [i] will be the
   one of [i], contained in [_loc].  The location of the
   expression to be put at the beginning of the file will be the
   one of the first occurrence of the constant [i]. *)
(** [cache_overload ?check f constants], where [f i] must return the
    relocatable expression for the constant [i], returns a function
    similar to [f] except that the expressions returned by [f] for a
    given constant and module are bound to a lid in the generated
    source so they are evaluated only once by the program obtained
    after preprocessing.  The cached name for the expression is stored
    in [constants].  It is ASSUMED that for a given [(i,m)], [f i m
    _loc] always returns the same expression (modulo the location). *)
let cache_overload ?check (f: 'a -> reloc_expr) constants : ('a -> reloc_expr) =
  if not_interactive then
    fun i m _loc ->
      let key = (i,m) in
      try
        (* [id] is the identifier to use to replace the constant [i]
           for the module [m]. *)
        let id, _ = Hashtbl.find constants key in <:expr< $lid:id$ >>
      with Not_found ->
        (* No [id] yet declared for [i], create one. *)
        (match check with None -> () | Some ck -> ck i m _loc);
        let id = new_lid() in
        Hashtbl.add constants key (id, f i m _loc);
        <:expr< $lid:id$ >>
  else
    (* Caching does not work in the toploop (never evaluated). *)
    fun i m _loc ->
      (match check with None -> () | Some ck -> ck i m _loc);
      f i m _loc
;;

(** The same as [cache_overload] except that one only wants to
    subtitute the lowercase identifier [lid] with [expr].  It is
    ASSUMED that for any [(lid,m)], where [m] is a module name, the
    same expression [expr] will be substituted (modulo locations). *)
let save_expr lid expr constants =
  if not_interactive then
    fun m _loc ->
      let key = (lid,m) in
      try
        (* [id] is the identifier to use to replace the [lid] for the
           module [m]. *)
        let id, _ = Hashtbl.find constants key in <:expr< $lid:id$ >>
      with Not_found ->
        (* No [id] yet declared for [i], create one. *)
        let id = new_lid() in
        Hashtbl.add constants key (id, expr m _loc);
        <:expr< $lid:id$ >>
  else
    (* Caching does not work in the toploop (never evaluated). *)
    expr
;;

(***********************************************************************
 * General cache mechanism (to be used by other modules)
 ************************************************************************)
(* This is important that we declare this filter after the other one
   so that the stucture item cached with [add_to_beginning_of_file]
   PRECEDE the ones for constants as, for example, one can cache part
   of an expression passed to the [lid] function below. *)

(* Often values or functions need to be cached at the beginning of the
   file.  This provides a general infrastructure for doing this. *)
let declarations_at_beginning = ref []

(* The filter are evaluated after the whole source is read, thus all
   constants will have been collected. *)
let () =
  if not_interactive then begin
    let add_top_declarations str_item =
      let add s decl = <:str_item@here< $decl$ $s$ >> in
      List.fold_left add str_item !declarations_at_beginning in
    AstFilters.register_str_item_filter add_top_declarations
  end

let add_to_beginning_of_file decl =
  declarations_at_beginning := decl :: !declarations_at_beginning


(***********************************************************************
 *                  Constants and lid overloadings
 ***********************************************************************)

let int t ?(cache=true) f =
  if cache then
    { t with int = cache_overload f int_constants; int_is_set = true }
  else { t with int = f; int_is_set = true }

let float t ?(cache=true) f =
  if cache then
    { t with float = cache_overload f float_constants; int_is_set = true }
  else { t with float = f; float_is_set = true }

let int32 t ?(cache=true) f =
  if cache then
    { t with int32 = cache_overload f int32_constants; int32_is_set = true }
  else { t with int32 = f; int32_is_set = true }

let int64 t ?(cache=true) f =
  if cache then
    { t with int64 = cache_overload f int64_constants; int64_is_set = true }
  else { t with int64 = f; int64_is_set = true }

let nativeint t ?(cache=true) f =
  if cache then
    { t with nativeint = cache_overload f nativeint_constants;
        nativeint_is_set = true }
  else { t with nativeint = f; nativeint_is_set = true }

let string t ?(cache=true) ~check ?(to_type=(fun m -> m)) f =
  if cache then
    let check s m _loc =
      try ignore(check s)
      with _ ->
        let msg = sprintf "The string %S does not represent a valid %s."
          s (to_type (Longident.to_string m)) in
        Loc.raise _loc (Stream.Error msg) in
    { t with string = cache_overload f string_constants ~check;
        string_is_set = true }
  else { t with string = f; string_is_set = true }

let poly_var t ?(cache=true) ~check ?(to_type=(fun m -> m)) f =
  if cache then
    let check s m _loc =
      try ignore(check s)
      with _ ->
        let msg = sprintf "The polymorpic variant `%s does not represent a \
          valid %s" s (to_type (Longident.to_string m)) in
        Loc.raise _loc (Stream.Error msg) in
    { t with poly_var = cache_overload f poly_var_constants ~check;
        poly_var_is_set = true }
  else { t with poly_var = f; poly_var_is_set = true }


let array t f = { t with array = f; array_is_set = true }
let list t f = { t with list = f; list_is_set = true }
let array_get t f = { t with array_get = f; array_get_is_set = true }
let array_set t f = { t with array_set = f; array_set_is_set = true }
let string_get t f = { t with string_get = f; string_get_is_set = true }
let string_set t f = { t with string_set = f; string_set_is_set = true }
let bigarray_get t f = { t with ba_get = f; ba_get_is_set = true }
let bigarray_set t f = { t with ba_set = f; ba_set_is_set = true }

let assign t f = { t with assign = f; assign_is_set = true }
let assign_ref t f = { t with assign_ref = f; assign_ref_is_set = true }


let lid t ?(cache=true) op reloc_expr =
  if is_operator op || Macro.is_lowercase_identifier op then
    if cache then
      let e = save_expr op reloc_expr lid_constants in
      { t with ops = Ops_map.add op e t.ops }
    else { t with ops = Ops_map.add op reloc_expr t.ops }
  else invalid_arg(sprintf "%S is not a valid identifier" op)

(* FIXME: check that there are no duplicate keys?  Otherwise issue and
   exn or simply print a warning? *)
let lid_subst t ov_ops =
  let add_subst (op, op') map = Ops_map.add op (qualify_lid op') map in
  let ops = List.fold_right add_subst ov_ops t.ops in
  { t with ops = ops }

let predefined_overloads =
  [("+","add"); ("-","sub"); ("~-", "neg"); ("*","mul"); ("/","div")]
    (* ("**","pow") is not pervasive enough to add it here. **)

let std_arithmetic t = lid_subst t predefined_overloads

let lid_remove t ov =
  { t with
      ops = List.fold_right (fun id map -> Ops_map.remove id map) ov t.ops
  }

let comparison_id ?(cache=true) ~(cmp: reloc_expr) t =
  let overload t op =
    let f m _loc = <:expr< fun x y ->
      Pervasives.$lid:op$ ($cmp m _loc$ x y) 0 >> in
    lid t op f ~cache in
  let t = overload t "<"  in
  let t = overload t "<=" in
  let t = overload t ">"  in
  let t = overload t ">=" in
  let t = overload t "="  in
  let t = overload t "<>" in
  (* [max] and [min] functions. *)
  let t = lid t "max" ~cache begin fun m _loc ->
    <:expr< fun x y -> if Pervasives.(>=) ($cmp m _loc$ x y) 0 then x else y >>
  end in
  lid t "min" ~cache begin fun m _loc ->
    <:expr< fun x y -> if Pervasives.(<=) ($cmp m _loc$ x y) 0 then x else y >>
  end

let comparison ?cache ?(cmp="compare") t =
  comparison_id ?cache ~cmp:(qualify_lid cmp) t

(** Return the list of elements separated by semicolons *)
let rec sem_map f = function
  | ExNil _ -> []
  | ExSem(_, e, tl) -> f e :: sem_map f tl
  | e -> [f e]

exception List_not_complete

(** Return the list of entries of a list expression. *)
let rec expr_list_map f = function
  | <:expr< [] >> -> []
  | <:expr< $e$ :: $tl$ >> -> f e :: expr_list_map f tl
  | _ -> raise List_not_complete (* e.g. single use of the constructor *)

let basic_subst t ~self ~super e m =
  let _loc = Ast.loc_of_expr e in
  match e with
    (* With caching, it is seldom needed to optimize for binary
       operators.  For a case where it is needed, see the Float.()
       overloading. *)
  | <:expr< $lid:id$ >> -> (try (Ops_map.find id t.ops) m _loc
                            with Not_found -> e)
  | <:expr< $int:i$ >> -> t.int (int_of_string i) m _loc
  | <:expr< $flo:f$ >> -> t.float (float_of_string f) m _loc
  | <:expr< $int32:i$ >> -> t.int32 (Int32.of_string i) m _loc
  | <:expr< $int64:i$ >> -> t.int64 (Int64.of_string i) m _loc
  | <:expr< $nativeint:n$ >> -> t.nativeint (Nativeint.of_string n) m _loc
  | <:expr< $str:s$ >> -> t.string s m _loc
  | <:expr< `$s$ >> -> t.poly_var s m _loc
  | <:expr< [| $e$ |] >> -> t.array (sem_map self e) m _loc
  | <:expr< [] >> -> t.list [] m _loc
  | <:expr< $e$ :: $tl$ >> ->
    (try t.list (self e :: expr_list_map self tl) m _loc
     with List_not_complete -> <:expr< $self e$ :: $self tl$ >>)
  (* Getters and setters *)
  | <:expr< $a$ .( $i$ ) <- $x$ >> ->
    t.array_set (self a) (self i) (self x) m _loc
  | <:expr< $a$ .( $i$ ) >> -> t.array_get (self a) (self i) m _loc
  | <:expr< $a$ .[ $i$ ] <- $x$ >> ->
    t.string_set (self a) (self i) (self x) m _loc
  | <:expr< $a$ .[ $i$ ] >> -> t.string_get (self a) (self i) m _loc

  (* Bigarrays notation, with the "wrapping trick".  Remove the
     protection if the construction is not overloaded. *)
  | <:expr< $lid:id$ (Bigarray.Array1.get $a$ $i$) >> when id = ba_syntax ->
    if t.ba_get_is_set then t.ba_get (self a) [self i] m _loc
    else <:expr< Bigarray.Array1.get $a$ $i$ >>
  | <:expr< $lid:id$ (Bigarray.Array2.get $a$ $i1$ $i2$) >>
      when id = ba_syntax ->
    if t.ba_get_is_set then t.ba_get (self a) [self i1; self i2] m _loc
    else <:expr< Bigarray.Array2.get $a$ $i1$ $i2$ >>
  | <:expr< $lid:id$ (Bigarray.Array3.get $a$ $i1$ $i2$ $i3$) >>
      when id = ba_syntax ->
    if t.ba_get_is_set then t.ba_get (self a) [self i1; self i2; self i3] m _loc
    else <:expr< Bigarray.Array3.get $a$ $i1$ $i2$ $i3$ >>
  | <:expr< $lid:id$ (Bigarray.Genarray.get $a$ [| $coords$ |]) >>
      when id = ba_syntax ->
    if t.ba_get_is_set then
      let c = sem_map self coords in t.ba_get (self a) c m _loc
    else <:expr< Bigarray.Genarray.get $a$ [| $coords$ |] >>
  | <:expr< $lid:id$ (Bigarray.Array1.set $a$ $i$ $x$) >>
      when id = ba_syntax ->
    if t.ba_set_is_set then t.ba_set (self a) [self i] (self x) m _loc
    else <:expr< Bigarray.Array1.set $a$ $i$ $x$ >>
  | <:expr< $lid:id$ (Bigarray.Array2.set $a$ $i1$ $i2$ $x$) >>
      when id = ba_syntax ->
    if t.ba_set_is_set then t.ba_set (self a) [self i1; self i2] (self x) m _loc
    else <:expr< Bigarray.Array2.set $a$ $i1$ $i2$ $x$ >>
  | <:expr< $lid:id$ (Bigarray.Array3.set $a$ $i1$ $i2$ $i3$ $x$) >>
      when id = ba_syntax ->
    if t.ba_set_is_set then
      t.ba_set (self a) [self i1; self i2; self i3] (self x) m _loc
    else <:expr< Bigarray.Array3.set $a$ $i1$ $i2$ $i3$ $x$ >>
  | <:expr< $lid:id$ (Bigarray.Genarray.set $a$ [| $coords$ |] $x$) >>
      when id = ba_syntax ->
    if t.ba_set_is_set then
      let c = sem_map self coords in t.ba_set (self a) c (self x) m _loc
    else <:expr< Bigarray.Genarray.set $a$ [| $coords$ |] $x$ >>

  | <:expr< $a$ := $x$ >> when t.assign_ref_is_set ->
    t.assign_ref (self a) (self x) m _loc
      (* This pattern starts with a "<-" so should be before "<-". *)
  | <:expr< $a$ <- $x$ >> when t.assign_is_set ->
    (* Must be after all other constructions using [<-].  *)
    t.assign (self a) (self x) m _loc

  | e -> super e (* decompose [e] (recursion on the AST). *)
;;

(***********************************************************************
 *                          Miscellaneous
 ***********************************************************************)

(* Wish of Hezekiah Carty <hcarty@atmos.umd.edu> *)
let openin ?(remove=false) t =
  { t with openin = not remove }

(***********************************************************************
 *     General substitution functions and applying overloadings
 ***********************************************************************)

let transf_module tr = tr.m

let transf_qualify tr lid _loc = qualify_lid lid tr.m _loc

let apply_transf tr e =
  match tr.after with
  | f :: tl ->
      (* Apply the next function on the stack and keep the history in [tr]. *)
      f { tr with after = tl; before = f :: tr.before } e
  | [] ->
      (* All "general substitution" overloadings have been applied,
         now come the basic ones. *)
      basic_subst tr.t ~self:tr.self#expr ~super:tr.transversal e tr.m

let super = apply_transf

let self tr = tr.self

let expr t f = { t with ops_transf = f :: t.ops_transf }

let before t f = { t with pre_process = f :: t.pre_process }
let after t f = { t with post_process = f :: t.post_process }

class apply_transf_expr t m =
object(self)
  inherit Ast.map as map_super

  (* This method allows to execute [f tr] with [tr] properly
     initialized with the references to the object. *)
  method exec : 'a. (transf -> 'a) -> 'a = fun f ->
    f { t = t; m = m; self = (self :> Ast.map);
        before = []; after = t.ops_transf;
        transversal = map_super#expr }

  method expr e =
    match e with
    | <:expr< $lid:id$ $ov$ >> when id = overloaded -> ov
      (* If a subexpression has been overloaded, do not modify it
         anymore (thus do not recurse into it).  This rule is first
         so, NO transformation function can BYPASS it.  We remove the
         "overloaded" indicator because it may prevent some
         optimizations to take place.  *)
    | <:expr< $lid:id$ $id:_$ $_$ >> when id = suspended -> e
      (* If one is parsing the body of [X.(... M.(e) ...)] and [M] is
         suspended, one does not want to touch it (the suspension will
         be resolved later). *)
    | _ ->
        let tr = { t = t; m = m; self = (self :> Ast.map);
                   before = []; after = t.ops_transf;
                   transversal = map_super#expr } in
        super tr e
end


let apply t m =
  let transf = (new apply_transf_expr t m)#expr in
  if t.openin then
    fun e ->
      List.iter (fun f -> f()) t.pre_process;
      let e = transf e in
      let e = List.fold_right (fun f e -> f e) t.post_process e in
      let _loc = Ast.loc_of_expr e in
      <:expr< let open $Longident.to_id m _loc$ in $e$ >>
  else
    fun e ->
      List.iter (fun f -> f()) t.pre_process;
      let e = transf e in
      List.fold_right (fun f e -> f e) t.post_process e

let expr_fold t f a0 =
  (* [self] cannot change a mutable variable in its closure (shared by
     [f]) because the transformation must be performed on multiple
     subtrees and changing the value in one subtree must not affect
     the others.  Thus [self a] inserts the closure [f ~self a] in the
     stack in place of the original [f] and creates a new tranversal
     object. *)
  let rec self a tr =
    let new_f = f ~self ~super a in
    (* Replace the old [f] in [tr.before] by the new one: *)
    let before = new_f :: List.tl tr.before in
    let t = { tr.t with ops_transf = List.rev_append before tr.after } in
    (new apply_transf_expr t tr.m :> Ast.map) (* => new [tr] passed around *)
  and super a tr e =
    (* Similar to [self] except that one must continue from where we
       are on the stack. *)
    let new_f = f ~self ~super a in
    let before = new_f :: List.tl tr.before in
    let after = tr.after in
    let t = { tr.t with ops_transf = List.rev_append before after } in
    let o = new apply_transf_expr t tr.m in
    (* Continue where we left off: *)
    o#exec (fun tr ->
              let tr = { tr with before = before; after = after} in
              apply_transf tr e)
  in
  { t with ops_transf = f ~self ~super a0 :: t.ops_transf }
;;

(***********************************************************************
 *                   Default set of overloadings
 ***********************************************************************)

module Default =
struct
  let is_set = ref false
  let overloadings = ref empty
  module S = Set.Make(Longident)
  let applied = ref S.empty
    (* Default overloading may result in some cached values to be
       saved.  We must track the affected modules in case the
       overloading changes. *)

  let apply m e =
    if !is_set then (
      applied := S.add m !applied;
      apply !overloadings m e
    )
    else raise Not_found

  let update f =
    S.iter save_cached_values !applied;
    applied := S.empty;
    is_set := true;
    overloadings := f !overloadings

  (* [m] may be used with default overloadings with saved values, then
     be declared overloaded.  In this case, the saved values must be
     put away. *)
  let save_for m =
    save_cached_values m;
    applied := S.remove m !applied

  let unset () =
    is_set := false;
    overloadings := empty

  let longident = Longident.of_list [ "DEFAULT" ]
end


(***********************************************************************
 *                 Linking overloadings and modules
 ***********************************************************************)

module Link_map = Map.Make(Longident)

let association_table = ref Link_map.empty

(* [m_ov] is the "overloading name" i.e. the name X used for X.(...).
   [m] is the actual module used for qualifying the identifiers. *)
let associate_longident t (m_ov:module_longident) m =
  (* Save away the previously defined (lid, expr) for the module [m].
     We do it here because the concrete syntax allows to change the
     overloadings in the course of parsing the file and a user of this
     library may trugger this function in his own concrete syntax
     constructs. *)
  Default.save_for m; (* => save_cached_values m *)
  association_table := Link_map.add m_ov (t, m, apply t m) !association_table

let associate t ?qualify m_ov =
  (* Module_longident.of_string checks [m_ov]. *)
  let m_ov = Longident.of_string m_ov in
  let m = match qualify with
    | Some m -> Longident.of_string m
    | None -> m_ov in
  associate_longident t m_ov m

let no_overloadings m _loc =
  let msg = sprintf "No overloadings defined for the module %S."
    (Longident.to_string m) in
  Loc.raise _loc (Stream.Error msg)

let apply_overloadings_for m _loc e =
  try let _, _, app = Link_map.find m !association_table in app e
  with Not_found ->
    try Default.apply m e
    with Not_found ->  no_overloadings m _loc

let overloadings_for m _loc =
  try let t, _, _ = Link_map.find m !association_table in t
  with Not_found -> no_overloadings m _loc


(***********************************************************************
 *                       Delaying overloding
 ***********************************************************************)

module Suspend =
struct

  let suspended_modules = Hashtbl.create 10

  let add_longident m =
    try incr(Hashtbl.find suspended_modules m)
    with Not_found -> Hashtbl.add suspended_modules m (ref 1)

  let add m =
    add_longident(Longident.of_string m)

  let remove_longident m =
    try
      let c = Hashtbl.find suspended_modules m in
      if !c <= 1 then Hashtbl.remove suspended_modules m
      else decr c;
    with Not_found -> ()

  let remove m =
    remove_longident(Longident.of_string m)

  let is_suspended m =
    try !(Hashtbl.find suspended_modules m) > 0
    with Not_found -> false

  (* Mark overloading of [e] as suspended to be able to act on it later. *)
  let expr e m _loc =
    (<:expr< $lid:suspended$ $id:Longident.to_id m _loc$ $e$ >>)
      (* keep in sync with [apply_expr] *)

  (* Traverse an expression and resolve all suspended overloadings. *)
  class resolve_ov =
  object(self)
    inherit Ast.map as super

    method expr = function
      (* One must enter in already overloaded expressions (because
         they may contain suspended overloadings) so there is not
         match for [overloaded]. *)
    | <:expr@_loc< $lid:id$ $id:m$ $e$ >> when id = suspended ->
      let m = Longident.of_id m in
      (* [apply_overloadings_for] will not enter already overloaded
         sub-expressions.  So we recurse first: thus for [M.(... N.(e)
         ...)] (both suspended), [N.(e)] will be overloaded and
         protected first and the overloading on the body of [M] will
         not modify [e].  We use the global mechanism to see whether
         we are in an overloaded environment -- gather the above
         situation and the one where the macro is exanded in an
         overloading [X.(... F(M) ...)]. *)
      Overloading.enter();
      let e' = apply_overloadings_for m _loc (self#expr e) in
      Overloading.leave();
      Overloading.protect e'

    | e -> super#expr e
  end

  let resolve = (new resolve_ov)#expr
end

(* Collaborating with [Macro]. *)
let () =
  (* For all uid param [x], delay the overloading of $x$.(...) in the
     macro body [e] *)
  let suspend = function
    | Macro.Uid m -> Suspend.add m
    | Macro.Lid _ | Macro.Macro _ | Macro.Unused -> () in
  Macro.Hook.define_params (fun params -> List.iter suspend params);
  (* After the macro body is read, remove the suspended modules.  If
     an argument is repeated, the overloading will be removed the
     same number of times (this is important). *)
  let unsuspend = function
    | Macro.Uid m -> Suspend.remove m;
    | Macro.Lid _  | Macro.Macro _ | Macro.Unused -> () in
  Macro.Hook.define (fun params body -> List.iter unsuspend params; body);
  Macro.Hook.macro_expansion Suspend.resolve
;;

(***********************************************************************
 *               Other similar concrete constructions
 ***********************************************************************)

let module_square_brackets =
  let no_assoc_fun m _loc e =
    let m = Longident.to_string m in
    failwith(sprintf "%s.[ ... ]: no function to perform" m) in
  ref no_assoc_fun

let module_curly_brackets =
  let no_assoc_fun m _loc e =
    let m = Longident.to_string m in
    failwith(sprintf "%s.{ ... }: no function to perform" m) in
  ref no_assoc_fun


(***********************************************************************
 *                         Concrete syntax
 ***********************************************************************)

let is_unqualified = function
  | <:ident< $lid:_$ >> | <:ident< $uid:_$ >> -> true (*FIXME: others?*)
  | _ -> false

(* For (what is inside the braces of) an expression [{ p = e;... }]
   and [{ x with p = e;... }], qualify the fields as needed. *)
let rec qualify_record_constr tr e0 m fields = match e0 with
  | RbSem(loc, f1, f2) ->
      RbSem(loc, qualify_record_constr tr f1 m fields,
            qualify_record_constr tr f2 m fields)
  | RbEq(loc, (IdLid(_, f) as lid), e) ->
      let lid = if List.mem f fields then qualify lid m else lid in
      RbEq(loc, lid, (self tr)#expr e)
  | _ -> (self tr)#rec_binding e0

(* Only for the concrete syntax.  When using the API, it is "easy" *)
let qualify_record_fields t fields =
  let qualify tr e0 = match e0 with
    | <:expr< $e$ . $lid:f$ >> when List.mem f fields ->
      let _loc = Ast.loc_of_expr e0 in
      <:expr< $(self tr)#expr e$ . $qualify_lid f tr.m _loc$ >>
    | ExRec(loc, f, e) ->
        let fields = qualify_record_constr tr f tr.m fields in
        ExRec(loc, fields, (self tr)#expr e)
    | _ -> super tr e0 in
  expr t qualify


let handle_exn _loc f a =
  try f a
  with
    Invalid_identifier lid ->
      let msg = ("The string \"" ^ lid ^ "\" does not designate \
        a valid lowercase_identifier.") in
      Loc.raise _loc (Stream.Error msg)

(** [reassociate m f] associate [m] with the set of overloadings [f t]
    where [t] is the set of overloadings previously associated with
    [m] or, if none was, [t = empty]. *)
let reassociate m_ov f =
  if Longident.compare m_ov Default.longident = 0 then
    Default.update f
  else
    try
      let t, m, _ = Link_map.find m_ov !association_table in
      associate_longident (f t) m_ov m
    with Not_found ->
      associate_longident (f empty) m_ov m_ov

open Syntax

(* This just change the location of the expression (only the toplevel
   one, one does not touch to subexpressions).  It is useful for error
   reporting.   (Complete list in Camlp4/Camlp4Ast.partial.ml) *)
let relocate loc e = match (e: Ast.expr) with
  | ExNil _                   -> ExNil loc
  | ExId  (_, e1)             -> ExId  (loc, e1)
  | ExAcc (_, e1, e2)         -> ExAcc (loc, e1, e2)
  | ExAnt (_, e1)             -> ExAnt (loc, e1)
  | ExApp (_, e1, e2)         -> ExApp (loc, e1, e2)
  | ExAre (_, e1, e2)         -> ExAre (loc, e1, e2)
  | ExArr (_, e1)             -> ExArr (loc, e1)
  | ExSem (_, e1, e2)         -> ExSem (loc, e1, e2)
  | ExAsf e1                  -> ExAsf e1
  | ExAsr (_, e1)             -> ExAsr (loc, e1)
  | ExAss (_, e1, e2)         -> ExAss (loc, e1, e2)
  | ExChr (_, e1)             -> ExChr (loc, e1)
  | ExCoe (_, e1, e2, e3)     -> ExCoe (loc, e1, e2, e3)
  | ExFlo (_, e1)             -> ExFlo (loc, e1)
  | ExFor (_, e1, e2, e3, e4, e5) -> ExFor (loc, e1, e2, e3, e4, e5)
  | ExFun (_, e1)             -> ExFun (loc, e1)
  | ExIfe (_, e1, e2, e3)     -> ExIfe (loc, e1, e2, e3)
  | ExInt (_, e1)             -> ExInt (loc, e1)
  | ExInt32 (_, e1)           -> ExInt32 (loc, e1)
  | ExInt64 (_, e1)           -> ExInt64 (loc, e1)
  | ExNativeInt (_, e1)       -> ExNativeInt (loc, e1)
  | ExLab (_, e1, e2)         -> ExLab (loc, e1, e2)
  | ExLaz (_, e1)             -> ExLaz (loc, e1)
  | ExLet (_, e1, e2, e3)     -> ExLet (loc, e1, e2, e3)
  | ExLmd (_, e1, e2, e3)     -> ExLmd (loc, e1, e2, e3)
  | ExMat (_, e1, e2)         -> ExMat (loc, e1, e2)
  | ExNew (_, e1)             -> ExNew (loc, e1)
  | ExObj (_, e1, e2)         -> ExObj (loc, e1, e2)
  | ExOlb (_, e1, e2)         -> ExOlb (loc, e1, e2)
  | ExOvr (_, e1)             -> ExOvr (loc, e1)
  | ExRec (_, e1, e2)         -> ExRec (loc, e1, e2)
  | ExSeq (_, e1)             -> ExSeq (loc, e1)
  | ExSnd (_, e1, e2)         -> ExSnd (loc, e1, e2)
  | ExSte (_, e1, e2)         -> ExSte (loc, e1, e2)
  | ExStr (_, e1)             -> ExStr (loc, e1)
  | ExTry (_, e1, e2)         -> ExTry (loc, e1, e2)
  | ExTup (_, e1)             -> ExTup (loc, e1)
  | ExCom (_, e1, e2)         -> ExCom (loc, e1, e2)
  | ExTyc (_, e1, e2)         -> ExTyc (loc, e1, e2)
  | ExVrn (_, e1)             -> ExVrn (loc, e1)
  | ExWhi (_, e1, e2)         -> ExWhi (loc, e1, e2)
  | ExOpI (_, e1, e2)         -> ExOpI (loc, e1, e2)
  | ExFUN (_, e1, e2)         -> ExFUN (loc, e1, e2)
  | ExPkg (_, e1)             -> ExPkg (loc, e1)

type val_longident =
  | Ident of Ast.ident
  | Overloading of Ast.expr
  | Square_brackets of Ast.expr
  | Curly_brackets of Ast.expr

let extend() =
  (* Since module_longident_dot_lparen is not public, we cannot delete
     the rule that was added in 3.12 for local open. *)
  (* DELETE_RULE Gram expr: TRY module_longident_dot_lparen; sequence; ")" END; *)
  EXTEND Gram
    GLOBAL: expr str_item label_longident
    infixop0 infixop1 infixop2 infixop3 infixop4;

  expr: LEVEL "simple"
    [ [ (m, v) = val_longident_or_overloading ->
      let m = Longident.of_list m in
      (match v with
      | Ident i ->
        let i = qualify i m in
        let _loc = Ast.loc_of_ident i in <:expr< $id:i$ >>
      | Overloading e ->
        Overloading.leave();
        if Suspend.is_suspended m then
                 (* To preserve the location of [M.(e)] set it to [e]. *)
                 Suspend.expr (relocate _loc e) m _loc
               else
                 let e' = apply_overloadings_for m _loc e in
                 (* The transformed expression [e'] must have the
                    location of the whole [M.(e)] so that error
                    reporting highlight the whole [M.(e)] e.g. in case
                    [e'] does not have the correct type. *)
                 let e' = relocate _loc e' in
                 Overloading.protect e'
           | Square_brackets e -> !module_square_brackets m _loc e
           | Curly_brackets e ->  !module_curly_brackets m _loc e
          )
      ] ];
  (* FIXME: do we allow antiquotations of the original Camlp4 gram? *)
  val_longident_or_overloading:
    [ [ (* base cases *)
        i = a_UIDENT; "."; _ = overloading_enter; e = expr; ")" ->
          ([i], Overloading e)
      | i = a_UIDENT; "."; "["; e = expr; "]" -> ([i], Square_brackets e)
      | i = a_UIDENT; "."; "{"; e = expr; "}" -> ([i], Curly_brackets e)
      | i = a_UIDENT -> ([], Ident(<:ident< $uid:i$ >>)) (* constructor *)
      | i = a_LIDENT -> ([], Ident(<:ident< $lid:i$ >>))
      (* Recursive case: *)
      | i = a_UIDENT; "."; (j, v) = SELF -> (i :: j, v)
      ] ];
  (* Separate rule just to set the flag that we are inside an
     overloaded expression. *)
  overloading_enter:
    [ [ "(" -> Overloading.enter() ] ];

  str_item:
    [ [ f = overloading_declaration -> <:str_item< >> ] ];

  module_longident: (* local to this EXTEND *)
    [ [ m = module_longident_list -> Longident.of_list m ] ];
  module_longident_list:
    [ [ m = a_UIDENT; "."; l = SELF -> m :: l
      | i = a_UIDENT -> [i]
      ] ];
  overloading_declaration:
    [ [ "OVERLOAD"; m = module_longident; "(";
        a = LIST1 substitution SEP ";"; ")" ->
          let subst t (id, expr) =
            let reloc_expr m _loc =
              if is_unqualified expr then <:expr< $id:qualify expr m$ >>
              else <:expr< $id:expr$ >> in
            lid t id reloc_expr ~cache:false in
          reassociate m (fun t -> List.fold_left subst t a)
      | "OVERLOAD"; m = module_longident; "inherit"; m' = module_longident ->
          reassociate m (fun t -> concat t (overloadings_for m' _loc))
      | "OVERLOAD"; m = module_longident; "="; m' = module_longident ->
          associate_longident (overloadings_for m' _loc) m m'

      | "OVERLOAD_ARITHMETIC"; m = module_longident ->
          reassociate m std_arithmetic

      | "OVERLOAD_COMPARISON"; (m, f) = module_and_opt_compare ->
          let cmp m _loc = <:expr< $id:qualify f m$ >> in
          reassociate m (fun t -> comparison_id ~cmp t ~cache:false)

      | "OVERLOAD_INT"; (m, f) = module_and_function ->
          let ov_f i m _loc = <:expr< $id:qualify f m$ $`int:i$ >> in
          reassociate m (fun t -> int t ov_f ~cache:false)
      | "OVERLOAD_FLOAT"; (m, f) = module_and_function ->
          let ov_f x m _loc = <:expr< $id:qualify f m$ $`flo:x$ >> in
          reassociate m (fun t -> float t ov_f ~cache:false)
      | "OVERLOAD_INT32"; (m, f) = module_and_function ->
          let ov_f i m _loc = <:expr< $id:qualify f m$ $`int32:i$ >> in
          reassociate m (fun t -> int32 t ov_f ~cache:false)
      | "OVERLOAD_INT64"; (m, f) = module_and_function ->
          let ov_f i m _loc = <:expr< $id:qualify f m$ $`int64:i$ >> in
          reassociate m (fun t -> int64 t ov_f ~cache:false)
      | "OVERLOAD_NATIVEINT"; (m, f) = module_and_function ->
          let ov_f n m _loc = <:expr< $id:qualify f m$ $`nativeint:n$ >> in
          reassociate m (fun t -> nativeint t ov_f ~cache:false)
      | "OVERLOAD_STRING"; (m, f) = module_and_function ->
          let ov_f s m _loc = <:expr< $id:qualify f m$ $str:s$ >> in
          reassociate m (fun t -> string t ~check:(fun _ -> ()) ov_f ~cache:false)
            (* no possible compile time check for the concrete syntax *)
      | "OVERLOAD_POLY_VAR"; (m, f) = module_and_function ->
          let ov_f pv m _loc = <:expr< $id:qualify f m$ $str:pv$ >> in
          reassociate m (fun t -> poly_var t ~check:(fun _ -> ()) ov_f
                           ~cache:false)

      | "OVERLOAD_ARRAY_GET"; (m, f) = module_and_function ->
          let ov_f a i m _loc = <:expr< $id:qualify f m$ $a$ $i$ >> in
          reassociate m (fun t -> array_get t ov_f)
      | "OVERLOAD_ARRAY_SET"; (m, f) = module_and_function ->
          let ov_f a i x m _loc = <:expr< $id:qualify f m$ $a$ $i$ $x$ >> in
          reassociate m (fun t -> array_set t ov_f)
      | "OVERLOAD_BIGARRAY_GET"; (m, f) = module_and_function ->
          let ov_f a i m _loc =
            <:expr< $id:qualify f m$ $a$ [| $Ast.exSem_of_list i$ |] >> in
          reassociate m (fun t -> bigarray_get t ov_f)
      | "OVERLOAD_BIGARRAY_SET"; (m, f) = module_and_function ->
          let ov_f a i x m _loc =
            <:expr< $id:qualify f m$ $a$ [| $Ast.exSem_of_list i$ |] $x$ >> in
          reassociate m (fun t -> bigarray_set t ov_f)
      | "OVERLOAD_STRING_GET"; (m, f) = module_and_function ->
          let ov_f a i m _loc = <:expr< $id:qualify f m$ $a$ $i$ >> in
          reassociate m (fun t -> string_get t ov_f)
      | "OVERLOAD_STRING_SET"; (m, f) = module_and_function ->
          let ov_f a i x m _loc = <:expr< $id:qualify f m$ $a$ $i$ $x$ >> in
          reassociate m (fun t -> string_set t ov_f)
      | "OVERLOAD_ASSIGN_REF"; (m, f) = module_and_function ->
          let ov_f a b m _loc = <:expr< $id:qualify f m$ $a$ $b$ >> in
          reassociate m (fun t -> assign_ref t ov_f)
      | "OVERLOAD_ASSIGN"; (m, f) = module_and_function ->
          let ov_f a b m _loc = <:expr< $id:qualify f m$ $a$ $b$ >> in
          reassociate m (fun t -> assign t ov_f)

      | "OVERLOAD_RECORD_FIELD"; m = module_longident; "(";
          a = LIST1 record_field SEP ";"; ")" ->
            reassociate m (fun t -> qualify_record_fields t a)

      (* FIXME: other types? *)

      | "OVERLOAD_OPENIN"; m = module_longident ->
          reassociate m (fun t -> openin t)
      | "OVERLOAD_OPENIN"; m = module_longident; "("; "false"; ")" ->
          reassociate m (fun t -> openin t ~remove:true)

      | "OVERLOAD_REMOVE"; m = module_longident;
          "("; a = LIST1 operator SEP ";"; ")" ->
            reassociate m (fun t -> lid_remove t a)

      ] ];
  substitution:
    [ [ (* label_longident : fully qualified lident.  Is it appropriate? *)
        f = LIDENT; "->"; e = label_longident -> (f,e)
      | f = LIDENT -> (f, <:ident< $lid:f$ >>)
      ] ];
  module_and_function:
    [ [ m = module_longident; "("; f = label_longident; ")" -> (m, f)
      ] ];
  module_and_opt_compare:
    [ [ m = module_longident; "("; f = label_longident; ")" -> (m, f)
      | m = module_longident -> (m, <:ident< compare >>)
      ] ];
  operator:
    [ [  f = LIDENT -> f ] ];
  record_field:
    [ [ f = LIDENT -> f ] ];
  END
;;

let () =
  try extend()
  with Failure _ ->
    printf "Could not set up the syntax extension.  You probably forgot \
      to load a camlp4 grammar.\n"


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
