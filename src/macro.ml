(* Macros may be parametrised by modules in which case the X.(...)
   construct in their bodies must be delayed until the macro is
   used. *)
(* Pbm with macros:

   - Hard to debug (pbm of locations => tweak the locate file name
   when using the macro, the other fields pointing to the original
   positions?)

   - no constant arguments (so that one can statically decide
   according to these).

   - No module arguments (This is expecially interesting in conjuction
   with overloading, F(M) = M.(x+y) )
*)

open Printf
open Camlp4.PreCast
open Syntax

(***********************************************************************
 *                            Utilities
 ***********************************************************************)

let printer =
  let module P = Camlp4.Printers.OCaml.Make(Syntax)
  in new P.printer ()

let string_of_expr e =
  let buf = Buffer.create 50 in
  let ff = Format.formatter_of_buffer buf in
  printer#expr ff e;
  Format.pp_print_flush ff ();
  Buffer.contents buf


(***********************************************************************
 *                 Lowercase and module identifiers
 ***********************************************************************)

exception Invalid_identifier of string

let is_uppercase c = 'A' <= c && c <= 'Z'
let is_lowercase c = ('a' <= c && c <= 'z') || c = '_'
let is_inner_char c =
  is_uppercase c || is_lowercase c || ('0' <= c && c <= '9') || c = '\''

let is_lowercase_identifier lid =
  let l = String.length lid in
  let rec loop i = i = l || (is_inner_char lid.[i] && loop (i+1)) in
  l <> 0 && is_lowercase lid.[0] && loop 1

let is_capitalized_identifier lid =
  let l = String.length lid in
  let rec loop i = i = l || (is_inner_char lid.[i] && loop (i+1)) in
  l <> 0 && is_uppercase lid.[0] && loop 1


let is_uppercase_ c = ('A' <= c && c <= 'Z') || c = '_'

let is_uppercase_identifier s =
  let len = String.length s in
  let rec check_char i =
    i >= len || (is_uppercase_ s.[i] && check_char(i+1)) in
  s <> "" && check_char 0


module Module_longident =
struct
  type t = string list
      (* Module components e.g. [A.B] is represented as [["A";"B"]].  *)

  let compare = Pervasives.compare

  exception Invalid of string

  (** Return the string representing the module longident [m] *)
  let to_string m = String.concat "." m

  (** Return true if the string [m] is a module_longident.  *)
  let is m =
    let l = String.length m in
    let rec loop i =
      i = l
      || ( m.[i] = '.' && is_uppercase m.[i+1] && loop (i+2) )
      (* m.[i+1] exists because '.' cannot be the last char. *)
      || ( is_inner_char m.[i] && loop (i+1) ) in
    l <> 0 && is_uppercase m.[0] && m.[l-1] <> '.' && loop 1

  let of_string s =
    let rec loop l i0 i1 =
      if i0 = 0 then
        if is_uppercase s.[0] then String.sub s 0 (i1 + 1) :: l
        else raise(Invalid s)
      else if is_inner_char s.[i0] then loop l (i0 - 1) i1
      else if s.[i0] = '.' && is_uppercase s.[i0 + 1] then
        let i0' = i0 - 1 in
        loop (String.sub s (i0 + 1) (i1 - i0) :: l) i0' i0'
      else raise(Invalid s) in
    let i1 = String.length s - 1 in
    if i1 < 0 || s.[i1] = '.' then raise(Invalid s)
    else loop [] i1 i1

  (* <:ident< A.B.C >> = <:ident< (A.B).C >> *)
  let to_id m _loc =
    match m with
    | [] -> invalid_arg "Pa_do.Delimited_overloading.Module_longident.to_id"
    | i0 :: tl ->
        let idacc id i = <:ident< $id:id$ . $uid:i$ >> in
        List.fold_left idacc (<:ident< $uid:i0$ >>) tl

  (* The inverse of module_longident_to_id *)
  let of_id =
    let rec decompose acc = function
      | <:ident< $uid:i$ >> -> i :: acc
      | <:ident< $id:id$ . $uid:i$ >> -> decompose (i :: acc) id
      | _ -> invalid_arg "Pa_do.Delimited_overloading.Module_longident.of_id"
    in
    fun m -> decompose [] m


  let of_list l =
    List.iter (fun m ->
                 if not(is_capitalized_identifier m) then raise(Invalid m)) l;
    l


  (* Not tail rec but we do not expect fully qualified identifiers to be
     of length greater than the stack. *)
  let qualify id m =
    let _loc = Ast.loc_of_ident id in
    let q = List.fold_right (fun i e -> <:ident< $uid:i$ . $id:e$ >>) m id in
    (<:ident< $id:q$ >>)

  let qualify_lid lid m _loc =
    if is_lowercase_identifier lid then
      let i = qualify <:ident< $lid:lid$ >> m in <:expr< $id:i$ >>
    else raise (Invalid_identifier lid)

  let qualify_uid uid m _loc =
    if is_capitalized_identifier uid then
      let i = qualify <:ident< $uid:uid$ >> m in <:expr< $id:i$ >>
    else raise (Invalid_identifier uid)
end

(* For [Macro] only: *)
let raise_replace_by_longident name loc =
  let msg =
    sprintf "The parameter %S should be replaced by a longident" name in
  Loc.raise loc (Stream.Error msg)

let rec module_longident_of_id name loc id =
  try
    match id with
    | <:expr< $id:i$ >> -> Module_longident.of_id i
    | _ -> raise_replace_by_longident name loc
  with Invalid_argument _ -> raise_replace_by_longident name loc



(***********************************************************************
 *                              Hooks
 ***********************************************************************)

type param = Lid of string | Uid of string | Macro of string | Unused

(* Other macro packages may need some way of cooperating with macros,
   especially to delay their expansion with IFDEF.  This is why we
   provide  these hooks.  They are applied in the order they are defined. *)
module Hook =
struct

  let after_define_params = Queue.create ()
  let after_define = Queue.create ()
  let after_macro_expansion = Queue.create ()

  let apply_define_params (params: param list) =
    Queue.iter (fun f -> f params) after_define_params

  let define_params f = Queue.add f after_define_params

  let apply_define params body =
    Queue.fold (fun e f -> f params e) body after_define

  let define f = Queue.add f after_define

  let apply_macro_expansion (body: Ast.expr) =
    Queue.fold (fun e f -> f e) body after_macro_expansion

  let macro_expansion f = Queue.add f after_macro_expansion

(* FIXME: More hooks needed. *)
end

(***********************************************************************
 *                          Macro storage
 ***********************************************************************)

exception Invalid of string

(* Control which macros can be expanded.  In the body of a DEFINE, the
   parameters should not be expanded even if they are macros.  In a
   IFDEF branch, no macro is expanded because the branch can define
   other macros and we do not want that substituting these macros
   affects the body of already substituted macros.  Example:
   [DEFINE X = A  IFDEF X THEN  DEFINE A = 1  DEFINE B = X END]. *)
module Allow =
struct
  let suspend_level = ref 0
    (* to be able to suspend macro replacement (see [eval]). *)
  let not_for = Hashtbl.create 10

  let subst m = !suspend_level = 0 && not(Hashtbl.mem not_for m)

  (* We use a counter because there may be several groups. *)
  let disable () = incr suspend_level
  let reenable () = decr suspend_level

  (* No macros embedded in other => no need to count groupings *)
  let remove m = Hashtbl.add not_for m 0
  let clear () = Hashtbl.clear not_for
end

(* Defined macros (global value).  *)
let defined = Hashtbl.create 10

let is_defined_macro name = Hashtbl.mem defined name

let is_defined name =
  if is_uppercase_identifier name then is_defined_macro name
  else raise(Invalid name)

(***********************************************************************
 *                           Substitution
 ***********************************************************************)

(* As [loc_to_string] but without the "File" to also use it in
   [fname_with_loc].  The format is compatible with Emacs compilation mode. *)
let partial_loc_to_string loc =
  let (fname, line0, bol0, off0, line1, bol1, off1, _) = Loc.to_tuple loc in
  let line =
    if line0 = line1 then sprintf "%i" line0
    else sprintf "%i-%i" line0 line1 in
  (* "File \"" added by [Loc.to_string] or [loc_to_string] *)
  sprintf "%s\", line %s, characters %i-%i:"
    fname line (off0 - bol0) (off1 - bol1)

(* As of OCaml 3.11.0, Loc.to_string returns location strings
   incompatible with Emacs 22.3.  Thus we use: *)
let loc_to_string loc =
  "File \"" ^ (partial_loc_to_string loc)

(** [(new reloc_fname use_loc msg)#expr e] prefix all locations in the
    expression [e] with the llocation [use_loc] (typically the
    location where the macro is used) and the message [msg]. *)
class reloc_fname use_loc msg =
  let fname = partial_loc_to_string use_loc in
  let fname_with_loc _loc =
    let fname_loc =
      sprintf "%s\n  %s\n  File \"%s" fname msg (Loc.file_name _loc) in
    Loc.set_file_name fname_loc _loc in
object
  inherit Ast.map as super
  method loc _loc = fname_with_loc _loc
  method _Loc_t _loc = fname_with_loc _loc (* camlp4 3.10.0 *)
end

(* The original macros were really hard to debug because the location
   of the macro body is lost.  Here we use [loc], the location of the
   macro usage, to tweak the file name to report the location of use
   while the original locations are preserved so that your editor can
   easily send you to the point where the problem occurs inside the
   body of the macro. *)
class reloc use_loc macro =
  let fname_use = sprintf "Expanding of the macro \
      %S at the previous location yields the error:" macro in
  reloc_fname use_loc fname_use

type param_values = {
  lid : (string * Ast.expr) list; (* replacements for lid *)
  uid : (string * string list) list; (* replacements for modules params *)
  macro : (string * (Loc.t * string)) list; (* replacements for macros *)
}

(* Split and check the arguments depending on whether the variables
   are lowercase or uppercase. *)
let split_args use_loc macro args values macro_loc =
  let rec loop env a v =
    match a, v with
    | [], [] -> env
    | (Lid a) :: atl, v :: vtl ->
        loop { env with lid = (a, v) :: env.lid } atl vtl
    | (Uid a) :: atl, v :: vtl ->
        let i = module_longident_of_id a use_loc v in
        loop { env with uid = (a, i) :: env.uid } atl vtl
    | Macro a :: atl, v :: vtl ->
        let uid = match v with
          | <:expr@loc< $uid:s$ >> when is_uppercase_identifier s -> (loc, s)
          | _ ->
              let msg = sprintf "can only substitute the parameter \"%s()\" of \
                the\n  macro %S with an uppercase identifier.  See\n  \
                %s definition of %S."
                a macro (loc_to_string macro_loc) macro in
              Loc.raise use_loc (Stream.Error msg)
        in
        loop { env with macro = (a, uid) :: env.macro } atl vtl
    | Unused :: atl, _ :: vtl ->
        loop env atl vtl (* ignore the param value *)
    | _ ->
        let msg = sprintf "expected %d parameters; found %d.  \
          See\n  %s definition of %S."
          (List.length args) (List.length values)
          (loc_to_string macro_loc) macro in
        Loc.raise use_loc (Stream.Error msg)
  in
  loop { lid = []; uid = []; macro = [] } args values

exception Not_constructor

(** [get_constructor e] returns [(uid, args)] if [e] is (syntactically) a
    constructor.  @raise Not_constructor if [e] is not a constructor. *)
let get_constructor =
  (* The constructors are represented in curried form in Camlp4. *)
  let rec get = function
  | <:expr< $uid:u$ >> -> (u, [])
  | <:expr< $c$ $a$ >> -> let (u, args) = get c in (u, a :: args)
  | _ -> raise Not_constructor
  in
  fun e -> let (u, args) = get e in (u, List.rev args)

let rec expr_of_macro _loc macro args =
  (* The constructors are represented in curried form in Camlp4. *)
  List.fold_left (fun m a -> <:expr< $m$ $a$ >>) <:expr< $uid:macro$ >> args

(** [subst_macro use_loc macro params values body] transversal to
    substitute the parameters [params] with their corresponding
    [values] in the [body] of the macro. *)
class subst_macro use_loc macro params values body =
  let env = split_args use_loc macro params values (Ast.loc_of_expr body) in
object(self)
  inherit reloc use_loc macro as super

  method perform =
    (* First substitute all macro parameters with their values
       [self#expr body], only then apply the (now well qualified)
       overloadings or other hooks. *)
    Hook.apply_macro_expansion (self#expr body)

  method expr = function
  | <:expr@_loc< $lid:x$ >> as e ->
    (try
       let e' = List.assoc x env.lid in
       (* We substitute [x] by the expression [e'].  We want to
          preserve the locations of [e'] in case the error is inside [e']. *)
       let arg_loc = new reloc_fname _loc (
         sprintf "Expanding the macro %S, substituting the variable %S \
           yields:" macro x) in
       arg_loc#expr e'
     with Not_found -> super#expr e)
  | e ->
    (try
       let macro, values = get_constructor e (* or Not_constructor *) in
       let macro_loc, macro = List.assoc macro env.macro (* or Not_found *) in
       (* Transform params values first: *)
       let values = List.map self#expr values in
       (* Make sure one is allowed to expand the macro (e.g. this is not the
          case if the macro is an argument of a surrounding macro). *)
       if Allow.subst macro then (
         try
          (* Get the macro from the GLOBAL environment *)
           match Hashtbl.find defined macro with
           | Some([], _) | None ->
             let msg = sprintf "The macro %S must accept parameters." macro in
             Loc.raise macro_loc (Stream.Error msg)
           | Some(params, body) ->
             let loc = Ast.loc_of_expr e in
             (new subst_macro loc macro params values body)#perform
         with Not_found ->
           let msg = sprintf "The macro %S must be previously defined." macro in
           Loc.raise macro_loc (Stream.Error msg))
       else (
         (* One is not allowed to _expand_ the macro but if it is an
            argument of the rurrounding macro, its name must be changed. *)
         expr_of_macro (Ast.loc_of_expr e) macro values)
     with Not_found | Not_constructor -> super#expr e)

  method ident i = match i with
  | <:ident@loc< $uid:m$ >> ->
    (try
       let m' = List.assoc m env.uid in
       Module_longident.to_id m' (self#loc loc)
     with Not_found -> i)
  | _ -> super#ident i
end



let bad_patt _loc =
  let msg = "This macro cannot be used in a pattern (see its definition)" in
  Loc.raise _loc (Failure msg)

let patt_of_expr env =
  let rec loop e =
    let _loc = Ast.loc_of_expr e in
    match e with
    | <:expr< $e1$ $e2$ >> -> Ast.PaApp(_loc, loop e1, loop e2)
    (* <:patt< $e1$ $e2$ >> does not work *)
    | <:expr< >> -> <:patt< >>
    | <:expr< $lid:x$ >> ->
      (try List.assoc x env with Not_found -> <:patt< $lid:x$ >>)
    | <:expr< $uid:x$ >> ->
      (try List.assoc x env with Not_found -> <:patt< $uid:x$ >>)
    | <:expr< $int:x$ >> -> <:patt< $int:x$ >>
    | <:expr< $str:s$ >> -> <:patt< $str:s$ >>
    | <:expr< ($tup:x$) >> -> <:patt< ($tup:loop x$) >>
    | Ast.ExRec(_, bi, _) (* <:expr< { $bi$ } >> *) ->
        let rec substbi = function
          | <:rec_binding< $b1$; $b2$ >> ->
              <:patt< $substbi b1$; $substbi b2$ >>
          | <:rec_binding< $i$ = $e$ >> -> <:patt< $i$ = $loop e$ >>
          | _ -> bad_patt _loc   in
        <:patt< { $substbi bi$ } >>
    | _ -> bad_patt _loc
  in
  loop

(***********************************************************************
 *                         Macro definition
 ***********************************************************************)

module Macro_params =
struct
  let level_inside = ref 0
    (* global variable that tells whether the current parsing position is
       inside macro parameters values. *)
  let enter () = incr level_inside
  let leave () = decr level_inside
  let is_outside () = !level_inside = 0
end

let define_macro name args_expr =
  begin match args_expr with
  | Some([], e) ->
      EXTEND Gram
        GLOBAL: Syntax.expr Syntax.patt;
      Syntax.expr: LEVEL "simple"
        [ [ UIDENT $name$ ->
              if Allow.subst name then (new reloc _loc name)#expr e
              else <:expr< $uid:name$ >> ] ];
      Syntax.patt: LEVEL "simple"
        [ [ UIDENT $name$ ->
              if Allow.subst name then
                (* FIXME: does not work *)
                let p = patt_of_expr [] e in
                (new reloc _loc name)#patt p
              else <:patt< $uid:name$ >>       ] ];
      END
  | Some(params, body) ->
      EXTEND Gram
        GLOBAL: Syntax.expr Syntax.patt;

      values_enter: [ [ -> Macro_params.enter() ] ];
      Syntax.expr: LEVEL "apply"
        (* Force the parameter values of the macro to be inside braces
           (this also allows the second case not to be a particular
           case of the first): *)
        [ [ UIDENT $name$; "("; _ = values_enter; values = SELF; ")" ->
              Macro_params.leave();
              if Allow.subst name then
                (* The parameters have been read while macro expansion
                   was enabled => no need to transform them. *)
                let values = match values with
                  | <:expr< $tup:e$ >> -> Ast.list_of_expr e []
                  | e -> [e] in
                (new subst_macro _loc name params values body)#perform
              else
                <:expr< $uid:name$ $values$ >>
          | UIDENT $name$ ->
              if Macro_params.is_outside() then (
                let msg = sprintf "Warning: The macro %S exists but requires \
                arguments, left unchanged." name in
                print_warning _loc msg;
              );
              <:expr< $uid:name$ >>
          ] ];
      (*Syntax.patt: LEVEL "simple"
        [ [ UIDENT $name$; param = SELF ->
        let pl =match param with
        | <:patt< ($tup:p$) >> -> Ast.list_of_patt p []
        | p -> [p] in
        let env_var, env_mod = split_args _loc args pl in
        let e = (new subst_macro name _loc env_var env_mod)#expr e in
        let e = Delimited_overloading.Suspend.resolve e in
        patt_of_expr [] e
        ] ];*)
      END
  | None -> ()
  end;
  Hashtbl.add defined name args_expr

let bad_param test s =
  if not test then
    invalid_arg(sprintf "Pa_do.Macro.define: bad param name %S." s)

let define ?expr name =
  if is_uppercase_identifier name then
    let expr = match expr with
      | None -> None
      | Some(args, e) ->
          let check = function
            | Lid s -> bad_param (is_lowercase_identifier s) s
            | Uid s -> bad_param (is_capitalized_identifier s) s
            | Macro s -> bad_param (is_uppercase_identifier s) s
            | Unused -> () in
          List.iter check args;
          Some(args, e)
    in
    define_macro name expr
  else raise(Invalid name)

let undef name =
  if is_uppercase_identifier name then
    try
      let args_expr = Hashtbl.find defined name in
      (match args_expr with
       | Some([], _) ->
           DELETE_RULE Gram expr: UIDENT $name$ END;
           DELETE_RULE Gram patt: UIDENT $name$ END;
       | Some _ ->
           DELETE_RULE Gram expr: UIDENT $name$; SELF END;
           DELETE_RULE Gram patt: UIDENT $name$; SELF END;
       | None -> ());
      Hashtbl.remove defined name
    with Not_found -> ()
  else raise(Invalid name)


(***********************************************************************
 *                          File inclusion
 ***********************************************************************)

(* A list of directories to search for INCLUDE statements. *)
let include_dirs = ref []

(* Last added dirs will be first in the list but must be searched
   last. *)
let add_include_dir dir =
  if dir <> "" then
    include_dirs := dir :: !include_dirs

let file_exists file dir = Sys.file_exists (Filename.concat dir file)

let parse_include_file entry file =
  let file =
    try
      let dir = List.find (file_exists file) (List.rev !include_dirs) in
      Filename.concat dir file
    with Not_found -> file (* absolute path or relative to current dir *) in
  let ch = open_in file in
  let st = Stream.of_channel ch in
  let g = Gram.parse entry (Loc.mk file) st in
  close_in ch;
  g

let rec buffer_of_channel content buf len ch =
  let read = input ch buf 0 len in
  if read = 0 then content (* End of file *)
  else (
    Buffer.add_substring content buf 0 read;
    buffer_of_channel content buf len ch
  )

let include_as_string file =
  let file =
    try
      let dir = List.find (file_exists file) (List.rev !include_dirs) in
      Filename.concat dir file
    with Not_found -> file (* absolute path or relative to current dir *) in
  let ch = open_in file in
  let buf = String.create 0x1000 in
  let content = buffer_of_channel (Buffer.create 0x1000) buf 0x1000 ch in
  close_in ch;
  Buffer.contents content


(***********************************************************************
 *                          Macro language
 ***********************************************************************)

(* Because of the IFDEF construction, some parts if the AST must be
   "saved" to be processed later.  Hence the following type to hold
   the structure. *)

type 'a macro_or_item =
  | Def of string * (param list * Ast.expr) option
  | Undef of string
  | If_then_else of string * 'a macro_or_item list * 'a macro_or_item list
  | Item of 'a (* structure or sig item *)
  | Include of 'a Lazy.t

(* The body of the THEN or ELSE part may contain DEFINE, IFDEF,
   INCLUDE,... statements and items.  We do _not_ want to perform any
   macro subst. when reading them because, executing local macros
   later, has the risk of transforming the body of the already
   subst. macros.  => suspend replacement of macros when parsing these
   (this behavior is different from the macros provided with camlp4
   but is considered better).  *)

(** [expand_macros] is a class to expand currently defined macros (in
    the global environment) in expressions. *)
let expand_macros =
object(self)
  inherit Ast.map as super

  (* See [define_macro] *)
  method expr e =
    let _loc = Ast.loc_of_expr e in
    try
      let macro, values = get_constructor e (* or Not_constructor *) in
      if is_defined_macro macro && Allow.subst macro then
        (* The [values] have been read _without_ macro expansion =>
           first recurse to transform them. *)
        let values = List.map self#expr values in
        match Hashtbl.find defined macro with
        | Some([], body) ->
            (match (new reloc _loc macro)#expr body with
             | <:expr< $uid:_$ >> ->
               (* constructor => currying values (for Campl4) *)
               List.fold_left (fun c a -> <:expr< $c$ $a$ >>) body values
             | _ ->
                 (* Not constructor => tuple (only if more than one param) *)
                 match values with
                 | [] -> body
                 | [p] -> <:expr< $body$ $p$ >>
                 | _ -> <:expr< $body$ $tup: Ast.exCom_of_list values$ >>)
        | Some(params, body) ->
            (new subst_macro _loc macro params values body)#perform
        | None -> super#expr e
      else super#expr e
    with Not_constructor -> super#expr e

(* FIXME: patt *)
end


let rec eval ~delayed nil cons = function
  | Def(name, args_expr) ->
      let args_expr =
        if delayed then
          (* The define comes from a delayed block.  Substitute its
             body with existing macros except those which are masked
             by parameter names (it is already the case if the body
             was read with the macros turned on). *)
          match args_expr with
          | Some(pl, e) ->
              List.iter (function Uid x -> Allow.remove x | _ -> ()) pl;
              let e = expand_macros#expr e in
              Allow.clear();
              Some(pl, e)
          | None -> None
        else args_expr in
      define_macro name args_expr; nil
  | Undef name -> undef name; nil
  | If_then_else(name, st1, st2) ->
      eval_list nil cons (if is_defined_macro name then st1 else st2)
  | Item s ->
      (* This comes from a delayed IFDEF branch.  [s] may contain
         macros to be expanded -- but no macros defs. *)
      expand_macros#str_item s
  | Include f -> Lazy.force f

and eval_list nil cons = function
  | [] -> nil
  | s :: tl ->
      (* Order important: [s] possible DEFINE side effect *)
      let i1 = eval ~delayed:true nil cons s in
      let i2 = eval_list nil cons tl in
      cons i1 i2


(* FIXME: provide generic MAX, MIN, MINMAX, and SORT macros based on
   the ideas
   http://alaska-kamtchatka.blogspot.com/2009/03/small-sorts.html
   (including comments).  For efficiency:

   MINMAX(a1,...,an) (fun the_min the_max -> expr)

   where the variables [the_min] and [the_max] are substituted in
   [expr] -- so [expr] is copied at the leaves of the sort if needed
   (similarly for SORT).
*)

(***********************************************************************
 *                         Concrete syntax
 ***********************************************************************)

open Syntax

let check_duplicate_params =
  let rec has_dup = function
    | [] | [_] -> false
    | Unused :: tl -> has_dup tl
    | a :: ((b :: _) as tl) -> a = b || has_dup tl in
  fun _loc pl ->
    if has_dup (List.sort compare pl) then (
      let msg = "Duplicate macro parameter not allowed." in
      Loc.raise _loc (Stream.Error msg);
    )

let () =
  EXTEND Gram
    GLOBAL: expr patt str_item sig_item;

  str_item: FIRST
    [ [ stm = macro_statement ->
          let cons a b = <:str_item< $a$ $b$ >> in
          eval ~delayed:false (<:str_item< >>) cons stm ]];

  opt_macro_value:
    [ [ "("; pl = macro_params; ")"; "="; e = expr ->
          (* Allow again the expansion of macros with names identical
             to parameters.  No macro definition can be embedded into
             another => no multiple levels to take care. *)
          (Allow.clear();
           Some (pl, Hook.apply_define pl e))
      | "="; e = expr -> Some ([], Hook.apply_define [] e)
      | -> None
      ] ];
  macro_params:
    [ [ pl = LIST1 [x = macro_1param -> x] SEP "," ->
          (check_duplicate_params _loc pl;
           Hook.apply_define_params pl;
           pl)
      ] ];
  macro_1param:
    [ [ x = LIDENT -> Lid x
      | x = UIDENT; "("; ")" ->
          (* Do not allow [x] to be expanded in the macro body even if
             it is a macro name. *)
          Allow.remove x;
          Macro x
      | x = UIDENT ->
          Allow.remove x;
          Uid x
      | "_" -> Unused ] ];
  macro_statement:
    [ [ "DEFINE"; i = UIDENT; def = opt_macro_value -> Def(i, def)
      | "UNDEF"; i = UIDENT -> Undef i

      | "IFDEF"; i = ifdef_uident; "THEN"; st1 = smlist; st2 = else_macro ->
          (Allow.reenable();
           If_then_else(i, st1, st2))
      | "IFNDEF"; i = ifdef_uident; "THEN"; st1 = smlist; st2 = else_macro ->
          (Allow.reenable();
           If_then_else(i, st2, st1))

      | "INCLUDE"; fname = STRING ->
          Include(lazy (parse_include_file str_items fname))
      ] ];
  ifdef_uident:
    [ [ i = UIDENT -> Allow.disable(); i ] ];
  (* Store away the str_item for later eval: *)
  smlist:
    [ [ sml = LIST1
          [ d = macro_statement; semi -> d
          | si = str_item; semi -> Item si ] -> sml     ] ];
  else_macro:
    [ [ "ELSE"; st = smlist; endif -> st
      | endif -> []     ] ];
  endif:
    [ [ "END" -> ()
      | "ENDIF" -> () ] ];

  expr: LEVEL "top"
    [ [ "IFDEF"; i = UIDENT; "THEN"; e1 = expr; e2 = else_expr ->
          if is_defined_macro i then e1 else e2
      | "IFNDEF"; i = UIDENT; "THEN"; e1 = expr; e2 = else_expr ->
          if is_defined_macro i then e2 else e1
      | "INCLUDE_AS_STRING"; fname = STRING ->
          <:expr< $str:include_as_string fname$ >>
      ] ];
  else_expr:
    [ [ "ELSE"; e = expr; endif -> e
      | endif -> <:expr< () >>
      ] ];

  (* FIXME: macros in sig *)

  (* Independent of the macro machinery: *)
  expr: LEVEL "simple"
    [ [ LIDENT "__FILE__" -> <:expr< $`str:Loc.file_name _loc$ >>
      | LIDENT "__LOCATION__" ->
          let (fname, line0, bol0, off0, line1, bol1, off1, ghost) =
            Loc.to_tuple _loc in
          let g = if ghost then <:expr< true >> else <:expr< false >> in
          <:expr< Loc.of_tuple
            ($`str:fname$, $`int:line0$, $`int:bol0$, $`int:off0$,
             $`int:line1$, $`int:bol1$, $`int:off1$, $g$) >> ] ];
  END;;

(* FIXME: do not activate macros by default or provide a separate
   module, say pa_do_macro *)

(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
