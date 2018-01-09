(*pp camlp4 *)
(* File: pa_infix.ml

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


module Id = struct
  let name = "pa_infix"
  let version = "1.0"
end

open Printf
open Camlp4
open PreCast
open Syntax

module OCaml = Camlp4.Printers.OCaml.Make(Syntax)

let interactive = !Sys.interactive

type operator = string

type kind = Prefix | Infix | Postfix


module Level =
struct
  type assoc = Sig.Grammar.assoc = NonA | RightA | LeftA

  (** Print type assoc to debug **)
  let string_of_assoc = function
    | RightA -> "RightA"
    | LeftA  -> "LeftA"
    | NonA  -> "NonA"

  type unary
  type binary
  type arity = Unary | Binary
  type level = {
    nickname: string; (* nickname for error reporting *)
    name: string;     (* The camlp4 name for the level *)
    arity: arity;
    assoc: assoc;
  }
  type 'a t = level
      (* We use the phantom type trick for increased security on the
         outside, but on the inside, this is the same. *)

  type 'a precedence =
    | Highest
    | Higher of 'a t
    | Lower of 'a t
    | Lowest

  (* Default admissible levels *)
  let assignment = { name=":=";  assoc=NonA; arity=Binary;
                     nickname= "Level.assignment" }
  let disjunction = { name="||";  assoc=RightA; arity=Binary;
                      nickname= "Level.disjunction" }
  let conjunction = { name="&&";  assoc=RightA; arity=Binary;
                      nickname= "Level.conjunction" }
  let comparison = { name="<";  assoc=LeftA; arity=Binary;
                     nickname= "Level.comparison" }
  let concatenation = { name="^";  assoc=RightA; arity=Binary;
                        nickname= "Level.concatenation" }
  let addition = { name="+";  assoc=LeftA; arity=Binary;
                   nickname= "Level.addition" }
  let multiplication = { name="*";  assoc=LeftA; arity=Binary;
                         nickname= "Level.multiplication" }
  let exponentiation = { name="**";  assoc=RightA (*sic*); arity=Binary;
                         nickname= "Level.exponentiation" }
  let default_unary = { name="~-";  assoc=NonA; arity=Unary;
                        nickname= "Level.default_unary" }

  let name l = l.nickname

  let assoc l = l.assoc

  let arity l = l.arity

  exception Bad_arity

  (* [check_arity arity_fct l] raises the exception [Bad_arity] if
     the arity [arity_fct] required by a function is not the same as
     the arity of level [l]. *)
  let check_arity arity_fct level =
    if arity_fct <> (arity level) then raise Bad_arity

  (* Convert the level to a string suitable for the EXTEND statement
     (it already exists, so we just refere to it). *)
  let to_camlp4 l = Some(Sig.Grammar.Level l.name)

  (* Create an empty camlp4 level.  The idea is to use

     EXTEND Gram
       GLOBAL: expr;
       expr: $relative_pos$
         [ $name$ $assoc$      [  ] ];
     END;;

     But the anti-quotation does not work so one directly uses the
     Camlp4 API.  *)
  let camlp4_create name position assoc =
    try
      Gram.extend expr
        (Some position, [(Some name, Some assoc, [(* no production rule *)])])
    with _ -> failwith "Pa_infix: you must load a grammar"

  (* Internal unattainable levels delimiting the level range of the
     operators *)
  let highest_binary = "__Pa_infix.binary_highest__"
  let lowest_binary = "__Pa_infix.binary_lowest__"
  let highest_unary = "__Pa_infix.unary_highest__"
  let lowest_unary = "__Pa_infix.unary_lowest__"
  let () =
    camlp4_create highest_binary (Sig.Grammar.After exponentiation.name) NonA;
    camlp4_create lowest_binary (Sig.Grammar.Before assignment.name) NonA;
    camlp4_create highest_unary (Sig.Grammar.After default_unary.name) NonA;
    camlp4_create lowest_unary (Sig.Grammar.Before default_unary.name) NonA

  let level_no = ref 0

  let create arity_fct nickname maybe_assoc lowest highest relpos =
    (* FIXME: name creation might be better thought out for an easy
       [compare] function. *)
    incr level_no;
    let name = "__Pa_infix__" ^ string_of_int !level_no in
    let position, assoc = match relpos with
      | Highest -> (Sig.Grammar.Before highest, NonA)
      | Higher l ->
          check_arity arity_fct l;
          (Sig.Grammar.After l.name, assoc l)
      | Lower l ->
          check_arity arity_fct l;
          (Sig.Grammar.Before l.name, assoc l)
      | Lowest -> (Sig.Grammar.After lowest, NonA) in
    let assoc = match maybe_assoc with None -> assoc | Some a -> a in
    let nickname = match nickname with
      | None -> (match relpos with
                 | Highest -> sprintf "Highest %i" !level_no
                 | Higher l ->
                     sprintf "Higher %s (%i)" l.nickname !level_no
                 | Lower l ->
                     sprintf "Lower %s (%i)" l.nickname !level_no
                 | Lowest -> sprintf "Lower %i" !level_no)
      | Some n -> n in
    camlp4_create name position assoc;
    { name = name;  assoc = assoc; arity = arity_fct;
      nickname = nickname }

  let binary ?name ?assoc relpos =
    create Binary name assoc lowest_binary highest_binary relpos

  let unary ?name relpos =
    create Unary name (Some NonA) lowest_unary highest_unary relpos

  let equal l1 l2 = l1.name = l2.name

  let compare l1 l2 = String.compare l1.name l2.name
end

(** Datastructure and operations for operators which are set. *)
module Op =
struct
  (** List of new or redefined (level or kind) operators.
      It prevents the user from defining, in the future, these operators
      as operators of another kind or at another level.
      It contains elements of type [operator * kind * 'a Level.t]. *)
  let ops = ref []
    (* FIXME: maybe convert it later to a Map. *)

  (* [get op] returns the triplet [(op,k,l)] of the list [ops].  Raises
     [Not_found] if [op] does not belong to the list. *)
  let get op =
    List.find (fun (o,_,_) -> o = op) !ops

  let level op =
    let (_,_,given_level) = get op in given_level

  let kind op =
    let (_,given_kind,_) = get op in given_kind

  let add op kind level =
    ops := (op,kind,level) :: !ops

  let is_set op = List.exists (fun (o,_,_) -> o = op) !ops
end

(***********************************************************************
 *                        Allowed operators
 ***********************************************************************)

(* Forbidden & extra operator names
 ***********************************************************************)

exception Not_operator of operator
exception Forbidden of operator

(* List of the operators of which we do not allow to change the
   priority and associativity (because either it is not natural, will
   change the meaning of standard OCaml code, or it doesn't work
   because of conflicts with other rules). *)
let forbidden_ops =
  [ "+"; "-"; "~-"; "*"; "/";
    "+."; "-."; "~-."; "*."; "/."; "**";
    "="; "<>"; "<"; ">"; "<="; ">=";
    "=="; "!="; ":=";
    "^"; "!";
    "||"; "or"; "&&";  (* boolean ops *)
    (* Remark: "->", "<-", ":", ":>", ".", "~", "?" and "|" can not
       be used as operators. They are not matched by [prefixop],
       [infixop0],... and we do not want to allow to change their meaning. *)
    (* FIXME: What about "@"? *)
    (* None of OCaml keywords can be used as an operator but we use the
       function from Camlp4 to determine those. *)
    "or";
    "mod";	"land";		"lor";
    "lxor";	"lsl";		"lsr";
    "asr"
  ]

(* Tells whether [op] is a pure alphabetical non-empty string starting
   with a lowercase. *)
let is_alpha op =
  let is_lowercase c = 'a' <= c && c <= 'z' in
  let is_letter c = is_lowercase c || ('A' <= c && c <= 'Z') in
  let rec loop i w =
    (i = String.length w) || (is_letter w.[i] && loop (i + 1) w)  in
  op <> "" && is_lowercase op.[0] && loop 1 op


(* FIXME: do we allow (possibly with a flag) \lident to be used as an
   infix (this setting its precedence and assoc on a case by case
   basis).  The flag couls tell which first char is used ("$",
   "`",... may be good candidates, "" means disabled) *)


(* Standard operator names
 ***********************************************************************)
(* Standard operators are those accepted by OCaml without preprocessing.
   Inspired by Camlp4OCamlRevisedParser.ml *)

(* Exclude an operator [x] as soon as it is set (even at its default
   level).  This is because we allow its substitution by a possibly
   different expression than the default function application. *)
let setup_parser entry level is_op =
  Gram.Entry.setup_parser entry
    (parser
    | [< '((KEYWORD x | SYMBOL x), ti) when not(Op.is_set x) && is_op x >] ->
      let _loc = Gram.token_location ti in
      <:expr< $lid:x$ >>)

(* [symbolchar s i] checks that [s.[i...]] contains only allowed symbols. *)
let symbolchar =
  let list =
    ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
     '@'; '^'; '|'; '~'; '\\'] in
  let rec loop s i =
    (i = String.length s) || (List.mem s.[i] list && loop s (i + 1)) in
  loop

(* Prefix operators (start with '!', '?', '~') *)
let is_prefixop op=
  let list = ['!'; '?'; '~'] in
  let excl = ["!="; "??"] in
  not (List.mem op excl) && String.length op >= 2 &&
    List.mem op.[0] list && symbolchar op 1

let () = setup_parser prefixop Level.default_unary is_prefixop

(* infix operator (level 0) (comparison operators, and some others) *)
let is_infixop0 op =
  let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
  let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
  let excl = ["<-"; "||"; "&&"] in
  List.mem op list_ok ||
    (not (List.mem op excl) && String.length op >= 2 &&
       List.mem op.[0] list_first_char_ok && symbolchar op 1)

let () = setup_parser infixop0 Level.comparison is_infixop0

(* infix operator (level 1) (start with '^', '@') *)
let is_infixop1 op =
  let list = ['@'; '^'] in
  String.length op >= 1 && List.mem op.[0] list && symbolchar op 1

let () = setup_parser infixop1 Level.concatenation is_infixop1

(* infix operator (level 2) (start with '+', '-') *)
let is_infixop2 op =
  let list = ['+'; '-'] in
  op <> "->" && String.length op >= 1 && List.mem op.[0] list && symbolchar op 1

let () = setup_parser infixop2 Level.addition is_infixop2

(* infix operator (level 3) (start with '*', '/', '%') *)
let is_infixop3 op =
  let list = ['*'; '/'; '%'; '\\'] in
  String.length op >= 1 && List.mem op.[0] list &&
    (op.[0] <> '*' || String.length op < 2 || op.[1] <> '*') && symbolchar op 1

let () = setup_parser infixop3 Level.multiplication is_infixop3

(* infix operator (level 4) (start with "**") (right assoc) *)
let is_infixop4 op =
  String.length op >= 2 && op.[0] = '*' && op.[1] = '*' && symbolchar op 2

let () = setup_parser infixop4 Level.exponentiation is_infixop4

(* HACK: the camlp4 [infixop5] is not public.  However, defining a new
   grammar entry with the _same_name_ allows DELETE_RULE to erase the
   old rule involving infixop5, namely the level "&&".  Then we
   recreate it with our new entry. *)
let infixop5 = Gram.Entry.mk "infixop5"

let is_infixop5 op = op = "&&" || op = "&"

let () =
  setup_parser infixop5 Level.conjunction is_infixop5;
  (try DELETE_RULE Gram expr: SELF; infixop5; SELF END
   with Not_found -> failwith "Pa_infix: you must load a grammar");
  try
    EXTEND Gram
      GLOBAL: expr infixop5;

    expr: AFTER "||"
      [ "&&" RIGHTA  [ e1 = SELF; op = infixop5; e2 = SELF ->
                         <:expr< $op$ $e1$ $e2$ >> ] ];
    END;
  with _ -> failwith "Pa_infix: you must load a grammar"


let is_infixop6 op = op = "or" || op = "||"
  (* We can question these about their level or associativity but not
     redefine them. *)

(* Alphabetical operators
 ***********************************************************************)

(* [is_set_as_alpha op] returns yes if the operator [op] has been set
   "manually" as an alphabetic operator. *)
let is_set_as_alpha op = is_alpha op && Op.is_set op

let rec op_make_kwds_filter =
  parser
  | [< '(LIDENT x, _loc) when is_set_as_alpha x; xs >] ->
      [< '(KEYWORD x, _loc); op_make_kwds_filter xs >]
  | [< 'x; xs >] -> [< 'x; op_make_kwds_filter xs >]

let rec op_kwds_filter =
  parser
  | [< '((KEYWORD "(", _) as tok); xs >] ->
      (match xs with parser
         [< '(KEYWORD x, _loc) when is_set_as_alpha x;
            '(KEYWORD ")", _); xs >] ->
           [< '(LIDENT x, _loc); op_kwds_filter xs >]
       | [< xs >] ->
           [< 'tok; op_kwds_filter xs >])
  | [< 'x; xs >] -> [< 'x; op_kwds_filter xs >]

let () =
  Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> op_make_kwds_filter (f strm));
  Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> op_kwds_filter (f strm))


(* Operator predicates
 ***********************************************************************)

let is_prefix op =
  is_prefixop op || op = "!"
  (* "!" is not matched by the function [is_prefixop], so we add it as
     an operator to be able to write e.g. [level "!"]  *)

let is_infix op =
  is_infixop0 op || is_infixop1 op || is_infixop2 op
  || is_infixop3 op || is_infixop4 op || is_infixop5 op
  || is_infixop6 op || op = ":=" (* ":=" is an operator but we do not
                                    want to redefine it. *)
  || is_alpha op (* default kind for alphabetic ops is infix *)

let is_operator op =
  op <> "" && (is_prefix op || is_infix op)

let check_is_operator op =
  if not(is_operator op) then raise(Not_operator op)

let is_forbidden op =
  List.mem op forbidden_ops || OCaml.is_keyword op

let check_is_not_forbidden op =
  check_is_operator op;
  if is_forbidden op then raise(Forbidden op)


(***********************************************************************
 *             Changing the associativity and priority
 ***********************************************************************)

let string_of_kind = function
  | Prefix -> "Prefix"
  | Infix -> "Infix"
  | Postfix -> "Postfix"

exception Conflicting_kind of operator * kind * kind
exception Conflicting_level of operator * string * string
  (* FIXME: now that we have handle error, why not to use Level.t? *)

(* [given_kind op] returns the kind of the operator [op] that has been
   given "manually".
   Raises [Not_operator op] if [op] is not a valid operator name.
   Raises [Not_found] if [op] is not in the list [ops]. *)
let given_kind op =
  check_is_operator op;
  Op.kind op

(* [default_kind op] returns the kind by default for the operator [op].
   Raises [Not_operator op] if [op] is not a valid operator name. *)
let default_kind op =
  check_is_operator op;
  if is_prefix op then Prefix
  else if is_infix op then Infix
  else assert false (* It should not happen because if [op] is an
                         operator, it falls in one of the previous cases. *)

(* [kind op] returns the kind of the operator [op]: if [op] is not an
   operator, it raises the exception [Not_operator]; if the kind of [op] has
   been set "manually", then it is returned, else the kind by defaut is
   returned. *)
let kind op =
  try given_kind op
  with Not_found -> default_kind op

(* [given_level op] returns the level at which the operator [op] has been
   given "manually".
   Raises [Not_operator op] if [op] is not a valid operator name.
   Raises [Not_found] if [op] is not in the list [ops]. *)
let given_level op =
  check_is_operator op;
  Op.level op

(* [default_level op] returns the level by default for the operator [op].
   Raises [Not_operator op] if [op] is not a valid operator name. *)
let default_level op =
  check_is_operator op;
  if is_prefix op then Level.default_unary
  else if is_infixop0 op then Level.comparison
  else if is_infixop1 op then Level.concatenation
  else if is_infixop2 op then Level.addition
  else if is_infixop3 op || is_alpha op then Level.multiplication
  else if is_infixop4 op then Level.exponentiation
  else if is_infixop5 op then Level.conjunction
  else if is_infixop6 op then Level.disjunction
  else if op = ":=" then Level.assignment
  else assert false (* It should not happen because if [op] is an
                         operator, it falls in one of the previous cases. *)

(* [level op] returns the level of the operator [op]: if [op] is not an
   operator, it raises the exception [Not_operator]; if the level of [op] has
   been set "manually", then it is returned, else the level by defaut is
   returned. *)
let level op =
  try given_level op
  with Not_found -> default_level op

let assoc op =
  (* No need to check if [op] is an operator because [level op] makes it. *)
  Level.assoc(level op)

(* [nickname l] returns the [nickname] of the level [l]. *)
let nickname l = l.Level.nickname

(** [check_kind op] checks that the operator [op] has not already been
    defined with another kind.
    @raise Conflicting_kind(op,prev_kind,kind) if the operator [op]
    was of kind [prev_kind] and now one wants to set it to [kind]. *)
let check_kind op kind =
  try
    let prev_kind = given_kind op in
    if prev_kind <> kind then
      raise(Conflicting_kind(op, prev_kind, kind))
  with Not_found -> ()

(** [check_level op] checks that the operator [op] has not already been
    defined at another level.
    @raise Conflicting_level(op,prev_level,level) if the operator [op]
    was at level [prev_level] and now one wants to set it to [level]. *)
let check_level op level =
  try
    let prev_level = given_level op in
    if prev_level <> level then
      raise(Conflicting_level(op, nickname prev_level, nickname level))
  with Not_found -> ()

(** [arity_of_kind k] returns the arity of the kind [k] (Infix -> Binary,
    Prefix | Postfix -> Unary). *)
let arity_of_kind kind =
  match kind with
  | Infix -> Level.Binary
  | Prefix | Postfix -> Level.Unary

(** [check_and_add op k l] checks that the operator [op] is not forbidden,
    that the kind [k] and the level [l] are allowed (ie, checks that
    [op] has not been defined earlier with a different kind or level) and
    that the arity of the level [l] is not in conflict with the kind [k].
    If the checks are OK, [(op,k,l)] is added to the list [ops]. *)
let check_and_add op kind level =
  check_is_not_forbidden op;
  if interactive && is_alpha op then
    failwith "Alphanumeric operators do not currently work in the toploop.";
  check_kind op kind;
  check_level op level;
  Level.check_arity (arity_of_kind kind) level;
  Op.add op kind level

(** [is_set op] returns [true] if the operator [op] has been set "manually". *)
let is_set op =
  check_is_operator op;
  not(is_forbidden op) && Op.is_set op

(* We want to write

   EXTEND Gram
   GLOBAL: expr;
   expr: $position$
   [$name$ $assoc$
   [x = SELF; [SYMBOL $op$ | KEYWORD $op$]; y = SELF ->
   <:expr< $lid:op$ $x$ $y$>>]];
   END

   However the $position$, $name$ and $assoc$ antiquotations do not
   work.  As we wanted to avoid repeating the EXTEND statement 15
   times for binary operators (3 assoc + None, 3 levels + None) and
   need a way to set up new names, we directly use the Caml code that
   the EXTEND statement would have generated (tweaked for clarity).
   (We used camlp4of on the wanted code to have a starting point for
   the code below.)

   Since the same has been done for unary operators, we start by
   defining the common code to both cases.

   BEWARE that [Gram.Action.mk] is UNSAFE in camlp4, its number of
   parameters is the length of the preceding rule plus _loc
*)

(* [symbol_op op] returns the rule [SYMBOL $op$ | KEYWORD $op$] *)
let symbol_op =
  let is_KEYWORD op = function KEYWORD k when k = op -> true | _ -> false
  and is_SYMBOL op = function SYMBOL s when s = op -> true | _ -> false
  and no_action = Gram.Action.mk (fun token _loc -> ()) in
  fun op ->
    Gram.srules expr
      [ ([Gram.Stoken(is_KEYWORD op, "$KEYWORD op") ], no_action);
        ([Gram.Stoken(is_SYMBOL op,  "$SYMBOL op") ],  no_action)
      ]

let extend_expr op l production_rule =
  Gram.extend expr
    (Level.to_camlp4 l,  [ (None, None, [ production_rule ]) ])

let app2 op = fun x y _loc -> (<:expr< $lid:op$ $x$ $y$ >>)
let expr_app1 expr op = match expr with
  | Some f -> f
  | None -> fun x _loc -> (<:expr< $lid:op$ $x$ >>)

let infix op ?(expr=app2 op) (level: Level.binary Level.t) =
  (* Make the checks and add [(op,Infix,l)] to the list [ops]. *)
  check_and_add op Infix level;
  let production_rule = (* Production rule for infix operators. *)
    [ Gram.Sself; symbol_op op; Gram.Sself ],
    (Gram.Action.mk (fun y _ x _loc -> expr x y _loc)) in
  extend_expr op level production_rule

let prefix ?expr ?(level=Level.default_unary) op =
  (* Make the checks and add [(op,Prefix,level)] to the list [ops]. *)
  check_and_add op Prefix level;
  let expr = expr_app1 expr op in
  let production_rule = (* Production rule for prefix operators. *)
    [ symbol_op op; Gram.Sself ],
    (Gram.Action.mk (fun x _ _loc -> expr x _loc )) in
  extend_expr op level production_rule

let postfix ?expr ?(level=Level.default_unary) op =
  (* Make the checks and add [(op,Postfix,level)] to the list [ops]. *)
  check_and_add op Postfix level;
  let expr = expr_app1 expr op in
  let production_rule = (* Production rule for postfix operators. *)
    [ Gram.Sself; symbol_op op ],
    (Gram.Action.mk (fun _ x _loc -> expr x _loc )) in
  extend_expr op level production_rule

(***********************************************************************
 *                         Concrete syntax
 ***********************************************************************)

(** [msg_of_exn exn] transforms the exception [exn] into a meaningful
    error message. *)
let handle_exn error f a =
  try f a
  with
  | Not_operator op ->
      error("The string \"" ^ op ^ "\" does not designate \
        a valid operator name.")
  | Forbidden op ->
      error("Changing the operator \"" ^ op ^ "\" is not allowed.")
  | Level.Bad_arity ->
      error "Using an unary level for a binary operator or vice versa."
  | Conflicting_kind (op,k0,k) ->
      error(sprintf "Conflicting kind for operator %S, was %S but now \
	        trying to set it to %S."
              op (string_of_kind k0) (string_of_kind k))
  | Conflicting_level (op,l0,l) ->
      error(sprintf "Conflicting level for operator %S, was %S but now \
		trying to set it to %S."
              op l0 l)

let handle_error =
  (* FIXME: Maybe re-raise exceptions to have a traceback. *)
  let error msg =
    print_string "Pa_infix: ";
    print_endline msg;
    exit 2 in
  fun f a -> handle_exn error f a


(* Redefine [check_is_operator] with an error handling appropriate for
   the concrete syntax.  *)
let check_is_operator _loc op =
  let error msg = Loc.raise _loc (Stream.Error msg) in
  handle_exn error check_is_operator op

(* Global level so it can be deleted. *)
let pa_infix_declaration = Gram.Entry.mk "pa_infix_declaration"
;;

EXTEND Gram
  GLOBAL: str_item pa_infix_declaration;

str_item:
  [ [ (loc, set) = pa_infix_declaration ->
        let error msg = Loc.raise loc (Stream.Error msg) in
        handle_exn error set ();
        <:str_item< >>
    ] ];

pa_infix_declaration:
  [ [ "INFIX"; op = operator; a = associativity; p = relative_precedence ->
        (_loc, (fun _ -> infix op (Level.binary ~assoc:a p)))
    | "INFIX"; op = operator; p = relative_precedence; a = associativity ->
        (* Same as 1st rule but needed because camlp4 3.10.2 does not
           support [A | B -> body] (which it interprets as [A -> () |
           B -> body]).  See OCaml bug #0004590. *)
        (_loc, (fun _ -> infix op (Level.binary ~assoc:a p)))
    | "INFIX"; op = operator; p = precedence ->
        (* If one gives a level, one inherits is associativity *)
        (_loc, (fun _ -> infix op p))
    | "INFIX"; op = operator; p = relative_precedence ->
        (* Default associativity for the new level *)
        (_loc, (fun _ -> infix op (Level.binary p)))

    | "PREFIX"; op = operator; p = relative_precedence ->
        (_loc, (fun _ -> prefix op ~level:(Level.unary p)))
    | "PREFIX"; op = operator; p = precedence ->
        (_loc, (fun _ -> prefix op ~level:p))
    | "PREFIX"; op = operator ->
        (_loc, (fun _ -> prefix op))

    | "POSTFIX"; op = operator; p = relative_precedence ->
        (_loc, (fun _ -> postfix op ~level:(Level.unary p)))
    | "POSTFIX"; op = operator; p = precedence ->
        (_loc, (fun _ -> postfix op ~level:p))
    | "POSTFIX"; op = operator ->
        (_loc, (fun _ -> postfix op))
    ] ];
associativity:
  [ [ UIDENT "LEFTA" -> Level.LeftA
    | UIDENT "RIGHTA" -> Level.RightA
    | UIDENT "NONA" -> Level.NonA ] ];
relative_precedence:
  [ [ UIDENT "HIGHEST" -> Level.Highest
    | UIDENT "HIGHER"; op = operator -> Level.Higher(level op)
    | UIDENT "LOWER"; op = operator -> Level.Lower(level op)
    | UIDENT "LOWEST" -> Level.Lowest
    ] ];
precedence:
  [ [ UIDENT "LEVEL"; op = operator -> level op ] ];
operator:
  [ [ op = LIDENT -> check_is_operator _loc op; op
    | "("; op = LIDENT; ")" -> check_is_operator _loc op; op
    ] ];
END;;



let desactivate_concrete_syntax() =
  DELETE_RULE Gram str_item: pa_infix_declaration END
;;

let () =
  Options.add "-no-pa-infix" (Arg.Unit desactivate_concrete_syntax)
    " Turns off the concrete syntax for changing operators.";

(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
