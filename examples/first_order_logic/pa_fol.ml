(* Syntax extension to enable the use of [Fol.(1 + 3 * `x**3 + 18 * `x
 * `y**5)] for multi-variables polynomials.  In fact we will do more
 * and allow an easy writing of First Order Logic expressions like [1
 * < 2 && (forall x; 1 < x => 1 < x**2) && (forall x y; 1 < x && 1 < y
 * => 1 < x * (1 + 2 * y))].
 *
 * In order to show the usefullness of this endeavor, we uses the same
 * data structures as the code http://www.cl.cam.ac.uk/users/jrh/atp
 * In particular, we want to be able write the examples in the
 * "real.ml" file.  *)

open Camlp4.PreCast
open Pa_infix
open Pa_do.Delimited_overloading

let library_basename = "fol.ml"

(* Qualify only if we are not inside the library [Fol]. *)
let qualify_uid u m _loc =
  let basename = Filename.basename (Loc.file_name _loc) in
  if basename = library_basename then <:expr< $uid:u$ >>
  else Pa_do.Macro.Module_longident.qualify_uid u m _loc

(* Make sure the new binary operators used have the right priority and
   associativity. *)
let () =
  let l = Level.binary (Level.Lower Level.disjunction) in
  infix "=>" l;         (* implication *)
  infix "<=>" l;        (* iff *)
  let l = Level.binary (Level.Higher Level.disjunction) ~assoc:Level.RightA in
  infix "//" l          (* separate variables after quantification *)

(* Polynomials.  FIXME: they need to be presented in a specific form. *)
let rec term_of_expr tr e =
  let _loc = Ast.loc_of_expr e in
  let q c = qualify_uid c (transf_module tr) _loc in
  match e with
    (* "^" cannot be used for exponentiation because of bad priority
       properties. *)
  | <:expr< $lid:f$ $x$ $y$ >> when List.mem f ["+"; "-"; "*"] ->
    <:expr< $q "Fn"$ ($str:f$, [$term_of_expr tr x$; $term_of_expr tr y$]) >>
  | <:expr< $x$ ** $y$ >> ->
    <:expr< $q "Fn"$ ("^", [$term_of_expr tr x$; $term_of_expr tr y$]) >>
  | <:expr< ~- $x$ >> -> (* unary minus *)
    <:expr< $q "Fn"$ ("-", [$term_of_expr tr x$]) >>
  | _ -> super tr e

let is_capital c = 'A' <= c && c <= 'Z'

let overloading_term =
  let of_int i m _loc =
    <:expr< $qualify_uid "Fn" m _loc$ ($str:string_of_int i$, []) >> in
  let t = int empty of_int ~cache:false in
  (* `P (capitalized) -> proposition; `x (non capitalized) -> variable *)
  let of_poly_var p m _loc =
    if is_capital p.[0] then <:expr< $qualify_uid "P" m _loc$ ($str:p$) >>
    else <:expr< $qualify_uid "Var" m _loc$ ($str:p$) >> in
  let t = poly_var t ~check:(fun _ -> ()) of_poly_var ~cache:false in
  (* Opening the module is not needed and is detrimental in fol.ml
     (since the module [Fol] does not exist yet there). *)
  let t = openin t ~remove:true in
  expr t term_of_expr

let term m x = apply overloading_term m x

let relations = ["="; "<"; "<="; ">"; ">="]

let atom_of_expr tr e =
  let _loc = Ast.loc_of_expr e in
  match e with
  | <:expr< $lid:f$ $x$ $y$ >> when List.mem f relations ->
     let m = transf_module tr in
     <:expr< $qualify_uid "R" m _loc$ ($str:f$, [$term m x$; $term m y$]) >>
  | _ -> super tr e

let overloading_atom =
  let of_poly_var p m _loc =
    if is_capital p.[0] then <:expr< $qualify_uid "P" m _loc$ ($str:p$) >>
    else Loc.raise _loc (Stream.Error "Propositions must be capitalized.") in
  let t = poly_var empty ~check:(fun _ -> ()) of_poly_var ~cache:false in
  expr t atom_of_expr


(* Recurse to find all the variables under the quantification symbol. *)
let rec forall_or_exists quantif vars e q _loc =
  match quantif with
  | <:expr< forall `$x$ >> ->
    let forall = q "Forall" in
    List.fold_left (fun e v -> <:expr< $forall$($str:v$, $e$) >>) e (x :: vars)
  | <:expr< exists `$x$ >> ->
    let exists = q "Exists" in
    List.fold_left (fun e v -> <:expr< $exists$($str:v$, $e$) >>) e (x :: vars)
  | <:expr< $quantif$ `$x$ >> ->
    forall_or_exists quantif (x :: vars) e q _loc
  | _ -> Loc.raise _loc (Stream.Error "\"forall\" and \"exists\" must be \
           followed by space separated variables.")

let rec fol_of_expr tr e =
  let _loc = Ast.loc_of_expr e in
  let q c = qualify_uid c (transf_module tr) _loc in
  match e with
  | <:expr< false >> -> <:expr< $q "False"$ >>
  | <:expr< true >> -> <:expr< $q "True"$ >>
  | <:expr< not $e$ >> -> <:expr< $q "Not"$ ($fol_of_expr tr e$) >>
  | <:expr< $x$ && $y$ >> ->
    <:expr< $q "And"$ ($fol_of_expr tr x$, $fol_of_expr tr y$) >>
  | <:expr< $x$ || $y$ >> ->
    <:expr< $q "Or"$  ($fol_of_expr tr x$, $fol_of_expr tr y$) >>
  | <:expr< $x$ => $y$ >> ->
    <:expr< $q "Imp"$ ($fol_of_expr tr x$, $fol_of_expr tr y$) >>
  | <:expr< $x$ <=> $y$ >> ->
    <:expr< $q "Iff"$ ($fol_of_expr tr x$, $fol_of_expr tr y$) >>

  | <:expr< $quantif$ // $e$ >> ->      (* forall | exists *)
    forall_or_exists quantif [] (fol_of_expr tr e) q _loc
  | _ ->
      let a = apply overloading_atom (transf_module tr) e in
      <:expr< $q "Atom"$ ($a$) >>
;;

let overloading_fol =
  expr empty fol_of_expr

let () =
  let qualify = if Pa_do.Macro.is_defined "INSIDE_FOL" then ""
                else "Fol" in
  associate overloading_term "Term" ~qualify;
  associate overloading_fol "Atom" ~qualify;
  associate overloading_fol "Fol"


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
