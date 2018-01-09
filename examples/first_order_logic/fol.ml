open Format

(* Term, like [sqrt(1 - cos x)] which is coded as
   [Fn("sqrt",[Fn("-",[Fn("1",[]); Fn("cos", [Var "x"])])])]. *)
type term =
  | Var of string
  | Fn of string * term list

let rec print_term prec fm =
  match fm with
  | Var x -> print_string x
  | Fn("^",[tm1;tm2]) -> print_infix_term true prec 24 "^" tm1 tm2
  | Fn("/",[tm1;tm2]) -> print_infix_term true prec 22 " /" tm1 tm2
  | Fn("*",[tm1;tm2]) -> print_infix_term false prec 20 " *" tm1 tm2
  | Fn("-",[tm1;tm2]) -> print_infix_term true prec 18 " -" tm1 tm2
  | Fn("+",[tm1;tm2]) -> print_infix_term false prec 16 " +" tm1 tm2
  | Fn("::",[tm1;tm2]) -> print_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> print_fargs f args

and print_fargs f args =
  print_string f;
  if args <> [] then (
    printf "(@[<0>";
    print_term 0 (List.hd args); print_break 0 0;
    List.iter (fun t -> printf ",@;"; print_term 0 t) (List.tl args);
    printf "@])"
  )

and print_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then (print_string "("; open_box 0) else ();
  print_term (if isleft then newprec else newprec+1) p;
  print_string sym;
  print_break (if String.sub sym 0 1 = " " then 1 else 0) 0;
  print_term (if isleft then newprec+1 else newprec) q;
  if oldprec > newprec then (close_box(); print_string ")") else ();;

let print_term tm =
  printf "@["; print_term 0 tm; printf "@]"

(** Lift operations up to numerals. *)
let mk_numeral n = Fn(Num.string_of_num n,[]);;

(** Get the constant multiple of the "maximal" monomial (implicit lex order) *)
let rec poly_cmul k p =
  match p with
  | Fn("+",[c1; c2]) -> Term.(poly_cmul k c1 + poly_cmul k c2)
  | Fn("-",[c1; c2]) -> Term.(poly_cmul k c1 - poly_cmul k c2)
  | Fn("*",[Var x; q]) -> Term.(Var x * poly_cmul k q)
  | Fn(ns,[]) -> mk_numeral Num.(k * of_string ns)
  | _ -> Term.(0)

(** Formal derivative of a polynomial term. *)
let rec poly_diffn x n p =
  match p with
  | Fn("+",[c; Fn("*",[y; q])]) when y = x ->
      Term.(poly_cmul Num.(of_int n) c + x * poly_diffn x Int.(n+1) q)
  | _ -> poly_cmul Num.(of_int n) p


(************************************************************************)

(* First order logic formula.  The string is for example an order
   relation like "<". *)
type fol = R of string * term list

(* Propositional formula. *)
type prop = P of string

(************************************************************************)

type 'a formula =
  | False
  | True
  | Atom of 'a
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Iff of 'a formula * 'a formula
  | Forall of string * 'a formula
  | Exists of string * 'a formula;;

