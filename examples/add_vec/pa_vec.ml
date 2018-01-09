(* This extension demonstrates how to write a simple optimization. *)

open Camlp4.PreCast
open Pa_infix
open Pa_do.Delimited_overloading

let t =
  int empty (fun i m _loc -> <:expr< $`flo:float_of_int i$ >>) ~cache:false

(* In case the optimisations do not apply, use these. *)
let t = lid_subst t [("+", "add"); ("-", "sub"); ("~-", "neg");
                     ("to_string", "to_string");
                    ]

(* Collect all the terms of a sum in a list (preserve the order). *)
let rec collect_add_terms tr e = match e with
  | <:expr< ( + ) $a$ $b$ >> ->
    collect_add_terms tr a @ collect_add_terms tr b
  | _ -> [(self tr)#expr e] (* term at a leaf of the addition tree,
                               apply the overloadings recursively to it. *)

(* Optimization to use a single temporary array for a sum of
   vectors. *)
let t =
  let optim tr e =
    match e with
    | <:expr@_loc< ( + ) $_$ $_$ >> ->
      (* "First" addition.  Collect all terms and write the expression. *)
      (match collect_add_terms tr e with
       | a :: tl ->
           let acc = new_lid() in
           let add_to = transf_qualify tr "add_to" _loc in
           let add_term e x = (<:expr< $e$; $add_to$ $lid:acc$ $x$ >>) in
           let sum = List.fold_left add_term (<:expr< >>) tl in
           <:expr< (let $lid:acc$ = $transf_qualify tr "copy" _loc$ $a$ in
                    $sum$;
                    $lid:acc$ ) >>
       | [] -> assert false)
    | _ -> super tr e in
  expr t optim

(* Add the new "<-+" assignment with optimization.  These
   optimisations must be executed before the ones for "isolated"
   additions, thus must be declared after. *)
let t =
  infix "<-+" Level.assignment;
  let optim tr e =
    match e with
    | <:expr@_loc< $lid:"<-+"$ $lid:y$ $sum$ >> ->
      let terms = collect_add_terms tr sum in
      let add_to = transf_qualify tr "add_to" _loc in
      let add_term e x = (<:expr< $e$; $add_to$ $lid:y$ $x$ >>) in
      List.fold_left add_term (<:expr< >>) terms
    | _ -> super tr e in
  expr t optim



let () = associate t "Vec"



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
