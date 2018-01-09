(* do-notation for the monadic style.

   TODO: perform optimization based on the monad laws, see e.g.
   http://www.haskell.org/pipermail/haskell-cafe/2006-September/017927.html

   Optimizations from:
   http://www.univ-orleans.fr/lifo/Members/David.Teller/publications/ml2008.pdf
*)

open Camlp4.PreCast
open Printf

let do_notation m e =
  e

open Syntax;;

EXTEND Gram
  GLOBAL: expr str_item label_longident
  infixop0 infixop1 infixop2 infixop3 infixop4;

expr: BEFORE "simple"
  [ [ m = UIDENT; "."; "do"; "("; e = SELF; ")" ->
      printf "DEBUG: %s.do(): " m;
      Printers.OCaml.print_implem <:str_item< $exp:e$ >>;
      do_notation m e
    ] ];
END



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
