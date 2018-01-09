(* Test the concrete syntax *)

open Printf
open Camlp4.PreCast
open Test

let simple =
  group "Simple concrete syntax tests"
    [concrete_syntax "/+/"
       "INFIX ( /+/ ) LEVEL (+)"
       (`Compare_expr ["1 /+/ 2 + 3 /+/ 4",
                <:expr@here< ((1 /+/ 2) + 3) /+/ 4 >>;
               ]);
     concrete_syntax "/-/"
       "INFIX ( /-/ ) RIGHTA HIGHER (+)"
       (`Compare_expr ["1 /-/ 2 + 3 /-/ 4",
                <:expr@here< (1 /-/ 2) + (3 /-/ 4) >>;
               ]);
     concrete_syntax "/*/"
       "INFIX ( /*/ ) LEVEL ( * )"
       (`Compare_expr ["1 /*/ 2 + 3 /*/ 4",
                <:expr@here< (1 /*/ 2) + (3 /*/ 4) >>;
               ]);
  (* concrete_syntax "prefix & (bug 43)"
       "PREFIX ( & );;"
       (`Compare_expr ["( &4 )", <:expr@here< ( & ) 4 >>;
                       "(&4)", <:expr@here< ( & ) 4 >>;
       ]); *)
    ]

let alpha =
  group "Alphabetic operators."
    [concrete_syntax "o"
        "INFIX ( o ) LEVEL ( * );;"
        (`Compare_expr ["1 o 2 + 3 o 4",
                       <:expr@here< (o 1 2) + (o 3 4) >>;
        ]);
     concrete_syntax "plus"
       "INFIX plus LEVEL ( + );;"
       (`Compare_expr ["1 plus 2 * 3 plus 4",
                      <:expr@here< plus (plus 1 (2 * 3)) 4 >>;
       ]);
     concrete_syntax "subset"
       "INFIX subset LEVEL ( && );;"
       (`Compare_expr ["1 subset 2 * 3 subset 4",
                      <:expr@here< subset 1 (subset (2 * 3) 4) >>;
       ]);
     concrete_syntax "incr"
       "PREFIX incr LEVEL ( ! );;"
       (`Compare_expr ["print_int incr 1",
                       <:expr@here< print_int (incr 1) >>;
                      ]);
     concrete_syntax "mul"
       "INFIX mul LEVEL ( * );;
        let ( mul ) = ( * )"
       (`Compare_val [[], Compare_val.int(),
                     "1 mul 2 + 3 mul 4", <:expr@here< 14 >> ;
       ])
    ]

let arg_order =
  group "Order of the arguments"
    [concrete_syntax "x /- y -> /- x y"
        "INFIX ( /- ) LEVEL ( + );;
         let ( /- ) = ( - )"
       (`Compare_val [[Var.int "x"; Var.int "y"],
                     Compare_val.int(),
                     "x /- y", <:expr@here< x - y >>;
       ]);
    ]

let not_operator = (None, Some true, ".*valid.*name.*")
let forbidden_op = (None, Some true, ".*not.*allowed.*")
let bad_arity = (None, Some true, ".*unary.*level.*")
let conflicting_kind = (None, Some true, ".*Conflicting.*kind.*")
let conflicting_level = (None, Some true, ".*Conflicting.*level.*")

let errors =
  group "Error messages tests"
    [concrete_syntax "Not operator"
       "INFIX ( .. ) LEVEL ( + );;" (`Camlp4_error not_operator);

     concrete_syntax "Forbidden operator"
       "INFIX ( := ) LEVEL ( + );;" (`Camlp4_error forbidden_op);

     concrete_syntax "Bad arity"
       "INFIX ( /*/ ) LEVEL ( ! );;" (`Camlp4_error bad_arity);

     concrete_syntax "Conflicting kind"
       "INFIX ( /- ) LEVEL ( + );;
        PREFIX ( /- );;" (`Camlp4_error conflicting_kind);

     concrete_syntax "Conflicting level"
       "POSTFIX ( ++ );;
        POSTFIX ( ++ ) LEVEL ( + );;" (`Camlp4_error conflicting_level);
    ]



let () =
  run (group "Concrete syntax tests" ~libs:["pa_infix.cmo"]
          [simple; alpha; arg_order; errors])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
