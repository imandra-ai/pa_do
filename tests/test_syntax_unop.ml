(* Test concrete syntax *)
(************************)

PREFIX ( *** ) LEVEL ( ~~ );;

let z = *** 2 * 3
(* RESULT: let z = ( *** ) (2 * 3) *)

PREFIX ( /+/ ) LEVEL ( ~~ );;

let z = /+/ 2 * 3
(* RESULT: let z = (( /+/ ) 2) * 3) *)

POSTFIX ( /// ) LEVEL ( !! );;

let z = 2 * 3 ///
(* RESULT: let z =  2 * 3 *)
  (* <:expr<2 * 3 /// >> *)

POSTFIX ( // ) LEVEL ( ~~ );;

let ( // ) x = x + 1
let z = 2 * 3 //
  (* RESULT: let z =  2 * (( // ) 3) *)

  (* <:expr<2 * 3 //>> *)

POSTFIX ( rem ) LEVEL ( ! );;

let ( rem ) x = x + 1
let z = 1 rem * 2

