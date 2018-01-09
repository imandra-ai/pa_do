
(** Complex.OVERLOAD(( + ) -> add; ( - ) -> sub; norm -> norm)
Complex.OVERLOAD(( * )-> mul) **)

let z1 = Float.( ( + ) )
let z2 = Int32.( ( * ) )
let z3 = Float.( 1 + 4 - x / 1)
let z4 = Int32.( 1 + 4 - x / 1 + a.(2))

let z5 = Big_int.( 3 * x * x + 5 * x + 7 = 0 );;

let z6 = [| |]
let z7 = [| 1; 2 |]
