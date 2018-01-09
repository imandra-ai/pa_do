INFIX ( %+ ) RIGHTA HIGHER (+)
INFIX ( %++ ) LOWER (+) RIGHTA
INFIX ( %- ) LOWER (+);;
INFIX ( %* ) LEVEL (+);;

let z = 1 + 2 %+ 3 %+ 4 + 5
let z = 1 %+ 2 %+ 3 %+ 4
let z = 1 %+ 2 %++ 3 %+ 4
let z = 1 + 2 %- 3 %- 4 + 5
let z = 1 + 2 %* 3 %* 4 + 5
let z = 1 rem 2 + 3

let ( rem ) x y = x / y
