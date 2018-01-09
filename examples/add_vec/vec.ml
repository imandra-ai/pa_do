
type t = float array

let add x y =
  Array.init (Array.length x) (fun i -> x.(i) +. y.(i))

let sub x y =
  Array.init (Array.length x) (fun i -> x.(i) -. y.(i))

let neg x =
  Array.init (Array.length x) (fun i -> -. x.(i))

let add_to y x =
  for i = 0 to Array.length y - 1 do y.(i) <- y.(i) +. x.(i) done

let copy = Array.copy
