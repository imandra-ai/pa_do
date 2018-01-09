type 'a t = float
external meters : float -> [`Meters] t = "%identity"
external feet : float -> [`Feet] t = "%identity"

let neg = ( ~-. )
let add = ( +. )
let sub = ( -. )
let mul = ( *. )
let div = ( /. )
external to_float : 'a t -> float = "%identity"
