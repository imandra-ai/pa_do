(* Set the infix |> at the right level so that [a = b |> c] reads [a =
   (b |> c)] and optimize (inline [let ( |> ) x f = f x]).
*)

open Camlp4.PreCast
open Pa_infix
module L = Level

let () =
  let l = L.binary (L.Higher L.comparison) ~assoc:L.LeftA in
  let expr x y _loc = <:expr< $y$ $x$ >> in
  infix "|>" ~expr l



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
