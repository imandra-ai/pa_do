(* Pi digits computed with the sreaming algorithm given on pages 4, 6
   & 7 of "Unbounded Spigot Algorithms for the Digits of Pi", Jeremy
   Gibbons, August 2004.
   http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/spigot.pdf
*)

open Printf
open Num

(* Linear Fractional Transformation *)
module LFT =
struct
  let floor_ev (q,r,s,t) x = Num.( quo_num (q * x + r) (s * x + t) )

  let unit = Num.(1, 0, 0, 1)

  let comp (q,r,s,t) (q',r',s',t') =
    Num.(q * q' + r * s', q * r' + r * t',
         s * q' + t * s', s * r' + t * t')
end

let next z = Num.(LFT.floor_ev z 3)
and safe z n = Num.(n = LFT.floor_ev z 4)
and prod z n = Num.(LFT.comp (10, (-10) * n, 0, 1) z)
and cons z k =
  let den = 2*k+1 in
  Num.(LFT.comp z (of_int k, of_int(Int.(2*den)), 0, of_int den))

let rec digit k z n row col =
  if n > 0 then
    let y = next z in
    if safe z y then
      if col = 10 then (
        let row = row + 10 in
        printf "\t:%i\n%s" row (string_of_num y);
        digit k (prod z y) (n-1) row 1
      )
      else (
        print_string(string_of_num y);
        digit k (prod z y) (n-1) row (col+1)
      )
    else digit (k+1) (cons z k) n row col
  else
    printf "%*s\t:%i\n" (10 - col) "" (row + col)

(** [digits n] outputs the first [n] digits of pi. *)
let digits n = digit 1 LFT.unit n 0 0

let () =
  if Array.length Sys.argv <> 2 then
    printf "Usage: %s <number of digits>\n" Sys.argv.(0)
  else
    digits(int_of_string Sys.argv.(1))


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
