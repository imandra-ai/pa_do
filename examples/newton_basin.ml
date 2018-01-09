open Printf
open Graphics
(* Newton method for z^3 - 1 = 0 is

   z -> (2z + 1/z^2)/3

   This map is a contraction on a ball of radius r around each root,
   for any r small enough to satisfy

   (1+r)^3 <= (1-r)^3 + 1
*)
let r = 0.165

(* This program draws the basins of attraction of the roots, i.e., all
   points converging to a given root are put in the same color. *)
let pi2 = 8. *. atan 1.
let root0 = Complex.polar 1. 0.
let root1 = Complex.polar 1. (pi2 /. 3.)
let root2 = Complex.polar 1. (2. *. pi2 /. 3.)
let color0 = red
let color1 = blue
let color2 = green

let root_color niter z =
  Complex.(
    let z = ref z in
    for i = Int.(1) to niter do
      z := (2 * !z + 1 / !z**2) / 3
    done;
    if abs(!z - root0) <= r then Some color0
    else if abs(!z - root1) <= r then Some color1
    else if abs(!z - root2) <= r then Some color2
    else None
  )

let convergence_graph ?(niters=50) ?(xpixels=700) xmin xmax
    ?(ypixels=700) ymin ymax =
  let img = Array.make_matrix xpixels ypixels transp in
  let dx = (xmax -. xmin) /. float(xpixels - 1)
  and dy = (ymax -. ymin) /. float(ypixels - 1) in
  for j = 0 to ypixels - 1 do
    for k = 0 to xpixels - 1 do
      let z = Complex.(re(xmin +. dx *. float k) + im(ymin +. dy *. float j)) in
      match root_color niters z with
      | None -> ()
      | Some c -> img.(j).(k) <- c
    done;
    printf " %.0f%%\r%!" (100. *. float j /. float(ypixels - 1));
  done;
  img

let () =
  let i = convergence_graph (-2.) 2. (-2.) 2. in
  open_graph  " 600x600-50+20";
  draw_image(make_image i) 0 0;
  let rec wait () =
    let s = wait_next_event [Key_pressed] in
    if s.key <> 'q' then wait() in
  (* wait(); *)
  close_graph()


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
