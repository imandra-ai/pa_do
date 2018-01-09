(* Same as newton_basin.ml but without any overloading. *)
open Printf
open Graphics

let r = 0.165

let pi2 = 8. *. atan 1.
let root0 = Complex.polar 1. 0.
let root1 = Complex.polar 1. (pi2 /. 3.)
let root2 = Complex.polar 1. (2. *. pi2 /. 3.)
let color0 = red
let color1 = blue
let color2 = green

let ( +: ) = Complex.add
let ( -: ) = Complex.sub
let ( *: ) = Complex.mul
let ( *.: ) r z = { Complex.re = r *. z.Complex.re; im = r *. z.Complex.im }
let ( /:. ) z r = { Complex.re = z.Complex.re /. r; im = z.Complex.im /. r }

let root_color niter z =
  let z = ref z in
  for i = 1 to niter do
    z := (2. *.: !z +: Complex.inv(!z *: !z)) /:. 3.
  done;
  if Complex.norm(!z -: root0) <= r then Some color0
  else if Complex.norm(!z -: root1) <= r then Some color1
  else if Complex.norm(!z -: root2) <= r then Some color2
  else None


let convergence_graph ?(niters=50) ?(xpixels=700) xmin xmax
    ?(ypixels=700) ymin ymax =
  let img = Array.make_matrix xpixels ypixels transp in
  let dx = (xmax -. xmin) /. float(xpixels - 1)
  and dy = (ymax -. ymin) /. float(ypixels - 1) in
  for j = 0 to ypixels - 1 do
    for i = 0 to xpixels - 1 do
      let z = { Complex.re = xmin +. dx *. float i;
                Complex.im = ymin +. dy *. float j } in
      match root_color niters z with
      | None -> ()
      | Some c -> img.(j).(i) <- c
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
