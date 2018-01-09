(* Show the convenience of Delimited Overloading to write and optimize
   a simple ray tracer written by Jon Harrop.
   http://www.ffconsultancy.com/languages/ray_tracer/comparison.html
*)

let delta = sqrt epsilon_float

(* 3D vector space.  Delimited overloading will allow the usual
   operators to be used and inline them. *)
module Vec =
struct
  type t = {x:float; y:float; z:float}
  let zero = {x=0.; y=0.; z=0.}
  let add a b = {x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z}
  let sub a b = {x = a.x -. b.x; y = a.y -. b.y; z = a.z -. b.z}
  let mul s r = {x = s *. r.x; y = s *. r.y; z = s *. r.z}
  let dot a b = a.x *. b.x +. a.y *. b.y +. a.z *. b.z
  let norm r = sqrt(dot r r)         (* naive but good enough for here *)
  let unitize r = mul( 1. /. norm r) r
end

let ray_sphere orig dir center radius =
  let v = Vec.(center - orig) in
  let b = Vec.(dot v dir) in
  (* [Float.(x**2)] are optimized to [x *. x]. *)
  let d2 = Float.(b**2 - Vec.(dot v v) + radius**2) in
  if d2 < 0. then infinity else
    let d = sqrt d2 in
    let t1 = b -. d and t2 = b +. d in
    if t2 > 0. then if t1 > 0. then t1 else t2 else infinity

let rec intersect orig dir (l, _ as hit) (center, radius, scene) =
  match ray_sphere orig dir center radius, scene with
  | l', _ when l' >= l -> hit
  | l', [] -> l', Vec.(unitize (orig + l' * dir - center))
  | _, scenes -> intersects orig dir hit scenes
and intersects orig dir hit = function
  | [] -> hit
  | scene::scenes -> intersects orig dir (intersect orig dir hit scene) scenes

let light = Vec.(unitize [|1.; 3.; -2.|])
and ss = 4

let rec ray_trace dir scene =
  let l, n = intersect Vec.zero dir (infinity, Vec.zero) scene in
  let g = Vec.(dot n light) in
  if g <= 0. then 0. else
    let p = Vec.(l * dir + delta * n) in
    if fst (intersect p light (infinity, Vec.zero) scene) < infinity then 0. else g

let rec create level c r =
  let obj = c, r, [] in
  if level = 1 then obj else
    let a = 3. *. r /. sqrt 12. in
    let aux x' z' = create (level - 1) (Vec.(c + [|x'; a; z'|])) (0.5 *. r) in
    c, 3. *. r, [obj; aux (-.a) (-.a); aux a (-.a); aux (-.a) a; aux a a]

let level, n =
  try int_of_string Sys.argv.(1), int_of_string Sys.argv.(2) with _ -> 6, 512

let scene = create level (Vec.([|0.; -1.; 4.|])) 1.;;

Printf.printf "P5\n%d %d\n255\n" n n;;
for y = n - 1 downto 0 do
  for x = 0 to n - 1 do
    let g = ref 0. in
    for dx = 0 to ss - 1 do
      for dy = 0 to ss - 1 do
	let aux x d = float x -. float n /. 2. +. float d /. float ss in
	let dir = Vec.(unitize [|aux x dx; aux y dy; float n|]) in
	g := !g +. ray_trace dir scene
      done;
    done;
    let g = 0.5 +. 255. *. !g /. float (ss*ss) in
    Printf.printf "%c" (char_of_int (int_of_float g))
  done;
done


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
