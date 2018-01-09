(* Provide a convenient notation and inline all operations of the
   [Vec] submodule.
*)

open Camlp4.PreCast
open Pa_do.Delimited_overloading

(* Allow to use integers as floats *)
let t =
  int empty (fun i m _loc -> <:expr< $`flo:float_of_int i$ >>) ~cache:false

(* Allow [|x; y; z|] to represent a vector.  Check the size. *)
let t = array t begin fun v m _loc ->
  match v with
  | [x;y;z] -> <:expr< { Vec.x = $x$; Vec.y = $y$; Vec.z = $z$} >>
  | _ -> Loc.raise _loc (Stream.Error "The vector should have length 3.")
end

type bindings = (string * Ast.expr) list
    (* A list of GENRATED bindings [(v0,e0); ...; (vN,bN)] in reverse
       order: [let vN = eN in ... let v0 = e0 in expression]. *)

(* Return the updated list of bindings and a NEW variable name. *)
let add_binding (b: bindings) expr = match expr with
  | <:expr< $id:_$ >>  (* already a name *)
  | <:expr< $int:_$ >>
  | <:expr< $flo:_$ >> (* constants, no harm in duplicating them *)
  | <:expr< ! $id:_$ >> (* dereference of a name *)
    -> (b, expr)
  | _ -> let v = new_lid() in ((v, expr) :: b, <:expr@here< $lid:v$ >>)

(* The following type will be useful to express optimizations more
   clearly. *)
type t =
  | Coord of Ast.expr * Ast.expr * Ast.expr
      (* Vector of which we know the 3 components. *)
  | Unknown of Ast.expr   (* possibly a vector of another value *)

let expr_of_t _loc = function
  | Coord(x,y,z) -> <:expr< { Vec.x = $x$; Vec.y = $y$; Vec.z = $z$} >>
  | Unknown v -> v

(* Return a 4-uple [(b,x,y,z)] where [b] are the updted bindings and
   [x], [y], [z] are expressions giving the 3 components of the vector. *)
let components (b,v) = match v with
  | Coord(x,y,z) -> (b, x,y,z)
  | Unknown v ->
      let _loc = Ast.loc_of_expr v in
      let b, v = add_binding b v in
      (b, <:expr< $v$.Vec.x >>, <:expr< $v$.Vec.y >>, <:expr< $v$.Vec.z >>)

let bind_components ((b,v) as b_v) = match v with
  | Coord(x,y,z) ->
      let b, x = add_binding b x in
      let b, y = add_binding b y in
      let b, z = add_binding b z in
      (b, x,y,z)
  | Unknown _ -> components b_v

let add _loc b_v1 b_v2 =
  let b1, x1, y1, z1 = components b_v1 in
  let b2, x2, y2, z2 = components b_v2 in
  (b1 @ b2, Coord(<:expr< $x1$ +. $x2$ >>, <:expr< $y1$ +. $y2$ >>,
                  <:expr< $z1$ +. $z2$ >>))

let sub _loc b_v1 b_v2 =
  let b1, x1, y1, z1 = components b_v1 in
  let b2, x2, y2, z2 = components b_v2 in
  (b1 @ b2, Coord(<:expr< $x1$ -. $x2$ >>, <:expr< $y1$ -. $y2$ >>,
                  <:expr< $z1$ -. $z2$ >>))

let mul _loc (b1,r) b_v2 =
  let b1, r = add_binding b1 (expr_of_t _loc r) (* assumed float *) in
  let b2, x2, y2, z2 = components b_v2 in
  (b1 @ b2, Coord(<:expr< $r$ *. $x2$ >>, <:expr< $r$ *. $y2$ >>,
                  <:expr< $r$ *. $z2$ >>))

let dot _loc b_v1 b_v2 =
  let b1, x1, y1, z1 = components b_v1 in
  let b2, x2, y2, z2 = components b_v2 in
  (b1 @ b2, Unknown(<:expr< $x1$ *. $x2$ +. $y1$ *. $y2$ +. $z1$ *. $z2$ >>))

let norm _loc b_v =
  let b, x,y,z = bind_components b_v in
  (b, Unknown(<:expr< sqrt($x$ *. $x$ +. $y$ *. $y$ +. $z$ *. $z$) >>))

let unitize _loc b_v =
  let b, x,y,z = bind_components b_v in
  let b, u = add_binding b <:expr<
    1. /. (sqrt($x$ *. $x$ +. $y$ *. $y$ +. $z$ *. $z$)) >> in
  mul _loc ([], Unknown u) (b, Coord(x,y,z))


(* Return a couple [(b, v)] where [b] is a list of bindings ans [v] is
   a value of type [vec] giving the current vector. *)
let rec optimize tr expr : bindings * t =
  let _loc = Ast.loc_of_expr expr in
  (* Process [expr] with the basic overloadings first. *)
  match super tr expr with
    (* Do not recurse in already overloaded expressions *)
  | <:expr< $lid:id$ $_$ >> when id = overloaded -> ([], Unknown expr)
  | <:expr< $lid:id$ $id:_$ $_$ >> when id = suspended -> ([], Unknown expr)

  | <:expr< { Vec.x = $x$; Vec.y = $y$; Vec.z = $z$ } >> -> ([], Coord(x,y,z))
  | <:expr< $v1$ + $v2$ >> -> add _loc (optimize tr v1) (optimize tr v2)
  | <:expr< $v1$ - $v2$ >> -> sub _loc (optimize tr v1) (optimize tr v2)
  | <:expr< $r$ * $v2$ >> -> mul _loc (optimize tr r) (optimize tr v2)
  | <:expr< dot $v1$ $v2$ >> -> dot _loc (optimize tr v1) (optimize tr v2)
  | <:expr< norm $v$ >> -> norm _loc (optimize tr v)
  | <:expr< unitize $v$ >> -> unitize _loc (optimize tr v)
  | _ -> ([], Unknown(super tr expr))

let t =
  expr t begin fun tr expr ->
    let _loc = Ast.loc_of_expr expr in
    let b, e = optimize tr expr in
    let e0 = expr_of_t _loc e in
    List.fold_left (fun e (v,x) -> <:expr< let $lid:v$ = $x$ in $e$ >>) e0 b
  end

(* You can override some optimizing functions above, by UNcommenting
   the following overloadings: *)
let t = lid_subst t [
(*   ("+", "add"); *)
(*   ("-", "sub"); *)
(*   ("*", "mul"); *)
(*   ("dot", "dot"); *)
(*   ("norm", "norm"); *)
(*   ("unitize", "unitize") *)
]


let () = associate t "Vec"


(* Local Variables: *)
(* compile-command: "omake --no--progress pa_ray.cmo" *)
(* End: *)
