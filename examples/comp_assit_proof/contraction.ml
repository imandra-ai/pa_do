open Printf

OVERLOAD_ARITHMETIC Interval;;
OVERLOAD_INT Interval(of_int);; (* number [i] lifted to interval [i..i] *)
(* Do not overload float literals which may suffer from truncation. *)
OVERLOAD Interval(( ** ) -> pow;  ( < ) -> lt; ( > ) -> gt;
                  of_int; of_float; make; lower; to_string; inv; abs);;
OVERLOAD_RECORD_FIELD Interval(lo; up);;

let radius_of_ball_in_function_space = ldexp 1. (-18)

let contraction_factor = Interval.(172 / 100)

(***********************************************************************
 * Bounds involving polynomials
 ***********************************************************************)

module Poly =
struct
  type t = Interval.t list
      (* Coefficients of a polynomial [[c0; c1; ...; cdeg]].  The
         empty list [[]] represents the null polynomial. *)

  (* Polynomial of degree 0. *)
  let of_float x = [Interval.of_float x]

  let to_string ?(var="z") p =
    let b = Buffer.create 100 in
    let rec coeff i = function
      | [] -> if i = 0 then "0." else Buffer.contents b
      | c :: tl ->
          if i > 0 then Buffer.add_string b " + ";
          Buffer.add_string b (Interval.to_string c);
          Buffer.add_string b var;  Buffer.add_string b "^";
          Buffer.add_string b (string_of_int i);
          coeff (i+1) tl in
    coeff 0 p

  let neg p = List.map (fun c -> Interval.(- c)) p

  let interval_mul r p = List.map (fun c -> Interval.(r * c)) p

  let rec add p1 p2 = match p1, p2 with
    | _, [] -> p1
    | [], _ -> p2
    | h1 :: tl1, h2 :: tl2 -> Interval.(h1 + h2) :: add tl1 tl2

  let sub p1 p2 = add p1 (neg p2)    (* FIXME: direct impl. more efficient *)

  let rec mul p1 p2 = match p1 with
    | [] -> []
    | c :: tl -> add (interval_mul c p2) (Interval.(0) :: mul tl p2)

  let div p c = interval_mul (Interval.inv c) p

  (* Almost [List.nth] except that we must return 0 if the coefficient
     is not present. *)
  let rec coeff p i = match p with
    | [] -> Interval.(0)
    | c :: tl -> if i = 0 then c else coeff tl (i - 1)

  let rec der_term i = function
    | [] -> []
    | c (* x^[i] *) :: tl -> Interval.(of_int i * c) :: der_term (i+1) tl

  let der = function
    | [] | [ _ ] -> []
    | _ :: tl -> der_term 1 tl

  (* Radius of the common domain of analyticity for functions in B. *)
  let domain = Interval.(17 / 10)

  (* The "improvement" of p. 26 of the paper is natural here, so we
     perform it. *)
  let norm p = Interval.(List.fold_right (fun c n -> abs c + domain * n) p 0)

  (* [collapse p] return a polynomial collapsing the intervals of the
     coefficients of [p] to a single point. *)
  let rec collapse p =
    List.map (fun c -> Interval.(of_float(0.5 *. (c.lo +. c.up)))) p

  (* [split p n] splits the polynomial [p] into [q, r] according to
     the equation [p(x) = q(x) + x^(N+1) * r(x)] -- the paper says [r]
     is discarded but this is not always true so we return it too. *)
  let rec split p n =
    if n < 0 then ([], p)
    else match p with
    | [] -> ([], [])
    | c :: tl -> let q, r = split tl (n - 1) in (c :: q, r)
end;;
OVERLOAD_ARITHMETIC Poly;;
OVERLOAD Poly(( *. ) -> interval_mul;  coeff; der; norm; split);;

let degree = 9

let approximate p = Poly.collapse(fst(Poly.split p degree))

(* Approximates the composition of [p] and [r].  (It is important that
   the approximation is done -- through [approximate] -- at each
   recursive call not to fall outside the safe range of numbers.) *)
let rec approximate_comp p r = match p with
  | [] -> []
  | c :: tl -> approximate Poly.([c] + approximate_comp tl r * r)

let approximate_j g j f =
  approximate Poly.( j + approximate_comp f g / (coeff g 1) )

let q g = approximate_comp g g

(* [comp_pow f n] is the funtion [f] composed [n] times with itself. *)
let rec comp_pow f n x =
  if n = 0 then x else comp_pow f (n-1) (f x)

(***********************************************************************
 * Bounds involving analytic functions
 ***********************************************************************)

module StdB =
struct
  type t = { p: Poly.t;  g: Interval.t;  h: Interval.t }
      (** Represent the a set of functions of {%$\mathcal{B}$%}.
          See paper p. 29.  (We keep here the lower bounds on the norms.) *)

  (* Polynomial 0 with no errors. *)
  let zero_error = { p = [];  g = Interval.(0);  h = Interval.(0) }

  let of_poly p0 = { zero_error with p = p0 }

  let dn1 = Interval.pow Poly.domain (degree + 1)
  let norm f = Interval.(lower(Poly.norm f.p + Poly.domain * f.g + dn1 * f.h))

  (* [coeff0 f] returns a bound for the value [f](0), the evaluation of the
     "function" [f] at 0 -- independent of the function in the class. *)
  let coeff0 f = Poly.coeff f.p 0

  (* Bound on [f]'(0) which belongs to p'(0) + [-norm g, norm g] *)
  let coeff1 f = Interval.(Poly.coeff f.p Int.(1) + make (-. f.g.up) f.g.up)

  let interval_mul i f = Interval.(
    let abs_i = abs i in
    { p = Poly.(i *. f.p); g = abs_i * f.g;   h = abs_i * f.h }
  )

  let neg f = { f with p = Poly.(- f.p) }

  let add f1 f2 = Interval.(
    { p = Poly.(f1.p + f2.p);  g = lower(f1.g + f2.g);  h = lower(f1.h + f2.h) }
  )

  let sub f1 f2 = add f1 (neg f2)

  let mul f1 f2 = Interval.(
    let p1, r = Poly.(split (f1.p * f2.p) degree) in
    let norm_fp1 = Poly.norm f1.p in
    let norm_fp2 = Poly.norm f2.p in
    let g = norm_fp1 * f2.g + f1.g * norm_fp2 + Poly.domain * (f1.g * f2.g)
    and h = Poly.norm r + dn1 * f1.h * f2.h + norm_fp1 * f2.h + f1.h * norm_fp2
      + Poly.domain * (f1.g * f2.h + f1.h * f2.g) in
    { p = p1;  g = lower g;  h = lower h }
  )

  let div f c = interval_mul (Interval.inv c) f

  let rec poly_comp p f = match p with
    | [] -> zero_error
    | h :: tl -> add (of_poly [h]) (mul f (poly_comp tl f))

  let n1 = degree + 1

  let rec comp f1 f2 =
    let norm_f2 = norm f2 in
    if not(norm_f2 < Poly.domain) then invalid_arg "StdB.comp";
    let r = Interval.(norm_f2 * f1.g + norm_f2**n1 * f1.h) in
    let c = of_poly Interval.([make (-. r.up) r.up]) in
    let g = { zero_error with g = Interval.(lower r / Poly.domain) } in
    add (add (poly_comp f1.p f2) c) g

  let sup j mu = Interval.(
    let mu1 = 1 - mu in
    lower(of_int j * mu**Int.(j-1) / mu1 + mu**j / mu1**Int.(2))
  )

  let dn = Interval.pow Poly.domain degree

  (* f1' o f2 *)
  let der_comp f1 f2 =
    let mu = Interval.(norm f2 / Poly.domain) in
    if not Interval.(mu < 1) then invalid_arg "StdB.der_comp";
    let r = Interval.(sup Int.(1) mu * f1.g + sup n1 mu * dn * f1.h) in
    let c = of_poly Interval.([make (-. r.up) r.up]) in
    let g = { zero_error with g = Interval.(lower r / Poly.domain) } in
    add (add (poly_comp (Poly.der f1.p) f2) c) g

end;;
OVERLOAD_ARITHMETIC StdB;;
OVERLOAD StdB(( *. ) -> interval_mul;
              of_poly; norm; coeff0; coeff1; comp; der_comp);;
OVERLOAD_RECORD_FIELD StdB(p; g; h);;


(***********************************************************************
 * The Schröder equation
 ***********************************************************************)

let degree1 = degree + 1

(* Assume [g](0) = 0 and [j](0) = [j]'(0) = 0. *)
let norms_y g j =
  Interval.(
    let l = abs(StdB.coeff1 g) in
    let x = StdB.norm g / Poly.domain in
    let c = x * x / l in
    if not(c < 1) then invalid_arg(sprintf "norm_y: c=%s not< 1" (to_string c));
    let fg = (j.StdB.g + Poly.norm j.StdB.p / Poly.domain) / (1 - c) in
    let fh = j.StdB.h / (1 - x**degree1 / l) in
    fg, fh
  )

(* The solution [f] to the Schröder equation [f(z) - (1/l) f(g(z)) = j(z)]
   with [l] = [g]'(0).  Compute an approximate solution [f0] and then
   bound the residues. *)
let y g j = StdB.(
  let jp = approximate j.p in
  let f0 = of_poly(comp_pow (approximate_j g.p jp) 16 jp) in
  let j1 = j - (f0 - comp f0 g / coeff1 g) in
  let fg, fh = norms_y g j1 in
  { f0 with g = fg;  h = fh }
)

(* Same as [y] but for the homogeneous Schröder equation ([j] = 0). *)
let eta g = StdB.(
  let p = approximate ((comp_pow q 6) g.p) in
  let f0 = of_poly Poly.(p / coeff p 1) in
  let j1 = -(f0 - comp f0 g / coeff1 g) in
  let fg, fh = norms_y g j1 in
  { f0 with g = fg;  h = fh }
)

(***********************************************************************
 * Bounds on Maps and Operators
 ***********************************************************************)

let h g = (eta g, StdB.coeff1 g)

let c (f,l) =
  let b = Interval.(l * l) in
  let f1 = StdB.of_poly [Interval.(b - 1); b] in
  (StdB.comp f f1, l)

let s (f1, l) = (StdB.(f1 * f1), l)

let n (f2, l) =
  let f = StdB.(f2 / coeff0 f2) in
  (* [f - 1] = remove the term of order 0 from the polynomial: *)
  ( { f with StdB.p = Interval.(0) :: List.tl f.StdB.p }, l)

let k (f3, l) =
  (* [f3]'(0) is bounded by [StdB.coeff1 f3] : *)
  let p' = Interval.([0; contraction_factor * (l - StdB.coeff1 f3)]) in
  { f3 with StdB.p = Poly.(f3.StdB.p + p') }

let r g = k(n(s(c(h g))))

let der_h g k =
  let mu = StdB.coeff1 k in
  let (f,l) = h g in
  let j = StdB.((der_comp f g * k - mu *. f) / l) in
  (y g j, mu)

let der_c (f,l) (h,mu) =
  Interval.(
    let b = l * l in
    let psi = StdB.of_poly [b - 1; b] in
    let c = 2 * l * mu in
    let h1 = StdB.(comp h psi + der_comp f psi * of_poly [c; c]) in
    (h1, mu)
  )

let der_s (f1,_) (h1, mu) = (StdB.(Interval.(2) *. f1 * h1), mu)

let der_n (f2,_) (h2, mu) =
  let s = StdB.coeff0 f2 in
  let h3 = StdB.(h2 / s - coeff0 h2 *. f2 / Interval.(s * s)) in
  (h3, mu)

let der_k (_,_) h3_mu = k h3_mu (* [k] is linear *)

let der_r g dir =
  (* Chain rule *)
  let hg = h g in
  let chg = c hg in
  let schg = s chg in
  let nschg = n schg in
  der_k nschg (der_n schg (der_s chg (der_c hg (der_h g dir))))

(* Bound on the linear operator [der_r g].  Here a general linear
   operator [lin] from {%$\mathcal{B}_0$%} into itself is treated. *)

let write_useful_information j yj =
  printf "norm of L(h%-2i) is approx. = %g\n%!" j yj.Interval.up

let di = Interval.inv Poly.domain

let zeta h0 = StdB.(of_poly [Interval.(0); di] * h0)

(* In the paper the interval [0 .. yj] is returned but we drop the
   useless lower bound. *)
let rec operator_norm_pi lin j =
  Interval.(
    if Int.(j = degree1) then
      let h = { StdB.zero_error with StdB.h = lower(di ** j) } in
      let yj = StdB.norm(lin h) in
      write_useful_information j yj;
      yj.up
    else
      let hj = (comp_pow zeta j) (StdB.of_poly [1]) in
      let yj = StdB.norm(lin hj) in
      write_useful_information j yj;
      max yj.up (operator_norm_pi lin Int.(j + 1))
  )

let operator_norm lin = operator_norm_pi lin 1

(***********************************************************************
 * Proving the Theorem
 ***********************************************************************)

let approximate_r p2 =
  let rp2 = r (StdB.of_poly p2) in
  approximate rp2.StdB.p

let () =
  let guess = Interval.([0; -4/10]) in
  let p0 = (comp_pow approximate_r 13) guess in
  let g0 = StdB.of_poly p0 in
  printf "The approx. fixpoint g0(z) = %s\n" (Poly.to_string p0);
  let eps = StdB.(norm(r g0 - g0)) in
  printf "Epsilon is approx. = %s\n" (Interval.to_string eps);
  let beta = radius_of_ball_in_function_space in
  printf "Beta is approx. = %g\n" beta;
  let ball = Interval.(make 0.0 beta / Poly.domain) in
  let u = { StdB.p = p0;  g = ball;  h = Interval.(0) } in
  let rho = operator_norm(der_r u) in
  printf "Rho is approx. = %g\n" rho;
  (* Check the inequality to complete the proof: *)
  let rhs = Interval.((1 - of_float rho) * of_float beta) in
  if Interval.(eps < rhs) then
    let r = Interval.(eps / (1 - of_float rho)) in
    printf "There exist fixpoint g* in B[g0, %g]. q.e.d.\n" r.Interval.up
  else printf "proof failed! (should not happen)\n"


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
