(* Computes the closed Newton-Cotes integration formulas coefficients
   w_i using the following definition:

   w_i = \int_0^n phi_i(t) dt

   phi_i(t) = \prod_{k=0,...,n; k \ne i} (t - k)/(i - k)

   where n + 1 is the number of equidistant points.
*)

open Format

(** [mult coeffs a1 a0] multiply the polynomial whose coefficients are
    stored in the list [coeffs] (as [[coeff(x**d);...; coeff(x**0)]])
    by the polynomial [a1] t - [a0].  It returns the coeffs of the
    product. *)
let mult coeffs a1 a0 =
  Num.(let rec mlt hc = function
         | [] -> [ - hc * a0 ]
         | c :: tc -> (c * a1 - hc * a0) :: (mlt c tc) in
       mlt 0 coeffs )

(** [phi n i] return the coefficients of the polynomial
    {% $t \mapsto \prod_{k=0,...,n; k \ne i} (t - k)/(i - k)$ %} *)
let phi (n:int) (i:int) =
  if i < 0 || i > n then invalid_arg "phi: 0 <= i <= n required";
  let rec prod coeffs k =
    if k > n then coeffs
    else if k = i then prod coeffs (k+1) (* skip index [i] *)
    else Num.(
      let i_k = 1 / (of_int i - of_int k) in
      prod (mult coeffs i_k (of_int k * i_k)) Int.(k + 1)
    ) in
  prod [Num.(1)] 0


(* Nodal polynomial: pi n == \prod_{k=0}^n (t - k) *)
let pi (n:int) =
  let rec prod coeffs k =
    if k <= n then prod (Num.(mult coeffs 1 (of_int k))) (k + 1)
    else coeffs in
  prod [Num.(1)] 0


(** [integ_poly coeffs a b] integrate on the interval [a], [b] the
    polynomial whose coefficients are stored in the list [coeff].  If
    provided, [deg] is supposed to be the degree of the polynomial,
    i.e., [List.length coeffs - 1].  This option is given to avoid
    computing the length if it is available by other means.  *)
let integ_poly ?deg coeffs a b =
  (* Use a Horner scheme to eval the primitive of the poly at a and b. *)
  Num.(
    let rec integ eva evb d = function
      | [] -> evb * b - eva * a
      | c :: tc ->
          let pcoeff = c / (of_int d) in
          integ (eva * a + pcoeff) (evb * b + pcoeff) Int.(d - 1) tc  in
    match deg with
    | None -> integ 0 0 (List.length coeffs) coeffs
    | Some d -> integ 0 0 Int.(d + 1) coeffs
  )

(* Newton-Cotes weights *)
let w_closed n i =
  Num.(integ_poly ~deg:n (phi n i) 0 (of_int n))

(* Newton-Cotes constants in error estimations -- see "Méthodes
   Numériques pour le calcul scientifique" by Quarteroni and al. *)
let m_closed n =
  Num.(let p = mult (pi n) 1 0 in
       integ_poly ~deg:Int.(n+2) p 0 (of_int n))
and k_closed n =
  Num.(integ_poly ~deg:Int.(n+1) (pi n) 0 (of_int n))


(* Display some Newton-Cotes weights *)
let () =
  let nmax = 10
  and colmax = 5 in
  open_tbox();
  (* Print header and set tabs *)
  printf "*** Closed Newtow-Cotes formulas ***@\n";
  printf "n  "; set_tab(); printf "w0";
  for i = 1 to min colmax nmax do
    printf "           ";  set_tab();  printf "w%i" i;
  done;
  printf "@\n";
  for n = 1 to nmax do
    printf "%i" n;
    for i = 0 to min colmax n do
      print_tab();  printf "%s" Num.(to_string (w_closed n i));
    done;
    printf "@\n"
  done;
  close_tbox()

(* Errors estimates -- table 8.3 p. 290 in "Méthodes Numériques pour
   le calcul scientifique" by Quarteroni and al. *)
let () =
  let nmax = 8
  and is_even n = (n mod 2 = 0)
  and fact n =
    Num.(let rec f acc n =
           if n = 0 then acc
           else f (acc * n) (n - 1) in
         f 1 n) in
  open_tbox();
  printf "*** Errors ***@\n";
  printf "n  "; set_tab();
  printf "closed (\\mathcal M_n)    "; set_tab();
  printf "@\n";
  for n = 1 to nmax do
    printf "%i" n; print_tab();
    let cst_closed =
      Num.(if is_even n then m_closed n / fact(of_int n + 2)
           else k_closed n / fact(of_int n + 1))
    and order = if is_even n then n + 1 else n in
    printf "%s h^%i f^(%i)" Num.(to_string cst_closed) (order + 2) (order + 1);
    print_tab(); printf "[%s]" (Num.approx_num_fix 8 cst_closed);
    printf "@\n"
  done;
  close_tbox()


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
