open Format

let zero = Term.(0)

let a = Atom.(`x + `y < `z)


let f = Fol.(`x**4 + `x**2 + 1 = 0 && 0 = 1 => 2 * `x - 3 = 0)

let () =
  let p = Term.(`x + `x * `y + `y**2 + 2) in
  printf "p = "; Fol.print_term p;
  printf "@\nD(x) p = ";
  Fol.print_term (Fol.poly_diffn Term.(`x) 1 p);
  printf "@\n"


let f = Fol.(exists `x `y // `x**4 + `y**2 + 1 = 0)


let gold = Fol.(`P && `Q <=> ((`P <=> `Q) <=> `P || `Q))



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
