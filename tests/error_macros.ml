(* Test error locations (as this is a UI issue, this has to be done by
   hand at the moment unfortunately). *)

DEFINE F(M) = Float.(1 + M.(2))

(* let a = F(Int) *)
(* let b = Int32.(1 + F(Int64)) *)

DEFINE F(M,N) = 3 + M.(1 + N.(2))

let c = F(Float, Int32)


(* Local Variables: *)
(* compile-command: "camlp4o ../src/pa_do.cmo error_macros.ml" *)
(* compile-command: "ocamlc -pp 'camlp4o ../src/pa_do.cmo' -c error_macros.ml" *)
(* End: *)
