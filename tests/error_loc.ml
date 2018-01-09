(* Test error locations (as this is a UI issue, this has to be done by
   hand at the moment unfortunately). *)


let x = Float.(1 + Int.(1))


(* Local Variables: *)
(* compile-command: "ocamlc -pp 'camlp4o ../src/pa_do.cmo' -c error_loc.ml" *)
(* End: *)
