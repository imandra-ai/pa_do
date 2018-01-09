
let x = Vec.( [|1; 2; 3|] )

let () =
  Vec.(
    let y = [| 3; 4; 5|] in
    let u = x + x + y in
    y  <-+ x + u;
  )



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
