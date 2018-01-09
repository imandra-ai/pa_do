(* Feature request #46.  Simple example inspired by the post
   http://camltastic.blogspot.com/2008/05/phantom-types.html

   Requires the module [Length] and the preprocessor pa_length
*)
open Printf

let () =
  Length.(
    let m1 = 10. in
    let m2 = 20. in
    printf "10m + 20m = %g\n" (to_float (m1 + m2));
    let f1 = (40.: feet) in
    let f2 = feet 50. in
    printf "40ft + 50ft = %g\n" (to_float (f1 + f2));
    (* Uncomment the following line to see how the type system
       prevents to add meters to feet: *)
    (* printf "10m + 50ft = %g\n" (to_float (m1 + f2)) *)
  )



(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
