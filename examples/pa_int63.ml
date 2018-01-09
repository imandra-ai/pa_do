(* This syntax extension, based on Delimited Overloading, defines the
   [int63] type and Overloadings for the [Int63] module name (with no
   implementation) which will be translated into the appropriate OCaml
   operations.

   Note that the only thing that interests us about [int63] is that it
   contains enough space for 63 bits integers -- the result of the
   operations are undefined if they overflow the [int63] range.

   This example comes from the virt-df program where it enables to
   write code that uses the standard [int] operations on 64 bits
   platforms and [int64] on 32 bits platforms.
*)

open Camlp4.PreCast
open Pa_do
open Delimited_overloading

let overload_int63, typ =
  if Sys.word_size = 64 then
    (Std.Int.overloading, fun _loc -> <:ctyp< int >>)
  else (* 32 bits *)
    (* Use 64 bits but with the min/max of 63. *)
    let t = lid Std.Int64.overloading "max_int" ~cache:false
      (fun _ _loc -> <:expr< 0x3FFFFFFF_FFFFFFFFL >>) in
    let t = lid t "min_int" ~cache:false
      (fun _ _loc -> <:expr< -0x40000000_00000000L >>) in
    (t, fun _loc -> <:ctyp< Int64.t >>)

let () = associate overload_int63 "Int63"

(* Let the int63 type be known to the lexer *)
let () =
  EXTEND Gram
    GLOBAL: Syntax.expr Syntax.ctyp;

    Syntax.ctyp: LEVEL "simple"
    [ [ "int63" -> typ _loc ] ];
  END

(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
