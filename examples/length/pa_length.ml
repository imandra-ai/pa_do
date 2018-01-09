open Camlp4.PreCast
open Pa_do.Delimited_overloading

let t = std_arithmetic empty

(* Interpret floats in the metric system by default.  We do not want
   caching because, the function [Length.meters] is just the identity
   (and caching will prevent the compiler from optimizing it away). *)
let t = float t ~cache:false
  (* After being linked to the [Length] module name through the
     [associate] statement below, [meters] will be qualified as
     [Length.meters].  The later was not used to allow other
     qualifications in case this overloading is linked to another
     module name. *)
  (fun x m _loc -> <:expr< $qualify_lid "meters" m _loc$ $`flo:x$ >> )

let t = lid_subst t [("to_float", "to_float")]

(* Interpret [feet 10.] and [(10: feet)] as a length in feet before
   [10.] is transformed into meters. *)
let t =
  let feet tr e =
    let _loc = Ast.loc_of_expr e in
    match e with
    | <:expr< feet $flo:x$ >>
    | <:expr< ($flo:x$: feet) >> ->
      (* Not further processed by the other rules in [super]. *)
      <:expr< $transf_qualify tr "feet" _loc$ $flo:x$ >>
    | _ -> super tr e in
  expr t feet

let () = associate t "Length"
