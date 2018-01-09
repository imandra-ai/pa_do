open Printf
open Camlp4.PreCast
open Test

let random() = string_of_float (Random.float 1e7)

let eq_int_list = Compare_val.list (Compare_val.int ()) ~comp_no:10

let rec list_init n acc f =
  if n <= 0 then acc else list_init (n-1) (f n :: acc) f

let list_init n f = list_init n [] f

(* Naive way of generating random strings. *)
let random_string () =
  let len = Random.int 1000 in
  let a = Char.code 'a' in
  let s = String.create len in
  for i = 0 to len - 1 do s.[i] <- Char.chr(Random.int 26 + a) done;
  s

(* Silly example to exercise the concrete syntax. *)
let module_convert = "
module Convert =
struct
  let add l1 l2 = [List.hd l1 + List.hd l2]
  let sub l1 l2 = [List.hd l1 - List.hd l2]
  let mul l1 l2 = [List.hd l1 * List.hd l2]
  let div l1 l2 = [List.hd l1 / List.hd l2]
  let neg l1 = [- List.hd l1]
  let abs l1 = [abs (List.hd l1)]
  let compare l1 l2 = Pervasives.compare (List.hd l1) (List.hd l2)
  let compare2 l1 l2 = Pervasives.compare (List.hd l1) (List.hd l2)

  let get = List.nth
  let set l i x = List.nth l i + x
  let assign_ref a b = 3 * a + 7 * b
  let assign = assign_ref

  let list_of_int (a:int) = [a]
  let list_of_int32 (a:int32) = [a]
  let list_of_int64 (a:int64) = [a]
  let list_of_nativeint (a:nativeint) = [a]
  let list_of_string (s:string) = [s]
  let list_of_poly_var (p:string) = [p]
end;;
"

let simple =
  let arithmetic_tests =
    [[Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( [x] + [y] )",
     <:expr@here< [x + y] >> ;

     [Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( [x] - [y] )",
     <:expr@here< [x - y] >> ;

     [Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( [x] * [y] )",
     <:expr@here< [x * y] >> ;

     [Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( [x] / [y] )",
     <:expr@here< Convert.div [x] [y] >>;

     [Var.int "x"], eq_int_list,
     "Convert.( ~- [x])",
     <:expr@here< [~-x] >>; ] in

  let comparison_tests =
    [[Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] < [y] )",
     <:expr@here< x < y >> ;

     [Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] <= [y] )",
     <:expr@here< x <= y >> ;

     [Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] > [y] )",
     <:expr@here< x > y >> ;

     [Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] >= [y] )",
     <:expr@here< x >= y >> ;

     [Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] = [y] )",
     <:expr@here< x = y >> ;

     [Var.int "x"; Var.int "y"],
     Compare_val.bool ~comp_no:10 (),
     "Convert.( [x] <> [y] )",
     <:expr@here< x <> y >> ;

     [Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( max [x] [y] )",
     <:expr@here< [max x y] >> ;

     [Var.int "x"; Var.int "y"], eq_int_list,
     "Convert.( min [x] [y] )",
     <:expr@here< [min x y] >> ; ] in

  group "Simple concrete syntax tests for pa_do"
    [concrete_syntax "+"
       "OVERLOAD Pervasives ( ( + ) -> ( +. ))"
       (`Compare_val [[Var.float "x"; Var.float "y"],
                      Compare_val.float ~comp_no:10 (),
                      "Pervasives.(x + y)",
                      (<:expr@here< x +. y >>); ]);
     concrete_syntax "Arithmetic"
       (module_convert
        ^ "OVERLOAD_ARITHMETIC Convert")
       (`Compare_val arithmetic_tests);
     concrete_syntax "inherit"
       (module_convert
        ^ "OVERLOAD_ARITHMETIC Temporary
        OVERLOAD Temporary ( abs -> abs ; to_string -> to_string )
        OVERLOAD Convert inherit Temporary")
       (`Compare_val ([[Var.int "x"], eq_int_list,
                       "Convert.( abs [x] )",
                       <:expr@here< [abs x] >>]
                      @ arithmetic_tests ));
     concrete_syntax "Comparison (default compare)"
       (module_convert ^ "OVERLOAD_COMPARISON Convert")
       (`Compare_val comparison_tests);
     concrete_syntax "Comparison (specify function)"
       (module_convert ^ "OVERLOAD_COMPARISON Convert(compare2)")
       (`Compare_val comparison_tests);

     concrete_syntax "int"
       "OVERLOAD_INT Pervasives (float_of_int)"
       (let tests = list_init 20 (fun _ ->
                                    let i = Random.int 10000 - 5000 in
                                    ([], Compare_val.float () ~comp_no:1,
                                     sprintf "Pervasives.(%i)" i,
                                     <:expr@here< $`flo:float_of_int i$ >>)) in
        `Compare_val tests);
     concrete_syntax "float"
       "OVERLOAD_FLOAT Pervasives (int_of_float)"
       (let tests = list_init 20 (fun _ ->
                                    let f = Random.float 10000. -. 5000. in
                                    ([], Compare_val.int () ~comp_no:1,
                                     sprintf "Pervasives.(%f)" f,
                                     <:expr@here< $`int:int_of_float f$ >>)) in
        `Compare_val tests);
     concrete_syntax "int32"
       (module_convert
        ^ "OVERLOAD_INT32 Convert (list_of_int32)")
       (let tests = list_init 20 begin fun _ ->
          let i = Random.int32 10000l in
          ([], Compare_val.list (Compare_val.int32 ()) ~comp_no:1,
           sprintf "Convert.(%lil)" i,
           <:expr@here< [$`int32:i$] >>)
        end in
        `Compare_val tests);
     concrete_syntax "int64"
       (module_convert
        ^ "OVERLOAD_INT64 Convert (list_of_int64)")
       (let tests = list_init 20 begin fun _ ->
          let i = Random.int64 10000L in
          ([], Compare_val.list (Compare_val.int64 ()) ~comp_no:1,
           sprintf "Convert.(%LiL)" i,
           <:expr@here< [$`int64:i$] >>)
        end in
        `Compare_val tests);
     concrete_syntax "Nativeint"
       (module_convert
        ^ "OVERLOAD_NATIVEINT Convert (list_of_nativeint)")
       (let tests = list_init 20 begin fun _ ->
          let i = Random.nativeint 10000n in
          ([], Compare_val.list (Compare_val.nativeint ()) ~comp_no:1,
           sprintf "Convert.(%nin)" i,
           <:expr@here< [$`nativeint:i$] >>)
        end in
        `Compare_val tests);
     concrete_syntax "string"
       (module_convert
        ^ "OVERLOAD_STRING Convert (list_of_string)")
       (let tests = list_init 20 begin fun _ ->
          let s = random_string() in
          ([], Compare_val.list (Compare_val.string ()) ~comp_no:1,
           sprintf "Convert.(%S)" s,
           <:expr@here< [$str:s$] >>)
        end in
        `Compare_val tests);
     concrete_syntax "poly_var"
       (module_convert
        ^ "OVERLOAD_POLY_VAR Convert (list_of_poly_var)")
       (let cmp = Compare_val.make ~comp_no:1 (<:expr@here< ( = ) >>) in
        `Compare_val [[], cmp,
                      "Convert.( `x )", <:expr@here< [ "x" ] >>;
                      [], cmp,
                      "Convert.( `y )", <:expr@here< [ "y" ] >>;  ]);

     concrete_syntax "array_get"
       (module_convert
        ^ "OVERLOAD_ARRAY_GET Convert (get)")
       (`Compare_val [[Var.int "i" ~bound:5], Compare_val.int (),
                      "Convert.( [5;4;3;2;1;0].(i) )",
                      <:expr@here< Convert.get [5;4;3;2;1] i >> ]);
     concrete_syntax "array_set"
       (module_convert
        ^ "OVERLOAD_ARRAY_SET Convert (set)")
       (`Compare_val [[Var.int "i" ~bound:5; Var.int "x"],
                      Compare_val.int (),
                      "Convert.( [5;4;3;2;1;0].(i) <- x )",
                      <:expr@here< Convert.set [5;4;3;2;1;0] i x >>; ]);
     concrete_syntax "string_get"
       (module_convert
        ^ "OVERLOAD_STRING_GET Convert (get)")
       (`Compare_val [[Var.int "i" ~bound:5], Compare_val.int (),
                      "Convert.( [5;4;3;2;1;0].[i] )",
                      <:expr@here< Convert.get [5;4;3;2;1] i >> ]);
     concrete_syntax "string_set"
       (module_convert
        ^ "OVERLOAD_STRING_SET Convert (set)")
       (`Compare_val [[Var.int "i" ~bound:5; Var.int "x"],
                      Compare_val.int (),
                      "Convert.( [5;4;3;2;1;0].[i] <- x )",
                      <:expr@here< Convert.set [5;4;3;2;1;0] i x >>; ]);
     concrete_syntax "assign_ref"
       (module_convert
        ^ "OVERLOAD_ASSIGN_REF Convert (assign_ref)")
       (`Compare_val [[Var.int "a"; Var.int "b"],  Compare_val.int (),
                      "Convert.( a := b )",
                      <:expr@here< Convert.assign_ref a b >> ]);
     concrete_syntax "assign"
       (module_convert
        ^ "OVERLOAD_ASSIGN Convert (assign)")
       (`Compare_val [[Var.int "a"; Var.int "b"],  Compare_val.int (),
                      "Convert.( a <- b )",
                      <:expr@here< Convert.assign a b >> ]);

     concrete_syntax "openin"
       (module_convert
        ^ "OVERLOAD_OPENIN Convert")
       (`Compare_val [[Var.int "x"; Var.int "y"],
                      Compare_val.int (),
                      "Convert.( compare (list_of_int x) (list_of_int y) )",
                      <:expr@here< Convert.compare (Convert.list_of_int x)
                        (Convert.list_of_int y) >> ]);
     (* FIXME: better remove test needed *)
     concrete_syntax "remove"
       "OVERLOAD_REMOVE Int32 ( + ; * )
       Int32.( a + b )"
       (`Camlp4_error(None, None, "")) (* tighten regexp? *)
    ]



let () =
  run (group "Concrete syntax tests" ~libs:["pa_do.cmo"]
          [simple])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)


