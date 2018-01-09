(* File: delimited_overloading.ml

   Copyright (C) 2008

     Dany Maslowski <dan_mski@hotmail.com>

     Julie De Pril <julie.depril@umons.ac.be>

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Camlp4.PreCast
open Pa_do.Delimited_overloading

(** [reloc f] returns a relocatable expression applying the function $m$.$f$ *)
let reloc_int f =
  fun i m _loc -> <:expr< $qualify_lid f m _loc$ $`int:i$ >>
let reloc_int32 f =
  fun i m _loc -> <:expr< $qualify_lid f m _loc$ $str:Int32.to_string i$ >>
let reloc_int64 f =
  fun i m _loc -> <:expr< $qualify_lid f m _loc$ $str:Int64.to_string i$ >>
let reloc_str f =
  fun i m _loc -> <:expr< $qualify_lid f m _loc$ $str:i$ >>

(** Overloadings for the module Ratio *)
let overloading_ratio =
  let t = int empty (reloc_int "ratio_of_int") in
  let t = int32 t (reloc_int32 "ratio_of_string") in
  let t = int64 t (reloc_int64 "ratio_of_string") in
  let t =
    string t (reloc_str "ratio_of_string") ~check:Ratio.ratio_of_string in
  let ov_ops = [
    "+","add_ratio";  "-","sub_ratio";  "*","mult_ratio";  "/","div_ratio";
    ("~-","minus_ratio"); ("min","min_ratio"); ("max","max_ratio");
    ("=","eq_ratio"); ("<","lt_ratio"); (">","gt_ratio"); ("<=","le_ratio");
    (">=","ge_ratio"); ("abs","abs_ratio"); ("sign","sign_ratio");
    ("square","square_ratio");
    ("floor", "floor_ratio"); ("ceil", "ceiling_ratio");
    "of_int", "ratio_of_int";  "to_int", "int_of_ratio";
    "float", "float_of_ratio";
    "of_string", "ratio_of_string"; "to_string", "string_of_ratio" ] in
  (* FIXME: more functions to overload? *)
  let t = lid_subst t ov_ops in
  let t = lid t "<>" begin fun m _loc ->
    <:expr< fun x y -> Pervasives.not($qualify_lid "eq_ratio" m _loc$ x y) >>
  end in
  (* FIXME: we also want to optimize [div_ratio_big_int],... *)
  let t = comparison t ~cmp:"compare_ratio" in
  let optimize tr expr =
    let _loc = Ast.loc_of_expr expr in
    let q f = transf_qualify tr f _loc in
    match expr with
    | <:expr< $int:i$ + $x$ >> | <:expr< ( + ) $x$ $int:i$ >> ->
      <:expr< $q "add_int_ratio"$ $int:i$ $(self tr)#expr x$ >>
    | <:expr< $int:i$ - $x$ >> ->
      <:expr< $q "add_int_ratio"$ $int:i$
        ($q "minus_ratio"$ $(self tr)#expr x$) >>
    | <:expr< $x$ - $int:i$ >> ->
      <:expr< $q "add_int_ratio"$ $int:"-" ^ i$ $(self tr)#expr x$ >>
    | <:expr< $int:i$ * $x$ >> | <:expr< $x$ * $int:i$ >> ->
      <:expr< ($q "mult_int_ratio"$ $int:i$ $(self tr)#expr x$) >>
    | <:expr< $int:i$ / $x$ >> ->
      <:expr< $q "div_int_ratio"$ $int:i$ $(self tr)#expr x$ >>
    | <:expr< $x$ / $int:i$ >> ->
      <:expr< $q "div_ratio_int"$ $(self tr)#expr x$ $int:i$ >>
    | <:expr< $x$ ** $int:i$ >> ->
      <:expr< $q"power_ratio_positive_int"$ $(self tr)#expr x$  $int:i$ >>
    | <:expr< $x$ ** $y$ >> ->
      <:expr< ($q "power_ratio_positive_big_int"$
                 $(self tr)#expr x$  $(self tr)#expr y$) >>
    | _ -> super tr expr in
  expr t optimize

(** Overloadings for the module Num *)
let overloading_num =
  let t = int empty (reloc_int "num_of_int") in
  let t = int32 t (reloc_int32 "num_of_string") in
  let t = int64 t (reloc_int64 "num_of_string") in
  let t = string t (reloc_str "num_of_string") ~check:Num.num_of_string in
  let ov_ops = [
    ("+","add_num"); ("-","sub_num"); ("*","mult_num"); ("/","div_num");
    ("**","power_num"); ("~-","minus_num");("mod","mod_num");("abs","abs_num");
    ("<","lt_num");(">","gt_num");("<=","le_num");(">=","ge_num");
    ("=","eq_num");("succ","succ_num");("pred","pred_num");("incr","incr_num");
    ("decr","decr_num");("max","max_num");("min","min_num");
    ("floor", "floor_num"); ("ceil", "ceiling_num");
    "of_int", "num_of_int";  "to_int", "int_of_num";
    "float", "float_of_num";
    ("of_nat", "num_of_nat");
    ("of_ratio", "num_of_ratio"); ("of_bif_int", "num_of_bif_int");
    ("of_string", "num_of_string"); ("to_string", "string_of_num");
  ] in
  (* FIXME: more functions to overload? *)
  let t = lid_subst t ov_ops in
  lid t "<>" begin fun m _loc ->
    <:expr< fun x y -> Pervasives.not($qualify_lid "eq_num" m _loc$ x y) >>
  end


(** Overloadings for the module Big_int *)
let overloading_big_int =
  let t = int empty (reloc_int "big_int_of_int") in
  let t = int32 t (reloc_int32 "big_int_of_string") in
  let t = int64 t (reloc_int64 "big_int_of_string") in
  let t =
    string t (reloc_str "big_int_of_string") ~check:Big_int.big_int_of_string in
  let t = comparison t ~cmp:"compare_big_int" in
  (* We override some comparison operators with specialized functions *)
  let ov_ops = [
    ("+","add_big_int"); ("-","sub_big_int"); ("*","mult_big_int");
    ("/","div_big_int"); ("~-","minus_big_int"); ("abs","abs_big_int");
    ("sqrt","sqrt_big_int"); ("mod","mod_big_int"); ("max","max_big_int");
    ("min","min_big_int"); ("succ","succ_big_int"); ("pred","pred_big_int");
    ("=","eq_big_int"); ("<","lt_big_int"); (">","gt_big_int");
    ("<=","lt_big_int"); (">=","ge_big_int"); ("gcd","gcd_big_int");
    ("square","square_big_int");
    "land", "and_big_int";  "lor", "or_big_int";  "lxor", "xor_big_int";
    "lsl", "shift_left_big_int";  "lsr", "shift_right_big_int";
    "asr", "shift_right_towards_zero_big_int";
    "of_int", "big_int_of_int";      "to_int", "int_of_big_int";
    "of_int32", "big_int_of_int32";  "to_int32", "int32_of_big_int";
    "of_nativeint", "big_int_of_nativeint";
    "to_nativeint", "nativeint_of_big_int";
    "of_int64", "big_int_of_int64";  "to_int64", "int64_of_big_int";
    "float", "float_of_big_int";
    "of_string", "big_int_of_string"; "to_string", "string_of_big_int" ] in
  let t = lid_subst t ov_ops in
  (* FIXME: more functions to overload? *)
  (* FIXME: the following must be part of a general optimisation scheme. *)
  let optimize tr expr =
    let _loc = Ast.loc_of_expr expr in
    let self = (self tr)#expr in
    let qualify id = transf_qualify tr id _loc in
    match expr with
    | <:expr< ( + ) $int:i$ $x$ >> | <:expr< ( + ) $x$ $int:i$>> ->
      <:expr< $qualify "add_int_big_int"$ $int:i$ $self x$ >>
    | <:expr< $int:i$ - $x$ >> ->
      let neg_i = - int_of_string i in
      <:expr< $qualify "minus_big_int"$
        ( $qualify "add_int_big_int"$ $`int:neg_i$ $self x$ ) >>
    | <:expr< $x$ - $int:i$ >>  ->
      let neg_i = - int_of_string i in
      <:expr< $qualify "add_int_big_int"$ $`int:neg_i$ $self x$ >>
    | <:expr< ( * ) $int:i$ $x$ >> |  <:expr< ( * ) $x$ $int:i$ >>  ->
      <:expr< $qualify "mult_int_big_int"$ $int:i$ $self x$ >>
    | <:expr< ( / ) $int:i$ $x$ >> |  <:expr< ( / ) $x$ $int:i$ >>  ->
      <:expr< $qualify "div_int_big_int"$ $int:i$ $self x$ >>
    | <:expr< ( ** ) $int:i$ $int:j$ >> ->
      <:expr< $qualify "power_int_positive_int"$ $int:i$ $int:j$ >>
    | <:expr< ( ** ) $x$ $int:i$ >> ->
      <:expr< $qualify "power_big_int_positive_int"$ $self x$  $int:i$ >>
    | <:expr< ( ** ) $int:i$ $x$>> ->
      <:expr< $qualify "power_int_positive_big_int"$ $int:i$  $self x$ >>
    | <:expr< ( ** ) $x$ $y$ >> ->
      <:expr< $qualify "power_big_int_positive_big_int"$  $self x$  $self x$ >>
    | _ -> super tr expr in
  expr t optimize


let () =
  associate overloading_ratio "Ratio";
  associate overloading_num "Num";
  associate overloading_big_int "Big_int"


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
