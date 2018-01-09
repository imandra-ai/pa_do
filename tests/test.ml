(* File: test.ml

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

open Format
open Camlp4.PreCast

(* We need to be able to gather the results of the tests so that the
   minimal failures can be printed.  There should be _no_ use of exit
   in the tests because if a test fails, even because the syntax is
   not correct, this does not forbid other tests to be run.  If a test
   wants to exit early (say because a syntax error prevents the
   extension from being compiled), it must use the exception [Exit].
   Each grouping will use an exception handler to identify precisely
   the place of the failure in the tree of tests.
*)

(***********************************************************************
 *                              Utils
 ***********************************************************************)

let is_space_or_semicolon c =
  c = ' ' || c = '\t' || c = '\n' || c = '\r' || c = ';'

let strip_end_spaces_and_semicolon =
  let rec strip s pos =
    if pos < 0 then ""
    else if is_space_or_semicolon s.[pos] then strip s (pos - 1)
    else (* do not strip s.[pos] *) String.sub s 0 (pos + 1) in
  fun s -> strip s (String.length s - 1)


(***********************************************************************
 *                     Generating random values
 ***********************************************************************)

module Var =
struct
  type t = {
    libs : string list;
    to_channel_and_string : out_channel -> string;
    (* Copies on an out channel a random value and returns the string
       representing the value.
       Thanks to this function, the type [t] does not depend on a
       parameter 'a. *)
    input_val : Syntax.Ast.expr;
    name : string
  }

  let to_channel_and_string random to_string stdout =
    let v = random() in
    Marshal.to_channel stdout v [];
    to_string v

  let make ?(libs=[])
      ?(to_string=fun v -> Marshal.to_string v [Marshal.Closures])
      ?(input_val=(<:expr@here< Marshal.from_channel>>)) random var =
    { libs = libs;
      to_channel_and_string = to_channel_and_string random to_string;
      input_val = input_val;
      name = var;
    }

  let int ?(bound=10_000) ?(random=fun () -> Random.int bound) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random string_of_int;
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : int) >>;
      name = var;
    }

  let int32 ?(bound=10_000l) ?(random=fun () -> Random.int32 bound) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random Int32.to_string;
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : int32) >>;
        name = var;
    }

  let int64 ?(bound=10_000L) ?(random=fun () -> Random.int64 bound) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random Int64.to_string;
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : int64) >>;
        name = var;
    }

  let nativeint ?(bound=10_000n) ?(random=fun () -> Random.nativeint bound) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random Nativeint.to_string;
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : nativeint) >>;
        name = var;
    }

  let float ?(bound=10_000.) ?(random=fun () -> Random.float bound) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random string_of_float;
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : float) >>;
      name = var;
    }

  let string ?(bound=50) ?(random=begin fun () ->
    let len = Random.int bound in
    let s = ref "" in
    for i = 1 to len do
      let char = Char.chr (Random.int 256) in
      s := !s ^ (Char.escaped char)
    done;
    !s end) var =
    { libs = [];
      to_channel_and_string = to_channel_and_string random (fun s -> s);
      input_val =
        <:expr@here< fun fh -> (Marshal.from_channel fh : string) >>;
      name = var;
    }

  let () = Random.self_init()
end

(***********************************************************************
 *                         Comparing values
 ***********************************************************************)

module Compare_val =
struct
  type t = {
    libs : string list;
    comp_no : int option;
    equal : Syntax.Ast.expr;
  }

  let make ?(libs=[]) ?comp_no equal =
    { libs = libs;
      comp_no = comp_no;
      equal = equal;
    }

  let bool ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun (a:bool) b -> Pervasives.(=) a b >>;
    }

  let int ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun (a:int) b -> Pervasives.(=) a b >>;
    }

  let int32 ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b -> Pervasives.(=) (Int32.compare a b) 0 >>;
    }

  let int64 ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b -> Pervasives.(=) (Int64.compare a b)0 >>;
    }

  let nativeint ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b ->
        Pervasives.(=) (Nativeint.compare a b) 0 >>;
    }

  let float_eq eps =
    let is_nan = <:expr@here< fun x -> Pervasives.( <> ) x x >> in
    <:expr@here< fun a b ->
      Pervasives.( || )
        (Pervasives.( && ) ($is_nan$ a) ($is_nan$ b))
        (Pervasives.(<) (abs_float (a -. b)) $`flo:eps$) >>;;

  let float ?comp_no ?(eps=1e-12) () =
    { libs = [];
      comp_no = comp_no;
      equal = float_eq eps;
    }

  let complex ?comp_no ?(eps=1e-12) () =
    { libs = [];
      comp_no = comp_no;
      equal =
        <:expr@here< fun z1 z2 ->
          Pervasives.( && )
            ($float_eq eps$ z1.Complex.re z2.Complex.re)
            ($float_eq eps$ z1.Complex.im z2.Complex.im) >>;
    }

  let string ?comp_no () =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b -> Pervasives.(=) a b >>;
    }

  let array ?comp_no comp =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b ->
        Pervasives.(=) (Array.length a) (Array.length b) &&
          (let is_eq = ref true in
           Array.iteri (fun i v ->
             if not(($comp.equal$) v b.(i))
             then is_eq := false) a;
          !is_eq)
        >>;
    }

  let list ?comp_no comp =
    { libs = [];
      comp_no = comp_no;
      equal = <:expr@here< fun a b ->
        Pervasives.(=) (List.length a) (List.length b) &&
          (let is_eq = ref true in
           List.iter2 (fun v_a v_b ->
             if not(($comp.equal$) v_a v_b)
             then is_eq := false) a b;
          !is_eq)
        >>;
    }
end

(***********************************************************************
 *                Grouping tests and handling errors
 ***********************************************************************)

type group_state = {
  libs : string list;
  header : Syntax.Ast.str_item;
  comp_no : int option;
  dir : string;
}

type failed =
  [ `Failed_exn of Loc.t * Syntax.Ast.patt
  | `Failed_regexp of Loc.t option * string
  | `Failed_val of Loc.t * (string * string) list list
  | `Failed_expr of Loc.t * Syntax.Ast.expr * Syntax.Ast.expr
  | `Exe_pgm_error of Loc.t option * string * string option
  | `Uncaught_exn of Loc.t option * string
  | `Syntax_error of Loc.t * string
  ]

exception Exit of failed

type result = [ `OK | `Failed of failed list ]

type test =
  | Group of string * group_state * test list
  | Test of string * (group_state -> result)

(** Default number of comparisons made to compare the values of two
    expressions. *)
let default_comp_no = 100

let group name ?(libs=[]) ?(header=(<:str_item@here< >>))
    ?comp_no tests =
  let group_state =
    { libs = libs;
      header = header;
      comp_no = comp_no;
      dir = "";
    } in
  Group(name, group_state, tests)

type api_test_case =
  [ `Raise of Syntax.Ast.patt
  | `Compare_expr of (string * Syntax.Ast.expr) list
  | `Compare_val of
      (Var.t list * Compare_val.t * string * Syntax.Ast.expr) list ]

type concrete_test_case =
  [ `Camlp4_error of
    (string * int * int * int * int * int * int * bool) option
    * bool option * string
  | `Compare_expr of (string * Syntax.Ast.expr) list
  | `Compare_val of
      (Var.t list * Compare_val.t * string * Syntax.Ast.expr) list ]

(***********************************************************************
 *                       Temp files and dirs
 ***********************************************************************)

module Temp =
struct
  let file_no = ref 0

  let filename g =
    incr file_no;
    Filename.concat g.dir ("test_" ^ (string_of_int !file_no) ^ ".ml")

  let open_out g =
    let n = filename g in
    (n, open_out n)

  let dir =
    let prng = Random.State.make_self_init () in
    let dir_name prefix suffix =
      let rnd = (Random.State.bits prng) land 0xFFFFFF in
      Filename.concat Filename.temp_dir_name
        (Printf.sprintf "%s%06x%s" prefix rnd suffix) in
    let rec try_name counter =
      let name = dir_name "test_" "_dir" in
      try
        Unix.mkdir name 0o700;
        name
      with (Sys_error _ | Unix.Unix_error(_,_,_)) as e ->
        if counter >= 1000 then raise e else try_name (counter + 1) in
    fun () ->
      try_name 0

  let cleanup_dir dir =
    try
      let files = Sys.readdir dir in
      Array.iter (fun f -> Sys.remove (Filename.concat dir f)) files;
      Unix.rmdir dir
    with e ->
      eprintf "Exception: %s occurred when deleting temporary files.\n%!"
        (Printexc.to_string e)

end

(***********************************************************************
 *                    Printing OCaml expressions
 ***********************************************************************)
(* Suggested by Jeremy Yallop <jeremy.yallop@ed.ac.uk> on the Caml list *)

let printer =
  let module P = Camlp4.Printers.OCaml.Make(Syntax)
  in new P.printer ()

let print_expr formatter e =
  printer#expr formatter e

let print_str_item formatter s =
  printer#str_item formatter s

let print_patt formatter p =
  printer#patt formatter p

(***********************************************************************
 *        Finding OCaml programs and their default arguments
 ***********************************************************************)

let ocamlc_pgm = ref "ocamlc"

let camlp4_pgm = ref "camlp4"

let include_args = ref []
  (* include path for camlp4, ocamlc,... *)

let not_debug = ref true

let dynlink_supported = ref false

let args = Arg.align [
  ("--ocamlc", Arg.Set_string ocamlc_pgm,
   " The complete path to the ocamlc compiler.");
  ("--camlp4", Arg.Set_string camlp4_pgm,
   " The complete path to the camlp4 preprocessor.");
  ("-I", Arg.String(fun inc -> include_args := "-I" :: inc :: !include_args),
   " Add an include path to search for libraries.");
  ("--dynlink", Arg.Bool(fun b -> dynlink_supported := b),
   "bool Tells whether dynlink is supported (it is since OCaml 3.11.0)");
  ("--debug", Arg.Unit(fun () -> not_debug := false),
   " Leave the temporary files and the locations of errors in them.")
]

(* FIXME: The tests may want to use additional flags, allow that *)
let () =
  let anon_fun _ = raise(Arg.Bad "no anonymous argument") in
  let usage = Filename.basename Sys.argv.(0) in
  Arg.parse args anon_fun usage

(***********************************************************************
 *                         Launch programs
 ***********************************************************************)

let input_all fh =
  let buf = Buffer.create 256 in
  try
    while true do
      Buffer.add_string buf (input_line fh);
      Buffer.add_char buf '\n';
    done;
    assert(false)
  with End_of_file -> Buffer.contents buf

let maybe_quote f =
  if String.contains f ' ' || String.contains f '\"'
  then Filename.quote f
  else f

(* [copy_env l] return environment variables (with their value) among
   the variables in [l] that are found in the current environment. *)
let copy_env l =
  let l = List.fold_left (fun l s ->
                            try (s ^ "=" ^ Sys.getenv s) :: l
                            with Not_found -> l
                         ) [] l in
  Array.of_list l

(* Launches a program and raises [Exit] if the process is interrupted. *)
let launch pgm args ?(send_stdin=fun _ -> ()) msg ?loc ?test fun_exit =
  (* Protect the program name and arguments against spaces (frequent
     on win32).  For win32, we also need to pass some environment
     variables (especially PATH and OCAMLLIB). *)
  let args = List.map maybe_quote args in
  let pgm = Filename.quote pgm ^ " " ^ String.concat " " args in
  let env = copy_env ["PATH"; "PATHEXT"; "OCAMLLIB"; "TEMP"; "TMP";
                      "LIB"; "LIBPATH"; "PATHEXT"] in
  let (out, stdin, err) as p = Unix.open_process_full pgm env in
  send_stdin stdin;
  close_out stdin;
  let out = input_all out
  and err = input_all err in
  match Unix.close_process_full p with
  | Unix.WSIGNALED _ ->
      raise(Exit(`Exe_pgm_error(?loc, msg ^ "killed by signal.", ?test)))
  | Unix.WSTOPPED _ ->
      raise(Exit(`Exe_pgm_error(?loc, msg ^ "stopped by signal.", ?test)))
  | Unix.WEXITED i -> fun_exit i out err

let pgms_checked = ref false

let version pgm remember_pgm msg =
  let msg_err = sprintf "While testing for %S, got " pgm in
  try
    launch pgm ["-v"] msg_err begin fun exit_code _ _ ->
      if exit_code <> 0 then (printf msg pgm; exit 2)
      else remember_pgm := pgm
    end
  with Exit(`Exe_pgm_error(_, msg, _)) -> printf "%s" msg

(* Tests whether ocamlc and camlp4 are found or not. *)
let check_pgms ?(ocamlc=(!ocamlc_pgm)) ?(camlp4=(!camlp4_pgm)) () =
  if not(!pgms_checked) then begin
    let error_ocamlc : (_,_,_) format =
      "Cannot run the program %S.  Make sure it is in your path \
      or use the \"--ocamlc\" command line option.\n" in
    let error_camlp4 : (_,_,_) format =
      "Cannot run the program %S.  Make sure it is in your path \
      or use the \"--camlp4\" command line option.\n" in
    version ocamlc ocamlc_pgm error_ocamlc;
    version camlp4 camlp4_pgm error_camlp4;
    pgms_checked := true;
  end

let error_without_loc err =
  try
    let len = String.length err in
    if !not_debug && len > 6 && String.sub err 0 6 = "File \"" then
        let start_msg = String.index err '\n' + 1 in
        String.sub err start_msg (len - start_msg)
    else err
  with Not_found -> err

let pgm_output_if_ok ?loc ?test msg err_code out err =
  if err_code <> 0 then
    let msg = msg ^ (error_without_loc err) in
    raise(Exit(`Exe_pgm_error(loc, msg, test)))
  else out

(** [run_ocamlc args] runs "ocamlc" with the [args].  Returns the exit
    code and the output of the process.  On the first invocation,
    finds the ocamlc compiler and caches it. *)
let run_ocamlc ?loc ?test args =
  let msg =
    "Test: while executing ocamlc, the following error was \
      encountered:\n" in
  launch !ocamlc_pgm (!include_args @ args)
    msg ?loc ?test (pgm_output_if_ok ?loc ?test msg)
    (* Could not compile the code module, most likely because
       the user made a mistake. *)

let run_camlp4 ?loc ?test args fname =
  (* FIXME: do we test the "you must load a grammar" error? *)
  let msg =
    "Test: while executing camlp4, the following error was \
      encountered:\n" in
  launch !camlp4_pgm (["-parser"; "o"] @ !include_args @ args
                      @ ["-printer"; "dumpcamlp4"; fname])
    msg ?loc ?test (pgm_output_if_ok ?loc ?test msg)
    (* It is possible that camlp4 exits abnormally, this indicates
       the failure of a test. *)

(* Launches camlp4 and returns the status and the error (used in the
   test [`Camlp4_error]). *)
let run_camlp4_for_error ?loc ?test args fname =
  let msg =
    "Test: while executing camlp4, the following error was \
      encountered:\n" in
  launch !camlp4_pgm (["-parser"; "o"] @ !include_args @ args
                       @ ["-printer"; "dumpcamlp4"; fname])
    msg ?loc ?test (fun i _ err -> (i,err))

(* Read the AST dump that camlp4 does with "-printer dumpcamlp4".
   For the format, see the code in Camlp4/Printers/DumpCamlp4Ast.ml *)
let extract_camlp4ast output =
  let magic = Camlp4_config.camlp4_ast_impl_magic_number in
  let len = String.length magic in
  if String.length output < len || String.sub output 0 len <> magic then
    failwith "Wrong magic number for the Camlp4 AST dump";
  let ast = Marshal.from_string output len in
  (ast: Syntax.Ast.str_item)
;;

(***********************************************************************
 *        Extract the desired expression from Camlp4 output
 ***********************************************************************)

exception Found_expr of Ast.expr

let expr_special_name = "test__expr__"

class find_special_name =
object(self)
  inherit Ast.map as super

  method binding = function
  | (<:binding< $lid:i$ = $e$ >>) when i = expr_special_name ->
              raise(Found_expr e)
  | s -> super#binding s
end


(* Return the Camlp4 AST corresponding to parsing the [test]
   expression with camlp4 launched with arguments [args]. *)
let expr_camlp4_test group_state ?loc ?test args f =
  let out = run_camlp4 ?loc ?test args f in
  try
    ignore(new find_special_name#str_item (extract_camlp4ast out));
    let msg = "Could not extract the expression under test from Camlp4 \
      output.  Please contact the authors of this library." in
    raise(Exit(`Exe_pgm_error(?loc, msg, ?test)))
  with Found_expr e -> e
;;

(***********************************************************************
 *                    Camlp4 extension, API test
 ***********************************************************************)

let str_item_of_expr expr =
  let _loc = Ast.loc_of_expr expr in
  <:str_item< let () = $expr$ >>;;

let concat_str_item s1 s2 =
  let _loc = Ast.loc_of_str_item s1 in  (* FIXME: better? *)
  <:str_item< $s1$ $s2$ >>

(** Dump the [str_item].  The file will contain the locations of the
    AST and can be directly compiled by ocamlc. *)
let dump_ast ?output_file str_item =
  try
    Printers.DumpOCamlAst.print_implem str_item ?output_file
  with Loc.Exc_located(loc, exn) ->
(*    Loc.print std_formatter loc;
    printf ":\n%s\n" (Printexc.to_string exn); *)
    raise (Exit(`Syntax_error(loc, Printexc.to_string exn)))

(** Message to be printed to indicate that the expected exception has
    been raised. *)
let expected_exception = "<exn OK>"

let is_expected_exn output =
  try
    let i = String.index output '\n' in
    String.sub output 0 i = expected_exception
  with Not_found -> output = expected_exception


class un_localize =
  let dummy_loc = Loc.mk "dummy" in
object(self)
  inherit Ast.map as super

  method loc (_:Loc.t) = dummy_loc
  method _Loc_t (_:Loc.t) = dummy_loc (* previous name of method [loc] in
                                         camlp4 3.10.0. *)
end

let un_localize = (new un_localize)#expr

(* Tells whether the two [str_item]s are the same regardless of their
   locations *)
let equivalent_ast e1 e2 = un_localize e1 = un_localize e2

(* Applies [f] and takes care about exceptions. *)
let handle_exn ?loc result_error f =
  try f()
  with
  | Exit err -> result_error err
  | e -> result_error (`Uncaught_exn(loc, Printexc.to_string e))

let compare_expr test_list compute_and_get =
  let failed_list =
    List.fold_left begin fun failed (test, expr) ->
      let loc = Ast.loc_of_expr expr in
      handle_exn ~loc (fun err -> err :: failed) begin fun () ->
        let expr' = compute_and_get test expr in
        (* Printers.OCaml.print_implem ast; *)
        (*Parser.OCaml.parse_expr Loc.ghost expr0 *)
        if not(equivalent_ast expr' expr) then
          (`Failed_expr(loc,expr',expr)) :: failed
        else failed
      end
    end [] test_list in
  if failed_list = [] then `OK else `Failed(failed_list)


let output_header ff group_state decl =
  print_str_item ff group_state.header;
  let decl = strip_end_spaces_and_semicolon decl in
  if decl <> "" then fprintf ff "%s;;@\n" decl;
  fprintf ff "%!"

let check_var var_list = ()
  (* FIXME: to implement *)

let compare_val group_state name l ?decl fcmo =
  let failed_list =
    List.fold_left begin fun failed (var_list, compare_t, test, expr) ->
      let loc = Ast.loc_of_expr expr in
      handle_exn ~loc (fun err -> err :: failed) begin fun () ->
        check_var var_list;
        (* FIXME: test on unbound variables? *)
        let fname, fh = Temp.open_out group_state in
        let ff = formatter_of_out_channel fh in
        output_header ff group_state "";
        (* Read the variables given on standard input *)
        List.iter begin fun var_t ->
          fprintf ff "let %s = (" var_t.Var.name;
          print_expr ff var_t.Var.input_val;
          fprintf ff ") stdin\n"
        end var_list;
        (match decl with
         | Some d -> if d <> "" then fprintf ff "%s;;\n" d
         | None -> ());
        (* Compare the (evaluated) expressions with overloading and
           the one supposed to return the same result. *)
        fprintf ff "if not((";
        print_expr ff compare_t.Compare_val.equal;
        fprintf ff "@\n) ( (* overloading: %s *)@\n%s@\n) ( (* expr *)@\n"
          name test;
        print_expr ff expr; (* FIXME: way not to loos the location? *)
        fprintf ff "@\n)) then exit 5";
        close_out fh;
        let libs =
          List.fold_left (fun libs var_t -> var_t.Var.libs @ libs) [] var_list in
        let fexe = Filename.chop_extension fname ^ ".exe" in
        let _ =
          run_ocamlc ~loc ~test
            [ "-o"; fexe; "-I"; "+camlp4"; String.concat " " libs;
              String.concat " " compare_t.Compare_val.libs;
              "-pp";
              !camlp4_pgm ^ " " ^ String.concat " " !include_args
              ^ " -parser o -printer a "
              ^ String.concat " " group_state.libs ^ " " ^ fcmo;
              fname ] in
        let test_no =
          match compare_t.Compare_val.comp_no with
          | Some n -> n
          | None ->
              (match group_state.comp_no with
               | Some n -> n
               | None -> default_comp_no) in
        let msg =
          "Test: while executing the test file, the following \
                  error was encountered:\n" in
        let loc = Ast.loc_of_expr expr in
        let failed_values = ref [] in
        for i = 1 to test_no do
          let values = ref [] in
          let send_stdin pgm_in =
            List.iter begin fun var_t ->
              let v_string = var_t.Var.to_channel_and_string pgm_in in
              values := (var_t.Var.name, v_string) :: !values
            end var_list in
          launch fexe !include_args ~send_stdin msg ~loc ~test
            (fun i out err ->
               if i = 5 (* exit 5 above *) then
                failed_values := List.rev !values :: !failed_values
              else if i <> 0 then
                let msg = msg ^ (error_without_loc err) in
                raise(Exit(`Exe_pgm_error(Some loc, msg, Some test))))
        done;
        if !failed_values <> [] then `Failed_val(loc,!failed_values) :: failed
        else failed
      end
    end [] l in
  if failed_list = [] then `OK else `Failed(failed_list)

let fcmo_dump_ast ?loc ?test f content =
  dump_ast content ~output_file:f;
  (* compile it *)
  let _ = run_ocamlc ?loc ?test [ "-c"; f] in
  Filename.chop_extension f ^ ".cmo"

let api name expr test =
  let run group_state =
    let expr = match test with
      | `Raise patt ->
          (* Test for exceptions, so add an exception handler so
             exceptions are checked on the Caml side. *)
          let _loc = Ast.loc_of_expr expr in
          <:expr< (try $expr$; assert(false)
                   with $patt$ ->
                     print_string $str:expected_exception$;
                     exit 0) >>
      | _ -> expr in
    let loc = Ast.loc_of_expr expr in
    let content = concat_str_item group_state.header (str_item_of_expr expr) in
    (* Create a file with the API invocation. *)
    let f = Temp.filename group_state in
    (* Apply campl4 to the test cases with the compiled module *)
    match test with
    | `Raise patt ->
        handle_exn ~loc (fun err -> `Failed([err])) (fun () ->
          let fcmo = fcmo_dump_ast ~loc f content in
          (* Checking that loading the syntax module in camlp4 prints
             the desired message. *)
          let out =
            run_camlp4 ~loc
              ["-parser"; String.concat " " group_state.libs;
               "-parser"; fcmo] "" in
          if is_expected_exn out then `OK
          else `Failed([`Failed_exn(loc, patt)]))
    | `Compare_expr l ->
        handle_exn ~loc (fun err -> `Failed([err])) (fun () ->
          let fcmo = fcmo_dump_ast ~loc f content in
          let compute_and_get test expr =
            let fname, fh = Temp.open_out group_state in
            if test <> "" then
              Printf.fprintf fh "let %s = %s;;\n%!" expr_special_name test;
            (* FIXME: raise an error when test = "" *)
            close_out fh;
            let args = ["-parser"; String.concat " " group_state.libs;
                        "-parser"; fcmo] in
            expr_camlp4_test group_state args fname
              ~test ~loc:(Ast.loc_of_expr expr)
          in
          compare_expr l compute_and_get)
    | `Compare_val l ->
        handle_exn ~loc (fun err -> `Failed([err])) (fun () ->
          Printers.OCaml.print_implem content ~output_file:f;
          let _ =
            run_ocamlc ~loc
              (if !dynlink_supported then
                 [ "-I"; "+camlp4"; "dynlink.cma"; "camlp4lib.cma";
                   "-pp"; "camlp4of.opt"; "-c"; f ]
               else [ "-I"; "+camlp4"; "camlp4lib.cma";
                    "-pp"; "camlp4of.opt"; "-c"; f ]) in
          let fcmo = Filename.chop_extension f ^ ".cmo" in
          compare_val group_state name l fcmo)
  in
  Test(name, run)


let concrete_syntax name decl test =
  let run group_state =
    match test with
    | `Camlp4_error(loc, with_loc, s) ->
        let loc = match loc with
          | Some l -> Some(Loc.of_tuple l)
          | None -> None in
        handle_exn ?loc (fun err -> `Failed([err])) (fun () ->
          let file, fh = Temp.open_out group_state in
          output_header (formatter_of_out_channel fh) group_state decl;
          close_out fh;
          let start =
            match with_loc with
            | Some true -> " *File \"[^\"]+\"[- ,()a-z0-9]+:[\n\r]+"
            | Some false | None -> "" in
          let regexp = Str.regexp (start ^ s) in
          let args = ["-parser"; String.concat " " group_state.libs] in
          let (i, err) = run_camlp4_for_error args file ?loc ~test:s in
          if i = 0 then
            `Failed([`Failed_regexp(loc, "")]) (* No error *)
          else if Str.string_match regexp err 0 then `OK
          else `Failed([`Failed_regexp(loc, error_without_loc err)]))
    | `Compare_expr l ->
        handle_exn (fun err -> `Failed([err])) begin fun () ->
          let compute_and_get test expr =
            let file, fh = Temp.open_out group_state in
            let ff = formatter_of_out_channel fh in
            output_header ff group_state decl;
            if test <> "" then
              fprintf ff "let %s = %s;;@\n%!" expr_special_name test;
            (* FIXME: raise an error when test = "" *)
            close_out fh;
            let args = ["-parser"; String.concat " " group_state.libs] in
            expr_camlp4_test group_state args file
              ~test ~loc:(Ast.loc_of_expr expr)
          in
          compare_expr l compute_and_get
        end
    | `Compare_val l ->
        handle_exn (fun err -> `Failed([err])) (fun () ->
          compare_val group_state name l ~decl:decl "")
  in
  Test(name, run)

(***********************************************************************
 *              Running the tests and collecting info
 ***********************************************************************)

type info = {
  type_test : test; (* Group or Test. *)
  result : result; (* Result of the test. *)
  sub_tests : info list (* Information about the sub-tests. *)
 }

let rec run_test no_test group_state info_list test =
  match test with
  | Group(name, g_st, tests) ->
      let group_state =
        { libs = List.append group_state.libs g_st.libs;
          header = concat_str_item group_state.header g_st.header;
          comp_no =
            (match g_st.comp_no with
            | Some n -> Some n
            | None -> group_state.comp_no);
          dir = group_state.dir } in
      let sub_tests = run_all no_test group_state tests in
      if sub_tests = [] then
        info_list (* No need to keep the group if its sub-tests succeeded. *)
      else
        let info_group = { type_test = test;
                           result = `OK;
                           sub_tests = sub_tests } in
        info_group :: info_list
  | Test(name, run) ->
      incr no_test;
      (* we need to "agglomerate" the results of all tests for a global
         report (success to all => no details, otherwise we want to
         know which one failed) *)
      let result = run group_state in
       match result with
      | `OK -> info_list (* No need to keep information. *)
      | _ ->
          let info_test = { type_test = test;
                            result = result;
                            sub_tests = [] } in
          info_test :: info_list

and run_all no_test group_state tests =
  List.fold_left (run_test no_test group_state) [] tests

(* Motto: each block introduces its new line. *)
let rec print_all no_failed info =
  let print_failed_tests failed_tests =
    let print_val l =
      List.iter (fun (name,value) ->
                   printf "@\nVariable %s with value %s" name value) l in
    if List.hd failed_tests = [] then (* No variables. *)
      printf "@\nThe two expressions are not equal."
    else
      let test_no = ref 1 in
      List.iter
        (fun l -> printf "@\nTest %i:" !test_no; incr test_no; print_val l)
        failed_tests in
  let loc_and_err loc msg err =
    match loc with
    | Some l ->
        printf "@\n%s:%s@\n%s" (Loc.to_string l) msg err
    | None ->
        printf "@\n%s@\n%s" msg err in
  match info.type_test with
  | Group(name,_,_) ->
      printf "@\n@[<2>+Group: %s" name;
      List.iter (print_all no_failed) (List.rev info.sub_tests);
      printf "@]"
  | Test(name,_) ->
      match info.result with
      | `OK -> () (* nothing to print *)
      | `Failed l ->
          incr no_failed;
          printf "@\n@[<2>+Test: %s" name;
          List.iter begin fun f -> match f with
          | `Failed_exn(loc, patt) ->
              printf "@\n%s:@\nThe following exception was not raised:@\n"
                (Loc.to_string loc);
              print_patt std_formatter patt
          | `Failed_regexp(loc, e) ->
              let expl =
                if e = "" then "No camlp4 error."
                else "The regular expression is not matched by the \
                Camlp4 error:" in
              (match loc with
              | Some l ->
                  if e = "" then
                    printf "@\n%s:@\n%s" (Loc.to_string l) expl
                  else
                    printf "@\n%s:@\n%s@\n%s" (Loc.to_string l) expl e
              | None ->
                  if e = "" then
                    printf "@\n%s" expl
                  else
                    printf "@\n%s@\n%s" expl e)
          | `Failed_val(loc, l) ->
              printf "@\n%s:" (Loc.to_string loc);
              print_failed_tests (List.rev l);
          | `Failed_expr(loc, expr, test) ->
              printf "@\n%s:@\nGOT   : " (Loc.to_string loc);
              print_expr std_formatter expr;
              printf "@\nWANTED: ";
              print_expr std_formatter test
          | `Exe_pgm_error(loc, err, _test) ->
              loc_and_err loc "" err
          | `Uncaught_exn(loc, exn) ->
              let msg = "Uncaught exception:" in
              loc_and_err loc msg exn
          | `Syntax_error(loc, exn) ->
              printf "@\n%s:@\nSyntax error:@\n%s"  (Loc.to_string loc) exn
          end (List.rev l);
          printf "@]"

let run ?ocamlc ?camlp4 test =
  check_pgms ?ocamlc ?camlp4 ();
  let group_state =
    { libs = [];
      header = <:str_item@here< >>;
      comp_no = None;
      dir = Temp.dir() } in
  printf "File %S:%!" (Filename.basename Sys.argv.(0));
  let no_test = ref 0 in
  let info = run_all no_test group_state [test] in
  if !not_debug then
    Temp.cleanup_dir group_state.dir;
  (* print info *)
  assert (List.length info < 2);
  if info = [] then
    if !no_test = 0 then
      printf "@\nNo test.@\n"
    else if !no_test = 1 then
      printf "@\nThe test succeeded.@\n"
    else
      printf "@\nAll %i tests succeeded.@\n" !no_test
  else (
    let no_failed = ref 0 in
    print_all no_failed (List.hd info);
    printf "@\n%i test%s failed on %i.@\n"
      !no_failed (if !no_failed > 1 then "s" else "") !no_test
  )


(* Local Variables: *)
(* compile-command: "omake --no--progress" *)
(* End: *)
