(* File: pa_do_top_make.ml

   Copyright (C) 2012

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


open Format

let eval_string ?(print_outcome=false) ?(err_formatter=err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let () =
  if not(eval_string "#camlp4o;;") then
    eprintf "Could not execute `#camlp4o'.  Issue `#use \"topfind\";;'.";
  if not (eval_string "#require \"pa_do\";;") then
    eprintf "Package \"pa_do\" not loadable!"
