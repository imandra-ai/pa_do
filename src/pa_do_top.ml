(* File: pa_do_top.ml

   Copyright (C) 2008

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

let () =
  let parse_toplevel_phrase = !Toploop.parse_toplevel_phrase in
  let parse_and_reset buf =
    Pa_do.Delimited_overloading.Overloading.toplevel_reset();
    parse_toplevel_phrase buf in
  Toploop.parse_toplevel_phrase := parse_and_reset


(* Local Variables: *)
(* compile-command: "make -C .." *)
(* End: *)
