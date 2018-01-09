(* File: pa_do_linalg.ml

   Copyright (C) 2010

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

(* u <- u + neg_grad u is "common".  One does not know about [neg_grad
   u] so assume it returns a "fresh" vector.  Can be transformed into
   let g = neg_grad u in u <- u + g *)

type t =
  | Slice of string * expr * expr * expr
  | Add of t * t
