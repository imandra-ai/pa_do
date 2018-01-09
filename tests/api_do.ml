open Camlp4.PreCast
open Pa_do.Delimited_overloading

let () =
  let t = create "X" in
  int t (fun i _loc -> <:expr< $str:string_of_int i$ >>);
  array t (fun l _loc ->
             let cons l a = <:expr< $a$ :: $l$ >> in
             List.fold_left cons (<:expr< [] >>) l
          );
  array_get t (fun a i _loc -> <:expr< X.get $a$ $i$ >> );
  array_set t (fun a i x _loc -> <:expr< X.set $a$ $i$ $x$ >> );
  std_arithmetic t;
  associate_t t "X"

