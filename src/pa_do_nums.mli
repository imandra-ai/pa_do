open Pa_do

(** Overloadings for the modules [Num], [Big_int] and [Ratio].

    Overloaded values comprise integer literals, arithmetic and
    comparison functions, [of_int], [to_int], [float], [of_string],
    [to_string], [min], [max],...
    Also strings are overloaded to be able to write literal values for
    these modules.  The syntax of the string must be the one expected
    by the [of_string] function.

    Because of compile time checks, one must load the "nums.cma"
    library into camlp4 i.e., use it like [camlp4 -parser ... nums.cma
    pa_do.cmo pa_do_nums.cmo ...].
*)

val overloading_num : Delimited_overloading.t

val overloading_ratio : Delimited_overloading.t

val overloading_big_int : Delimited_overloading.t
