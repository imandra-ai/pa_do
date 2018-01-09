open Printf
open Camlp4.PreCast
open Test

let not_operator =
  let not_op op =
    api (sprintf "%S cannot be used as operator" op)
      (<:expr@here< prefix $str:op$ >>)
      (`Raise <:patt@here< Not_operator _ >>) in
  group "Not operators"
    (List.map not_op ["..";
                      "->"; "<-"; ":"; ":>"; "."; "~"; "?"; "|";
                      "_ff"; (* initial unserscore *)
                      "gth12"; "123"; "f4t"; (* numbers *)
                        "+e"; "'t"; (* mix *)
                      "XX"; "Er"; (* uppercase *)
                      "{"; "r[e]"; "\255";
                     ])

let not_allowed =
  let not_allowed op =
    api (sprintf "Operator %S not allowed" op)
      (<:expr@here< prefix $str:op$ >>)
      (`Raise <:patt@here< Forbidden _ >>) in
  group "Allowed operators"
    (List.map not_allowed ["+"; "-"; "*"; "/";
                           "+."; "-."; "*."; "/."; "**";
                           "="; "<>"; "<"; ">"; "<="; ">=";
                           "=="; "!="; ":=";
                           "^"; "!";
                           "||"; "or"; "&&";
                           (* OCaml keywords. *)
                           "and";	"as";		"assert";
                           "begin";	"class";	"constraint";
                           "do";	"done";		"downto";
                           "else";	"end";		"exception";
                           "external";	"false";	"for";
                           "fun";	"function";	"functor";
                           "if";	"in";		"include";
                           "inherit";	"initializer";	"lazy";
                           "let";	"match";	"method";
                           "module";	"mutable";	"new";
                           "object";	"of";		"open";
                           "or";
                           "parser";
                           "private";	"rec";		"sig";
                           "struct";	"then";		"to";
                           "true";	"try";		"type";
                           "val";	"virtual";	"when";
                           "while";	"with";
                           "mod";	"land";		"lor";
                           "lxor";	"lsl";		"lsr";
                           "asr";
                          ])


let prefix =
  group "Simple prefix"
    [api "/+/"
       ( <:expr@here< prefix "/+/" >> )
       (`Compare_expr ["/+/ 4", <:expr@here< ( /+/ ) 4 >>;
               ]);
  (* api "prefix & (bug 43)"
       ( <:expr@here< prefix "&" >> )
       (`Compare_expr ["( &4 )", <:expr@here< ( & ) 4 >>;
                "(&4)", <:expr@here< ( & ) 4 >>;
               ]); *)
    ]

let postfix =
  group "Simple postfix"
    [api "/+/"
       ( <:expr@here< postfix "/+/" >> )
       (`Compare_expr ["4 /+/", <:expr@here< ( /+/ ) 4 >>;
               ]);
  (* api "postfix & (bug 43)"
       ( <:expr@here< postfix "&" >> )
       (`Compare_expr ["(4& )", <:expr@here< ( & ) 4 >>;
                "(4&)", <:expr@here< ( & ) 4 >>;
                "let (&) x = x + 1 in 2&",
                <:expr@here< let ( & ) x = x + 1 in 2& >>;
               ]); *)
    ]


let unary_wrong_levels =
  let wrong_level op =
    [api (sprintf "Level of %S not allowed for prefix operators" op)
       (<:expr@here< prefix "//" ~level:(level $str:op$) >>)
       (`Raise <:patt@here< Level.Bad_arity >>);
     api (sprintf "Level of %S not allowed for postfix operators" op)
       (<:expr@here< postfix "//" ~level:(level $str:op$) >>)
       (`Raise <:patt@here< Level.Bad_arity >>);
     api (sprintf "Level %S is not unary" op)
       (<:expr@here< ignore(Level.unary (Level.Higher (level $str:op$))) >>)
       (`Raise <:patt@here< Level.Bad_arity >>);
    ] in
  group "Unary wrong levels"
    (List.concat
       [wrong_level "||";
        wrong_level "&&";
        wrong_level "=";
        wrong_level "^";
        wrong_level "+";
        wrong_level "*";
        wrong_level "**";
       ])


let conflicts =
  group "Conflicts"
    [api "unary" <:expr@here< prefix "/+/"; postfix "/+/" >>
       (`Raise <:patt@here< Conflicting_kind(_, _, _) >>);
    ]


let () =
  run (group "Basic tests" ~libs:["pa_infix.cmo"]
          ~header:<:str_item@here< open Pa_infix  >>
          [not_operator; not_allowed; prefix; postfix;
           unary_wrong_levels; conflicts])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
