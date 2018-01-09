open Printf
open Camlp4.PreCast
open Test

let unary_unary =
  group "Mixing several unary binary operators"
    [api "/+/"
       <:expr@here< (postfix "++";
                     let l = Level.unary (Level.Higher Level.default_unary) in
                     prefix "!!" ~level:l) >>
       (`Compare_expr ["!! 4 ++", <:expr@here< (++) (( !! ) 4) >>;
                "!! 4 + 5 ++", <:expr@here< (( !! ) 4) + (( ++ ) 5) >>;
               ])
    ]

let binary_binary =
  group "Mixing several binary operators"
    [api "/+/ v.s. +"
       (<:expr@here< infix "/+/" Level.addition >>)
       (`Compare_expr ["4 + 5 /+/ 6 + 7",
                      <:expr@here< (( /+/ ) (4 + 5) 6) + 7 >> ]);
    ]

let unary_binary =
  group "Mixing unary and binary operators"
    [api "Unary have higher precedence"
       (<:expr@here< postfix "%"; infix "++" Level.addition >>)
       (`Compare_expr ["3 + 4 %", <:expr@here< 3 + (( % ) 4) >>;
                "3 ++ 4 %", <:expr@here< ( ++ ) 3 (( % ) 4) >>;
                "3 ** 4 %", <:expr@here< ( ** ) 3 (( % ) 4) >>;
               ]);
    ]



let misc =
  group "Various tests mixing several operators"
    [api "test 1"
       (<:expr@here< (prefix "/+/";
                      postfix "%";
                      postfix "@";
                      let l = Level.binary ~assoc:Level.RightA Level.Highest in
                      infix "/*" l;
                      let l1 = level "!" in
                      let l2 = level "+" in
                      prefix ~level:l1 "/-";
                      infix "/+" l2;
                      postfix "ab";
                      prefix "bc";
                      infix "rem" (Level.binary Level.Lowest);
                     ) >> )
       (`Compare_expr ["3 + 2 %", <:expr@here< 3 + (( % ) 2 ) >>;
                "3 * 2 @", <:expr@here< 3 * (( @ ) 2 ) >>;
                "1 /* 2 /* 3 + 4", <:expr@here< (1 /* (2 /* 3)) + 4 >>;
                "/- 2 * 3", <:expr@here< (( /- ) 2) * 3 >>;
                "1 /+ 2 * 3", <:expr@here< 1 /+ (2 * 3) >>;
                "3 ab * 1 + 2", <:expr@here< ((ab 3) * 1) + 2 >>;
                "let ( bc ) x y = x + y in ()",
                <:expr@here< let bc x y = x + y in () >>;
                "f bc 2 + 3", <:expr@here< (f (bc 2)) + 3 >>;
                "a rem b + c", <:expr@here< rem a (b + c) >>;
               ]);
     api "test 2"
       (<:expr@here< (prefix "//";
                      prefix ~level:(level "!") "//";
                      let l = Level.binary ~assoc:Level.RightA Level.Highest in
                      infix "/-/" l;
                      infix "@" (Level.binary Level.Highest);
                      infix "&" Level.addition;
                      prefix "!%";
                      postfix ~level:(Level.unary Level.Lowest) "%";
                     ) >> )
       (`Compare_expr ["// 1 /-/ // 2 + 3",
                <:expr@here< ((( // ) 1) /-/ (( // ) 2)) + 3 >>;
                "1 /-/ 2 @ 3",
                <:expr@here< 1 /-/ (2 @ 3) >>;
                "if 1 < 2 & 3 < 4 then 1 else 2",
                <:expr@here< if 1 < (2 & 3) < 4 then 1 else 2 >>;
                "!% 1 %",
                <:expr@here< ( % ) (( !% ) 1) >>;
               ]);
    ]

let () =
  run (group "Mixing operators" ~libs:["pa_infix.cmo"]
          ~header:<:str_item@here< open Pa_infix  >>
          [unary_unary; binary_binary; unary_binary; misc])


(* Local Variables: *)
(* compile-command: "omake --no--progress test" *)
(* End: *)
