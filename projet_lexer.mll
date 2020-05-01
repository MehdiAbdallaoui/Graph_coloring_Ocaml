{ }

rule translate = parse
  | " "* ['a'-'z' 'A'-'Z']+ " "* "["
   (* activate "color" rule *)
   { color lexbuf }
  | "(*"
   (* activate "comment" rule *)
   { comment lexbuf }
  | _ as c
   (* print the current char *)
   { print_char c }
  | eof
   (* exit *)
   { exit 0 }

and comment = parse
  | "*)" "\n"*
   (* go to the "translate" rule *)
   { translate lexbuf }
  | _
   (* skip comments *)
   { comment lexbuf }

and color = parse
  | "]" "\n"*
   (* go to the "translate" rule *)
   { translate lexbuf }
  | _
   (* skip comments *)
   { color lexbuf }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      translate lexbuf
    done

  let _ = Printexc.print main ()
}
