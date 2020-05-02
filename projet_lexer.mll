(* Header section *)
{ }

(* Rules section *)
rule translate = parse (* Declaration of the rule : translate *)
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
   (* exit if end of file*)
   { exit 0 }

and comment = parse (* Declaration of the rule : comment *)
  | "*)" "\n"*
   (* go to the "translate" rule *)
   { translate lexbuf }
  | _
   (* skip comments *)
   { comment lexbuf }

and color = parse (* Declaration of the rule : color *)
  | "]" "\n"*
   (* go to the "translate" rule *)
   { translate lexbuf }
  | _
   (* skip content *)
   { color lexbuf }

(* trailer section *)
{
  let main () =
    let lexbuf = Lexing.from_channel stdin in (* Create a lexer buffer on the given input channel stdin *)
    while true do
      translate lexbuf
    done

  let _ = Printexc.print main () (* Execute function main *)
}
