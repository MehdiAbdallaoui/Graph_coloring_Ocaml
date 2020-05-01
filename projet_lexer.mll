{ }

rule token = parse
  | [’ ’ ’\t’ ’\n’]+
   (* skip spaces *)
   { token lexbuf }
  | "(*"
   (* activate "comment" rule *)
   { comment lexbuf }
  | _
   (* activate "translate" rule*)
   { translate lexbuf}

and comment = parse
  | "*)"
   (* go to the "token" rule *)
   { token lexbuf }
  | _
   (* skip comments *)
   { comment lexbuf }

and translate = parse
  | (*"current_directory" { print_string (Sys.getcwd ()) }
  | _ as c  { print_char c }
  | eof   { exit 0 }*) (*A MODIFIER POUR TRAITER LE FICHIER DOT DE LA QUESTION 13*)

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      translate lexbuf
    done

  let _ = Printexc.print main ()
}