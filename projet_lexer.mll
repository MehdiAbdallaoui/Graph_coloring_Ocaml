{ }

rule translate = parse
  | "current_directory"
   { print_string (Sys.getcwd ()) }
  | "(*"
   (* activate "comment" rule *)
   { comment lexbuf }
  | _ as c  { print_char c }
  | eof   { exit 0 }

and comment = parse
  | "*)" "\n"*
   (* go to the "translate" rule *)
   { translate lexbuf }
  | _
   (* skip comments *)
   { comment lexbuf }

{
  let main () =
    let lexbuf = Lexing.from_channel stdin in
    while true do
      translate lexbuf
    done

  let _ = Printexc.print main ()
}
