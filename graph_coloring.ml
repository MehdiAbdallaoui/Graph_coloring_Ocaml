module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

type graph = StringSet.t StringMap.t



(*to_dot*)
let to_dot g =
  let _ = Printf.printf "digraph MonGraph {\n" in
  let _ =
    StringMap.iter (fun u us -> StringSet.iter (fun v -> Printf.printf "  %s -> %s;\n" u v) us) g
  in
  Printf.printf "}\n"

(*fin to_dot*)

(*add_edge*)
let add_edge_aux u v g = 
  let u_succ = try StringMap.find u g with Not_found -> StringSet.empty in   
  let u_succ' = (StringSet.add v u_succ) in
  (StringMap.add u u_succ' g)     
  
let add_edge u v g =
  let g' = (add_edge_aux u v g) in
  (add_edge_aux v u g')   
  
let graph =
  let g = add_edge "a" "b" StringMap.empty in
  let g = add_edge "a" "e" g in
  add_edge "g" "m" g     

(*fin add_edge*)

