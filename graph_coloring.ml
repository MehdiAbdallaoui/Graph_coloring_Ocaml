(*Graphes non-orientés*)

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

(*remove_vertex*)
let remove_edge u v g = 
  let u_succ = try StringMap.find u g with Not_found -> StringSet.empty in   
  let u_succ' = (StringSet.remove v u_succ) in
  (StringMap.add u u_succ' g)


let remove_vertex u g =
  let voisins_de_u = try StringMap.find u g with Not_found -> StringSet.empty in
  let g' = StringMap.remove u g in
      StringSet.fold
        (fun v a ->
          remove_edge v u a)
        voisins_de_u
        g'
(*fin remove_vertex*)


(*Coloriages disponibles*)
(*Module IntSet*)
module Int = struct
  type t = int
  let compare = fun x y -> x - y (*or let compare = compare, to verify*)
end
  
module IntSet = Set.Make(Int) (*On crée un module IntSet qui aura les mêmes attributs/fonctions que Set.Make ayant les propriétés de Int *)
(*fin module IntSet*)


(*color_set*)
let rec color_set_aux i j acc = if i > j then IntSet.empty 
                                else if i = j then IntSet.add i acc 
                                else let acc1 = IntSet.add i acc in (color_set_aux (i+1) j acc1)

let color_set j = color_set_aux 1 j IntSet.empty
(*fin color_set*)

(*disp_color*)
type disp_color = IntSet.t StringMap.t
(*fin disp_color*)


(*to_dot_init_colors*)
let to_dot_init_colors g =
  let _ = Printf.printf "MonGraph init colors {\n" in
  let _ =
    StringMap.iter (fun u us -> IntSet.iter (fun v -> Printf.printf "  %s -> %d;\n" u v) us) g
  in
  Printf.printf "}\n"

(*fin to_dot_init_colors*)

(*add_edge_init_colors*)
let add_edge_init_colors u v g = 
  let u_succ = try StringMap.find u g with Not_found -> IntSet.empty in   
  let u_succ' = (IntSet.add v u_succ) in
  (StringMap.add u u_succ' g)
(*fin add_edge_init_colors*)


(*init_colors*)
let init_colors g k =
  let acc = StringSet.empty in
    let nodes_of_g = StringMap.fold (fun u us a -> StringSet.add u a) g acc in
      let range = color_set k in
        let graph = StringMap.empty in
          StringSet.fold
            (fun v g -> 
              IntSet.fold
                (fun r g1->
                add_edge_init_colors v r g1)
                range
                g
            )
            nodes_of_g
            graph
(*fin init_colors*)

(*remove_color*)
let remove_color i v c =
  let v_succ = try StringMap.find v c with Not_found -> IntSet.empty in   
    let v_succ' = (IntSet.remove i v_succ) in
      (StringMap.add v v_succ' c)
(*fin remove_color*)

(*exception Failed*)
exception Failed of string

(*Test*)
let test_exception f = if f=0 then raise (Failed "Pas de chance! Aucun coloriage n'a ete trouve.")
(*fin test*)
(*fin exception Failed*)

(*try_first*)
let rec try_first f s = 
  let exception_message = "Pas de chance! Aucun coloriage n'a ete trouve." in
    if s = IntSet.empty then raise (Failed exception_message)
    else let i = IntSet.choose s in
      try f i 
      with Failed _ -> let s' = IntSet.remove i s in
                        try_first f s'

(*Test*)
let s =                             
  let s0 = IntSet.add 1 IntSet.empty in
  let s0 = IntSet.add 2 s0 in
  let s0 = IntSet.add 3 s0 in
  IntSet.add 4 s0

let try_first_test = try_first (fun x -> if x < 4 then raise (Failed "Echec") else x) s

let try_first_test = try_first (fun x -> if x < 5 then raise (Failed "Echec") else x) s
(*fin test*)
(*fin try_first*)