(*Graphes non-orientés*)

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

type graph = StringSet.t StringMap.t

(*to_dot
  Modification de to_dot
*)
    
let to_dot g =
  let _ = Printf.printf "graph MonGraph {\n" in
  let _ =
    StringMap.iter (fun u us -> if(StringSet.is_empty us)
      then Printf.printf " %s -- Empty" u
      else StringSet.iter (fun v -> Printf.printf "  %s -- %s;\n" u v) us) g
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
  let g = add_edge "b" "c" g in
  add_edge "c" "a" g 


(*fin add_edge*)

    
(*remove_vertex*)
    
let remove_edge u v g = 
  let u_succ = try StringMap.find u g with Not_found -> StringSet.empty in   
  let u_succ' = (StringSet.remove v u_succ) in (StringMap.add u u_succ' g)


let remove_vertex u g =
  let voisins_de_u = try StringMap.find u g with Not_found -> StringSet.empty in
  let g' = StringMap.remove u g in StringSet.fold (fun v a -> remove_edge v u a) voisins_de_u g'
       

(*Coloriages disponibles*)
(*Module IntSet*)
module Int = struct
  type t = int
  let compare = fun x y -> x - y
end
  
module IntSet = Set.Make(Int) (*On crée un module IntSet qui aura les mêmes attributs/fonctions que Set.Make ayant les propriétés de Int *)
(*fin module IntSet*)


(*color_set*)
  
let rec color_set_aux i j acc = if i > j then raise (Failure "Echec! Votre argument doit etre >= 1") 
                                else if i = j then IntSet.add i acc 
                                else let acc1 = IntSet.add i acc in (color_set_aux (i+1) j acc1)


let color_set j = color_set_aux 1 j IntSet.empty
(*
déclaration de color_set avec une exception
let color_set j = if j <= 1 then failwith "Impossible" else color_set_aux 1 j IntSet.empty;;
*)

(*fin color_set*)

(*disp_color*)
    
type disp_color = IntSet.t StringMap.t 
    
(*fin disp_color*)

(*to_dot_init_colors*)

let to_dot_init_colors g =
  let _ = Printf.printf "MonGraph init colors {\n" in
  let _ =
    StringMap.iter (fun u us -> IntSet.iter (fun v -> Printf.printf "  %s -- %d;\n" u v) us) g
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
(*Méthode 1*)
let init_colors g k =
  let acc = StringSet.empty in
    let nodes_of_g = StringMap.fold (fun u us a -> StringSet.add u a) g acc in
      let range = color_set k in
        let graph = StringMap.empty in
        StringSet.fold (fun v g -> IntSet.fold (fun r g1-> add_edge_init_colors v r g1) range g) nodes_of_g graph
	  
(*fin Méthode 1*)

(*Méthode 2*)
(*
  let init_colors g k =
    StringMap.map (fun a -> color_set k) g
*)
(*fin Méthode 2*)
(*fin init_colors*)

(*remove_color*)
	    
let remove_color i v c =
  let v_succ = StringMap.find v c  in
    let v_succ' = (IntSet.remove i v_succ) in (StringMap.add v v_succ' c)
      
(*fin remove_color*)

(*exception Failed*)
      
exception Failed of string
   
(*fin exception Failed*)

(*try_first*)
    
let rec try_first f s = 
  let exception_message = "Pas de chance! Aucun coloriage n'a ete trouve." in
    if s = IntSet.empty then raise (Failed exception_message)
    else let i = IntSet.choose s in
      try f i with Failed _ -> let s' = IntSet.remove i s in try_first f s'

(*Test*)
let s =                             
  let s0 = IntSet.add 1 IntSet.empty in
  let s0 = IntSet.add 2 s0 in
  let s0 = IntSet.add 3 s0 in
  IntSet.add 4 s0

let try_first_test = try_first (fun x -> if x < 4 then raise (Failed "Echec") else x) s

let try_first_test = try_first (fun x -> if x < 5 then raise (Failed "Echec") else x) s

let try_first_test = try_first (fun x -> if (x=1 || x=2) then raise (Failed "ERREUR 404") else x) s

(*fin test*)
(*fin try_first*)


(*coloring*)
type coloring = int StringMap.t
(*fin coloring*)

(*to_dot_coloriage*)
let to_dot_coloriage g =
  let _ = Printf.printf "MonGraph coloré {\n" in
  let _ =
    StringMap.iter (fun u us -> Printf.printf " %s -- %d \n" u us) g
  in
  Printf.printf "}\n"
(*fin to_dot_coloriage*)

(*color*)
let rec color g c =
  if StringMap.is_empty g then StringMap.empty
  else
    let (s,voisins) = StringMap.choose g in

    let s_colors = StringMap.find s c in

    try_first (fun col -> let g' = remove_vertex s g in
			  let c' = StringSet.fold (fun v gc -> remove_color col v gc) voisins  c in
			  StringMap.add s col (color g' c')   
      )
      s_colors


(*test*)
let graph =
  let g = add_edge "a" "c" StringMap.empty in
  let g = add_edge "a" "e" g in
  let g = add_edge "a" "d" g in
  let g = add_edge "b" "d" g in
  let g = add_edge "d" "e" g in
  let g = add_edge "d" "f" g in          
  add_edge "e" "f" g;;

to_dot graph;;

(*let c = init_colors graph 2 in
let color_test = color graph c (*Exception: Failed "Pas de chance! Aucun coloriage n'a ete trouve."*)*)

let c = init_colors graph 3;;

to_dot_init_colors c;;

let color_test = color graph c;;

to_dot_coloriage color_test;;
(*fin test*)
(*fin color*)

