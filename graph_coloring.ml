                                                
                                                (******************************)
                                                (*    Graphes non-orientés    *)
                                                (******************************)

(*
    * type: graph
    * Rôle: Définit un graphe dont tous les noeuds sont de type string. Il associe à chacun des noeuds un ensemble de noeuds
            (successeurs) de type string également, d'où le StringSet.t
*)
module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

type graph = StringSet.t StringMap.t


(*
    * fonction: to_dot
    * Rôle: Affiche le graphe non-orienté sous-forme d'associations (noeuds, successeurs) avec des arcs non-orientés.
    *  @param g => Le graphe cible de type "graph"
*)    
let to_dot g =
  let _ = Printf.printf "graph MonGraph {\n" in
  let _ =
    StringMap.iter (fun u us -> if(StringSet.is_empty us)
      then Printf.printf " %s -- Empty" u
      else StringSet.iter (fun v -> Printf.printf "  %s -- %s;\n" u v) us) g
  in
  Printf.printf "}\n"



(*
    * fonction: add_edge_aux
    * Rôle: Ajoute le noeud v aux successeurs du noeud u dans le graphe g, et retourne le nouveau graphe
    *  @param u => Le noeud dont l'ensemble de ses successeurs sera mis-à-jour
    *  @param v => Le successeur à associer à u
    *  @param g => Le graphe auquel u appartient
*)     
let add_edge_aux u v g = 
  let u_succ = try StringMap.find u g with Not_found -> StringSet.empty in
  let u_succ' = (StringSet.add v u_succ) in
  (StringMap.add u u_succ' g)     

(*
    * fonction: add_edge
    * Rôle: Crée un arc non-orienté dont les extrémités sont u et v, dans le graphe g, et retourne le nouveau graphe
    *  @param u => L'une des extrémités de l'arc
    *  @param v => L'autre extrémité de l'arc
    *  @param g => Le graphe cible
*)
let add_edge u v g =
  let g' = (add_edge_aux u v g) in
  (add_edge_aux v u g')

(*
    * Exemple de test
    * Rôle: Crée un graphe appelé "graph" ayant 3 arcs non-orientés
*)
let graph =
  let g = add_edge "a" "b" StringMap.empty in
  let g = add_edge "b" "c" g in
  add_edge "c" "a" g 
    

(*
    * fonction: remove_node
    * Rôle: Supprime le noeud v des successeurs du noeud u, dans le graphe g, et retourne le nouveau graphe
    *  @param u => Le noeud dont l'ensemble des successeurs sera mis-à-jour
    *  @param v => Le noeud à supprimer des successeurs de u
    *  @param g => Le graphe auquel u appartient
*)
let remove_node u v g = 
  let u_succ = try StringMap.find u g with Not_found -> StringSet.empty in   
  let u_succ' = (StringSet.remove v u_succ) in (StringMap.add u u_succ' g)

(*
    * fonction: remove_vertex
    * Rôle: Supprime le noeud u dans le graphe g, et ce en supprimant tous les arcs ayant u comme extrémité,
            puis, retourne le nouveau graphe
    *  @param u => Le noeud à supprimer
    *  @param g => Le graphe auquel u appartient
*)
let remove_vertex u g =
  let voisins_de_u = try StringMap.find u g with Not_found -> StringSet.empty in
  let g' = StringMap.remove u g in StringSet.fold (fun v a -> remove_node v u a) voisins_de_u g'
       


                                                (******************************)
                                                (*    Coloriages disponibles  *)
                                                (******************************)

(*Module IntSet*)
(*
    * module : Int
    * Rôle: Définit un module dont le type est un entier
*)
module Int = struct
  type t = int
  let compare = fun x y -> x - y
end
  
(*
    * module: IntSet
    * Rôle: Crée un module IntSet qui aura les mêmes attributs/fonctions que Set.Make avec les propriétés
            du module Int déclaré au-dessus
*)
module IntSet = Set.Make(Int) (* *)
(*fin module IntSet*)


(*
    * fonction: color_set_aux
    * Rôle: Crée un intervalle de type IntSet, dont les extrémités sont i et j
    *  @param i => L'extrémité inférieure de l'intervalle
    *  @param j => L'extrémité supérieure de l'intervalle
    *  @param acc => Le IntSet initial ou l'intervalle vide de départ
    * Exception : Lève une exception Failure si l'extrémité droite de l'intervalle est strictement inférieure à
                  son extrémité gauche
*)  
let rec color_set_aux i j acc = if i > j then raise (Failure "Echec! Votre argument doit etre >= 1") 
                                else if i = j then IntSet.add i acc 
                                else let acc1 = IntSet.add i acc in (color_set_aux (i+1) j acc1)

(*
    * fonction: color_set
    * Rôle: Crée un intervalle de type IntSet, dont les extrémités sont 1 et j
    *  @param j => L'extrémité supérieure de l'intervalle
*)
let color_set j = color_set_aux 1 j IntSet.empty


(*
    * type: disp_color
    * Rôle: Définit un map dont tous les noeuds sont de type string, mais qui associe à chacun de ces noeuds un ensemble
            de couleurs de type entier, d'où le IntSet.t
*)
type disp_color = IntSet.t StringMap.t 


(*
    * fonction: to_dot_init_colors
    * Rôle: Affiche le map sous-forme d'associations (noeuds, couleurs).
    *  @param g => Le map cible de type "disp_color"
*)
let to_dot_init_colors g =
  let _ = Printf.printf "MonGraph init colors {\n" in
  let _ =
    StringMap.iter (fun u us -> IntSet.iter (fun v -> Printf.printf "  %s -- %d;\n" u v) us) g
  in
  Printf.printf "}\n"


(*
    * fonction: add_edge_init_colors
    * Rôle: Ajoute la couleur v à celles associées au noeud u dans le map g, et retourne le nouveau map
    *  @param u => Le noeud dont l'ensemble des couleurs sera mis-à-jour
    *  @param v => La couleur à associer à u
    *  @param g => Le map auquel u appartient
*)    
let add_edge_init_colors u v g = 
 let u_succ = try StringMap.find u g with Not_found -> IntSet.empty in   
  let u_succ' = (IntSet.add v u_succ) in
  (StringMap.add u u_succ' g)


(*
    * fonction: init_colors
    * Rôle: Retourne un map qui associe à chaque sommet du graphe g l’ensemble des entiers de 1 à k
    *  @param g => Le graphe auquel u appartient
    *  @param k => L'extrémité supérieure de l'intervalle à associer aux noeuds de g
*)

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

(*
    * fonction: remove_color
    * Rôle: Supprime la couleur i de l'ensemble des couleurs associées au noeud v, dans le map c, et retourne le nouveau map
    *  @param v => Le noeud dont l'ensemble des couleurs sera mis-à-jour
    *  @param i => La couleur à supprimer des couleurs disponibles pour v
    *  @param c => Le map auquel v appartient
*)	    
let remove_color i v c =
  let v_succ = StringMap.find v c  in
    let v_succ' = (IntSet.remove i v_succ) in (StringMap.add v v_succ' c)
      


                                                (******************************)
                                                (*          Coloriage         *)
                                                (******************************)
(*
    * exception: Failed
    * Rôle: Cette exception se lève en affichant un message d'erreur personnalisé
    *  @param => Elle prend en paramètre un string qui est le message d'erreur à afficher
*)      
exception Failed of string


(*
    * fonction: try_first
    * Rôle: Retourne le premier résultat de l’application de f à un élément de s qui ne lève pas l’exception Failed
    *  @param f => La fonction à appliquer sur les éléments de s
    *  @param s => Le IntSet à partir duquel try_first choisit les éléments pour leur appliquer la fonction f
    * Exception : Lève une exception Failed si "s" est vide
*)
let rec try_first f s = 
  let exception_message = "Pas de chance! Aucun coloriage n'a ete trouve." in
    if s = IntSet.empty then raise (Failed exception_message)
    else let i = IntSet.choose s in
      try f i with Failed _ -> let s' = IntSet.remove i s in try_first f s'

(*
    * Exemple de test
    * Rôle: Crée un IntSet ayant les valeurs de 1 à 4, puis on lui on applique 3 fonctions anonymes différentes pour couvrir
            les différents cas.
*)
let s =                             
  let s0 = IntSet.add 1 IntSet.empty in
  let s0 = IntSet.add 2 s0 in
  let s0 = IntSet.add 3 s0 in
  IntSet.add 4 s0

let try_first_test = try_first (fun x -> if x < 4 then raise (Failed "Echec") else x) s

let try_first_test = try_first (fun x -> if x < 5 then raise (Failed "Echec") else x) s

let try_first_test = try_first (fun x -> if (x=1 || x=2) then raise (Failed "ERREUR 404") else x) s


(*
    * type: coloring
    * Rôle: Définit un map dont tous les noeuds sont de type string, mais qui associe à chacun de ces noeuds une et une seule
            couleur de type entier, d'où le int.
*)
type coloring = int StringMap.t

(*
    * fonction: to_dot_coloriage
    * Rôle: Affiche le map sous-forme d'associations (noeud, couleur).
    *  @param g => Le map cible de type "coloring"
*)
let to_dot_coloriage g =
  let _ = Printf.printf "MonGraph coloré {\n" in
  let _ =
    StringMap.iter (fun u us -> Printf.printf " %s -- %d \n" u us) g
  in
  Printf.printf "}\n"


(*
    * fonction: color
    * Rôle: Attribue une couleur parmi celles disponibles pour chacun des noeuds dans le map c, à chacun des sommets
            du graphe g de manière que deux sommets reliés par une arête soient de couleurs différentes, et retourne un map
            coloré de type "coloring"
    *  @param g => Le graphe de type "graph"
    *  @param c => Le graphe de type "disp_color"
*)
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


(*
    * Exemple de test
    * Rôle: - Créer le graphe présenté sur l'énoncé
            - L'afficher
            - Créer un map "c" qui associe 3 couleurs à tous les noeuds
            - L'afficher
            - Appliquer la fonction "color" au graphe et map créé précédemment
            - Afficher le coloriage
*)
let graph =
  let g = add_edge "a" "c" StringMap.empty in
  let g = add_edge "a" "e" g in
  let g = add_edge "a" "d" g in
  let g = add_edge "b" "d" g in
  let g = add_edge "d" "e" g in
  let g = add_edge "d" "f" g in          
  add_edge "e" "f" g;;

to_dot graph;;

(* Cas d'aucun coloriage trouvé :
let c = init_colors graph 2 in
let color_test = color graph c
*)

let c = init_colors graph 3;;

to_dot_init_colors c;;

let color_test = color graph c;;

to_dot_coloriage color_test;;

(*
    *  fonction: couleur
    *  Rôle: attribuer une couleur pour chaque entier entre 1 et 15 (possibilité d'attribuer 15 couleurs différentes aux noeuds)
    *  @param code => un entier 
    *  Return => une chaine de caractères  
*)

let couleur code = match code with
    0 -> failwith "Le code doit etre entre 1 et 15"
  | 1 -> "red"
  | 2 -> "chartreuse"
  | 3 -> "cyan"
  | 4 -> "gold"
  | 5 -> "green"
  | 6 -> "indigo"
  | 7 -> "aliceblue"
  | 8 -> "darkgoldenrod1"
  | 9 -> "grey"
  | 10 -> "magenta"
  | 11 -> "navyblue"
  | 12 -> "peru"
  | 13 -> "seagreen4"
  | 14 ->  "yellow4"
  | 15 -> "tomato3"
  | _ -> failwith "Le code doit etre entre 1 et 15"


(*
  QUESTION 13 :

    *  fonction: formatdot 
    *  Rôle : A partir d'un graphe et un coloriage des noeuds on affiche sur la sortie standard un graph au format DOT avec les différentes couleurs
    *  @param g : graph de type 'graph'
    *  @param c : coloriage de type coloring
    *  Return => unit
*)
    
let formatdot g c =
  let col = color g c in
  let _ = Printf.printf "graph mon_graph{\n" in
  let _ = StringMap.iter (fun n colnumber -> Printf.printf " %s [style=\"filled\", color=%s]\n" n (couleur colnumber)) col in
    let _ = StringMap.iter (fun u us -> StringSet.iter (fun x -> Printf.printf " %s -- %s ;\n" u x) us) g in
      Printf.printf "}\n";;
