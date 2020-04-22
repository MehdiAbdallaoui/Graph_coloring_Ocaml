type graph

val to_dot : StringSet.t StringMap.t -> unit

val add_edge_aux : StringMap.key -> StringSet.elt -> StringSet.t StringMap.t -> StringSet.t StringMap.t

val add_edge : StringMap.key -> StringSet.elt -> StringSet.t StringMap.t -> StringSet.t StringMap.t

val remove_edge : StringMap.key -> StringSet.elt -> StringSet.t StringMap.t -> StringSet.t StringMap.t

val remove_vertex : StringMap.key -> StringSet.t StringMap.t -> StringSet.t StringMap.t

(*A VERIFIER : interface du module Int*)
module Int : 
sig 
  type t 
  val compare : int -> int -> int
end

(*TODO : interface du module IntSet si c'est faisable*)

val color_set_aux : IntSet.elt -> IntSet.elt -> IntSet.t -> IntSet.t

val color_set : IntSet.elt -> IntSet.t

type disp_color

val to_dot_init_colors : IntSet.t StringMap.t -> unit

val add_edge_init_colors : StringMap.key -> IntSet.elt -> IntSet.t StringMap.t -> IntSet.t StringMap.t

val init_colors : 'a StringMap.t -> IntSet.elt -> IntSet.t StringMap.t

val remove_color : IntSet.elt -> StringMap.key -> IntSet.t StringMap.t -> IntSet.t StringMap.t

val try_first : (IntSet.elt -> 'a) -> IntSet.t -> 'a

type coloring

