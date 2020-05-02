type graph

val to_dot : graph -> unit

val add_edge_aux : string -> string -> graph -> graph

val add_edge : string -> string -> graph -> graph

val remove_edge : string -> string -> graph -> graph

val remove_vertex : string -> graph -> graph

module Int : 
sig 
  type t 
  val compare : int -> int -> int
end

module IntSet :
  sig
    type t
  end

val color_set_aux : int -> int -> IntSet.t -> IntSet.t

val color_set : int -> IntSet.t

type disp_color

val to_dot_init_colors : disp_color -> unit

val add_edge_init_colors : string -> int -> disp_color -> disp_color

val init_colors : graph -> int -> disp_color

val remove_color : int -> string -> disp_color -> disp_color

val try_first : (int -> 'a) -> IntSet.t -> 'a

type coloring

val color : graph -> disp_color -> coloring