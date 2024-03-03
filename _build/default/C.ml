
let foi x = float_of_int x 
let iof x = int_of_float x 

let r_k = ref 1.

let k () = !r_k

let r_l0 = ref 25.

let r_fact_dist_max = ref 1.

let fact_dist_max () = !r_fact_dist_max

let l0 () = !r_l0

let r_nb_particles = ref 1

let nb_particles () = !r_nb_particles

let r_reset = ref (fun () -> ())

let r_update_cells = ref (fun () -> ())

let update_cells() = !r_update_cells()

let reset() = !r_reset()

let r_f_ressort_selecter = ref 0

let a_f_ressort_selecter = [|"Linéaire";"carré";"inverse";"inverse carré"|]

let voir_ressorts = ref false

let voir_liens = ref false

let voir_chunks = ref false

let run_simulation = ref false 

let manipule_polygones = ref false 

let was_holding_polygons = ref false 

let spoon_activated = ref false 

let arrows = ref false 

let arrow_size = ref 0.2

let persistance_frames = ref 10 

let spoon_strength = ref 50. 

let spoon_radius = ref 100. 

let polygon_tolerance = 20.

let last_polygon_selected = ref 0 

let last_polygon_corner_selected = ref 0

let r_a = ref 0.05

let r_a_solid = ref 1. 

let r_k_solid = ref 100.

let r_delta_t = ref 60.

let r_color_factor = ref 5.

let delta_t() = 1./. !r_delta_t

let a () = !r_a

let r_g = ref 10.

let g () = !r_g

let h = 900 
let w = 1700

let speed_to_color speed = 
	Raylib.color_from_hsv (Raylib.Vector3.create (245. -. !r_color_factor*.speed) 1. (0.8))

let speed_to_color_with_fade speed fade = 
	Raylib.color_from_hsv (Raylib.Vector3.create (245. -. !r_color_factor*.speed) fade 0.8)


