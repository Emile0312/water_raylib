(*
=============================================================================

--------------------------DEFINITION DES TYPES-------------------------------

=============================================================================
*)


open Raylib

type point = int * int
type vect = float * float
type mat2 = vect * vect

let ($+.) (a : vect)  (b : vect) : vect = 
	match a,b with 
	| (x1,y1),(x2,y2) -> ((x1 +. x2),(y1 +. y2))

let ($-.) (a : vect)  (b : vect) : vect = 
	match a,b with 
	| (x1,y1),(x2,y2) -> ((x1 -. x2),(y1 -. y2))

let ( $*.) (a : float)  (b : vect) : vect = 
	match b with 
	| (x2,y2) -> (a*.x2,a*.y2) 

let ($.) (a : vect)  (b : vect) : float = 
	let (a1,a2),(b1,b2) = a,b in a1*.b1 +. a2*.b2

let ( $/.) (a : float)  (b : vect) : vect = 
	match b with 
	| (x2,y2) -> (x2/.a,y2/.a) 

let det (m : mat2) = 
	let (a,c),(b,d) = m in 
	a*.d -. b*.c

let inverse (m : mat2) = 
	let (a,c),(b,d) = m in 
	let delta = det m in   
		(delta $/.(d,-.c)),(delta $/.(-.b,a))

let (&.) (*produit matriciel MN*) ( m : mat2) (n : mat2) : mat2 = 
	let (a1,c1),(b1,d1) = m in 
	let (a2,c2),(b2,d2) = n in 
	((a1*.a2 +. b1*.c2),(c1*.a2 +. d1*.c2)),((a1*.b2 +. b1*.d2),(c1*.b2 +. d1*.d2))

let (&*.) (*produit matriciel MV avec v vecteur*) ( m : mat2) (v : vect) : vect = 
	let (a,c),(b,d) = m in 
	let x,y = v in 
	(a*.x +. b*.y),(c*.x +. d*.y)

let orthogonal (a1,b1) (a2,b2) = 
	(b2-.a2),(a1-.b1)



type particle = 
{ 
mutable x : float;
mutable y : float;
mutable vel_x : float;
mutable vel_y : float;
mutable acc_x : float;
mutable acc_y : float;
mutable mass : float;
mutable linked_particles : particle list;
mutable force_multiplier : float;
mutable force_function : int ref;
(*plan : mutable special_force_multiplier : float; *)
}

type particle_cells = 
{
	mutable size : int; (*la taille du carré h - en nb de case sur une ligne / colone *)
	mutable nb_cells : int; (*la taille totale h*h - raccourci *)
	mutable hash : int array;
	mutable index : int array;
	mutable statend : (int*int) array;
}

type solid_particle = 
{
	mutable x : float;
	mutable y : float;
	mutable vel_x : float;
	mutable vel_y : float;
	mutable acc_x : float;
	mutable acc_y : float;
	mutable acc2_x : float;
	mutable acc2_y : float;
	mutable mass : float;
	mutable fixed : bool;
	mutable fixed_links : (int * float * bool) list;
	(*other solid_particle, float : lenght, bool : is_collision_on *)
(*plan : mutable special_force_multiplier : float; *)


}

type solid = solid_particle array

(*
=============================================================================

--------------------------GESTION DE LA GRILLE-------------------------------

=============================================================================
*)

let create_grid () = 
	let d = 2.*.C.l0() *. C.fact_dist_max () in 
	let size = C.iof (1000./.d) + 1 in 
	{
	size = size;
	nb_cells = size*size;
	hash = Array.make (C.nb_particles()) 0;
	index = Array.init (C.nb_particles()) (fun i -> i);
	statend = Array.make (size*size) (0,0) ;
	}

let update_grid_particles (pearls : particle array) (grid : particle_cells) = 
	let case (p : particle) = 
		let max_dist = 2.*.C.l0()*.C.fact_dist_max() in 
		let i,j = C.iof ((p.x)/.max_dist),C.iof ((p.y)/.max_dist) in 
		j*grid.size + i
	in

	(*on actuallise les hash - on note que les hashs sont pas loin les un des autres
	 -- hash est 'presque' trié *)
	for i = 0 to Array.length grid.hash - 1 do 
		grid.hash.(i) <- case pearls.(grid.index.(i))
	done;

	(*tri de SHELL !! *)

	let tab = [|701; 301; 132; 57; 23; 10; 4; 1|] in 
	let swap t i j = 
		let c = t.(i) in 
		t.(i) <- t.(j);
		t.(j) <- c
	in 
	(*je sais ça à l'air n⁴ mais c'est n^1.25*)
	let n = Array.length grid.hash in 
	for m = 0 to Array.length tab - 1 do 
		for r = 0 to m - 1 do 
			let i = ref (m+r) in 
			while !i < n do 
				let j = ref ( !i) in 
				while !j > 0 && grid.hash.(!j) < grid.hash.(!j-m) do 
					swap grid.hash !j (!j-m);
					swap grid.index !j (!j-m);
					j := !j - m
				done;
				i := !i + m
			done;
		done;
	done;
	(* degbug remplacé par du dégub visuel
	Array.iter (fun a -> Printf.printf "%d " a) grid.hash;
	Printf.printf "\n";
	Array.iter (fun a -> Printf.printf "%d " a) grid.index;
	Printf.printf "\n"; *)
	(*et on update statend : *)
	let start = ref 0 in 
	for i = 0 to grid.nb_cells - 1 do
		let ended = ref !start in 
		(*on cherche entre start et end-1*)
		while !ended < Array.length grid.hash && grid.hash.(!ended) = i do 
			ended := !ended +1;
		done;
		grid.statend.(i) <- (!start,!ended);
		start := !ended;
	done;
	(*Array.iter (fun (a,b) -> Printf.printf "(%d %d) | " a b) grid.statend;
	Printf.printf "\n"*)

type mur = vect * vect


(*
=============================================================================

--------------------------FORCES DE RESSORT ---------------------------------

=============================================================================
*)

let dist (p : particle) (q : particle) : float =
	sqrt ((p.x-.q.x)*.(p.x-.q.x) +. (p.y-.q.y)*.(p.y-.q.y))

let dist_solid (p : solid_particle) (q : solid_particle) : float =
	sqrt ((p.x-.q.x)*.(p.x-.q.x) +. (p.y-.q.y)*.(p.y-.q.y))

let norm ((x,y) : vect) = sqrt (x*.x +.y*.y)

let squared_norm ((x,y) : vect) = x*.x +.y*.y

let squared_dist (p : particle) (q : particle) : float =
	(p.x-.q.x)*.(p.x-.q.x) +. (p.y-.q.y)*.(p.y-.q.y)

let unitaire (a : vect) =
	(norm a) $/. a



let f_ressort_carre (p : particle) (q : particle) : vect = 
	(*force exercée par q sur p*)
	let d = dist p q in (*tentative perso à améliorer*)
	q.force_multiplier*.((!C.r_k/.(3.*. !C.r_l0))*.d*.d -. (4.*. !C.r_k/.(3.))*.d +. !C.r_k*. !C.r_l0)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y))

let f_ressort_lineaire (p : particle) (q : particle) : vect = 
	(*force exercée par q sur p*)
	let d = dist p q in 
	q.force_multiplier*. !C.r_k*.( !C.r_l0-.d) (*modèle linéaire du ressort*)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y)) 

let f_ressort_lineaire_solid (p : solid_particle) (q : solid_particle)  (l0 : float) : vect = 
	(*force exercée par q sur p*)
	let d = dist_solid p q in 
	 !C.r_k_solid*.(l0 -.d) (*modèle linéaire du ressort*)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y)) 

let f_ressort_inverse (p : particle) (q : particle) : vect = 
	(*force exercée par q sur p*)
	let d = dist p q in 
	q.force_multiplier*.(!C.r_k*. !C.r_l0*. !C.r_l0/.d -. !C.r_k) (*du 1/d classique*)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y)) 

let f_ressort_inverse_squared (p : particle) (q : particle) : vect = 
	(*force exercée par q sur p*)
	let d2 = squared_dist p q in 
	q.force_multiplier*.(!C.r_k*. !C.r_l0 *. !C.r_l0 *. !C.r_l0/.(2. *.d2) -. (!C.r_k*. !C.r_l0)/.2.) (*du 1/d classique*)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y)) 

let f_amortissement (p : particle) (q : particle) : vect = 
	(*à changer !!!*)
	if norm (p.vel_x -. q.vel_x, p.vel_y -. q.vel_y) <> 0. then 
	let res = ((p.vel_x -. q.vel_x),(p.vel_y -. q.vel_y)) in 
	let u = (p.x-.q.x),(p.y-.q.y) in 
	let proj = (squared_norm u) $/. ((res $. u) $*. u) in 
	(-. !C.r_a)   $*. proj
	else (0.,0.)

let f_amortissement_solid (p : solid_particle) (q : solid_particle) : vect = 
	if norm (p.vel_x -. q.vel_x, p.vel_y -. q.vel_y) <> 0. then 
	let res = ((p.vel_x -. q.vel_x),(p.vel_y -. q.vel_y)) in 
	let u = (p.x-.q.x),(p.y-.q.y) in 
	let proj = (squared_norm u) $/. ((res $. u) $*. u) in 
	(-. !C.r_a_solid)   $*. proj
	else (0.,0.)

let f_spoon_gaussienne (p : particle) (q : particle) : vect = 
	(*force exercée par q sur p*)
	let d2 = squared_dist p q in 
	(q.force_multiplier*.exp(-.(d2/. (!C.spoon_radius*. !C.spoon_radius)))) (*... une gaussienne.*)
	 $*. (unitaire (p.x -. q.x, p.y -. q.y)) (*seulement pour la cuillière*)


let tableau_f_ressort = [|f_ressort_lineaire;f_ressort_carre;f_ressort_inverse;f_ressort_inverse_squared;
(*à partir d'ici : fcts non séléctionnables sur l'ui *)
	f_spoon_gaussienne|]

(*
=============================================================================

--------------------------GESTION DES SOLIDES  ------------------------------

=============================================================================
*)

let triangle = 
[| {
	x = 540.;
	y = 500.;
	vel_x = 0.;
	vel_y =	0.;
	acc_x = 0.;
	acc_y = 0.;
	acc2_x = 0.;
	acc2_y = 0.;
	mass = 1.;
	fixed = false;
	fixed_links = [(1,40.,true);(2,40.,true)]

};
{
	x = 500.;
	y = 540.;
	vel_x = 0.;
	vel_y =	0.;
	acc_x = 0.;
	acc_y = 0.;
	acc2_x = 0.;
	acc2_y = 0.;
	mass = 1.;
	fixed = false;
	fixed_links = [(0,40.,true);(2,40.,true)]

};
{
	x = 540.;
	y = 540.;
	vel_x = 0.;
	vel_y =	0.;
	acc_x = 0.;
	acc_y = 0.;
	acc2_x = 0.;
	acc2_y = 0.;
	mass = 1.;
	fixed = false;
	fixed_links = [(0,40.,true);(1,40.,true)]

}
|]

let solides = [|triangle|]

let update_solid_acceleration s solid = 
	let ax, ay = List.fold_left 
	(fun acc (k, l0, _) -> acc 
		$+. (f_ressort_lineaire_solid s solid.(k) l0 ) 
		$+. (f_amortissement_solid s solid.(k) )
	) 
	(s.acc2_x,s.acc2_y) 
	s.fixed_links in 
	s.acc_x <- ax;
	s.acc_y <- ay;
	s.acc2_x <- 0.;
	s.acc2_y <- 0.

let update_solid_position s polys solid (pearls : particle array) grid = 
	(*même fonction que pour les particules parce que flemme *)
	
	let qx,qy = ref (s.vel_x*.C.delta_t()), ref (s.vel_y*.C.delta_t()) in  
	let last_hit_nb = ref (-1) in
	let last_hit_side = ref (-1) in  

	let is_in_triangle x pos1 pos2 pos3 = 
		let systeme = (pos2 $-. pos1),(pos3 $-. pos1) in 
		let s,t = (inverse systeme) &*. (x $-. pos1) in 
		(s <= 1. && s >= 0. && t <= 1. && t >= 0.
		&& s +. t <= 1.)
	in
	let sign (ax,ay) (bx,by) (cx,cy) = 
				(ax -. cx)*. (by -. cy) -. (bx -. cx)*.(ay -. cy)
	in

	let triangle pos1 pos2 pos3 s2 = 
		(*debuging !!!*)
		draw_triangle (Raylib.Vector2.create ((C.foi C.h)*.(fst pos1)/.1000.) ((C.foi C.h)*.(snd pos1)/.1000.))
		(Raylib.Vector2.create ((C.foi C.h)*.(fst pos2)/.1000.) ((C.foi C.h)*.(snd pos2)/.1000.))
		(Raylib.Vector2.create ((C.foi C.h)*.(fst pos3)/.1000.) ((C.foi C.h)*.(snd pos3)/.1000.))
		Color.pink;
		for i = 0 to Array.length pearls - 1 do 
			if is_in_triangle (pearls.(i).x,pearls.(i).y) pos1 pos2 pos3 then 
			begin
				let x = (pearls.(i).x,pearls.(i).y) in 
				let u = pos1 $-. pos3 in  
				let h = pos3 $+. 
				((squared_norm (pos1 $-. pos3)) $/. 
					(((x $-. pos3) $. (pos1 $-. pos3)) $*. (pos1 $-. pos3))) in 
				let th = (norm (h $-. pos3)) /. (norm (pos1 $-. pos3)) in 
				let systeme = (pos3 $-. pos2, h $-. x) in 
				if det systeme != 0. && th >= 0. && th <= 1. then (
					(*let (u,t) = (inverse systeme) &*. (h $-. pos3) in 
					let new_x,new_y = h $+. (t $*. (h $-. x) ) in 
					let vb = norm (th *. s.vel_x +. (1. -. th) *. s2.vel_x ,
					th *. s.vel_y +. (1. -. th) *. s2.vel_y ) in 
					let va = norm (pearls.(i).vel_x,pearls.(i).vel_y) in 
					let va_x = pearls.(i).vel_x in 
					let va_y = pearls.(i).vel_y in 
					let mb = ((s.mass +. s2.mass)/.2.)  in 
					let ma = pearls.(i).mass in 
					let energy = mb *. vb *. vb +. ma *. va *. va in 
					let momentum_x = ma *.  +. mb *. vb in 
					let va_prime1 = (va *. momentum /. (mb +. ma)) +. sqrt( 
						(va *. momentum /. (mb +. ma))*. (va *. momentum /. (mb +. ma))
						-. (
							momentum*.momentum /. (ma*. (mb +. ma))
							-. energy*.mb /. (ma*.(mb +. ma))
						)
					) in 
					let va_prime2 = (va *. momentum /. (mb +. ma)) -. sqrt( 
						(va *. momentum /. (mb +. ma))*. (va *. momentum /. (mb +. ma))
						-. (
							momentum*.momentum /. (ma*. (mb +. ma))
							-. energy*.mb /. (ma*.(mb +. ma))
						)
					) in 
					let va_prime = if Float.abs (va_prime1 -. va) > Float.abs (va_prime2 -. va) then va_prime1 else va_prime2 in 
					let vb_prime = (momentum +. va_prime*.ma)/.mb in 
					let vel_dir_x,vel_dir_y = unitaire ( 
						(ma *. pearls.(i).vel_x, ma *. pearls.(i).vel_y) $+.
						(mb *. th *. s.vel_x, mb *.th *. s.vel_y) $+.
						(mb *. (1. -. th) *. s2.vel_x, mb *. (1. -. th) *. s2.vel_y)) 
					in 

					pearls.(i).x <- new_x;
					pearls.(i).y <- new_y;
					pearls.(i).vel_x <- va_prime *. vel_dir_x;
					pearls.(i).vel_y <- va_prime *. vel_dir_y;
					s.vel_x <-  vb_prime *. th *. vel_dir_x;
					s.vel_y <-  vb_prime *. th *. vel_dir_y;
					s2.vel_x <-  vb_prime *. (1. -. th) *. vel_dir_x;
					s2.vel_y <-  vb_prime *. (1. -. th) *. vel_dir_y;*)
					(*bonne tentative mais le système est vectoriel ET irrésoluble ...*)

					let (u,t) = (inverse systeme) &*. (h $-. pos3) in 
					let new_x,new_y = h $+. (t $*. (h $-. x) ) in 
					let vb = (th *. s.vel_x +. (1. -. th) *. s2.vel_x ,
					th *. s.vel_y +. (1. -. th) *. s2.vel_y ) in 
					let va = (pearls.(i).vel_x,pearls.(i).vel_y) in 
					let xa = (pearls.(i).x,pearls.(i).y) in 
					let xb = new_x,new_y in 
					let mb = ((s.mass +. s2.mass)/.2.)  in 
					let ma = pearls.(i).mass in 
					let energy = mb *. (squared_norm vb) +. ma *. (squared_norm va)  in 
					let momentum = (ma $*. va) $+. (mb $*. vb) in 
					let orthog =  unitaire (orthogonal pos3 pos2) in
					let pass_mat = inverse ( unitaire (pos2 $-. pos3),orthog) in 
					let mat_trans_totale = pass_mat &. ((1.,0.),(0.,-.1.)) &. (inverse pass_mat) in

					(*) 
					let x = orthog in 
					let kscalx = (x $. momentum) /. (norm x) in 
					let delta = kscalx *. kscalx -. 4. *. (
							mb *. (norm momentum) -. energy) /. (2. *. ma *. (norm x)) 
					in 
					(if delta < 0. then failwith "delta negatif, what.");
					let lamb = 0.5 *. ( -. kscalx +. sqrt (delta)) in

					let nvs_x,nvs_y = mb $/. (momentum $-. ((ma *. lamb ) $*. x)) in 
					let nvp_x,nvp_y = lamb $*. x in  *)

					let va_prime_x,va_prime_y = va $-. ((
						(2.*.mb/.(ma+.mb)) *.((va $-. vb) $.(xa $-. xb)) /.(squared_norm (C.delta_t() $*. va))
					) $*. (xa $-. xb)) in 
					let vb_prime_x,vb_prime_y = vb $-. ((
						(2.*.ma/.(ma+.mb)) *.((vb $-. va) $.(xb $-. xa)) /.(squared_norm (C.delta_t() $*. va))
					) $*. (xb $-. xa)) in 

					pearls.(i).vel_x <- va_prime_x;
					pearls.(i).vel_y <- va_prime_y;

					s.vel_x <-  th *. vb_prime_x;
					s.vel_y <-  th *. vb_prime_y;
					s2.vel_x <-  (1. -. th) *. vb_prime_x;
					s2.vel_y <-  (1. -. th) *. vb_prime_y;

					pearls.(i).x <- new_x;
					pearls.(i).y <- new_y;
				)
			end
		done
	in
	let rec process_collision unit : unit = 
		
		let nearest_collision = ref None in 
		let min_t = ref 1. in 
		let final_u = ref 1. in 
		for j = 0 to Array.length polys - 1 do 
			for i = 0 to Array.length polys.(j) - 2 do 
				if not (j = !last_hit_nb && i = !last_hit_side) then
				begin
				let a1,a2 = polys.(j).(i) in 
				let b1,b2 = polys.(j).(i+1) in 
				let mat = (!qx, !qy),(b1-.a1,b2-.a2) in 
				if det mat <> 0. then 
				let t,u = (inverse mat) &*. ((b1,b2) $-. (s.x,s.y)) in 
				if u >= 0. && u <= 1. && t >= 0. 
				&& t <= !min_t then (
					nearest_collision := Some ((a1,a2),(b1,b2),i,j);
					min_t := t;
					final_u := u;
				)
				else ()
				else ()
				end
			done;
		done;
		match !nearest_collision with 
		| None -> (
			let old_pos = (s.x,s.y) in 
			let new_pos = (s.x +. !qx,s.y +. !qy) in 
			(*faut trouver un triangle / faire un pour chaque triangle
			triangle : ancienne position, nouvelle position, + lien
			Une update par lien *)
			(*

			let is_in_triangle p pos1 pos2 pos3 = 
				let d1,d2,d3 = sign p pos1 pos2, sign p pos2 pos3, sign p pos3 pos1 in 
				not ( ((d1 < 0.) || (d2 < 0.) || (d3 < 0.)) 
					&& ((d1 > 0.) || (d2 > 0.) || (d3 > 0.)) )
			in *)

			
			List.iter (fun (nb,_,cond) -> 
				if cond then (
				if sign new_pos old_pos  (solid.(nb).x,solid.(nb).y) < 0. then
				 triangle new_pos old_pos  (solid.(nb).x,solid.(nb).y) solid.(nb) 
				else triangle old_pos new_pos  (solid.(nb).x,solid.(nb).y) solid.(nb)))
			s.fixed_links;
			s.x <- s.x +. !qx; 
			s.y <- s.y +. !qy
		)
		| Some (a,b,i,j) -> (
			let (a1,a2),(b1,b2) = a,b in 
			let h1,h2 = ( !final_u $*. a) $+. ((1.-. !final_u) $*.b) in 
			let orthog =  (b2-.a2),(a1-.b1) in
			let pass_mat = inverse ((b $-. a),orthog) in 
			let mat_trans_totale = pass_mat &. ((1.,0.),(0.,-.1.)) &. (inverse pass_mat) in 
			let p_x, p_y = ((1.-. !min_t)/. !min_t) $*. (mat_trans_totale &*. ((h1 -. s.x),(h2 -. s.y))) in 
			let new_vel_x, new_vel_y = mat_trans_totale &*. (s.vel_x,s.vel_y) in 
			let old_pos = (s.x,s.y) in 
			let new_pos = (h1,h2) in 
			List.iter (fun (nb,_,cond) -> 
				if cond then (
				if sign new_pos old_pos  (solid.(nb).x,solid.(nb).y) < 0. then
				 triangle new_pos old_pos  (solid.(nb).x,solid.(nb).y) solid.(nb) 
				else triangle old_pos new_pos  (solid.(nb).x,solid.(nb).y) solid.(nb)))
			s.fixed_links;
			qx := p_x;
			qy := p_y;
			s.vel_x <- (1. -. !C.r_a) *. new_vel_x; 
			s.vel_y <- (1. -. !C.r_a) *.new_vel_y;
			solid.(i).acc_x <- 0.;
			solid.(i).acc_y <- 0.;
			last_hit_nb := j;
			last_hit_side := i;
			process_collision();
		)
	in	
	process_collision();
	(if (s.x > 1000. || s.x < 0. || s.y > 1000. || s.y < 0.)
	then 
	(	
		let new_x = 50. +. Random.float (50.) in 
		let new_y = 50. +. Random.float (50.) in 
		let dec_x = s.x -. new_x in 
		let dec_y = s.y -. new_y in 

		for i = 0 to Array.length solid -1 do 
			solid.(i).x <- solid.(i).x -. dec_x;
			solid.(i).y <- solid.(i).y -. dec_y;
			solid.(i).vel_x <- 0.;
			solid.(i).vel_y <- 0.;
			solid.(i).acc_x <- 0.;
			solid.(i).acc_y <- 0.;
			solid.(i).acc2_x <- 0.;
			solid.(i).acc2_y <- 0.;
		done
	))

let update_solid_velocities s = 
	s.vel_x <- s.vel_x +. (s.acc_x/.s.mass)*.C.delta_t();
	s.vel_y <- s.vel_y +. (s.acc_y/.s.mass +. !C.r_g)*. C.delta_t()

let update_solid solid polys pearls grid = 
	for i = 0 to Array.length solid -1 do (*à parraléliser une fois en C*)
		update_solid_acceleration solid.(i) solid;
	done;
	for i = 0 to Array.length solid -1 do 
		(*important qu'on le fasse après - sinnon problèmes*)
		update_solid_velocities solid.(i);
		update_solid_position solid.(i) polys solid pearls grid;
	done


(*
=============================================================================

--------------------------GESTION DES PARTICULES ----------------------------

=============================================================================
*)

let create_particles nb_particles = 
	let max_dist = C.fact_dist_max()*.C.l0() in
	let taille_carre = nb_particles |> C.foi |> sqrt |> C.iof in 
	Array.init nb_particles (fun i -> {
		x = Random.float (1000.);
		y = Random.float (1000.);
		vel_x = 0.;
		vel_y = 0.;
		(*l'accélération est pas utilisé dans les calculs
		mais elle doit être stocké puis aplliquée*)
		acc_x = 0.;
		acc_y = 0.;
		mass = 1.;
		linked_particles = []; 
		force_multiplier = 1.;
		force_function = C.r_f_ressort_selecter
		}
	)

let update_particle_links grid pearls k m polys = 

	pearls.(k).linked_particles <- []; (*très important !!*)

	let dist_max = C.fact_dist_max()*.C.l0() in 
	let update_at_cell i1 j1 = 
		if i1 >= 0 && i1 < grid.size && j1 >= 0 && j1 < grid.size then begin
		let start,ended = grid.statend.(j1*grid.size + i1) in 
		for i = start to ended - 1 do 
			(if !C.voir_liens then begin
			Raylib.draw_line 
			(C.iof ((C.foi C.h)*.pearls.(k).x/.1000.))
			(C.iof ((C.foi C.h)*.pearls.(k).y/.1000.)) 
			(C.iof ((C.foi C.h)*.pearls.(grid.index.(i)).x/.1000.))
			(C.iof ((C.foi C.h)*.pearls.(grid.index.(i)).y/.1000.)) 
			Color.pink
			end);
			if dist pearls.(k) pearls.(grid.index.(i)) <= dist_max
				&& k <> grid.index.(i) then 
				pearls.(k).linked_particles <- pearls.(grid.index.(i)) :: pearls.(k).linked_particles
		done
		end else ()
	in

	let grid_pas = 2.*.dist_max in 
	let i0,j0 = C.iof (pearls.(k).x/.grid_pas),C.iof (pearls.(k).y/.grid_pas) in
	(*code pour cercher en la case i :*)
	update_at_cell i0 j0;
	if grid_pas*.(C.foi i0) +. grid_pas/.2. > pearls.(k).x then begin(*on cherche en la case i+1*)
		update_at_cell (i0-1) j0; 
		if grid_pas*.(C.foi j0) +. grid_pas/.2. > pearls.(k).y then 
			(update_at_cell (i0) (j0-1);
			update_at_cell (i0-1) (j0-1))
		else 
			(update_at_cell (i0) (j0+1);
			update_at_cell (i0-1) (j0+1))
	end
	else begin
		update_at_cell (i0+1) j0; 
		if grid_pas*.(C.foi j0) +. grid_pas/.2. > pearls.(k).y then 
			(update_at_cell (i0) (j0-1);
			update_at_cell (i0+1) (j0-1))
		else 
			(update_at_cell (i0) (j0+1);
			update_at_cell (i0+1) (j0+1))
	end;
	(*gestion des murs*)
	(*ancient code; crée un lien ressort en 1/d²
	let collision_mur (a: vect) (b : vect) = 
		let x = (pearls.(k).x,pearls.(k).y) in
		let k_ab =  ((x $-. a) $. (unitaire (b $-. a)))/. (norm (b $-. a)) in 
		(*représente à combien h est proche de a ou b, entre 0 et 1 si h est sur le segment*)
		if k_ab >= 0. && k_ab <= 1. then begin 
			let projtx,projty = a $+. ( k_ab $*. (b $-. a) ) in 
			let h = {
			x = projtx;
			y = projty;
			vel_x = 0.;
			vel_y = 0.;
			acc_x = 0.;
			acc_y = 0.;
			mass = 1.;
			linked_particles = []; 
			force_multiplier = 1.;
			force_function = (ref 3);
			} in 
			(if dist h pearls.(k) < C.fact_dist_max()*.C.l0() then 
				pearls.(k).linked_particles <- h :: pearls.(k).linked_particles)
			end
		else if k_ab < 0. then begin 
			let a1,a2 = a in 
			let h = {
			x = a1;
			y = a2;
			vel_x = 0.;
			vel_y = 0.;
			acc_x = 0.;
			acc_y = 0.;
			mass = 1.;
			linked_particles = []; 
			force_multiplier = 1.;
			force_function = (ref 3);
			} in 
			(if dist h pearls.(k) < C.fact_dist_max()*.C.l0() then 
				pearls.(k).linked_particles <- h :: pearls.(k).linked_particles)
			end
		else begin
			let b1,b2 = b in 
			let h = {
			x = b1;
			y = b2;
			vel_x = 0.;
			vel_y = 0.;
			acc_x = 0.;
			acc_y = 0.;
			mass = 1.;
			linked_particles = []; 
			force_multiplier = 1.;
			force_function = (ref 3);
			} in 
			(if dist h pearls.(k) < C.fact_dist_max()*.C.l0() then 
				pearls.(k).linked_particles <- h :: pearls.(k).linked_particles)
			end
	in  

	for i = 0 to Array.length m - 1 do 
		let b,a = m.(i) in 
		collision_mur a b
	done;
	(*collision avec un ploygone ouvert ici !*)
	for i = 0 to Array.length polys - 1 do 
		for j = 0 to Array.length polys.(i) - 2 do 
			collision_mur polys.(i).(j) polys.(i).(j+1)
		done
	done; *)
	(if !C.spoon_activated && is_mouse_button_down MouseButton.Left then begin
		let h = {
			x = (C.foi (get_mouse_x()))*.1000./.(C.foi C.h);
			y = (C.foi (get_mouse_y()))*.1000./.(C.foi C.h);
			vel_x = 0.;
			vel_y = 0.;
			acc_x = 0.;
			acc_y = 0.;
			mass = 1.;
			linked_particles = []; 
			force_multiplier = !C.spoon_strength;
			force_function = (ref 4);
			} in 
			pearls.(k).linked_particles <- h :: pearls.(k).linked_particles
	end) 



let update_particle_acceleration p = 
	let ax, ay = List.fold_left 
	(fun acc q -> acc $+. (tableau_f_ressort.(!(q.force_function)) p q) $+. (f_amortissement p q)) 
	(0.,0.) 
	p.linked_particles in 
	p.acc_x <- ax;
	p.acc_y <- ay

let update_particle_position (p : particle) polys solids = 
	(*ici le delta_t() est obligatoire, pas de !r_delta_t *)
	p.vel_x <- p.vel_x +. (p.acc_x/.p.mass)*.C.delta_t();
	p.vel_y <- p.vel_y +. (p.acc_y/.p.mass +. !C.r_g)*. C.delta_t();
	(*nouveau code de collision avec des murs - utilise seulement les polysgones*)
	let qx,qy = ref (p.vel_x*.C.delta_t()), ref (p.vel_y*.C.delta_t()) in  
	let last_hit_nb = ref (-1) in
	let last_hit_side = ref (-1) in 
	let last_hit_shape = ref (-1) in 
	let rec process_collision unit : unit = 
		(*renvoie la première collision - sa position si elle en a une*) 
		let nearest_collision = ref None in 
		let min_t = ref 1. in 
		let final_u = ref 1. in 
		for j = 0 to Array.length polys - 1 do 
			for i = 0 to Array.length polys.(j) - 2 do 
				if not (j = !last_hit_nb && i = !last_hit_side) then
				begin
				let a1,a2 = polys.(j).(i) in 
				let b1,b2 = polys.(j).(i+1) in 
				(*resolution de système 2-2 par inverstion de matrice*)
				(*
				système : p + tdv = au + (1-u)b 
				tdv + (b-a)u = b - p
				mat(dv,(b-a))*(t,u) = b - p 
				(t,u) = mat-1(v,(b-a))*(b - p)
				*)
				let mat = (!qx, !qy),(b1-.a1,b2-.a2) in 
				if det mat <> 0. then 
				let t,u = (inverse mat) &*. ((b1,b2) $-. (p.x,p.y)) in 
				if u >= 0. && u <= 1. && t >= 0. 
				&& t <= !min_t then (
					(*collision*) 
					nearest_collision := Some ((a1,a2),(b1,b2),i,j,false,0, 0.);
					min_t := t;
					final_u := u;
				)
				else ()
				else ()
				end
			done;
		done;
		for k = 0 to Array.length solids - 1 do 
			for i = 0 to Array.length solids.(k) - 1 do 
				List.iter (fun (j, _, cond) ->
					if cond && 
					not (j = !last_hit_nb && i = !last_hit_side && k = !last_hit_shape) then
					begin
					let a1,a2 = solids.(k).(i).x,solids.(k).(i).y in 
					let b1,b2 = solids.(k).(j).x,solids.(k).(j).y in 
					(*resolution de système 2-2 par inverstion de matrice*)
					(*
					système : p + tdv = au + (1-u)b 
					tdv + (b-a)u = b - p
					mat(dv,(b-a))*(t,u) = b - p 
					(t,u) = mat-1(v,(b-a))*(b - p)
					*)
					let mat = (!qx, !qy),(b1-.a1,b2-.a2) in 
					if det mat <> 0. then 
					let t,u = (inverse mat) &*. ((b1,b2) $-. (p.x,p.y)) in 
					if u >= 0. && u <= 1. && t >= 0. 
					&& t <= !min_t then (
						(*collision*) 
						nearest_collision := Some ((a1,a2),(b1,b2),i,j,true,k, !final_u);
						min_t := t;
						final_u := u;
					)
					else ()
					else ()
					end) solids.(k).(i).fixed_links
			done;
		done;
		

		match !nearest_collision with 
		| None -> (
			p.x <- p.x +. !qx;
			p.y <- p.y +. !qy;
		)
		| Some (a,b,i,j,false,_,_) -> (
			let (a1,a2),(b1,b2) = a,b in 
			let h1,h2 = ( !final_u $*. a) $+. ((1.-. !final_u) $*.b) in 
			let orthog =  (b2-.a2),(a1-.b1) in
			let pass_mat = inverse ((b $-. a),orthog) in 
			let mat_trans_totale = pass_mat &. ((1.,0.),(0.,-.1.)) &. (inverse pass_mat) in 
			(*plus optimisé ... plus ou moinp. dans l'idée;
			si je veux que ce soit opti, j'en stocke une par segment ...*)
			(*u est selon orthg et v selon b-a*)
			let p_x, p_y = ((1.-. !min_t)/. !min_t) $*. (mat_trans_totale &*. ((h1 -. p.x),(h2 -. p.y))) in 
			let new_vel_x, new_vel_y = mat_trans_totale &*. (p.vel_x,p.vel_y) in 
			p.x <- h1;
			p.y <- h2;
			qx := p_x;
			qy := p_y;
			p.vel_x <- (1. -. !C.r_a) *. new_vel_x; 
			p.vel_y <- (1. -. !C.r_a) *.new_vel_y;
			last_hit_nb := j;
			last_hit_side := i;
			last_hit_shape := -1;
			process_collision();
			(*à terme - faire un truc un peux plus propre*)
		)
		| Some (a,b,i,j,true,k, u) -> (
			let (a1,a2),(b1,b2) = a,b in 
			let h1,h2 = ( !final_u $*. a) $+. ((1.-. !final_u) $*.b) in 
			let orthog = (b2-.a2),(a1-.b1) in
			let pass_mat = inverse ((b $-. a),orthog) in 
			let mat_trans_totale = pass_mat &. ((1.,0.),(0.,-.1.)) &. (inverse pass_mat) in 
			(*plus optimisé ... plus ou moinp. dans l'idée;
			si je veux que ce soit opti, j'en stocke une par segment ...*)
			(*u est selon orthg et v selon b-a*)
			let p_x, p_y = ((1.-. !min_t)/. !min_t) 
			$*. (mat_trans_totale &*. ((h1 -. p.x),(h2 -. p.y))) in 
			let vb = (!final_u *. solids.(k).(i).vel_x +. (1. -. !final_u) *. solids.(k).(j).vel_x ,
				!final_u *. solids.(k).(i).vel_y +. (1. -. !final_u) *. solids.(k).(j).vel_y ) in 
			let va = (p.vel_x,p.vel_y) in 
			let xa = (p.x,p.y) in 
			let xb = (h1,h2) in
			let mb = ((solids.(k).(i).mass +. solids.(k).(j).mass)/.2.)  in 
			let ma = p.mass in 
			let energy = mb *. (squared_norm vb) +. ma *. (squared_norm va)  in 
			let momentum = (ma $*. va) $+. (mb $*. vb) in 

			(*point critique*)(*)
			let x = momentum  in 

			let kscalx = (x $. momentum) /. (norm x) in 
			let delta = kscalx *. kscalx -. 4. *. (
					mb *. (norm momentum) -. energy) /. (2. *. ma *. (norm x)) 
			in 
			(if delta < 0. then failwith "delta negatif, what. in particles ! ");
			let lamb = 0.5 *. ( -. kscalx -. sqrt (delta)) in

			let nvs_x,nvs_y = mb $/. (momentum $-. ((ma *. lamb ) $*. x)) in 
			let nvp_x,nvp_y = lamb $*. x in 
			*)
			let diff = (squared_norm (C.delta_t() $*. va)) $*. unitaire (xa $-. xb) in 
			let va_prime_x,va_prime_y = va $-. ((
				(2.*.mb/.(ma+.mb)) *.((va $-. vb) $.diff) /.(squared_norm (C.delta_t() $*. va))
			) $*. diff) in 
			let vb_prime_x,vb_prime_y = vb $-. ((
				(2.*.ma/.(ma+.mb)) *.((va $-. vb) $. diff) /.(squared_norm (C.delta_t() $*. va))
			) $*. ((0.,0.) $-. diff)) in 

			solids.(k).(i).vel_x <- !final_u *. vb_prime_x;
			solids.(k).(i).vel_y <- !final_u *. vb_prime_y;
			solids.(k).(j).vel_x <- (1. -. !final_u) *. vb_prime_x;
			solids.(k).(j).vel_y <- (1. -. !final_u) *. vb_prime_y;

			qx := va_prime_x *. C.delta_t() -. (p.x -. h1);
			qy := va_prime_y *. C.delta_t() -. (p.y -. h2);
			p.vel_x <- va_prime_x;
			p.vel_y <- va_prime_y;
			p.x <- h1;
			p.y <- h2;
			p.x <- p.x +. !qx;
			p.y <- p.y +. !qy;
			last_hit_nb := j;
			last_hit_side := i;
			last_hit_shape := k;
			(*process_collision();*)
			(*à terme - faire un truc un peux plus propre*)
			(*le truc plus propre !*)
		)
		
	in	
	process_collision();
	if p.x > 1000. || p.x < 0. || p.y > 1000. || p.y < 0.
	then 
	(
		p.x <- 50. +. Random.float (50.);
		p.y <- 50. +. Random.float (50.);
		p.vel_y <- 0.;
		p.vel_x <- 0.;
		p.linked_particles <- [];
	)

let update_particles grid (pearls : particle array) walls polys solids = 
	update_grid_particles pearls grid;
	for i = 0 to Array.length pearls -1 do (*à parraléliser une fois en C*)
		update_particle_links grid pearls i walls polys;
		update_particle_acceleration pearls.(i);
	done;
	for i = 0 to Array.length pearls -1 do 
		(*important qu'on le fasse après - sinnon problèmes*)
		update_particle_position pearls.(i) polys solids;
	done
(*
=============================================================================

-------------------------- RENDU ET AFFICHAGE -------------------------------

=============================================================================
*)

let draw_speed_arrow ((o1,o2): vect) ((u,v) : vect) color = 
	let speed = norm (u,v) in 

	
	let rotation = 180.*.(
	if  u <> 0. 
		then 
		if u > 0. 
			then atan (v/.u)
		else Float.pi -. atan (-.v/.u)
	else (if v > 0. then 1. else -.1.)*.(Float.pi/.2.)
	)/.Float.pi in 
	let dirx,diry = unitaire (u,v) in 
	let decalagex,decalagey = 2. $*. (diry,-.dirx) in
	let rectangle = Raylib.Rectangle.create 
	(((C.foi C.h)*.o1/.1000.) +. ((C.foi C.h)*.decalagex/.1000.))
	(((C.foi C.h)*.o2/.1000.) +. ((C.foi C.h)*.decalagey/.1000.) )
	speed 4. in 
	draw_rectangle_pro rectangle 
	(Raylib.Vector2.create  0. 0. ) 
	rotation color;
	let p1x,p1y = (o1,o2) $+. ((speed +. 8.) $*. (dirx,diry)) in 
	
	let p3x,p3y =  (o1,o2) $+. (speed  $*. (dirx,diry)) $+. (6. $*. (-.diry,dirx)) in 
	let p2x,p2y =  (o1,o2) $+. (speed  $*. (dirx,diry)) $+. (6. $*. (diry,-.dirx)) in 
	draw_triangle (Raylib.Vector2.create ((C.foi C.h)*.p1x/.1000.) ((C.foi C.h)*.p1y/.1000.))
	(Raylib.Vector2.create ((C.foi C.h)*.p2x/.1000.) ((C.foi C.h)*.p2y/.1000.))
	(Raylib.Vector2.create ((C.foi C.h)*.p3x/.1000.) ((C.foi C.h)*.p3y/.1000.))
	color


let render_particles grid (pearls : particle array) = 
	(if !C.voir_ressorts then
	for i = 0 to Array.length pearls -1 do 

		List.iter (fun (q : particle) -> Raylib.draw_line 
		(C.iof ((C.foi C.h)*.pearls.(i).x/.1000.))
		(C.iof ((C.foi C.h)*.pearls.(i).y/.1000.)) 
		(C.iof ((C.foi C.h)*.q.x/.1000.))
		(C.iof ((C.foi C.h)*.q.y/.1000.)) 
		Color.gray
		 ) pearls.(i).linked_particles
	done); 
	for i = 0 to Array.length pearls -1 do 
		let color_factor = (pearls.(i).vel_x*.pearls.(i).vel_x +. pearls.(i).vel_y*.pearls.(i).vel_y)
			|> sqrt  in 
		Raylib.draw_circle 
		(C.iof ((C.foi C.h)*.pearls.(i).x/.1000.))
		(C.iof ((C.foi C.h)*.pearls.(i).y/.1000.))
		4.
		(color_factor |> C.speed_to_color );
		(if !C.voir_chunks then 
			let case (p : particle) = 
				let max_dist = 2.*.C.l0()*.C.fact_dist_max() in 
				let i,j = C.iof ((p.x)/.max_dist),C.iof ((p.y)/.max_dist) in 
				j*grid.size + i
			in
		Raylib.draw_text (Int.to_string (case pearls.(i))) 
		(C.iof ((C.foi C.h)*.pearls.(i).x/.1000.))
		(C.iof ((C.foi C.h)*.pearls.(i).y/.1000.))
		14 
		(color_factor  |> C.speed_to_color)
		);
		(if !C.arrows then 
			draw_speed_arrow (pearls.(i).x,pearls.(i).y) (!C.arrow_size $*. (pearls.(i).vel_x,pearls.(i).vel_y) )
			(color_factor  |> C.speed_to_color)
		);
	done;
	(if !C.voir_chunks then begin 
		let d = 2.*.C.l0() *. C.fact_dist_max () in 
		for i = 0 to grid.size - 1 do 
			Raylib.draw_line 0 (C.iof ((C.foi i)*.d*.(C.foi C.h)/.1000.)) C.h (C.iof ((C.foi i)*.d*.(C.foi C.h)/.1000.)) Color.purple;
			Raylib.draw_line (C.iof ((C.foi i)*.d*.(C.foi C.h)/.1000.)) 0 (C.iof ((C.foi i)*.d*.(C.foi C.h)/.1000.)) C.h Color.purple;
		done;
		for i = 0 to grid.size - 1 do 
			for j = 0 to grid.size - 1 do 
				Raylib.draw_text (Int.to_string (j*grid.size + i)) 
				(C.iof (d/.2. +. (C.foi i)*.d*.(C.foi C.h)/.1000.))
				(C.iof (d/.2. +. (C.foi j)*.d*.(C.foi C.h)/.1000.))
				 20 Color.purple;
			done 
		done
	end);
	Raylib.draw_line C.h 0 C.h C.h Color.black

let render_walls m = 
	for i = 0 to Array.length m - 1 do 
		let (a1,a2),(b1,b2) = m.(i) in 
		draw_line (C.iof ((C.foi C.h)*.a1/.1000.))
		(C.iof ((C.foi C.h)*.a2/.1000.)) 
		(C.iof ((C.foi C.h)*.b1/.1000.))
		(C.iof ((C.foi C.h)*.b2/.1000.))   Color.beige
	done

let render_solids (solid : solid_particle array) = 
	for i = 0 to Array.length solid -1 do 
		Raylib.draw_circle 
		(C.iof ((C.foi C.h)*.solid.(i).x/.1000.))
		(C.iof ((C.foi C.h)*.solid.(i).y/.1000.))
		3. Color.brown;
		List.iter (fun (k,_,_) ->
		draw_line_ex 
		(Vector2.create ((C.foi C.h)*.solid.(i).x/.1000.) ((C.foi C.h)*.solid.(i).y/.1000.) )
		(Vector2.create ((C.foi C.h)*.solid.(k).x/.1000.) ((C.foi C.h)*.solid.(k).y/.1000.) ) 
		1.
		Color.brown)
		solid.(i).fixed_links ;
		(if !C.arrows then 
			draw_speed_arrow (solid.(i).x,solid.(i).y) 
			(!C.arrow_size $*. (solid.(i).vel_x,solid.(i).vel_y) )
			Color.brown
		);
	done


let render_polygons (polys : (float * float) array array) = 
	for i = 0 to Array.length polys - 1 do
		for j = 0 to Array.length polys.(i) - 2 do
			let (a1,a2),(b1,b2) = polys.(i).(j),polys.(i).(j+1) in 
			draw_line_ex 
			(Vector2.create ((C.foi C.h)*.a1/.1000.) ((C.foi C.h)*.a2/.1000.) )
			(Vector2.create ((C.foi C.h)*.b1/.1000.) ((C.foi C.h)*.b2/.1000.) ) 
			3.
			Color.orange
		done
	done


(*
=============================================================================

-------------------------- GESTION DES POLYGONES ----------------------------

=============================================================================
*)

exception Trouve of int*int


let update_polygons polygons = 
	if !C.manipule_polygones then begin

	let polys = !polygons in 
	for i = 0 to Array.length polys - 1 do
		for j = 0 to Array.length polys.(i) - 1 do
			let (a1,a2) = polys.(i).(j) in 
			draw_circle 
			(C.iof ((C.foi C.h)*.a1/.1000.))
			(C.iof ((C.foi C.h)*.a2/.1000.)) 
			C.polygon_tolerance Color.orange
		done
	done;
	(if !C.last_polygon_selected < Array.length polys 
		&& !C.last_polygon_corner_selected < Array.length polys.(!C.last_polygon_selected) then
	let (a1,a2) = polys.(!C.last_polygon_selected).(!C.last_polygon_corner_selected) in 
	draw_circle 
	(C.iof ((C.foi C.h)*.a1/.1000.))
	(C.iof ((C.foi C.h)*.a2/.1000.)) 
	C.polygon_tolerance Color.red
	);

	let x,y = (C.foi (get_mouse_x()))*.1000./.(C.foi C.h),(C.foi (get_mouse_y()))*.1000./.(C.foi C.h) in 
	(if is_mouse_button_down MouseButton.Left 
		&& x < 1000. && x > 0. && y < 1000. && y > 0. then 
		let get_current_poly_point () = 
			for i = 0 to Array.length polys - 1 do
				for j = 0 to Array.length polys.(i) - 1 do
					if norm ((x,y) $-. polys.(i).(j)) < C.polygon_tolerance then 
					raise (Trouve(i,j))
				done
			done
		in 
		try 
			get_current_poly_point();
			if !C.was_holding_polygons then 
				polys.(!C.last_polygon_selected).(!C.last_polygon_corner_selected) <- (x,y);
			

		with Trouve(i,j) -> (
			polys.(i).(j) <- (x,y);
			C.last_polygon_selected := i;
			C.last_polygon_corner_selected := j;
			C.was_holding_polygons := true;
		)
	);
	(if is_mouse_button_released MouseButton.Left && not !C.was_holding_polygons
		&& x < 1000. && x > 0. && y < 1000. && y > 0.
		&& (Array.length polys) > 0
		&& norm (polys.(!C.last_polygon_selected).(!C.last_polygon_corner_selected) $-. (x,y)) > C.polygon_tolerance
		then (
		polys.(!C.last_polygon_selected) <- Array.init 
			(Array.length polys.(!C.last_polygon_selected) + 1)
			(fun i -> if i < !C.last_polygon_corner_selected + 1 then 
				polys.(!C.last_polygon_selected).(i)
			else if i = !C.last_polygon_corner_selected + 1 then 
				(x,y)
			else polys.(!C.last_polygon_selected).(i-1)
			);
		C.last_polygon_corner_selected := !C.last_polygon_corner_selected + 1;
		) 
	); 
	(if is_mouse_button_released MouseButton.Left then C.was_holding_polygons :=  false);
	(if is_mouse_button_released MouseButton.Right 
		&& x < 1000. && x > 0. && y < 1000. && y > 0. then begin
		let n = Array.length polys in 
		polygons := Array.init (n + 1) 
		(fun i -> if i < n then 
			polys.(i)
		else [|(x,y)|]
		) ;
		C.last_polygon_corner_selected := 0;
		C.last_polygon_selected := n;
	end);
	(if is_key_released Key.F then 
		let k = !C.last_polygon_selected in 
		let n = Array.length polys in 
		polygons := Array.init (n - 1) 
		(fun i -> if i < k then 
			polys.(i)
		else polys.(i+1)
		) ;
		C.last_polygon_corner_selected := 0;
		C.last_polygon_selected := 0;
	);
	end







(*
=============================================================================

-------------------------- MAIN ---------------------------------------------

=============================================================================
*)


let _ = 
	Random.self_init();
	Raylib.init_window (C.w) (C.h) "WATAH";
	Raylib.set_target_fps 60; 
	Printf.printf ("Rayblib worked\n");

	let particles = ref ( create_particles (C.nb_particles()) ) in 
	let walls = [|
	(*(1000.,500.),(600.,900.);(400.,900.),(0.,500.);(400.,900.),(600.,900.)*)
	(*(0.,0.),(0.,1000.);(0.,1000.),(1000.,1000.);(1000.,1000.),(1000.,0.)*)
	|] in
	let grid = ref (create_grid ()) in 
	let polygons = ref [|[|(1000.,500.);(600.,900.);(400.,900.);(0.,500.)|]|] in 

	C.r_reset := (fun () -> 
		particles := create_particles (C.nb_particles());
		grid := create_grid ()
	);
	C.r_update_cells := (fun () -> 
		grid := create_grid ()
	);


	let rec mainloop () =
		if Raylib.window_should_close() then 
		Raylib.close_window()
		else begin
			update_polygons (polygons);
			update_solid triangle !polygons !particles !grid;
			(if not (!C.run_simulation) then
			update_particles !grid !particles walls !polygons solides);
			Ui.update();
			flush stdout;
			(*Raylib.draw_circle 0 0 30. Color.red;
			Raylib.draw_circle 0 100 30. Color.red;*)
			Raylib.begin_drawing();
			Raylib.clear_background Raylib.Color.white;
			render_particles !grid !particles;
			render_walls walls;
			render_polygons (!polygons);
			render_solids triangle;
			Raylib.end_drawing();
			mainloop()

		end
	in mainloop ()





	