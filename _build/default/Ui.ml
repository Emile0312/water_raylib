open Raylib

type pos = int * int * int * int
(*posx posy width height*)
(*les positions sont relatives au truc au dessus*)
type t = 
	Block of pos * t list
	| Button of pos * (unit -> unit) * Color.t * string 
	| Toggle of pos * int * bool ref * Color.t * string 
					(*min current max - on considère 1 incrément par pixel, linéraire*)
	| Scalef of pos * float * float ref * float * string
	| Scalei of pos * int * int ref * int * string
	| Scalei_fun of pos * int * int ref * int * (unit -> unit) * string
	| Scalef_fun of pos * float * float ref * float * (unit -> unit) * string
	| Scalecter of pos * int * string array *int ref * int * string 


let current = 
	(*le bloc global*)
	Block ((0,0, 1000,800),
		[
		Block((920,0,360,800),
			[
			Button((0,20,360,80),C.reset,Color.green,"Reset");
			Scalef((0,100,360,100),0., C.r_k, 10. ,"K ressort");
			Scalef_fun((0,200,360,100),0.0, C.r_l0, 200. , C.update_cells,"Longeur à vide");
			Scalef_fun((0,300,360,100),1., C.r_fact_dist_max, 2., C.update_cells,"facteur distance à vide");
			Scalef((0,400,360,100),0.0, C.r_g, 30. ,"gravité");
			Scalei_fun((0,500,360,100),0, C.r_nb_particles, 10000 ,C.reset,"nb particles");
			Scalef((0,600,360,100),0.0, C.r_a, 1. ,"friction");
			Scalef((0,700,360,100),10., C.r_delta_t, 200. ,"updates/s");
			Scalecter((0,800,360,100),0,C.a_f_ressort_selecter,C.r_f_ressort_selecter,3,"Type Force ressort")
			]
		);
		Block((1320,0,360,800),
			[
				Toggle((0,20,180,80),10,C.voir_ressorts,Color.blue,"Voir ressorts");
				Toggle((180,20,180,80),10,C.arrows,Color.pink,"flèches");
				Toggle((0,100,360,80),10,C.voir_chunks,Color.purple,"Voir Chunks");
				Toggle((0,180,360,80),10,C.run_simulation,Color.red,"Pause");
				Toggle((0,260,360,80),10,C.manipule_polygones,Color.orange,"Manipule les polygones");
				Toggle((0,340,360,80),10,C.spoon_activated,Color.skyblue,"Cuillière");
				Scalef((0,420,360,100),-.50., C.spoon_strength, 50. ,"force cuillière");
				Scalef((0,500,360,100),1., C.spoon_radius, 100. ,"taille cuillière");
				Scalef((0,600,360,100),1., C.r_color_factor, 10. ,"RGB");
				Scalef((0,700,360,100),10., C.r_k_solid, 1000. ,"ressort des solides");
				Scalef((0,800,360,100),1., C.r_a_solid, 100. ,"frotement des solides");
			]
		)
		])

let update () = 
	let is_in (x,y) (posx,posy,w,h) = 
		(x >= posx && x <= posx + w && y >= posy && y <= posy + h)
	in 
	let rec rndr_compute (x,y) (gx,gy) obj = 
		match obj with 
		| Block ((px,py,w,h),q) ->  
			List.iter (rndr_compute (x-px,y-py) (gx+px,gy+py)) q
		| Button((px,py,w,h),f,c,s) -> begin
			draw_rectangle (gx+px) (gy+py) w h c;
			Raylib.draw_text s (gx+px+20) (gy+py+h/2) 24 Color.black;
			(if is_mouse_button_released MouseButton.Left && is_in (x,y) (px,py,w,h) then 
			f ())
		end
		| Toggle((px,py,w,h),e,t,c,s) -> begin
			draw_rectangle (gx+px) (gy+py) w h c;
			(if !t then begin
				Raylib.draw_text s (gx+px+20) (gy+py+h/2) 24 Color.black;
				draw_rectangle (gx+px) (gy+py) e h Color.black;
				draw_rectangle (gx+px) (gy+py) w e Color.black;
				draw_rectangle (gx+px+w-e) (gy+py) e h Color.black;
				draw_rectangle (gx+px) (gy+py+h-e) w e Color.black;
			end else begin
				Raylib.draw_text s (gx+px+20) (gy+py+h/2) 24 Color.white;
				draw_rectangle (gx+px) (gy+py) e h Color.white;
				draw_rectangle (gx+px) (gy+py) w e Color.white;
				draw_rectangle (gx+px+w-e) (gy+py) e h Color.white;
				draw_rectangle (gx+px) (gy+py+h-e) w e Color.white;
			end);
			(if is_mouse_button_released MouseButton.Left && is_in (x,y) (px,py,w,h) then 
				t := not(!t)
			)	
		end
		| Scalef((px,py,w,h),fmin,rfc,fmax,s) -> begin
			draw_line (gx+px) (gy+py+3*h/4) (gx+px+w) (gy+py+3*h/4) Color.black;
			draw_text s (gx+px+w/4) (gy+py+h/4) 20 Color.black;
			draw_rectangle (gx+px+C.iof((C.foi w)*. (!rfc -. fmin)/.(fmax-.fmin))-5)
			(gy+py+3*h/4-15) 10 30 Color.red;
			draw_text (Float.to_string !rfc) 
			(gx+px+C.iof((C.foi w)*. (!rfc -. fmin)/.(fmax-.fmin))-8) 
			(gy+py+h/2-3) 20 Color.black;
			draw_text (Float.to_string fmin) (gx+px) (gy+py+h/2) 20 Color.black;
			draw_text (Float.to_string fmax) (gx+px+w) (gy+py+h/2) 20 Color.black;
			(if is_mouse_button_down MouseButton.Left && is_in (x,y) (px,py,w,h) then
			rfc := (fmin+.((fmax-.fmin)*.((C.foi x)))/.(C.foi w)))
		end
		| Scalef_fun((px,py,w,h),fmin,rfc,fmax,f,s) -> begin
			draw_line (gx+px) (gy+py+3*h/4) (gx+px+w) (gy+py+3*h/4) Color.black;
			draw_text s (gx+px+w/4) (gy+py+h/4) 20 Color.black;
			draw_rectangle (gx+px+C.iof((C.foi w)*. (!rfc -. fmin)/.(fmax-.fmin))-5)
			(gy+py+3*h/4-15) 10 30 Color.red;
			draw_text (Float.to_string !rfc) 
			(gx+px+C.iof((C.foi w)*. (!rfc -. fmin)/.(fmax-.fmin))-8) 
			(gy+py+h/2-3) 20 Color.black;
			draw_text (Float.to_string fmin) (gx+px) (gy+py+h/2) 20 Color.black;
			draw_text (Float.to_string fmax) (gx+px+w) (gy+py+h/2) 20 Color.black;
			(if is_mouse_button_down MouseButton.Left && is_in (x,y) (px,py,w,h) then
			(rfc := (fmin+. ((fmax-.fmin)*.((C.foi x)))/.(C.foi w));f())
			)
		end
		| Scalei((px,py,w,h),fmin,rfc,fmax,s) ->  begin
			draw_line (gx+px) (gy+py+3*h/4) (gx+px+w) (gy+py+3*h/4) Color.black;
			draw_text s (gx+px+w/4) (gy+py+h/4) 20 Color.black;
			draw_rectangle (gx+px+(w* !rfc)/(fmax-fmin)-5)
			(gy+py+3*h/4-15) 10 30 Color.red;
			draw_text (Int.to_string !rfc)
			(gx+px+(w* !rfc)/(fmax-fmin)-8) 
			(gy+py+h/2-3) 20 Color.black;
			draw_text (Int.to_string fmin) (gx+px) (gy+py+h/2) 20 Color.black;
			draw_text (Int.to_string fmax) (gx+px+w) (gy+py+h/2) 20 Color.black;
			(if is_mouse_button_down MouseButton.Left && is_in (x,y) (px,py,w,h) then 
			rfc := fmin+((fmax-fmin)*(x+w/(2*(fmax-fmin))))/w)
		end
		| Scalei_fun((px,py,w,h),fmin,rfc,fmax,f,s) ->  begin
			draw_line (gx+px) (gy+py+3*h/4) (gx+px+w) (gy+py+3*h/4) Color.black;
			draw_text s (gx+px+w/4) (gy+py+h/4) 20 Color.black;
			draw_rectangle (gx+px+(w* !rfc)/(fmax-fmin)-5)
			(gy+py+3*h/4-15) 10 30 Color.red;
			draw_text (Int.to_string !rfc)
			(gx+px+(w* !rfc)/(fmax-fmin)-8) 
			(gy+py+h/2-3) 20 Color.black;
			draw_text (Int.to_string fmin) (gx+px) (gy+py+h/2) 20 Color.black;
			draw_text (Int.to_string fmax) (gx+px+w) (gy+py+h/2) 20 Color.black;
			(if is_mouse_button_down MouseButton.Left && is_in (x,y) (px,py,w,h) then 
			(rfc := fmin+((fmax-fmin)*(x+w/(2*(fmax-fmin))))/w; f()))
		end
		| Scalecter((px,py,w,h),fmin,a,rfc,fmax,s) -> begin
			draw_line (gx+px) (gy+py+3*h/4) (gx+px+w) (gy+py+3*h/4) Color.black;
			draw_text s (gx+px+w/4) (gy+py+h/4) 20 Color.black;
			draw_rectangle (gx+px+(w* !rfc)/(fmax-fmin)-5)
			(gy+py+3*h/4-15) 10 30 Color.red;
			draw_text a.(!rfc)
			(gx+px+w/2-8) 
			(gy+py+h/2-3) 20 Color.black;
			draw_text (Int.to_string fmin) (gx+px) (gy+py+h/2) 20 Color.black;
			draw_text (Int.to_string fmax) (gx+px+w) (gy+py+h/2) 20 Color.black;
			(if is_mouse_button_down MouseButton.Left && is_in (x,y) (px,py,w,h) then 
			rfc := fmin+((fmax-fmin)*(x+w/(2*(fmax-fmin))))/w)
		end
	in rndr_compute (get_mouse_x(),get_mouse_y()) (0,0) current