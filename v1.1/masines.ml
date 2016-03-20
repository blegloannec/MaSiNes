open Num (* Pour pouvoir utiliser les operateurs sur les num : +/ -/ */ // >/ </ =/ *)
let num0 = num_of_int 0
let _ = Random.self_init ()

(* Types *)
type s_name = string
type s_speed = num
type s_color = (int*int*int)
type signal = { name: s_name; speed: s_speed; color: s_color; label: string option; svg_style: string option; tikz_style: string option }
type position = num
type collision = (s_name list) * (s_name list)
type d_config = (position*s_name) list
type s_id = int
type config = (position*s_id) list
type simulation_param = { nb_iter: int; max_t: num }
type output_param = { img_h: int; img_v: int; zoom_h: float; zoom_v: float; trans_h: float; line_w: float; label_max_t: num option }


(* Signature Module Description de machine *)
module type SIGNALMACHINEDESC =
sig
  val signals : signal list
  val collisions : collision list
  val init_conf : d_config
  val simulation_config : simulation_param option
  val output_config : output_param option
end


(* Parser de description XML *)
module ParseSMDesc : SIGNALMACHINEDESC =
struct
  let attrib_opt x a = try Some (Xml.attrib x a) with _ -> None
  let attrib_def x a def = try Xml.attrib x a with _ -> def
  let random_color () = ((Random.int 255),(Random.int 255),(Random.int 255))
  let parse_color s =
    let i = int_of_string s in
    (((i lsr 16) land 255),((i lsr 8) land 255),(i land 255))
  let xml_find_child xml tag = 
    let rec aux = function
      | [] -> None
      | x::t ->
	 try
	   if (Xml.tag x) = tag then Some x
	   else aux t
	 with _ -> aux t in
    aux (Xml.children xml)
  let parse_signals xml =
    Xml.fold (fun s x ->
      try
	if (Xml.tag x) = "meta_signal" then
	  let col = 
	    match xml_find_child x "color" with
	    | Some xcol ->
	      begin
		try parse_color (Xml.attrib xcol "rgb")
		with _ -> random_color ()
	      end
	    | None -> random_color () in
	  let lab =
	    match xml_find_child x "label" with
	    | Some xlab -> attrib_opt xlab "nom"
	    | None -> None in
	  {name=(Xml.attrib x "id"); speed=(num_of_string (Xml.attrib x "speed")); color=col; label=lab; svg_style=(attrib_opt x "svg_style"); tikz_style=(attrib_opt x "tikz_style")}::s
	else s
      with _ -> s) [] xml
  let parse_collisions xml =
    Xml.fold (fun c x ->
      try 
	if (Xml.tag x) = "rule" then
	  ((Xml.fold (fun s y ->
	    try
	      if (Xml.tag y) = "in" then (Xml.attrib y "idref")::s
	      else s
	    with _ -> s) [] x),
	   (Xml.fold (fun s y ->
	     try
	       if (Xml.tag y) = "out" then (Xml.attrib y "idref")::s
	       else s
	     with _ -> s) [] x))::c
	else c
      with _ -> c) [] xml
  let parse_init_conf xml =
    Xml.fold (fun s x ->
      try
	if (Xml.tag x) = "start" then ((num_of_string (Xml.attrib x "pos")),(Xml.attrib x "idref"))::s
	else s
      with _ -> s) [] xml
  let parse_simulation_config xml =
    match xml_find_child xml "simulation" with
    | Some x -> Some {nb_iter=(int_of_string (Xml.attrib x "iterations")); max_t=(num_of_string (attrib_def x "max_time" "0"))}
    | None -> None
  let parse_output_config xml =
    match xml_find_child xml "output" with
    | Some x -> Some {img_h=(int_of_string (Xml.attrib x "height")); img_v=(int_of_string (Xml.attrib x "width")); zoom_h=(float_of_string (attrib_def x "zoom_h" "1.")); zoom_v=(float_of_string (attrib_def x "zoom_v" "1.")); trans_h=(float_of_string (attrib_def x "trans_h" "0.")); line_w=(float_of_string (attrib_def x "line_w" "1.")); label_max_t=(try Some (num_of_string (Xml.attrib x "label_max_time")) with _ -> None)}
    | None -> None
  let xml = 
    if (Array.length Sys.argv)<>3 then begin (* ----- USAGE ----- *)
      Printf.eprintf "usage: %s [pdf|svg|tikz] in.xml > out\n" Sys.argv.(0);
      exit 1
    end
    else Xml.parse_file Sys.argv.(2)
  let signals = parse_signals xml
  let collisions = parse_collisions xml
  let init_conf = List.rev (parse_init_conf xml)
  let simulation_config = parse_simulation_config xml
  let output_config = parse_output_config xml
end


(* Signature Module Machine *)
module type SIGNALMACHINE =
sig
  val nb_signals : int
  val signals : signal array
  val speed_compare : s_id -> s_id -> int
  val collisions : (s_id list, s_id list) Hashtbl.t
  val init_conf : config
  val simulation_config : simulation_param
  val output_config : output_param
end


(* Foncteur qui fabrique une SM a partir d'une SMD *)
module MachineMaker (SigMacDesc : SIGNALMACHINEDESC) : SIGNALMACHINE =
struct
  let nb_signals : int = List.length SigMacDesc.signals
  let s_name2id_tbl : (s_name, s_id) Hashtbl.t = 
    let h = Hashtbl.create nb_signals in
    let cpt = ref 0 in
      List.iter (fun s -> Hashtbl.add h s.name (!cpt); incr cpt) SigMacDesc.signals; h
  let s_name2id : s_name -> s_id = fun name ->
      try Hashtbl.find s_name2id_tbl name
      with Not_found -> failwith "s_name2id error"
  let signals : signal array = Array.of_list SigMacDesc.signals
  (* Comparaison de signaux suivant leur vitesse *)
  let speed_compare : s_id -> s_id -> int = fun id1 id2 ->
    if signals.(id1).speed </ signals.(id2).speed then -1
    else if signals.(id1).speed >/ signals.(id2).speed then 1
    else 0
  (* Comparaison de signaux dans une configuration suivant la position, puis la vitesse. *)
  let pos_speed_compare (p1,id1) (p2,id2) =
      let pos_comp = compare_num p1 p2 in
      if pos_comp<>0 then pos_comp
      else compare_num signals.(id1).speed signals.(id2).speed
  (* La config. initiale est triee pour satisfaire la prop. fond. du simulateur infra *)
  let init_conf : config = List.sort pos_speed_compare (List.map (fun (pos,name) -> (pos,s_name2id name)) SigMacDesc.init_conf)
  (* Les signaux entrants d'une collision sur tries (suivant leur nom) pour normalisation.
     Les signaux sortants sont tries suivant leur vitesse pour preserver l'ordre requis au sein d'une configuration (cf. infra). *)
  let collisions : (s_id list, s_id list) Hashtbl.t =
    let h = Hashtbl.create nb_signals in
      List.iter (fun (l1,l2) -> Hashtbl.add h (List.sort compare (List.map s_name2id l1)) (List.sort speed_compare (List.map s_name2id l2))) SigMacDesc.collisions; h
  let simulation_config =
    match SigMacDesc.simulation_config with 
    | Some c -> c
    | None -> {nb_iter=100; max_t=num0}
  let output_config = 
    match SigMacDesc.output_config with
    | Some c -> c
    | None -> {img_h=100; img_v=100; zoom_h=1.; zoom_v=1.; trans_h=49.; line_w=1.; label_max_t=None}
end


(* Hashage sur type num *)
module HashedNum : Hashtbl.HashedType with type t = num =
struct
  type t = num
  let equal = (=/)
  let hash = Hashtbl.hash
end

module NumHashtbl = ((Hashtbl.Make(HashedNum)):Hashtbl.S with type key = num)


(* Foncteur simulateur de SM *)
module SiMaLator (SigMac : SIGNALMACHINE) =
struct
  type time = num
  type point = time * position
  type conf_sig = position * s_id * point
  (* Le 3eme element, de type point, est le point de depart du signal. *)
  (* ATTENTION : redefinition du type config pour la simulation. *)
  type config = time * (conf_sig list)


  (* Intersection : ne renvoie Some _ que si l'intersection est strictement dans le futur. *)
  let inter : conf_sig -> conf_sig -> point option = fun (p1,id1,_) (p2,id2,_) ->
    let s1 = SigMac.signals.(id1).speed and s2 = SigMac.signals.(id2).speed in
      if s1<>/s2 then begin
	let t = (p2-/p1)//(s1-/s2) in
	if t>/num0 then Some (t,s1*/t+/p1)
	else None
      end
      else None
	
  (* Generic output *)
  type vect_object = s_id*point*point (* id, start_point, end_point *)
  let diagram : vect_object list ref = ref []
  let print_signal id p0 p1 = diagram := (id,p0,p1)::(!diagram)

  (* Output variables *)
  let out_width = SigMac.output_config.img_v
  let out_height = SigMac.output_config.img_h
  let out_alphat = SigMac.output_config.zoom_v
  let out_alphap = SigMac.output_config.zoom_h
  let out_sigma = SigMac.output_config.trans_h
  let out_fh = float_of_int out_height

  (* SVG Output *)
  let svg_header () = 
    Printf.printf "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" width=\"%d\" height=\"%d\">\n" out_width out_height
  let svg_footer () = print_string "</svg>"
  let svg_signal (id,(t0,p0),(t1,p1)) =
    match SigMac.signals.(id).color,SigMac.signals.(id).svg_style with
    | (r,g,b),None -> Printf.printf "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" style=\"stroke:rgb(%d,%d,%d)\" />\n" (out_alphap*.((float_of_num p0)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t0)) (out_alphap*.((float_of_num p1)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t1)) r g b
    | (r,g,b),Some s -> Printf.printf "<line x1=\"%f\" y1=\"%f\" x2=\"%f\" y2=\"%f\" style=\"stroke:rgb(%d,%d,%d);%s\" />\n" (out_alphap*.((float_of_num p0)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t0)) (out_alphap*.((float_of_num p1)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t1)) r g b s
  let svg_output () =
    svg_header ();
    List.iter svg_signal (!diagram);
    svg_footer ()

  (* Cairo Output *)
  let cairo_signal ctx (id,(t0,p0),(t1,p1)) =
    match SigMac.signals.(id).color with (r,g,b) ->
    Cairo.set_source_rgb ctx ((float_of_int r)/.255.) ((float_of_int g)/.255.) ((float_of_int b)/.255.);
    Cairo.move_to ctx (out_alphap*.((float_of_num p0)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t0));
    Cairo.line_to ctx (out_alphap*.((float_of_num p1)+.out_sigma)) (out_fh-.out_alphat*.(float_of_num t1));
    Cairo.stroke ctx
  let cairo_output () =
    let surf = Cairo_pdf.surface_create_for_channel stdout (float_of_int out_width) out_fh in
    let ctx = Cairo.create surf in
    Cairo.set_line_width ctx SigMac.output_config.line_w; (* --- Cairo line width --- *)
    List.iter (cairo_signal ctx) (!diagram);
    Cairo.surface_finish surf

  (* TikZ Output *)
  let tikz_header () =
    Printf.printf "\\begin{tikzpicture}\n";
    Array.iter (fun s -> 
      match s.color with (r,g,b) ->
	Printf.printf "\\definecolor{couleur%s}{RGB}{%i,%i,%i}\n\\tikzstyle{draw%s}=[draw=couleur%s]\n" s.name r g b s.name s.name;
	Printf.printf "\\tikzstyle{node%s}=[pos=0.5%s]\n" s.name (if s.speed=/num0 then ", left" else ", above, sloped")) SigMac.signals
  let tikz_footer () = Printf.printf "\\end{tikzpicture}\n"
  let tikz_signal (id,(t0,p0),(t1,p1)) =
    let s =  SigMac.signals.(id) in
    match s.label,SigMac.output_config.label_max_t with
    | None,_ -> Printf.printf "\\draw[draw%s] (%f,%f) -- (%f,%f);\n" s.name (float_of_num p0) (float_of_num t0) (float_of_num p1) (float_of_num t1)
    | Some l,None -> Printf.printf "\\draw[draw%s] (%f,%f) -- node[node%s]{%s} (%f,%f);\n" s.name (float_of_num p0) (float_of_num t0) s.name l (float_of_num p1) (float_of_num t1)
    | Some l,Some labt ->
       if t0>=/labt then
	 Printf.printf "\\draw[draw%s] (%f,%f) -- (%f,%f);\n" s.name (float_of_num p0) (float_of_num t0) (float_of_num p1) (float_of_num t1)
       else
	 Printf.printf "\\draw[draw%s] (%f,%f) -- node[node%s]{%s} (%f,%f);\n" s.name (float_of_num p0) (float_of_num t0) s.name l (float_of_num p1) (float_of_num t1)
  let tikz_output () =
    tikz_header ();
    List.iter tikz_signal (!diagram);
    tikz_footer ()

  (* Fonctions auxiliaires *)
  let list_fold_lefti f a l = snd (List.fold_left (fun (i,a) b -> (i+1,f i a b)) (0,a) l)	
  let force_draw_to c t1 = List.iter (fun (_,id,(ts,ps)) -> if t1 >/ ts then print_signal id (ts,ps) (t1,SigMac.signals.(id).speed*/(t1-/ts)+/ps)) c
  let rec double_list_fold f a = function 
    | [] | _::[] -> a
    | h1::h2::t -> double_list_fold f (f a h1 h2) (h2::t)
  exception No_intersection of config

  (* Pour debug. 
  let print_conf c =
    List.iter (fun (p,id,_) -> Printf.eprintf "%f %s %f\n" (float_of_num p) SigMac.signals.(id).name (float_of_num SigMac.signals.(id).speed)) c *)

  (* COEUR DU CODE : Simulation d'une etape
     PROPRIETE FONDAMENTALE A PRESERVER : les signaux d'une configuration sont tries
     par position, puis par vitesse a position egale. *)
  let step : config -> config = fun (t,c) ->
    (* On calcule l'instant de la prochaine collision.
       Le calcul est fait en temps lineaire en ne considerant que les signaux consecutifs dans la configuration
       convenablement triee (cf. prop. fond. supra). *)
    let tmin = double_list_fold (fun tmin s1 s2 ->
      match inter s1 s2 with
      | Some (t,_) -> if tmin </ num0 || t </ tmin then t else tmin
      | None -> tmin) (num_of_int (-1)) c in
    (* Si pas d'intersection, on lance la meme config en exception. *) 
    if tmin <=/ num0 then raise (No_intersection (t,c));
    let newt = tmin +/ t in
    let hp2s : (s_id*point) NumHashtbl.t = NumHashtbl.create (List.length c) in
    (* On calcule les positions des signaux sur le nouveau front de collision et on
       produit une liste triee des positions.
       On les range par position dans une table de hashage. *)
    let positions = List.sort compare_num (list_fold_lefti (
      fun i l (p0,id,sp) ->
	let p = SigMac.signals.(id).speed */ tmin +/ p0 in
	let res = if NumHashtbl.mem hp2s p then l else p::l in
	NumHashtbl.add hp2s p (id,sp); res
    ) [] c) in
    (* On calcule la nouvelle configuration. Les positions sont considerees dans l'ordre croissant. *)
    let newc = List.fold_left (
      fun l p ->
        (* On recupere les signaux en position p. *)
	let sigs = NumHashtbl.find_all hp2s p in
	(* On trie les id pour satisfaire la normalisation initiale de la table des collisions. *)
	let sigs_id = List.sort compare (List.map fst sigs) in
	try 
	  (* Si on trouve une regle a appliquer, on remplace les signaux entrants par les sortants
             apres avoir affiche les signaux entrants. *)
	  let newsigs_id = Hashtbl.find SigMac.collisions sigs_id in
	  List.iter (fun (id,sp) -> print_signal id sp (newt,p)) sigs;
	  (* On concatene la liste a la fin de la configuration pour satisfaire le tri par position de la configuration produite. *)
	  l@(List.map (fun id -> (p,id,(newt,p))) newsigs_id)
	(* Sinon, on reproduit les signaux en les triant par vitesse pour preserver la prop. fond.
           On concatene la liste a la fin de la configuration pour satisfaire le tri par position de la configuration produite. *)
	with Not_found -> l@(List.map (fun (id,sp) -> (p,id,sp)) (List.sort (fun (s1,_) (s2,_) -> SigMac.speed_compare s1 s2) sigs))
    ) [] positions in
    (newt,newc)
    
  let rec run_conf : int -> config -> config = fun n conf ->
    if n = 0 then conf
    else try run_conf (n-1) (step conf) with No_intersection c -> c

  let run : int -> unit = fun n ->
    let tmax = SigMac.simulation_config.max_t in
    let (t,c) = run_conf n (num0, (List.map (fun (p,id) -> (p,id,(num0,p))) SigMac.init_conf)) in
    if t </ tmax then force_draw_to c tmax
    else force_draw_to c t

   let autorun : unit -> unit = fun () -> run SigMac.simulation_config.nb_iter
end



(* MAIN *)
module MyMachine = SiMaLator(MachineMaker(ParseSMDesc))

let main () =
  MyMachine.autorun ();
  if Sys.argv.(1)="svg" then
    MyMachine.svg_output ()
  else if Sys.argv.(1)="tikz" then
    MyMachine.tikz_output ()
  else
    MyMachine.cairo_output ()

let _ = main ()
