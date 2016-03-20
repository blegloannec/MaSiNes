open Num;;
let num0 = num_of_int 0;;

(* Type Description de machine *)
module type SIGNALMACHINEDESC =
sig
  type s_name = string
  type s_label = string
  type s_color = int * int * int
  type s_speed = num
  type signal = s_name * s_speed * s_color * s_label
  type position = num
  type collision = (s_name list) * (s_name list)
  type config = (position*s_name) list
  val signals : signal list
  val collisions : collision list
  val init_conf : config
end;;

(* EXEMPLES
module Middle : SIGNALMACHINEDESC =
struct
  type s_name = string
  type s_color = string
  type s_speed = num
  type signal = s_name * s_speed * s_color
  type position = num
  type collision = (s_name list) * (s_name list)
  type config = (position*s_name) list
  let signals = [("w",num_of_int 0,"black"); ("a",num_of_int 0,"blue"); ("ar",num_of_int 1,"blue"); ("aR",num_of_int 3,"red"); ("aL",num_of_int (-3),"red")]
  let collisions = [(["aR";"w"],["aL";"w"]); (["ar";"aL"],["a"])]
  let init_conf = [(num_of_int 0,"w");(num_of_int 0,"ar");(num_of_int 0,"aR");(num_of_int 1,"w")]
end;;

module Zenon : SIGNALMACHINEDESC =
struct
  type s_name = string
  type s_color = string
  type s_speed = num
  type signal = s_name * s_speed * s_color
  type position = num
  type collision = (s_name list) * (s_name list)
  type config = (position*s_name) list
  let signals = [("wl",num_of_int 1,"black"); ("wr",num_of_int (-1),"black"); ("ar",num_of_int 3,"blue"); ("al",num_of_int (-3),"red")]
  let collisions = [(["ar";"wr"],["al";"wr"]); (["al";"wl"],["ar";"wl"])]
  let init_conf = [(num_of_int 0,"wl");(num_of_int 0,"ar");(num_of_int 1,"wr");(num_of_int 1,"al")]
end;;

module Tree : SIGNALMACHINEDESC =
struct
  type s_name = string
  type s_color = string
  type s_speed = num
  type signal = s_name * s_speed * s_color
  type position = num
  type collision = (s_name list) * (s_name list)
  type config = (position*s_name) list
  let signals = [("w",num_of_int 0,"black"); ("ar",num_of_int 1,"blue"); ("al",num_of_int (-1),"green"); ("aR",num_of_int 3,"red"); ("aL",num_of_int (-3),"orange")]
  let collisions = [(["aR";"w"],["aL";"w"]); (["aL";"w"],["aR";"w"]); (["ar";"aL"],["w";"aR";"ar";"aL";"al"]); (["al";"aR"],["w";"aR";"ar";"aL";"al"])]
  let init_conf = [(num_of_int 0,"w");(num_of_int 0,"ar");(num_of_int 0,"aR");(num_of_int 1,"w")]
end;;
 FIN EXEMPLES *)


(* Parser POURRI de description .sm *)
let parse_num : string -> num = fun s ->
  try Scanf.sscanf s "%d/%d" (fun x y -> (num_of_int x)//(num_of_int y))
  with _ -> begin
    try Scanf.sscanf s "%d" (fun x -> num_of_int x)
    with _ -> failwith "num parsing error"
  end
    
module ParseSMDesc : SIGNALMACHINEDESC =
struct
  type s_name = string
  type s_label = string
  type s_color = int * int * int
  type s_speed = num
  type signal = s_name * s_speed * s_color * s_label
  type position = num
  type collision = (s_name list) * (s_name list)
  type config = (position*s_name) list
  let parse_signals () = 
    let nb = ref 0 in
      Scanf.scanf "%d\n" (fun i -> nb := i);
      let s = Array.make (!nb) ("",num0,(0,0,0),"") in
	for i = 0 to (!nb)-1 do
	  Scanf.scanf "%s %s %i %i %i %s\n" (fun x y r g b l -> s.(i) <- (x,parse_num y,(r,g,b),l))
	done;
	Array.to_list s
  let parse_collisions () =
    let nb = ref 0 in
      Scanf.scanf "%d" (fun i -> nb := i);
      let s = Array.make (!nb) ([],[]) in
	for i = 0 to (!nb)-1 do
	  let g = ref 0 and d = ref 0 in
	    Scanf.scanf "\n%d %d" (fun x y -> g := x; d := y);
	    let tg = Array.make (!g) "" and td = Array.make (!d) "" in
	      for j = 0 to (!g)-1 do
		Scanf.scanf " %s" (fun x -> tg.(j) <- x);
	      done;
	      for j = 0 to (!d)-1 do
		Scanf.scanf " %s" (fun x -> td.(j) <- x);
	      done;
	      s.(i) <- (Array.to_list tg, Array.to_list td)
	done;
	Array.to_list s
  let parse_init_conf () = 
    let nb = ref 0 in
      Scanf.scanf "\n%d\n" (fun i -> nb := i);
      let s = Array.make (!nb) (num0,"") in
	for i = 0 to (!nb)-1 do
	  Scanf.scanf "%s %s\n" (fun x y -> s.(i) <- (parse_num x,y))
	done;
	Array.to_list s
  let signals = parse_signals ()
  let collisions = parse_collisions ()
  let init_conf = parse_init_conf ()
end;;



(* Type Machine *)
module type SIGNALMACHINE =
sig
  type s_id = int
  type s_name = string
  type s_label = string
  type s_color = int * int * int
  type s_speed = num
  type signal = s_name * s_speed * s_color * s_label
  type position = num
  type config = (position*s_id) list
  val nb_signals : int
  val signals : signal array
  val collisions : (s_id list, s_id list) Hashtbl.t
  val init_conf : config
  val get_name : s_id -> s_name
  val get_speed : s_id -> s_speed
  val get_color : s_id -> s_color
  val get_label : s_id -> s_label
end;;


(* Fabrique une Machine a partir d'une Description *)
module MachineMaker (SigMacDesc : SIGNALMACHINEDESC) : SIGNALMACHINE =
struct
  type s_name = SigMacDesc.s_name
  type s_label = SigMacDesc.s_label
  type s_speed = SigMacDesc.s_speed
  type s_color = SigMacDesc.s_color
  type s_id = int
  type signal = SigMacDesc.signal
  type position = SigMacDesc.position
  type config = (position*s_id) list
  let nb_signals : int = List.length SigMacDesc.signals
  let s_name2id_tbl : (s_name, s_id) Hashtbl.t = 
    let h = Hashtbl.create nb_signals in
    let cpt = ref 0 in
      List.iter (fun (name,_,_,_) -> Hashtbl.add h name (!cpt); incr cpt) SigMacDesc.signals; h
  let s_name2id : s_name -> s_id = fun name ->
      try Hashtbl.find s_name2id_tbl name
      with Not_found -> (-1)
  let signals : signal array = Array.of_list SigMacDesc.signals
  let init_conf : config = List.map (fun (pos,name) -> (pos,s_name2id name)) SigMacDesc.init_conf
  let collisions : (s_id list, s_id list) Hashtbl.t =
    let h = Hashtbl.create nb_signals in
      List.iter (fun (l1,l2) -> Hashtbl.add h (List.sort compare (List.map s_name2id l1)) (List.sort compare (List.map s_name2id l2))) SigMacDesc.collisions; h
  let get_name : s_id -> s_name = fun id -> match signals.(id) with (n,_,_,_) -> n
  let get_speed : s_id -> s_speed = fun id -> match signals.(id) with (_,s,_,_) -> s
  let get_color : s_id -> s_color = fun id -> match signals.(id) with (_,_,c,_) -> c
  let get_label : s_id -> s_label = fun id -> match signals.(id) with (_,_,_,l) -> l
end;;


(* Hashage sur type num *)
module HashedNum : Hashtbl.HashedType with type t = num =
struct
  type t = num
  let equal = (=/)
  let hash = Hashtbl.hash
end;;

module NumHashtbl = ((Hashtbl.Make(HashedNum)):Hashtbl.S with type key = num);;


(* Simulateur de Machine *)
module SiMaLator (SigMac : SIGNALMACHINE) =
struct
  type time = num
  type position = num
  type point = time * position  
  type s_id = SigMac.s_id
  type config = time * ((position * s_id * point) list)

  (* Intersection *)
  let inter : position*s_id*point -> position*s_id*point -> point option = fun (p1,id1,_) (p2,id2,_) ->
    let s1 = SigMac.get_speed id1 and s2 = SigMac.get_speed id2 in
      if s1<>/s2 then begin
	let t = (p2-/p1)//(s1-/s2) in
	  if t>/num0 then Some (t,s1*/t+/p1)
	  else None
      end
      else None
	
  (* Tikz Output *)
  let tmax = ref (num_of_int 1)
  let label_iter_inf = ref 0
  let configure_output tm l = tmax := tm; label_iter_inf := l
  let print_tikz_header () =
    Printf.printf "\\begin{tikzpicture}\n";
    Array.iter (fun (n,s,(r,g,b),l) -> Printf.printf "\\definecolor{couleur%s}{RGB}{%i,%i,%i}\n\\tikzstyle{draw%s}=[draw=couleur%s]\n" n r g b n n; Printf.printf "\\tikzstyle{node%s}=[pos=0.5%s]\n" n (if s=/num0 then ", left" else ", above, sloped")) SigMac.signals
  let print_tikz_footer () = Printf.printf "\\end{tikzpicture}\n"
  let print_signal id (t0,p0) (t1,p1) iter =
    let lab = (SigMac.get_label id) in
      if (lab = "") || (iter < !label_iter_inf) then Printf.printf "\\draw[draw%s] (%f,%f) -- (%f,%f);\n" (SigMac.get_name id) (float_of_num p0) (float_of_num t0) (float_of_num p1) (float_of_num t1)
      else Printf.printf "\\draw[draw%s] (%f,%f) -- node[node%s]{%s} (%f,%f);\n" (SigMac.get_name id) (float_of_num p0) (float_of_num t0) (SigMac.get_name id) lab (float_of_num p1) (float_of_num t1)

  (* Fonctions auxiliaires *)
  let list_fold_lefti f a l = snd (List.fold_left (fun (i,a) b -> (i+1,f i a b)) (0,a) l)	
  let force_draw_to c t1 = List.iter (fun (_,id,(ts,ps)) -> if t1 >/ ts then print_signal id (ts,ps) (t1,(SigMac.get_speed id)*/(t1-/ts)+/ps) 0) c
    
  (* Simulation d'une etape *)
  let step : config -> int -> config = fun (t,c) n ->
    let ctab = Array.of_list c in
    let ltab = Array.length ctab in
    let get_start_point i = match ctab.(i) with (_,_,pt) -> pt in
    let tmin = ref (num_of_int (-1)) in
      for i = 0 to ltab-2 do
	for j = i+1 to ltab-1 do
	  match inter ctab.(i) ctab.(j) with
	    | Some (t,_) -> 
		if !tmin </ num0 || t </ !tmin then tmin := t
	    | None -> ()
	done
      done;
      if !tmin <=/ num0 then (t,c)
      else begin
	let newt = !tmin +/ t in
	let hp2s : (s_id*int) NumHashtbl.t = NumHashtbl.create ltab in
	let positions = list_fold_lefti (
	  fun i l (p0,id,_) ->
	    let p = (SigMac.get_speed id) */ !tmin +/ p0 in
	      try 
		ignore (NumHashtbl.find hp2s p);
		NumHashtbl.add hp2s p (id,i);
		l
	      with Not_found -> begin
		NumHashtbl.add hp2s p (id,i);
		p::l
	      end
	) [] c in
	let newc = List.fold_left (
	  fun l p -> 
	    let sigs = NumHashtbl.find_all hp2s p in
	    let sigs_id = List.sort compare (List.map fst sigs) in
	      try 
		let newsigs_id = Hashtbl.find SigMac.collisions sigs_id in
		  List.iter (fun (id,i) -> print_signal id (get_start_point i) (newt,p) n) sigs;
		  (List.map (fun id -> (p,id,(newt,p))) newsigs_id)@l
	      with Not_found -> (List.map (fun (id,i) -> (p,id,get_start_point i)) sigs)@l
	) [] positions in
	  (newt,newc)
      end
    
  let rec run_conf : int -> config -> config = fun n conf ->
    if n = 0 then conf
    else run_conf (n-1) (step conf n)
  let run : int -> unit = fun n ->
    print_tikz_header ();
    let (t,c) = run_conf n (num0, (List.map (fun (p,id) -> (p,id,(num0,p))) SigMac.init_conf)) in
      if t </ !tmax then force_draw_to c (!tmax)
      else force_draw_to c t;
      print_tikz_footer ()
end;;



(* MAIN *)
module MyMachine = SiMaLator(MachineMaker(ParseSMDesc));;
(*module MyTree = SiMaLator(MachineMaker(Tree));;*)

let main () =
  if (Array.length Sys.argv)<=3 then 
    prerr_endline "Arguments : nb_iter tps_max label_iter_inf"
  else begin
    MyMachine.configure_output (parse_num Sys.argv.(2)) (int_of_string Sys.argv.(3));
    MyMachine.run (int_of_string Sys.argv.(1))
  end;;

main ();;
