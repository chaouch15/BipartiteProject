open Graph
open Printf
    
type path = string

(* Format of text files:
   % This is a comment

   % A node with its coordinates (which are not used), and its id.
   n 88.8 209.7 0
   n 408.9 183.0 1

   % Edges: e source dest label id  (the edge id is not used).
   e 3 1 11 0 
   e 0 2 8 1

*)

(* Compute arbitrary position for a node. Center is 300,300 *)
let iof = int_of_float
let foi = float_of_int

let index_i id = iof (sqrt (foi id *. 1.1))

let compute_x id = 20 + 180 * index_i id

let compute_y id =
  let i0 = index_i id in
  let delta = id - (i0 * i0 * 10 / 11) in
  let sgn = if delta mod 2 = 0 then -1 else 1 in

  300 + sgn * (delta / 2) * 100
  

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "%% This is a graph.\n\n" ;

  (* Write all nodes (with fake coordinates) *)
  n_iter_sorted graph (fun id -> fprintf ff "n %d %d %d\n" (compute_x id) (compute_y id) id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  let _ = e_fold graph (fun count id1 id2 lbl -> fprintf ff "e %d %d %d %s\n" id1 id2 count lbl ; count + 1) 0 in
  
  fprintf ff "\n%% End of graph\n" ;
  
  close_out ff ;
  ()

let export path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "digraph finite_state_machine {\n" ;
  fprintf ff "fontname=\"Helvetica,Arial,sans-serif\"\nnode [fontname=\"Helvetica,Arial,sans-serif\"]\nedge [fontname=\"Helvetica,Arial,sans-serif\"]\nrankdir=LR;\nnode [shape = doublecircle]; 0 3 4 8;\nnode [shape = circle];\n";  
  (* Write all nodes (with fake coordinates) *)
  e_iter graph (fun id1 id2 label -> fprintf ff "%.1d -> %.1d [label = \"%s\"];\n" id1 id2 label);
  fprintf ff "\n" ;

  fprintf ff "}\n" ;
  
  close_out ff ;
  ()

  (********************new**************************************************************************************)

  type problem =
  { employees: string list ;
    jobs: string list ;
    voeux: (string * string) list }

(* let node_id (node_name: string) -> int *)

let append_item lst a = lst @ [a]

let read_ff_e (prob :problem) line= try 
Scanf.sscanf line " e %s "(fun name ->  { prob with employees = append_item prob.employees name});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"

let read_ff_j (prob :problem) line= try 
Scanf.sscanf line " j %s " (fun  job_name ->  { prob with jobs = append_item prob.jobs job_name} );
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"

let read_ff_v (prob :problem) line= try 
Scanf.sscanf line " v %s %s "(fun name voeu->  { prob with voeux = append_item prob.voeux (name,voeu)});
with e ->
 Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
 failwith "from_file"


 let rec iter_list list n i  =  match list with 
    |[] -> i
    |id:: tail -> if id=n then i else iter_list tail n (i+1)
 
 let node_id (node_name: string) (recu : problem)  = match List.exists (fun i -> i=node_name) recu.employees with 
  |true ->  iter_list recu.employees node_name 2
  |false -> let len= List.length recu.employees in 
  iter_list recu.jobs node_name (len+2)

(*********crer le dictionnaire qui aura l'association entre name and id ********)
let  diction1 ( recu : problem ) =
  let rec aux liste= match liste with 
  |[] -> []
  |x:: rest ->  (x,(node_id x recu)) :: aux rest
in 
aux recu.employees

let diction2 (recu : problem) = 
  let rec aux liste=match liste with 
  |[] -> []
  |x :: rest -> (x,(node_id x recu )) :: aux rest 
in 
aux recu.jobs

let diction prob = List.append (diction1 prob) (diction2 prob)


let source0 gr = (new_node gr 0)
let dest1 gr =   (new_node gr 1)

let extremite0 problem gr =
  let rec aux gr1 liste = match liste with
    |[] -> gr1
    |(x,y) :: rest -> aux (new_arc gr1 0 y 1)  rest 
in 
aux gr (diction1 problem )

let extremite1 problem gr =
  let rec aux liste gr1= match liste with
    |[] -> gr1
    |(x,y) :: rest -> aux rest (new_arc gr1 y 1  1) 
in 
aux (diction2 problem) gr


(**********creer le graphe à partir de notre problème  **************)
let converter (x,id) graph = new_node graph id

let rec read_node graph prob = 
  let rec aux graph1 liste = match liste with 
|[] -> graph1
|(x,y) :: rest -> aux (new_node graph1 y) rest 
in 
aux graph (diction prob)


let read_arc graph problem  =
  let rec aux graph1 liste = match liste with 
|[] -> graph1
|(x,y) :: rest -> let x1 = List.assoc x (diction problem)
and y1 = List.assoc y (diction problem) 
in 
Printf.printf "%d %d%! \n "y1 x1;
aux (new_arc graph1 x1 y1 1) rest 
in 
aux graph problem.voeux

  (**************end new*******************************************************************************************)
(* Reads a line with a node. *)

(*let read_node_e graph problem line =try
   Scanf.sscanf line " e %s " (fun name  id -> new_node graph (node_id id problem))
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let read_node_j graph problem line =try
      Scanf.sscanf line " j %s %d" (fun _ name id -> new_node graph (node_id id problem ))
     with e ->
       Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
       failwith "from_file"
  

(* Ensure that the given node exists in the graph. If not, create it. 
 * (Necessary because the website we use to create online graphs does not generate correct files when some nodes have been deleted.) *)
let ensure graph id = if node_exists graph id then graph else new_node graph id

(* Reads a line with an arc. *)
let read_arc graph problem line =
  try Scanf.sscanf line "v %s %s %_d %s@%%"
        (fun id1 id2 label -> new_arc (ensure (ensure graph  (node_id id1 problem) ) (node_id id2 problem) )  (node_id id1 problem) (node_id id2 problem)  label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"*)

(* Reads a comment or fail. *)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(***********************new******************************************************)
let from_file path =
  let infile = open_in path in 

  let rec loop problem1 =
    try
      let line = input_line infile in 

      let line = String.trim line in

      let problem2 =
        (* Ignore empty lines *)
        if line = "" then problem1

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
        |'e' -> read_ff_e problem1 line
        |'j' -> read_ff_j problem1 line
        |'v' -> read_ff_v problem1 line
        | _ -> read_comment problem1 line
      in 
      loop problem2
    with End_of_file -> problem1
  in

  let problem3 =
  { employees = [] ;
    jobs = [];
    voeux = [] } in 

  let final_problem1 = loop problem3 in
  close_in infile ;
  final_problem1  

  (******************************End new ***************************************************)

(*let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : n or e. *)
        else match line.[0] with
          |'e'-> read_node_e graph final_problem line
          |'j' -> read_node_j graph final_problem line
          |'v' -> read_arc graph final_problem line

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment graph line
      in      
      loop graph2

    with End_of_file -> graph (* Done *)
  in
  let final_graph = loop empty_graph in
  close_in infile ;
  final_graph*)
  
