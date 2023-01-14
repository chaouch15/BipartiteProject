open Graph
open Tools

let rec find_path gr s d acu1 li_out_arcs acu = 
  let acu = s::acu in
   match li_out_arcs with
|[]-> []
|(head,lbl)::tail when d==head && lbl !=0-> List.rev(d::acu1)
|(head,lbl)::tail when (List.mem head acu) ||(lbl=0 )-> find_path gr s d (acu1) tail (head::acu)
|(head,lbl)::tail->
  if (find_path gr head d (head::acu1) (out_arcs gr head) (head::acu) == [])  then  
    find_path gr s d (acu1) tail (head::acu) else find_path gr head d (head::acu1) (out_arcs gr head) (head::acu)

let path gr s d = find_path gr s d [s] (out_arcs gr s) []

let rec find_flow_min gr path min = match path with
  |[]->  min
  |[id] -> min
  |id1::id2::tail->  (*Printf.printf " %d : %!" id1;*) match find_arc gr id1 id2 with
  | None -> assert false
  | Some lbl ->(* Printf.printf "%d : %!" lbl;*) if (lbl) < min then 
     find_flow_min gr (id2::tail) (lbl) else find_flow_min gr (id2::tail) min 

let find_min gr path =  (* Printf.printf " : %! "  ;*) find_flow_min gr path max_int

let rec print_id li_id = match li_id with
  |[] -> Printf.printf ".\n%!"
  |id :: tail -> Printf.printf "%d :\n %!" id ;print_id tail


let rec add_flow gr path v  = match path with 
  | [lbl] -> gr
  |id1::id2::tail ->
  add_flow  (add_arc (add_arc gr id1 id2 (-v)) id2 id1 v) (id2::tail) v
  |[] -> assert false



let flow_max gr s d  = 
  let rec aux path_found gr1 flow =
    print_id path_found;
    match path_found with 
      |[]-> flow
      |chem ->  let min = find_min gr1 chem in 
      let graph_aux = add_flow gr1 chem min in
        aux (path graph_aux s d) graph_aux (flow + min)
in
aux (path gr s d) gr 0

