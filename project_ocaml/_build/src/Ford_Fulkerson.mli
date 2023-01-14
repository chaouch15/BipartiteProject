open Graph
val path : int graph -> id -> id -> id list 
val print_id : id list -> unit
val find_min : int graph -> id list ->  int
val add_flow : int graph -> id list -> int -> int graph
val flow_max : int graph -> id -> id -> int 




(*let find_path g id1 id2 = 

  let rec loop acu r = match r with
    |[] -> []
    |(id,lbl) :: rest when id = id2 && lbl != 0 -> id2 :: acu 
    |(idx,lblx) :: rest -> if (List.mem idx acu) || (lblx = 0)
      then loop acu rest 
      else match (loop (idx::acu) (out_arcs g idx)) with
        |[] -> loop acu rest
        |x -> x
  in

  let rslt = loop [id1] (out_arcs g id1) in
  match (List.rev rslt) with
    |[id] when id = id1 -> []
    |x -> x
;;*)