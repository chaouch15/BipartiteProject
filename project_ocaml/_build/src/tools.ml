open Graph
(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = n_fold gr new_node empty_graph


let gmap gr f = e_fold gr (fun acu id1 id2 lbl->new_arc acu id1 id2 (f lbl)) (clone_nodes gr) 

let add_arc g id1 id2 n =
    match find_arc g id1 id2 with
    | None -> new_arc g id1 id2 n
    | Some l ->  new_arc g id1 id2 (n+l)

                