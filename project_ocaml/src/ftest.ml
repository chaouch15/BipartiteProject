open Gfile
open Tools
open Ford_Fulkerson
open Graph


let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)


  (* These command-line arguments are not used for the moment. *)
  and source =int_of_string Sys.argv.(2)
  and sink= int_of_string Sys.argv.(3)
  in 
  let problem = from_file infile in 
  let s = source0 empty_graph in
  let d = dest1 s in
  let graph1= extremite0 problem d in
  let graph2 = read_node graph1 problem in
  Printf.printf "avant \n ";
  let graph3 = read_arc graph2 problem in 
  Printf.printf "apres \n ";
  let graph4 = extremite1 problem graph3 in
  let flow = flow_max graph4 source sink in
  let graph = gmap graph4 string_of_int in
  Printf.printf "%d %!\n " flow;
  let () = export outfile graph in ()

  (*let dic = diction problem in 
  Printf.printf "%d %! \n " (List.length dic);
  ()*)
  (* Rewrite the graph that has been read. *)

 

  (*let loop p = match p with
    |(x::restx, y::resty, z::restz) -> Printf.printf "%s %s %s %! \n" x y z
    |_ -> assert false*)


  (********************test Fulkerson*********)

  (*let graph = from_file infile in
  let int_graph = gmap graph int_of_string in*)
  (*let path = path int_graph source sink in*)

  (*let rec print t = match t with
    |[] -> ()
    |x::rest -> Printf.printf "%d " x ; print rest
  in*)

  (*let min = min_arc_path int_graph path in
  let gr2 = iter_FF int_graph path min in
  let gr3 = gmap gr2 string_of_int in 

  let min = min_arc_path int_graph path in
  let gr2 = iter_FF int_graph path min in
  let gr3 = gmap gr2 string_of_int in *)
  (*let gr3 = gmap int_graph string_of_int in*)
  (*let flow = flow_max int_graph source sink in
  let gr3 = gmap int_graph string_of_int in
  Printf.printf "%d %!\n " flow;


  (* Rewrite the graph that has been read. *)
  let () = export outfile gr3 in ()*)




  (*and list = 
    List.mapi (fun i _ -> 
      (int_of_string Sys.argv.(i), List.map int_of_string (Str.split (Str.regexp " ") Sys.argv.(i+1))))( Array.to_list Sys.argv)
    in
     let graph = gmap  (create_node_pple list) string_of_int in 
  let ()= export outfile graph in ()*)
