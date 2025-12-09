let get_distance p1 p2 =
  let rec aux p1 p2 acc =
    match p1, p2 with
    | [], [] -> acc
    | x::xs, y::ys ->
        let d = (x-y) in aux xs ys (acc + d*d)
    | _ -> invalid_arg "different lengths"
  in aux p1 p2 0
(* let get_distance p1 p2 =
  let sq x = x *. x in
  sqrt (
    List.fold_left (+.) 0.0
      (List.map2 (fun x y ->
         let dx = float x -. float y in sq dx) p1 p2)
  ) *)

type edge = int list * int list
let rec compare_list a b =
  match a, b with
  | [], [] -> 0
  | x::xs, y::ys ->
      let c = compare x y in
      if c <> 0 then c else compare_list xs ys
  | [], _ -> -1
  | _, [] -> 1

let compare_edge (p1,p2) (p1',p2') =
  let d1 = get_distance p1 p2 in
  let d2 = get_distance p1' p2' in
  let cd = compare d1 d2 in
  if cd <> 0 then cd
  else
    let c1 = compare_list p1 p1' in
    if c1 <> 0 then c1
    else compare_list p2 p2'

module MinEdgeQueue = Pqueue.MakeMin(struct
  type t = edge
  let compare = compare_edge
end)

module EdgeSet = Set.Make(struct
  type t = edge
  let compare = compare_edge
end)

let get_pq points =
  let pq = MinEdgeQueue.create () in
  let rec aux = function
    | [] -> pq
    | x :: xs ->
        let rec aux' = function
          | [] -> ()
          | y :: ys ->
              if x = y then
                aux' ys
              else begin
                MinEdgeQueue.add pq (x, y);
                aux' ys
              end
        in
        aux' points;
        aux xs
  in
  aux points

let ht_to_string ht = 
  let keys = Hashtbl.to_seq_keys ht in
  List.fold_left (
    fun acc a -> 
      Printf.sprintf "%s(%d) -> %d\n" acc a (Hashtbl.find ht a)
    ) "" (List.of_seq keys)

let connected_to_the_same ht p1 p2 = Hashtbl.mem ht p1 && Hashtbl.mem ht p2 && Hashtbl.find ht p1 = Hashtbl.find ht p2

let get_n_max n l = let sorted = List.sort Stdlib.compare l in
let rec aux i res = function
| [] -> res
| x::xs -> if i <= n then aux (i-1) (x::res) xs else aux (i-1) res xs
    in aux (List.length l) [] sorted

let get_connected_groups points =
  let edge_queue = get_pq points in
  (* let p1, p2 = MinEdgeQueue.get_min_elt edge_queue in Utils.print_int_list p1; Utils.print_int_list p2; *)
  let count_ht = List.length points |> Hashtbl.create in
  let ht = List.length points |> Hashtbl.create in
  let rec aux i made_connections = if made_connections >= 9 then () else if MinEdgeQueue.is_empty edge_queue then ()
  else let e = MinEdgeQueue.pop_min edge_queue in
  if e = None then raise (Invalid_argument "aaa")
  else
    let (p1, p2) = Option.get e in Utils.print_int_list p1;
    let added =
      let in_ht1 = Hashtbl.mem ht p1 in
      let in_ht2 = Hashtbl.mem ht p2 in
      match in_ht1, in_ht2 with
      | true, true ->
          let id1 = Hashtbl.find ht p1 in
          let id2 = Hashtbl.find ht p2 in
          if id1 = id2 then false  (* same component, do nothing *)
          else
            (* merge id2 into id1 *)
            let size1 = Hashtbl.find count_ht id1 in
            let size2 = Hashtbl.find count_ht id2 in
            (* update all points of id2 to id1 *)
            Hashtbl.iter (fun key v -> if v = id2 then Hashtbl.replace ht key id1) ht;
            Hashtbl.replace count_ht id1 (size1 + size2);
            Hashtbl.remove count_ht id2;
            true
      | true, false ->
          let id = Hashtbl.find ht p1 in
          Hashtbl.add ht p2 id;
          Hashtbl.replace count_ht id ((Hashtbl.find count_ht id) + 1);
          true
      | false, true ->
          let id = Hashtbl.find ht p2 in
          Hashtbl.add ht p1 id;
          Hashtbl.replace count_ht id ((Hashtbl.find count_ht id) + 1);
          true
      | false, false ->
          (* new connection *)
          Hashtbl.add ht p1 i;
          Hashtbl.add ht p2 i;
          Hashtbl.add count_ht i 2;
          true
    in
    if added then aux (i+1) (made_connections+1) else aux i made_connections  
  in aux 0 0; print_string (count_ht |> ht_to_string); List.fold_left (fun acc a -> a * acc) 1 (get_n_max 3 (List.of_seq (Hashtbl.to_seq_values count_ht)))

let get_points lines =
  let rec aux res = function
  | [] -> res
  | l::lx -> let a = String.split_on_char ',' l in
    let rec get_int_list = function
    | [] -> []
    | c::cs -> (int_of_string c)::(get_int_list cs)
  in aux ((get_int_list a)::res) lx
in aux [] lines