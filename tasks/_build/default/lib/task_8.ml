let get_distance p1 p2 =
  let rec aux p1' p2' res = match (p1', p2') with
  | ([], []) -> sqrt (float_of_int res)
  | (_, []) | ([], _) -> raise (Invalid_argument "arrays have different dimensions")
  | (x::xs, y::ys) -> aux xs ys ((y-x) * (y-x) + res)
in aux p1 p2 0

type edge = int list * int list
let compare_edge e1 e2 = let (p1, p2), (p1', p2') = e1, e2 in if get_distance p1 p2 >= get_distance p1' p2' then 1 else 0

module MinEdgeQueue = Pqueue.MakeMin(struct
  type t = edge
  let compare = compare_edge
end)

module EdgeSet = Set.Make(struct
  type t = edge
  let compare = compare_edge
end)

let a = MinEdgeQueue.create ()
let b = MinEdgeQueue.of_array [|([1; 2], [2; 3]);([1; 2], [4; 5])|]

let get_pq points =
  let pq = MinEdgeQueue.create () in
  let seen = EdgeSet.empty in
  let rec aux = function
  | [] -> pq
  | x::xs -> let rec aux' = function
    | [] -> ()
    | y::ys -> if x = y || EdgeSet.mem (y, x) seen then aux' ys
    else MinEdgeQueue.add pq (x, y); let _ = EdgeSet.add (x, y) seen in aux' ys in
    aux' points; aux xs in
  aux points

let ht_to_string ht = 
  let keys = Hashtbl.to_seq_keys ht in
  List.fold_left (
    fun acc a -> 
      Printf.sprintf "%s(%d) -> %d\n" acc a (Hashtbl.find ht a)
    ) "" (List.of_seq keys)

let get_connected_groups points =
  let edge_queue = get_pq points in
  let count_ht = List.length points |> Hashtbl.create in
  let ht = List.length points |> Hashtbl.create in
  let rec aux i = if MinEdgeQueue.is_empty edge_queue then ()
  else let e = MinEdgeQueue.pop_min edge_queue in
  if e = None then raise (Invalid_argument "aaa")
  else let (p1, p2) = Option.get e in
  (* Printf.printf "%d/n" (MinEdgeQueue.length edge_queue); *)
    let added =
    if Hashtbl.mem ht p1 then
      let () = Hashtbl.add ht p2 (Hashtbl.find ht p1); Hashtbl.replace count_ht i ((Hashtbl.find count_ht (Hashtbl.find ht p1)) + 1) in false
    else if Hashtbl.mem ht p2 then
      let () = Hashtbl.add ht p1 (Hashtbl.find ht p2); Hashtbl.replace count_ht i ((Hashtbl.find count_ht (Hashtbl.find ht p2)) + 1) in false
    else
      let () = Hashtbl.add ht p1 i; Hashtbl.add ht p2 i; Hashtbl.add count_ht i 1 in true
    in if added then aux (i+1) else aux i
  in aux 0; print_string (count_ht |> ht_to_string)

let get_points lines =
  let rec aux res = function
  | [] -> res
  | l::lx -> let a = String.split_on_char ',' l in
    let rec get_int_list = function
    | [] -> []
    | c::cs -> (int_of_string c)::(get_int_list cs)
  in aux ((get_int_list a)::res) lx
in aux [] lines