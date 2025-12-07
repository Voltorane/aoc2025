type op = Add | Mul
module IntSet = Set.Make(Int);;

let string_of_list l =
  let rec aux res = function
| [] -> res^"]"
| x::xs -> aux (Printf.sprintf "%s %d," res x ) xs
in aux "[" l

let ht_to_string ht = 
  let keys = Hashtbl.to_seq_keys ht in
  List.fold_left (
    fun acc a -> 
      let vals = Hashtbl.find ht a 
      in Printf.sprintf "%s(%d) -> %s\n" acc a (string_of_list vals)
    ) "" (List.of_seq keys)

let _op_of_string = function
| "+" -> Add
| "*" -> Mul
| _ -> raise (Invalid_argument "Invalid op string")

let rec _get_op_list = function
| [] -> []
| x::xs -> if x = "" then _get_op_list xs else (_op_of_string x)::(_get_op_list xs)

let rec _get_int_list = function
| [] -> []
| x::xs -> if x = "" then _get_int_list xs else (int_of_string x)::(_get_int_list xs)

let _get_op_ids op op_list =
  let rec aux i curr = function
  | [] -> curr
  | x::xs -> if x = op then aux (i+1) (i::curr) xs
  else aux (i+1) curr xs
in aux 0 [] op_list

let _make_op_ht op_index_list =
  let ht = Hashtbl.create (List.length op_index_list) in
  let rec aux = function
    | [] -> ht
    | x::xs -> Hashtbl.add ht x []; aux xs in
  aux op_index_list

let _populate_mul_ht_and_get_other_number_sum numbers cols ht =
  let rec aux i sum = function
  | [] -> (ht, sum)
  | x::xs -> let col = i mod cols in
  if (Hashtbl.mem ht col) then 
    let () =  Hashtbl.replace ht col (x::(Hashtbl.find ht col)) in
  aux (i+1) sum xs
  else aux (i+1) (x+sum) xs
in aux 0 0 numbers

let _get_results numbers operations =
  let cols = List.length operations in
  let mul_ids = _get_op_ids Mul operations in
    let (mul_ht, other_num_sum) = _populate_mul_ht_and_get_other_number_sum numbers cols (_make_op_ht mul_ids) in
    let rec aux curr = function
    | [] -> curr
    | id::xs -> let res = List.fold_left (fun acc a -> acc * a) 1 (Hashtbl.find mul_ht id) in
      aux (res+curr) xs
in other_num_sum + (aux 0 mul_ids)

let _read_input file_name =
  let lines = Utils.read_lines file_name in
  if lines = [] then raise (Invalid_argument "invalid lines") 
  else
    let rec aux numbers = function
    | [] -> raise (Invalid_argument "invalid")
    | [s] -> numbers, (_get_op_list (String.split_on_char ' ' s))
    | s::xs -> aux ((_get_int_list (String.split_on_char ' ' s))@numbers) xs
in aux [] lines

let _get_operations file_name = let _, ops = _read_input file_name in ops
let _get_num_start_ids file_name =
  let rec aux = function
  | [] -> raise (Invalid_argument "invalid")
  | [s] -> let rec finds_start_ids i res = function
    | [] -> res
    | x::xs -> if x != ' ' then finds_start_ids (i+1) (i::res) xs
    else finds_start_ids (i+1) res xs
  in finds_start_ids 0 [] (List.of_seq (String.to_seq s))
  | _::ss -> aux ss 
in IntSet.of_list (aux (Utils.read_lines file_name))


let _int_list_of_char_list char_list placeholder =
  let rec aux curr = function
  | [] -> curr
  | x::xs -> if x = ' ' then aux (placeholder::curr) xs else aux (((int_of_char x) - 48)::curr) xs
in aux [] char_list

let _make_number_ht number_lines num_start_ids ht =
  (* let ht = Hashtbl.create cols in *)
  let rec for_lines = function
  | [] -> ()
  | l::lx ->
    let rec aux i col = function
    | [] -> ()
    | x::xs -> if not (IntSet.mem i num_start_ids) then aux (i+1) col xs else
      Printf.printf "%d %d %s %s %s\n" i col l x (ht_to_string ht);
      let exists = Hashtbl.mem ht col in 
      let placeholder = -1(* if exists then -1 else 0*) in
      let new_int_list = _int_list_of_char_list (List.of_seq (String.to_seq x)) placeholder in
      let () = 
        if not(exists) then
          (*initialize nums*)
          Hashtbl.add ht col new_int_list
        else
            let rec get_updated_digits existing_list new_list res = match (existing_list, new_list) with
            | ([], []) -> res
            | (e::es, n::ns) -> if n = -1 then get_updated_digits es ns (e::res) else get_updated_digits es ns ((e*10 + n)::res)
            | (_, _) -> raise (Invalid_argument "existing and new lists have different dimensions")
          in Printf.printf "replacing %d\n" col; Hashtbl.replace ht col (get_updated_digits (Hashtbl.find ht col) new_int_list [])
      in aux (i+1) (col+1) xs
    in aux 0 0 (String.split_on_char ' ' l); for_lines lx
  in for_lines number_lines; ht

(*skipping last line*)
let _get_number_lines file_name =
  let lines = Utils.read_lines file_name in
  let rec aux res = function
  | [_] | [] -> res
  | x::xs -> aux (x::res) xs
in aux [] lines

(* let _get_int_list_2 *)
let _num_start_ids = _get_num_start_ids "inputs/test_input_6"
let _numbers_list = _get_number_lines "inputs/test_input_6"
let _cols = List.length (_get_operations "inputs/test_input_6")
let _ht :((int, int list)Hashtbl.t) = Hashtbl.create 12345