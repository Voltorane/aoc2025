type op = Add | Mul

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

      