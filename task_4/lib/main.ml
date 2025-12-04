let rec _print_int_list l = match l with
| [] -> ()
| x::xs -> Printf.printf "%d, " x; _print_int_list xs
let rec _print_int_list_with_indexes l = match l with
| [] -> ()
| (x,i)::xs -> Printf.printf "(%d; %d)" x i; _print_int_list_with_indexes xs
let rec _pow x n = match n with 
| 0 -> 1
| _ -> x * (_pow x (n-1))
let _int_to_list n = if n = 0 then [0] else
  let rec helper m = match m with
  | 0 -> []
  | _ -> (m mod 10)::(helper (m / 10))
in List.rev (helper n)

let _combine_digits l r = l * 10 + r

let _max a b = if a >= b then a else b
let _explode_string s = List.init (String.length s) (String.get s);;
let _string_to_int_list s = 
  let l = _explode_string s in
    let rec aux = function
        | [] -> []
        | x::xs -> (int_of_char x) - 48 :: (aux xs)
  in aux l
(* 
1. find max between start and end-k
2. continue from max_index and k-1
*)
let _max_with_ind l = 
  let rec helper curr_max curr_max_i i = function
    | [] -> (curr_max, curr_max_i)
    | x::xs -> let res = max x curr_max in
      if res > curr_max then helper res i (i+1) xs
      else helper curr_max curr_max_i (i+1) xs
  in helper (List.nth l 0) 0 0 l

let _list_to_int l =
  let rec aux curr i = function
    | [] -> curr
    | x::xs -> aux (x*(_pow 10 i)+curr) (i+1) xs
in aux 0 0 (List.rev l)
let _drop_last n l = (List.rev (List.drop (n)(List.rev l)))
let _get_max_k_digit_num k s =
  let l = _string_to_int_list s in
    let rec aux curr_list digits_left = match digits_left with
    | 1 -> let (x, _) = (_max_with_ind curr_list) in [x]
    | _ -> let next_digit, next_start = _max_with_ind (_drop_last (digits_left-1) curr_list) in
    next_digit::(aux (List.drop (next_start+1) curr_list) (digits_left-1))
in _list_to_int (aux l k)
let _read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines
let rec _get_sum_joltage_k k = function
| [] -> 0
| x::xs -> (_get_max_k_digit_num k x) + (_get_sum_joltage_k (k-1) xs)

let get_flat_list file_name: int * int * char list = let l = _read_lines file_name in
  (String.length (List.nth l 0)), (List.length l), (List.fold_left (fun acc a -> ((_explode_string a)@acc)) [] l)

let _get_hashtable char_list =
  let rec aux i table = function
  | [] -> table
  | x::xs -> let is_occupied = x = '@' in Hashtbl.add table i is_occupied; aux (i+1) table xs
in aux 0 (Hashtbl.create (List.length char_list)) char_list

let is_surrounding_free x y w h ht =
  (* Printf.printf "x: %d y: %d \n" x y; *)
    let has_right = (*Printf.printf "%d \n" (w*y+(x+1));*) if x < w-1 && Hashtbl.find ht (w*y+(x+1)) then 1 else 0 in
    let has_left = (*print_string "b";*) if x > 0 && Hashtbl.find ht (w*y+(x-1)) then 1 else 0 in
    let has_up = (*print_string "c";*) if y > 0 && Hashtbl.find ht (w*(y-1)+x) then 1 else 0 in
    let has_dn = (*print_string "d";*) if y < h-1 && Hashtbl.find ht (w*(y+1)+x) then 1 else 0 in
    let has_lu = (*print_string "e";*) if x > 0 && y > 0 && Hashtbl.find ht (w*(y-1)+(x-1)) then 1 else 0 in
    let has_ld = (*print_string "f";*) if x > 0 && y < h-1 && Hashtbl.find ht (w*(y+1)+(x-1)) then 1 else 0 in
    let has_ru = (*print_string "g";*) if x < w-1 && y > 0 && Hashtbl.find ht (w*(y-1)+(x+1)) then 1 else 0 in
    let has_rd = (*print_string "h";*) if x < w-1 && y < h-1 && Hashtbl.find ht (w*(y+1)+(x+1)) then 1 else 0 in
    if has_right + has_left + has_dn + has_up + has_lu + has_ld + has_ru + has_rd < 4 then true else false

let _get_free_sum w h char_list =
  let ht = _get_hashtable char_list in
    let rec aux x y new_char_list curr = function
    | [] -> (curr, new_char_list)
    | c::xc -> let next_x, next_y = if x = w-1 then 0, y+1 else x+1, y in
      if c = '@' && is_surrounding_free x y w h ht then
        let () =  Hashtbl.replace ht (h*y+x) false 
      in aux next_x next_y ('.'::new_char_list) (curr+1) xc
      else aux next_x next_y (c::new_char_list) curr xc
    in aux 0 0 [] 0 char_list

let w, h, list = (get_flat_list "input_4")

let rec _get_max_removed w h char_list =
  let (removed, new_char_list) = _get_free_sum w h char_list in
  if removed = 0 then 0 else removed + _get_max_removed w h new_char_list

let _print_hashtable ht = Hashtbl.iter (fun x y -> Printf.printf "%s -> %s\n" (string_of_int x) (string_of_bool y)) ht;;