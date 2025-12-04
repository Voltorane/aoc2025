(* let count_digit n _ = if n = 0 then 1 else
  let rec helper t = if t = 0 then 0 else 1 + helper (t / 10) in
  helper n
let rec pow x n = match n with 
| 0 -> 1
| _ -> x * (pow x (n-1))
let _is_repeated n num_digits = n mod (pow 10 (num_digits / 2)) - n / (pow 10 (num_digits / 2)) == 0
let rec consists_only_of n len_n q = if n = q then true else let len_q = (count_digit q 0) in
(* Printf.printf("c: %d %d %d %d\n") n q len_n len_q; *)
if len_n = 1 then n = q else
 if len_n mod len_q != 0 then false
  else match len_n with
  | 0 -> true
  | _ -> if n mod (pow 10 (len_q)) = q then consists_only_of (n / (pow 10 (len_q))) (len_n - len_q) q 
  else false

(* I need to handle  *)
let _only_repeated n = if count_digit n 0 = 1 then false else
  let rec helper n q curr =
    (* Printf.printf("%d %d %d\n") n q curr;  *)
    if count_digit q 0 > (count_digit n 0) / 2 then false
    else let repeated = consists_only_of n (count_digit n 0) q in
      if repeated then true
      else helper n (n / pow 10 ((count_digit n 0) - curr)) ((count_digit q 0) + 1)
in if n < 10 then true else helper n (n / pow 10 ((count_digit n 0))) 1
let is_invalid n = _only_repeated n
let rec sum_invalid_in_range curr e curr_sum = 
  if curr = e+1 then curr_sum
  else
    if is_invalid curr then let () = Printf.printf "invalid %d \n" curr in sum_invalid_in_range (curr+1) e (curr_sum+curr)
    else sum_invalid_in_range (curr+1) e curr_sum
let rec _sum_invalid_across_ranges ranges curr_sum = match ranges with
| [] -> curr_sum
| (s,e)::xs -> _sum_invalid_across_ranges xs curr_sum+(sum_invalid_in_range s e 0)
let read (file_name : string) : string =
  In_channel.with_open_text file_name In_channel.input_all

let _ranges = 
  let l = String.split_on_char ',' (read "../input_2") in
    let rec get_ranges arr curr = match arr with
    | [] -> curr
    | x::xs -> let split = (String.split_on_char '-' x) in
      let first, second = ((int_of_string (List.nth split 0), int_of_string (List.nth split 1))) in
        get_ranges xs ((first, second)::curr) in
          get_ranges l []


          let () = print_newline ()
          
          
let () = print_int (_sum_invalid_across_ranges _ranges 0)
(* let () = Printf.printf("%b\n") (_only_repeated 99099) *)
let () = Printf.printf("%b\n") (consists_only_of 121212 6 12) *)
let rec _print_int_list l = match l with
| [] -> ()
| x::xs -> Printf.printf "%d, " x; _print_int_list xs
let rec _print_int_list_with_indexes l = match l with
| [] -> ()
| (x,i)::xs -> Printf.printf "(%d; %d)" x i; _print_int_list_with_indexes xs

let int_to_list n = if n = 0 then [0] else
  let rec helper m = match m with
  | 0 -> []
  | _ -> (m mod 10)::(helper (m / 10))
in List.rev (helper n)

(* let list_with_indexes l =
  let rec helper l_1 i curr =  match l_1 with
| [] -> curr
| x::xs -> helper xs (i-1) ((x, i)::curr)
in helper (List.rev l) ((List.length l) - 1) []

let compare_int_w_index (a, _) (b, _) = if a = b then 0 else if a >= b then 1 else -1 
let sort_list l rev = 
  let sorted = List.sort compare_int_w_index l
in if rev then List.rev sorted else sorted *)

(* let sorted_list = sort_list (list_with_indexes (int_to_list 987654321111111)) true *)
(* let sorted_list = sort_list (list_with_indexes (int_to_list 8119)) true
let () = _print_int_list_with_indexes sorted_list; print_newline () *)

let combine_digits l r = l * 10 + r

let max a b = if a >= b then a else b

(* let get_max_num l = 
  let rec helper l_1 r_1 = match l_1,r_1 with
  | ([],[]) -> Printf.printf "invalid lists"; -1
  | ((x, _)::_, []) -> x
  | ([], (x, _)::_) -> x
  | ((x, i)::xs),((y, j)::ys) -> Printf.printf "(%d %d) (%d %d)\n" x i y j;
    if i < j then (combine_digits x y) else if i = j then 0 else
      max (helper ((x, i)::xs) ys) (helper xs ((y,j)::ys)) 
in helper l (List.filter (fun (_, i) -> i != 0) l) *)

(* let () = print_int (get_max_num sorted_list) *)
(* let () = _print_int_list_with_indexes (List.filter (fun (_, i) -> i != 0) sorted_list) *)
(* let get_max_num_actual l =
  let rec helper list max_val next_max = match list with
  | [] -> combine_digits max_val next_max
  | x::xs -> if x > max_val then helper xs x max_val else helper xs max_val (max x next_max)
in helper (List.drop 2 (List.rev l)) (List.nth (List.rev l) 1) (List.nth (List.rev l) 0) *)
(* let sorted l = List.sort (fun (x, i) (y, j) -> if x > y && i < j then 1 else -1) *)

(* let () = print_int (get_max_num_actual (int_to_list 8119)) *)

(* 
1. find max between start and end-k
2. continue from max_index and k-1
*)
let max_with_ind l = 
  let rec helper curr_max curr_max_i i = function
| [] -> (curr_max, curr_max_i)
| x::xs -> let res = max x curr_max in
if res > curr_max then helper res i (i-1) xs
else helper curr_max curr_max_i (i-1) xs
in helper (List.nth l 0) 0 0


let get_max_k_digit_num k l = 
  let rec acc k digits_left = function
  | [] -> let k_list = List.rev (List.drop (k-1) (List.rev l)) in
    let x, i = max_with_ind k_list in
