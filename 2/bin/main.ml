let rec count_digit n curr = match n with
| 0 -> curr
| _ -> count_digit (n / 10) (curr + 1)
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
let _only_repeated n =
  let rec helper n q curr =
    (* Printf.printf("%d %d %d\n") n q curr;  *)
    if count_digit q 0 > (count_digit n 0) / 2 then false
    else let repeated = consists_only_of n (count_digit n 0) q in
      if repeated then true
      else helper n (n / pow 10 ((count_digit n 0) - curr)) ((count_digit q 0) + 1)
in if n < 10 then true else helper n (n / pow 10 ((count_digit n 0)-1)) 1
let is_invalid n = _only_repeated n
  (* let num_digits = count_digit n 0 in
    if num_digits mod 2 == 0 then
      is_repeated n num_digits
    else
      false *)
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
  let l = String.split_on_char ',' (read "../test_input_2") in
    let rec get_ranges arr curr = match arr with
    | [] -> curr
    | x::xs -> let split = (String.split_on_char '-' x) in
      let first, second = ((int_of_string (List.nth split 0), int_of_string (List.nth split 1))) in
        get_ranges xs ((first, second)::curr) in
          get_ranges l []


let () = print_int (_sum_invalid_across_ranges _ranges 0)
let () = print_newline ()


(* let () = Printf.printf("%b\n") (_only_repeated 121212) *)
(* let () = Printf.printf("%b\n") (consists_only_of 121212 6 12) *)