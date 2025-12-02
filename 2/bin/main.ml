let rec count_digit n curr = match n with
| 0 -> curr
| _ -> count_digit (n / 10) (curr + 1)
let rec pow x n = match n with 
| 0 -> 1
| _ -> x * (pow x (n-1))
let is_repeated n num_digits = n mod (pow 10 (num_digits / 2)) - n / (pow 10 (num_digits / 2)) == 0
let is_invalid n = 
  let num_digits = count_digit n 0 in
    if num_digits mod 2 == 0 then
      is_repeated n num_digits
    else
      false
let rec sum_invalid_in_range curr e curr_sum = 
  if curr = e+1 then curr_sum
  else
    if is_invalid curr then sum_invalid_in_range (curr+1) e (curr_sum+curr)
    else sum_invalid_in_range (curr+1) e curr_sum
let rec sum_invalid_across_ranges ranges curr_sum = match ranges with
| [] -> curr_sum
| (s,e)::xs -> sum_invalid_across_ranges xs curr_sum+(sum_invalid_in_range s e 0)
let read (file_name : string) : string =
  In_channel.with_open_text file_name In_channel.input_all
let s = read "../input_2"
let ranges = 
  let l = String.split_on_char ',' s in
    let rec get_ranges arr curr = match arr with
    | [] -> curr
    | x::xs -> let split = (String.split_on_char '-' x) in
      let first, second = ((int_of_string (List.nth split 0), int_of_string (List.nth split 1))) in
        get_ranges xs ((first, second)::curr) in
          get_ranges l []


let () = print_int (sum_invalid_across_ranges ranges 0)