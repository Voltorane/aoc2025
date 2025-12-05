let _merge_intervals = function
| [] -> []
| (s, e)::xs -> let rec aux prev_start prev_end curr = function
  | [] -> ((prev_start, prev_end)::curr)
  | (s, e)::xs -> if prev_end >= s then aux prev_start (max e prev_end) curr xs
  else aux s e ((prev_start, prev_end)::curr) xs
in List.sort (fun (s1, _) (s2, _) -> compare s1 s2) (aux s e [] xs)

let _get_input lines =
  let rec aux is_second_part intervals ids = function
  | [] -> (List.sort (fun (s1, _) (s2, _) -> compare s1 s2) intervals, List.sort compare ids)
  | x::xs -> if x = "" then aux true intervals ids xs
  else if is_second_part then aux is_second_part intervals ((int_of_string x)::ids) xs
  else let s, e = int_of_string (List.nth (String.split_on_char '-' x) 0), int_of_string (List.nth (String.split_on_char '-' x) 1) in
  aux is_second_part ((s, e)::intervals) ids xs
in aux false [] [] lines

let _get_fresh_ingredients intervals ids =
  let rec aux intervals ids curr = match ids with
  | [] -> curr (* no more ids *)
  | id::xs -> match intervals with
    | [] -> curr (* no more intervals *)
    | (s, e)::ys -> if id < s then aux intervals xs curr
      else if id > e then aux ys ids curr
      else aux intervals xs (curr+1)
  in aux intervals ids 0

let _get_result_from_file file_name = let lines = Utils.read_lines file_name in
  let intervals, ids = _get_input lines in
  _get_fresh_ingredients (_merge_intervals intervals) ids

let rec _get_intervals_area = function
| [] -> 0
| (s, e)::xs -> (e - s + 1) + _get_intervals_area xs