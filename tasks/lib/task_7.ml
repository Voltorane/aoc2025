module IntSet = Set.Make(Int)

let string_of_list l = List.fold_left (fun acc a -> acc^(string_of_int a)^", ") "" l
let string_of_set set = let l = IntSet.to_list set in string_of_list l

let divider = '^'

let get_start_id line = let l = Utils.get_ids_of_element (Utils.char_list_of_string line) 'S' in List.nth l 0

let get_divider_ids lines =
  let rec aux res = function
  | [] -> res
  | line::xs -> let curr_ids = IntSet.of_list (Utils.get_ids_of_element (Utils.char_list_of_string line) divider) in
  aux (curr_ids::res) xs
in aux [] lines

let get_sum_divisions file_name =
  let lines = Utils.read_lines file_name in
  let start_id = get_start_id (List.nth lines 0) in
  let divider_ids = get_divider_ids lines in
  let rec aux splits curr_beam_ids curr_dividers = match curr_dividers with
  | [] -> splits
  | x::xs -> 
    if IntSet.is_empty x then
      aux splits curr_beam_ids xs
    else
      (* let () = Printf.printf "beams: %s dividers: %s\n" (string_of_list curr_beam_ids) (string_of_set x) in *)
      let rec get_new_beam_ids new_splits i res = function
      | [] -> (res, new_splits)
      | b::bs ->
        if IntSet.mem b x then
          (* let () = print_string "a\n" in *)
          get_new_beam_ids (new_splits+1) (i+1) ((b-1)::(b+1)::res) bs
        else
          (* let () = print_string "b\n" in *)
          get_new_beam_ids new_splits (i+1) (b::res) bs
    in let new_beams, new_splits = get_new_beam_ids 0 0 [] (IntSet.to_list (IntSet.of_list curr_beam_ids)) in
  aux (splits+new_splits) new_beams xs 
in aux 0 [start_id] (List.rev divider_ids)
