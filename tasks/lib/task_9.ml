let max x y = if x > y then x else y

let read_input file_name =
  let lines = Utils.read_lines file_name in
  let rec aux = function
  | [] -> []
  | x::xs -> let l = String.split_on_char ',' x in 
  let x, y = int_of_string (List.nth l 0), int_of_string (List.nth l 1) 
  in (x, y)::(aux xs)
in aux lines

let find_max_area points = 
  let rec aux m = function
  | [] -> m
  | (x1, y1)::xs -> let rec inner l new_max = match l with
    | [] -> max new_max m
    | (x2, y2)::ys -> if x1 = x2 && y1 = y2 then inner ys new_max else 
      let area = (((abs (x2 - x1)) + 1) * ((abs (y2 - y1)) + 1))
      in Printf.printf "(%d, %d), (%d, %d): %d\n" x1 y1 x2 y2 area; max area new_max |> inner ys in
    let new_max = inner points 0 in aux (max m new_max) xs
in aux 0 points