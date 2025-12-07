let read_lines file_name =
  In_channel.with_open_text file_name In_channel.input_lines

let read_string file_name =
  In_channel.with_open_text file_name In_channel.input_all

let char_list_of_string s = List.of_seq (String.to_seq s)

let get_ids_of_element l el =
  let rec aux i res = function
  | [] -> res
  | x::xs -> if x = el then aux (i+1) (i::res) xs
  else aux (i+1) res xs
in aux 0 [] l