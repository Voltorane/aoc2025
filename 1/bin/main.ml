let read_lines (file_name : string) : string list =
  In_channel.with_open_text file_name In_channel.input_lines

let l = read_lines "../input_1"
(* let reversed = List.rev l *)

type direction = Rigth | Left
let extract s = match String.get s 0 with
  | 'R' -> (Rigth, int_of_string (String.sub s 1 ((String.length s) - 1)))
  | 'L' -> (Left, int_of_string (String.sub s 1 ((String.length s) - 1)))
  | _ -> raise (Invalid_argument s)

let rec get_movements l = match l with
| [] -> []
| x::xs -> (extract x) :: get_movements xs

let rec get_zeroes movements curr res = match movements with
| [] -> res
| (Rigth, n)::xs ->
  let hundreds = n / 100 in
    let new_curr = ((curr + (n - (hundreds * 100))) mod 100) in
  Printf.printf "R %d %d %d \n" n curr new_curr ;
    if new_curr == 0 then (get_zeroes xs new_curr (res + 1)) else (get_zeroes xs new_curr res)
| (Left, n)::xs -> 
  let hundreds = n / 100 in
    let new_curr = if curr - n < 0 then (100 + curr - (n - (hundreds * 100))) mod 100 else curr - (n - (hundreds * 100)) in
      Printf.printf "L %d %d %d \n" n curr new_curr ;
      if new_curr == 0 then (get_zeroes xs new_curr (res + 1)) else (get_zeroes xs new_curr res)

let () = print_int (get_zeroes (get_movements l) 50 0)