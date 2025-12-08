module MinIntQueue = Pqueue.MakeMin(Int)

let a = MinIntQueue.create ()
let b = MinIntQueue.of_array [|1; 2; 3;|]

let get_distance p1 p2 =
  let rec aux p1' p2' res = match (p1', p2') with
  | ([], []) -> sqrt (float_of_int res)
  | (_, []) | ([], _) -> raise (Invalid_argument "arrays have different dimensions")
  | (x::xs, y::ys) -> aux xs ys ((y-x) * (y-x) + res)
in aux p1 p2 0

let get_min_queue points =
  let pq = MinIntQueue.create () in
  