let init = "0,1," |> Rope.of_string

let find_index_of_nth r nth =
  let length = Rope.length r in

  let rec finder pos count =

    if pos > length - 1
    then None
    else
      let idx = Rope.index_from r pos ',' in
      let new_count = count + 1 in
      if new_count < nth
      then finder (idx + 1) new_count
      else if idx + 1 > length - 1 then None else Some(idx + 1) in

  if nth == 0 then Some(1) else
    let start_index = finder 0 0 in
    Some((Rope.index_from r (Option.get start_index) ',') + 1)

let find_nth_of_num num r =
  let loc = Rope.search_forward_string ("," ^ string_of_int num ^ ",") r 0 in

  let rec finder pos count =
    if pos < 0
    then None
    else
      let idx = Rope.rindex_from_opt r pos ',' in
      match idx with
      | Some i -> if i >= 0 then finder (i - 1) (count + 1) else Some(count)
      | None -> Some(count) in

  finder loc 0

let total_numbers r =
  let length = Rope.length r in
  let rec counter pos count =

    if pos > length - 1
    then count
    else
      let idx = Rope.index_from_opt r pos ',' in
      match idx with
      | Some i -> counter (i + 1) (count + 1)
      | None -> count in

  counter 0 0

let get_next_pos r input num =
  let cur_pos = find_nth_of_num num r in
  let total = total_numbers r in
  let rough_next_pos = input mod total + (Option.get cur_pos) in
  if rough_next_pos >= total
  then rough_next_pos - total
  else rough_next_pos

let insert r x nth =
  match nth with
  | 0 -> 
    let first = Rope.sub r 0 2 in
    let last = Rope.sub r 2 (Rope.length r - 2) in
    Rope.concat (Rope.of_string (string_of_int x ^ ",")) [first; last;]
  | _ -> 
    if nth <= (total_numbers r)
    then
      let break_point = Option.get(find_index_of_nth r nth) in
      let first = Rope.sub r 0 (break_point) in
      let last = Rope.sub r (break_point) (Rope.length r - break_point) in
      Rope.concat (Rope.of_string (string_of_int x ^ ",")) [first; last;]
    else
      Rope.concat2 r (Rope.of_string (string_of_int x ^ ","))

let generate r limit input =
  let rec maker new_r num =
    if num < limit
    then
      let next_pos = get_next_pos new_r input num in
      let new_buffer = insert new_r (num + 1) next_pos in
      maker new_buffer (num + 1)
    else new_r in

  maker r 1

let find_next_num r num =
  let nth_of = Option.get(find_nth_of_num num r) in
  let index_of = Option.get (find_index_of_nth r nth_of) in
  let end_of = Rope.index_from r index_of ',' in
  Rope.sub r index_of (end_of - index_of)

let buffer = generate init 2017 329
(* let big_buffer = generate init 50000000 329 *)
(* let _ = buffer |> Rope.print_endline *)
let part1 = find_next_num buffer 2017 |> Rope.print_endline
(* let part2 = find_next_num big_buffer 0 |> Rope.print_endline *)

let find_next_pos l p step =
  (step + p) mod l

let find_num_after_zero step =

  (*
    fake_simulator
    1 0 0
    2 0 1
   *)
  let rec fake_simulator l p x =
    let next_pos = find_next_pos l (p + 1) step in
    if l > 50000000 then x else
    if next_pos == 0 then fake_simulator (l + 1) next_pos l else fake_simulator (l + 1) next_pos x in

  fake_simulator 1 0 0

let part2 = print_endline (string_of_int (find_num_after_zero 329))