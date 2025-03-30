open Array

let zip xs ys =
  let len = min (length xs) (length ys) in
  if len = 0 then [||]
  else
    let arr = make len (xs.(0), ys.(0)) in
    for i = 0 to len - 1 do
      set arr i (xs.(i), ys.(i))
    done;
    arr

let foldi f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r i a.(i)
  done;
  !r

(* Check that arr is sorted from indexes [s, e) *)
let is_sorted_from arr s e =
  let r = ref true in
  for i = s to e - 2 do
    r := !r && arr.(i) <= arr.(i + 1)
  done;
  !r

let is_sorted arr = is_sorted_from arr 0 (length arr)

(* Sort an array from [start, stop) *)
let sort_from arr start stop =
  let len = stop - start in
  if len > 0 then (
    let subarr = sub arr start len in
    sort compare subarr;
    blit subarr 0 arr start len)

(* sort keys and vals in place from start to stop, using keys to sort both *)
let sort_like_from keys vals start stop =
  let len = stop - start in
  if len > 0 then (
    let subkey = sub keys start len in
    let subval = sub vals start len in
    let pairs = zip subkey subval in
    sort compare pairs;
    for i = 0 to len - 1 do
      set keys (start + i) (fst pairs.(i));
      set vals (start + i) (snd pairs.(i))
    done)

(* sort keys and vals in place, using keys to sort both *)
let sort_like keys vals =
  let stop = min (length keys) (length vals) in
  sort_like_from keys vals 0 stop

(* swaps arr.i and arr.j in place. Raises if either is an invalid index *)
let swap arr i j =
  let temp = arr.(i) in
  set arr i arr.(j);
  set arr j temp

(* Turns arr into the cumulative sum in place *)
let cumsum arr =
  let sum = ref 0 in
  for i = 0 to length arr - 1 do
    sum := !sum + arr.(i);
    set arr i !sum
  done

(* Returns the transpose of a matrix *)
let transpose matrix =
  let rows = length matrix in
  let cols = if rows = 0 then 0 else length matrix.(0) in
  init cols (fun j -> init rows (fun i -> matrix.(i).(j)))

(* Returns the identity of nxn *)
let eye n = init_matrix n n (fun i j -> if i = j then 1. else 0.)

(* Returns the range [start, stop) *)
let range ?(start = 0) stop =
  let len = stop - start in
  init len (fun i -> i + start)

let print_float_array arr =
  Printf.printf "[|";
  Array.iter (fun x -> Printf.printf " %.2f;" x) arr;
  Printf.printf " |]\n"
