open Dynarray

let ( .!() ) = Dynarray.get
let ( .!()<- ) = Dynarray.set
let of_array_array m = map of_array (of_array m)
let to_array_array m = to_array (map to_array m)

let subarray xs start len =
  if len <= 0 then [||] else Array.init len (fun i -> xs.!(i + start))

let sub xs start len = subarray xs start len |> of_array

let insert arr index x =
  let n = Dynarray.length arr in
  if index < 0 || index > n then invalid_arg "insert: index out of bounds"
  else if n = 0 then Dynarray.add_last arr x
  else (
    Dynarray.add_last arr (Dynarray.get arr (n - 1));
    (* grow by copying last element *)
    for i = n - 1 downto index do
      Dynarray.set arr (i + 1) (Dynarray.get arr i)
    done;
    Dynarray.set arr index x)

let zip xs ys =
  let len = min (length xs) (length ys) in
  if len = 0 then create ()
  else
    let arr = make len (xs.!(0), ys.!(0)) in
    for i = 0 to len - 1 do
      set arr i (xs.!(i), ys.!(i))
    done;
    arr

let zip_array xs ys = zip (of_array xs) (of_array ys) |> to_array

let foldi f x a =
  let r = ref x in
  for i = 0 to length a - 1 do
    r := f !r i a.!(i)
  done;
  !r

let map_inplace f xs =
  for i = 0 to length xs - 1 do
    xs.!(i) <- f xs.!(i)
  done

(** Check that arr is sorted from indexes [s, e (exclusive)] *)
let is_sorted_from arr s e =
  let r = ref true in
  for i = s to e - 2 do
    r := !r && arr.!(i) <= arr.!(i + 1)
  done;
  !r

let is_sorted arr = is_sorted_from arr 0 (length arr)

(** Sort an array from [start, stop (exclusive)] *)
let sort_from arr start stop =
  let len = stop - start in
  if len > 0 then (
    let subarr = subarray arr start len in
    Array.sort Stdlib.compare subarr;
    let src = of_array subarr in
    blit ~src ~src_pos:0 ~dst:arr ~dst_pos:start ~len)

(** sort keys and vals in place from start to stop, using keys to sort both *)
let sort_like_from keys vals start stop =
  let len = stop - start in
  if len > 0 then (
    let subkey = subarray keys start len in
    let subval = subarray vals start len in
    let pairs = zip_array subkey subval in
    Array.sort Stdlib.compare pairs;
    for i = 0 to len - 1 do
      set keys (start + i) (fst pairs.(i));
      set vals (start + i) (snd pairs.(i))
    done)

(** [sort_like keys vals] sorts keys and vals in place, using keys to sort both *)
let sort_like keys vals =
  let stop = min (length keys) (length vals) in
  sort_like_from keys vals 0 stop

(** swaps arr.i and arr.j in place. Raises if either is an invalid index *)
let swap arr i j =
  let temp = arr.!(i) in
  arr.!(i) <- arr.!(j);
  arr.!(j) <- temp

(** Same as swap, but for arrays *)
let swap_arr arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

(** Turns arr into the cumulative sum in place *)
let cumsum arr =
  let sum = ref 0 in
  for i = 0 to length arr - 1 do
    sum := !sum + arr.!(i);
    set arr i !sum
  done

(** Returns the transpose of 2D Dynarray *)
let transpose_dynarray matrix =
  let rows = length matrix in
  let cols = if rows = 0 then 0 else length matrix.!(0) in
  init cols (fun j -> init rows (fun i -> matrix.!(i).!(j)))

(** Returns the transpose of a 2D Array *)
let transpose_array matrix =
  let open Array in
  match matrix with
  | [||] -> [||]
  | [| [||] |] -> [| [||] |]
  | matrix ->
      let rows = length matrix in
      let cols = length matrix.(0) in
      init cols (fun j -> init rows (fun i -> matrix.(i).(j)))

(** Returns the identity of nxn *)
let eye n = Array.init_matrix n n (fun i j -> if i = j then 1. else 0.)

(** Returns the range [start, stop (exclusive)] *)
let range ?(start = 0) stop =
  let len = stop - start in
  init len (fun i -> i + start)

let print_float_array arr =
  Printf.printf "[|";
  Array.iter (fun x -> Printf.printf " %.2f;" x) arr;
  Printf.printf " |]\n"

let print_int_array arr =
  Printf.printf "[|";
  Array.iter (fun x -> Printf.printf " %d;" x) arr;
  Printf.printf " |]\n"

(** [binary_search arr x] searches for [x] in [arr]. If it does not exist, returns [None]*)
let binary_search arr x =
  let rec aux low high =
    if low > high then None
    else
      let mid = (low + high) / 2 in
      if arr.!(mid) = x then Some mid
      else if arr.!(mid) < x then aux (mid + 1) high
      else aux low (mid - 1)
  in
  aux 0 (length arr - 1)

let binary_search_from arr start_idx end_idx x =
  let rec aux lo hi =
    if lo >= hi then Error lo
    else
      let mid = (lo + hi) / 2 in
      let mid_val = Dynarray.get arr mid in
      if mid_val = x then Ok mid else if mid_val < x then aux (mid + 1) hi else aux lo mid
  in
  aux start_idx end_idx

(** Lazily evaluates a list of conditions and returns the first [false] as an error with
    the corresponding message *)
let rec run_checks = function
  | [] -> Ok ()
  | cond :: rest ->
      let failed, msg = Lazy.force cond in
      if failed then Error msg else run_checks rest
