module Array = struct
  include Array

  let foldi f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i a.(i)
    done;
    !r

  let is_sorted arr =
    let n = Array.length arr in
    let rec check i =
      if i >= n - 1 then true else if arr.(i) > arr.(i + 1) then false else check (i + 1)
    in
    check 0
end

(* number of nonzero  elements described by this indptr *)
let nnz indptr = Array.(if length indptr = 0 then 0 else indptr.(length indptr - 1))

(* Fold over outer dimension, giving start and end indices for each outer dimension *)
let fold_outer (indptr : int array) (f : int -> int -> 'acc -> 'acc) (x : 'acc) : 'acc =
  let r = ref x in
  for i = 0 to Array.length indptr - 2 do
    r := f indptr.(i) indptr.(i + 1) !r
  done;
  !r

let check_indptr_structure indptr =
  let open Result in
  let ( let* ) = bind in
  let* () =
    if Array.length indptr < 1 then error "An indptr should have its len >= 1" else ok ()
  in
  let* () =
    if not (Array.is_sorted indptr) then error "Indptr should be sorted" else ok ()
  in
  ok ()
