module Array = struct
  include Array

  let foldi f x a =
    let r = ref x in
    for i = 0 to length a - 1 do
      r := f !r i a.(i)
    done;
    !r
end

(* number of nonzero  elements described by this indptr *)
let nnz indptr =
  if Array.length indptr = 0 then 0 else indptr.(Array.length indptr - 1)

(* Fold over outer dimension, giving start and end indices for each outer dimension *)
let fold_outer (indptr : int array) (f : int -> int -> 'acc -> 'acc) (x : 'acc)
    : 'acc =
  let r = ref x in
  for i = 0 to Array.length indptr - 2 do
    r := f indptr.(i) indptr.(i + 1) !r
  done;
  !r
