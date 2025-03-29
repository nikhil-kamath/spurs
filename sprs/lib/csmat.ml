(*
Sparse matrices in the Compressed Sparse Row / Column format

In the CSR format, a a matrix is a structure containing 3 vectors:
`indptr, indices, and data`
These vectors satisfy the following relation:
`for i in [0, nrows]`
`A(i, indices[indptr[i] .. indptr[i + 1]]) = data[indptr[i] .. indptr[i + 1]]`
In the CSC format, the relation is
`A(indices[indptr[i] .. indptr[i + 1]], i) = data[indptr[i] .. indptr[i + 1]]`
*)
open Sparse

let other_storage = function CSR -> CSC | CSC -> CSR

let inner_dims Cs_mat_base.{ storage; nrows; ncols; _ } =
  match storage with CSC -> nrows | CSR -> ncols

let outer_dims Cs_mat_base.{ storage; nrows; ncols; _ } =
  match storage with CSC -> ncols | CSR -> nrows

let nnz Cs_mat_base.{ indptr; _ } = Indptr.nnz indptr

(* Can be used to later access a non-zero element of a compressed matrix in constant time *)
type nnz_index = NNZ of int

(* Exception type *)
exception MatrixException of string

(*
  ==========================================
  Functions to create new sparse matrices
  ==========================================
*)
let new_checked storage shape indptr indices data =
  let nrows, ncols = shape in
  let inner, outer = match storage with CSR -> (ncols, nrows) | CSC -> (nrows, ncols) in
  let open Result in
  let ( let* ) = bind in
  let* () =
    if Array.(length data <> length indices) then
      error (Printf.sprintf "data and indices have different sizes")
    else ok ()
  in
  let* () = Utils.check_compressed_structure inner outer indptr indices in
  ok Cs_mat_base.{ storage; nrows; ncols; indptr; indices; data }

let try_new_csr shape = new_checked CSR shape
let try_new_csc shape = new_checked CSC shape

(* Create a new `CSR` sparse matrix. See `new_csc` for the `CSC` equivalent *)
let new_csr shape indptr indices data =
  match try_new_csr shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(* Create a new `CSC` sparse matrix. See `new_csr` for the `CSR` equivalent *)
let new_csc shape indptr indices data =
  match try_new_csc shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(* Create a new matrix *)
let new_from_unsorted storage shape indptr indices data =
  let nrows, ncols = shape in
  let inner, outer = match storage with CSR -> (ncols, nrows) | CSC -> (nrows, ncols) in
  let open Result in
  let ( let* ) = bind in
  let* () =
    if Array.(length data <> length indices) then
      error "data and indices have different sizes"
    else ok ()
  in
  Indptr.iter_outer indptr (fun start stop ->
      if not (Array_utils.is_sorted_from indices start stop) then
        Array_utils.sort_like_from indices data start stop);
  let* () = Utils.check_compressed_structure inner outer indptr indices in
  ok Cs_mat_base.{ storage; nrows; ncols; indptr; indices; data }

(* Try to create a `CSR` matrix. If necessary, the indices will be sorted in place *)
let new_csr_from_unsorted shape = new_from_unsorted CSR shape

(* Try to create a `CSC` matrix. If necessary, the indices will be sorted in place *)
let new_csc_from_unsorted shape = new_from_unsorted CSC shape

let print_int_array arr =
  Format.printf "@[<1>[%a]@]@."
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Format.pp_print_int)
    (Array.to_list arr)

(* Create a matrix mathematically equal to this one, but with the opposed storage
CSR -> CSC, CSC -> CSR*)
let to_other_storage m =
  let open Array in
  let indptr = make (inner_dims m + 1) 0 in
  let indices = make (nnz m) 0 in
  let data = copy m.data in
  (* get outer dims*)
  iter (fun inner -> set indptr inner (indptr.(inner) + 1)) m.indices;

  (* get cumulative sum, starting at 0 *)
  let cumsum = ref 0 in
  iteri
    (fun i x ->
      set indptr i !cumsum;
      cumsum := !cumsum + x)
    indptr;

  (* iterate through data, using inner and outer dimensions to assign corresponding indices/data*)
  Indptr.iter_outeri m.indptr (fun outer start stop ->
      for i = start to stop - 1 do
        let inner = m.indices.(i) in
        let x = m.data.(i) in
        let dest = indptr.(inner) in
        set indices dest outer;
        set data dest x;
        (* increment each inner dimension's start temporarily *)
        set indptr inner (indptr.(inner) + 1)
      done);

  (* undo the incrementing from the assignments *)
  let last = ref 0 in
  Array.iteri
    (fun i x ->
      set indptr i !last;
      last := x)
    indptr;

  Cs_mat_base.
    {
      storage = other_storage m.storage;
      nrows = m.nrows;
      ncols = m.ncols;
      indptr;
      indices;
      data;
    }
