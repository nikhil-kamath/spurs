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
let outer_dimension storage rows cols = match storage with CSR -> rows | CSC -> cols
let inner_dimension storage rows cols = match storage with CSR -> cols | CSC -> rows

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

let try_new_csr shape indptr indices data = new_checked CSR shape indptr indices data
let try_new_csc shape indptr indices data = new_checked CSC shape indptr indices data

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
