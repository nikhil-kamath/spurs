(**
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

(* Exception type *)
exception MatrixException of string

(**
  ==========================================================

  Functions to create new sparse matrices from various data

  ==========================================================
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

(** Create a new `CSR` sparse matrix. See `new_csc` for the `CSC` equivalent *)
let new_csr shape indptr indices data =
  match try_new_csr shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(** Create a new `CSC` sparse matrix. See `new_csr` for the `CSR` equivalent *)
let new_csc shape indptr indices data =
  match try_new_csc shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(** Create a new matrix *)
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

(** Try to create a `CSR` matrix. If necessary, the indices will be sorted in place *)
let new_csr_from_unsorted shape = new_from_unsorted CSR shape

(** Try to create a `CSC` matrix. If necessary, the indices will be sorted in place *)
let new_csc_from_unsorted shape = new_from_unsorted CSC shape

(**
  Create a matrix mathematically equal to this one, but with the opposed storage
  CSR -> CSC, CSC -> CSR
*)
let to_other_storage m =
  let open Array in
  let indptr = make (inner_dims m + 1) 0 in
  let indices = make (nnz m) 0 in
  let data = copy m.data in
  (* get outer dims*)
  iter (fun inner -> indptr.(inner) <- indptr.(inner) + 1) m.indices;

  (* get cumulative sum, starting at 0 *)
  let cumsum = ref 0 in
  iteri
    (fun i x ->
      indptr.(i) <- !cumsum;
      cumsum := !cumsum + x)
    indptr;

  (* iterate through data, using inner and outer dimensions to assign corresponding indices/data*)
  Indptr.iter_outeri m.indptr (fun outer start stop ->
      for i = start to stop - 1 do
        let inner = m.indices.(i) in
        let x = m.data.(i) in
        let dest = indptr.(inner) in
        indices.(dest) <- outer;
        data.(dest) <- x;
        (* increment each inner dimension's start temporarily *)
        indptr.(inner) <- indptr.(inner) + 1
      done);

  (* undo the incrementing from the assignments *)
  let last = ref 0 in
  Array.iteri
    (fun i x ->
      indptr.(i) <- !last;
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

(** Transposes a matrix in-place. Does not create a new matrix! *)
let transpose_mut (m : 'a Cs_mat_base.t) =
  m.storage <- other_storage m.storage;
  let nrows, ncols = (m.nrows, m.ncols) in
  m.nrows <- ncols;
  m.ncols <- nrows

(** Returns the transpose of this matrix, in the other format. *)
let transpose (m : 'a Cs_mat_base.t) =
  Cs_mat_base.
    {
      storage = other_storage m.storage;
      nrows = m.ncols;
      ncols = m.nrows;
      indptr = Array.copy m.indptr;
      indices = Array.copy m.indices;
      data = Array.copy m.data;
    }

(** Create a CSR matrix from a dense matrix, ignoring elements lower than `epsilon` *)
let csr_from_dense ?(epsilon = 0.00001) m =
  let open Array in
  let nrows = length m in
  let ncols = length m.(0) in
  let indptr = make (nrows + 1) 0 in
  let nnz = ref 0 in
  iteri
    (fun i row ->
      iter (fun x -> if abs_float x > epsilon then incr nnz) row;
      indptr.(i + 1) <- !nnz)
    m;
  let indices = make !nnz 0 in
  let data = make !nnz 0. in
  let dest = ref 0 in
  iter
    (fun row ->
      iteri
        (fun col x ->
          if abs_float x > epsilon then (
            indices.(!dest) <- col;
            data.(!dest) <- x;
            incr dest))
        row)
    m;
  Cs_mat_base.{ storage = CSR; nrows; ncols; indptr; indices; data }

(** Create a CSC matrix from a dense matrix, ignoring elements less than `epsilon`*)
let csc_from_dense ?(epsilon = 0.00001) m =
  let sm = m |> Array_utils.transpose |> csr_from_dense ~epsilon in
  transpose_mut sm;
  sm

(**
 =================================

 Common matrices in sparse formats

 =================================
*)

(** Identity matrix, stored as a CSR *)
let eye_csr n =
  let indptr = Array_utils.range (n + 1) in
  let indices = Array_utils.range n in
  let data = Array.make n 1. in
  Cs_mat_base.{ storage = CSR; nrows = n; ncols = n; indptr; indices; data }

(** Identity matrix, stored as a CSC *)
let eye_csc n =
  let m = eye_csr n in
  transpose_mut m;
  m

(** Create an empty matrix for building purposes *)
let empty storage inner_size =
  let shape = match storage with CSR -> (0, inner_size) | CSC -> (inner_size, 0) in
  new_checked storage shape [| 0; 1 |] [||] [||]

(** Create a new CSR matrix representing the zero matrix *)
let zero shape =
  let nrows, _ncols = shape in
  new_checked CSR shape (Array.make (nrows + 1) 0) [||] [||]

(**
 ===================

 Matrix Operations

 ===================
 *)

(** Scale the values in a sparse matrix inplace *)
let scale_inplace (m : float Cs_mat_base.t) c = Array.map_inplace (fun x -> x *. c) m.data

(** Return a new sparse matrix, scaled by c *)
let scale (m : float Cs_mat_base.t) c =
  let m2 = Cs_mat_base.copy m in
  scale_inplace m2 c;
  m2

(**
  ====================

  Indexing and lookups

  ====================
*)

(** Returns the inner vector at outer index `outer` *)
let outer_view (m : 'a Cs_mat_base.t) outer =
  if outer >= outer_dims m then None
  else
    let start, stop = Indptr.outer_inds_sz m.indptr outer in
    let len = stop - start in
    (* TODO: should we make the Array.subs reference copies? *)
    Some
      (Vec.new_trusted (inner_dims m)
         (Array.sub m.indices start len)
         (Array.sub m.data start len))

let nnz_index_outer_inner m outer inner =
  let ( let* ) = Option.bind in
  if outer >= outer_dims m then None
  else
    let offset, _ = Indptr.outer_inds_sz m.indptr outer in
    let* v = outer_view m outer in
    let* (NNZ index) = Vec.nnz_index v inner in
    Some (Nnz_index.NNZ (index + offset))

(** Find the non-zero index of the element specified by row and col.
 This search is logarithmic in the number of non-zeros in the corresponding
 outer slice. Once it is available, the `nnz_index` type allows retrieval with O(1) complexity *)
let nnz_index (m : 'a Cs_mat_base.t) row col =
  match m.storage with
  | CSR -> nnz_index_outer_inner m row col
  | CSC -> nnz_index_outer_inner m col row

(** Index a sparse matrix using an Nnz_index.t. Raises if out of bounds index *)
let get_nnz (m : 'a Cs_mat_base.t) (Nnz_index.NNZ i) = m.data.(i)

(** Reassign an Nnz_index.t. Raises if out of bounds index *)
let set_nnz (m : 'a Cs_mat_base.t) (Nnz_index.NNZ i) v = m.data.(i) <- v

(** Index a sparse matrix using row and column. Same complexity as nnz_index *)
let get m (row, col) =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  Some (get_nnz m i)

(** Reassign an elem using row and column. Same complexity as nnz_index *)
let set m (row, col) v =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  set_nnz m i v;
  Some ()

let ( .!() ) m i = get_nnz m i
let ( .!()<- ) m i v = set_nnz m i v
let ( .@() ) m rc = get m rc
let ( .@()<- ) m rc v = set m rc v |> Option.get (* Should this return the option? *)
