open Sparse

(* Exception type *)
exception VectorException of string

(** {1 Creation functions for CsVec}*)

let new_trusted n indices data =
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  Cs_vec_base.{ dim = n; indices; data }

(* Helper functions for creation *)
let new_checked n indices data =
  let open Dynarray in
  if not (Array_utils.is_sorted indices) then Error "Unsorted indices"
  else if length indices <> length data then Error "Indices and data have unequal lengths"
  else if length indices > 0 && get_last indices >= n then
    Error "Indices larger than vector size"
  else Ok Cs_vec_base.{ dim = n; indices; data }

(** Create a sparse vector.

    Returns an error:
    - If [indices] and [data] lengths differ
    - If the indices are out of order
    - If the data is longer than [n] *)
let try_new_csvec n indices data =
  new_checked n (Dynarray.of_array indices) (Dynarray.of_array data)

(** Same as {!try_new_csvec}, but raises an error if invalid *)
let new_csvec n indices data =
  match try_new_csvec n indices data with
  | Ok v -> v
  | Error s ->
      raise (VectorException (Printf.sprintf "Could not create sparse matrix: %s" s))

(** Creates a CsVec, sorting indices and data if necessary.

    Returns an error if the lengths mismatch. *)
let new_from_unsorted n indices data =
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  Array_utils.sort_like indices data;
  new_checked n indices data

(** Create an empty [CsVec] of size [n] for building purposes *)
let empty n = new_trusted n [||] [||]

(** {1 Indexing and Iteration Functions} *)

let nnz (v : 'a Cs_vec_base.t) = Dynarray.length v.data

(** Try to index a vector at a specific index. Returns [Some nnz] if index exists. *)
let nnz_index (v : 'a Cs_vec_base.t) index =
  let ( let* ) = Option.bind in
  let* i = Array_utils.binary_search v.indices index in
  Some (Nnz_index.NNZ i)

(** Access element at given [NNZ] index, with constant complexity. *)
let get_nnz (v : 'a Cs_vec_base.t) (Nnz_index.NNZ i) = Dynarray.get v.data i

(** Set element at given [NNZ] index, with constant complexity. *)
let set_nnz (v : 'a Cs_vec_base.t) (Nnz_index.NNZ i) x = Dynarray.set v.data i x

(** Access element at given index, with logarithmic complexity. *)
let get (v : 'a Cs_vec_base.t) index =
  let ( let* ) = Option.bind in
  let* i = nnz_index v index in
  Some (get_nnz v i)

(** Set element at given index, with logarithmic complexity. Returns [None] if the index
    does not exist already as a non-zero.. *)
let set (v : 'a Cs_vec_base.t) index x =
  let ( let* ) = Option.bind in
  let* i = nnz_index v index in
  Some (set_nnz v i x)

(** [fold f acc v] calls [f acc index data], through each index-data pair in [v] *)
let fold f (acc : 'acc) (v : 'a Cs_vec_base.t) =
  Dynarray.fold_left (fun a (i, d) -> f a i d) acc (Array_utils.zip v.indices v.data)

(** [iter f v] calls [f index data] on each index-data pair in [v]*)
let iter f (v : 'a Cs_vec_base.t) =
  let open Dynarray in
  for i = 0 to length v.indices do
    f v.indices.!(i) v.data.!(i)
  done

(** [map f v] returns a new vector with data mapped by [f] *)
let map f (v : 'a Cs_vec_base.t) =
  let indices = Dynarray.copy v.indices in
  let data = Dynarray.map f v.data in
  Cs_vec_base.{ dim = v.dim; indices; data }

(** [map_inplace f v] maps [f] onto [v.data] in place. *)
let map_inplace f (v : 'a Cs_vec_base.t) = Array_utils.map_inplace f v.data

(** Count how many elements make [f] return true *)
let count f (v : 'a Cs_vec_base.t) =
  let c = ref 0 in
  Dynarray.iter (fun x -> if f x then incr c) v.data;
  !c

(** Returns whether a vector is empty. *)
let is_empty (v : 'a Cs_vec_base.t) = Dynarray.is_empty v.data

(** {1 Building vectors}*)

(** Append an element to the sparse vector. The append should preserve the structure.

    Raises an exception if:
    - [ind] is lower or equal to the last element of v.indices
    - [ind] is greater than v.dim *)
let append (v : 'a Cs_vec_base.t) ind x =
  let open Dynarray in
  if ind >= v.dim then raise (VectorException "Out-of-bounds append");
  if length v.indices > 0 && ind <= get_last v.indices then
    raise (VectorException "Unsorted append");
  add_last v.indices ind;
  add_last v.data x

(** {1 Miscellaneous}*)

(** Check the sparse structure, namely that:
    - [indices] are sorted
    - all [indices] are less than [dim] *)
let check_structure (v : 'a Cs_vec_base.t) =
  if not (Array_utils.is_sorted v.indices) then Error "Unsorted indices"
  else if Dynarray.(length v.indices > 0 && get_last v.indices >= v.dim) then
    Error "Out of bounds index"
  else Ok ()

(** {1 Conversions}*)

(** Convert this vector into a new matrix with only one column. *)
let to_col (v : 'a Cs_vec_base.t) =
  let indptr = Dynarray.of_array [| 0; Dynarray.length v.indices |] in
  Cs_mat_base.
    {
      storage = CSC;
      nrows = v.dim;
      ncols = 1;
      indptr;
      indices = Dynarray.copy v.indices;
      data = Dynarray.copy v.data;
    }

(** Convert this vector into a new matrix with only one row. *)
let to_row (v : 'a Cs_vec_base.t) =
  let indptr = Dynarray.of_array [| 0; Dynarray.length v.indices |] in
  Cs_mat_base.
    {
      storage = CSR;
      nrows = 1;
      ncols = v.dim;
      indptr;
      indices = Dynarray.copy v.indices;
      data = Dynarray.copy v.data;
    }

(** Convert this vector into a dense array.

    The rest of the vector is filled with zeroes.*)
let to_dense (v : float Cs_vec_base.t) =
  let out = Array.make v.dim 0. in
  iter (Array.set out) v;
  out

(** Convert this vector into a index -> value hashtbl *)
let to_hashtbl (v : 'a Cs_vec_base.t) =
  let out = Hashtbl.create (nnz v) in
  iter (Hashtbl.add out) v;
  out

(** {1 Normalization}*)

(** Compute the L1-norm of [v] *)
let l1_norm (v : float Cs_vec_base.t) =
  v.data |> Dynarray.map abs_float |> Dynarray.fold_left ( +. ) 0.

(** Compute the squared L2-norm of [v] *)
let squared_l2_norm (v : float Cs_vec_base.t) =
  v.data |> Dynarray.map (fun x -> Float.pow x 2.) |> Dynarray.fold_left ( +. ) 0.

(** Compute the L2-norm of [v] *)
let l2_norm (v : float Cs_vec_base.t) = v |> squared_l2_norm |> Float.sqrt

(** Compute the p-order norm of [v]*)
let norm (v : float Cs_vec_base.t) p =
  if Dynarray.is_empty v.data then 0.
  else if Float.is_infinite p then
    (* p = inf returns max *)
    if p > 0. then v.data |> Dynarray.map abs_float |> Dynarray.fold_left max (-.infinity)
    (* p = -inf returns min *)
      else v.data |> Dynarray.map abs_float |> Dynarray.fold_left min infinity
      (* p = 0 returns count of nonzero values *)
  else if p = 0. then count (fun x -> x <> 0.) v |> float_of_int
  else
    (* standard norm *)
    v.data |> Dynarray.map abs_float
    |> Dynarray.map (fun x -> Float.pow x p)
    |> Dynarray.fold_left ( +. ) 0.
    |> fun sum -> Float.pow sum (1. /. p)

(** Divides the vector by its own L2-norm in-place. The zero vector is left unchanged *)
let normalize (v : float Cs_vec_base.t) =
  let n = l2_norm v in
  if n > 0. then map_inplace (fun x -> x /. n) v
