open Sparse

(** {1 Sparse Matrices}

    In the CSR (Compressed Sparse Row) format, a matrix is represented by three vectors:
    [indptr], [indices], and [data].

    These vectors satisfy the following relation:
    {[
      for i in [0, nrows]:
        A(i, indices[indptr[i] .. indptr[i + 1]]) = data[indptr[i] .. indptr[i + 1]]
    ]}

    In the CSC (Compressed Sparse Column) format, the relation becomes:
    {[
      for i in [0, ncols]:
        A(indices[indptr[i] .. indptr[i + 1]], i) = data[indptr[i] .. indptr[i + 1]]
    ]} *)

let other_storage = function CSR -> CSC | CSC -> CSR

let inner_dims Cs_mat_base.{ storage; nrows; ncols; _ } =
  match storage with CSC -> nrows | CSR -> ncols

let outer_dims Cs_mat_base.{ storage; nrows; ncols; _ } =
  match storage with CSC -> ncols | CSR -> nrows

let set_outer_dims (m : 'a Cs_mat_base.t) outer =
  match m.storage with CSR -> m.nrows <- outer | CSC -> m.ncols <- outer

let set_inner_dims (m : 'a Cs_mat_base.t) inner =
  match m.storage with CSR -> m.ncols <- inner | CSC -> m.nrows <- inner

let nnz Cs_mat_base.{ indptr; _ } = Indptr.nnz indptr

(* Exception type *)
exception MatrixException of string

(** {1 Creation Functions} *)

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
  let indptr = Dynarray.of_array indptr in
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  let* () = Utils.check_compressed_structure inner outer indptr indices in
  ok Cs_mat_base.{ storage; nrows; ncols; indptr; indices; data }

let try_new_csr shape = new_checked CSR shape
let try_new_csc shape = new_checked CSC shape

(** [new_csr indptr indices data] creates a new CSR matrix. Raises an exception if the
    inputs do not describe a valid CSR matrix.

    See {!new_csc} for the CSC equivalent. *)
let new_csr shape indptr indices data =
  match try_new_csr shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(** [new_csc indptr indices data] creates a new CSC matrix. Raises an exception if the
    inputs do not describe a valid CSC matrix.

    See {!new_csr} for the CSR equivalent. *)
let new_csc shape indptr indices data =
  match try_new_csc shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

(** Create a new matrix.

    Returns [Some matrix] if the inputs represent a valid sparse matrix, or [None] if the
    inputs are invalid. *)
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
  let indptr = Dynarray.of_array indptr in
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  Indptr.iter_outer indptr (fun start stop ->
      if not (Array_utils.is_sorted_from indices start stop) then
        Array_utils.sort_like_from indices data start stop);
  let* () = Utils.check_compressed_structure inner outer indptr indices in
  ok Cs_mat_base.{ storage; nrows; ncols; indptr; indices; data }

(** Try to create a CSR matrix.

    If necessary, the indices will be sorted. *)
let new_csr_from_unsorted shape = new_from_unsorted CSR shape

(** Try to create a CSC matrix.

    If necessary, the indices will be sorted. *)
let new_csc_from_unsorted shape = new_from_unsorted CSC shape

(** Iterates through the matrix, calling [f outer inner x] on each element. *)
let iteroi f (m : 'a Cs_mat_base.t) =
  let open Dynarray in
  Indptr.iter_outeri m.indptr (fun outer start stop ->
      for i = start to stop - 1 do
        let inner = m.indices.!(i) in
        let x = m.data.!(i) in
        f outer inner x
      done)

(** Create a matrix mathematically equal to this one, but with the opposite storage: CSR →
    CSC, or CSC → CSR. *)
let to_other_storage m =
  let open Dynarray in
  let indptr = make (inner_dims m + 1) 0 in
  let indices = make (nnz m) 0 in
  let data = copy m.data in
  (* get outer dims*)
  iter (fun inner -> indptr.!(inner) <- indptr.!(inner) + 1) m.indices;

  (* get cumulative sum, starting at 0 *)
  let cumsum = ref 0 in
  iteri
    (fun i x ->
      indptr.!(i) <- !cumsum;
      cumsum := !cumsum + x)
    indptr;

  (* iterate through data, using inner and outer dimensions to assign corresponding indices/data*)
  iteroi
    (fun outer inner x ->
      let dest = indptr.!(inner) in
      indices.!(dest) <- outer;
      data.!(dest) <- x;
      indptr.!(inner) <- indptr.!(inner) + 1)
    m;

  (* undo the incrementing from the assignments *)
  let last = ref 0 in
  Dynarray.iteri
    (fun i x ->
      indptr.!(i) <- !last;
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

(** Transpose a matrix in-place.

    Does not create a new matrix! *)
let transpose_mut (m : 'a Cs_mat_base.t) =
  m.storage <- other_storage m.storage;
  let nrows, ncols = (m.nrows, m.ncols) in
  m.nrows <- ncols;
  m.ncols <- nrows

(** Return the transpose of this matrix, in the other format.

    Does not modify the original matrix. *)
let transpose (m : 'a Cs_mat_base.t) =
  Cs_mat_base.
    {
      storage = other_storage m.storage;
      nrows = m.ncols;
      ncols = m.nrows;
      indptr = Dynarray.copy m.indptr;
      indices = Dynarray.copy m.indices;
      data = Dynarray.copy m.data;
    }

(** Create a CSR matrix from a dense matrix, ignoring elements lower than [epsilon]. *)
let csr_from_dense ?(epsilon = 0.00001) m =
  let open Array in
  let nrows = length m in
  let ncols = length m.(0) in
  let indptr = Dynarray.make (nrows + 1) 0 in
  let nnz = ref 0 in
  iteri
    (fun i row ->
      iter (fun x -> if abs_float x > epsilon then incr nnz) row;
      Dynarray.set indptr (i + 1) !nnz)
    m;
  let indices = Dynarray.make !nnz 0 in
  let data = Dynarray.make !nnz 0. in
  let dest = ref 0 in
  iter
    (fun row ->
      iteri
        (fun col x ->
          if abs_float x > epsilon then (
            Dynarray.set indices !dest col;
            Dynarray.set data !dest x;
            incr dest))
        row)
    m;
  Cs_mat_base.{ storage = CSR; nrows; ncols; indptr; indices; data }

(** Create a CSC matrix from a dense matrix, ignoring elements less than [epsilon]. *)
let csc_from_dense ?(epsilon = 0.00001) m =
  let sm = m |> Array_utils.transpose_array |> csr_from_dense ~epsilon in
  transpose_mut sm;
  sm

(** {1 Common Matrices} *)

(** Identity matrix, stored as a CSR. *)
let eye_csr n =
  let indptr = Array_utils.range (n + 1) in
  let indices = Array_utils.range n in
  let data = Dynarray.make n 1. in
  Cs_mat_base.{ storage = CSR; nrows = n; ncols = n; indptr; indices; data }

(** Identity matrix, stored as a CSC. *)
let eye_csc n =
  let m = eye_csr n in
  transpose_mut m;
  m

(** Create an empty matrix for building purposes *)
let empty storage inner_size =
  let shape = match storage with CSR -> (0, inner_size) | CSC -> (inner_size, 0) in
  let indptr = [| 0 |] in
  let indices = [||] in
  let data = [||] in
  new_checked storage shape indptr indices data

(** Create a new CSR matrix representing the zero matrix. *)
let zero shape =
  let nrows, ncols = shape in
  Cs_mat_base.
    {
      nrows;
      ncols;
      storage = CSR;
      indptr = Dynarray.make (nrows + 1) 0;
      indices = Dynarray.create ();
      data = Dynarray.create ();
    }

(** {1 Matrix Operations} *)

(** Scale the values in a sparse matrix inplace *)
let scale_inplace (m : float Cs_mat_base.t) c =
  Array_utils.map_inplace (fun x -> x *. c) m.data

(** Return a new sparse matrix, scaled by c *)
let scale (m : float Cs_mat_base.t) c =
  let m2 = Cs_mat_base.copy m in
  scale_inplace m2 c;
  m2

(** {1 Indexing and Iteration} *)

(** Return the inner vector at outer index [outer]. *)
let get_outer (m : 'a Cs_mat_base.t) outer =
  if outer >= outer_dims m then None
  else
    let start, stop = Indptr.outer_inds_sz m.indptr outer in
    let len = stop - start in
    (* TODO: should we make the Array.subs reference copies? *)
    Some
      (Csvec.new_trusted (inner_dims m)
         (Array_utils.sub m.indices start len |> Dynarray.to_array)
         (Array_utils.sub m.data start len |> Dynarray.to_array))

(** Same as {!get_outer}, but raises an exception if the outer index is invalid. *)
let get_outer_exn (m : 'a Cs_mat_base.t) outer = get_outer m outer |> Option.get

(** Calls [f outer v] on each outer dimension, where [v] is the corresponding sparse
    vector. *)
let itero f (m : 'a Cs_mat_base.t) =
  for outer = 0 to outer_dims m - 1 do
    f outer (get_outer_exn m outer)
  done

(** Try to find the value at the given outer and inner indices.

    Returns [None] if the indexing is invalid, otherwise returns [Some NNZ index]. *)

let nnz_index_outer_inner m outer inner =
  let ( let* ) = Option.bind in
  if outer >= outer_dims m then None
  else
    let offset, _ = Indptr.outer_inds_sz m.indptr outer in
    let* v = get_outer m outer in
    let* (NNZ index) = Csvec.nnz_index v inner in
    Some (Nnz_index.NNZ (index + offset))

(** Find the non-zero index of the element specified by row and column.

    This search is logarithmic in the number of non-zeros in the corresponding outer
    slice. Once available, the [`nnz_index`] type allows retrieval with O(1) complexity.

    Returns [None] if the element is not found, otherwise returns [Some NNZ index]. *)
let nnz_index (m : 'a Cs_mat_base.t) row col =
  match m.storage with
  | CSR -> nnz_index_outer_inner m row col
  | CSC -> nnz_index_outer_inner m col row

(** Index a sparse matrix using an [Nnz_index.t].

    Raises an exception if the index is out of bounds. *)
let get_nnz (m : 'a Cs_mat_base.t) (Nnz_index.NNZ i) = Dynarray.get m.data i

(** Reassign an index of a sparse matrix using an [Nnz_index.t].

    Raises an exception if the index is out of bounds. *)
let set_nnz (m : 'a Cs_mat_base.t) (Nnz_index.NNZ i) v = Dynarray.set m.data i v

(** Index a sparse matrix using row and column.

    Has the same complexity as [nnz_index].

    Returns [None] if the row and column are invalid, otherwise returns [Some value] at
    that position. *)
let get m (row, col) =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  Some (get_nnz m i)

(** Reassign an element using row and column.

    Has the same complexity as [nnz_index].

    Returns [None] if the row and column are invalid, otherwise sets the value at that
    position. *)
let set m (row, col) v =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  Some (set_nnz m i v)

let ( .!!() ) m i = get_nnz m i
let ( .!!()<- ) m i v = set_nnz m i v
let ( .@() ) m rc = get m rc
let ( .@()<- ) m rc v = set m rc v |> Option.get (* Should this return the option? *)

(** {1 Modifying and building matrices} *)

(** Append an outer dimension to an existing matrix, extending the size of the outer
    dimension by one.

    Raises an exception if the vector to add does not have compatible dimension. *)
let append_outer ?(epsilon = 0.000001) (m : 'a Cs_mat_base.t) (v : 'a array) =
  if Array.length v <> inner_dims m then
    raise (MatrixException "Trying to append improperly sized vector");
  let nnz = ref (nnz m) in
  Array.iteri
    (fun i x ->
      if abs_float x >= epsilon then (
        Dynarray.add_last m.indices i;
        Dynarray.add_last m.data x;
        incr nnz))
    v;
  Dynarray.add_last m.indptr !nnz;
  match m.storage with CSR -> m.nrows <- m.nrows + 1 | CSC -> m.ncols <- m.ncols + 1

let insert_outer_inner m outer inner x =
  let open Dynarray in
  let outer_dims = outer_dims m in
  (if outer >= outer_dims then (
     (* adding enough new outer dimensions *)
     let last_nnz = if length m.indptr > 0 then get_last m.indptr else 0 in
     append_array m.indptr (Array.make (outer - outer_dims) last_nnz);
     set_outer_dims m (outer + 1);
     add_last m.indptr (last_nnz + 1);
     add_last m.indices inner;
     add_last m.data x)
   else
     (* search for an insertion spot *)
     let start, stop = Indptr.outer_inds_sz m.indptr outer in
     match Array_utils.binary_search_from m.indices start stop inner with
     | Ok ind -> m.data.!(ind) <- x
     | Error ind ->
         Array_utils.insert m.indices ind inner;
         Array_utils.insert m.data ind x;
         Indptr.record_new_element m.indptr outer);
  if inner > inner_dims m then set_inner_dims m (inner + 1)

(** Insert an element in the matrix. If the element is already present, its value is
    overwritten.

    This is not an efficient operation.
    {b However, it is efficient if the elements are inserted in order} according to the
    formatting (for example, row-by-row for CSR matrices)

    {i If the index is out of bounds, the matrix will be resized to the necessary size.}
*)
let insert (m : 'a Cs_mat_base.t) row col x =
  match m.storage with
  | CSR -> insert_outer_inner m row col x
  | CSC -> insert_outer_inner m col row x

(** {1 Miscellaneous Functions & Nice-To-Haves} *)

(** Returns the density (proportion non-zero) of a matrix *)
let density (m : 'a Cs_mat_base.t) =
  float_of_int (nnz m) /. float_of_int (m.nrows * m.ncols)

(** Get the diagonal of a sparse matrix. {i Returns as a sparse vector} *)
let diag (m : 'a Cs_mat_base.t) =
  let dim = min m.nrows m.ncols in
  let indices = Dynarray.create () in
  let data = Dynarray.create () in
  for i = 0 to dim - 1 do
    match get m (i, i) with
    | Some x ->
        Dynarray.add_last indices i;
        Dynarray.add_last data x
    | None -> ()
  done;
  Csvec.new_trusted dim (Dynarray.to_array indices) (Dynarray.to_array data)

(** Create a new CSR matrix equivalent to this one. If this is a CSR matrix, it is
    returned as a value. For a version that copies, see {!to_csr} *)
let into_csr (m : 'a Cs_mat_base.t) =
  match m.storage with CSR -> m | CSC -> to_other_storage m

(** Create a new CSR matrix equivalent to this one. If this is a CSR matrix, create a
    copy. *)
let to_csr (m : 'a Cs_mat_base.t) =
  match m.storage with CSR -> Cs_mat_base.copy m | CSC -> to_other_storage m

(** Create a new CSC matrix equivalent to this one. If this is a CSC matrix, it is
    returned as a value. For a version that copies, see {!to_csc} *)
let into_csc (m : 'a Cs_mat_base.t) =
  match m.storage with CSR -> to_other_storage m | CSC -> m

(** Create a new CSC matrix equivalent to this one. If this is a CSC matrix, create a
    copy. *)
let to_csc (m : 'a Cs_mat_base.t) =
  match m.storage with CSR -> to_other_storage m | CSC -> Cs_mat_base.copy m

(** Returns [true] if the input matrix is in CSR format *)
let is_csr (m : 'a Cs_mat_base.t) = m.storage = CSR

(** Returns [true] if the input matrix is in CSC format *)
let is_csc (m : 'a Cs_mat_base.t) = m.storage = CSC

(** Returns a new sparse matrix with the elements mapped by [f] *)
let map f (m : 'a Cs_mat_base.t) =
  let m2 = Cs_mat_base.copy m in
  Array_utils.map_inplace f m2.data;
  m2

(** Maps [f] over the sparse matrix in-place. *)
let map_inplace f (m : 'a Cs_mat_base.t) = Array_utils.map_inplace f m.data

(** Returns the maximum number of nonzeros in each outer dimension *)
let max_outer_nnz (m : 'a Cs_mat_base.t) =
  let r = ref 0 in
  Indptr.iter_outer m.indptr (fun start stop -> r := max !r (stop - start));
  !r

(** Converts into a dense matrix. *)
let to_dense (m : float Cs_mat_base.t) =
  let res = Array.make_matrix m.nrows m.ncols 0. in
  let assign outer inner x =
    match m.storage with
    | CSR -> res.(outer).(inner) <- x
    | CSC -> res.(inner).(outer) <- x
  in
  iteroi (fun outer inner x -> assign outer inner x) m;
  res

(** Returns a vector containing the degree of each vertex, ie the number of neighbors of
    each vertex. We do not count diagonal entries as a neighbor. *)
let degrees (m : 'a Cs_mat_base.t) =
  let count = Array.make (outer_dims m) 0 in
  iteroi
    (fun outer inner _ -> if outer <> inner then count.(outer) <- count.(outer) + 1)
    m;
  count

(** Generate a one-hot matrix, compressing the inner dimension.

    Returns a matrix with the same size, the same CSR/CSC type, and a single value of 1.0
    within each {i populated} inner vector. *)
let to_inner_onehot (m : 'a Cs_mat_base.t) =
  let open Dynarray in
  let indptr_counter = ref 0 in
  let indptr = create () in
  let indices = create () in
  let data = create () in
  itero
    (fun _ v ->
      (* build indptr *)
      add_last indptr !indptr_counter;
      (* only add on populated outer dims *)
      if not (Csvec.is_empty v) then (
        (* keep the index of the max inner value for this outer dim *)
        let index =
          Csvec.fold
            (fun (maxi, maxd) i d -> if d > maxd then (i, d) else (maxi, maxd))
            (-1, -.infinity) v
          |> fst
        in
        add_last indices index;
        add_last data 1.;
        (* one-hot values *)
        incr indptr_counter))
    m;
  (* set final indptr *)
  add_last indptr !indptr_counter;
  Cs_mat_base.
    { storage = m.storage; nrows = m.nrows; ncols = m.ncols; indptr; indices; data }
