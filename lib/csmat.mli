(** {1 Compression Formats}*)

(** {b Creation directly from these formats requires knowledge of the underlying
       compression in CSR/CSC matrices.}

    For an easier experience, use the (WIP) {!Cstri} module. *)

(** Compressed Sparse Row (CSR) vs Compressed Sparse Column (CSC) formats.

    They are explained in more detail in {!t} *)
type compressed_storage = CSR | CSC

(** {1 Sparse Matrix Type}*)

type 'a t = {
  mutable storage : compressed_storage;
  mutable nrows : int;
  mutable ncols : int;
  indptr : int Dynarray.t;
  indices : int Dynarray.t;
  data : 'a Dynarray.t;
}
(** In the CSR (Compressed Sparse Row) format, a matrix is represented by three vectors:
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
    ]}

    The {i outer} dimension of a compressed matrix is always kept in full. For CSR
    matrices, the outer dimension is the rows, and for CSC matrices, the outer dimension
    is the columns. The {i inner} dimension of a compressed matrix is compressed with
    index-data pairs. It is the opposite of the outer dimension. *)

exception MatrixException of string
(** Raised when trying to create invalid sparse matrices. *)

(** {1 Raw Creation Functions} *)

val try_new_csr : int * int -> int array -> int array -> 'a array -> ('a t, string) result
(** [try_new_csr (rows, cols) indptr indices data] attempts to create a new CSR matrix. It
    checks that:
    - [indptr] is of length [rows + 1]
    - [indices] and [data] have the same length
    - the last element of [indptr] is equal to [length indices]
    - the indices for each inner dimension are sorted (see {!t})
    - every element in [indices] is less than [cols]

    Returns [Ok m] if the inputs represent a valid CSR matrix, otherwise [Error s] *)

val try_new_csc : int * int -> int array -> int array -> 'a array -> ('a t, string) result
(** [try_new_csr (rows, cols) indptr indices data] attempts to create a new CSR matrix. It
    checks that:
    - [indptr] is of length [cols + 1]
    - [indices] and [data] have the same length
    - the last element of [indptr] is equal to [length indices]
    - the indices for each inner dimension are sorted (see {!t})
    - every element in [indices] is less than [rows]

    Returns [Ok m] if the inputs represent a valid CSC matrix, otherwise [Error s] *)

val new_csr : int * int -> int array -> int array -> 'a array -> 'a t
(** [new_csr (rows, cols) indptr indices data] creates a new CSR matrix.

    Raises [MatrixException] if the inputs do not describe a valid CSR matrix, described
    in {!try_new_csr}*)

val new_csc : int * int -> int array -> int array -> 'a array -> 'a t
(** [new_csc (rows, cols) indptr indices data] creates a new CSC matrix.

    Raises [MatrixException] if the inputs do not describe a valid CSC matrix, described
    in {!try_new_csc}*)

val new_csr_from_unsorted :
  int * int -> int array -> int array -> 'a array -> ('a t, string) result
(** [new_csr_from_unsorted (rows, cols) indptr indices data] creates a new CSR matrix,
    sorting the index-data pairs for each inner dimension as needed.

    Raises [MatrixException] if the inputs do not describe a valid CSR matrix, described
    in {!try_new_csr}*)

val new_csc_from_unsorted :
  int * int -> int array -> int array -> 'a array -> ('a t, string) result
(** [new_csc_from_unsorted (rows, cols) indptr indices data] creates a new CSC matrix,
    sorting the index-data pairs for each inner dimension as needed.

    Raises [MatrixException] if the inputs do not describe a valid CSC matrix, described
    in {!try_new_csc}*)

(** {1 Common Creation Functions}*)

(** For an easier creation schema than the functions listed here, see {!Cstri}*)

val csr_from_dense : ?epsilon:float -> float array array -> float t
(** [csr from dense ~epsilon m] creates a new CSR matrix mathematically equivalent to [m],
    ignoring all elements less than [epsilon]*)

val csc_from_dense : ?epsilon:float -> float array array -> float t
(** [csc from dense ~epsilon m] creates a new CSC matrix mathematically equivalent to [m],
    ignoring all elements less than [epsilon]*)

val empty : compressed_storage -> 'a t
(** [empty c] creates a new empty (0, 0)-shaped sparse matrix with format [c] for building
    purposes. *)

val zero : int * int -> 'a t
(** [zero (rows, cols)] creates a new CSR matrix representing the zero matrix. *)

val eye_csr : int -> float t
(** [eye_csr n] creates the n x n matrix, compressed in CSR format. *)

val eye_csc : int -> float t
(** [eye_csc n] creates the n x n matrix, compressed in CSC format. *)

(** {1 Iteration and traversal}*)

val iteroi : (int -> int -> 'a -> unit) -> 'a t -> unit
(** [iteroi f m] iterates through [m], calling [f outer inner x] on each element.

    {b NOTE: For CSR matrices, it will call [f row col item], and for CSC matrices, it
       will call [f col row item]}. To avoid this behavior, use {!iterrc}.*)

val iterrc : (int -> int -> 'a -> unit) -> 'a t -> unit
(** [iterrc f m] iterates through [m], calling [f row col x] on each element.

    {b NOTE: Due to the nature of CSC matrices, do not depend on the iteration order of
       this function. It will jump back and forth between rows.} *)

val itero : (int -> 'a Csvec.t -> unit) -> 'a t -> unit
(** [itero f m] calls [f outer v] on each outer dimension, where [v] is the corresponding
    sparse vector. *)

val map : ('a -> 'a) -> 'a t -> 'a t
(** [map f m] returns a new sparse matrix with the same format and shape, with all data
    values [x] mapped to [f x]*)

val map_inplace : ('a -> 'a) -> 'a t -> unit
(** [map_inplace f x] maps all values [x] of [m] to [f x], in place. *)

val scale : float -> float t -> float t
(** [scale c m] returns a new sparse matrix with all elements of [m] scaled by [c] *)

val scale_inplace : float -> float t -> unit
(** [scale_inplace c m] scales all elements of [m] by [c] in place *)

(** {1 Indexing} *)

val get : 'a t -> int * int -> 'a option
(** [get m (row, col)] returns the non-zero value at [(row, col)]. This search is
    logarithmic in the number of non-zeroes in the corresponding outer dimension.

    Returns [None] if the row and column do not describe a non-zero value. *)

val set : 'a t -> int * int -> 'a -> unit option
(** [set m (row, col) x] reassigns the value at [(row, col)] to [x]. This search is
    logarithmic in the number of non-zeroes in the corresponding outer dimension.

    Returns [None] if the row and column do not describe a non-zero value. *)

val nnz : 'a t -> int
(** [nnz m] returns the number of non-zero values in [m]*)

val nnz_index : 'a t -> int -> int -> Common.Nnz_index.t option
(** [nnz_index m row col] finds the non-zero index of the element specified by [row] and
    [col]. Returns [None] if the indexing does not describe a non-zero element.

    This search is logarithmic in the number of non-zeroes in the corresponding outer
    slice. Once available, the {!Common.Nnz_index.t} allows retrieval with O(1) compexity.
*)

val get_nnz : 'a t -> Common.Nnz_index.t -> 'a
(** [get_nnz m i] indexes a sparse matrix using its non-zero index. See {!nnz_index}.

    Raises an exception if the index is invalid. *)

val set_nnz : 'a t -> Common.Nnz_index.t -> 'a -> unit
(** [set_nnz m i x] reassigns a values in sparse matrix using its non-zero index. See
    {!nnz_index}.

    Raises an exception if the index is invalid. *)

val get_outer : 'a t -> int -> 'a Csvec.t option
(** [get_outer m outer] returns the sparse vector of [m] at outer dimension [outer].
    Returns [None] if the index is out of bounds. *)

val get_outer_exn : 'a t -> int -> 'a Csvec.t
(** [get_outer_exn m outer] returns the sparse vector of [m] at outer dimension [outer].
    Raises an exception if the index is out of bounds. *)

val ( .@() ) : 'a t -> int * int -> 'a option
(** Shorthand for {!get}*)

val ( .@()<- ) : 'a t -> int * int -> 'a -> unit
(** Shorthand for {!set}. Raises an exception if trying to set an invalid value. *)

val ( .!!() ) : 'a t -> Common.Nnz_index.t -> 'a
(** Shorthand for {!get_nnz}*)

val ( .!!()<- ) : 'a t -> Common.Nnz_index.t -> 'a -> unit
(** Shorthand for {!set_nnz}*)

(** {1 Modifying, Building, and Converting Matrices}*)

val to_other_storage : 'a t -> 'a t
(** [to_other_storage m] returns a new matrix representing the same matrix as m
    mathematically, in the opposite conversion format.
    {b This is an expensive operation, linear in the number of non-zero data in the
       matrix}*)

val to_dense : float t -> float array array
(** [to_dense m] converts m into a dense matrix. *)

val append_outer : ?epsilon:float -> float t -> float array -> unit
(** [append_outer ~epsilon m v] appends [v] as a new outer dimension to [m], extending the
    size of the outer dimension by one. This is adding a new row to CSR matrices, and a
    new column to CSC matrices.

    Ignores all elements less than [epsilon]

    Raises an exception if the vector to add does not have the same size as the inner
    dimension of [m]. *)

val insert : 'a t -> int -> int -> 'a -> unit
(** [insert row col x] inserts an element in the matrix at [(row, col)]. If the element is
    already present, its value is overwritten.

    This is not an efficient operation.
    {b However, it is efficient if the elements are inserted in order} according to the
    formatting (for example, row-by-row for CSR matrices)

    {i If the index is out of bounds, the matrix will be resized to the necessary size.}
*)

val transpose_mut : 'a t -> unit
(** [transpose_mut m] converts [m] to its mathematical transpose in-place. Converts CSR
    matrices to CSC and vise-versa. This is a cheap operation.

    {b This is not the same as {!to_other_storage},} mathematically changing what the
    matrix is.*)

val transpose : 'a t -> 'a t
(** [transpose m] returns a new matrix that is the mathematical transpose of [m]. Returns
    a matrix in the opposite compression format. This is a cheap operation.

    {b This is not the same as {!to_other_storage},} mathematically changing what the
    matrix is. *)

val to_csr : 'a t -> 'a t
(** [to_csr m] creates a new CSR matrix equivalent to [m]. If [m] is a CSR matrix, create
    a copy. *)

val into_csr : 'a t -> 'a t
(** [into_csr m] returns a new CSR matrix equivalent to [m]. If [m] is a CSR matrix, it is
    returned as a value. For a version that copies, see {!to_csr} *)

val to_csc : 'a t -> 'a t
(** [to_csc m] creates a new CSC matrix equivalent to [m]. If [m] is a CSC matrix, create
    a copy. *)

val into_csc : 'a t -> 'a t
(** [into_csc m] returns a new CSC matrix equivalent to [m]. If [m] is a CSC matrix, it is
    returned as a value. For a version that copies, see {!to_csc} *)

val to_col : 'a Csvec.t -> 'a t
(** [to_col v] converts sparse vector [v] into a new matrix with only one column. *)

val to_row : 'a Csvec.t -> 'a t
(** [to_row v] converts sparse vector [v] into a new matrix with only one row. *)

(** {1 Mathematical Functions}*)

val density : 'a t -> float
(** [density m] returns the density (proportion non-zero) of a matrix *)

val diag : 'a t -> 'a Csvec.t
(** [diag m] returns the diagonal of a sparse matrix as a sparse vector *)

val degrees : 'a t -> int array
(** [degrees m] returns a vector containing the degree of each vertex, ie the number of
    neighbors of each vertex. We do not count diagonal entries as a neighbor. *)

val to_inner_onehot : float t -> float t
(** [to_inner_onehot] generates a one-hot matrix, compressing the inner dimension.

    Returns a matrix with the same size, the same CSR/CSC type, and a single value of 1.0
    within each {i populated} inner vector, at the index of the largest value of each
    inner vector. *)

(** {1 Attributes, Derived, and Miscellaneous Functions}*)

val is_csr : 'a t -> bool
(** [is_csr m] returns [true] if [m] is a CSR matrix. *)

val is_csc : 'a t -> bool
(** [is_csc m] returns [true] if [m] is a CSC matrix. *)

val max_outer_nnz : 'a t -> int
(** [max_outer_nnz m] returns the max number of nonzeros in any outer dimension in [m] *)

val get_storage : 'a t -> compressed_storage
val get_nrows : 'a t -> int
val get_ncols : 'a t -> int
val get_indptr : 'a t -> int Dynarray.t
val get_indices : 'a t -> int Dynarray.t
val get_data : 'a t -> 'a Dynarray.t

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Default pretty-printer for sparse matrices *)

val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
(** Default [show] for sparse matrices *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Default [equal] function for sparse matrices *)

val copy : 'a t -> 'a t
(** [copy m] creates a copy of [m], shallowly copying its inner [Dynarrays].

    In the case of primitive sparse matrices, this can be considered a deep copy. *)

val check_structure :
  int -> int -> int Dynarray.t -> int Dynarray.t -> (unit, string) Result.t
(** [check_structure outer inner indptr indices] checks the structure of [CsMat]
    components, ensuring:
    - indptr is of length [outer_dim() + 1]
    - indices and data have the same length, [nnz == indptr[outer_dims()]]
    - indices is sorted for each outer slice
    - indices are lower than [inner_dims()] *)

val print_float_matrix : float t -> unit
(** [print_float_matrix m] prints [m] to stdout. *)

val print_int_matrix : int t -> unit
(** [print_int_matrix m] prints [m] to stdout. *)
