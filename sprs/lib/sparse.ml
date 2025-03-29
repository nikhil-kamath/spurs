type compressed_storage = CSR | CSC [@@deriving show, eq]

module Cs_mat_base = struct
  (* Compressed matrix in the CSR or CSC format, with sorted indices.

      This sparse matrix format is the preferred format for performing
      arithmetic operations. Constructing a sparse matrix directly in this
      format requires knowledge of its internals. For easier matrix
      construction, use the triplet format.

      The `cs_mat_base` type is parameterized by the scalar type `'a` of its
      data.

      Unlike the rust library, all underlying data structures are arrays in this
      implementation.

      ## Storage format

      In the compressed storage format, the non-zero values of a sparse matrix
      are stored as the row and column location of the non-zero values, with a
      compression along the rows (CSR) or columns (CSC) indices. The dimension
      along which the storage is compressed is referred to as the *outer
      dimension*, the other dimension is called the *inner dimension*. For
      clarity, the remaining explanation will assume a CSR matrix, but the
      information stands for CSC matrices as well.

      ### Indptr

      An index pointer array `indptr` of size corresponding to the number of
      rows stores the cumulative sum of non-zero elements for each row. For
      instance, the number of non-zero elements of the i-th row can be obtained
      by computing `indptr[i + 1] - indptr[i]`. The total number of non-zero
      elements is thus `nnz = indptr[nrows]`. This index pointer array can then
      be used to efficiently index the `indices` and `data` array, which
      respectively contain the column indices and the values of the non-zero
      elements.

      ### Indices and data

      The non-zero locations and values are stored in arrays of size `nnz`,
      `indices` and `data`. For row `i`, the non-zeros are located in the slices
      `indices[indptr[i]..indptr[i+1]]` and `data[indptr[i]..indptr[i+1]]`. We
      require and enforce sorted indices for each row.

      ## Construction

      A sparse matrix can be directly constructed by providing its index
      pointer, indices and data arrays. The coherence of the provided structure
      is then verified.

      For situations where the compressed structure is hard to figure out up
      front, the [triplet format](struct.TriMatBase.html) can be used. A matrix
      in the triplet format can then be efficiently converted to a `CsMat`. *)

  type 'a t = {
    storage : compressed_storage;
    nrows : int;
    ncols : int;
    indptr : int array;
    indices : int array;
    data : 'a array;
  }
  [@@deriving show, eq]

  let get_storage m = m.storage
  let get_nrows m = m.nrows
  let get_ncols m = m.ncols
  let get_indptr m = m.indptr
  let get_indices m = m.indices
  let get_data m = m.data
end

module Cs_vec_base = struct
  (* A sparse vector, storing the indices of its non-zero data.

      It contains a sorted `indices` array and a corresponding `data` array. *)

  type 'a t = { dim : int; indices : int array; data : 'a array } [@@deriving show, eq]

  let get_dim v = v.dim
  let get_indices v = v.indices
  let get_data v = v.data
end

module Cs_tri_base = struct
  (* Sparse matrix in the triplet format.

    Sparse matrices in the triplet format use three arrays of equal sizes
    (accessible through the methods [`row_inds`], [`col_inds`], [`data`]), the
    first one storing the row indices of non-zero values, the second storing the
    corresponding column indices and the last array storing the corresponding
    scalar value. If a non-zero location is repeated in the arrays, the non-zero
    value is taken as the sum of the corresponding scalar entries.

    This format is useful for iteratively building a sparse matrix, since the
    various non-zero entries can be specified in any order, or even partially as
    is common in physics with partial derivatives equations.

    This format cannot be used for arithmetic operations. Arithmetic operations
    are more efficient in the compressed format. A matrix in the triplet format
    can be converted to the compressed format using the functions [`to_csc`] and
    [`to_csr`]. *)
  type 'a t = {
    rows : int;
    cols : int;
    row_inds : int array;
    col_inds : int array;
    data : 'a array;
  }
  [@@deriving show, eq]

  let get_rows m = m.rows
  let get_cols m = m.cols
  let get_row_inds m = m.row_inds
  let get_col_inds m = m.col_inds
  let get_data m = m.data
end

module Utils = struct
  (** Check the structure of `CsMat` components, ensuring:
      - indptr is of length `outer_dim() + 1`
      - indices and data have the same length, `nnz == indptr[outer_dims()]`
      - indices is sorted for each outer slice
      - indices are lower than `inner_dims()` *)
  let check_compressed_structure (inner : int) (outer : int) (indptr : int array)
      (indices : int array) : (unit, string) Result.t =
    let open Result in
    let ( let* ) = bind in
    let* () = Indptr.check_indptr_structure indptr in
    let* () =
      if Array.length indptr <> outer + 1 then
        error "Indptr length does not match dimension"
      else ok ()
    in
    let* () =
      if Option.is_some (Array.find_opt (fun x -> x < 0) indices) then
        error "Negative index"
      else ok ()
    in
    let* () =
      if Indptr.nnz indptr <> Array.length indices then
        error "Indices length and indptr's nnz do not match"
      else ok ()
    in
    let* () =
      if
        Indptr.map_outer_list indptr (fun s e -> Array_utils.is_sorted_from indices s e)
        |> List.for_all (fun x -> x)
      then ok ()
      else error "Indices are not sorted"
    in
    let* () =
      if not (Array.for_all (fun index -> index < inner) indices) then
        error "Index is larger than inner dimension"
      else ok ()
    in
    ok ()
end
