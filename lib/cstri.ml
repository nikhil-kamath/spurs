open Common

type 'a t = {
  rows : int;
  cols : int;
  row_inds : int Dynarray.t;
  col_inds : int Dynarray.t;
  data : 'a Dynarray.t;
}
[@@deriving show, eq]
(** Sparse matrix in the triplet format.

    Sparse matrices in the triplet format use three arrays of equal sizes (accessible
    through the methods [`row_inds`], [`col_inds`], [`data`]), the first one storing the
    row indices of non-zero values, the second storing the corresponding column indices
    and the last array storing the corresponding scalar value. If a non-zero location is
    repeated in the arrays, the non-zero value is taken as the sum of the corresponding
    scalar entries.

    This format is useful for iteratively building a sparse matrix, since the various
    non-zero entries can be specified in any order, or even partially as is common in
    physics with partial derivatives equations.

    This format cannot be used for arithmetic operations. Arithmetic operations are more
    efficient in the compressed format. A matrix in the triplet format can be converted to
    the compressed format using the functions [`to_csc`] and [`to_csr`]. *)

let get_rows m = m.rows
let get_cols m = m.cols
let get_row_inds m = m.row_inds
let get_col_inds m = m.col_inds
let get_data m = m.data
