open Sparse

let new_trusted n indices data =
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  Cs_vec_base.{ dim = n; indices; data }

let nnz_index (v : 'a Cs_vec_base.t) index =
  let open Option in
  let ( let* ) = bind in
  let* i = Array_utils.binary_search v.indices index in
  Some (Nnz_index.NNZ i)
