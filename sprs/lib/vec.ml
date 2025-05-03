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

(** [fold f acc v] calls [f acc index data], through each index-data pair in [v] *)
let fold f (acc : 'acc) (v : 'a Cs_vec_base.t) =
  Dynarray.fold_left (fun a (i, d) -> f a i d) acc (Array_utils.zip v.indices v.data)

(** Returns whether a vector is empty. *)
let is_empty (v : 'a Cs_vec_base.t) = Dynarray.is_empty v.data
