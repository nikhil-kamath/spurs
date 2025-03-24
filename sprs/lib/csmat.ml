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

let outer_dimension storage rows cols =
  match storage with CSR -> rows | CSC -> cols

let inner_dimension storage rows cols =
  match storage with CSR -> cols | CSC -> rows
