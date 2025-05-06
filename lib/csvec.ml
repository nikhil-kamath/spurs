open Common

type 'a t = { dim : int; indices : int Dynarray.t; data : 'a Dynarray.t }
[@@deriving show, eq]

let get_dim v = v.dim
let get_indices v = v.indices
let get_data v = v.data

let copy v =
  { dim = v.dim; indices = Dynarray.copy v.indices; data = Dynarray.copy v.data }

exception VectorException of string

let new_trusted n indices data =
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  { dim = n; indices; data }

(* Helper functions for creation *)
let new_checked n indices data =
  let open Dynarray in
  if not (Utils.is_sorted indices) then Error "Unsorted indices"
  else if length indices <> length data then Error "Indices and data have unequal lengths"
  else if length indices > 0 && get_last indices >= n then
    Error "Indices larger than vector size"
  else Ok { dim = n; indices; data }

let try_new_csvec n indices data =
  new_checked n (Dynarray.of_array indices) (Dynarray.of_array data)

let new_csvec n indices data =
  match try_new_csvec n indices data with
  | Ok v -> v
  | Error s ->
      raise (VectorException (Printf.sprintf "Could not create sparse matrix: %s" s))

let new_from_unsorted n indices data =
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  Utils.sort_like indices data;
  new_checked n indices data

let empty n = new_trusted n [||] [||]
let nnz (v : 'a t) = Dynarray.length v.data

let nnz_index (v : 'a t) index =
  let ( let* ) = Option.bind in
  let* i = Utils.binary_search v.indices index in
  Some (Nnz_index.NNZ i)

let get_nnz (v : 'a t) (Nnz_index.NNZ i) = Dynarray.get v.data i
let set_nnz (v : 'a t) (Nnz_index.NNZ i) x = Dynarray.set v.data i x

let get (v : 'a t) index =
  let ( let* ) = Option.bind in
  let* i = nnz_index v index in
  Some (get_nnz v i)

let set (v : 'a t) index x =
  let ( let* ) = Option.bind in
  let* i = nnz_index v index in
  Some (set_nnz v i x)

let fold f (acc : 'acc) (v : 'a t) =
  Dynarray.fold_left (fun a (i, d) -> f a i d) acc (Utils.zip v.indices v.data)

let iter f (v : 'a t) =
  let open Dynarray in
  for i = 0 to length v.indices do
    f v.indices.!(i) v.data.!(i)
  done

let map f (v : 'a t) =
  let indices = Dynarray.copy v.indices in
  let data = Dynarray.map f v.data in
  { dim = v.dim; indices; data }

let map_inplace f (v : 'a t) = Utils.map_inplace f v.data

let count f (v : 'a t) =
  let c = ref 0 in
  Dynarray.iter (fun x -> if f x then incr c) v.data;
  !c

let is_empty (v : 'a t) = Dynarray.is_empty v.data

let append (v : 'a t) ind x =
  let open Dynarray in
  if ind >= v.dim then raise (VectorException "Out-of-bounds append");
  if length v.indices > 0 && ind <= get_last v.indices then
    raise (VectorException "Unsorted append");
  add_last v.indices ind;
  add_last v.data x

let to_dense (v : float t) =
  let out = Array.make v.dim 0. in
  iter (Array.set out) v;
  out

let to_hashtbl (v : 'a t) =
  let out = Hashtbl.create (nnz v) in
  iter (Hashtbl.add out) v;
  out

let l1_norm (v : float t) =
  v.data |> Dynarray.map abs_float |> Dynarray.fold_left ( +. ) 0.

let squared_l2_norm (v : float t) =
  v.data |> Dynarray.map (fun x -> Float.pow x 2.) |> Dynarray.fold_left ( +. ) 0.

let l2_norm (v : float t) = v |> squared_l2_norm |> Float.sqrt

let norm (v : float t) p =
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

let normalize (v : float t) =
  let n = l2_norm v in
  if n > 0. then map_inplace (fun x -> x /. n) v
