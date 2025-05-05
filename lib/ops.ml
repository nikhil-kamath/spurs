exception OpException of string

(* fold matching from to *)
let rec fmft f (ind_a : 'ind Dynarray.t) data_a ind_b data_b i j imax jmax (acc : 'acc) =
  let open Common.Dynarray in
  if i >= imax || j >= jmax then acc
  else
    let idx_a = ind_a.!(i) in
    let idx_b = ind_b.!(j) in
    if idx_a < idx_b then fmft f ind_a data_a ind_b data_b (i + 1) j imax jmax acc
    else if idx_a > idx_b then fmft f ind_a data_a ind_b data_b i (j + 1) imax jmax acc
    else
      let a = data_a.!(i) in
      let b = data_b.!(j) in
      fmft f ind_a data_a ind_b data_b (i + 1) (j + 1) imax jmax (f acc idx_a a b)

(* fold from to *)
let rec fft f f0 (ind_a : 'ind Dynarray.t) data_a ind_b data_b i j imax jmax (acc : 'acc)
    =
  let open Common.Dynarray in
  match (i >= imax, j >= jmax) with
  | true, true -> acc
  | true, false ->
      let idx_b = ind_b.!(j) in
      let b = data_b.!(j) in
      fft f f0 ind_a data_a ind_b data_b i (j + 1) imax jmax (f0 acc idx_b b)
  | false, true ->
      let idx_a = ind_a.!(i) in
      let a = data_a.!(j) in
      fft f f0 ind_a data_a ind_b data_b (i + 1) j imax jmax (f0 acc idx_a a)
  | false, false ->
      let idx_a = ind_a.!(i) in
      let idx_b = ind_b.!(j) in
      let a = data_a.!(i) in
      let b = data_b.!(j) in
      if idx_a < idx_b then
        fft f f0 ind_a data_a ind_b data_b (i + 1) j imax jmax (f0 acc idx_a a)
      else if idx_a > idx_b then
        fft f f0 ind_a data_a ind_b data_b i (j + 1) imax jmax (f0 acc idx_b b)
      else fft f f0 ind_a data_a ind_b data_b (i + 1) (j + 1) imax jmax (f acc idx_a a b)

(** {b fold matching vectors} Folds left with [f acc a b] on every pair of matching
    indices between [v1] and [v2] *)
let fm_v f (acc : 'acc) (v1 : 'a Csvec.t) (v2 : 'b Csvec.t) =
  if v1.dim <> v2.dim then raise (OpException "fold-matching different-dimension vectors")
  else
    let imax = Csvec.nnz v1 in
    let jmax = Csvec.nnz v2 in
    fmft
      (fun ac _ a b -> f ac a b)
      v1.indices v1.data v2.indices v2.data 0 0 imax jmax acc

(** {b fold matching, indexed vectors} Folds left with [f acc idx a b] on every pair of
    matching indices between [v1] and [v2] *)
let fmi_v f (acc : 'acc) (v1 : 'a Csvec.t) (v2 : 'b Csvec.t) =
  if v1.dim <> v2.dim then raise (OpException "fold-matching different-dimension vectors")
  else
    let imax = Csvec.nnz v1 in
    let jmax = Csvec.nnz v2 in
    fmft f v1.indices v1.data v2.indices v2.data 0 0 imax jmax acc

(** {b fold non-matching, indexed} [fmz_v f f0 acc v1 v2] Folds left. It uses
    [f0 acc idx x] to update the accumulator if only a nonzero is found in one of the two
    vectors. On matching indices, it uses [f acc idx a b]. *)
let fni_v f f0 (acc : 'acc) (v1 : 'a Csvec.t) (v2 : 'b Csvec.t) =
  if v1.dim <> v2.dim then
    raise (OpException "fold-matching-zero different-dimension vectors")
  else
    let imax = Csvec.nnz v1 in
    let jmax = Csvec.nnz v2 in
    fft f f0 v1.indices v1.data v2.indices v2.data 0 0 imax jmax acc

(** Adds two sparse vectors, returning the result as a new vector.

    Raises an exception if the vectors have different dimensions. *)
let add_v (v1 : float Csvec.t) (v2 : float Csvec.t) =
  if v1.dim <> v2.dim then raise (OpException "adding two different-dimension vectors")
  else
    let open Dynarray in
    let indices = create () in
    let data = create () in
    fni_v
      (* ignore accumulator *)
      (fun () i a b ->
        add_last indices i;
        add_last data (a +. b))
      (fun () i x ->
        add_last indices i;
        add_last data x)
      () v1 v2;
    Csvec.{ dim = v1.dim; indices; data }

(** Calculates the dot-product of two sparse vectors. *)
let dot_v (v1 : float Csvec.t) (v2 : float Csvec.t) =
  if v1.dim <> v2.dim then
    raise (OpException "dot-product of two different-dimension vectors")
  else fm_v (fun acc a b -> acc +. (a *. b)) 0. v1 v2
