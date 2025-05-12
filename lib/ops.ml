exception OpException of string

(* fold from to *)
let rec fft f f0 (ind_a : 'ind Dynarray.t) data_a ind_b data_b i imax j jmax (acc : 'acc)
    =
  let open Common.Dynarray in
  match (i >= imax, j >= jmax) with
  | true, true -> acc
  | true, false ->
      let b, idx_b = (data_b.!(j), ind_b.!(j)) in
      fft f f0 ind_a data_a ind_b data_b i imax (j + 1) jmax (f0 acc idx_b b)
  | false, true ->
      let a, idx_a = (data_a.!(i), ind_a.!(i)) in
      fft f f0 ind_a data_a ind_b data_b (i + 1) imax j jmax (f0 acc idx_a a)
  | false, false ->
      let a, idx_a = (data_a.!(i), ind_a.!(i)) in
      let b, idx_b = (data_b.!(j), ind_b.!(j)) in
      if idx_a < idx_b then
        fft f f0 ind_a data_a ind_b data_b (i + 1) imax j jmax (f0 acc idx_a a)
      else if idx_a > idx_b then
        fft f f0 ind_a data_a ind_b data_b i imax (j + 1) jmax (f0 acc idx_b b)
      else fft f f0 ind_a data_a ind_b data_b (i + 1) imax (j + 1) jmax (f acc idx_a a b)

(* do nothing on non-matching indices *)
let fmfti (f : 'acc -> 'ind -> 'a -> 'a -> 'acc) = fft f (fun a _ _ -> a)
let fmft (f : 'acc -> 'a -> 'a -> 'acc) = fft (fun a _ l r -> f a l r) (fun a _ _ -> a)

(** {b fold non-matching, indexed} [fmz_v f f0 acc v1 v2] Folds left. It uses
    [f0 acc idx x] to update the accumulator if only a nonzero is found in one of the two
    vectors. On matching indices, it uses [f acc idx a b]. *)
let fni_v f f0 (acc : 'acc) (v1 : 'a Csvec.t) (v2 : 'b Csvec.t) =
  if v1.dim <> v2.dim then
    raise (OpException "fold-matching-zero different-dimension vectors")
  else
    let imax = Csvec.nnz v1 in
    let jmax = Csvec.nnz v2 in
    fft f f0 v1.indices v1.data v2.indices v2.data 0 imax 0 jmax acc

(** {b fold matching, indexed vectors} Folds left with [f acc idx a b] on every pair of
    matching indices between [v1] and [v2] *)
let fmi_v f = fni_v f (fun a _ _ -> a)

(** {b fold matching vectors} Folds left with [f acc a b] on every pair of matching
    indices between [v1] and [v2] *)
let fm_v f = fmi_v (fun acc _ a b -> f acc a b)

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

let mult ?(epsilon = 0.000001) ?(storage = Csmat.CSR) (m1 : float Csmat.t)
    (m2 : float Csmat.t) =
  (* ensure m1 is CSR and m2 is CSC *)
  let open Csmat in
  let m1 = into_csr m1 in
  let m2 = into_csc m2 in
  (* if not (is_csr m1 && is_csc m2) then
    raise (MatrixException "multiplying invalid formats"); *)
  if m1.ncols <> m2.nrows then raise (MatrixException "multiplying invalid dimensions");

  (*  TODO: make this generate the output in cstri or compressed format instead of dense *)
  let out = empty storage in
  Indptr.iter_outeri m1.indptr (fun row row_start row_stop ->
      Indptr.iter_outeri m2.indptr (fun col col_start col_stop ->
          let ip =
            fmft
              (fun acc l r -> acc +. (l *. r))
              m1.indices m1.data m2.indices m2.data row_start row_stop col_start col_stop
              0.
          in

          if ip >= epsilon then insert out row col ip));
  expand out m1.nrows m2.ncols;
  out

let ( *@ ) = mult
