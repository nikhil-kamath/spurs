open Common

type compressed_storage = CSR | CSC [@@deriving show, eq]

type 'a t = {
  mutable storage : compressed_storage;
  mutable nrows : int;
  mutable ncols : int;
  indptr : int Dynarray.t;
  indices : int Dynarray.t;
  data : 'a Dynarray.t;
}
[@@deriving show, eq]

let copy m =
  {
    storage = m.storage;
    nrows = m.nrows;
    ncols = m.ncols;
    indptr = Dynarray.copy m.indptr;
    indices = Dynarray.copy m.indices;
    data = Dynarray.copy m.data;
  }

let get_storage m = m.storage
let get_nrows m = m.nrows
let get_ncols m = m.ncols
let get_indptr m = m.indptr
let get_indices m = m.indices
let get_data m = m.data
let print_float_matrix m = show Fmt.float m |> print_endline
let print_int_matrix m = show Fmt.int m |> print_endline
let other_storage = function CSR -> CSC | CSC -> CSR

let inner_dims { storage; nrows; ncols; _ } =
  match storage with CSC -> nrows | CSR -> ncols

let outer_dims { storage; nrows; ncols; _ } =
  match storage with CSC -> ncols | CSR -> nrows

let set_outer_dims (m : 'a t) outer =
  match m.storage with CSR -> m.nrows <- outer | CSC -> m.ncols <- outer

let set_inner_dims (m : 'a t) inner =
  match m.storage with CSR -> m.ncols <- inner | CSC -> m.nrows <- inner

let nnz { indptr; _ } = Dynarray.get_last indptr

exception MatrixException of string

let check_structure (inner : int) (outer : int) (indptr : int Dynarray.t)
    (indices : int Dynarray.t) : (unit, string) Result.t =
  let open Dynarray in
  let ( let* ) = Result.bind in
  let* () = Indptr.check_indptr_structure indptr in
  Utils.run_checks
    [
      lazy (length indptr <> outer + 1, "Indptr length does not match dimension");
      lazy (exists (fun x -> x < 0) indices, "Negative index");
      lazy (get_last indptr <> length indices, "Indices length and indptr nnz mismatch");
      lazy (Indptr.check_indices indptr indices |> not, "Indices not sorted");
      lazy (exists (fun i -> i >= inner) indices, "Index larger than inner dimension");
    ]

let new_checked_dyn storage shape indptr indices data =
  let nrows, ncols = shape in
  let inner, outer = match storage with CSR -> (ncols, nrows) | CSC -> (nrows, ncols) in
  let ( let* ) = Result.bind in
  if inner < 0 || outer < 0 then Error "negative shape"
  else if Dynarray.(length data <> length indices) then
    Error "data and indices have different sizes"
  else
    let* () = check_structure inner outer indptr indices in
    Ok { storage; nrows; ncols; indptr; indices; data }

let new_checked storage shape indptr indices data =
  let indptr = Dynarray.of_array indptr in
  let indices = Dynarray.of_array indices in
  let data = Dynarray.of_array data in
  new_checked_dyn storage shape indptr indices data

let try_new_csr shape = new_checked CSR shape
let try_new_csc shape = new_checked CSC shape

let new_csr shape indptr indices data =
  match try_new_csr shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

let new_csc shape indptr indices data =
  match try_new_csc shape indptr indices data with
  | Ok m -> m
  | Error s ->
      raise (MatrixException (Printf.sprintf "Could not create sparse matrix: %s" s))

let new_from_unsorted storage shape indptr indices data =
  if Array.(length data <> length indices) then
    Error "data and indices have different sizes"
  else
    let indptr = Dynarray.of_array indptr in
    let indices = Dynarray.of_array indices in
    let data = Dynarray.of_array data in
    Indptr.iter_outer indptr (fun start stop ->
        if not (Utils.is_sorted_from indices start stop) then
          Utils.sort_like_from indices data start stop);
    new_checked_dyn storage shape indptr indices data

let new_csr_from_unsorted shape = new_from_unsorted CSR shape
let new_csc_from_unsorted shape = new_from_unsorted CSC shape

let iteroi f (m : 'a t) =
  let open Dynarray in
  Indptr.iter_outeri m.indptr (fun outer start stop ->
      for i = start to stop - 1 do
        let inner = m.indices.!(i) in
        let x = m.data.!(i) in
        f outer inner x
      done)

let iterrc f (m : 'a t) =
  match m.storage with CSR -> iteroi f m | CSC -> iteroi (Fun.flip f) m

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

  {
    storage = other_storage m.storage;
    nrows = m.nrows;
    ncols = m.ncols;
    indptr;
    indices;
    data;
  }

let transpose_mut (m : 'a t) =
  m.storage <- other_storage m.storage;
  let nrows, ncols = (m.nrows, m.ncols) in
  m.nrows <- ncols;
  m.ncols <- nrows

let transpose (m : 'a t) =
  {
    storage = other_storage m.storage;
    nrows = m.ncols;
    ncols = m.nrows;
    indptr = Dynarray.copy m.indptr;
    indices = Dynarray.copy m.indices;
    data = Dynarray.copy m.data;
  }

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
  { storage = CSR; nrows; ncols; indptr; indices; data }

let csc_from_dense ?(epsilon = 0.00001) m =
  let sm = m |> Utils.transpose_array |> csr_from_dense ~epsilon in
  transpose_mut sm;
  sm

let eye_csr n =
  let indptr = Utils.range (n + 1) in
  let indices = Utils.range n in
  let data = Dynarray.make n 1. in
  { storage = CSR; nrows = n; ncols = n; indptr; indices; data }

let eye_csc n =
  let m = eye_csr n in
  transpose_mut m;
  m

let empty storage =
  {
    nrows = 0;
    ncols = 0;
    storage;
    indptr = [| 0 |] |> Dynarray.of_array;
    indices = [||] |> Dynarray.of_array;
    data = [||] |> Dynarray.of_array;
  }

let zero shape =
  let nrows, ncols = shape in
  {
    nrows;
    ncols;
    storage = CSR;
    indptr = Dynarray.make (nrows + 1) 0;
    indices = Dynarray.create ();
    data = Dynarray.create ();
  }

let scale_inplace c (m : float t) = Utils.map_inplace (fun x -> x *. c) m.data

let scale c (m : float t) =
  let m2 = copy m in
  scale_inplace c m2;
  m2

let get_outer (m : 'a t) outer =
  if outer < 0 || outer >= outer_dims m then None
  else
    let start, stop = Indptr.outer_start_stop m.indptr outer in
    let len = stop - start in
    Some
      Csvec.
        {
          dim = inner_dims m;
          indices = Utils.sub m.indices start len;
          data = Utils.sub m.data start len;
        }

let get_outer_exn (m : 'a t) outer = get_outer m outer |> Option.get

let itero f (m : 'a t) =
  for outer = 0 to outer_dims m - 1 do
    f outer (get_outer_exn m outer)
  done

let nnz_index (m : 'a t) row col =
  let helper m outer inner =
    if outer < 0 || outer >= outer_dims m then None
    else
      let ( let* ) = Option.bind in
      let offset = Dynarray.get m.indptr outer in
      let* v = get_outer m outer in
      let* (NNZ index) = Csvec.nnz_index v inner in
      Some (Nnz_index.NNZ (index + offset))
  in
  match m.storage with CSR -> helper m row col | CSC -> helper m col row

let get_nnz (m : 'a t) (Nnz_index.NNZ i) = Dynarray.get m.data i
let set_nnz (m : 'a t) (Nnz_index.NNZ i) v = Dynarray.set m.data i v

let get m (row, col) =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  Some (get_nnz m i)

let set m (row, col) v =
  let ( let* ) = Option.bind in
  let* i = nnz_index m row col in
  Some (set_nnz m i v)

let ( .!!() ) m i = get_nnz m i
let ( .!!()<- ) m i v = set_nnz m i v
let ( .@() ) m rc = get m rc
let ( .@()<- ) m rc v = set m rc v |> Option.get (* Should this return the option? *)

let append_outer ?(epsilon = 0.000001) (m : 'a t) (v : 'a array) =
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
     let start, stop = Indptr.outer_start_stop m.indptr outer in
     match Utils.binary_search_from m.indices start stop inner with
     | Ok ind -> m.data.!(ind) <- x
     | Error ind ->
         Utils.insert m.indices ind inner;
         Utils.insert m.data ind x;
         Indptr.record_new_element m.indptr outer);
  if inner >= inner_dims m then set_inner_dims m (inner + 1)

let insert (m : 'a t) row col x =
  match m.storage with
  | CSR -> insert_outer_inner m row col x
  | CSC -> insert_outer_inner m col row x

let density (m : 'a t) = float_of_int (nnz m) /. float_of_int (m.nrows * m.ncols)

let diag (m : 'a t) =
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
  Csvec.{ dim; indices; data }

let into_csr (m : 'a t) = match m.storage with CSR -> m | CSC -> to_other_storage m
let to_csr (m : 'a t) = match m.storage with CSR -> copy m | CSC -> to_other_storage m
let into_csc (m : 'a t) = match m.storage with CSR -> to_other_storage m | CSC -> m
let to_csc (m : 'a t) = match m.storage with CSR -> to_other_storage m | CSC -> copy m
let is_csr (m : 'a t) = m.storage = CSR
let is_csc (m : 'a t) = m.storage = CSC

let map f (m : 'a t) =
  let m2 = copy m in
  Utils.map_inplace f m2.data;
  m2

let map_inplace f (m : 'a t) = Utils.map_inplace f m.data

let max_outer_nnz (m : 'a t) =
  let r = ref 0 in
  Indptr.iter_outer m.indptr (fun start stop -> r := max !r (stop - start));
  !r

let to_dense (m : float t) =
  let res = Array.make_matrix m.nrows m.ncols 0. in
  iterrc (fun row col x -> res.(row).(col) <- x) m;
  res

let degrees (m : 'a t) =
  let count = Array.make (outer_dims m) 0 in
  iteroi
    (fun outer inner _ -> if outer <> inner then count.(outer) <- count.(outer) + 1)
    m;
  count

let to_inner_onehot (m : 'a t) =
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

  { storage = m.storage; nrows = m.nrows; ncols = m.ncols; indptr; indices; data }

let to_col (v : 'a Csvec.t) =
  let indptr = Dynarray.of_array [| 0; Dynarray.length v.indices |] in
  {
    storage = CSC;
    nrows = v.dim;
    ncols = 1;
    indptr;
    indices = Dynarray.copy v.indices;
    data = Dynarray.copy v.data;
  }

let to_row (v : 'a Csvec.t) =
  let indptr = Dynarray.of_array [| 0; Dynarray.length v.indices |] in
  {
    storage = CSR;
    nrows = 1;
    ncols = v.dim;
    indptr;
    indices = Dynarray.copy v.indices;
    data = Dynarray.copy v.data;
  }
