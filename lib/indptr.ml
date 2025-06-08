open Utils

(* Fold over outer dimension, giving start and end indices, as well as outer dimension *)
let fold_outeri (indptr : int Dynarray.t) f x =
  let r = ref x in
  for i = 0 to Dynarray.length indptr - 2 do
    r := f i indptr.!(i) indptr.!(i + 1) !r
  done;
  !r

(* Fold over outer dimension, giving start and end indices for each outer dimension *)
let fold_outer (indptr : int Dynarray.t) f x =
  fold_outeri indptr (fun _ start stop acc -> f start stop acc) x

(** Iterate over outer dimension, giving start and end indices, as well as outer dimension
*)
let iter_outeri (indptr : int Dynarray.t) f =
  fold_outeri indptr (fun i s e _ -> f i s e) ()

(* Iterate over outer dimension, giving start and end indices for each outer dimension *)
let iter_outer (indptr : int Dynarray.t) f = fold_outer indptr (fun s e _ -> f s e) ()

let map_outer_list (indptr : int Dynarray.t) f =
  let l = ref [] in
  for i = 0 to Dynarray.length indptr - 2 do
    l := f indptr.!(i) indptr.!(i + 1) :: !l
  done;
  List.rev !l

let check_indices indptr indices =
  map_outer_list indptr (Utils.is_sorted_from indices) |> List.for_all Fun.id

let map_outer (indptr : int Dynarray.t) f = map_outer_list indptr f |> Array.of_list

let check_indptr_structure indptr =
  let open Result in
  let ( let* ) = bind in
  let* () =
    if Dynarray.length indptr < 1 then error "An indptr should have its len >= 1"
    else ok ()
  in
  let* () = if not (is_sorted indptr) then error "Indptr should be sorted" else ok () in
  ok ()

let outer_start_stop (indptr : int Dynarray.t) outer =
  (indptr.!(outer), indptr.!(outer + 1))

let record_new_element (indptr : int Dynarray.t) outer =
  for i = outer + 1 to Dynarray.length indptr - 1 do
    indptr.!(i) <- indptr.!(i) + 1
  done
