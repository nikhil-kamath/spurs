open Spurs
(** QCheck generators and testing *)

let ( =~ ) a b = Float.abs (a -. b) < 1e-5

let elem_gen density =
  QCheck2.Gen.(
    let* x = float_bound_exclusive 1. in
    if x >= density then return 0. else float_range 1. 10.)

let indices_gen_sized n =
  let open QCheck2.Gen in
  shuffle_l (List.init (n * 10) Fun.id) >|= List.take n >|= List.sort compare >|= Array.of_list

let data_gen_sized n =
  let open QCheck2.Gen in
  let* density = float_bound_inclusive 1. in
  array_repeat n (elem_gen density)

let vec_gen = QCheck2.Gen.sized data_gen_sized
let x = List.sort
let csvec_gen = QCheck2.Gen.(sized (fun n -> data_gen_sized n >|= Csvec.of_dense))
let csvec_print = Csvec.show Fmt.float
let csmat_print = Csmat.show Fmt.float

let dense_gen_of density =
  QCheck2.Gen.(
    let* rows = 0 -- 10 in
    let* cols = 0 -- 10 in
    let col_gen = array_repeat cols (elem_gen density) in
    array_repeat rows col_gen)

let dense_gen = QCheck2.Gen.(float_bound_inclusive 1. >>= dense_gen_of)

let csmat_gen_of density =
  QCheck2.Gen.(
    let* mat = dense_gen_of density in
    let* f = oneofl [ Csmat.csr_from_dense; Csmat.csc_from_dense ] in
    return (f mat))

let csmat_gen = QCheck2.Gen.(float_bound_inclusive 1. >>= csmat_gen_of)
