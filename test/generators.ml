open Spurs
(** QCheck generators and testing *)

let ( =~ ) a b = Float.abs (a -. b) < 1e-5

let indices_gen n =
  let open QCheck2.Gen in
  shuffle_l (List.init (n * 10) Fun.id) >|= List.take n >|= List.sort compare

let data_gen n =
  let open QCheck2.Gen in
  list_repeat n (float_range (-50.) 50.)

let csvec_gen =
  QCheck2.Gen.(
    sized (fun n ->
        let n = n + 1 in
        let* indices = indices_gen n >|= Dynarray.of_list in
        let* data = data_gen n >|= Dynarray.of_list in
        let* gap = 1 -- 3 in
        let dim = Dynarray.get_last indices + gap in
        return Csvec.{ dim; indices; data }))

let csvec_print = Csvec.show Fmt.float
