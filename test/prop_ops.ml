open Spurs
open Spurs.Ops
open Alcotest
open QCheck_alcotest
open Generators

let prop_dot_self =
  QCheck2.(
    Test.make ~name:"v * v = norm^2 (v)" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        dot_v v v =~ Csvec.squared_l2_norm v))
  |> QCheck_alcotest.to_alcotest

let prop_plus_minus =
  QCheck2.(
    Test.make ~name:"v + 0 = v" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        Csvec.equal ( =~ ) (add_v v (Csvec.scale 0. v)) v))
  |> to_alcotest

let prop_plus_plus =
  QCheck2.(
    Test.make ~name:"v + v = v * 2" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        Csvec.equal ( =~ ) (add_v v v) (Csvec.scale 2. v)))
  |> to_alcotest

let prop_to_from_dense =
  QCheck2.(
    Test.make ~name:"v = v |> of_dense |> to_dense" ~count:1_000 ~print:(Print.array Print.float)
      vec_gen (fun l -> l = (l |> Csvec.of_dense |> Csvec.to_dense)))
  |> to_alcotest

let () =
  run "Spurs.Ops (properties)"
    [ ("vector operation properties", [ prop_dot_self; prop_plus_minus; prop_plus_plus; prop_to_from_dense ]) ]
