open Spurs
open Spurs.Ops
open Alcotest
open Generators

let csvec = testable (Fmt.of_to_string (Csvec.show Fmt.float)) (Csvec.equal ( = ))

let test_v_add () =
  let v1 = [| 0.; 0.; 1.; 1.; 0.; 1. |] |> Csvec.of_dense in
  let v2 = [| 0.; 1.; 0.; 0.; 0.; 1. |] |> Csvec.of_dense in
  let ex = [| 0.; 1.; 1.; 1.; 0.; 2. |] |> Csvec.of_dense in
  check csvec "" ex (add_v v1 v2)

let test_v_add_mismatch () =
  let v1 = [| 0.; 0.; 1.; 1.; 0.; 1. |] |> Csvec.of_dense in
  let v2 = [| 0.; 1.; 0.; 0.; 0.; 1.; 0. |] |> Csvec.of_dense in
  check_raises "" (OpException "adding two different-dimension vectors") (fun () ->
      add_v v1 v2 |> ignore)

let test_v_dot () =
  let v1 = [| 0.; 0.; 1.; 3.; 0.; 1. |] |> Csvec.of_dense in
  let v2 = [| 0.; 3.; 0.; 2.; 0.; 5. |] |> Csvec.of_dense in
  check (float 0.000001) "" 11. (dot_v v1 v2)

let test_v_dot_mismatch () =
  let v1 = [| 0.; 0.; 1.; 3.; 0.; 1. |] |> Csvec.of_dense in
  let v2 = [| 0.; 3.; 0.; 2.; 0. |] |> Csvec.of_dense in
  check_raises "" (OpException "dot-product of two different-dimension vectors") (fun () ->
      dot_v v1 v2 |> ignore)

let test_dot_normalize () =
  let v = [| 0.; 0.; 1.; 3.; 0.; 1. |] |> Csvec.of_dense in
  check (float 0.000001) "" (Csvec.squared_l2_norm v) (dot_v v v)

let prop_dot_self =
  QCheck2.(
    Test.make ~name:"v * v = norm^2 (v)" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        dot_v v v =~ Csvec.squared_l2_norm v))
  |> QCheck_alcotest.to_alcotest

let prop_plus_minus =
  QCheck2.(
    Test.make ~name:"v + 0 = v" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        Csvec.equal ( =~ ) (add_v v (Csvec.scale 0. v)) v))
  |> QCheck_alcotest.to_alcotest

let prop_plus_plus =
  QCheck2.(
    Test.make ~name:"v + v = v * 2" ~count:1_000 ~print:csvec_print csvec_gen (fun v ->
        Csvec.equal ( =~ ) (add_v v v) (Csvec.scale 2. v)))
  |> QCheck_alcotest.to_alcotest

let prop_to_from_dense =
  QCheck2.(
    Test.make ~name:"v = v |> of_dense |> to_dense" ~count:1_000 ~print:(Print.array Print.float)
      Gen.(array (float_range (-10.) 10.))
      (fun l ->
        Printf.printf "Vector: %s\n" (csvec_print (Csvec.of_dense l));

        l = (l |> Csvec.of_dense |> Csvec.to_dense)))
  |> QCheck_alcotest.to_alcotest

let () =
  run "Spurs.Ops"
    [
      ( "Vector-Vector operations",
        [
          test_case "Addition" `Quick test_v_add;
          test_case "Addition, mismatched lengths" `Quick test_v_add_mismatch;
          test_case "Dot product" `Quick test_v_dot;
          test_case "Dot product, mismatched lengths" `Quick test_v_dot_mismatch;
          test_case "Self dot product" `Quick test_dot_normalize;
        ] );
      ("Properties", [ prop_dot_self; prop_plus_minus; prop_plus_plus; prop_to_from_dense ]);
    ]
