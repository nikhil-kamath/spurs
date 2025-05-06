open Spurs
open Spurs.Ops
open Alcotest

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
    ]
