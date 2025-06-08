open Spurs
open Spurs.Ops
open Alcotest

let csvec = testable (Fmt.of_to_string (Csvec.show Fmt.float)) (Csvec.equal ( = ))
let csmat = testable (Fmt.of_to_string (Csmat.show Fmt.float)) (Csmat.equal ( = ))

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

let test_mult () =
  let open Csmat in
  let a =
    [| [| 0.0; 0.0; 2.0; 0.0 |]; [| 0.0; 0.0; 3.0; 4.0 |]; [| 5.0; 0.0; 0.0; 0.0 |] |]
    |> csr_from_dense
  in
  let b = [| [| 0.0; 6.0 |]; [| 0.0; 0.0 |]; [| 7.0; 0.0 |]; [| 0.0; 8.0 |] |] |> csc_from_dense in
  let expected = [| [| 14.0; 0.0 |]; [| 21.0; 32.0 |]; [| 0.0; 30.0 |] |] |> csr_from_dense in
  check csmat "" expected (mult a b)

let test_add () =
  let open Csmat in
  let a = [| [| 1.; 0.; 2. |]; [| 0.; 3.; 0. |]; [| 4.; 0.; 5. |] |] |> csr_from_dense in
  let b = [| [| 0.; 6.; 0. |]; [| 7.; 0.; 8. |]; [| 0.; 0.; 9. |] |] |> csr_from_dense in
  let expected = [| [| 1.; 6.; 2. |]; [| 7.; 3.; 8. |]; [| 4.; 0.; 14. |] |] |> csr_from_dense in
  check csmat "" expected (add a b)

let test_add_2 () =
  let open Csmat in
  let a =
    [| [| 1.; 0.; 2. |]; [| 0.; 3.; 0. |]; [| 4.; 0.; 5. |]; [| 4.; 0.; 5. |] |] |> csr_from_dense
  in
  let b =
    [| [| -1.; 6.; 0. |]; [| 7.; 0.; 8. |]; [| 0.; 0.; 9. |]; [| 4.; 0.; 5. |] |] |> csc_from_dense
  in
  let expected =
    [| [| 0.; 6.; 2. |]; [| 7.; 3.; 8. |]; [| 4.; 0.; 14. |]; [| 8.; 0.; 10. |] |] |> csc_from_dense
  in
  check csmat "" expected (add ~storage:CSC a b)

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
      ( "Matrix-Matrix operations",
        [
          test_case "Multiply" `Quick test_mult;
          test_case "Add" `Quick test_add;
          test_case "Add 2" `Quick test_add_2;
        ] );
    ]
