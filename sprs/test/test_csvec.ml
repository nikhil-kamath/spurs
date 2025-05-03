open Sprs.Sparse
open Sprs.Csvec
open Alcotest

let cs_vec_base_float =
  testable (Fmt.of_to_string (Cs_vec_base.show Fmt.float)) (Cs_vec_base.equal ( = ))

let cs_mat_base_float =
  testable (Fmt.of_to_string (Cs_mat_base.show Fmt.float)) (Cs_mat_base.equal ( = ))

let test_invalid_unsorted () =
  let indices = [| 1; 3; 2 |] in
  let data = [| 0.; 0.; 0. |] in
  let v = try_new_csvec 4 indices data in
  check (result cs_vec_base_float string) "" (Error "Unsorted indices") v

let test_invalid_mismatch () =
  let indices = [| 1; 2; 3 |] in
  let data = [| 0.; 0. |] in
  let v = try_new_csvec 4 indices data in
  check (result cs_vec_base_float string) "" (Error "Indices and data have unequal lengths") v

let test_invalid_oob () =
  let indices = [| 1; 2; 3 |] in
  let data = [| 0.; 0.; 0. |] in
  let v = try_new_csvec 3 indices data in
  check (result cs_vec_base_float string) "" (Error "Indices larger than vector size") v

let test_from_unsorted () =
  let indices = [| 1; 3; 2 |] in
  let data = [| 111.; 333.; 222. |] in
  let v = new_from_unsorted 5 indices data in
  let expected = new_csvec 5 [| 1; 2; 3 |] [| 111.; 222.; 333. |] in
  check bool "Should be valid" true (Result.is_ok v);
  check cs_vec_base_float "" expected (Result.get_ok v)

let test_append () =
  let v = new_csvec 5 [| 0; 1; 2 |] [| 0.; 0.; 0. |] in
  let expected = new_csvec 5 [| 0; 1; 2; 3 |] [| 0.; 0.; 0.; 0. |] in
  append v 3 0.;
  check cs_vec_base_float "" expected v

let test_append_oob () =
  let v = empty 3 in
  check_raises "Should raise" (VectorException "Out-of-bounds append") (fun () -> append v 5 5.)

let test_append_unsorted () =
  let v = new_csvec 5 [| 0; 3 |] [| 0.; 0. |] in
  check_raises "Should raise" (VectorException "Unsorted append") (fun () -> append v 2 0.)

let test_to_col () =
  let v = new_csvec 3 [| 0; 2 |] [| 99.; 99. |] in
  let expected = Sprs.Csmat.csc_from_dense [| [| 99. |]; [| 0. |]; [| 99. |] |] in
  check cs_mat_base_float "" expected (to_col v)

let test_to_row () =
  let v = new_csvec 3 [| 0; 2 |] [| 99.; 99. |] in
  let expected = Sprs.Csmat.csr_from_dense [| [| 99.; 0.; 99. |] |] in
  check cs_mat_base_float "" expected (to_row v)

let test_l1_norm () =
  let v = empty 0 in
  check (float 0.00001) "" 0. (l1_norm v);

  let v = new_csvec 8 [| 0; 1; 4; 5; 7 |] [| 0.; -1.; 4.; -5.; 7. |] in
  check (float 0.00001) "" (1. +. 4. +. 5. +. 7.) (l1_norm v)

let test_l2_norm () =
  let v = empty 0 in
  check (float 0.00001) "" 0. (l2_norm v);

  let v = new_csvec 8 [| 0; 1; 4; 5; 7 |] [| 0.; 1.; 4.; 5.; 7. |] in
  let expected = 1. +. 16. +. 25. +. 49. in
  check (float 0.00001) "" expected (squared_l2_norm v);

  let expected = Float.sqrt expected in
  check (float 0.00001) "" expected (l2_norm v)

let test_norm () =
  let v = empty 4 in
  check (float 0.00001) "" 0. (norm v infinity);
  check (float 0.00001) "" 0. (norm v 0.);
  check (float 0.00001) "" 0. (norm v 4.);

  let v = new_csvec 8 [| 0; 1; 4; 5; 7 |] [| 0.; 1.; -4.; 5.; -7. |] in
  check (float 0.00001) "" 7. (norm v infinity);
  check (float 0.00001) "" 0. (norm v (-.infinity));
  check (float 0.00001) "" 4. (norm v 0.);
  check (float 0.00001) "" (l1_norm v) (norm v 1.);
  check (float 0.00001) "" (l2_norm v) (norm v 2.)

let test_normalize () =
  let v = empty 5 in
  normalize v;
  check int "" 0 (nnz v);

  let v = new_csvec 8 [| 1; 4; 5 |] [| 0.; 0.; 0. |] in
  let expected = Cs_vec_base.copy v in
  normalize v;
  check int "" 3 (nnz v);
  check cs_vec_base_float "" expected v;

  let v = new_csvec 8 [| 0; 1; 4; 5; 7 |] [| 0.; 1.; 4.; 5.; 7. |] in
  let norm = Float.sqrt (1. +. (4. *. 4.) +. (5. *. 5.) +. (7. *. 7.)) in
  let expected =
    new_csvec 8 [| 0; 1; 4; 5; 7 |] [| 0.; 1. /. norm; 4. /. norm; 5. /. norm; 7. /. norm |]
  in
  normalize v;
  check cs_vec_base_float "" expected v;
  check (float 0.00001) "" 0. (l2_norm v -. 1.)

let () =
  run "Sparse.Csvec"
    [
      ( "new",
        [
          test_case "Invalid, unsorted indices" `Quick test_invalid_unsorted;
          test_case "Invalid, mismatched dimensions" `Quick test_invalid_mismatch;
          test_case "Invalid, out of bounds" `Quick test_invalid_oob;
          test_case "Valid, from sorted" `Quick test_from_unsorted;
        ] );
      ( "modifications",
        [
          test_case "Appending" `Quick test_append;
          test_case "Appending out of bounds" `Quick test_append_oob;
          test_case "Appending unsorted" `Quick test_append_unsorted;
        ] );
      ( "conversions",
        [ test_case "To col" `Quick test_to_col; test_case "To row" `Quick test_to_row ] );
      ( "math",
        [
          test_case "L1 norm" `Quick test_l1_norm;
          test_case "L2 norm" `Quick test_l2_norm;
          test_case "Norm" `Quick test_norm;
          test_case "Normalize" `Quick test_normalize;
        ] );
    ]
