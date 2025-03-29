open Sprs.Sparse
open Sprs.Csmat
open Alcotest

let cs_mat_base = testable (Fmt.of_to_string (Cs_mat_base.show Fmt.int)) (Cs_mat_base.equal ( = ))
let cs_mat_base_result = result cs_mat_base string

let test_new_csr_success () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices data in
  check bool "Should be valid CSR" true (Result.is_ok m);
  let m = Result.get_ok m in
  check (array int) "Should contain same indptr" indptr (Cs_mat_base.get_indptr m);
  check (array int) "Should contain same indices" indices (Cs_mat_base.get_indices m);
  check (array int) "Should contain same data" data (Cs_mat_base.get_data m)

let test_invalid_indptr_size () =
  let indptr_fail = [| 0; 1; 2 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9; 9; 9 |] in
  check_raises "Should raise"
    (MatrixException "Could not create sparse matrix: Indptr length does not match dimension")
    (fun () -> new_csr (3, 3) indptr_fail indices data |> ignore)

let test_invalid_oob_index () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices_fail = [| 0; 1; 4 |] in
  let data = [| 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices_fail data in
  check cs_mat_base_result "Should return error"
    (Result.error "Index is larger than inner dimension")
    m

let test_invalid_bad_nnz_count () =
  let indptr_fail = [| 0; 1; 2; 4 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr_fail indices data in
  check cs_mat_base_result "Should return error"
    (Result.error "Indices length and indptr's nnz do not match")
    m

let test_invalid_data_indices_mismatch1 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices_fail = [| 0; 1 |] in
  let data = [| 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices_fail data in
  check cs_mat_base_result "Should return error"
    (Result.error "data and indices have different sizes")
    m

let test_invalid_data_indices_mismatch2 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data_fail = [| 9; 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices data_fail in
  check cs_mat_base_result "Should return error"
    (Result.error "data and indices have different sizes")
    m

let test_invalid_data_indices_mismatch3 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data_fail = [| 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices data_fail in
  check cs_mat_base_result "Should return error"
    (Result.error "data and indices have different sizes")
    m

let test_invalid_unsorted () =
  let indptr = [| 0; 2; 1; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9; 9; 9 |] in
  let m = try_new_csr (3, 3) indptr indices data in
  check cs_mat_base_result "Should return error" (Result.error "Indptr should be sorted") m

let test_invalid_indices_ordering () =
  let indptr = [| 0; 2; 4; 5; 6; 7 |] in
  let indices = [| 3; 2; 3; 4; 2; 1; 3 |] in
  let data = [| 1; 1; 1; 1; 1; 1; 1 |] in
  let m = try_new_csr (5, 5) indptr indices data in
  check cs_mat_base_result "Should return error" (Result.error "Indices are not sorted") m

let test_new_csr_csc_success () =
  let indptr = [| 0; 2; 5; 6 |] in
  let indices = [| 2; 3; 1; 2; 3; 3 |] in
  let data = [| 1; 1; 1; 1; 1; 1 |] in
  let csr = try_new_csr (3, 4) indptr indices data in
  let csc = try_new_csc (4, 3) indptr indices data in
  check bool "Should be valid" true (Result.is_ok csr);
  check bool "Should be valid" true (Result.is_ok csc)

let test_invalid_csc_indptr_length () =
  let indptr = [| 0; 2; 5; 6 |] in
  let indices = [| 2; 3; 1; 2; 3; 3 |] in
  let data = [| 1; 1; 1; 1; 1; 1 |] in
  let m = try_new_csc (3, 4) indptr indices data in
  check cs_mat_base_result "Should return error"
    (Result.error "Indptr length does not match dimension")
    m

let () =
  run "Sparse.Csmat"
    [
      ( "new",
        [
          test_case "Valid 3x3" `Quick test_new_csr_success;
          test_case "Invalid indptr length" `Quick test_invalid_indptr_size;
          test_case "Invalid out of bounds indices" `Quick test_invalid_oob_index;
          test_case "Invalid number of non-zeroes" `Quick test_invalid_bad_nnz_count;
          test_case "Invalid data-indices mismatch 1" `Quick test_invalid_data_indices_mismatch1;
          test_case "Invalid data-indices mismatch 2" `Quick test_invalid_data_indices_mismatch2;
          test_case "Invalid data-indices mismatch 3" `Quick test_invalid_data_indices_mismatch3;
          test_case "Invalid unsorted indptr" `Quick test_invalid_unsorted;
          test_case "Invalid wrongly ordered indices" `Quick test_invalid_indices_ordering;
          test_case "Valid csr/csc" `Quick test_new_csr_csc_success;
          test_case "Invalid dims for csc" `Quick test_invalid_csc_indptr_length;
        ] );
    ]
