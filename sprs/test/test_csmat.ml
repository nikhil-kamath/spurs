open Sprs
open Sprs.Sparse
open Sprs.Csmat
open Alcotest

let cs_mat_base = testable (Fmt.of_to_string (Cs_mat_base.show Fmt.int)) (Cs_mat_base.equal ( = ))

let cs_mat_base_float =
  testable (Fmt.of_to_string (Cs_mat_base.show Fmt.float)) (Cs_mat_base.equal ( = ))

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

let test_csr_unsorted_indices () =
  let indptr = [| 0; 3; 3; 5; 6; 7 |] in
  let indices_sorted = [| 1; 2; 3; 2; 3; 4; 4 |] in
  let indices_shuffled = [| 1; 3; 2; 2; 3; 4; 4 |] in
  let data = [| 0; 1; 2; 3; 4; 5; 6 |] in
  let m = new_csr_from_unsorted (5, 5) indptr indices_shuffled (Array.copy data) in
  check bool "Should be valid" true (Result.is_ok m);
  let m = Result.get_ok m in
  check (array int) "Should have sorted indices" indices_sorted m.indices;
  Array_utils.swap data 1 2;
  check (array int) "Should have rearranged data" data m.data

let test_csr_with_empty_row () =
  let indptr = [| 0; 3; 3; 5; 6; 7 |] in
  let indices = [| 1; 2; 3; 2; 3; 4; 4 |] in
  let data = [| 0.7; 0.1; 0.3; 0.1; 0.6; 0.3; 0.5 |] in
  let m = try_new_csr (5, 5) indptr indices data in
  check bool "Should be valid" true (Result.is_ok m)

let test_csr_to_csc () =
  let indptr_csr = [| 0; 2; 4; 5; 6; 7 |] in
  let indices_csr = [| 2; 3; 3; 4; 2; 1; 3 |] in
  let data_csr = [| 3; 4; 2; 5; 5; 8; 7 |] in
  let indptr_csc = [| 0; 0; 1; 3; 6; 7 |] in
  let indices_csc = [| 3; 0; 2; 0; 1; 4; 1 |] in
  let data_csc = [| 8; 3; 5; 4; 2; 7; 5 |] in
  let csr = new_csr (5, 5) indptr_csr indices_csr data_csr in
  let csc_truth = new_csc (5, 5) indptr_csc indices_csc data_csc in
  let csc = to_other_storage csr in
  let csr_again = to_other_storage csc in
  check cs_mat_base "Should be equal" csc_truth csc;
  check cs_mat_base "Should be equal" csr csr_again

let test_csr_to_csc_2 () =
  let indptr_csr = [| 0; 2; 4; 7; 9; 12 |] in
  let indices_csr = [| 0; 4; 1; 5; 0; 2; 6; 3; 5; 1; 4; 6 |] in
  let data_csr = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 |] in
  let indptr_csc = [| 0; 2; 4; 5; 6; 8; 10; 12 |] in
  let indices_csc = [| 0; 2; 1; 4; 2; 3; 0; 4; 1; 3; 2; 4 |] in
  let data_csc = [| 1; 5; 3; 10; 6; 8; 2; 11; 4; 9; 7; 12 |] in
  let csr = new_csr (5, 7) indptr_csr indices_csr data_csr in
  let csc = new_csc (5, 7) indptr_csc indices_csc data_csc in
  check cs_mat_base "Should be equal" csc (to_other_storage csr);
  check cs_mat_base "Should be equal" csr (to_other_storage csc)

let test_csr_from_dense () =
  let m = Array_utils.eye 3 in
  let m_sparse = csr_from_dense m in
  check cs_mat_base_float "Should be equal" (eye_csr 3) m_sparse;

  let m = [| [| 1.; 0.; 2.; 1e-7; 1. |]; [| 0.; 0.; 0.; 1.; 0. |]; [| 3.; 0.; 1.; 0.; 0. |] |] in
  let m_sparse = csr_from_dense m in
  let m_expected =
    new_csr (3, 5) [| 0; 3; 4; 6 |] [| 0; 2; 4; 3; 0; 2 |] [| 1.; 2.; 1.; 1.; 3.; 1. |]
  in
  check cs_mat_base_float "Should be equal" m_sparse m_expected

let test_csc_from_dense () =
  let m = Array_utils.eye 3 in
  let m_sparse = csc_from_dense m in
  check cs_mat_base_float "Should be equal" (eye_csc 3) m_sparse;

  let m = [| [| 1.; 0.; 2.; 1e-7; 1. |]; [| 0.; 0.; 0.; 1.; 0. |]; [| 3.; 0.; 1.; 0.; 0. |] |] in
  let m_sparse = csc_from_dense m in
  let m_expected =
    new_csc (3, 5) [| 0; 2; 2; 4; 5; 6 |] [| 0; 2; 0; 2; 1; 0 |] [| 1.; 3.; 2.; 1.; 1.; 1. |]
  in
  check cs_mat_base_float "Should be equal" m_expected m_sparse

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
          test_case "Valid sorting indices" `Quick test_csr_unsorted_indices;
          test_case "Valid empty row" `Quick test_csr_with_empty_row;
          test_case "CSR <-> CSC conversion" `Quick test_csr_to_csc;
          test_case "CSR <-> CSC conversion 2" `Quick test_csr_to_csc_2;
          test_case "CSR from dense" `Quick test_csr_from_dense;
          test_case "CSC from dense" `Quick test_csc_from_dense;
        ] );
    ]
