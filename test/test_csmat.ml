open Spurs
open Spurs.Csmat
open Alcotest

let csmat_int = testable (Fmt.of_to_string (show Fmt.int)) (Csmat.equal ( = ))
let csmat = testable (Fmt.of_to_string (show Fmt.float)) (Csmat.equal ( = ))
let csmat_result = result csmat string
let csvec = testable (Fmt.of_to_string (Csvec.show Fmt.float)) (Csvec.equal ( = ))
let nnz_index_testable = testable (Fmt.of_to_string Common.Nnz_index.show) Common.Nnz_index.equal
let nnz_index_option = option nnz_index_testable

let test_valid_1 () =
  let indices = [| 2; 4; 0; 3; 1 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 4 in
  let inner = 5 in
  check (result unit string) "Valid Sparse matrix 1" (Ok ())
    (check_structure inner outer indptr indices)

let test_valid_2 () =
  let indices =
    [
      [| 0; 3; 1; 4; 7; 2; 5; 6 |];
      [| 9; 12; 0; 3; 6; 8; 11; 2 |];
      [| 5; 9; 1; 4; 7; 3; 10; 6 |];
      [| 11; 0; 8; 12; 5; 9; 2; 7 |];
      [| 13; 1; 6; 10; 4; 8; 11; 3 |];
      [| 12; 5; 9; 13; 7; 10; 1; 8 |];
      [| 14; 2; 6; 12; 0; 9; 3; 7 |];
      [| 11; 4; 10; 5; 13; 2; 6; 14 |];
    ]
    |> Array.concat |> Dynarray.of_array
  in
  let indptr =
    [
      [| 0; 2; 5; 7; 10; 13; 15 |];
      [| 18; 21; 23; 25; 28; 30; 33 |];
      [| 36; 39; 41; 44; 46; 49; 52 |];
      [| 54; 57; 59; 61; 64 |];
    ]
    |> Array.concat |> Dynarray.of_array
  in
  let outer = 25 in
  let inner = 15 in
  check (result unit string) "Valid Sparse matrix 2" (Ok ())
    (check_structure inner outer indptr indices)

let test_invalid_indptr_length () =
  let indices = [| 2; 4; 0; 3; 1 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 3 in
  let inner = 5 in
  check (result unit string) "Should error with invalid indptr length"
    (Error "Indptr length does not match dimension")
    (check_structure inner outer indptr indices)

let test_invalid_negative_index () =
  let indices = [| 2; 4; 0; -3; 1 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 4 in
  let inner = 5 in
  check (result unit string) "Should error with negative index" (Error "Negative index")
    (check_structure inner outer indptr indices)

let test_invalid_mismatch_indices_indptr () =
  let indices = [| 2; 4; 0; 3 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 4 in
  let inner = 5 in
  check (result unit string) "Should error with size mismatch"
    (Error "Indices length and indptr nnz mismatch")
    (check_structure inner outer indptr indices)

let test_invalid_unsorted_indices () =
  let indices = [| 2; 3; 4; 1; 3 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 4 in
  let inner = 5 in
  check (result unit string) "Should error with unsorted indices" (Error "Indices not sorted")
    (check_structure inner outer indptr indices)

let test_invalid_index_oob () =
  let indices = [| 2; 3; 0; 1; 5 |] |> Dynarray.of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> Dynarray.of_array in
  let outer = 4 in
  let inner = 5 in
  check (result unit string) "Should error with index out of bounds"
    (Error "Index larger than inner dimension")
    (check_structure inner outer indptr indices)

let test_new_csr_success () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices data in
  check bool "Should be valid CSR" true (Result.is_ok m);
  let m = Result.get_ok m in
  check (array int) "Should contain same indptr" indptr (get_indptr m |> Dynarray.to_array);
  check (array int) "Should contain same indices" indices (get_indices m |> Dynarray.to_array);
  check (array (float 0.000001)) "Should contain same data" data (get_data m |> Dynarray.to_array)

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
  let data = [| 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices_fail data in
  check csmat_result "Should return error" (Result.error "Index larger than inner dimension") m

let test_invalid_bad_nnz_count () =
  let indptr_fail = [| 0; 1; 2; 4 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr_fail indices data in
  check csmat_result "Should return error" (Result.error "Indices length and indptr nnz mismatch") m

let test_invalid_data_indices_mismatch1 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices_fail = [| 0; 1 |] in
  let data = [| 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices_fail data in
  check csmat_result "Should return error" (Result.error "data and indices have different sizes") m

let test_invalid_data_indices_mismatch2 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data_fail = [| 9.; 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices data_fail in
  check csmat_result "Should return error" (Result.error "data and indices have different sizes") m

let test_invalid_data_indices_mismatch3 () =
  let indptr = [| 0; 1; 2; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data_fail = [| 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices data_fail in
  check csmat_result "Should return error" (Result.error "data and indices have different sizes") m

let test_invalid_unsorted () =
  let indptr = [| 0; 2; 1; 3 |] in
  let indices = [| 0; 1; 2 |] in
  let data = [| 9.; 9.; 9. |] in
  let m = try_new_csr (3, 3) indptr indices data in
  check csmat_result "Should return error" (Result.error "Indptr should be sorted") m

let test_invalid_indices_ordering () =
  let indptr = [| 0; 2; 4; 5; 6; 7 |] in
  let indices = [| 3; 2; 3; 4; 2; 1; 3 |] in
  let data = [| 1.; 1.; 1.; 1.; 1.; 1.; 1. |] in
  let m = try_new_csr (5, 5) indptr indices data in
  check csmat_result "Should return error" (Result.error "Indices not sorted") m

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
  let data = [| 1.; 1.; 1.; 1.; 1.; 1. |] in
  let m = try_new_csc (3, 4) indptr indices data in
  check csmat_result "Should return error" (Result.error "Indptr length does not match dimension") m

let test_csr_unsorted_indices () =
  let indptr = [| 0; 3; 3; 5; 6; 7 |] in
  let indices_sorted = [| 1; 2; 3; 2; 3; 4; 4 |] in
  let indices_shuffled = [| 1; 3; 2; 2; 3; 4; 4 |] in
  let data = [| 0; 1; 2; 3; 4; 5; 6 |] in
  let m = new_csr_from_unsorted (5, 5) indptr indices_shuffled (Array.copy data) in
  check bool "Should be valid" true (Result.is_ok m);
  let m = Result.get_ok m in
  check (array int) "Should have sorted indices" indices_sorted (Dynarray.to_array m.indices);
  Utils.swap_arr data 1 2;
  check (array int) "Should have rearranged data" data (Dynarray.to_array m.data)

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
  check csmat_int "Should be equal" csc_truth csc;
  check csmat_int "Should be equal" csr csr_again

let test_csr_to_csc_2 () =
  let indptr_csr = [| 0; 2; 4; 7; 9; 12 |] in
  let indices_csr = [| 0; 4; 1; 5; 0; 2; 6; 3; 5; 1; 4; 6 |] in
  let data_csr = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 |] in
  let indptr_csc = [| 0; 2; 4; 5; 6; 8; 10; 12 |] in
  let indices_csc = [| 0; 2; 1; 4; 2; 3; 0; 4; 1; 3; 2; 4 |] in
  let data_csc = [| 1; 5; 3; 10; 6; 8; 2; 11; 4; 9; 7; 12 |] in
  let csr = new_csr (5, 7) indptr_csr indices_csr data_csr in
  let csc = new_csc (5, 7) indptr_csc indices_csc data_csc in
  check csmat_int "Should be equal" csc (to_other_storage csr);
  check csmat_int "Should be equal" csr (to_other_storage csc)

let test_csr_from_dense () =
  let m = Utils.eye 3 in
  let m_sparse = csr_from_dense m in
  check csmat "Should be equal" (eye_csr 3) m_sparse;

  let m = [| [| 1.; 0.; 2.; 1e-7; 1. |]; [| 0.; 0.; 0.; 1.; 0. |]; [| 3.; 0.; 1.; 0.; 0. |] |] in
  let m_sparse = csr_from_dense m in
  let m_expected =
    new_csr (3, 5) [| 0; 3; 4; 6 |] [| 0; 2; 4; 3; 0; 2 |] [| 1.; 2.; 1.; 1.; 3.; 1. |]
  in
  check csmat "Should be equal" m_sparse m_expected

let test_csc_from_dense () =
  let m = Utils.eye 3 in
  let m_sparse = csc_from_dense m in
  check csmat "Should be equal" (eye_csc 3) m_sparse;

  let m = [| [| 1.; 0.; 2.; 1e-7; 1. |]; [| 0.; 0.; 0.; 1.; 0. |]; [| 3.; 0.; 1.; 0.; 0. |] |] in
  let m_sparse = csc_from_dense m in
  let m_expected =
    new_csc (3, 5) [| 0; 2; 2; 4; 5; 6 |] [| 0; 2; 0; 2; 1; 0 |] [| 1.; 3.; 2.; 1.; 1.; 1. |]
  in
  check csmat "Should be equal" m_expected m_sparse

let test_scale () =
  let m = scale 2. (eye_csr 3) in
  let m_expected = new_csr (3, 3) [| 0; 1; 2; 3 |] [| 0; 1; 2 |] [| 2.; 2.; 2. |] in
  check csmat "Should be equal" m_expected m

let test_nnz_index () =
  let m = eye_csr 11 in
  check nnz_index_option "" None (nnz_index m 2 3);
  check nnz_index_option "" None (nnz_index m 5 7);
  check nnz_index_option "" None (nnz_index m 0 11);
  check nnz_index_option "" (Some (NNZ 0)) (nnz_index m 0 0);
  check nnz_index_option "" (Some (NNZ 7)) (nnz_index m 7 7);
  check nnz_index_option "" (Some (NNZ 10)) (nnz_index m 10 10);

  let index = nnz_index m 8 8 |> Option.get in
  check (float 0.000001) "" 1. m.!!(index);

  m.!!(index) <- 2.;
  check (float 0.000001) "" 2. m.!!(index)

let test_index () =
  let m = eye_csr 11 in
  check (option (float 0.000001)) "" None m.@(2, 3);
  check (option (float 0.000001)) "" None m.@(5, 7);
  check (option (float 0.000001)) "" None m.@(0, 12);
  check (option (float 0.000001)) "" (Some 1.) m.@(0, 0);
  check (option (float 0.000001)) "" (Some 1.) m.@(3, 3);

  m.@(8, 8) <- 2.;
  check (option (float 0.000001)) "" (Some 2.) m.@(8, 8)

let test_append_outer () =
  let m_array = [| [| 1.; 2.; 3. |]; [| 4.; 5.; 6. |]; [| 7.; 8.; 9. |] |] in
  let m = csr_from_dense m_array in
  let v = [| 10.; 11.; 12. |] in
  let m_expected = csr_from_dense (Array.append m_array [| v |]) in
  append_outer m v;

  check csmat "" m_expected m

let test_append_outer_2 () =
  let m = eye_csr 3 in
  append_outer m [| 0.; 0.; 0. |];
  let m = to_other_storage m in
  append_outer m [| 0.; 0.; 0.; 1. |];

  check csmat "" (eye_csc 4) m

let test_insert_existing () =
  let m = eye_csr 2 in
  insert m 0 0 2.;
  let m_expected = csr_from_dense [| [| 2.; 0. |]; [| 0.; 1. |] |] in
  check csmat "" m_expected m

let test_insert_no_resize () =
  let m = eye_csr 2 in
  insert m 1 0 2.;
  let m_expected = csr_from_dense [| [| 1.; 0. |]; [| 2.; 1. |] |] in
  check csmat "" m_expected m

let test_insert_oob () =
  let m = empty CSR in
  insert m 0 0 1.;
  insert m 1 1 1.;
  insert m 2 2 1.;
  check csmat "" m (eye_csr 3)

let test_insert_formats () =
  let m =
    csr_from_dense [| [| 1.; 2.; 3. |]; [| 0.; 0.; 0. |]; [| 0.; 0.; 0. |]; [| 4.; 5.; 6. |] |]
  in
  let m2 = to_other_storage m in
  insert m 1 1 999.;
  insert m2 1 1 999.;
  let m2 = to_other_storage m2 in
  check csmat "" m m2

let test_density () =
  let m = eye_csr 3 in
  check (float 0.00001) "" (3. /. 9.) (density m)

let test_diag () =
  let m = eye_csr 3 in
  let expected = Csvec.new_csvec 3 [| 0; 1; 2 |] [| 1.; 1.; 1. |] in
  check csvec "" expected (diag m)

let test_diag_2 () =
  let m = csr_from_dense [| [| 0.; 0.; 0. |]; [| 0.; 1.; 0. |]; [| 0.; 0.; 0. |] |] in
  let expected = Csvec.new_csvec 3 [| 1 |] [| 1. |] in
  check csvec "" expected (diag m)

let test_map () =
  let m = eye_csr 2 in
  let expected = csr_from_dense [| [| 2.; 0. |]; [| 0.; 2. |] |] in
  let m2 = Csmat.map (fun x -> x +. 1.) m in
  check csmat "" expected m2;
  (* unchanged *)
  check (float 0.00001) "" 1. m.!!(NNZ 0)

let test_map_inplace () =
  let m = eye_csr 2 in
  let expected = csr_from_dense [| [| 2.; 0. |]; [| 0.; 2. |] |] in
  Csmat.map_inplace (fun x -> x +. 1.) m;
  check csmat "" expected m

let test_max_outer_nnz () =
  let m = eye_csc 3 in
  check int "" 1 (max_outer_nnz m);

  insert m 1 2 99.;
  check int "" 2 (max_outer_nnz m)

let test_to_dense () =
  let m = [| [| 1.; 0.; 2.; 1e-7; 1. |]; [| 0.; 0.; 0.; 1.; 0. |]; [| 3.; 0.; 1.; 0.; 0. |] |] in
  let m_csr = csr_from_dense m in
  let m_csc = csc_from_dense m in
  check (array (array (float 0.00001))) "" m (to_dense m_csr);
  check (array (array (float 0.00001))) "" m (to_dense m_csc)

let test_degrees () =
  (*
    1 0 0 3 1
    0 2 0 0 0
    0 0 0 1 0 
    3 0 1 1 0 
    1 0 0 0 1
  *)
  let indptr = [| 0; 3; 4; 5; 8; 10 |] in
  let indices = [| 0; 3; 4; 1; 4; 0; 2; 3; 0; 4 |] in
  let data = [| 1; 3; 1; 2; 1; 3; 1; 1; 1; 1 |] in
  let m = new_csc (5, 5) indptr indices data in
  check (array int) "" [| 2; 0; 1; 2; 1 |] (degrees m)

let test_zero () =
  let m = zero (3, 3) in
  let expected = csr_from_dense (Array.make_matrix 3 3 0.) in
  check csmat "" expected m

let test_onehot_zero () =
  let m = zero (3, 3) in
  check csmat "" m (to_inner_onehot m)

let test_onehot_eye () =
  let m = new_csr (2, 2) [| 0; 2; 4 |] [| 0; 1; 0; 1 |] [| 2.; 0.; 0.; 2. |] in
  check csmat "" (eye_csr 2) (to_inner_onehot m)

let test_onehot_csc () =
  let m = new_csc (2, 3) [| 0; 0; 1; 1 |] [| 1 |] [| 2. |] in
  let expected = new_csc (2, 3) [| 0; 0; 1; 1 |] [| 1 |] [| 1. |] in
  check csmat "" expected (to_inner_onehot m)

let () =
  run "Sparse.Csmat"
    [
      ( "check_structure",
        [
          test_case "Valid 5x5" `Quick test_valid_1;
          test_case "Valid 25x15" `Quick test_valid_2;
          test_case "Invalid indptr length" `Quick test_invalid_indptr_length;
          test_case "Invalid negative index" `Quick test_invalid_negative_index;
          test_case "Invalid index mismatch" `Quick test_invalid_mismatch_indices_indptr;
          test_case "Invalid unsorted indices" `Quick test_invalid_unsorted_indices;
          test_case "Invalid index out of bounds" `Quick test_invalid_index_oob;
        ] );
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
          test_case "Zero" `Quick test_zero;
        ] );
      ( "lookups",
        [
          test_case "NNZ lookups" `Quick test_nnz_index;
          test_case "Row/col lookups" `Quick test_index;
        ] );
      ("operations", [ test_case "Scaling" `Quick test_scale ]);
      ( "modifications",
        [
          test_case "Append outer" `Quick test_append_outer;
          test_case "Append outer 2" `Quick test_append_outer_2;
          test_case "Inserting existing element" `Quick test_insert_existing;
          test_case "Inserting without resizing" `Quick test_insert_no_resize;
          test_case "Inserting out of bounds" `Quick test_insert_oob;
          test_case "Inserting in CSC" `Quick test_insert_formats;
        ] );
      ( "other",
        [
          test_case "Density" `Quick test_density;
          test_case "Diag" `Quick test_diag;
          test_case "Diag 2" `Quick test_diag_2;
          test_case "Map" `Quick test_map;
          test_case "Map in place" `Quick test_map_inplace;
          test_case "Max outer nnz" `Quick test_max_outer_nnz;
          test_case "To dense" `Quick test_to_dense;
          test_case "Degrees" `Quick test_degrees;
          test_case "Onehot (zero)" `Quick test_onehot_zero;
          test_case "Onehot (eye)" `Quick test_onehot_eye;
          test_case "Onehot (CSC)" `Quick test_onehot_csc;
        ] );
    ]
