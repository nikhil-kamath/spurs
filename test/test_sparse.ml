open Spurs.Sparse.Utils
open Result

let check = Alcotest.(check Alcotest.(result unit string))
let of_array = Dynarray.of_array

let test_valid_1 () =
  let indices = [| 2; 4; 0; 3; 1 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 4 in
  let inner = 5 in
  check "Valid Sparse matrix 1" (ok ()) (check_compressed_structure inner outer indptr indices)

let test_valid_2 () =
  let indices =
    [|
      0;
      3;
      1;
      4;
      7;
      2;
      5;
      6;
      9;
      12;
      0;
      3;
      6;
      8;
      11;
      2;
      5;
      9;
      1;
      4;
      7;
      3;
      10;
      6;
      11;
      0;
      8;
      12;
      5;
      9;
      2;
      7;
      13;
      1;
      6;
      10;
      4;
      8;
      11;
      3;
      12;
      5;
      9;
      13;
      7;
      10;
      1;
      8;
      14;
      2;
      6;
      12;
      0;
      9;
      3;
      7;
      11;
      4;
      10;
      5;
      13;
      2;
      6;
      14;
    |]
    |> of_array
  in
  let indptr =
    [|
      0;
      2;
      5;
      7;
      10;
      13;
      15;
      18;
      21;
      23;
      25;
      28;
      30;
      33;
      36;
      39;
      41;
      44;
      46;
      49;
      52;
      54;
      57;
      59;
      61;
      64;
    |]
    |> of_array
  in
  let outer = 25 in
  let inner = 15 in
  check "Valid Sparse matrix 2" (ok ()) (check_compressed_structure inner outer indptr indices)

let test_invalid_indptr_length () =
  let indices = [| 2; 4; 0; 3; 1 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 3 in
  let inner = 5 in
  check "Should error with invalid indptr length"
    (error "Indptr length does not match dimension")
    (check_compressed_structure inner outer indptr indices)

let test_invalid_negative_index () =
  let indices = [| 2; 4; 0; -3; 1 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 4 in
  let inner = 5 in
  check "Should error with negative index" (error "Negative index")
    (check_compressed_structure inner outer indptr indices)

let test_invalid_mismatch_indices_indptr () =
  let indices = [| 2; 4; 0; 3 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 4 in
  let inner = 5 in
  check "Should error with size mismatch"
    (error "Indices length and indptr's nnz do not match")
    (check_compressed_structure inner outer indptr indices)

let test_invalid_unsorted_indices () =
  let indices = [| 2; 3; 4; 1; 3 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 4 in
  let inner = 5 in
  check "Should error with unsorted indices" (error "Indices are not sorted")
    (check_compressed_structure inner outer indptr indices)

let test_invalid_index_oob () =
  let indices = [| 2; 3; 0; 1; 5 |] |> of_array in
  let indptr = [| 0; 2; 2; 4; 5 |] |> of_array in
  let outer = 4 in
  let inner = 5 in
  check "Should error with index out of bounds"
    (error "Index is larger than inner dimension")
    (check_compressed_structure inner outer indptr indices)

let () =
  let open Alcotest in
  run "Sparse.Utils"
    [
      ( "check_compressed_structure",
        [
          test_case "Valid 5x5" `Quick test_valid_1;
          test_case "Valid 25x15" `Quick test_valid_2;
          test_case "Invalid indptr length" `Quick test_invalid_indptr_length;
          test_case "Invalid negative index" `Quick test_invalid_negative_index;
          test_case "Invalid index mismatch" `Quick test_invalid_mismatch_indices_indptr;
          test_case "Invalid unsorted indices" `Quick test_invalid_unsorted_indices;
          test_case "Invalid index out of bounds" `Quick test_invalid_index_oob;
        ] );
    ]
