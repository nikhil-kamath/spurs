open Spurs.Indptr

let indptr_1 = [| 0; 1; 2; 3 |] |> Dynarray.of_array
let indptr_2 = [| 0; 1; 2; 4; 5 |] |> Dynarray.of_array
let test_nnz_1 () = Alcotest.(check int) "Indptr 1 non-zeros" 3 (nnz indptr_1)
let test_nnz_2 () = Alcotest.(check int) "Indptr 2 non-zeros" 5 (nnz indptr_2)
let test_nnz_empty () = Alcotest.(check int) "Indptr empty array" 0 (nnz (Dynarray.create ()))

let () =
  let open Alcotest in
  run "Indptr"
    [
      ( "nnz",
        [
          test_case "3-array" `Quick test_nnz_1;
          test_case "5-array" `Quick test_nnz_2;
          test_case "empty array" `Quick test_nnz_empty;
        ] );
    ]
