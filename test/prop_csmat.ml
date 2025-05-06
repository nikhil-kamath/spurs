open Spurs.Csmat
open Alcotest
open QCheck_alcotest
open Generators

(* test configs *)
let prop_csmat name p =
  QCheck2.Test.make ~name ~count:1_000 ~print:csmat_print csmat_gen p |> to_alcotest

let prop_dense name p =
  QCheck2.(Test.make ~name ~count:1_000 ~print:(Print.array (Print.array Print.float)) dense_gen) p
  |> to_alcotest

let assume = QCheck2.assume

let prop_nnz_get_works =
  prop_csmat "m.!!(i) works" (fun m ->
      let n = nnz m in
      assume (n > 0);
      for i = 0 to n - 1 do
        ignore m.!!(NNZ i)
      done;
      true)

let prop_to_from_dense_csr =
  prop_dense "m = m |> csr_of_dense |> to_dense" (fun m -> m = (m |> csr_from_dense |> to_dense))

let prop_to_from_dense_csc =
  prop_dense "m = m |> csc_of_dense |> to_dense" (fun m -> m = (m |> csc_from_dense |> to_dense))

let () =
  run "Spurs.Csmat (properties)"
    [
      ("matrix properties", [ prop_to_from_dense_csr; prop_to_from_dense_csc; prop_nnz_get_works ]);
    ]
