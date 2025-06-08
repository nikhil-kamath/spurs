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

let prop_nnz_get_works =
  prop_csmat "m.!!(i) works" (fun m ->
      for i = 0 to nnz m - 1 do
        ignore m.!!(NNZ i)
      done;
      true)

let prop_to_from_dense_csr =
  prop_dense "m = m |> csr_of_dense |> to_dense" (fun m -> m = (m |> csr_from_dense |> to_dense))

let prop_to_from_dense_csc =
  prop_dense "m = m |> csc_of_dense |> to_dense" (fun m -> m = (m |> csc_from_dense |> to_dense))

let prop_double_conversion =
  prop_csmat "m = m |> to_other |> to_other" (fun m ->
      m = (m |> to_other_storage |> to_other_storage))

let prop_insert =
  QCheck2.(
    Test.make ~name:"x = m |> insert (i, j) x |> get (i, j)" ~count:1_000
      ~print:(Print.triple csmat_print (Print.pair Print.int Print.int) Print.float)
      (Gen.triple csmat_gen (Gen.pair Gen.small_nat Gen.small_nat) Gen.float)
      (fun (m, (i, j), x) ->
        insert m i j x;
        m.@(i, j) |> Option.get = x))
  |> to_alcotest ~verbose:true

let () =
  run "Spurs.Csmat.Props"
    [
      ( "matrix properties",
        [
          prop_to_from_dense_csr;
          prop_to_from_dense_csc;
          prop_nnz_get_works;
          prop_double_conversion;
          prop_insert;
        ] );
    ]
