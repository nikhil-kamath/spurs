open Spurs
open Spurs.Ops
open Alcotest
open QCheck_alcotest
open Generators

let epsilon = 0.000001

(* approx equal to *)
let ( === ) = Csmat.equal (fun l r -> abs_float (l -. r) < epsilon)

(** {b Test Configs} *)
let prop_csvec name p =
  QCheck2.Test.make ~name ~count:1_000 ~print:csvec_print csvec_gen p |> to_alcotest

let prop_vec name p =
  QCheck2.(Test.make ~name ~count:1_000 ~print:(Print.array Print.float) vec_gen p) |> to_alcotest

let prop_csmat name p =
  QCheck2.Test.make ~name ~count:1_000 ~print:csmat_print csmat_gen p |> to_alcotest

(** {b Properties}*)

let prop_dot_self = prop_csvec "v * v = norm^2 (v)" (fun v -> dot_v v v =~ Csvec.squared_l2_norm v)

let prop_plus_minus =
  prop_csvec "v + 0 = v" (fun v -> Csvec.equal ( =~ ) (add_v v (Csvec.scale 0. v)) v)

let prop_plus_plus =
  prop_csvec "v + v = v * 2" (fun v -> Csvec.equal ( =~ ) (add_v v v) (Csvec.scale 2. v))

let prop_to_from_dense =
  prop_vec "v = v |> of_dense |> to_dense" (fun l -> l = (l |> Csvec.of_dense |> Csvec.to_dense))

let prop_identity =
  prop_csmat "a = a * I" (fun m ->
      let i = Csmat.eye_csc m.ncols in
      m === mult ~storage:m.storage m i)

let prop_zero =
  prop_csmat "a * 0 = 0" (fun m ->
      let z = Csmat.zero (m.ncols, 10) |> Csmat.into_csc in
      m *@ z === Csmat.zero (m.nrows, 10))

let prop_assoc =
  QCheck2.(
    Test.make ~name:"(a * b) * c = a * (b * c)" ~count:1_000
      ~print:(Print.triple csmat_print csmat_print csmat_print)
      (Gen.triple csmat_gen csmat_gen csmat_gen) (fun (a, b, c) ->
        assume (a.ncols = b.nrows);
        assume (b.ncols = c.nrows);
        a *@ b *@ c === a *@ (b *@ c)))
  |> to_alcotest

let prop_distrib_left =
  QCheck2.(
    Test.make ~name:"a * (b + c) = a * b + a * c" ~count:10_000
      ~print:(Print.triple csmat_print csmat_print csmat_print)
      (Gen.triple csmat_gen csmat_gen csmat_gen) (fun (a, b, c) ->
        assume (a.ncols = b.nrows);
        assume (b.nrows = c.nrows);
        assume (b.ncols = c.ncols);
        a *@ (b +@ c) === (a *@ b) +@ (a *@ c)))
  |> to_alcotest

let prop_distrib_right =
  QCheck2.(
    Test.make ~name:"(b + c) * a = b * a + c * a" ~count:10_000
      ~print:(Print.triple csmat_print csmat_print csmat_print)
      (Gen.triple csmat_gen csmat_gen csmat_gen) (fun (a, b, c) ->
        assume (a.nrows = b.ncols);
        assume (b.nrows = c.nrows);
        assume (b.ncols = c.ncols);
        (b +@ c) *@ a === (b *@ a) +@ (c *@ a)))
  |> to_alcotest

let () =
  run "Spurs.Ops.Props"
    [
      ( "vector operation properties",
        [ prop_dot_self; prop_plus_minus; prop_plus_plus; prop_to_from_dense ] );
      ( "matrix operation properties",
        [ prop_identity; prop_zero; prop_assoc; prop_distrib_left; prop_distrib_right ] );
    ]
