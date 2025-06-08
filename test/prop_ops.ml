open Spurs
open Spurs.Ops
open Alcotest
open QCheck_alcotest
open Generators

let epsilon = 0.000001
let assume = QCheck2.assume

(* approx equal to *)
let ( === ) = Csmat.equal (fun l r -> abs_float (l -. r) < epsilon)

(** {b Test Configs} *)
let prop_csvec ?(count = 10_000) name p =
  QCheck2.Test.make ~name ~count ~print:csvec_print csvec_gen p |> to_alcotest

let prop_vec ?(count = 10_000) name p =
  QCheck2.(Test.make ~name ~count ~print:(Print.array Print.float) vec_gen p) |> to_alcotest

let prop_csmat ?(count = 10_000) name p =
  QCheck2.Test.make ~name ~count ~print:csmat_print csmat_gen p |> to_alcotest

let prop_csmat_2 ?(count = 10_000) name p =
  QCheck2.(
    Test.make ~name ~count
      ~print:(Print.pair csmat_print csmat_print)
      (Gen.pair csmat_gen csmat_gen) p
    |> to_alcotest)

let prop_csmat_3 ?(count = 10_000) name p =
  QCheck2.(
    Test.make ~name ~count
      ~print:(Print.triple csmat_print csmat_print csmat_print)
      (Gen.triple csmat_gen csmat_gen csmat_gen)
      p
    |> to_alcotest)

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

let prop_identity_right =
  prop_csmat "a = I * a" (fun m ->
      let i = Csmat.eye_csr m.nrows in
      m === mult ~storage:m.storage i m)

let prop_zero =
  prop_csmat "a * 0 = 0" (fun m ->
      let z = Csmat.zero (m.ncols, 10) |> Csmat.into_csc in
      m *@ z === Csmat.zero (m.nrows, 10))

let prop_zero_right =
  prop_csmat "0 * a = 0" (fun m ->
      let z = Csmat.zero (10, m.nrows) |> Csmat.into_csr in
      z *@ m === Csmat.zero (10, m.ncols))

let prop_assoc =
  prop_csmat_3 "(a * b) * c = a * (b * c)" (fun (a, b, c) ->
      assume (a.ncols = b.nrows);
      assume (b.ncols = c.nrows);
      a *@ b *@ c === a *@ (b *@ c))

let prop_distrib =
  prop_csmat_3 "a * (b + c) = a * b + a * c" (fun (a, b, c) ->
      assume (a.ncols = b.nrows);
      assume (b.nrows = c.nrows);
      assume (b.ncols = c.ncols);
      a *@ (b +@ c) === (a *@ b) +@ (a *@ c))

let prop_distrib_right =
  prop_csmat_3 "(b + c) * a = b * a + c * a" (fun (a, b, c) ->
      assume (a.nrows = b.ncols);
      assume (b.nrows = c.nrows);
      assume (b.ncols = c.ncols);
      (b +@ c) *@ a === (b *@ a) +@ (c *@ a))

let prop_add_commute =
  prop_csmat_2 "a + b = b + c" (fun (a, b) ->
      assume ((a.nrows, a.ncols) = (b.nrows, b.ncols));
      a +@ b === b +@ a)

let prop_additive_inverse =
  prop_csmat "a - a = 0" (fun m ->
      let inv = Csmat.scale (-1.) m in
      m +@ inv === Csmat.zero (m.nrows, m.ncols))

(* this test is very slow (i think bc of the to_other_storage calls) *)
let prop_transpose_distrib =
  prop_csmat_2 "(a * b) ^T = b^T * a^T" (fun (a, b) ->
      assume (a.ncols = b.nrows);
      Csmat.transpose (a *@ b) |> Csmat.into_csr === Csmat.transpose b *@ Csmat.transpose a)

let prop_transpose_distrib_add =
  prop_csmat_2 "(a + b) ^T = a^T + b^T" (fun (a, b) ->
      assume ((a.nrows, a.ncols) = (b.nrows, b.ncols));
      Csmat.transpose (a +@ b) |> Csmat.into_csr === Csmat.transpose a +@ Csmat.transpose b)

let () =
  run "Spurs.Ops.Props"
    [
      ( "vector operation properties",
        [ prop_dot_self; prop_plus_minus; prop_plus_plus; prop_to_from_dense ] );
      ( "matrix operation properties",
        [
          prop_identity;
          prop_identity_right;
          prop_zero;
          prop_zero_right;
          prop_assoc;
          prop_distrib;
          prop_distrib_right;
          prop_add_commute;
          prop_additive_inverse;
          prop_transpose_distrib;
          prop_transpose_distrib_add;
        ] );
    ]
