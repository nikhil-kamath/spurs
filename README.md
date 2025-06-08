# spurs

Rust's [sprs](https://docs.rs/sprs/latest/sprs/) crate is a sparse linear algebra library for Rust. This is a lightweight, natively implemented port of `sprs` to OCaml, which will (eventually) support all the same functionality.

The [documentation can be found here](https://nikhil-kamath.github.io/spurs/)!

## Installation

```
opam install spurs
```

## Quickstart Guide

wip, here's a snippet:

```ocaml
open Spurs
open Spurs.Csmat
open Spurs.Ops

let () =
  let a =
    [|
      [| 0.; 0.; 2.; 0.; 0. |];
      [| 0.; 1.; 2.; 0.; 0. |];
      [| 0.; 0.; 2.; 0.; 0. |];
      [| 0.; 0.; 0.; 4.; 0. |];
    |]
    |> csr_from_dense
  in
  let b =
    [|
      [| 0.; 0.; 2. |];
      [| 0.; 1.; 2. |];
      [| 0.; 0.; 2. |];
      [| 0.; 0.; 0. |];
      [| 0.; 0.; 0. |];
    |]
    |> csc_from_dense
  in
  let c = a *@ b in
  print_float_matrix c;
  print_matrix (to_dense c)
```

Prints:

```

{ Csmat.storage = Csmat.CSR; nrows = 4; ncols = 3; indptr = [0, 1, 3, 4, 4];
  indices = [2, 1, 2, 2]; data = [4, 1, 6, 4] }

[
    [0.00; 0.00; 4.00];
    [0.00; 1.00; 6.00];
    [0.00; 0.00; 4.00];
    [0.00; 0.00; 0.00];
]

```
