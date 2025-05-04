module Dynarray = struct
  include Dynarray

  (* Add a generic pretty-printer for Dynarray.t *)
  let pp pp_elem fmt arr =
    let lst = to_list arr in
    if lst = [] then Format.fprintf fmt "@[<hov 2>[@]@]"
    else
      Format.fprintf fmt "@[<hov 2>[%a]@]"
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_elem)
        lst

  let ( .!() ) = get
  let ( .!()<- ) = set
end

module Nnz_index = struct
  (** Can be used to later access a non-zero element of a compressed matrix in constant
      time *)
  type t = NNZ of int [@@deriving show, eq]
end
