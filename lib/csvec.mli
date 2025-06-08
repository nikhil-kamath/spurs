(** {1 Sparse Vector Type}*)
type 'a t = { dim : int; indices : int Dynarray.t; data : 'a Dynarray.t }
(** A sparse vector, storing the indices of its non-zero data.

    It contains a sorted [indices] array and a corresponding [data] array.

    The dimension of the vector is stored separately as [dim], as the lengths of the
    internal arrays only store data on the non-zero indices (and thus there may be zeroes
    padded to the "end")*)

exception VectorException of string
(** Returned on invalid states*)

(** {1 Raw Creation Functions}*)

val try_new_csvec : int -> int array -> 'a array -> ('a t, string) result
(** [try_new_csvec dim indices data] creates a sparse vector.

    Returns an Error:
    - If [indices] and [data] lengths differ
    - If the indices are out of order
    - If the any index is greater than or equal to [dim] *)

val new_csvec : int -> int array -> 'a array -> 'a t
(** [new_csvec dim indices data] creates a sparse vector. It performs the same checks as
    {!try_new_csvec}, but raises an error if invalid *)

val new_from_unsorted : int -> int array -> 'a array -> ('a t, string) result
(** [new_from_unsorted dim indices data] creates a sparse vector, attempting to sort the
    index-data pairs. It then performs the same checks as {!try_new_csvec}, returning an
    Error if invalid. *)

(** {1 Common Creation Functions}*)

val of_dense : ?epsilon:float -> float array -> float t
(** [of_dense ~epsilon arr] creates a sparse vector from [arr], ignoring elements less
    than [epsilon] *)

val empty : int -> 'a t
(** [empty dim] creates an empty vector of dimension [dim] for building purposes. *)

(** {1 Iteration and Traversal}*)

val fold : ('acc -> int -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold f acc v] folds left through [v], calling [f acc index data] for each index-data
    pair in [v]*)

val iter : (int -> 'a -> unit) -> 'a t -> unit
(** [iter f v] calls [f index data] on each index-data pair in [v]*)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f v] returns a new vector of the same non-zero indices and data mapped by [f] *)

val map_inplace : ('a -> 'a) -> 'a t -> unit
(** [map_inplace f v] maps [f] onto [v.data] in place. *)

val count : ('a -> bool) -> 'a t -> int
(** [count pred v] counts the number of elements in [v] that make [pred x] return true. *)

val scale : float -> float t -> float t
(** [scale c v] returns a new vector with data scaled by [c] *)

(** {1 Indexing}*)

val get : 'a t -> int -> 'a option
(** [get v i] accesses the element at [v] in index [i]. Returns [None] if there is not a
    non-zero value at that index. Runs in logarithmic time with respect to the number of
    nonzeroes in [v] *)

val set : 'a t -> int -> 'a -> unit option
(** [set v i x] reassigns the element in [v] in index [i] to [x]. Returns [None] if there
    was not a non-zero value at that index. Runs in logarithmic time with respect to the
    number of nonzero values in [v].
    {b If there is not a non-zero value at index [i], this will not add it.}*)

val nnz : 'a t -> int
(** [nnz v] returns the number of non-zero elements in [v] *)

val nnz_index : 'a t -> int -> Common.Nnz_index.t option
(** [nnz_index v i] attempts to index [v] at index [i], returning [Some nnz] if the
    non-zero value exists. Logarithmic in the number of non-zero values in [v]*)

val get_nnz : 'a t -> Common.Nnz_index.t -> 'a
(** [get_nnz v n] attempts to index [v] at non-zero index [n], returning the value in
    constant time. Raises an error if the non-zero index is invalid. . *)

val set_nnz : 'a t -> Common.Nnz_index.t -> 'a -> unit
(** [set_nnz v n x] attempts to reassign [v] at non-zero index [n] to [x]. Raises an error
    if the non-zero index is invalid. *)

(** {1 Modfying, Building and Converting}*)

val append : 'a t -> int -> 'a -> unit
(** [append v ind x] appends [x] to the sparse vector. The append should preserve the
    structure.

    Raises an exception if:
    - [ind] is lower or equal to the last element of v.indices
    - [ind] is greater than or equal to v.dim *)

val is_empty : 'a t -> bool
(** [is_empty v] returns [true] if [v] contains no data. *)

val to_dense : float t -> float array
(** [to_dense v] converts this vector into a dense array. *)

val to_hashtbl : 'a t -> (int, 'a) Hashtbl.t
(** [to_hashtbl v] converts [v] into a index -> data hash table. *)

(** {1 Mathematical Functions}*)

val l1_norm : float t -> float
(** [l1_norm v] returns the L1 norm of [v] *)

val l2_norm : float t -> float
(** [l2_norm v] returns the L2 norm of [v] *)

val squared_l2_norm : float t -> float
(** [squared_l2_norm v] returns the squared L2 norm of [v] *)

val norm : float t -> float -> float
(** [norm v p] computes the p-order norm of [v].

    Special cases:
    - if [p = +infinity], returns the absolute maximum of [v]
    - if [p = -infinity], returns the absolute minimum of [v]
    - if [p = 0], returns the count of non-zero values in [v]
    - otherwise, returns the mathematical p'th norm. *)

val normalize : float t -> unit
(** [normalize v] divides [v] by its own L2-norm in place. The zero vector is left
    unchanged. *)

(** {1 Attributes, Derived, and Miscellaneous Functions}*)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** Default pretty-printer *)

val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
(** Default show *)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** Default equal *)

val get_dim : 'a t -> int
val get_indices : 'a t -> int Dynarray.t
val get_data : 'a t -> 'a Dynarray.t
val copy : 'a t -> 'a t
