exception OpException of string

val add_v : ?epsilon:float -> float Csvec.t -> float Csvec.t -> float Csvec.t
(** [add_v ~epsilon v1 v2] returns a new sparse vector, [v1 + v2].

    Sums less than [epsilon] are ignored. Raises [OpException] if the vector dimensions
    are incompatible.*)

val dot_v : float Csvec.t -> float Csvec.t -> float
(** [dot_v v1 v2] returns the dot product of [v1] and [v2].

    Raises [OpException] if the vector dimensions are incompatible.*)

val mult :
  ?epsilon:float ->
  ?storage:Csmat.compressed_storage ->
  float Csmat.t ->
  float Csmat.t ->
  float Csmat.t
(** [mult ~epsilon ~storage m1 m2] returns the sparse matrix product of [m1] and [m2].

    Ignores results less than [epsilon], returning the result with format [storage].
    Raises [OpException] if the matrix dimensions are incompatible. *)

val ( *@ ) :
  ?epsilon:float ->
  ?storage:Csmat.compressed_storage ->
  float Csmat.t ->
  float Csmat.t ->
  float Csmat.t
(** Infix operator for [mult] *)

val add :
  ?epsilon:float ->
  ?storage:Csmat.compressed_storage ->
  float Csmat.t ->
  float Csmat.t ->
  float Csmat.t
(** [add ~epsilon ~storage m1 m2] returns the sparse matrix sum of [m1] and [m2].

    Ignores results less than [epsilon], returning the result with format [storage].
    Raises [OpException] if the matrix dimensions are incompatible. *)

val ( +@ ) :
  ?epsilon:float ->
  ?storage:Csmat.compressed_storage ->
  float Csmat.t ->
  float Csmat.t ->
  float Csmat.t
(** Infix operator for [add] *)
