module type STATE = sig
  type raw
  type t

  val init : int -> t
  val full_init : int array -> t

  val bits : t -> t * raw
  val float1 : t -> t * float
end

module type MT = sig
  type state

  val init : int -> state
  val full_init : int array -> state

  val int : int -> state -> state * int
  val int32 : int32 -> state -> state * int32
  val int64 : int64 -> state ->  state * int64

  val bool : state -> state * bool
  val float : float -> state -> state * float
end
