module type MT = sig
  type raw
  type state

  val init : int -> state
  val full_init : int list -> state

  val bits : state -> state * raw

  val int : state -> int -> state * int
  val int32 : state -> int32 -> state * int32
  val int64 : state -> int64 -> state * int64

  val bool : state -> state * bool
  val float : state -> float -> state * float
end
