(*
  xkcd random number generator
  https://www.xkcd.com/221/
*)

type state = unit
type raw = int

let init _ = ()
let full_init _ = ()

let bits () = (), 4

let int _ () = (), 4
let int32 _ () = (), Int32.of_int 4
let int64 _ () = (), Int64.of_int 4

let bool () = (), true
let float _ ()= (), 0.4
