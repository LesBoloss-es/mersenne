module Xkcd = Mersenne.Xkcd


let () =
  let state = Xkcd.init 424242 in
  let _, n = Xkcd.bits state in
  assert (n = 4)
