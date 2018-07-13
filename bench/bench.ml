open Mersenne


module MT = FInt63(Int63Hamt)


let rec randfoldnat fop dice init state = function
  | 0 -> init
  | n ->
    let state, r = dice state in
    randfoldnat fop dice (fop init r) state (n - 1)


let randsum bound = randfoldnat ( + ) (MT.int bound) 0
let randsumf bound =
  let fop (acc, mini, maxi) r =
    (acc +. r, min mini r, max maxi r)
  in
  randfoldnat fop (MT.float bound) (0., max_float, -1.)


let () =
  let n = 10000 in
  let bound = 10 in
  let state = MT.full_init [0x123; 0x234; 0x345; 0x456] in

  let average =
    let sum = randsum bound state n |> float_of_int in
    sum /. (float_of_int n)
  in
  Format.printf "Average of %d random integers in [[0;%d[[: %f@." n bound average ;

  let bound = Pervasives.float bound in
  let average, mini, maxi =
    let (sum, mini, maxi) = randsumf bound state n in
    (sum /. (Pervasives.float n)), mini, maxi
  in
  Format.printf "Average of %d random integers in [0;%f[: %f@." n bound average ;
  Format.printf "Min and max values are (%f, %f)@." mini maxi
