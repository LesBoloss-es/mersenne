open Mersenne


module MT = FInt63(Int63Hamt)


let rec randfoldnat fop dice init state = function
  | 0 -> init
  | n ->
    let state, r = dice state in
    randfoldnat fop dice (fop init r) state (n - 1)


let n = 1000000
let bound = 25
let fbound = Pervasives.float bound


(*** OCaml's random library / integers ***)
let () =
  Random.full_init [|0x123; 0x234; 0x345; 0x456|] ;
  let randsum = randfoldnat ( + ) (fun () -> (), Random.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum () n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf "OCaml / integers : avg = %f ; exec time = %f@." average dt

(*** MT / integers ***)
let () =
  let state = MT.full_init [0x123; 0x234; 0x345; 0x456] in
  let randsum = randfoldnat ( + ) (MT.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum state n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf "  MT  / integers : avg = %f ; exec time = %f@." average dt

(*** OCaml's random library / floats ***)
let () =
  Random.full_init [|0x123; 0x234; 0x345; 0x456|] ;
  let randsum = randfoldnat ( +. ) (fun () -> (), Random.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum () n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf "OCaml /  floats  : avg = %f ; exec time = %f@." average dt

(*** MT / floats ***)
let () =
  let state = MT.full_init [0x123; 0x234; 0x345; 0x456] in
  let randsum = randfoldnat ( +. ) (MT.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum state n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf "  MT  /  floats  : avg = %f ; exec time = %f@." average dt
