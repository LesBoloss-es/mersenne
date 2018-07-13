open Mersenne


module MTH = FInt63(Int63Hamt)
module MTM = FInt63(Int63Map)
module MTA = FInt63(Int63Array)


let rec randfoldnat fop dice init state = function
  | 0 -> init
  | n ->
    let state, r = dice state in
    randfoldnat fop dice (fop init r) state (n - 1)


let n = 1000000
let seed = [|0x123; 0x234; 0x345; 0x456|]
let bound = 25
let fbound = Pervasives.float bound


(*** OCaml's random library / integers ***)
let () =
  Random.full_init seed ;
  let randsum = randfoldnat ( + ) (fun () -> (), Random.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum () n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " OCaml / integers : avg = %f ; exec time = %f@." average dt

(*** MTH / integers ***)
let () =
  let state = MTH.full_init seed in
  let randsum = randfoldnat ( + ) (MTH.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum state n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Hamt  / integers : avg = %f ; exec time = %f@." average dt

(*** MTM / integers ***)
let () =
  let state = MTM.full_init seed in
  let randsum = randfoldnat ( + ) (MTM.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum state n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Map   / integers : avg = %f ; exec time = %f@." average dt

(*** MTA / integers ***)
let () =
  let state = MTA.full_init seed in
  let randsum = randfoldnat ( + ) (MTA.int bound) 0 in
  let t0 = Sys.time () in
  let average = (randsum state n |> float_of_int) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Array / integers : avg = %f ; exec time = %f@." average dt

(*** OCaml's random library / floats ***)
let () =
  Random.full_init seed ;
  let randsum = randfoldnat ( +. ) (fun () -> (), Random.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum () n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " OCaml /  floats  : avg = %f ; exec time = %f@." average dt

(*** MTH / floats ***)
let () =
  let state = MTH.full_init seed in
  let randsum = randfoldnat ( +. ) (MTH.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum state n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Hamt  /  floats  : avg = %f ; exec time = %f@." average dt

(*** MTM / floats ***)
let () =
  let state = MTM.full_init seed in
  let randsum = randfoldnat ( +. ) (MTM.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum state n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Map   /  floats  : avg = %f ; exec time = %f@." average dt

(*** MTA / floats ***)
let () =
  let state = MTA.full_init seed in
  let randsum = randfoldnat ( +. ) (MTA.float fbound) 0. in
  let t0 = Sys.time () in
  let average = (randsum state n) /. (float_of_int n) in
  let dt = Sys.time () -. t0 in
  Format.printf " Array /  floats  : avg = %f ; exec time = %f@." average dt
