module Int = struct
  type t = int
  let compare i j = i - j
end
module Imap = Map.Make(Int)

type raw = int
type t = {array : int Imap.t ; cursor : int}


let () =
  (* Sanity check *)
  if Sys.int_size < 32 then
    failwith "You cannot use this module on this architecture, int size must be ≥ 32 bits"


let n = 624
let m = 397


let set = Imap.add
let get = Imap.find


let twist =
  let matrix_a = 0x9908b0df in
  let umask = 0x80000000 in (* most significant w-r bits *)
  let lmask = 0x7fffffff in (* least significant r bits *)
  let mixbits u v = (u land umask) lor (v land lmask) in
  fun u v ->
    ((mixbits u v) lsr 1) lxor
    (if (v land 1) <> 0 then matrix_a else 0)


let incr_cursor state = { state with cursor = state.cursor + 1 }


let next_state =
  let rec next_state1 p array =
    if p < n - m then
      let newval = (get (p + m) array) lxor (twist (get p array) (get (p + 1) array)) in
      let array = set p newval array in
      next_state1 (p + 1) array
    else array
  in
  let rec next_state2 p array =
    if p < n - 1 then
      let newval = (get (p - n + m) array) lxor (twist (get p array) (get (p + 1) array)) in
      let array = set p newval array in
      next_state2 (p + 1) array
    else array
  in
  let finish array =
    let p = n - 1 in
    let newval = (get (p - n + m) array) lxor (twist (get p array) (get 0 array)) in
    set p newval array
  in
  fun state ->
    let array = state.array |> next_state1 0 |> next_state2 (n-m) |> finish in
    { array ; cursor = 0 }



let init =
  let rec init prev array = function
    | 0 -> array
    | k ->
      let j = n - k in
      let value = 1812433253 * (prev lxor (prev lsr 30)) + j in
      let value = value land 0xFFFFFFFF in
      let array = set j value array in
      init value array (k - 1)
  in
  fun seed ->
    let seed = seed land 0xFFFFFFFF in
    let array = Imap.singleton 0 seed in
    let array = init seed array (n - 1) in
    next_state { array ; cursor = -1 }


let uncons = function
  | x :: xs -> x, xs
  | [] -> assert false

let full_init seeds =
  let length = List.length seeds in
  let rec init1 i j k keys state =
    if k = 0 then i, state
    else
      let key, keys = uncons keys in
      let u = get i state in
      let v = get (i - 1) state in
      let state =
        let w = (u lxor ((v lxor (v lsr 30)) * 1664525)) + key + j in
        let w = w land 0xFFFFFFFF in
        set i w state
      in
      let i, state = if i + 1 >= n then 1, set 0 (get (n - 1) state) state else i + 1, state in
      let j, keys = if j + 1 >= length then 0, seeds else j + 1, keys in
      init1 i j (k - 1) keys state
  in
  let rec init2 k (i, state) =
    if k = 0 then state
    else
      let u = get i state in
      let v = get (i - 1) state in
      let state =
        let w = (u lxor ((v lxor (v lsr 30)) * 1566083941)) - i  in
        let w = w land 0xFFFFFFFF in
        set i w state
      in
      let i, state = if i + 1 >= n then 1, set 0 (get (n - 1) state) state else i + 1, state in
      init2 (k - 1) (i, state)
  in
  let k = max length n in
  let array = (init 19650218).array |> init1 1 0 k seeds |> init2 (n - 1) in
  next_state { array  ; cursor = -1 }





let bits state =
  let state = if state.cursor < n then state else next_state state in
  let y = get state.cursor state.array in
  let y = y lxor (y lsr 11) in
  let y = y lxor ((y lsl 7) land 0x9d2c5680) in
  let y = y lxor ((y lsl 15) land 0xefc60000) in
  let y = y lxor (y lsr 18) in
  incr_cursor state, y


let float1 state =
  let state, a = bits state in
  let state, b = bits state in
  let a = Pervasives.float (a land 0x07FFFFFF) in
  let b = Pervasives.float (b land 0x03FFFFFF) in
  let r = (a *. 67108864.0 +. b) *. (1.0 /. 9007199254740992.0) in
  state, r
