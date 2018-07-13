type raw = int
type t = {array : int array ; mutable cursor : int}


let () =
  (* Sanity check *)
  if Sys.int_size < 32 then
    failwith "You cannot use this module on this architecture, int size must be ≥ 32 bits"


let n = 624
let m = 397


let twist =
  let matrix_a = 0x9908b0df in
  let umask = 0x80000000 in (* most significant w-r bits *)
  let lmask = 0x7fffffff in (* least significant r bits *)
  let mixbits u v = (u land umask) lor (v land lmask) in
  fun u v ->
    ((mixbits u v) lsr 1) lxor
    (if (v land 1) <> 0 then matrix_a else 0)


let incr_cursor state = { state with cursor = state.cursor + 1 }


let next_state_inplace arr =
  for i = 0 to n - m - 1 do
    arr.(i) <- arr.(i + m) lxor (twist arr.(i) arr.(i + 1))
  done ;
  for i = n - m to n - 2 do
    arr.(i) <- arr.(i + m - n) lxor (twist arr.(i) arr.(i + 1))
  done ;
  arr.(n - 1) <- arr.(m - 1) lxor (twist arr.(n - 1) arr.(0))
let next_state state =
  let array = Array.copy state.array in
  next_state_inplace array ;
  { array ; cursor = 0 }



let init seed =
  let array = Array.make n 0 in
  array.(0) <- seed land 0xFFFFFFFF ;
  for i = 1 to n - 1 do
    let prev = array.(i - 1) in
    array.(i) <- (1812433253 * (prev lxor (prev lsr 30)) + i) land 0xFFFFFFFF
  done ;
  next_state_inplace array ;
  { array ; cursor = 0 }


let full_init seeds =
  let state = init 19650218 in
  let i = ref 1 in
  let j = ref 0 in
  for _ = 1 to (max (Array.length seeds) n) do
    let u = state.array.(!i) in
    let v = state.array.(!i - 1) in
    let key = seeds.(!j) in
    let w = (u lxor ((v lxor (v lsr 30)) * 1664525)) + key + !j in
    let w = w land 0xFFFFFFFF in
    state.array.(!i) <- w ;
    incr j ;
    incr i ;
    if !i >= n then (state.array.(0) <- state.array.(n - 1) ; i := 1) ;
    if !j >= Array.length seeds then (j :=  0) ;
  done ;
  for _ = 1 to n - 1 do
    let u = state.array.(!i) in
    let v = state.array.(!i - 1) in
    let w = (u lxor ((v lxor (v lsr 30)) * 1566083941)) - !i  in
    let w = w land 0xFFFFFFFF in
    state.array.(!i) <- w ;
    incr i ;
    if !i >= n then (state.array.(0) <- state.array.(n - 1) ; i := 1) ;
  done ;
  state.array.(0) <- 0x80000000 ;
  next_state_inplace state.array ;
  state


let bits state =
  let state = if state.cursor < n then state else next_state state in
  let y = state.array.(state.cursor) in
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
