module Make(S : Interface.STATE with type raw := int) = struct
  type state = S.t


  let init = S.init
  let full_init = S.full_init


  let rec int bound state =
    let state, r1 = S.bits state in
    let state, r2 = S.bits state in
    let r = r1 lor ((r2 land 0x3FFFFFFF) lsl 32) in
    let v = r mod bound in
    if r - v > 0x3FFFFFFFFFFFFFFF - bound + 1
    then int bound state
    else state, v
  let int bound state =
    if bound <= 0
    then invalid_arg "FInt63.int"
    else int bound state

  let rec int32 bound state =
    let state, r = S.bits state in
    let r = Int32.of_int (r land 0x7FFFFFFF) in
    let v = Int32.rem r bound in
    if (Int32.sub r v) > Int32.add (Int32.sub Int32.max_int bound) 1l
    then int32 bound state
    else state, v
  let int32 bound state =
    if bound <= 0l
    then invalid_arg "FInt63.int"
    else int32 bound state

  let rec int64 bound state =
    let state, r1 = S.bits state in
    let state, r2 = S.bits state in
    let r1 = Int64.shift_left (Int64.of_int r1) 31 in
    let r2 = Int64.shift_right (Int64.of_int r2) 1 in
    let r = Int64.logor r1 r2 in
    let v = Int64.rem r bound in
    if (Int64.sub r v) > Int64.add (Int64.sub Int64.max_int bound) 1L
    then int64 bound state
    else state, v
  let int64 bound state =
    if bound <= 0L
    then invalid_arg "FInt63.int"
    else int64 bound state


  let bool state =
    let state, r = S.bits state in
    let b = (r land 0x1) = 1 in
    state, b

  let float bound state =
    let state, r = S.float1 state in
    state, r *. bound
end
