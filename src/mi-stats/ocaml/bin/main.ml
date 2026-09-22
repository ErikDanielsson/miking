type mi_rng

external create_rng : int -> mi_rng = "mi_rng_create"
external sample : mi_rng -> float = "mi_rng_sample"

let () =
  let rng = create_rng 1 in
  let s = sample rng in
  print_float s
