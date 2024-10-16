type t = string [@@deriving eq, ord, show]

let fresh =
  let count = ref 0 in
  fun () ->
    let id = "id_" ^ string_of_int !count in
    incr count; id

let to_string id = id
