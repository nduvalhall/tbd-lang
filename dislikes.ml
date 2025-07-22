(* Variable *)
(* val: int *)
let a = 10
let a = 20

(*
a = 10
x = 12 * a
*)

(* Mutable variable *)
(* int *)
let x = ref 10 !x = 20
(* Does not exist *)

(* Function *)
let add a b = a ^ b

(*
add a b = a + b
*)

(* Record *)
type person1 = { name : string; age : int }
type person2 = { name : string; height : int }

(*
type Person1 = { name: string; age: int }
*)

(* Record inference *)
let greet p = "Hello " ^ p.name
(*
greet p = "Hello " ^ p.name
*)

let () = greet (person2 { name = "Miles"; age = 29 })
let () = greet { name = "Miles"; height = 178 }

(* Enum *)
type direction = Up | Right | Down | Left
type lift = Up | Down

let get_dir a = if a > 0 then Up else Down

let dir =
  let a = 12 in
  let d = get_dir a in
  match d with
  | Up -> Down
  | Left -> ( match d with Down -> Up | Right -> Left)

module Direction = struct
  type t = Up | Down | Left | Right

  let get_dir a = if a > 0 then Up else Down
end

module Lift = struct
  type t = Up | Down

  let get_dir a = if a > 0 then Up else Down
end

let (d : Direction.t) = Direction.get_dir 12
let (d : Lift.t) = Lift.get_dir 12

let () =
  print_string "Hiii";
  print_string "Hellooo";
  print_string "whtttttt"
