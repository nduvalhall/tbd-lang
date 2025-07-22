let factorial n =
  let rec aux n' acc =
    match n' with
    | 0 -> acc
    | k -> aux (k - 1) (k * acc)
  in aux n 1



let fibonacci n =
  let rec aux i prev next =
    print_endline (string_of_int next);
    match i = n with
    | true -> next
    | false -> aux (i + 1) (next) (prev + next)
  in aux 0 0 1

let () =
  let x = fibonacci 10 in
  print_endline (string_of_int x)



type person = {name: string; height: int}
type person2 = {name:string; age:int}

let p1 = {name="james"; height=160}
let p2 = {name="alice"; age=25}

let greet p x = print_string p.name; print_string x

let gp1 = greet p1
let gp2 = greet p2
