# Primitives

```
import core.io


enum Collection =
    | Scalar of int
    | Vector of int list
    | Matrix of int list list

enum Box = Int of int | Float of float

record Person = { name: string, age: int }

fun make_person name age =
    Person { name = name, age = age }

fun use_primitives () =
    let valid = true in   # bool
    let invalid = false in # bool

    let x = 42 in  # int
    let y = 42.0 in # float

    let s = "Hello world!" # string
    let name = "John" in # string
    let greet = "Hello {name}" # strings can contain values by default
    let nogreet = r"Hello {name}" # raw strings disable formatting

    let good = Ok "good" # string result
    let bad = Error "bad" # string result

    let zero = None # option
    let one = Some 1 # int option

    let l = [1, 2, 3, 4, 5] # int list
    let arr = {1, 2, 3, 4, 5} # int array

    let vec = Collection.Vector [1, 2, 3] # Collection
    let mat = Collection.Matrix [[1, 1, 1], [2, 2, 2], [3, 3, 3]] # Collection

    let john = Person { name = "John", age = 30 } # record


    let box1 = Box.Int 42 in
    let box2 = Box.Float 42.0 in

    let result =
        match box1 with
        | Box.Int i -> i + 27
        | Box.Float f -> f +. 27.0
    in

    fun closure a = (fun a -> a * 2) in
    ()


let x = 12 # error: no top level values

fun greet p =
    io.println "Hello {p.name}"

fun greet_all ps =
    fun rec aux ps =
        match ps with
        | [] -> ()
        | hd :: tl ->
            greet hd then
            aux tl
    in aux ps


""" Entrypoint """
fun main () =
    io.println "Starting program" then
    let ps = [make_person "John" 30, make_person "Alice" 30] in
    greet_all ps then
    io.println "Finished program"
```
