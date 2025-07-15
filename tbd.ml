let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  let _ = close_in ic in
  content

type token =
  | Import of string

let make_tokens words =
  let rec aux words tokens =
    match words with
    | [] -> tokens
    | [hd] -> tokens
    | hd::tl -> (match hd with
      | "import" -> (match tl with
        | [] -> failwith "import is missing module"
        | [f] -> aux [] (Import(f) :: tokens)
        | f::tl -> aux tl (Import(f) :: tokens))
      | _ -> aux tl tokens)
  in aux words []

let make_words contents =
  let lines = String.split_on_char '\n' contents in
  let rec aux lines words =
    match lines with
    | [] -> words
    | [x] -> aux [] ((String.split_on_char ' ' x)::words)
    | hd::tl -> aux tl ((String.split_on_char ' ' hd)::words)
  in List.rev (List.fold_left (fun acc s -> match s with "" -> acc | _ -> s::acc) [] (List.flatten (List.rev (aux lines []))))


let entry_point = "main.tbd"
let contents = read_file entry_point
let words = make_words contents
let tokens = make_tokens words
