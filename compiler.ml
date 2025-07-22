module Token = struct
  type t =
    | Ident of string
    | Equal
    | Number of int
    | Space
    | Unknown of string

  let make c =
    match c with
    | 'a'..'z' -> Ident(String.make 1 c)
    | 'A'..'Z' -> Ident(String.make 1 c)
    | '0'..'9' -> Number(int_of_string (String.make 1 c))
    | '=' -> Equal
    | ' ' -> Space
    | _ -> Unknown(String.make 1 c)
end

module Ast = struct
  type expr =
    | Val of string
    | IntLit of int
end



let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let get_chars s =
  String.to_seq s
  |> List.of_seq
  |> List.filter (fun c -> c <> '\n' && c <> '\t' && c <> '\r' )



let main =
  let raw = read_file "compile1.tbd" in
  let chars = get_chars raw in
  List.map (fun c -> Token.make c) chars
