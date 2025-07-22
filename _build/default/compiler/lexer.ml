type token = IDENT of string | SPACE | INT of char | EOF | ELSE of char

let is_char c =
  match c with 'a' .. 'z' | 'A' .. 'Z' | '_' | '\'' -> true | _ -> false

let next_word chars =
  let rec aux chars word_chars =
    begin match chars with
    | [] -> (chars, word_chars)
    | c :: rest ->
        begin match is_char c with
        | true -> aux rest (c :: word_chars)
        | false -> (chars, word_chars)
        end
    end
  in
  let chars, word_chars = aux chars [] in
  (chars, String.of_seq (List.to_seq (List.rev word_chars)))

let tokenize chars =
  let rec aux chars tokens =
    begin match chars with
    | [] -> tokens
    | c :: rest ->
        begin match c with
        | 'a' .. 'z' | 'A' .. 'Z' | '\'' ->
            let chars, word = next_word chars in
            aux chars (IDENT word :: tokens)
        | ' ' -> aux rest (SPACE::tokens)
        | '0'..'9' -> aux rest (INT(c) ::tokens)
        | _ -> aux rest (ELSE c :: tokens)
        end
    end
  in
  List.rev (aux chars [])


let () =let t = tokenize [ 'a'; 'b'; 'c'; ' ' ] in
let code = "let num = 1" in
let chars = List.of_seq (String.to_seq code) in
let ts = tokenize chars in ()
