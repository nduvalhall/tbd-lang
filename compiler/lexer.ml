type token =
  | TInteger of int
  | TChar of char
  | TBoolean of bool
  | TFloat of float
  | TString of string
  | TBinaryOperator of char
  | TBitwiseOperator of char
  | TOr
  | TAnd
  | TIdentifier of string
  | TAssignment
  | TPeriod
  | TComma
  | TSemicolon
  | TColon
  | TLCurly
  | TRCurly
  | TLBraces
  | TRBraces
  | TLBracket
  | TRBracket
  | TMatch
  | TOf
  | TUnit
  | TArrow
  | TType
  | TPipe
  | TCons
  | TQuote
  | TInvalid of string
  | TTypeVariable of string
  | TUnknown of char
  | TEOF

let token_to_string token =
  match token with
  | TInteger d -> Printf.sprintf "Integer(%d)" d
  | TChar c -> Printf.sprintf "Char(%c)" c
  | TBoolean b -> Printf.sprintf "Boolean(%b)" b
  | TFloat f -> Printf.sprintf "Float(%f)" f
  | TString s -> Printf.sprintf "String(%s)" s
  | TBinaryOperator s -> Printf.sprintf "BinaryOperator(%c)" s
  | TBitwiseOperator s -> Printf.sprintf "BitwiseOperator(%c)" s
  | TIdentifier s -> Printf.sprintf "Identifier(%s)" s
  | TAssignment -> "Assignment"
  | TPeriod -> "Period"
  | TComma -> "Comma"
  | TOr -> "Or"
  | TAnd -> "And"
  | TSemicolon -> "Semicolon"
  | TColon -> "Colon"
  | TLCurly -> "LCurly"
  | TUnit -> "Unit"
  | TRCurly -> "RCurly"
  | TLBraces -> "LBraces"
  | TRBraces -> "RBraces"
  | TLBracket -> "LBracket"
  | TRBracket -> "RBracket"
  | TType -> "Type"
  | TMatch -> "Match"
  | TArrow -> "Arrow"
  | TPipe -> "Pipe"
  | TOf -> "Of"
  | TCons -> "Cons"
  | TTypeVariable s -> Printf.sprintf "TypeVariable(%s)" s
  | TQuote -> "Quote"
  | TInvalid s -> Printf.sprintf "Invalid(%s)" s
  | TUnknown c -> Printf.sprintf "Unknown(%c)" c
  | TEOF -> "EOF"


let rec collect_identifier text chars  =
  let final s =
    match s with 
    | "true" -> TBoolean(true), text
    | "false" -> TBoolean(false), text
    | "match" -> TMatch, text
    | "of" -> TOf, text
    | "type" -> TType, text
    | s ->
      match String.starts_with ~prefix:"'" s,  String.ends_with ~suffix:"'" s with
      | true, true | false,true | false, false -> TIdentifier(s), text
      | true, false -> TTypeVariable(s), text
  in

  match text with
  | [] -> final (String.of_seq (List.to_seq (List.rev chars)))
  | hd::tl -> match hd with
    | 'a'..'z' | 'A'..'Z' | '_' | '\'' | '0'..'9' -> collect_identifier tl (hd::chars)
    | _ -> final (String.of_seq (List.to_seq (List.rev chars)))

let is_digit c = c >= '0' && c <= '9'


let rec collect_number text chars =
  let rec collect_float text' chars' =
    match text' with
    | [] -> TFloat(Float.of_string (String.of_seq (List.to_seq (List.rev chars')))), []
    | hd::tl -> 
      (match hd with
      | '0'..'9' -> collect_float tl (hd::chars')
      | _ -> TFloat(Float.of_string (String.of_seq (List.to_seq (List.rev chars')))), text')
    in
  let rec collect_integer text' chars' =
    match text' with
    | [] -> TInteger(Int.of_float (Float.of_string (String.of_seq (List.to_seq (List.rev chars'))))), []
    | hd::tl -> 
      (match hd with
      | '0'..'9' -> collect_integer tl (hd::chars')
      | '.' -> collect_float tl (hd::chars')
      | _ -> TInteger(Int.of_float (Float.of_string (String.of_seq (List.to_seq (List.rev chars'))))), text')
  in collect_integer text chars


let rec collect_string text chars  =
  match text with
  | [] -> TString(String.of_seq (List.to_seq (List.rev chars))), text
  | hd::tl -> match hd with
    | '"' ->  TString(String.of_seq (List.to_seq (List.rev (hd::chars)))), tl
    | _ -> collect_string tl (hd::chars)


let rec collect_unit text =
  match text with
  | [] -> TLBraces, text
  | hd::tl -> match hd with
    | ')' ->  TUnit, tl
    | ' ' | '\n' | '\t' | '\r' -> collect_unit tl
    | _ -> TLBraces, text

let char_to_digit c = Char.code c - Char.code '0'

let peek text =
  match text with
  | [] -> None
  | hd::tl -> Some (hd, tl)


let peek2 text =
  match text with
  | [] -> None
  | hd1::tl1 -> match tl1 with
    | [] -> None
    | hd2::tl2 -> Some (hd1, hd2, tl1, tl2)

let rec tokenize text tokens =
  match text with
  | [] -> List.rev tokens
  | hd ::tl ->
    match hd with
    | ' ' -> tokenize tl tokens
    | '0'..'9' -> 
      let number, text = collect_number tl [hd] in
      tokenize text (number :: tokens)
    | '+' | '-' | '*' | '/' ->
      (match peek tl with
      | Some (c, tl') when c = '>'-> tokenize tl' (TArrow:: tokens)
      | Some (c, tl') when is_digit c ->
        let number, text = collect_number tl [hd] in
        tokenize text (number :: tokens)
      | _ -> tokenize tl (TBinaryOperator(hd)::tokens))
    | '=' -> tokenize tl (TAssignment :: tokens)
    | '"'  -> 
      let string, text = collect_string tl [hd] in
      tokenize text (string::tokens)
    | 'a'..'z' | 'A'..'Z' | '_' | '\'' ->
      let identifier, text = collect_identifier tl [hd] in
      tokenize text (identifier :: tokens)
    | '|' ->
      (match peek tl with
      | Some (c, tl') when c = '>'-> tokenize tl' (TPipe:: tokens)
      | Some (c, tl') when c = '|'-> tokenize tl' (TOr:: tokens)
      | _ -> tokenize tl (TBitwiseOperator(hd)::tokens))
    | '&' ->
      (match peek tl with
      | Some (c, tl') when c = '&'-> tokenize tl' (TAnd:: tokens)
      | _ -> tokenize tl (TBitwiseOperator(hd)::tokens))
    | ':' ->
      (match peek tl with
      | Some (c, tl') when c = ':' -> tokenize tl' (TCons::tokens)
      | _ -> tokenize tl (TColon::tokens))
    | ';' -> tokenize tl (TSemicolon :: tokens)
    | '.' -> tokenize tl (TPeriod :: tokens)
    | ',' -> tokenize tl (TComma::tokens)
    | '{' -> tokenize tl (TLCurly::tokens)
    | '}' -> tokenize tl (TRCurly::tokens)
    | '(' ->
      let unit, text = collect_unit tl in
      tokenize text (unit::tokens)
    | ')' -> tokenize tl (TRBraces::tokens)
    | '[' -> tokenize tl (TLBracket::tokens)
    | ']' -> tokenize tl (TRBracket::tokens)
    | '\n' | '\t' | '\r' -> tokenize tl tokens
    | _ -> tokenize tl (TUnknown(hd)::tokens)

let rec print_tokens tokens =
  match tokens with
  | [] -> ()
  | hd::tl -> 
    print_endline (token_to_string hd);
    print_tokens tl


let () =
  let text = "
  type Option 'add = Some of 'add | None;
  type Person = { name : string, age : int};
  type Collection = Tuple of int * int * int | List of int list;
  123
  -123
  'a'
  'b
  '0
  'name
  'ident'
  true
  false
  12.34
  -12.34
  \"hello world\"
  1 + 2
  1 - 2
  1 * 2
  1 / 2
  1 | 2
  1 & 2
  true || false
  true && false
  function arg1 arg2
  =
  module.function
  ,
  ;
  :
  {}
  (a)
  []
  match
  (  )
  ->
  type
  |>
  ::
  $
  " in
  let tokens = tokenize (List.of_seq (String.to_seq text)) [] in
  print_tokens tokens;

