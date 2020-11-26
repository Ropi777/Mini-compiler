(*First step lexer: flat strcture and integers only*)

type token =
  (*types*)
  (*unary binary hex*)
  |INTEGER of int
  (*operators*)
  |PLUS 
  |MINUS
  |STAR
  |SLASH
  |PERCENT
  |EQUAL
  |PLUS_EQUAL
  |MINUS_EQUAL
  |STAR_EQUAL
  |SLASH_EQUAL
  |PERCENT_EQUAL

  |IDENTIFIER of string
  |INT

  |SEMICOLON
  |RETURN
  |MAIN

  |LBRACE
  |RBRACE
  |LPAREN
  |RPAREN

(*transform string representing program
as input into token list*)
let lexer text =
  
  (*data*)
  let len = String.length text in

  (*helper functions*)
  let eat_integer starting_pos =
    let first_nr = (String.get text starting_pos) in
    let rec eat ending_pos  = 
      if ending_pos  = len then ending_pos else
      match String.get text ending_pos with
      |'x'|'b' -> 
        if  first_nr = '0' &&  ending_pos = 1  then eat (ending_pos+1)
        else ending_pos
      |'0'..'9' -> eat (ending_pos+1)
      |_ -> ending_pos 
      in let ending_pos = eat starting_pos in
      (ending_pos + 1, 
      Stdlib.int_of_string 
        (String.sub text starting_pos (ending_pos - starting_pos)))
  in

  let make_identifier start_pos =
    let rec ident pos = 
      if pos >= len then pos else
      match String.get text pos with
      |'a'..'z'|'A'..'Z'|'_'|'-' -> ident (pos + 1)
      |_ -> pos 
    in let end_pos = ident (start_pos + 1) in
    (end_pos + 1, String.sub text start_pos (end_pos - start_pos))
  in    

  let next pos = 
    String.get text (pos+1)
   in

  (*main function, travel text char by char and construct token list*)
  let rec analyse cur_pos token_list = 
    if cur_pos >= len then (token_list) else
    match String.get text cur_pos with 
    |' '|'\t'|'\r'|'\n' -> analyse (cur_pos + 1) token_list
    |';' -> analyse (cur_pos + 1) (SEMICOLON::token_list)
    |'+' -> if next cur_pos = '=' && cur_pos + 1 < len
              then analyse (cur_pos + 2) (PLUS_EQUAL::token_list)
              else analyse (cur_pos + 1) (PLUS::token_list)
    |'-' -> if next cur_pos = '=' && cur_pos + 1 < len
                then analyse (cur_pos + 2) (MINUS_EQUAL::token_list)
                else analyse (cur_pos + 1) (MINUS::token_list) 
    |'*' -> if next cur_pos = '=' && cur_pos + 1 < len   
                then analyse (cur_pos + 2) (STAR_EQUAL::token_list)
                else analyse (cur_pos + 1) (STAR::token_list)
    |'/' -> if next cur_pos = '=' && cur_pos + 1 < len
                then analyse (cur_pos + 2) (SLASH_EQUAL::token_list)
                 else analyse (cur_pos + 1) (SLASH::token_list)
    |'%' -> if next cur_pos = '='  && cur_pos + 1 < len
                then analyse (cur_pos + 2) (PERCENT_EQUAL::token_list)
                else analyse (cur_pos + 1) (PERCENT::token_list)
    |'(' -> analyse (cur_pos + 1) (LPAREN::token_list)
    |')' -> analyse (cur_pos + 1) (RPAREN::token_list)
    |'{' -> analyse (cur_pos + 1) (LBRACE::token_list)
    |'}' -> analyse (cur_pos + 1) (RBRACE::token_list)
    |'=' -> analyse (cur_pos + 1) (EQUAL::token_list)

    |'0'..'9' -> let (new_pos, number) = eat_integer  cur_pos in
            analyse new_pos (INTEGER(number)::token_list)
    |'a'..'z'|'A'..'Z' -> let  (next_pos,ident) = make_identifier cur_pos 
      in match ident with
      |"return" -> analyse (next_pos) (RETURN::token_list)
      |"main" -> analyse (next_pos) (MAIN::token_list)
      |"int" -> analyse (next_pos) (INT::token_list)
      |word -> analyse (next_pos) (IDENTIFIER(word)::token_list)
      |_ ->failwith ("lexival error") 
    | _ -> failwith ("Lexical error, unknown token") 
  
 in List.rev (analyse 0 [])

