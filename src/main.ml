(*First step: Integer calculator*)
(*operators +, -, *, / *)

type token =
  |INT of int
  |PLUS 
  |MINUS
  |STAR
  |SLASH
  |LPAREN
  |RPAREN
  |EOF
(*transform string representing program
as input into token list*)
let lexer text =
  
  (*data*)
  let len = String.length text in

  (*helper functions*)
  let eat_integer starting_pos =
    let rec eat endng =
     if endng = len then endng else 
     match (String.get text endng) with
     |'0'..'9' -> eat (endng +1)
     | _ -> endng
    in let end_pos = (eat starting_pos) in 
    (end_pos, 
      (Stdlib.int_of_string(String.sub text
                                       starting_pos 
                                       (end_pos  - starting_pos))))
    in

  let rec analyse cur_pos token_list = 
    if cur_pos >= len then (EOF::token_list) else
    match String.get text cur_pos with
   
    |' '|'\t'|'\r'|'\n' -> analyse (cur_pos + 1) token_list
    |'+' -> analyse (cur_pos + 1) (PLUS::token_list)
    |'-' -> analyse (cur_pos + 1) (MINUS::token_list) 
    |'*' -> analyse (cur_pos + 1) (STAR::token_list)
    |'/' -> analyse (cur_pos + 1) (SLASH::token_list)
    |'(' -> analyse (cur_pos + 1) (LPAREN::token_list)
    |')' -> analyse (cur_pos + 1) (RPAREN::token_list)
    |'0'..'9' -> let (new_pos, number) = eat_integer cur_pos in
            analyse new_pos (INT(number)::token_list)
    | _ -> failwith ("Lexical error, unknown token")
    
    in List.rev (analyse 0 [])

(*
let ast =
  Expr of INT * OP * INT
  INT of int
  

(*transform tokenlist into abstract syntax tree*)
let parse token_list left_subtree =
  match token_list with
  (*if there is parens find closing paren and solve for this *)
  (*find one of operators * / before LPAREN *)
  (*if you cant then search for + or - *)
  (*divide into subtrees*)
  |I
*)



