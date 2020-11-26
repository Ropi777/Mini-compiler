open ../bin/main.ml

assert(lexer "" = [EOF])

assert(lexer "3*2" = [INT 3; STAR; INT 21; EOF]) 

assert(lexer "5" = [INT 5; EOF])

assert(lexer "5*25+(2*10-4)" = [INT 5; STAR; INT 25; PLUS; LPAREN; INT 2; STAR; INT 10; MINUS; INT 4;
 RPAREN; EOF]
)

