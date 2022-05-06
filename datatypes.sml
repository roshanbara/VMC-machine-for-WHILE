(* datatypes.sml *)
signature DATATYPES =
sig datatype AST        =   AST of PROG
    and     PROG        =   PROG of EXP*BLK
    and     BLK         =   BLK of EXP*EXP
    and     EXP         =   EMPTY
                        |   TT
                        |   FF
                        |   SEQ of EXP*EXP
                        |   DEC of EXP*dtype
                        |   CMD of EXP
                        |   SET of EXP*EXP
                        |   READ of EXP
                        |   WRITE of EXP
                        |   ITE of EXP*EXP*EXP
                        |   WH of EXP*EXP
                        |   ADDOP of EXP*EXP
                        |   MULTOP of EXP*EXP
                        |   ID of string
                        |   VAL of int
                        |   NEG of EXP
                        |   LT of EXP*EXP
                        |   LEQ of EXP*EXP
                        |   EQ of EXP*EXP
                        |   GT of EXP*EXP
                        |   GEQ of EXP*EXP
                        |   NEQ of EXP*EXP
                        |   PLUS of EXP*EXP
                        |   MINUS of EXP*EXP
                        |   MOD of EXP*EXP
                        |   TIMES of EXP*EXP
                        |   DIV of EXP*EXP
                        |   NOT of EXP
                        |   AND of EXP*EXP
                        |   OR of EXP*EXP
    and     dtype       =   INT
                        |   BOOL
end;

structure DataTypes : DATATYPES =
struct
    datatype AST        =   AST of PROG
    and     PROG        =   PROG of EXP*BLK
    and     BLK         =   BLK of EXP*EXP
    and     EXP         =   EMPTY
                        |   TT
                        |   FF
                        |   SEQ of EXP*EXP
                        |   DEC of EXP*dtype
                        |   CMD of EXP
                        |   SET of EXP*EXP
                        |   READ of EXP
                        |   WRITE of EXP
                        |   ITE of EXP*EXP*EXP
                        |   WH of EXP*EXP
                        |   ADDOP of EXP*EXP
                        |   MULTOP of EXP*EXP
                        |   ID of string
                        |   VAL of int
                        |   NEG of EXP
                        |   LT of EXP*EXP
                        |   LEQ of EXP*EXP
                        |   EQ of EXP*EXP
                        |   GT of EXP*EXP
                        |   GEQ of EXP*EXP
                        |   NEQ of EXP*EXP
                        |   PLUS of EXP*EXP
                        |   MINUS of EXP*EXP
                        |   MOD of EXP*EXP
                        |   TIMES of EXP*EXP
                        |   DIV of EXP*EXP
                        |   NOT of EXP
                        |   AND of EXP*EXP
                        |   OR of EXP*EXP
    and     dtype       =   INT
                        |   BOOL
end;
