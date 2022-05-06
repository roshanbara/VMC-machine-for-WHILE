(* while.yacc *)
open DataTypes
%%
%name WHILE
%term PROGRAM | VAR | INTEGER | BOOLEAN | READS | WRITES | IF | THEN | ELSE | WHIL | DO
    | ENDWHILE | LTT | LFF | DCOLON | SETS | EOS | COMMA | LNOT | LTHAN | LEQUAL | EQUALS | GTHAN
    | GEQUAL | NEQUAL | UNMINUS | ADD | SUB | MULT | DIVISION | MODULO | LBRACE | RBRACE | LPAREN | RPAREN | LOR | LAND
    | NUM of int | ILLCH | IDE of string | COLON | EOF | ENDIF

%nonterm Program of AST | Block of BLK | DeclarationSeq of EXP | Declaration of EXP
    | Type of dtype | VariableList of EXP | CommandSeq of EXP | CommandSeqn of EXP | Command of EXP
    | Expression of EXP | IntExpression of EXP | IntTerm of EXP | IntFactor of EXP | BoolExpression of EXP
    | BoolTerm of EXP | BoolFactor of EXP | Comparison of EXP | Variable of EXP | Identifier of EXP | Numeral of EXP
%pos int
%eop EOF
%noshift EOF
%nodefault
%verbose
%keyword PROGRAM VAR INTEGER BOOLEAN READS WRITES IF THEN ELSE ENDIF WHIL DO ENDWHILE LTT LFF
%arg (fileName) : string


%%
Program     : PROGRAM Identifier DCOLON Block                 (AST(PROG(Identifier, Block)))
Block       : DeclarationSeq CommandSeq                       (BLK(DeclarationSeq, CommandSeq))
DeclarationSeq  : Declaration DeclarationSeq                  (SEQ(Declaration, DeclarationSeq))
                | Declaration                                 (Declaration)
Declaration : VAR VariableList COLON Type EOS                 (DEC(VariableList, Type))
Type        : INTEGER                                         (INT)
            | BOOLEAN                                         (BOOL)
VariableList: Variable COMMA VariableList                     (SEQ(Variable, VariableList))
            | Variable                                        (Variable)
CommandSeq  : LBRACE Command EOS CommandSeqn RBRACE           (SEQ(CMD(Command), CommandSeqn))
            | LBRACE RBRACE                                   (EMPTY)
CommandSeqn : Command EOS CommandSeqn                         (SEQ(CMD(Command), CommandSeqn))
            |                                                 (EMPTY)
Command     : Variable SETS Expression                        (SET(Variable, Expression))
            | READS Variable                                  (READ(Variable))
            | WRITES IntExpression                            (WRITE(IntExpression))
            | IF BoolExpression THEN CommandSeq ELSE CommandSeq ENDIF       (ITE(BoolExpression, CommandSeq1, CommandSeq2))
            | IF Variable THEN CommandSeq ELSE CommandSeq ENDIF             (ITE(Variable, CommandSeq1, CommandSeq2))
            | WHIL BoolExpression DO CommandSeq ENDWHILE      (WH(BoolExpression, CommandSeq))
            | WHIL Variable DO CommandSeq ENDWHILE            (WH(Variable, CommandSeq))
Expression  : IntExpression                                   (IntExpression)
            | BoolExpression                                  (BoolExpression)
IntExpression : IntExpression ADD IntTerm                     (PLUS(IntExpression,IntTerm))
              | IntExpression SUB IntTerm                     (MINUS(IntExpression,IntTerm))
              | IntTerm                                       (IntTerm)
IntTerm     : IntTerm MULT IntFactor                          (TIMES(IntTerm, IntFactor))
            | IntTerm DIVISION IntFactor                      (DIV(IntTerm, IntFactor))
            | IntTerm MODULO IntFactor                        (MOD(IntTerm, IntFactor))
            | IntFactor                                       (IntFactor)
IntFactor   : Numeral                                         (Numeral)
            | Variable                                        (Variable)
            | LPAREN IntExpression RPAREN                     (IntExpression)
            | UNMINUS  IntFactor                              (NEG(IntFactor))
BoolExpression  : BoolExpression LOR BoolTerm                 (OR(BoolExpression, BoolTerm))
                | BoolTerm                                    (BoolTerm)
BoolTerm    : BoolTerm LAND Variable                          (AND(BoolTerm, Variable))
            | BoolTerm LAND BoolFactor                        (AND(BoolTerm, BoolFactor))
            | Variable LAND BoolFactor                        (AND(Variable, BoolFactor))
            | Variable LAND Variable                          (AND(Variable1, Variable2))
            | BoolFactor                                      (BoolFactor)
BoolFactor  : LTT                                             (TT)
            | LFF                                             (FF)
            | Comparison                                      (Comparison)
            | LPAREN BoolExpression RPAREN                    (BoolExpression)
            | LNOT Variable                                   (NOT(Variable))
            | LNOT BoolFactor                                 (NOT(BoolFactor))
Comparison  : IntExpression LTHAN IntExpression               (LT(IntExpression1, IntExpression2))
            | IntExpression LEQUAL IntExpression              (LEQ(IntExpression1, IntExpression2))
            | IntExpression EQUALS IntExpression              (EQ(IntExpression1, IntExpression2))
            | IntExpression GTHAN IntExpression               (GT(IntExpression1, IntExpression2))
            | IntExpression GEQUAL IntExpression              (GEQ(IntExpression1, IntExpression2))
            | IntExpression NEQUAL IntExpression              (NEQ(IntExpression1, IntExpression2))
Variable    : Identifier                                      (Identifier)
Identifier  : IDE                                             (ID(IDE))
Numeral     : NUM                                             (VAL(NUM))
