(* while.lex *)
structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token
type lexarg = string
type arg = lexarg

val lin = ref 1;
val col = ref 0;
val eolpos = ref 0;
val badCh : string * string * int * int -> unit = fn
    (fileName,bad,line,col) =>
    TextIO.output(TextIO.stdOut,fileName^"["
            ^Int.toString line^"."^Int.toString col
            ^"] Invalid character \""^bad^"\"\n");
val eof = fn fileName => T.EOF (!lin,!col);
fun negate x = x * ~1;

(* Keyword management *)
structure KeyWord :
sig val find : string ->
                (int * int -> (svalue,int) token) option
end =
struct
    val TableSize = 422 (* 211 *)
    val HashFactor = 5
    val hash = fn
        s => List.foldr (fn (c,v) =>
        (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
    val HashTable = Array.array(TableSize,nil) :
                    (string * (int * int -> (svalue,int) token))
                    list Array.array
    val add = fn
        (s,v) => let val i = hash s
                    in Array.update(HashTable,i,(s,v)
                        :: (Array.sub(HashTable, i)))
                end
    val find = fn
        s => let val i = hash s
                fun f ((key,v)::r) = if s=key then SOME v
                                    else f r
                    | f nil = NONE
            in f (Array.sub(HashTable, i))
            end
    val _ = (List.app add [
        ("program",         T.PROGRAM),
        ("var",             T.VAR),
        ("int",             T.INTEGER),
        ("bool",            T.BOOLEAN),
        ("read",            T.READS),
        ("write",           T.WRITES),
        ("if",              T.IF),
        ("then",            T.THEN),
        ("else",            T.ELSE),
        ("endif",           T.ENDIF),
        ("while",           T.WHIL),
        ("do",              T.DO),
        ("endwh",           T.ENDWHILE),
        ("tt",              T.LTT),
        ("ff",              T.LFF)
        ])
end;
open KeyWord;

%%
%full
%header (functor WHILELexFun(structure Tokens: WHILE_TOKENS));
%arg (fileName:string);
%s WILE;
alpha           = [A-Za-z];
digit           = [0-9];
alphanum        = [A-Za-z0-9];
pos             = "+";
neg             = "~";
ws              = [\ \t];
eol             = (\n);

%%
<INITIAL>{ws}* => (lin:=1; eolpos:=0;
                    YYBEGIN WILE; continue ());
<WILE>{ws}* => (continue ());
<WILE>{eol} => (lin:=(!lin)+1;
            eolpos:=yypos+size yytext; continue ());
<WILE>{alpha}{alphanum}* => (case find yytext of
                    SOME v  => (col:=yypos-(!eolpos);
                                v(!lin,!col))
                | _         => (col:=yypos-(!eolpos);
                                T.IDE(yytext,!lin,!col)));
<WILE>"::"    => (col:=yypos-(!eolpos); T.DCOLON(!lin,!col));
<WILE>":"     => (col:=yypos-(!eolpos); T.COLON(!lin,!col));
<WILE>":="    => (col:=yypos-(!eolpos); T.SETS(!lin,!col));
<WILE>";"     => (col:=yypos-(!eolpos); T.EOS(!lin,!col));
<WILE>","     => (col:=yypos-(!eolpos); T.COMMA(!lin,!col));
<WILE>"!"     => (col:=yypos-(!eolpos); T.LNOT(!lin,!col));
<WILE>"<"     => (col:=yypos-(!eolpos); T.LTHAN(!lin,!col));
<WILE>"<="    => (col:=yypos-(!eolpos); T.LEQUAL(!lin,!col));
<WILE>"="     => (col:=yypos-(!eolpos); T.EQUALS(!lin,!col));
<WILE>">"     => (col:=yypos-(!eolpos); T.GTHAN(!lin,!col));
<WILE>">="    => (col:=yypos-(!eolpos); T.GEQUAL(!lin,!col));
<WILE>"<>"    => (col:=yypos-(!eolpos); T.NEQUAL(!lin,!col));
<WILE>"~"     => (col:=yypos-(!eolpos); T.UNMINUS(!lin,!col));
<WILE>"+"     => (col:=yypos-(!eolpos); T.ADD(!lin,!col));
<WILE>"-"     => (col:=yypos-(!eolpos); T.SUB(!lin,!col));
<WILE>"*"     => (col:=yypos-(!eolpos); T.MULT(!lin,!col));
<WILE>"/"     => (col:=yypos-(!eolpos); T.DIVISION(!lin,!col));
<WILE>"%"     => (col:=yypos-(!eolpos); T.MODULO(!lin,!col));
<WILE>"{"     => (col:=yypos-(!eolpos); T.LBRACE(!lin,!col));
<WILE>"}"     => (col:=yypos-(!eolpos); T.RBRACE(!lin,!col));
<WILE>"("     => (col:=yypos-(!eolpos); T.LPAREN(!lin,!col));
<WILE>")"     => (col:=yypos-(!eolpos); T.RPAREN(!lin,!col));
<WILE>"||"    => (col:=yypos-(!eolpos); T.LOR(!lin,!col));
<WILE>"&&"    => (col:=yypos-(!eolpos); T.LAND(!lin,!col));
<WILE>{pos}{digit}+    => (col:=yypos-(!eolpos); T.NUM((List.foldl (fn(a, r)=>Char.ord(a)-Char.ord(#"0") + 10*r) 0 (String.explode (String.extract(yytext, 1, NONE)))),!lin,!col));
<WILE>{neg}{digit}+    => (col:=yypos-(!eolpos); T.NUM(negate (List.foldl (fn(a, r)=>Char.ord(a)-Char.ord(#"0") + 10*r) 0 (String.explode (String.extract(yytext, 1, NONE)))),!lin,!col));
<WILE>.       => (col:=yypos-(!eolpos);
                badCh (fileName,yytext,!lin,!col);
                T.ILLCH(!lin,!col));