(* while_ast.sml *)
structure WHILE :
sig val parse : string -> DataTypes.AST
    val AST_to_list : DataTypes.AST * string list -> string list
    val DEC_extract : DataTypes.AST * (string * string * int) list * int ref -> (string * string * int) list
end =
struct
exception WHILEError;
fun parse (fileName) =
    let val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
            n => if TextIO.endOfStream inStream
                then ""
                else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
            (msg,line,col) =>
            print (fileName^"["^Int.toString line^":"
                    ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = WHILEParser.parse
                    (15,
                    (WHILEParser.makeLexer grab fileName),
                    printError,
                    fileName)
            handle WHILEParser.ParseError => raise WHILEError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in tree
    end

(* Converts AST to Postfix notation and puts in list *)
    fun AST_to_list (a, l) =
    let 

        fun AST_to_PostfixList (DataTypes.EMPTY, l) = l
            | AST_to_PostfixList (DataTypes.SEQ(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["SEQ"]
            | AST_to_PostfixList (DataTypes.CMD(x), l) = l @ AST_to_PostfixList(x, [])
            | AST_to_PostfixList (DataTypes.SET(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["SET"]
            | AST_to_PostfixList (DataTypes.READ(x), l) = l @ AST_to_PostfixList(x, []) @ ["READ"]
            | AST_to_PostfixList (DataTypes.WRITE(x), l) = l @ AST_to_PostfixList(x, []) @ ["WRITE"]
            | AST_to_PostfixList (DataTypes.ITE(x, y, z), l) = l @ AST_to_PostfixList(x, []) @ ["$1"] @ AST_to_PostfixList(y, []) @ ["$2"] @ AST_to_PostfixList(z, []) @ ["ITE"]
            | AST_to_PostfixList (DataTypes.WH(x, y), l) = l @ ["@1"] @ AST_to_PostfixList(x, []) @ ["@2"] @ AST_to_PostfixList(y, []) @ ["WH"]
            | AST_to_PostfixList (DataTypes.PLUS(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["PLUS"]
            | AST_to_PostfixList (DataTypes.MINUS(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["MINUS"]
            | AST_to_PostfixList (DataTypes.TIMES(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["TIMES"]
            | AST_to_PostfixList (DataTypes.DIV(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["DIV"]
            | AST_to_PostfixList (DataTypes.MOD(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["MOD"]
            | AST_to_PostfixList (DataTypes.NEG(x), l) = l @ AST_to_PostfixList(x, []) @ ["NEG"]
            | AST_to_PostfixList (DataTypes.OR(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["OR"]
            | AST_to_PostfixList (DataTypes.AND(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["AND"]
            | AST_to_PostfixList (DataTypes.TT, l) = l @ ["tt"]
            | AST_to_PostfixList (DataTypes.FF, l) = l @ ["ff"]
            | AST_to_PostfixList (DataTypes.NOT(x), l) = l @ AST_to_PostfixList(x, []) @ ["NOT"]
            | AST_to_PostfixList (DataTypes.LT(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["LT"]
            | AST_to_PostfixList (DataTypes.LEQ(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["LEQ"]
            | AST_to_PostfixList (DataTypes.EQ(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["EQ"]
            | AST_to_PostfixList (DataTypes.GT(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["GT"]
            | AST_to_PostfixList (DataTypes.GEQ(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["GEQ"]
            | AST_to_PostfixList (DataTypes.NEQ(x, y), l) = l @ AST_to_PostfixList(x, []) @ AST_to_PostfixList(y, []) @ ["NEQ"]
            | AST_to_PostfixList (DataTypes.ID(x), l) = l @ [x]
            | AST_to_PostfixList (DataTypes.VAL(x), l) = l @ [Int.toString(x)]
            | AST_to_PostfixList (_, l) = l;

        fun AST_CMD_extract (DataTypes.AST(DataTypes.PROG(_, DataTypes.BLK(_, x))), l) = AST_to_PostfixList(x, l);
            (* | AST_CMD_extract (_, l) = l; *)

        
        val final_list = AST_CMD_extract(a, []);

    in final_list
    end

    

(* Contructs Symbol Table in form of list of (Var ID, Type, Index in Memory) *)
    fun DEC_extract (DataTypes.AST(DataTypes.PROG(_, DataTypes.BLK(abc, _))), l, idx) = 
    let 
        fun inc x = (x := !x + 1; !x);
        fun dttostr (DataTypes.INT) = "INT"
            | dttostr (DataTypes.BOOL) = "BOOL";


            
        fun DEC_to_SymTbl (DataTypes.SEQ(DataTypes.ID(x1), y1), z1, l, idx) = l @ [(x1, z1, inc idx)] @ DEC_to_SymTbl(y1, z1, [], idx)
            | DEC_to_SymTbl (DataTypes.SEQ(DataTypes.DEC(x1, y1), z1), _, l, idx) = l @ DEC_to_SymTbl(x1, dttostr(y1), [], idx) @ DEC_to_SymTbl(z1, "anyval", [], idx)
            | DEC_to_SymTbl (DataTypes.DEC(x, y), _, l, idx) = l @ DEC_to_SymTbl(x, dttostr(y), [], idx)
            | DEC_to_SymTbl (DataTypes.ID(x), y, l, idx) = l @ [(x, y, inc idx)]
            | DEC_to_SymTbl (_, _, l, _) = l;
    in
        DEC_to_SymTbl(abc, "INT", l, idx)
    end

end;
