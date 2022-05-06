(* CM.make "WHILE.cm";
use "Stack.sml"; *)
(* open DataTypes; *)

structure VMC :
sig val execute : string -> (string FunStack.Stack * int array * string FunStack.Stack)
    val toString : (string FunStack.Stack * int array * string FunStack.Stack) -> (string list * int list * string list)
    val postfix : DataTypes.AST -> string FunStack.Stack
end =
struct
    exception TypeMismatch;
    exception BadVariable;
    exception BadInput;
    exception DivbyZeroError;
    exception ModbyZeroError;
    exception BadOperation;
    exception BadBoolOperation;
    exception BoolInWriteError;

    fun toString x =
        let
            fun arrayToList arr = Array.foldr (op ::) [] arr;
            fun get1 (tup:(string FunStack.Stack * int array * string FunStack.Stack)) = #1 tup;
            fun get2 (tup:(string FunStack.Stack * int array * string FunStack.Stack)) = #2 tup;
            fun get3 (tup:(string FunStack.Stack * int array * string FunStack.Stack)) = #3 tup;
            val ret = (FunStack.stack2list(get1 x), arrayToList(get2 x), FunStack.stack2list(get3 x));
        in ret
        end;

    fun postfix AST_const =
        let
            val tmp_C = WHILE.AST_to_list (AST_const, []);
            val ret_C = FunStack.list2stack(tmp_C);
        in 
            ret_C
        end;

    fun execute fname = 
        let 
            val code_AST = WHILE.parse(fname);
            

            val C = postfix (code_AST)
            val Symbol_Table = WHILE.DEC_extract (code_AST, [], ref 0);

            val V = FunStack.create("");

            val M = Array.array(100, 0);


            val first_cmd = FunStack.top(C);
            val tmp_CMDs = FunStack.create("");

            
                
            fun printstack st = 
                let
                    val _ = TextIO.print("[");
                    fun tmpfun (hd::tl) = let val _ = TextIO.print(hd^","); val _ = tmpfun(tl); in () end
                            | tmpfun ([]) = TextIO.print("]\n");
                    val _ = tmpfun(st);
                    in () end;

            (* val V = []; *)

            (* Auxillary functions to extract parts of tuple *)
            fun getfirst(x:string*string*int) = #1 x;
            fun getsecond(x:string*string*int) = #2 x;
            fun getthird(x:string*string*int) = #3 x;


            (* fun printSyT li = 
                let
                    val _ = TextIO.print("[");
                    fun prin (hd::tl) =
                        let
                            val _ = TextIO.print("("^getfirst(hd)^","^getsecond(hd)^","^Int.toString(getthird(hd))^"),");
                            val _ = prin(tl);
                            in () end
                        | prin ([]) = TextIO.print("]\n");
                    val _ = printSyT(li);
                    in () end;

            val _ = printSyT(Symbol_Table); *)

            (* Searches the symbol table and returns the tuple containing variable else raises BadVariable exception *)
            fun get_tup (a) = 
                let
                                (* val _ = TextIO.print("In gettup Looking for "^ a ^"\n"); *)
                    fun searchh (x, l) =
                        let
                            (* val _ = TextIO.print("In Search Looking for "^x^"\n"); *)
                            val k = if(List.null(l)) then raise BadVariable else if(x = getfirst(List.hd(l))) then List.hd(l) else searchh(x, List.tl(l));
                        in
                            k
                        end;
                    val b = searchh(a, Symbol_Table);
                in b
                end;

            (* Checks if str is int *)
            fun str_is_int (xs) = 
                let 
                    val l = String.explode(xs);
                    val cond1 = List.all (Char.isDigit) (List.tl(l));
                    val h = if (List.hd(l) = #"~" orelse Char.isDigit(List.hd(l))) then true else false;
                    val retval = if (cond1 andalso h) then true else false;
                in 
                    retval
                end;

            (* Checks type of varible a is and matches with x *)
            fun check_type (a, x) = 
                let
                    val tup = get_tup(a);
                    (* val _ = TextIO.print("In check_type Looking for "^a^" -> ("^getfirst(tup)^","^getsecond(tup)^","^Int.toString(getthird(tup))^") "); *)
                    val test1 = if(str_is_int(x) = true) then "true" else "false";
                    (* val _ = TextIO.print(x^ " "^test1^"\n");  *)
                    val retval = if(((x = "tt" orelse x = "ff") andalso getsecond(tup) = "BOOL") orelse (str_is_int(x) = true andalso getsecond(tup) = "INT"))then true else raise BadVariable;
                in
                    retval
                end;

            (* Takes a varible and returns integer index if var in M *)
            fun get_idx(a) = 
                let
                    val tup = get_tup(a);
                    val retval = getthird(tup);
                    (* val _ = TextIO.print("Got idx\n"); *)
                in retval
                end;


            (* Deals with read command *)
            fun cmd_read (a) = 
                let
                    fun usrinput c = Option.valOf (TextIO.inputLine(TextIO.stdIn));
                    fun stoiusr "tt" = 1
                        | stoiusr "ff" = 0
                        | stoiusr s = Option.valOf (Int.fromString (s));

                    val _ = TextIO.print("Input "^ a ^ " : ");
                    val y = usrinput(1);
                    val x = String.substring (y, 0, (String.size(y) - 1));
                    (* val _ = TextIO.print("IN Read this is input " ^ x^ " " ^ Int.toString(String.size(x))); *)
                    (* val _ = TextIO.print("Check Again" ^ x ^ "\n"); *)

                    val _ = check_type(a, x);
                    (* val _ = TextIO.print("IN Read After typecheck, input " ^ x ^ "pp\n"); *)
                    (* val _ = TextIO.print("Mem val updated\n"); *)
                    (* val _ = TextIO.print("in read goint to get idx\n"); *)
                    val idx = get_idx(a);
                    (* val _ = TextIO.print("in read After getting idx" ^ Int.toString(idx)^ "pp\n"); *)
                    val x2 = Array.update(M,idx,stoiusr(x));
                    (* val _ = TextIO.print("in read Mem val updated\n"); *)

                in
                    ()
                end;

                    (* Take user input and check matching type in symbol table and move input to adr(a); *)


            fun cmd_write(a) =
                let
                    fun bool_conv (x) = if(x = 0) then "ff" else "tt";
                    fun find_a_val ("tt") = "tt"
                        | find_a_val ("ff") = "ff"
                        | find_a_val (a_var) = if(str_is_int(a_var)) then a_var
                                                else if (getsecond(get_tup(a_var)) = "BOOL") then bool_conv(Array.sub(M, get_idx(a_var)))
                                                else Int.toString(Array.sub(M, get_idx(a_var)));
                    
                    val a_val = find_a_val(a);
                    val _ = if(str_is_int(a_val) = false) then raise BoolInWriteError else 1;
                    val _ = TextIO.print("Output: ");
                    val _ = TextIO.print(a_val);
                    val _ = TextIO.print("\n");
                in
                    ()
                end;


            fun cmd_set (a, b) = 
                let 
                    fun find_b_val ("tt") = "tt"
                        | find_b_val ("ff") = "ff"
                        | find_b_val (b_var) = if (str_is_int(b_var) = true) then b_var
                                                else Int.toString(Array.sub(M, get_idx(b)));

                    (* val t2 =  *)
                    (* val _ = TextIO.print("in Cmd set before type checking var "^a^ " and the second "^ b^"\n"); *)
                    val _ = check_type(a, find_b_val(b));

                    fun b_to_intval ("tt") = 1
                        | b_to_intval ("ff") = 0
                        | b_to_intval (b_strval) = Option.valOf(Int.fromString(b_strval));

                    (* val _ = TextIO.print("in Cmd set after type checking var second "^b^ " \n"); *)

                    val _ = Array.update(M, get_idx(a), b_to_intval(find_b_val(b)));
                in ()
                end;


            fun cmd_arith (a, b, op_type) =
                let 

                    fun find_b_val (b_var) = if (str_is_int(b_var) = true) then b_var
                                                else (check_type(b_var, "1");Int.toString(Array.sub(M, get_idx(b_var))));
                    fun strval_to_intval (b_var) = Option.valOf(Int.fromString(b_var));

                    (* val _ = TextIO.print("in Cmd arith before type checking var "^a^ " and the second "^ b^"\n"); *)

                    val a_val_for_op = strval_to_intval(find_b_val(a));
                    val b_val_for_op = strval_to_intval(find_b_val(b));
                    (* val _ = TextIO.print("in Cmd arith before type checking var "^Int.toString(a_val_for_op)^ " and the second "^ Int.toString(b_val_for_op)^"\n"); *)

                    fun find_res ("PLUS") = Int.toString(a_val_for_op + b_val_for_op)
                        | find_res ("MINUS") = Int.toString(a_val_for_op - b_val_for_op)
                        | find_res ("TIMES") = Int.toString(a_val_for_op * b_val_for_op)
                        | find_res ("DIV") = if(b_val_for_op = 0) then raise DivbyZeroError else Int.toString(a_val_for_op div b_val_for_op)
                        | find_res ("MOD") = if(b_val_for_op = 0) then raise ModbyZeroError else Int.toString(a_val_for_op mod b_val_for_op)
                        | find_res ("LT") = if(a_val_for_op < b_val_for_op) then "tt" else "ff"
                        | find_res ("LEQ") = if(a_val_for_op <= b_val_for_op) then "tt" else "ff"
                        | find_res ("EQ") = if(a_val_for_op = b_val_for_op) then "tt" else "ff"
                        | find_res ("GT") = if(a_val_for_op > b_val_for_op) then "tt" else "ff"
                        | find_res ("GEQ") = if(a_val_for_op >= b_val_for_op) then "tt" else "ff"
                        | find_res ("NEQ") = if(a_val_for_op <> b_val_for_op) then "tt" else "ff"
                        | find_res (_) = raise BadOperation;

                    val result = find_res(op_type);
                in
                    result
                end;

            fun cmd_cmp (a, b, op_type) = 
                let
                    fun find_b_val ("tt") = true
                        | find_b_val ("ff") = false
                        | find_b_val (b_var) = (check_type(b_var, "tt"); if(Array.sub(M, get_idx(b_var))) = 1 then true else false);

                    val a_val_for_op = find_b_val(a);
                    val b_val_for_op = find_b_val(b);

                    fun find_res("NOT") = not a_val_for_op
                        | find_res("AND") = a_val_for_op andalso b_val_for_op
                        | find_res("OR") = a_val_for_op orelse b_val_for_op
                        | find_res(_) = raise BadBoolOperation;

                    val result = if(find_res(op_type)) then "tt" else "ff";
                in
                    result
                end;


            fun rules (dump_stack, V_Stack, C_Stack, "READ") =
                    let
                        val a = FunStack.top(V_Stack);
                        val _ = cmd_read(a);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.pop(V_Stack), M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("READ", dump_stack), FunStack.pop(V_Stack), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "WRITE") =
                    let
                        val a = FunStack.top(V_Stack);
                        (* val _ = TextIO.print(a ^ "is being printed and C_stack is\n"); *)
                        (* val _ = printstack(C_Stack); *)
                        val _ = cmd_write(a);
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.pop(V_Stack), M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("WRITE", dump_stack), FunStack.pop(V_Stack), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "SET") = 
                    let 
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_set(a, b);
                        val ret = if(FunStack.empty(C_Stack) = true) then (VStack1, M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("GT", dump_stack), VStack1, FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "PLUS") =
                    let
                        (* val _ = TextIO.print("In rules PLUS\n"); *)
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "PLUS");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("PLUS", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "MINUS") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "MINUS");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("MINUS", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "TIMES") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "TIMES");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("TIMES", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end

                | rules (dump_stack, V_Stack, C_Stack, "DIV") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "DIV");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("DIV", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "MOD") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "MOD");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("MOD", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "NEG") =
                    let
                        val a = FunStack.top(V_Stack);
                        val VStack1 = FunStack.pop(V_Stack);
                        val new_el = cmd_arith("0", a, "MINUS");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("MOD", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "LT") =
                    let
                        (* val _ = TextIO.print("In LT rules ****\n"); *)
                
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "LT");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("LT", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "LEQ") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "LEQ");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("LEQ", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "EQ") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "EQ");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("EQ", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "GT") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "GT");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("GT", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "GEQ") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "GEQ");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("GEQ", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "NEQ") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_arith(a, b, "NEQ");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("NEQ", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "NOT") =
                    let
                        val a = FunStack.top(V_Stack);
                        val VStack1 = FunStack.pop(V_Stack);
                        val new_el = cmd_cmp(a, "tt", "NOT");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("NOT", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "AND") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_cmp(a, b, "AND");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("AND", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "OR") =
                    let
                        val b = FunStack.top(V_Stack);
                        val new_V_Stack = FunStack.pop(V_Stack);
                        val a = FunStack.top(new_V_Stack);
                        val VStack1 = FunStack.pop(new_V_Stack);
                        val new_el = cmd_cmp(a, b, "OR");
                        val new_cmd = FunStack.top(C_Stack);
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(new_el, VStack1),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("OR", dump_stack), FunStack.push(new_el, VStack1), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end

                | rules (dump_stack, V_Stack, C_Stack, "@1") =
                    let 
                        (* fun findWH  *)
                        (* fun remcmds (cs1, counter) = 
                            let 
                                val garb_cmd = FunStack.top(cs1);
                                val res =   if(garb_cmd = "WH" andalso counter = 1) then cs1 else 
                                            if(garb_cmd = "WH" andalso counter <> 1) then remcmds(FunStack.pop(cs1), counter-1) else
                                            if(garb_cmd = "@1") then remcmds(FunStack.pop(cs1), counter+1) else
                                            remcmds(FunStack.pop(cs1), counter);
                                in res end; *)
                        (* val _ = TextIO.print("This is the incoming cst in @1 rule\n"); *)
                        (* val _ = printstack(C_Stack); *)

                        fun extractcmds (cs1, ts3, counter) = 
                            let 
                                val garb_cmd = FunStack.top(cs1);
                                val res =   if(garb_cmd = "WH" andalso counter = 1) then ts3 else
                                            if(garb_cmd = "WH" andalso counter <> 1) then
                                                let
                                                    val newts3 = FunStack.push("WH", ts3);
                                                    val newcs1 = FunStack.pop(cs1);
                                                in  extractcmds(newcs1, newts3, counter-1) end else
                                            if(garb_cmd = "@1") then 
                                                let
                                                    val newts3 = FunStack.push("@1", ts3);
                                                    val newcs1 = FunStack.pop(cs1);
                                                in  extractcmds(newcs1, newts3, counter + 1) end else
                                                
                                            extractcmds(FunStack.pop(cs1), FunStack.push(garb_cmd, ts3),counter);
                                in res end;

                        fun addcmds (ts2, cs2) = 
                            let 
                                val res = if(FunStack.empty(ts2)) then cs2 else
                                            let
                                                val garb_cmd = FunStack.top(ts2);
                                            in addcmds(FunStack.pop(ts2), FunStack.push(garb_cmd, cs2)) end;
                            in res end;
                        
                        val extractedcst = extractcmds(C_Stack, FunStack.pop(FunStack.create("")), 1);

                        (* val _ = TextIO.print("This is the extracted cst\n"); *)
                        (* val _ = printstack(extractedcst); *)
                        val ncs1 = FunStack.push("@1", C_Stack);

                        val ncs2 = addcmds(extractedcst, ncs1);
                        (* val _ = TextIO.print("This is the latest cst\n"); *)
                        (* val _ = printstack(ncs2); *)
                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(ncs2);
                            in rules(FunStack.push("@1", dump_stack), V_Stack, FunStack.pop(ncs2), new_cmd)
                            end;
                        in ret end

                | rules (dump_stack, V_Stack, C_Stack, "@2") =
                    let 
                        (* val _ = TextIO.print("This is V-Stack in rules of @2\n"); *)
                        (* val _ = printstack(V_Stack); *)
                        (* val _ = TextIO.print("This is in rules of @2  Mem [a] = "^Int.toString(Array.sub(M, 1))^ "\n"); *)
                        val while_cond = if (FunStack.top(V_Stack) = "tt") then true else false;
                        (* val _ = TextIO.print("before type check in rules of @2\n"); *)
                        val _ = if(FunStack.top(V_Stack) = "tt" orelse FunStack.top(V_Stack) = "ff") then () else raise BadVariable;
                        (* val _ = TextIO.print("after type check in rules of @2\n"); *)

                        fun remcmds (cs1, counter) = 
                            let 
                                val garb_cmd = FunStack.top(cs1);
                                val res =   if(garb_cmd = "WH" andalso counter = 2) then FunStack.pop(cs1) else 
                                            if(garb_cmd = "WH" andalso counter > 2) then remcmds(FunStack.pop(cs1), counter-1) else
                                            if(garb_cmd = "@1") then remcmds(FunStack.pop(cs1), counter+1) else
                                            remcmds(FunStack.pop(cs1), counter);
                                in res end;
                        
                        (* val _ = TextIO.print("Cstack before removal****************************************** @2\n"); *)
                        (* val _ = printstack(C_Stack); *)
                        val newstach = if(while_cond = true) then C_Stack else remcmds(C_Stack, 1);
                        (* val _ = TextIO.print("newstach Cstack after removal****************************************** @2\n"); *)
                        (* val _ = printstack(newstach); *)

                        (* val _ = TextIO.print("Exit from****************************************** @2\n"); *)



                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(newstach);
                                (* val _ = printstack(FunStack.pop(newstach)); *)
                            in rules(FunStack.push("@2", dump_stack), V_Stack, FunStack.pop(newstach), new_cmd)
                            end;
                        in ret end


                | rules (dump_stack, V_Stack, C_Stack, "WH") =
                    let
                        
                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("WH", dump_stack), V_Stack, FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "$1") =
                    let
                        (* val _ = TextIO.print("in rules $1 with val at top of v stack"^ FunStack.top(V_Stack)^"\n"); *)
                        val cnt = 1;
                        val ite_cond = if (FunStack.top(V_Stack) = "tt") then true else false;
                        (* val _ = TextIO.print("before type check in rules of $1\n"); *)
                        val _ = if(FunStack.top(V_Stack) = "tt" orelse FunStack.top(V_Stack) = "ff") then true else raise BadVariable;
                        (* val _ = TextIO.print("after type check in rules of $1\n"); *)
                        val tmp_dump_stack = dump_stack;
                        (* val _ = TextIO.print(" C_stack printed below in rules of $1\n"); *)
                        (* val _ = printstack(C_Stack); *)
                        (* val _ = TextIO.print("after printing C_stack in rules of $1\n"); *)
                        
                        fun remcmds (cs1, counter) = 
                            let 
                                val garb_cmd = FunStack.top(cs1);
                                val res =   if(garb_cmd = "ITE" andalso counter = 1) then FunStack.pop(cs1) else 
                                            if(garb_cmd = "ITE" andalso counter <> 1) then remcmds(FunStack.pop(cs1), counter-1) else
                                            if(garb_cmd = "$1") then remcmds(FunStack.pop(cs1), counter+1) else
                                            if(garb_cmd = "$2") then remcmds(FunStack.pop(cs1), counter) else
                                            remcmds(FunStack.pop(cs1), counter);
                                in res end;


                        fun tmprules1 (tmpC_stack, tmp_cs, "$1", true, counter, counter1) = 
                                let
                                    val newtmp_cs = if (counter = 0) then tmp_cs else FunStack.push("$1", tmp_cs);
                                    val newcounter = counter + 1;
                                    val res = if(FunStack.empty(tmpC_stack) = true) then newtmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), newtmp_cs, garb_cmd, true, newcounter, counter1+1)
                                                end;

                                    in res end
                            | tmprules1 (tmpC_stack, tmp_cs, "$1", false, counter, counter1) = 
                                let
                                    val newcounter = counter + 1;
                                    val res = if(FunStack.empty(tmpC_stack) = true) then tmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), tmp_cs, garb_cmd, false, newcounter, counter1 + 1)
                                                end;

                                    in res end
                            | tmprules1 (tmpC_stack, tmp_cs, "$2", true, counter, counter1) = 
                                let
                                    val newcounter = counter - 1;
                                    (* val _ = TextIO.print(Int.toString(newcounter)^ ", "^Int.toString(counter1)^" is val of new counter and counter1 in $2 tmprules1\n"); *)
                                    val newtmp_cs = if (newcounter = 0) then tmp_cs else FunStack.push("$2", tmp_cs);
                                    val newcond = if(newcounter = 0) then false else true;
                                    (* val _ = TextIO.print("*****new counter check kar le 0 hai to false kar hoga in $2 tmprules1\n"); *)
                                    (* val _ = printstack(newtmp_cs); *)
                                    val res = if(FunStack.empty(tmpC_stack) = true) then newtmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), newtmp_cs, garb_cmd, newcond, newcounter, counter1)
                                                end;

                                    in res end
                            | tmprules1 (tmpC_stack, tmp_cs, "$2", false, counter, counter1) = 
                                let
                                    val newcounter = counter - 1;
                                    (* val _ = TextIO.print(Int.toString(newcounter)^ " is val of new counter in $2 tmprules1\n"); *)
                                    (* val _ = printstack(tmp_cs); *)
                                    val newcond = if(newcounter = 0 andalso counter1 = 1) then true else false;
                                    val res = if(FunStack.empty(tmpC_stack) = true) then tmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), tmp_cs, garb_cmd, newcond, newcounter, counter1)
                                                end;
                                    in res end
                            | tmprules1 (tmpC_stack, tmp_cs, "ITE", fx, 0, counter1) = 
                                let 
                                    val newcounter1 = counter1 - 1;
                                    val newtcs1 = if(newcounter1 <= 1) then tmp_cs else FunStack.push("ITE", tmp_cs);
                                    val res = if(newcounter1 <= 1) then 
                                    let 
                                            (* val _ = TextIO.print(Int.toString(newcounter1)^" is val of counter 1 Focus here ITE found and ends***************\n"); *)
                                            (* val _ = printstack(tmpC_stack); *)
                                            (* val _ = printstack(tmp_cs); *)
                                    in tmp_cs end else
                                        let 
                                            val garb_cmd = FunStack.top(tmpC_stack);
                                            (* val _ = TextIO.print(Int.toString(newcounter1)^" is val of counter 1 Focus here ITE found but***************\n"); *)
                                            (* val _ = printstack(tmpC_stack); *)
                                            (* val _ = TextIO.print("Below is stack extracted till now Focus here ITE found but***************\n"); *)
                                            (* val _ = printstack(newtcs1); *)
                                        in
                                            tmprules1(FunStack.pop(tmpC_stack), newtcs1, garb_cmd, true, 0, newcounter1)
                                        end;
                                    in res end

                            | tmprules1 (tmpC_stack, tmp_cs, curr_cmd, true, counter, counter1) = 
                                let
                                    val newtmp_cs = FunStack.push(curr_cmd, tmp_cs);
                                    val newcounter1 = if(curr_cmd = "ITE") then (counter1-1) else counter1;
                                    (* val _ = TextIO.print("in true other var of tmprules1 in rules of $1; currcmd ="^curr_cmd^"\n"); *)

                                    val res = if(FunStack.empty(tmpC_stack) = true) then newtmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), newtmp_cs, garb_cmd, true, counter, newcounter1)
                                                end;
                                    in res end
                            | tmprules1 (tmpC_stack, tmp_cs, curr_cmd, false, counter, counter1) = 
                                let
                                    val newcounter1 = if(curr_cmd = "ITE") then (counter1-1) else counter1;
                                    (* val _ = TextIO.print("in false other var of tmprules1 in rules of $1; currcmd ="^curr_cmd^" with counter1 =" ^Int.toString(newcounter1)^"\n"); *)
                                    val res = if(FunStack.empty(tmpC_stack) = true) then tmp_cs
                                            else 
                                                let 
                                                    val garb_cmd = FunStack.top(tmpC_stack);
                                                in
                                                    tmprules1(FunStack.pop(tmpC_stack), tmp_cs, garb_cmd, false, counter, newcounter1)
                                                end;
                                    in res end;
                        fun addcmds (ts2, cs2) = 
                            let 
                                val res = if(FunStack.empty(ts2)) then cs2 else
                                            let
                                                val garb_cmd = FunStack.top(ts2);
                                            in addcmds(FunStack.pop(ts2), FunStack.push(garb_cmd, cs2)) end;
                            in res end;

                        (* val _ = TextIO.print("before finding tmpC1 in rules of $1 after this the remaining stack is found\n"); *)
                        val tmpC1 = remcmds(C_Stack, 1);
                        (* val _ = TextIO.print("after finding tmpC1 in rules of $1 below is tmpC1 the commads left after removal\n"); *)
                        (* val _ = printstack(tmpC1); *)
                        val tcs2 = tmprules1(C_Stack, FunStack.pop(FunStack.create("")), "$1", ite_cond, 0, 0);
                        (* val _ = TextIO.print("in Rule $1 printing the extracted rules#################\n"); *)
                        (* val _ = printstack(tcs2); *)
                        (* val _ = TextIO.print("in Rule $1 printed the extracted rules#################\n"); *)
                        val tmpC2 = addcmds(tcs2, tmpC1);
                        (* val _ = TextIO.print("in Rule $1 at exit printing new command stack#################\n"); *)
                        (* val _ = printstack(tmpC2); *)


                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(tmpC2);
                            in rules(dump_stack, V_Stack, FunStack.pop(tmpC2), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "$2") =
                    let
                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("$2", dump_stack), V_Stack, FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "SEQ") =
                    let 
                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("SEQ", dump_stack), V_Stack, FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, "ITE") =
                    let 
                        val cnt = 1;
                        val ret = if(FunStack.empty(C_Stack) = true) then (V_Stack,  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push("ITE", dump_stack), V_Stack, FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end
                | rules (dump_stack, V_Stack, C_Stack, x)   =
                    let 
                        (* val _ = TextIO.print("Cstack in other var "^x^ "\n"); *)
                        (* val _ = printstack(C_Stack); *)
                        val ret = if(FunStack.empty(C_Stack) = true) then (FunStack.push(x, V_Stack),  M, C_Stack) else
                            let
                                val new_cmd = FunStack.top(C_Stack);
                            in rules(FunStack.push(x, dump_stack), FunStack.push(x, V_Stack), FunStack.pop(C_Stack), new_cmd)
                            end;
                        in ret end;
            val VMCtuple = rules(tmp_CMDs, V, FunStack.pop(C), first_cmd);
            in VMCtuple
            end
    end;


