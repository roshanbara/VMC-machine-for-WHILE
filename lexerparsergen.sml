(* glue.sml Create a lexer and a parser *)
structure WHILELrVals  = WHILELrValsFun(
                structure Token = LrParser.Token);
structure WHILELex     = WHILELexFun(
                structure Tokens = WHILELrVals.Tokens);
structure WHILEParser = JoinWithArg(
                structure ParserData = WHILELrVals.ParserData
                structure Lex=WHILELex
                structure LrParser=LrParser);