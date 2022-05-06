# COL226 Assignment: Abstract Syntax Trees (AST) for WHILE
## Instructions to RUN
- Add your WHILE program code to a file with extension
- Enter sml interactive mode using sml in Terminal
- Enter the following command
```
CM.make "WHILE.cm";
```
- To run your code, use the following line in SML
```
VMC.execute "<yourfilename_with_extension>";
```
- It will correctly evaluate the commands for syntactically correct program.

- To use toString function, use the following line in SML
```
VMC.toString <varible containing the tuple>;
```
- It will output a configuration of the VMC machine as 3-tuple of lists
representing the values of V, M and C.

- To use postfix function, use the following line in SML
```
VMC.postfix <varible containing the AST>;
```
- It will output a Block of commands
into post-fix form in a string Stack

- To generate AST, use the following line in SML
```
WHILE.parse "<filename_with_extension>";
```
- It will output the AST for the program file

> *NOTE : The AST generator is in WHILE structure, the Stack definitions are in FunStack structure, execute, toString and postfix functions are in VMC structure*

## Design Decisions
- For giving user the Input prompt, I added "Input \<var name\> :" so that the user knows what type of input is to be given 
- For displaying Output, I added "Output :" and then the value at runtime.

## Sources Consulted
- For simple syntax-related doubts, I consulted StackOverflow.
- SML Documentation

## Changes in AST generator from Previous Assignment
- Added 2 lines and a datatype in Yacc and Lex file respectively to ease the making of evaluator.
- Added Markers in `while` command and `if else then` command to distinguish parts of their condition and statements from the normal program so that they can be evaluated properly.

## Acknowledgements
- As mentioned in previous assignment too, the AST Generator is greatly inspired from "User’s Guide to ML-Lex and ML-Yacc" on rogerprice.org