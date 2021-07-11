# Boolf Parser, Type Checker and Evaluator

**Abstract** : A parser and evaluator written for a custom defined, strongly typed functional language of integer and boolean expressions. Built on top of Standard ML, using the tools mllex and mlyacc.

## Index
- [**Syntax**](#syntax)
    - [**Function Declarations**](#function-declarations)
    - [**Expressions**](#expressions)
    - [**Variable Declarations**](#variable-declarations)
    - [**Lambda Abstractions**](#lambda-abstractions)
    - [**Conditional Branching**](#conditional-branching)
- [**Formal Syntax (EBNF Definition)**](#formal-syntax)
- [**Types and Type Checking**](#types-and-type-checking)
    - [**Type Error Messages**](#type-error-messages)
- [**Output Format**](#output-format)
    1. [**Type Checking**](#1-type-checking)
    2. [**Evaluated Values**](#2-evaluated-values)
    3. [**Abstract Syntax Trees**](#3-abstract-syntax-trees)
- [**Execution Instructions**](#execution-instructions)


## Syntax

Informally, each program in this language is a set of statements. Each statement except the last one is followed by a ```;```. 

### Function Declarations
A statement is either a function declaration of the form  
```fun <func_name>(<arg_name> : <arg_typ>) : <return_typ> => function_body```  
or an expression. Note that only single-argument functions are supported, and the types are not optional. Also, the language supports recursion for functions defined this way.

### Expressions
An expression can be either an integer (use ```NEGATE``` for negative integers) or a boolean constant (```TRUE``` or ```FALSE```) or their combinations with operators. The different boolean operators allowed are ```AND, OR, XOR, NOT, EQUALS, IMPLIES``` and integer operators allowed are ```PLUS, MINUS, TIMES, NEGATE, LESSTHAN, GREATERTHAN, EQUALS```. Precedence and associativity rules are the same as those in the C language. Parentheses can be used to override these rules. 

### Variable Declarations
The language supports variable declarations through a ```let in end``` construct. The in must be followed by an expression (which could itself be a "let-expression"), and the let must be followed by a variable declaration, like ```x = TRUE```. An example is   
```
    let
        x = TRUE
    in
        x AND NOT x
    end
```

### Lambda Abstractions
The language supports assigning lambda functions to variables, and passing them as arguments to a function or returning them as the value of a function. Thus lambda abstractions are treated as **first class objects**. Inside a ```let in end``` block a lambda function can be assigned like ```x = fn (y:(int->bool)):bool => (y 1)```. The general format for lambda abstractions is thus ```fn (<arg_name>:<arg_type>):<ret_type> => <body>```. These cannot be recursively defined. Note also that function application is done as ```(<function_name> <argument>)```.

### Conditional Branching
The language supports conditional branching like so:  
```
    if TRUE OR FALSE then
        1 PLUS 4
    else
        1 MINUS 4
    fi
```
Note that both branches are necessary.

## Formal Syntax
The grammar of the language is formally written through the following EBNF Definition.  
```
    program ::= statement_list.
    statement_list ::= statement_list ";" stmnt
                    | stmnt.
    stmnt ::= "fun" identifier "(" identifier ":" type ")" ":" type "=>" expr
            | expr.
    expr ::= expr binop expr
           | unop expr
           | "if" expr "then" expr "else" expr "fi"
           | "let" identifer "=" expr "in" expr "end"
           | "fn" "(" identifier ":" type ")" ":" type "=>" expr.
           | "(" expr expr ")"
           | "(" expr ")"
           | number
           | bool_const
           | identifier.
    binop ::= "PLUS"
            | "MINUS"
            | "TIMES"
            | "EQUALS"
            | "AND"
            | "OR"
            | "XOR"
            | "IMPLIES"
            | "LESSTHAN"
            | "GREATERTHAN".
    unop ::= "NEGATE"
           | "NOT".
    type ::= "int"
           | "bool"
           | type "->" type
           | "(" type ")".
    number ::= digit {digit}.
    digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9".
    identifier ::= letter {letter}.
    letter ::= upper_case | lower_case.
    upper_case ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z".
    lower_case ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z".
```

## Types and Type Checking
The allowed types are ```int```, ```bool``` and ```(t1 -> t2)``` for any valid types ```t1, t2```. The evaluator also type checks the program before evaluation and prints out an error message to the console. The message contains the **reason for the type error**, the **line and column numbers** where the error was found, as well as the **Required Types** and **Types Present**. I have made custom testcases for each kind of type error, and they are present in [Type Checking Error Testcases](customTests/error/typeChecking/).

### Type Error Messages
- ```First Expression is not a function type in function application expression```
- ```Types of Formal and Actual Parameters don't match in Function Application```
- ```Right hand side of function declaration does not agree with function result type```
- ```Condition in if expression doesn't have required type```
- ```Two arms of if expression don't have equal types```
- ```Right hand side of lambda function does not agree with function result type```
- ```Types of operands don't match required types in Binary Operator Expression```
- ```Type of operand doesn't match required type in Unary Operator Expression```
- ```Variable <var_name> has not been declared```

## Output Format
The output consists of 3 sections
### 1. Type Checking
This contains the type of each statement in the program (for function declarations we display a custom message with the type of the function). Each statement's type is delimited by two lines of hyphens.  
  
### 2. Evaluated Values
This contains the evaluated value (the normal form of our calculus) of each statement, again delimited by two lines of hyphens. When the value is an integer or boolean, we simply display it along with its type, and when it is a Lambda Abstraction or function declaration, we display its type along with the Abstract Syntax Tree of the normal form (reducing the body to the normal form might include things like reducing a "0 PLUS 1" inside the function declaration body to "1").

### 3. Abstract Syntax Trees
This contains the Abstract Syntax Trees of each statement, again delimited by two lines of hyphens. The ASTs have been pretty printed using a specially designed printing function.

## Execution Instructions
- Make sure you have the `MLTon` compiler for Standard ML installed. The tools `mllex` and `mlyacc` are also required, they should be installed along with the MLTon compiler in case of a normal installation.
- Navigate to the directory containing the Makefile.
- Type `make` to compile the parser. An executable named `boolf` will be generated.
- Run `./boolf <PathToInputFile>` to execute.
- If you want to compile again, run `make clean` followed by `make`.
