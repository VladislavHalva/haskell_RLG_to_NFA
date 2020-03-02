**Project:** FIT VUT Brno, FLP project - 2019/2020 
**Assignment:** PLG-2-NKA
**Author:** Vladislav Halva
**Login:** xhalva04

# Description

The application reads the input data from a specified file or standard input and converts it to an inner representation at first.
An operation corresponding to given option is done afterwards and the result is written to standard output. The input and output
format of grammar text representation is as follows: 

    <List of nonterminals>\n
    <List of terminals>\n
    <Initial nonterminal>\n
    <Rule 1>\n
    ...
    <Rule n>\n


If option **-i** is chosen, the input data are read and a right linear grammar is parsed, inspected and is written to standard
output if correct, otherwise and error message is written. The application checks if the input corresponds to specification,
if the grammar is right linear and if all terminals and nonterminals used in rules are stated in appropriate lists.

For option **-1** it first executes the same procedure as in case of *-i*, but the grammar is converted to a regular form afterwards
i.e. all rules with more than one terminal on the right side are replaced by a set of rules with one terminal (and nonterminal)
on the right side. The new terminals always match the pattern "A@", where '@' stands for a unique integer. The converted 
grammar is written to standard output at the end.

Option **-2** executes the parsing and conversion to regular form at first. Afterwards the program constructs a nondeterministic 
finite automaton which accepts the same language that is specified by the given grammar. States of the automaton are
labelled with integers. The automaton is written to standard output in following form:

    <List of states>\n
    <Initial state>\n
    <List of accepting states>\n
    <Rule 1>\n
    ...
    <Rule n>\n

# Build 

The program can be build by typing

    make

in the root of the project directory (contains Makefile). It creates an executable file *plg-2-nka* in the same directory.

# Usage

    ./plg-2-nka options [input]

+ *input*: is a name of the input file. If not specified, the program reads from the standard input (STDIN).
+ *options*: 
    - -i  Prints the grammar read from input in specified format.
    - -1  Prints the grammar read from input converted to regular form.
    - -2  Prints a nondeterministic finite automaton that accepts the same language that is specified by the input grammar.
+ If no or incorrect arguments are specified, a help message like this is written to standard output.

# Example inputs and outputs

**Input**

    A,B
    a,b,c
    A
    A->aaB
    A->ccB
    B->bB
    B->#


**Output for *-i***

    A,B
    a,b,c
    A
    A->aaB
    A->ccB
    B->bB
    B->#
    
**Output for *-1***

    A,A0,B,A1
    a,b,c
    A
    A->aA0
    A0->aB
    A->cA1
    A1->cB
    B->bB
    B->#

**Output for *-2***

    0,1,2,3
    0
    2
    0,a,1
    1,a,2
    0,c,3
    3,c,2
    2,b,2

