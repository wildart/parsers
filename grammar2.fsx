#load "lexer.fsx"
#load "grammartools.fsx"

open CSCI374.Lexer
open CSCI374.GrammarTools

// Load parsers
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

(*
Grammar:
    S → S + ( S )
    S → a
*)
let grammar = [|
    NonTerminal S => [NonTerminal S; Terminal PLUS; Terminal LPAR; NonTerminal S; Terminal RPAR];
    NonTerminal S => [Terminal A]
|]

// Show grammar rules
printGrammar grammar

// Input
let inputString = "a+(a)"
printfn "Input string: %s\n" inputString;;

///
/// Top-down parsing: LL(1)
///

let table = [(S,A) => 2]

try
    printfn "LL(1) parser:";
    CSCI374.LL1.parse grammar table inputString
    // CSCI374.LL1.parser grammar (Map.ofList table) inputString true
    printfn "Done!!!\n"
with
    | Failure(msg) -> printfn "Failed to parse: `%s`!\n" msg


(*
Remove left recursion:

    1: S  → a T
    2: T → + ( S ) T
    3: T → Ɛ
*)

let grammar_ll = [|
    NonTerminal S => [Terminal A; NonTerminal T];
    NonTerminal T => [Terminal PLUS; Terminal LPAR; NonTerminal S; Terminal RPAR; NonTerminal T];
    NonTerminal T => [Terminal EPS];
|]

// Show grammar rules
printGrammarCompact grammar_ll;;

(*
Construct parse table

    X    FIRST(X)   FOLLOW(X)
    S    {a}        {$, )}
    T    {Ɛ, +}     {$, )}

    N\T a + ( ) $
    S   1
    T   2   3 3
*)

let table_ll = [(S,A) => 1; (T,PLUS) => 2; (T,RPAR) => 3; (T,END) => 3]

printfn "LL(1) parser (no left recursion):";
CSCI374.LL1.parse grammar_ll table_ll inputString;;
// CSCI374.LL1.parser grammar_ll (Map.ofList table_ll) inputString true
printfn "Done!!!\n"

///
/// Bottom-up parsing: LR(0)
///

/// Action Table
let action = [|
    Map.ofList [(A, Shift 2)]; // 0
    Map.ofList [(PLUS, Shift 3); (END, Accept)]; // 1
    Map.ofSeq (seq {for t in [A; PLUS; LPAR; RPAR; END] -> (t, Reduce 2) }); // 2
    Map.ofList [(LPAR, Shift 4)]; // 3
    Map.ofList [(A, Shift 2)]; // 4
    Map.ofList [(PLUS, Shift 3); (RPAR, Shift 6)]; // 5
    Map.ofSeq (seq {for t in [A; PLUS; LPAR; RPAR; END] -> (t, Reduce 1) }); // 6
|]

/// GOTO Table
let goto = [|
    Map.ofList [(S, 1)];    // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.empty;              // 3
    Map.ofList [(S, 5)];    // 4
    Map.empty;              // 5
    Map.empty;              // 6
|]

printfn "LR(0) parser:";;
CSCI374.LR0.parse grammar action goto inputString;;
