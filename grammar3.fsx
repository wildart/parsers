#load "lexer.fsx"
#load "grammartools.fsx"

open CSCI374.Lexer
open CSCI374.GrammarTools

// Load parsers
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

(*
Grammar:
    S -> S * T | S + T | T
    T -> a | b
*)
let grammar = [|
    NonTerminal S => [NonTerminal S; Terminal MULT; NonTerminal T];
    NonTerminal S => [NonTerminal S; Terminal PLUS; NonTerminal T];
    NonTerminal S => [NonTerminal T];
    NonTerminal T => [Terminal A];
    NonTerminal T => [Terminal B]
|]

// Show grammar rules
printGrammarCompact grammar;;

// Input
let inputString = "b+b*b"
printfn "Input string: %s\n" inputString;;


///
/// Top-down parsing: LL(1)
///

(*
Remove left recursion:

    S  -> T U
    U -> * T U | + T U | Ɛ
    T  -> a | b
*)

let grammar_ll = [|
    NonTerminal S => [NonTerminal T; NonTerminal U];
    NonTerminal U => [Terminal MULT; NonTerminal T; NonTerminal U];
    NonTerminal U => [Terminal PLUS; NonTerminal T; NonTerminal U];
    NonTerminal U => [Terminal EPS];
    NonTerminal T => [Terminal A];
    NonTerminal T => [Terminal B]
|]

// Show grammar rules
printGrammar grammar_ll;
//printGrammarCompact grammar_ll;

(*
Construct parse table

    X    FIRST(X)   FOLLOW(X)
    S    {a,b}      {$}
    U    {*,+,Ɛ}    {$}
    T    {a,b}      {$}

    N\T a b + * $
    S   1 1
    U       2 3 4
    T   5 6
*)

let table = [((S,A),1); ((S,B),1);
             ((U,MULT),2); ((U,PLUS),3); ((U,END),4);
             ((T,A),5); ((T,B),6) ]

printfn "LL(1) parser:";
CSCI374.LL1.parse grammar_ll table inputString;;
printfn "Done!!!\n"


///
/// Bottom-up parsing: LR(0)
///

/// Action Table
let action = [|
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 0
    Map.ofList [(END, Accept); (MULT, Shift 5); (PLUS, Shift 6)]; // 1
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 3) }); // 2
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 4) }); // 3
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 5) }); // 4
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 5
    Map.ofList [(A, Shift 3); (B, Shift 4)]; // 6
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 1) }); // 7
    Map.ofSeq (seq {for t in [A; B; MULT; PLUS; END] -> (t, Reduce 2) }); // 8
|]

/// GOTO Table
let goto = [|
    Map.ofList [(T, 2); (S, 1)]; // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.empty;              // 3
    Map.empty;              // 4
    Map.ofList [(T, 7)];    // 5
    Map.ofList [(T, 8)];    // 6
    Map.empty;              // 7
    Map.empty;              // 8
|]

printfn "LR(0) parser:";;
CSCI374.LR0.parse grammar action goto inputString;;
