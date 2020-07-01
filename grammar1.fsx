#load "grammartools.fsx"
#load "rd1-parser.fsx"
#load "ll1-parser.fsx"
#load "lr0-parser.fsx"

open CSCI374.ParserTypes
open CSCI374.GrammarTools

let grammar = parseGrammarString """
    S → T | ( S + T )
    T → a
"""

// Show grammar rules
printGrammar grammar

// Input
let inputString = "((a+a)+a)"
printfn "Input string: %s\n" inputString;;

inputString |> Seq.toList |> CSCI374.Lexer.tokenize |> printfn "Tokenized input: %A\n"

///
/// Top-down parsing: Recursive Descent
///
printfn "RD parser:";;
CSCI374.RD1.parse grammar inputString;;
// CSCI374.RD1.parseVerbose grammar inputString;;
printfn "Done!!!\n";;


///
/// Top-down parsing: LL(1)
///

/// LL(1) parse table
let table = [(S,A) => 1; (S,LPAR) => 2; (T,A) => 3]

printfn "LL(1) parser:";;
CSCI374.LL1.parse grammar table inputString;;
printfn "Done!!!\n";;

///
/// Bottom-up parsing: LR(0)
///

/// Action Table
let action = [|
    Map.ofList [LPAR => Shift 3; A => Shift 4]; // 0
    Map.ofList [END => Accept]; // 1
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> t => Reduce 1 }); // 2
    Map.ofList [LPAR => Shift 3; A => Shift 4]; // 3
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> t => Reduce 3 }); // 4
    Map.ofList [PLUS => Shift 6]; // 5
    Map.ofList [A => Shift 4];    // 6
    Map.ofList [RPAR => Shift 8]; // 7
    Map.ofSeq (seq {for t in [LPAR; PLUS; RPAR; A; END] -> t => Reduce 2 }); // 8
|]

/// GOTO Table
let goto = [|
    Map.ofList [T => 2; S => 1]; // 0
    Map.empty;              // 1
    Map.empty;              // 2
    Map.ofList [T => 2; S => 5]; // 3
    Map.empty;              // 4
    Map.empty;              // 5
    Map.ofList [T => 7];    // 6
    Map.empty;              // 7
    Map.empty;              // 8
|]

printfn "LR(0) parser:";;
CSCI374.LR0.parse grammar action goto inputString;;
