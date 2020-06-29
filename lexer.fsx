namespace CSCI374

module Lexer =

    type TOKEN = LPAR | RPAR | PLUS | MINUS | MULT | DIV | A | B | C | D | E | F | INVALID | END | EPS with
         member this.ToStr =
             match this with
             | LPAR -> "("
             | RPAR -> ")"
             | PLUS -> "+"
             | MINUS -> "-"
             | MULT -> "*"
             | DIV -> "/"
             | A -> "a"
             | B -> "b"
             | C -> "c"
             | D -> "d"
             | E -> "e"
             | F -> "f"
             | END -> "$"
             | EPS -> "ε"
             | _ ->""

    type RULE = S | T | U | V | W | X | Y | Z with
         member this.ToStr =
             match this with
             | S -> "S"
             | T -> "T"
             | U -> "U"
             | V -> "V"
             | W -> "W"
             | X -> "X"
             | Y -> "Y"
             | Z -> "Z"

    type SYMBOL =
        | Terminal of TOKEN
        | NonTerminal of RULE
        | Error with
        member this.ToStr =
            match this with
            | Terminal t -> t.ToStr
            | NonTerminal n -> n.ToStr
            | Error -> "ERROR"

    type SYMBOLS = SYMBOL list
    type PRODUCTION = SYMBOL * SYMBOLS

    let SymbolsToStrDlm dlm (xs:SYMBOLS) :string =
        List.fold (fun a (s:SYMBOL) -> a + (s.ToStr)+dlm) "" xs

    let SymbolsToStr (xs:SYMBOLS) :string =
        SymbolsToStrDlm "" xs

    let RuleToStr ((lhs,rhs):PRODUCTION) :string =
        sprintf "%s → %s" (lhs.ToStr) (SymbolsToStr rhs)
    type ACTION =
        | Shift of int
        | Reduce of int
        | Accept

    // lexical analyser
    let tokenize input =
        if (Seq.isEmpty input) then
            TOKEN.END, Seq.empty<char>
        else
            let t = match Seq.head input with
                    | '(' -> TOKEN.LPAR
                    | ')' -> TOKEN.RPAR
                    | '+' -> TOKEN.PLUS
                    | '-' -> TOKEN.MINUS
                    | '*' -> TOKEN.MULT
                    | '/' -> TOKEN.DIV
                    | 'a' -> TOKEN.A
                    | 'b' -> TOKEN.B
                    | 'c' -> TOKEN.C
                    | 'd' -> TOKEN.D
                    | 'e' -> TOKEN.E
                    | 'f' -> TOKEN.F
                    | _   -> TOKEN.INVALID
            t, Seq.tail input
