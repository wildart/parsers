namespace CSCI374

// #load "grammartools.fsx"

module LL1 =

    open ParserTypes
    open GrammarTools

    // syntactic analyser
    let parser (grammar: PRODUCTION []) (table:Map<(RULE * TOKEN),int>) input verbose =
        if grammar.Length = 0 then
            failwith "Grammmar is empty!"
        // Push a $ on the stack
        // Initialize the stack to the start symbol.
        let stack = [NonTerminal (fst grammar.[0]); Terminal END]

        let rec analyse stack (token, input) =
            if verbose then (printfn "Token: %A, Stack: %A" token stack) else ()
            if List.isEmpty stack then ()
            else
                // take element from top of the stack
                let sym = List.head stack
                // get next token
                match sym with
                | Terminal term ->
                    match term with
                    | EPS ->
                        // remove Ɛ from stack
                        if verbose then (printfn "pop %A" (List.head stack)) else ()
                        // do not read new token
                        analyse (List.tail stack) (token, input)
                    | term when term = token ->
                        // pop stack
                        if verbose then (printfn "pop %A" (List.head stack)) else ()
                         // advance input, read new token
                        analyse (List.tail stack) (Lexer.token input)
                    | _ ->
                        failwithf "bad term on input: %A" token
                | NonTerminal nterm ->
                    begin
                        // Use nonterminal and current input symbol to find correct production in table.
                        if verbose then (printfn "svalue: %A, token: %A" nterm token) else ()
                        let ruleIdx =
                            try
                                table.[(nterm, token)]
                            with
                                | :? System.Collections.Generic.KeyNotFoundException -> failwith (sprintf "No rule found for %A → %A" nterm token)
                        let rule = grammar.[ruleIdx-1]
                        // Show grammar rule
                        printGrammarRule verbose grammar ruleIdx
                        // Pop stack & push right-hand side of production from table onto stack, last symbol first.
                        let newstack = (snd rule) @ (List.tail stack)
                        analyse newstack (token, input)
                    end
                | _ -> failwith "error"

        analyse stack (Lexer.token input)

    let parse grammar table input =
        parser grammar (Map.ofList table) (Seq.toList input) false
