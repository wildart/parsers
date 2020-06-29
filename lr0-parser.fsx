namespace CSCI374

// #load "lexer.fsx"
// #load "grammartools.fsx"

module LR0 =

    open Lexer
    open GrammarTools

    // syntactic analyser
    let parser (grammar: PRODUCTION []) (action: Map<TOKEN,ACTION>[]) (goto: Map<RULE,int>[]) input verbose =
        if grammar.Length = 0 then
            failwith "Grammmar is empty!"
        // Push a 0 on the stack
        let stack = [0]

        let rec analyse stack (token, input) =
            if verbose then (printfn "Input: %A, Token: %A, Stack: %A" input token stack) else ()
            if not (List.isEmpty stack) then
                // current state is taken from top of stack
                let st = List.head stack
                let act = action.[st].[token]
                if verbose then (printfn "State: %A, Action: %A" st act)
                // get next token
                match act with
                // shift and go to a new state
                | Shift state -> analyse (state::stack) (tokenize input) // advance input
                // reduce by rule: X ::= A1...An
                | Reduce ruleIdx ->
                    match grammar.[ruleIdx-1] with
                    | (NonTerminal lhs, rhs) ->
                        // print current gramma rule
                        if verbose then printf "Reduce Rule "; else ()
                        printGrammarRule verbose grammar ruleIdx
                        // restore state before reduction from top of stack
                        let newstack = List.skip (List.length rhs) stack
                        let tops = List.head newstack
                        if verbose then (printfn "Stack top: %A" tops)
                        let state = goto.[(tops)].[lhs] // state after reduction
                        analyse (state::newstack) (token, input)
                    | _ -> failwith "Incorrect production"
                | Accept -> printfn "Accepted!!!"
            else ()

        analyse stack (tokenize input)

    let parse grammar action goto input = parser grammar action goto input false
