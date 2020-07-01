namespace CSCI374

// #load "grammartools.fsx"

module RD1 =
    open ParserTypes
    open GrammarTools

    type Tokenizer(grammar: PRODUCTION [], verbose: bool) =
        let mutable inputState = []
        let mutable curentToken = INVALID
        member this.InputState
            with get() = inputState
            and set(value) = inputState <- value
        member this.CurrentToken = curentToken
        member this.IsVerbose = verbose
        member this.NextToken() =
            let tkn, input = Lexer.token inputState
            inputState <- input
            curentToken <- tkn
            this
        member this.PrintRule ruleIdx =
            printGrammarRule false grammar ruleIdx // print rule
        new(grammar) = Tokenizer(grammar, false)

    let (==>) (cnxt:Tokenizer) (prod:Tokenizer->Tokenizer) =
        if cnxt.IsVerbose then
            printfn "Enter <%A> with token `%A`" prod cnxt.CurrentToken
        let nextcnxt = prod cnxt
        if cnxt.IsVerbose then
            printfn "Exit <%A> with token `%A`" prod cnxt.CurrentToken
        nextcnxt

    let (@) (cnxt:Tokenizer) ruleIdx =
        cnxt.PrintRule ruleIdx
        cnxt

    //==========================================================================
    // Recursive-descent parser
    //--------------------------------------------------------------------------
    let rec ProdS (cnxt:Tokenizer) =
        if cnxt.CurrentToken = LPAR then
            cnxt.NextToken() @2==> ProdS ==> Match PLUS ==> ProdT ==> Match RPAR // 2: S -> ( S + T )
        else
            cnxt @1==> ProdT // 1: S -> T

    and ProdT (cnxt:Tokenizer) =
        cnxt @3==> Match A // 3: T -> a

    and Match term cnxt =
        if cnxt.IsVerbose then printfn "Match %A with %A" term cnxt.CurrentToken
        // if we matched the current token with a terminal symbol
        if term = cnxt.CurrentToken then
            cnxt.NextToken() // read next token
        else
            failwith (sprintf "Cannot match symbol `%A` with `%A`" term cnxt.CurrentToken)
    //==========================================================================

    let parser (cnxt:Tokenizer) :Tokenizer =
        // Start parsing by calling function for starting symbol
        cnxt.NextToken() ==> ProdS

    let parse (grammar: PRODUCTION []) input =
        Tokenizer(grammar, InputState=(Seq.toList input)) |> parser |> ignore

    let parseVerbose (grammar: PRODUCTION []) input =
        Tokenizer(grammar, true, InputState=(Seq.toList input)) |> parser |> ignore
