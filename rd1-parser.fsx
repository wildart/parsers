namespace CSCI374

// #load "lexer.fsx"
// #load "grammartools.fsx"

module RD1 =
    open Lexer
    open GrammarTools

    let enterProd sym cnxt =
        let grammar, verbose, (tkn, input) = cnxt
        if verbose then (printfn "Enter <%A> with token `%A`" sym tkn) else ()
        cnxt

    let exitProd sym cnxt =
        let grammar, verbose, (tkn, input) = cnxt
        if verbose then (printfn "Exit <%A> with token `%A`" sym tkn) else ()
        cnxt

     (*
        1: S -> T
        2: S -> ( S + T )
        3: T -> a
    *)
    let rec ProdS cnxt =
        let grammar, verbose, (tkn, input) = cnxt
        if tkn = LPAR then
            let newcnxt = (grammar, verbose, (tokenize input))
            printGrammarRule false grammar 2 // print rule
            newcnxt |> enterProd S |> ProdS |> exitProd S
                    |> Match PLUS
                    |> enterProd T |> ProdT |> exitProd T
                    |> Match RPAR  // match token
        else
            printGrammarRule false grammar 1 // print rule
            cnxt |> enterProd T |> ProdT |> exitProd T

    and ProdT cnxt =
        let grammar, verbose,  (tkn, input) = cnxt
        printGrammarRule false grammar 3 // print rule
        Match A cnxt // match token

    and Match term cnxt =
        let (grammar, verbose, (tkn, input)) = cnxt
        if verbose then (printfn "Match %s" term.ToStr) else ()
        if term = tkn then
            (grammar, verbose, (tokenize input)) // read next token
        else
            failwith (sprintf "Cannot match symbol `%A` with `%A`" term tkn)

    let parser cnxt :(PRODUCTION [] * bool * (TOKEN * seq<char>)) =
        cnxt |> enterProd S |> ProdS |> exitProd S

    let parse (grammar: PRODUCTION []) input =
        let cnxt = (grammar, false, (tokenize input))
        cnxt |> parser |> ignore
