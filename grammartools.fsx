namespace CSCI374

// #load "lexer.fsx"

module GrammarTools =

    open Lexer
    open Microsoft.FSharp.Reflection

    let rng = System.Random()
    let (=>) s ss = (s,ss)

    let strGrammarRule verbose (grammar:PRODUCTION []) ruleIdx =
        let lhs, rhs = grammar.[ruleIdx-1]
        if verbose then
            sprintf "%d: %A → %A" ruleIdx lhs rhs
        else
            sprintf "%d: %s → %s" ruleIdx (lhs.ToStr) (SymbolsToStr rhs)

    let printGrammarRule verbose (grammar:PRODUCTION []) ruleIdx =
        (strGrammarRule verbose grammar ruleIdx) |> printfn "%s"

    let strGrammar (grammar:PRODUCTION []) =
        seq {
            for i in seq {1 .. (Array.length grammar)} ->
            strGrammarRule false grammar i
        } |> Seq.fold (fun a v -> a + v + "\n") ""

    let strGrammarCompact (grammar:PRODUCTION []) =
        seq {
            for nt in grammar |> Array.map fst |> Array.distinct ->
                let rhs = grammar
                        |> Array.filter (fun p -> (fst p) = nt)
                        |> Array.map (snd >> SymbolsToStr)
                        |> Array.fold (fun a s -> a + s + " | ") ""
                        |> fun s -> s.TrimEnd([|'|'; ' '|])
                sprintf "%s → %s" (nt.ToStr) rhs
        } |> Seq.fold (fun a v -> a + v + "\n") ""

    let printGrammar (grammar:PRODUCTION []) =
        printfn "Grammar:\n%s" (strGrammar grammar)

    let printGrammarCompact (grammar:PRODUCTION []) =
        printfn "Grammar:\n%s" (strGrammarCompact grammar)

    let findProduction grammar sym =
        Array.filter (fun (lhs,rhs) -> lhs = sym) grammar

    let gensentence (grammar:PRODUCTION []) sym debug maxdepth =
        let rnd = System.Random()
        let rec expand sym depth :(string * int) =
            if debug then printfn "D:%d, SYM: %A" depth sym else ()
            let prods = findProduction grammar sym
            let prodIdx =
                if prods.Length = 1 then 0
                else System.Random().Next() % prods.Length
            processRHS (snd prods.[prodIdx]) (depth+1)
        and processRHS rhs depth :(string * int) =
            if debug then printfn "D:%d, RHS: %A" depth rhs else ()
            if depth = maxdepth then failwithf "Reached maximum depth: %d" maxdepth else ()
            match rhs with
            | [] -> ("", depth)
            | h::xs ->
                let cstf =
                    match h with
                    | Terminal s -> (s.ToStr, depth)
                    | NonTerminal s -> expand h depth
                    | _ -> failwith "ERROR"
                let estf = processRHS xs depth
                let reached = max (snd cstf) (snd estf)
                ((fst cstf) + (fst estf), reached)
        expand sym 0

    let grammarSentence (grammar:PRODUCTION []) =
        gensentence grammar (fst grammar.[0]) false 100

    let grammarSentences grammar mindepth maxdepth =
        let mutable c = 0
        seq {
            while true do
                c <- c+1
                let sentence, depth =
                    try
                        gensentence grammar (fst grammar.[0]) false maxdepth
                    with
                        | Failure a -> "", 9
                if sentence.Length > 0 && mindepth <= depth then
                    yield sentence
                    c <- 0
                else ()
                if c = 1000 then failwith "Cannot generate sentences with specified parameters"
        }

    let getUnionTypes<'T> () =
        let cases = FSharpType.GetUnionCases(typeof<'T>)
        Array.map (fun (uc:UnionCaseInfo) -> uc.Name) cases

    let getTypeByName<'T> tname =
        let cases = FSharpType.GetUnionCases(typeof<'T>)
        let typ = Array.find (fun (uc:UnionCaseInfo) -> uc.Name = tname) cases
        FSharpValue.MakeUnion(typ, [| |]) :?> 'T

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    // shuffle an array (in-place)
    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rng.Next(i, Array.length a))) a
        a

    let sample n xs =
        xs
        |> Array.map (fun x -> rng.Next(),x)
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.take n

    // Returns number of nonterminals and terminals in the rule
    let getRuleSize rsize =
        let curRuleSize = rng.Next(1,rsize+1)
        let ntermCount = (float curRuleSize) *rng.NextDouble() |> floor |> int
        let termCount = curRuleSize - ntermCount
        ntermCount, termCount

    // Checks if collection of rules has nonterminals
    let hasNonterminals rules =
        let ntCount = List.fold (fun a e -> a + (fst e)) 0 rules
        ntCount > 0

    let genRules rhsRules ruleSize =
        [for i in 1 .. rng.Next(1,rhsRules+1) -> getRuleSize ruleSize]

    let genProdParams (nterms:string []) (terms:string []) rhsRules ruleSize =
        seq {
            for (i,nt) in Array.zip [|0 .. nterms.Length-1|] nterms do
                let mutable generate = true
                while generate do
                    let rhs = genRules rhsRules ruleSize
                    // first production should have nonterminals
                    if hasNonterminals rhs || i <> 0 then
                        generate <- false
                        let rhsSym = [
                            for (ntc, tc) in rhs ->
                                let ntsym =
                                    (Array.filter (fun s -> s <> nt) nterms |> sample ntc)
                                    |> Array.map (getTypeByName<RULE> >>  NonTerminal)
                                let tsym =
                                    (terms |> sample tc)
                                    |> Array.map (getTypeByName<TOKEN> >> Terminal)
                                Array.append ntsym tsym |> shuffle |> Array.toList
                        ]
                        yield ((getTypeByName<RULE> >>  NonTerminal) nt, rhsSym)
        }

    let genGrammar prodNumber termsNumber rhsRules ruleSize specialSym = [|
        let nterms = getUnionTypes<RULE> () |> sample prodNumber
        let tkns = getUnionTypes<TOKEN> ()
        let terms =
            if specialSym then tkns else (Array.skip 6 tkns)
            |> Array.takeWhile (fun t -> t <> "INVALID")
            |> sample termsNumber
        let rules = genProdParams nterms terms rhsRules ruleSize
        for (nt,prods) in rules do
            for p in prods -> (nt,p)
        |]

    let makeLeftRecursive grammar =
        let i = rng.Next(0, Array.length grammar)
        let lhs, rhs = grammar.[i]
        let newrhs =
            match rhs with
            | (NonTerminal s)::xs -> lhs::xs
            | xs -> lhs::xs
        grammar.[i] <- lhs, newrhs
        grammar

    let parseGrammarString (sgrammar:string) =
        let trim (s:string) = s.Trim([|' '|])
        let split (c:char) (s:string) = s.Split(c)
        let astuple (arr:'a []) = arr.[0], arr.[1]
        let charToRule (c:char) = (string c) |> getTypeByName<RULE> |> NonTerminal
        let strToToken (s:string) = s.ToUpper () |>  getTypeByName<TOKEN> |> Terminal
        let str chs = Seq.fold (fun str x -> str + x.ToString()) "" chs
        let srules = sgrammar |> split '\n' |> Array.filter (fun s -> s.Length <> 0)
        [|
            for srule in srules do
                let slhs, srhs = srule |> split '→' |> Array.map trim |> astuple
                let lhs = getTypeByName<RULE> slhs |> NonTerminal
                yield! [
                    for sprod in (srhs |> Seq.filter (fun c -> c <> ' ') |> str |> split '|' |> Array.map trim) -> lhs, [
                        for c in sprod ->
                            match c with
                            | '(' -> strToToken "LPAR"
                            | ')' -> strToToken "RPAR"
                            | '+' -> strToToken "PLUS"
                            | '-' -> strToToken "MINUS"
                            | '*' -> strToToken "MULT"
                            | '/' -> strToToken "DIV"
                            | '$' -> strToToken "END"
                            | c when int c = 949 -> strToToken "EPS"
                            | c when (System.Char.IsUpper c) -> charToRule c
                            | _ -> strToToken (string c)
                    ]
                ]
        |]
