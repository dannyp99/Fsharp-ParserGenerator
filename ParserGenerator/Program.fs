// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.Collections.Generic
open System.Linq
open Gitem

// Define a function to construct a message to print
type GrammarSym =
    struct
         val mutable public Sym: string
         val mutable public FsharpType: string
         val mutable public Terminal: bool
         val mutable public Label: string
         val mutable public Precedence: int32

        new(s: string, isTerminal: bool) = {
            Sym = s
            Terminal = isTerminal
            Label = String.Empty
            FsharpType = "String"
            Precedence = 20
        }

        new(gs: GrammarSym) = {
            Sym = gs.Sym
            FsharpType = gs.FsharpType
            Terminal = gs.Terminal
            Label = gs.Label
            Precedence = gs.Precedence
        }

        override this.ToString() =
            this.Sym + ": " + this.Label + " Precedence: " + this.Precedence.ToString()
    end

type GrammarRule =
    struct
        val mutable public Lhs: GrammarSym
        val mutable public Rhs: List<GrammarSym>
        val mutable public Action: string
        val mutable public Precedence: int32
        val mutable public Operation: string

        new(lh: string) = {
            Lhs = new GrammarSym(lh, false)
            Rhs = new List<_>(100)
            Action = String.Empty
            Precedence = 0
            Operation = String.Empty
            
        }
    end

type Grammar =
    struct
        val mutable public Symbols: Dictionary<string, GrammarSym>
        val mutable public Rules: List<GrammarRule>
        val mutable public TopSym: string
        val mutable public Linenum: int32
        val mutable public Nullable: HashSet<string>
        val mutable public First: Dictionary<string, HashSet<string>>
        val mutable public Rulesfor: Dictionary<string, HashSet<int>>
        val mutable public Extras: string
        val mutable public ReSync: string
        val mutable public AbsynType: string

        new(empty: string) = {
            Symbols = new Dictionary<_,_>(100)
            Rules = new List<_>(100)
            TopSym = String.Empty
            Linenum = 0
            Nullable = new HashSet<_>(100)
            First = new Dictionary<_,_>(100)
            Rulesfor = new Dictionary<_,_>(100)
            AbsynType = String.Empty
            ReSync = "EOF"
            Extras = String.Empty
        }

        member this.NonTerminal(s: string): bool =
            not this.Symbols.[s].Terminal

        member this.Terminal(s: string): bool =
            this.Symbols.[s].Terminal

        member this.PrintFirst(): unit = 
            for pair in this.First do
                printfn "First(%s) = " pair.Key
                for item in pair.Value do
                    printfn "%A" item
                printfn ""
            printfn ""

        member public this.ParseStdin(): unit = 
            let mutable line: string = String.Empty
            let mutable atEOF: bool = false

            while (not atEOF) do
                line <- Console.ReadLine()
                this.Linenum <- this.Linenum + 1

                if line = null then
                    atEOF <- true
                else if line.Length > 1 && line.[0] = '!' then
                    this.Extras <- this.Extras + line.Substring(1) + "\n"
                else if line.Length > 1 && not (line.[0] = '#') then 
                    let linelen: int = line.Length
                    let toks: List<string> = line.Split(' ', StringSplitOptions.RemoveEmptyEntries).ToList();
                    let mutable newTerm: GrammarSym = new GrammarSym()
                    match toks.[0] with
                    | "EOF" -> 
                        atEOF <- true

                    | "terminal" ->
                        for i in 0..toks.Count do
                            newTerm <- new GrammarSym(toks.[i], true)
                            this.Symbols.Add(toks.[i], newTerm)

                    | "typedterminal" ->
                        newTerm <- new GrammarSym(toks.[1], true)
                        newTerm.FsharpType <- toks.[2]
                        this.Symbols.Add(toks.[1], newTerm)

                    | "nonterminal" ->
                        newTerm <- new GrammarSym(toks.[1], false)
                        newTerm.FsharpType <- toks.[2]
                        this.Symbols.Add(toks.[1], newTerm)

                    | "nonterminals" ->
                        for i in 1..toks.Count do
                            newTerm <- new GrammarSym(toks.[i], false)
                            newTerm.FsharpType <- this.AbsynType
                            this.Symbols.Add(toks.[i], newTerm)

                    | "topsym" ->
                        this.TopSym <- toks.[1]

                    | "left" | "right" ->
                        let mutable precLevel: int32 = 0
                        if not (System.Int32.TryParse(toks.[2], &precLevel)) then
                            precLevel <- 20
                        if toks.[0] = "right" then
                            precLevel <- -1 * precLevel

                        let mutable gSym: GrammarSym = new GrammarSym()
                        if this.Symbols.TryGetValue(toks.[1], &gSym) then
                            gSym.Precedence <- precLevel

                    | "resync" ->
                        this.ReSync <- toks.[1]

                    | "absyntype" ->
                        this.AbsynType <- toks.[1]

                    | _ -> 
                        if this.NonTerminal(toks.[0]) && toks.[1] = "-->" then
                            let lhsSym: GrammarSym = this.Symbols.[toks.[0]]
                            let rhsSyms: List<GrammarSym> = new List<_>()
                            let mutable maxPrec: int32 = 0
                            let mutable semAction: string = "}"
                            let mutable breaker: bool = true
                            while breaker do
                                let mutable i: int32 = 2
                                if toks.[i] = "{" then
                                    semAction <- String.Join(" ", toks.Skip(i+1).ToList())
                                    breaker <- false
                                i <- i + 1
                                let tokLab: string[] = toks.[i].Split(':')
                                let mutable newSym: GrammarSym = new GrammarSym(this.Symbols.[tokLab.[0]])
                                if tokLab.Length > 1 then
                                    newSym.Label <- tokLab.[1]
                                if Math.Abs(newSym.Precedence) > Math.Abs(maxPrec) then
                                    maxPrec <- newSym.Precedence
                                rhsSyms.Add(newSym)

                            let mutable rule: GrammarRule = new GrammarRule()
                            rule.Lhs <- lhsSym
                            rule.Rhs <-rhsSyms
                            rule.Operation <- String.Empty
                            rule.Action <- semAction
                            rule.Precedence <- maxPrec
                            
                            this.Rules.Add(rule)
                        else 
                            raise (Exception("line format unrecognized"))
                    //pattern match
                //if line
            //while not (atEOF)


            let startnt: GrammarSym = new GrammarSym("START", false)
            let eofTerm: GrammarSym = new GrammarSym("EOF", true)
            this.Symbols.Add("START", startnt)
            this.Symbols.Add("EOF", eofTerm)
            let topGSym: GrammarSym = this.Symbols.[this.TopSym]
            let mutable startRule = new GrammarRule()
            startRule.Lhs <- startnt
            startRule.Rhs <- new List<_>([| topGSym; eofTerm; |])

            this.Rules.Add(startRule)

        member public this.ComputFirst(): unit =
            //ComputeFirst()
            let FIRST = new Dictionary<string, HashSet<string>>()
            let mutable changed = true
            while changed do
                changed <- false
                for rule in this.Rules do
                    let nt: string = rule.Lhs.Sym
                    if not (FIRST.ContainsKey(nt)) then
                        changed <- true
                        FIRST.Add(nt, new HashSet<string>(100))
                    let FirstNt: HashSet<string> = FIRST.[nt]
                    let mutable i = 0
                    let mutable isNullable = true
                    while (i < rule.Rhs.Count && isNullable) do
                        let gs: GrammarSym = rule.Rhs.[i]
                        if (gs.Terminal) then
                            changed <- FirstNt.Add(gs.Sym) || changed
                            isNullable <- false
                        
                        else if not (gs.Sym = nt) then
                            let mutable firstGs = new HashSet<string>()
                            if FIRST.TryGetValue(gs.Sym, &firstGs) then
                                for sym in firstGs do
                                    changed <- FirstNt.Add(sym) || changed

                        if (gs.Terminal || not(this.Nullable.Contains(gs.Sym))) then
                            isNullable <- false
                        i <- i + 1
                    //while Rhs is Nullable
                //For each rule
            //while changed
            for nt in FIRST.Keys do
                let rcell = FIRST.[nt]
                this.First.Add(nt, rcell)

        member public this.ComputeNullable(): unit =
            let mutable changed = true
            while changed do
                let mutable rulei = 0
                changed <- false
                for rule in this.Rules do
                    let mutable addOrNot = true
                    for g in rule.Rhs do
                        if g.Terminal || not (this.Nullable.Contains(g.Sym)) then addOrNot <- false
                    if addOrNot then changed <- this.Nullable.Add(rule.Lhs.Sym) || changed
                    if not (this.Rulesfor.ContainsKey(rule.Lhs.Sym)) then
                        this.Rulesfor.Add(rule.Lhs.Sym, new HashSet<int32>(100))
                    let ruleSet = this.Rulesfor.[rule.Lhs.Sym]
                    ruleSet.Add(rulei) |> ignore //discard the value and remove the warning
                    rulei <- rulei + 1
                // foreach rule
            //while
        
        member public this.FirstSequence (seq: List<GrammarSym>, la: string): HashSet<string> = 
            let mutable nullable: bool = true
            let firsts = new HashSet<string>();
            for sym in seq do
                if sym.Terminal then
                    nullable <- false
                    firsts.Add(sym.Sym) |> ignore
                else 
                    for t in this.First.[sym.Sym] do
                        firsts.Add(t) |> ignore
                    nullable <- this.Nullable.Contains(sym.Sym)
                //missing if statement that breaks for loop if not (nullable)
            if nullable then
                firsts.Add(la) |> ignore
            firsts

        member public this.StateClosure (states: HashSet<Gitem>): unit =
            let closed = new Stack<Gitem>(states)
            while (closed.Count > 0) do
                let state = closed.Pop()
                if not (state = null) then
                    let ri = state.Ri
                    let pi = state.Pi
                    let la = state.La
                    let rule = this.Rules.[ri]
                    let lhs = rule.Lhs.Sym
                    if pi < rule.Rhs.Count && not (rule.Rhs.[pi].Terminal) then
                        let nti = rule.Rhs.[pi]
                        let lookaheads = this.FirstSequence(rule.Rhs.Skip(pi+1).ToList(), la)
                        for rulent in this.Rulesfor.[nti.Sym] do
                            for lafollow in lookaheads do
                                let newItem = new Gitem(rulent, 0, lafollow)
                                if not (states.Contains(newItem)) then
                                    states.Add(newItem)
                                    closed.Push(newItem)
                            
                            //foreach lookahead
                        //foreach Rule of nonterminals
                    //if
                //not null state
            //while
        //StateClosure
    end

[<EntryPoint>]
let main argv =
    let message = "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code