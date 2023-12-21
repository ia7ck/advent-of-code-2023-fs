open Xunit
open FsUnit.Xunit

type Module =
    | FlipFlop of name: string * destinations: string list
    | Conjunction of name: string * destinations: string list
    | End of name: string

    member self.Name() =
        match self with
        | FlipFlop(name, _) -> name
        | Conjunction(name, _) -> name
        | End name -> name

    member self.Destinations() =
        match self with
        | FlipFlop(_, destinations) -> destinations
        | Conjunction(_, destinations) -> destinations
        | End _ -> []

type Power =
    | On
    | Off

type Pulse =
    | Low
    | High

let part1 (broadcaster: string list, modules: Module list) =
    let endModules =
        let names = modules |> List.map (_.Name()) |> Set.ofList

        modules
        |> List.collect (_.Destinations())
        |> List.filter (fun name -> Set.contains name names |> not)
        |> List.distinct
        |> List.map End

    let modules = modules @ endModules
    let moduleByName = modules |> List.map (fun m -> m.Name(), m) |> Map.ofList

    let pushButton flipFlapPower conjunctionMemory =
        let rec relay
            queue
            (flipFlapPower: Map<string, Power>)
            (conjunctionMemory: Map<string, Map<string, Pulse>>)
            low
            high
            =
            if List.isEmpty queue then
                flipFlapPower, conjunctionMemory, low, high
            else
                let newQueue, newFFPower, newConjMemory =
                    (([], flipFlapPower, conjunctionMemory), queue)
                    ||> List.fold (fun acc (pulse, name) ->
                        let newQueue, newFFPower, newConjMemory = acc

                        match pulse, moduleByName[name] with
                        | Low, End _ -> acc
                        | High, End _ -> acc
                        | Low, FlipFlop(name, destinations) ->
                            let newPower, newPulse =
                                match flipFlapPower[name] with
                                | On -> Off, Low
                                | Off -> On, High

                            newQueue @ (List.map (fun dest -> newPulse, dest) destinations),
                            Map.add name newPower newFFPower,
                            (newConjMemory, destinations)
                            ||> List.fold (fun newConjMemory dest ->
                                match Map.tryFind dest newConjMemory with
                                | None -> newConjMemory
                                | Some mem -> Map.add dest (Map.add name newPulse mem) newConjMemory)
                        | High, FlipFlop _ -> acc
                        | _, Conjunction(name, destinations) ->
                            let allHigh = Map.forall (fun _ p -> p = High) conjunctionMemory[name]
                            let newPulse = if allHigh then Low else High

                            newQueue @ (List.map (fun dest -> newPulse, dest) destinations),
                            newFFPower,
                            (newConjMemory, destinations)
                            ||> List.fold (fun newConjMemory dest ->
                                match Map.tryFind dest newConjMemory with
                                | None -> newConjMemory
                                | Some mem -> Map.add dest (Map.add name newPulse mem) newConjMemory))

                let low' = queue |> List.filter (fun (pulse, _) -> pulse = Low) |> List.length
                let high' = queue |> List.filter (fun (pulse, _) -> pulse = High) |> List.length
                relay newQueue newFFPower newConjMemory (low + low') (high + high')

        let source = List.map (fun name -> Low, name) broadcaster

        let flipFlapPower, conjunctionMemory, low, high =
            relay source flipFlapPower conjunctionMemory 0 0

        flipFlapPower, conjunctionMemory, 1 + low, high

    let flipFlopPower =
        modules
        |> List.choose (fun m ->
            match m with
            | FlipFlop(name, _) -> Some(name, Off)
            | _ -> None)
        |> Map.ofList

    let conjunctionMemory =
        (Map.empty, modules)
        ||> List.fold (fun memory m ->
            let name = m.Name()

            (memory, m.Destinations())
            ||> List.fold (fun memory dest ->
                match moduleByName[dest] with
                | Conjunction(name', _) ->
                    memory
                    |> Map.change name' (fun pulses ->
                        match pulses with
                        | None -> Some(Map [ name, Low ])
                        | Some pulses -> Some(Map.add name Low pulses))
                | _ -> memory))

    let _, _, low, high =
        ((flipFlopPower, conjunctionMemory, 0, 0), [ 1..1000 ])
        ||> List.fold (fun (flipFlopPower, conjunctionMemory, low, high) _ ->
            let flipFlopPower, conjunctionMemory, low', high' =
                pushButton flipFlopPower conjunctionMemory

            flipFlopPower, conjunctionMemory, (low + low'), (high + high'))

    low * high

let parse (input: string) =
    let broadcaster =
        input.Split("\n")
        |> Seq.filter (_.StartsWith("broadcaster"))
        |> Seq.map (fun l -> l.Replace("broadcaster -> ", "").Split(", ") |> Array.toList)
        |> Seq.exactlyOne

    let modules =
        input.Split("\n")
        |> Seq.filter (fun l -> not (l.StartsWith("broadcaster")))
        |> Seq.map (fun l ->
            let name, destinations =
                let l' = l.Replace("%", "").Replace("&", "").Split(" -> ")
                l'[0], l'[1].Split(", ") |> Array.toList

            match l[0] with
            | '%' -> FlipFlop(name = name, destinations = destinations)
            | '&' -> Conjunction(name = name, destinations = destinations)
            | _ -> failwithf $"unimplemented, l = {l}")
        |> Seq.toList

    broadcaster, modules

module Example =
    let inputA =
        "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a"

    let inputB =
        "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output"

    [<Fact>]
    let testPart1A () =
        parse inputA |> part1 |> should equal 32000000

    [<Fact>]
    let testPart1B () =
        parse inputB |> part1 |> should equal 11687500

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let texts = parse input

    part1 texts |> printfn "part1: %d"
    // part2: 手計算で解く。
    // 「rx」は conjunction「vr」の出力から決まる
    // 「vr」は4つの conjunction「pk」「fg」「dk」「fm」の出力から決まる
    // pk, fg, dk, fm がすべて High になるタイミングを求める
    // 実験すると、pk はボタンを押した回数が N 回、2N 回、3N 回、…… のときに High になることがわかる; N≦5000
    // fg, dk, fm も同じで High になる周期がわかる
    // 4つの周期の LCM を答える

    0
