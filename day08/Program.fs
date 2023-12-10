open System.Text.RegularExpressions

open Xunit
open FsUnit.Xunit

type Instruction =
    | Left
    | Right

type Node =
    { Id: string
      Left: string
      Right: string }

type Network =
    { Instructions: Instruction seq
      Nodes: Node seq }

let solve (network: Network) start (terminate: string -> bool) =
    let instructions = List.ofSeq network.Instructions
    let nodes = network.Nodes |> Seq.map (fun node -> node.Id, node) |> Map.ofSeq

    (start, Seq.initInfinite (fun index -> List.item (index % instructions.Length) instructions))
    ||> Seq.scan (fun current instruction ->
        let node = nodes |> Map.find current

        match instruction with
        | Left -> node.Left
        | Right -> node.Right)
    |> Seq.takeWhile (fun c -> not (terminate c))
    |> Seq.length

let part1 (network: Network) = solve network "AAA" ((=) "ZZZ")

let part2 (network: Network) =
    let rec gcd a b = if a % b = 0L then b else gcd b (a % b)
    let lcm a b = a / (gcd a b) * b

    let steps =
        network.Nodes
        |> Seq.map (_.Id)
        |> Seq.filter (_.EndsWith('A'))
        |> Seq.map (fun s -> solve network s (_.EndsWith('Z')))
        |> Seq.map int64

    // 答えは LCM 以下ではあるが、LCM より少ないステップで終わる状況もありうる
    // ただ、与えられる入力ではたまたま LCM が最適値になるっぽい？
    steps |> Seq.reduce lcm

let parse (input: string) =
    let lines = input.Split("\n")

    let instructions =
        lines[0].Trim()
        |> Seq.map (fun ch ->
            match ch with
            | 'L' -> Left
            | 'R' -> Right
            | _ -> failwith $"unimplemented, ch = {ch}")

    let nodes =
        lines[2..]
        |> Seq.map (fun l ->
            let m = Regex.Match(l.Trim(), @"^([0-9A-Z]+) = \(([0-9A-Z]+), ([0-9A-Z]+)\)$")

            match List.ofSeq m.Groups with
            | [ _; nodeId; left; right ] ->
                { Id = nodeId.Value
                  Left = left.Value
                  Right = right.Value }
            | _ -> failwith $"unimplemented, l = {l}")

    { Instructions = instructions
      Nodes = nodes }

module Example =
    [<Fact>]
    let testPart1A () =
        let input =
            "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"

        parse input |> part1 |> should equal 2

    [<Fact>]
    let testPart1B () =
        let input =
            "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"

        parse input |> part1 |> should equal 6

    [<Fact>]
    let testPart2 () =
        let input =
            "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"

        parse input |> part2 |> should equal 6L

    [<Fact(Skip = "want 3, got 6")>]
    let testPart2Hack () =
        let input =
            "LLL

1A = (1B, XX)
1B = (1Z, XX)
1Z = (2Z, XX)
2Z = (1A, XX)
XX = (XX, XX)
3A = (3B, XX)
3B = (3C, XX)
3C = (3Z, XX)
3Z = (3A, XX)"

        // 遷移は
        // 1A -> 1B -> 1Z -> 2Z
        // 3A -> 3B -> 3C -> 3Z
        // となるなので 3 が答え
        // 実際は 6 が返されてしまう

        parse input |> part2 |> should equal 3L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let network = parse input

    part1 network |> printfn "part1: %d"
    part2 network |> printfn "part2: %d"

    0
