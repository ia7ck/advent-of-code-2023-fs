open Xunit
open FsUnit.Xunit

let part1 (histories: int seq seq) =
    let rec predictNext (history: int seq) =
        if Seq.forall ((=) 0) history then
            0
        else
            let diff = history |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)
            let p = predictNext diff
            (Seq.last history) + p

    histories |> Seq.sumBy predictNext

let part2 (histories: int seq seq) =
    let rec predictBack (history: int seq) =
        if Seq.forall ((=) 0) history then
            0
        else
            let diff = history |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)
            let p = predictBack diff
            (Seq.head history) - p

    histories |> Seq.sumBy predictBack

let parse (input: string) =
    input.Split("\n") |> Seq.map (fun l -> l.Split(' ') |> Seq.map int)

module Example =
    let input =
        "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 114

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 2

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let histories = parse input

    part1 histories |> printfn "part1: %d"
    part2 histories |> printfn "part2: %d"

    0
