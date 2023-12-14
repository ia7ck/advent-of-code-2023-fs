open Xunit
open FsUnit.Xunit

type Race = { Time: int64; Distance: int64 }

let solve (races: Race seq) =
    races
    |> Seq.map (fun r ->
        [ 0L .. r.Time ]
        |> Seq.filter (fun t -> t * (r.Time - t) > r.Distance)
        |> Seq.length)
    |> Seq.reduce (*)

let part1 (races: Race seq) = solve races

let part2 (races: Race seq) =
    let (time, distance) =
        ((0L, 0L), races)
        ||> Seq.fold (fun (time, distance) r ->
            let concat (x: int64) (y: int64) = x * (pown 10L (string y).Length) + y
            (concat time r.Time, concat distance r.Distance))

    solve [ { Time = time; Distance = distance } ]

let parse (input: string) =
    match input.Split("\n") with
    | [| time; distance |] ->
        let time =
            time
                .Replace("Time:", "")
                .Trim()
                .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int

        let distance =
            distance
                .Replace("Distance:", "")
                .Trim()
                .Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map int

        Seq.zip time distance |> Seq.map (fun (t, d) -> { Time = t; Distance = d })
    | _ -> failwith $"unimplemented, input = {input}"

module Example =
    let input =
        "Time:      7  15   30
Distance:  9  40  200"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 288

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 71503

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let races = parse input

    part1 races |> printfn "part1: %d"
    part2 races |> printfn "part2: %d"

    0
