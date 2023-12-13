open Xunit
open FsUnit.Xunit

type Reflection =
    | Row of int
    | Column of int

    member self.Value() =
        match self with
        | Row i -> i * 100
        | Column j -> j

let reflection pattern =
    let pattern = pattern |> Seq.map List.ofSeq |> Seq.toList

    let refRows pattern =
        let h = List.length pattern

        [ 1 .. (h - 1) ]
        |> List.filter (fun i ->
            let len = min i (h - i)

            pattern[(i - len) .. (i - 1)]
            |> List.zip (List.rev pattern[i .. (i + len - 1)])
            |> List.forall (fun (r, r') -> r = r'))

    let refColumns pattern = pattern |> List.transpose |> refRows

    (List.map Row (refRows pattern)) @ (List.map Column (refColumns pattern))


let part1 (patterns: char list list list) =
    patterns
    |> List.sumBy (fun pattern -> reflection pattern |> List.exactlyOne |> (_.Value()))

let part2 (patterns: char list list list) =
    patterns
    |> List.sumBy (fun pattern ->
        let originalRef = reflection pattern |> List.exactlyOne

        let pattern =
            pattern
            |> List.map (fun row -> row |> List.map ((=) '.') |> List.toArray)
            |> List.toArray

        let h, w = pattern.Length, pattern[0].Length

        List.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
        |> List.choose (fun (i, j) ->
            pattern[i][j] <- not (pattern[i][j])
            let otherRefs = reflection pattern |> List.filter ((<>) originalRef)
            pattern[i][j] <- not (pattern[i][j]) // もどす
            List.tryExactlyOne otherRefs)
        |> List.distinct
        |> List.exactlyOne
        |> (_.Value()))

let parse (input: string) =
    input.Split("\n\n")
    |> Seq.map (fun pattern -> pattern.Split("\n") |> Seq.map List.ofSeq |> Seq.toList)
    |> Seq.toList

module Example =
    let input =
        "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 405

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 400

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let patterns = parse input

    part1 patterns |> printfn "part1: %d"
    part2 patterns |> printfn "part2: %d"

    0
