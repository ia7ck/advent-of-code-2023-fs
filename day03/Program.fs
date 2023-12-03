open Xunit
open FsUnit.Xunit

type Number =
    { Value: int
      Positions: (int * int) seq }

let collectNumbers (schematic: string array) =
    schematic
    |> Seq.indexed
    |> Seq.collect (fun (i, s) ->
        let rec collectNumbers j =
            if j >= s.Length then
                []
            else
                match s[j] with
                | ch when System.Char.IsDigit(ch) ->
                    let v =
                        s[j..]
                        |> Seq.takeWhile (fun ch -> System.Char.IsDigit(ch))
                        |> System.String.Concat

                    let positions = Seq.zip (Seq.initInfinite (fun _ -> i)) [ j .. (j + v.Length - 1) ]

                    { Value = int v; Positions = positions } :: collectNumbers (j + v.Length)
                | _ -> collectNumbers (j + 1)

        collectNumbers 0)

let dirs8 = [ (-1, 0); (-1, -1); (0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1) ]

let part1 (schematic: string array) =
    let isPartNumber (n: Number) =
        Seq.allPairs n.Positions dirs8
        |> Seq.exists (fun ((i, j), (di, dj)) ->
            let i' = i + di
            let j' = j + dj

            0 <= i'
            && i' < schematic.Length
            && 0 <= j'
            && j' < schematic[i'].Length
            && System.Char.IsDigit(schematic[i'][j']) = false
            && schematic[i'][j'] <> '.')

    collectNumbers schematic
    |> Seq.filter isPartNumber
    |> Seq.sumBy (fun n -> n.Value)

let part2 (schematic: string array) =
    let numbers = collectNumbers schematic

    let adjacentNumbers i j =
        numbers
        |> Seq.filter (fun n ->
            n.Positions
            |> Seq.exists (fun (ni, nj) -> dirs8 |> Seq.exists (fun (di, dj) -> i + di = ni && j + dj = nj)))

    let gearPositions =
        [ 0 .. (schematic.Length - 1) ]
        |> Seq.collect (fun i ->
            [ 0 .. (schematic[i].Length - 1) ]
            |> Seq.choose (fun j -> if schematic[i][j] = '*' then Some((i, j)) else None))

    gearPositions
    |> Seq.sumBy (fun (i, j) ->
        match List.ofSeq (adjacentNumbers i j) with
        | [ n1; n2 ] -> n1.Value * n2.Value
        | _ -> 0)

let parse (input: string) = input.Split("\n")

module Example =
    let input =
        "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 4361

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 467835

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let schematic = parse input

    part1 schematic |> printfn "part1: %d"
    part2 schematic |> printfn "part2: %d"

    0
