open Xunit
open FsUnit.Xunit

let tiltLeft row =
    let rec tiltLeft row roundedRocks emptySpaces acc =
        match row with
        | [] -> emptySpaces @ roundedRocks @ acc |> List.rev
        | '#' :: t -> tiltLeft t [] [] ('#' :: (emptySpaces @ roundedRocks @ acc))
        | 'O' :: t -> tiltLeft t ('O' :: roundedRocks) emptySpaces acc
        | '.' :: t -> tiltLeft t roundedRocks ('.' :: emptySpaces) acc
        | _ -> failwithf $"unimplemented, row = %A{row}"

    tiltLeft row [] [] []

let tiltNorth platform =
    platform |> List.transpose |> List.map tiltLeft |> List.transpose

let tiltWest platform = platform |> List.map tiltLeft

let tiltSouth platform =
    platform |> List.rev |> tiltNorth |> List.rev

let tiltEast platform =
    platform |> List.map List.rev |> tiltWest |> List.map List.rev

let totalLoad platform =
    platform
    |> List.transpose
    |> List.sumBy (fun row ->
        row
        |> List.indexed
        |> List.sumBy (fun (i, c) -> if c = 'O' then row.Length - i else 0))

let part1 (platform: char list list) = platform |> tiltNorth |> totalLoad

let part2 (platform: char list list) =
    let tilt platform =
        platform |> tiltNorth |> tiltWest |> tiltSouth |> tiltEast

    let rec totalLoad' platform cycle (memo: Map<char list list, int>) naive =
        if cycle = 0 then
            totalLoad platform
        elif naive || not (Map.containsKey platform memo) then
            totalLoad' (tilt platform) (cycle - 1) (Map.add platform cycle memo) naive
        else
            let cycle' = Map.find platform memo
            let length = cycle' - cycle
            totalLoad' platform (cycle % length) memo true

    totalLoad' platform 1000000000 Map.empty false

let parse (input: string) =
    input.Split("\n") |> Seq.map List.ofSeq |> Seq.toList

module Example =
    let input =
        "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 136

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 64

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let platform = parse input

    part1 platform |> printfn "part1: %d"
    part2 platform |> printfn "part2: %d"

    0
