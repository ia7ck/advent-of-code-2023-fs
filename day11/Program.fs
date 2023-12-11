open Xunit
open FsUnit.Xunit

let double (image: char list list) =
    let rec doubleRows (image: char list list) =
        image
        |> List.collect (fun row -> if List.contains '#' row then [ row ] else [ row; row ])

    image |> doubleRows |> List.transpose |> doubleRows |> List.transpose

let galaxies (image: char list list) =
    Seq.allPairs [ 0 .. (image.Length - 1) ] [ 0 .. (image[0].Length - 1) ]
    |> Seq.filter (fun (i, j) -> image[i][j] = '#')

let manhattan (i, j) (i', j') = abs (i - i') + abs (j - j')

let part1 (image: char list list) =
    let g = galaxies (double image)

    Seq.allPairs g g
    |> Seq.sumBy (fun ((i, j), (i', j')) -> manhattan (i, j) (i', j'))
    |> (/)
    <| 2

let part2 (repeat: int64) (image: char list list) =
    let g = galaxies image
    let g2 = galaxies (double image)

    Seq.zip (Seq.allPairs g g) (Seq.allPairs g2 g2)
    |> Seq.sumBy (fun (((i, j), (i', j')), ((i2, j2), (i2', j2'))) ->
        let d = manhattan (i, j) (i', j')
        int64 d + int64 (manhattan (i2, j2) (i2', j2') - d) * (repeat - 1L))
    |> (/)
    <| 2L

let parse (input: string) =
    input.Split("\n") |> Seq.map (List.ofSeq) |> Seq.toList

module Example =
    let input =
        "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 374


    [<Fact>]
    let testPart2 () =
        parse input |> part2 2L |> should equal 374L
        parse input |> part2 10L |> should equal 1030L
        parse input |> part2 100L |> should equal 8410L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let image = parse input

    part1 image |> printfn "part1: %d"
    part2 1000000 image |> printfn "part2: %d"

    0
