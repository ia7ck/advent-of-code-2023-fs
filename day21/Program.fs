open Xunit
open FsUnit.Xunit

let count (n: int) (grid: char list list) =
    let h, w = grid.Length, grid[0].Length

    let step pos =
        pos
        |> List.collect (fun (i, j) ->
            [ -1, 0; 0, -1; 1, 0; 0, 1 ]
            |> List.choose (fun (di, dj) ->
                let ni, nj = i + di, j + dj

                if grid[(ni % h + h) % h][(nj % w + w) % w] = '#' then
                    None
                else
                    Some(ni, nj)))
        |> List.distinct

    let si, sj =
        List.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
        |> List.find (fun (i, j) -> grid[i][j] = 'S')

    ([ si, sj ], [ 1..n ]) ||> List.fold (fun pos _ -> step pos) |> List.length



let part1 (n: int) (grid: char list list) =
    let h, w = grid.Length, grid[0].Length
    assert (n <= h)
    assert (n <= w)

    count n grid

let part2 (grid: char list list) =
    let h, w = grid.Length, grid[0].Length
    assert (h = 131)
    assert (w = 131)

    let a = 65
    let d = 131
    let k = 202300
    assert (a + d * k = 26501365)

    let y0 = int64 (count a grid)
    let y1 = int64 (count (a + d) grid)
    let y2 = int64 (count (a + d * 2) grid)
    let z = (y2 - y1) - (y1 - y0)
    // assert ((y3 - y2) - (y2 - y1) = z)
    // ⇒ y3 = z + 2 * y2 - y1
    // assert ((y4 - y3) - (y3 - y2) = (y3 - y2) - (y2 - y1) = z)
    // ⇒ y4 = z + 2 * y3 - y2

    let _, yk =
        ((y1, y2), [ 3..k ]) ||> List.fold (fun (y'', y') _ -> y', z + 2L * y' - y'')

    yk

let parse (input: string) =
    input.Split("\n") |> Seq.map List.ofSeq |> Seq.toList

module Example =
    let input =
        "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."

    [<Fact>]
    let testPart1 () =
        parse input |> part1 6 |> should equal 16

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    part1 64 grid |> printfn "part1: %d"
    part2 grid |> printfn "part2: %d"

    0
