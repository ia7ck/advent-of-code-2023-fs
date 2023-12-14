open Xunit
open FsUnit.Xunit

[<RequireQualifiedAccess>]
type Tile =
    | ``┃``
    | ``━``
    | ``┗``
    | ``┛``
    | ``┓``
    | ``┏``
    | Ground
    | Start

    member self.Dir() =
        match self with
        | Tile.``┃`` -> [ -1, 0; 1, 0 ]
        | Tile.``━`` -> [ 0, -1; 0, 1 ]
        | Tile.``┗`` -> [ -1, 0; 0, 1 ]
        | Tile.``┛`` -> [ 0, -1; -1, 0 ]
        | Tile.``┓`` -> [ 0, -1; 1, 0 ]
        | Tile.``┏`` -> [ 1, 0; 0, 1 ]
        | _ -> []

let connect (grid: Tile[][]) (i, j) (di, dj) =
    let i', j' = i + di, j + dj

    if 0 <= i' && i' < grid.Length && 0 <= j' && j' < grid[i'].Length then
        (grid[i'][j']).Dir()
        |> List.exists (fun (di', dj') -> i = i' + di' && j = j' + dj')
    else
        false

let interpolateStartPipe h w (grid: Tile[][]) =
    let si, sj =
        Seq.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
        |> Seq.find (fun (i, j) -> grid[i][j] = Tile.Start)

    let start =
        [ Tile.``┃``; Tile.``━``; Tile.``┗``; Tile.``┛``; Tile.``┓``; Tile.``┏`` ]
        |> List.filter (fun p -> p.Dir() |> List.forall (connect grid (si, sj)))
        |> List.exactlyOne // exactlyOne ???

    let newGrid =
        Array.init h (fun i -> Array.init w (fun j -> if (i, j) = (si, sj) then start else grid[i][j]))

    si, sj, newGrid

let visitLoop (grid: Tile[][]) =
    let h = grid.Length
    let w = grid[0].Length

    let dist = Array.init h (fun _ -> Array.init w (fun _ -> None))

    // update dist
    let rec bfs (graph: (int * int) list[][]) queue =
        if List.isEmpty queue then
            ()
        else
            let newQueue =
                queue
                |> List.collect (fun (i, j) ->
                    graph[i][j]
                    |> List.filter (fun (i', j') ->
                        if Option.isNone (dist[i'][j']) then
                            dist[i'][j'] <- Some(Option.get (dist[i][j]) + 1)
                            true
                        else
                            false))

            bfs graph newQueue

    let si, sj, grid = interpolateStartPipe h w grid

    let graph =
        Array.init h (fun i ->
            Array.init w (fun j ->
                (grid[i][j]).Dir()
                |> List.filter (connect grid (i, j))
                |> List.map (fun (di, dj) -> i + di, j + dj)))

    dist[si][sj] <- Some 0
    bfs graph [ (si, sj) ]

    h, w, grid, dist

let part1 (grid: Tile[][]) =
    let h, w, _, dist = visitLoop grid

    Seq.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
    |> Seq.map (fun (i, j) -> Option.defaultValue -1 (dist[i][j]))
    |> Seq.max

let part2 (grid: Tile[][]) =
    let h, w, grid, dist = visitLoop grid

    // https://en.wikipedia.org/wiki/Even%E2%80%93odd_rule
    // 右斜め下に半直線を引いて交差を数える
    // 回数が奇数なら内側、偶数なら外側
    let rec cross (i, j) count =
        if i >= h || j >= w then
            count
        else
            let c =
                match dist[i][j] with
                | Some _ ->
                    // このふたつはややこしいが 0 or 2 とみなすのがよさそう
                    if grid[i][j] = Tile.``┗`` || grid[i][j] = Tile.``┓`` then
                        2
                    else
                        1
                | None -> 0

            cross (i + 1, j + 1) (count + c)

    Seq.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
    |> Seq.filter (fun (i, j) -> Option.isNone (dist[i][j]) && (cross (i, j) 0) % 2 = 1)
    |> Seq.length

let parse (input: string) =
    input.Split("\n")
    |> Array.map (fun l ->
        l.Trim()
        |> Seq.map (fun c ->
            match c with
            | '|' -> Tile.``┃``
            | '-' -> Tile.``━``
            | 'L' -> Tile.``┗``
            | 'J' -> Tile.``┛``
            | '7' -> Tile.``┓``
            | 'F' -> Tile.``┏``
            | '.' -> Tile.Ground
            | 'S' -> Tile.Start
            | _ -> failwith $"unimplemented, c = {c}")
        |> Seq.toArray)

module Example =

    [<Fact>]
    let testPart1A () =
        let input =
            ".....
.S-7.
.|.|.
.L-J.
....."

        parse input |> part1 |> should equal 4

    [<Fact>]
    let testPart1B () =
        let input =
            "..F7.
.FJ|.
SJ.L7
|F--J
LJ..."

        parse input |> part1 |> should equal 8

    [<Fact>]
    let testPart2A () =
        let input =
            "...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
..........."

        parse input |> part2 |> should equal 4

    [<Fact>]
    let testPart2B () =
        let input =
            "..........
.S------7.
.|F----7|.
.||....||.
.||....||.
.|L-7F-J|.
.|..||..|.
.L--JL--J.
.........."

        parse input |> part2 |> should equal 4

    [<Fact>]
    let testPart2C () =
        let input =
            ".F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ..."

        parse input |> part2 |> should equal 8

    [<Fact>]
    let testPart2D () =
        let input =
            "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"

        parse input |> part2 |> should equal 10

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    part1 grid |> printfn "part1: %d"
    part2 grid |> printfn "part2: %d"

    0
