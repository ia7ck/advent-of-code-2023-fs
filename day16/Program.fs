open Xunit
open FsUnit.Xunit

[<RequireQualifiedAccess>]
type Cell =
    | Empty
    | MirrorUp // '/'
    | MirrorDown // '\'
    | ``|``
    | ``-``

type Dir =
    | U
    | L
    | D
    | R

    member self.Next(i, j) =
        match self with
        | U -> (i - 1, j)
        | L -> (i, j - 1)
        | D -> (i + 1, j)
        | R -> (i, j + 1)

let solve (grid: Cell list list) (si, sj) dir =
    let h, w = grid.Length, grid[0].Length

    let rec dfs (i, j) (dir: Dir) visited =
        if i < 0 || i >= h || j < 0 || j >= w then
            visited
        else if Set.contains (i, j, dir) visited then
            visited
        else
            let newVisited = Set.add (i, j, dir) visited

            match grid[i][j] with
            | Cell.Empty -> dfs (dir.Next(i, j)) dir newVisited
            | Cell.MirrorUp ->
                let newDir =
                    match dir with
                    | U -> R
                    | L -> D
                    | D -> L
                    | R -> U

                dfs (newDir.Next(i, j)) newDir newVisited
            | Cell.MirrorDown ->
                let newDir =
                    match dir with
                    | U -> L
                    | L -> U
                    | D -> R
                    | R -> D

                dfs (newDir.Next(i, j)) newDir newVisited
            | Cell.``|`` ->
                match dir with
                | U
                | D -> dfs (dir.Next(i, j)) dir newVisited
                | L
                | R ->
                    let newVisited = dfs (U.Next(i, j)) U newVisited
                    dfs (D.Next(i, j)) D newVisited
            | Cell.``-`` ->
                match dir with
                | L
                | R -> dfs (dir.Next(i, j)) dir newVisited
                | U
                | D ->
                    let newVisited = dfs (L.Next(i, j)) L newVisited
                    dfs (R.Next(i, j)) R newVisited

    let visited = dfs (si, sj) dir Set.empty
    visited |> Seq.map (fun (i, j, _) -> i, j) |> Seq.distinct |> Seq.length


let part1 (grid: Cell list list) = solve grid (0, 0) R

let part2 (grid: Cell list list) =
    let h, w = grid.Length, grid[0].Length

    let edges =
        [ for j in 0 .. (w - 1) -> 0, j ]
        @ [ for i in 0 .. (h - 1) -> i, w - 1 ]
        @ [ for j in 0 .. (w - 1) -> h - 1, j ]
        @ [ for i in 0 .. (h - 1) -> i, 0 ]

    Seq.allPairs edges [ U; L; D; R ]
    |> Seq.map (fun ((si, sj), dir) ->
        // 100×100×4 回程度はしる
        solve grid (si, sj) dir)
    |> Seq.max


let parse (input: string) =
    input.Split('\n')
    |> Seq.map (fun l ->
        l.TrimEnd()
        |> Seq.map (function
            | '.' -> Cell.Empty
            | '/' -> Cell.MirrorUp
            | '\\' -> Cell.MirrorDown
            | '|' -> Cell.``|``
            | '-' -> Cell.``-``
            | c -> failwithf $"unimplemented, c = {c}")
        |> Seq.toList)
    |> Seq.toList

module Example =
    let input =
        """.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|...."""

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 46

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 51

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    part1 grid |> printfn "part1: %d"
    part2 grid |> printfn "part2: %d"

    0
