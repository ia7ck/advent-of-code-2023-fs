open Xunit
open FsUnit.Xunit
open System.Collections.Generic

type Dir =
    | U
    | L
    | D
    | R

    member self.Delta() =
        match self with
        | U -> -1, 0
        | L -> 0, -1
        | D -> 1, 0
        | R -> 0, 1

let dijkstra (grid: int list list) (si, sj) minStep maxStep =
    let h, w = grid.Length, grid[0].Length

    let validSteps (i, j) (dir: Dir) =
        (0, (i, j), 0)
        |> List.unfold (fun (weight, (i, j), step) ->
            if step > maxStep || i < 0 || i >= h || j < 0 || j >= w then
                None
            else
                let di, dj = dir.Delta()
                let weight' = weight + grid[i][j]
                let i', j' = i + di, j + dj
                let step' = step + 1
                Some(((i, j), weight), (weight', (i', j'), step')))
        |> List.map (fun ((i, j), weight) -> (i, j), weight + grid[i][j])
        |> (fun steps -> steps[minStep..])

    let next (i, j) dir =
        let next dir =
            validSteps (i, j) dir
            |> List.map (fun ((i', j'), w) ->
                // ややこしいが、始点 grid[i][j] の寄与が二重にならないように引く
                ((i', j'), w - grid[i][j], dir))

        match dir with
        | U
        | D -> (next L) @ (next R)
        | L
        | R -> (next U) @ (next D)

    let cost = Dictionary<int * int * Dir, int>() // (i, j, dir), cost
    let queue = PriorityQueue<int * int * Dir, int>()

    for dir in [ U; L; D; R ] do
        let key = (si, sj, dir)
        cost.Add(key, 0)
        queue.Enqueue(key, 0)

    let rec loop () =
        match queue.TryDequeue() with
        | false, _, _ -> ()
        | true, (i, j, dir), c ->
            for (i', j'), w, dir' in next (i, j) dir do
                let key = (i', j', dir')
                let value = c + w

                match cost.TryGetValue(key) with
                | true, v when v <= value -> ()
                | _ ->
                    // false, _
                    // true, v when v > value
                    cost[key] <- value // overwrite
                    queue.Enqueue(key, value)

            loop ()

    loop ()
    cost

let solve (grid: int list list) minStep maxStep =
    let h, w = grid.Length, grid[0].Length

    dijkstra grid (0, 0) minStep maxStep
    |> Seq.filter (fun (KeyValue((i, j, _), _)) -> (i, j) = (h - 1, w - 1))
    |> Seq.map (fun (KeyValue(_, v)) -> v)
    |> Seq.min

let part1 (grid: int list list) = solve grid 1 3

let part2 (grid: int list list) = solve grid 4 10

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun l -> l.TrimEnd() |> Seq.map (fun ch -> int ch - int '0') |> Seq.toList)
    |> Seq.toList

module Example =
    let input =
        "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 102


    [<Fact>]
    let testPart2A () = parse input |> part2 |> should equal 94

    [<Fact>]
    let testPart2B () =
        let input =
            "111111111111
999999999991
999999999991
999999999991
999999999991"

        parse input |> part2 |> should equal 71

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    part1 grid |> printfn "part1: %d"
    part2 grid |> printfn "part2: %d"

    0
