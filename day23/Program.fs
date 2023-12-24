open System.Collections.Generic

open Xunit
open FsUnit.Xunit

let part1 (grid: char list list) =
    let h, w = grid.Length, grid[0].Length

    let memo = Dictionary()

    let rec longestStep (ti, tj) (pi, pj) =
        assert (grid[ti][tj] <> '#')

        if (ti, tj) = (0, 1) then
            0
        else
            match memo.TryGetValue((ti, tj)) with
            | true, step -> step
            | false, _ ->
                let upper =
                    if
                        ti >= 1
                        && (grid[ti - 1][tj] = '.' || grid[ti - 1][tj] = 'v')
                        && (ti - 1, tj) <> (pi, pj)
                    then
                        Some(longestStep (ti - 1, tj) (ti, tj))
                    else
                        None

                let lower =
                    if
                        ti + 1 < h
                        && (grid[ti + 1][tj] = '.' || grid[ti + 1][tj] = '^')
                        && (ti + 1, tj) <> (pi, pj)
                    then
                        Some(longestStep (ti + 1, tj) (ti, tj))
                    else
                        None

                let left =
                    if
                        tj >= 1
                        && (grid[ti][tj - 1] = '.' || grid[ti][tj - 1] = '>')
                        && (ti, tj - 1) <> (pi, pj)
                    then
                        Some(longestStep (ti, tj - 1) (ti, tj))
                    else
                        None

                let right =
                    if
                        tj + 1 < w
                        && (grid[ti][tj + 1] = '.' || grid[ti][tj + 1] = '<')
                        && (ti, tj + 1) <> (pi, pj)
                    then
                        Some(longestStep (ti, tj + 1) (ti, tj))
                    else
                        None

                let step = [ upper; lower; left; right ] |> List.reduce max |> Option.get |> (+) 1
                memo.Add((ti, tj), step)
                step

    assert (grid[h - 1][w - 2] = '.')
    longestStep (h - 1, w - 2) (-1, -1)

let part2 (grid: char list list) =
    let h, w = grid.Length, grid[0].Length

    let isInside (i, j) = 0 <= i && i < h && 0 <= j && j < w

    let start = 0, 1
    let goal = h - 1, w - 2

    let dirs = [ (-1, 0); (0, -1); (1, 0); (0, 1) ]

    let cross =
        List.allPairs [ 0 .. (h - 1) ] [ 0 .. (w - 1) ]
        |> List.filter (fun (i, j) ->
            let adj =
                dirs
                |> List.filter (fun (di, dj) ->
                    let ni, nj = i + di, j + dj
                    isInside (ni, nj) && grid[ni][nj] <> '#')

            grid[i][j] <> '#' && List.length adj >= 3)

    let cross = start :: goal :: cross

    let rec visitCross (route: (int * int) list) =
        assert (List.length route >= 2)
        let i, j = List.head route

        if List.contains (i, j) cross then
            route
        else
            let ni, nj =
                dirs
                |> List.choose (fun (di, dj) ->
                    let ni, nj = i + di, j + dj

                    if isInside (ni, nj) && grid[ni][nj] <> '#' && not (List.contains (ni, nj) route) then
                        Some(ni, nj)
                    else
                        None)
                |> List.exactlyOne

            visitCross ((ni, nj) :: route)


    let edges =
        cross
        |> List.collect (fun (si, sj) ->
            dirs
            |> List.choose (fun (di, dj) ->
                let ni, nj = si + di, sj + dj

                if isInside (ni, nj) && grid[ni][nj] <> '#' then
                    let route = visitCross [ (ni, nj); (si, sj) ]
                    let ti, tj = List.head route
                    Some((si, sj), (ti, tj), List.length route - 2)
                else
                    None))
        |> List.distinctBy (fun (s, t, w) -> min s t, max s t, w)


    let adjacent (i, j) =
        edges
        |> List.choose (fun (s, t, _) ->
            if s = (i, j) then Some t
            elif t = (i, j) then Some s
            else None)

    let edgeWeight (i, j) (i', j') =
        edges
        |> List.choose (fun (s, t, w) ->
            if (s = (i, j) && t = (i', j')) || (s = (i', j') && t = (i, j)) then
                Some(w)
            else
                None)
        |> List.exactlyOne

    let rec longestStep (routes: (int * int) list list) (acc: int) =
        if List.isEmpty routes then
            acc
        else
            let newRoutes, newAcc =
                (([], acc), routes)
                ||> List.fold (fun (newRoutes, newAcc) route ->
                    assert (List.length route >= 1)
                    let i, j = List.head route

                    if (i, j) = goal then
                        let weight = route |> List.pairwise |> List.sumBy (fun (s, t) -> edgeWeight s t)
                        newRoutes, max newAcc (List.length route + weight - 1)
                    else
                        let adj =
                            adjacent (i, j)
                            |> List.filter (fun (i', j') -> not (List.contains (i', j') route))

                        List.map (fun t -> t :: route) adj @ newRoutes, newAcc)

            longestStep newRoutes newAcc

    longestStep [ [ start ] ] 0

let parse (input: string) =
    input.Split("\n") |> Seq.map (_.TrimEnd()) |> Seq.map List.ofSeq |> Seq.toList

module Example =
    let input =
        "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 94


    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 154

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let grid = parse input

    part1 grid |> printfn "part1: %d"
    part2 grid |> printfn "part2: %d"

    0
