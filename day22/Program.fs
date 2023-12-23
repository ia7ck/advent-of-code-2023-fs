open Xunit
open FsUnit.Xunit

type Brick =
    | X of id: int * minX: int * maxX: int * y: int * z: int
    | Y of id: int * x: int * minY: int * maxY: int * z: int
    | Z of id: int * x: int * y: int * minZ: int * maxZ: int

let fallDown (bricks: Brick list) =
    let intersect (l, r) (l', r') =
        assert (l <= r)
        assert (l' <= r')
        not (r < l' || r' < l)

    let bricks =
        List.sortBy
            (fun b ->
                match b with
                | X(_, _, _, _, z) -> z
                | Y(_, _, _, _, z) -> z
                | Z(_, _, _, minZ, _) -> minZ)
            bricks

    let newBricks =
        ([], bricks)
        ||> List.fold (fun newBricks b ->
            match b with
            | X(id, minX, maxX, y, z) ->
                let newZ =
                    newBricks
                    |> List.choose (fun b' ->
                        match b' with
                        | X(_, minX', maxX', y', z') when intersect (minX, maxX) (minX', maxX') && y = y' ->
                            Some(z' + 1)
                        | Y(_, x', minY', maxY', z') when minX <= x' && x' <= maxX && minY' <= y && y <= maxY' ->
                            Some(z' + 1)
                        | Z(_, x', y', _, maxZ') when minX <= x' && x' <= maxX && y = y' -> Some(maxZ' + 1)
                        | _ -> None)
                    |> List.sortDescending
                    |> List.tryHead
                    |> Option.defaultValue 1

                assert (newZ <= z)
                X(id = id, minX = minX, maxX = maxX, y = y, z = newZ) :: newBricks
            | Y(id, x, minY, maxY, z) ->
                let newZ =
                    newBricks
                    |> List.choose (fun b' ->
                        match b' with
                        | X(_, minX', maxX', y', z') when minX' <= x && x <= maxX' && minY <= y' && y' <= maxY ->
                            Some(z' + 1)
                        | Y(_, x', minY', maxY', z') when x = x' && intersect (minY, maxY) (minY', maxY') ->
                            Some(z' + 1)
                        | Z(_, x', y', _, maxZ') when x = x' && minY <= y' && y' <= maxY -> Some(maxZ' + 1)
                        | _ -> None)
                    |> List.sortDescending
                    |> List.tryHead
                    |> Option.defaultValue 1

                assert (newZ <= z)
                Y(id = id, x = x, minY = minY, maxY = maxY, z = newZ) :: newBricks
            | Z(id, x, y, minZ, maxZ) ->
                let newMinZ =
                    newBricks
                    |> List.choose (fun b' ->
                        match b' with
                        | X(_, minX', maxX', y', z') when minX' <= x && x <= maxX' && y = y' -> Some(z' + 1)
                        | Y(_, x', minY', maxY', z') when x = x' && minY' <= y && y <= maxY' -> Some(z' + 1)
                        | Z(_, x', y', _, maxZ') when x = x' && y = y' -> Some(maxZ' + 1)
                        | _ -> None)
                    |> List.sortDescending
                    |> List.tryHead
                    |> Option.defaultValue 1

                assert (newMinZ <= minZ)

                Z(id = id, x = x, y = y, minZ = newMinZ, maxZ = maxZ - (minZ - newMinZ))
                :: newBricks)

    let update = set bricks <> set newBricks
    if update then Some newBricks else None

let part1 (bricks: Brick list) =
    // settle
    let bricks = fallDown bricks |> Option.get

    let disintegrate =
        bricks
        |> List.filter (fun b ->
            let bricks' = List.filter ((<>) b) bricks
            let newBricks' = fallDown bricks'
            Option.isSome newBricks')

    List.length bricks - List.length disintegrate

let part2 (bricks: Brick list) =
    // settle
    let bricks = fallDown bricks |> Option.get

    bricks
    |> List.sumBy (fun b ->
        let bricks' = List.filter ((<>) b) bricks
        let newBricks' = fallDown bricks'

        match newBricks' with
        | None -> 0
        | Some newBricks' -> List.length bricks' - Set.count (Set.intersect (set bricks') (set newBricks')))


let parse (input: string) =
    input.Split("\n")
    |> Seq.indexed
    |> Seq.map (fun (i, l) ->
        let l = l.TrimEnd().Split('~')
        let p, q = l[0].Split(','), l[1].Split(',')
        let x, y, z = int p[0], int p[1], int p[2]
        let x', y', z' = int q[0], int q[1], int q[2]

        match x = x', y = y', z = z' with
        | false, true, true -> X(id = i, minX = min x x', maxX = max x x', y = y, z = z)
        | true, false, true -> Y(id = i, x = x, minY = min y y', maxY = max y y', z = z)
        | true, true, false -> Z(id = i, x = x, y = y, minZ = min z z', maxZ = max z z')
        | true, true, true -> Z(id = i, x = x, y = y, minZ = z, maxZ = z)
        | _ -> failwith $"unimplemented, l = %A{l}")
    |> Seq.toList

module Example =
    let input =
        "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 5

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 7

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let bricks = parse input

    part1 bricks |> printfn "part1: %d"
    part2 bricks |> printfn "part2: %d"

    0
