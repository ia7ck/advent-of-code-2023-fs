open Xunit
open FsUnit.Xunit

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

type DigPlan =
    { Dir: Dir
      Length: int
      Color: string }

    member self.Encode() =
        assert (self.Color.Length = 7)
        assert self.Color.StartsWith('#')
        let distance = System.Convert.ToInt64(self.Color[1..5], 16)

        let dir =
            match self.Color[6] with
            | '0' -> R
            | '1' -> D
            | '2' -> L
            | '3' -> U
            | _ -> failwith $"unimplemented, color = {self.Color}"

        distance, dir

// part2 と共通化できる?
let part1 (digPlans: DigPlan list) =
    let rec trace (i, j) (digPlans: DigPlan list) route =
        match digPlans with
        | [] -> List.rev route
        | h :: t ->
            let di, dj = h.Dir.Delta()

            let (ni, nj), newRoute =
                (((i, j), route), [ 1 .. h.Length ])
                ||> List.fold (fun ((i, j), route) _ -> (i + di, j + dj), (i, j) :: route)

            trace (ni, nj) t newRoute

    let loop = trace (0, 0) digPlans [] // loop ???

    let minRow, maxRow, minColumn, maxColumn =
        ((1000, -1000, 1000, -1000), loop)
        ||> List.fold (fun (minRow, maxRow, minColumn, maxColumn) (row, col) ->
            min minRow row, max maxRow row, min minColumn col, max maxColumn col)

    let minRow, maxRow, minColumn, maxColumn =
        minRow - 1, maxRow + 1, minColumn - 1, maxColumn + 1

    let loop = Set.ofSeq loop

    // めも：DFSっぽくやると stack overflow になったのでBFSする
    let rec visitExterior queue seen =
        if List.isEmpty queue then
            seen
        else
            let newQueue, newSeen =
                (([], seen), queue)
                ||> List.fold (fun (newQueue, newSeen) (row, col) ->
                    ((newQueue, newSeen), [ U; L; D; R ])
                    ||> List.fold (fun (newQueue, newSeen) dir ->
                        let di, dj = dir.Delta()
                        let ni, nj = row + di, col + dj

                        if
                            minRow <= ni
                            && ni <= maxRow
                            && minColumn <= nj
                            && nj <= maxColumn
                            && not (Set.contains (ni, nj) newSeen)
                            && not (Set.contains (ni, nj) loop)
                        then
                            (ni, nj) :: newQueue, Set.add (ni, nj) newSeen
                        else
                            newQueue, newSeen))

            visitExterior newQueue newSeen

    let start = minRow, minColumn
    let exterior = visitExterior [ start ] (set [ start ])

    (maxRow - minRow + 1) * (maxColumn - minColumn + 1) - Set.count exterior


let part2 (digPlans: DigPlan list) =
    let corners, last =
        (([], (0L, 0L)), digPlans)
        ||> List.fold (fun (corners, (i, j)) plan ->
            let distance, dir = plan.Encode()
            let di, dj = dir.Delta()
            let ni, nj = i + int64 di * distance, j + int64 dj * distance
            (i, j) :: corners, (ni, nj))

    assert (last = (0L, 0L))

    // ややこしいが、マスではなくマスの「左上」「右下」の格子点を見る
    let lattice = corners @ (List.map (fun (i, j) -> (i + 1L, j + 1L)) corners)
    let ys, xs = List.unzip lattice

    let padding = 5L

    let ys =
        (List.min ys - padding) :: (List.max ys + padding) :: ys
        |> List.sort
        |> List.distinct

    let xs =
        (List.min xs - padding) :: (List.max xs + padding) :: xs
        |> List.sort
        |> List.distinct

    let rec trace (y, x) (digPlans: DigPlan list) loop =
        match digPlans with
        | [] -> loop
        | h :: t ->
            let distance, dir = h.Encode()
            let dy, dx = dir.Delta()
            let ny, nx = y + int64 dy * distance, x + int64 dx * distance

            if y = ny then
                let i = List.findIndex ((=) y) ys
                let left = List.findIndex ((=) (min x nx)) xs
                let right = List.findIndex ((=) (max x nx)) xs
                let newLoop = loop @ [ for j in left..right -> i, j ]
                trace (ny, nx) t newLoop
            else
                assert (x = nx)
                let j = List.findIndex ((=) x) xs
                let top = List.findIndex ((=) (min y ny)) ys
                let bottom = List.findIndex ((=) (max y ny)) ys
                let newLoop = loop @ [ for i in top..bottom -> i, j ]
                trace (ny, nx) t newLoop

    let loop = trace (0L, 0L) digPlans [] |> Set.ofSeq

    let rec visitExterior queue seen =
        if List.isEmpty queue then
            seen
        else
            let newQueue, newSeen =
                (([], seen), queue)
                ||> List.fold (fun (newQueue, newSeen) (i, j) ->
                    ((newQueue, newSeen), [ U; L; D; R ])
                    ||> List.fold (fun (newQueue, newSeen) dir ->
                        let di, dj = dir.Delta()
                        let ni, nj = i + di, j + dj

                        if
                            0 <= ni
                            && ni < ys.Length
                            && 0 <= nj
                            && nj < xs.Length
                            && not (Set.contains (ni, nj) newSeen)
                            && not (Set.contains (ni, nj) loop)
                        then
                            (ni, nj) :: newQueue, Set.add (ni, nj) newSeen
                        else
                            newQueue, newSeen)

                )

            visitExterior newQueue newSeen

    let start = 0, 0
    let exterior = visitExterior [ start ] (set [ start ])

    let area (i, j) =
        assert (i < ys.Length)
        assert (j < xs.Length)

        let height =
            match List.tryItem (i + 1) ys with
            | Some y -> y - ys[i]
            | None -> 0L

        let width =
            match List.tryItem (j + 1) xs with
            | Some x -> x - xs[j]
            | None -> 0L

        height * width

    (List.max ys - List.min ys) * (List.max xs - List.min xs)
    - Seq.sumBy area exterior

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun l ->
        match l.TrimEnd().Split(' ') with
        | [| dir; length; color |] ->
            { Dir =
                (match dir with
                 | "U" -> U
                 | "L" -> L
                 | "D" -> D
                 | "R" -> R
                 | _ -> failwith $"unimplemented, dir = {dir}")
              Length = int length
              Color = color.Replace("(", "").Replace(")", "") }
        | _ -> failwith $"unimplemented, l = {l}")
    |> Seq.toList

module Example =
    let input =
        "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 62

    [<Fact>]
    let testPart2Small () =
        let input =
            "R 6 (#000060)
D 5 (#000051)
L 2 (#000022)
D 2 (#000021)
R 2 (#000020)
D 2 (#000021)
L 5 (#000052)
U 2 (#000023)
L 1 (#000012)
U 2 (#000023)
R 2 (#000020)
U 3 (#000033)
L 2 (#000022)
U 2 (#000023)"

        parse input |> part2 |> should equal 62L

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 952408144115L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let plans = parse input

    part1 plans |> printfn "part1: %d"
    part2 plans |> printfn "part2: %d"

    0
