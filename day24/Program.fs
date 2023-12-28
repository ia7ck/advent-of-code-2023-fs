open Xunit
open FsUnit.Xunit

type Hailstone =
    { Px: decimal
      Py: decimal
      Pz: decimal
      Vx: decimal
      Vy: decimal
      Vz: decimal }

let intersectLater (h1: Hailstone) (h2: Hailstone) =
    // (h1.Px + s * h1.Vx, h1.Py + s * h1.Vy) = (h2.Px + t * h2.Vx, h2.Py + t * h2.Vy)
    // s = (h2.Px + t * h2.Vx - h1.Px) / h1.Vx
    // -> h1.Py + (h2.Px + t * h2.Vx - h1.Px) * (h1.Vy / h1.Vx) = h2.Py + t * h2.Vy
    // -> (h2.Vx * (h1.Vy / h1.Vx) - h2.Vy) * t = h2.Py - h1.Py - (h2.Px - h1.Px) * (h1.Vy / h1.Vx)
    // -> t = ...

    if h1.Vx = 0m then
        None
    else
        let n = h1.Vy / h1.Vx
        let d = h2.Vx * n - h2.Vy

        if h1.Vy * h2.Vx = h2.Vy * h1.Vx then
            // 平行
            // if d = 0m then だと精度が足りなかった
            None
        else
            let t = ((h2.Py - h1.Py) - (h2.Px - h1.Px) * n) / d
            let s = (h2.Px + t * h2.Vx - h1.Px) / h1.Vx

            // 時刻が正
            if s > 0m && t > 0m then
                let x = h1.Px + s * h1.Vx
                let y = h1.Py + s * h1.Vy
                Some((s, t), (x, y))
            else
                None

let part1 (lower: decimal) (upper: decimal) (stones: Hailstone list) =
    List.allPairs stones stones
    |> List.filter (fun (s, s') ->
        match intersectLater s s' with
        | None -> false
        | Some(_, (x, y)) -> lower <= x && x <= upper && lower <= y && y <= upper)
    |> List.length
    |> (fun count ->
        assert (count % 2 = 0)
        count / 2)

let part2 (stones: Hailstone list) =
    let limit = 250m // 解が見つかるように適当に大きめの値

    List.allPairs [ -limit .. limit ] [ -limit .. limit ]
    |> List.collect (fun (vx, vy) ->
        let h0, h1, h2 = stones[0], stones[1], stones[2]

        let p1 =
            intersectLater
                { h0 with
                    Vx = h0.Vx - vx
                    Vy = h0.Vy - vy }
                { h1 with
                    Vx = h1.Vx - vx
                    Vy = h1.Vy - vy }

        let p2 =
            intersectLater
                { h0 with
                    Vx = h0.Vx - vx
                    Vy = h0.Vy - vy }
                { h2 with
                    Vx = h2.Vx - vx
                    Vy = h2.Vy - vy }

        match p1, p2 with
        | Some((_, t1), (x1, y1)), Some((_, t2), (x2, y2)) when (x1, y1) = (x2, y2) ->
            [ -limit .. limit ]
            |> List.choose (fun vz ->
                let z1 = h1.Pz + t1 * (h1.Vz - vz)
                let z2 = h2.Pz + t2 * (h2.Vz - vz)

                let rock =
                    { Px = x1
                      Py = y1
                      Pz = z1
                      Vx = vx
                      Vy = vy
                      Vz = vz }

                if z1 = z2 && List.forall (fun h -> intersectLater h rock |> Option.isSome) stones then
                    Some((x1, y1, z1), (vx, vy, vz))
                else
                    None)
        | _ -> [])
    |> List.filter (fun ((x, y, z), _) ->
        System.Decimal.IsInteger x
        && System.Decimal.IsInteger y
        && System.Decimal.IsInteger z)
    |> List.map (fun ((x, y, z), v) ->
        (System.Decimal.ToInt64 x, System.Decimal.ToInt64 y, System.Decimal.ToInt64 z), v)

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun l ->
        let l = l.Split('@')
        let p = l[0].Trim().Split(',', System.StringSplitOptions.RemoveEmptyEntries)
        let v = l[1].Trim().Split(',', System.StringSplitOptions.RemoveEmptyEntries)

        match p, v with
        | [| px; py; pz |], [| vx; vy; vz |] ->
            { Px = decimal px
              Py = decimal py
              Pz = decimal pz
              Vx = decimal vx
              Vy = decimal vy
              Vz = decimal vz }
        | _ -> failwithf $"unimplemented, l = %A{l}")
    |> Seq.toList

module Example =
    let input =
        "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3"

    [<Fact>]
    let testIntersect () =
        let stones = parse input

        intersectLater stones[0] stones[1]
        |> Option.get
        |> (fun (_, (x, y)) ->
            // 本当は相対誤差を見たほうがいい？
            System.Math.Round(x, 3) |> should equal 14.333m
            System.Math.Round(y, 3) |> should equal 15.333m)

        intersectLater stones[0] stones[2]
        |> Option.get
        |> (fun (_, (x, y)) ->
            System.Math.Round(x, 3) |> should equal 11.667m
            System.Math.Round(y, 3) |> should equal 16.667m)

        intersectLater stones[0] stones[3]
        |> Option.get
        |> (fun (_, (x, y)) ->
            x |> should equal 6.2m
            y |> should equal 19.4m)

        intersectLater stones[0] stones[4] |> should equal None

        intersectLater stones[1] stones[2] |> should equal None

        intersectLater stones[1] stones[3]
        |> Option.get
        |> (fun (_, (x, y)) ->
            x |> should equal -6m
            y |> should equal -5m)

        intersectLater stones[1] stones[4] |> should equal None

        intersectLater stones[2] stones[3]
        |> Option.get
        |> (fun (_, (x, y)) ->
            x |> should equal -2m
            y |> should equal 3m)

        intersectLater stones[2] stones[4] |> should equal None

        intersectLater stones[3] stones[4] |> should equal None

    [<Fact>]
    let testPart1 () =
        parse input |> part1 7m 27m |> should equal 2

    [<Fact>]
    let testPart2 () =
        parse input
        |> part2
        |> List.sort
        |> should
            equalSeq
            [ ((24L, 13L, 10L), (-3m, 1m, 2m))
              // なぜか2個目の解がある
              ((207L, -128L, -272L), (-46M, 34M, 68m)) ]

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let stones = parse input

    part1 200000000000000m 400000000000000m stones |> printfn "part1: %d"

    part2 stones
    |> List.exactlyOne
    |> (fun ((x, y, z), _) -> printfn $"part2: {x + y + z}")

    0
