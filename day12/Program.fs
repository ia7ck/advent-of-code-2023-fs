open System.Collections.Generic
open Xunit
open FsUnit.Xunit

type Spring =
    | Operational
    | Damaged

type Row =
    { Springs: Spring option list
      Sizes: int list }

let part1 (rows: Row list) =
    let runLength l =
        let rec runLength l acc =
            match l with
            | [] -> List.rev acc
            | h :: t ->
                match acc with
                | [] -> runLength t [ (h, 1) ]
                | (prev, length) :: tt when prev = h -> runLength t ((prev, length + 1) :: tt)
                | acc -> runLength t ((h, 1) :: acc)

        runLength l []

    let enumerate springs =
        let rec enumerate springs patterns =
            match springs with
            | [] -> patterns |> List.map List.rev
            | h :: t ->
                let newPatterns =
                    match patterns, h with
                    | patterns, Some s -> List.map (fun p -> s :: p) patterns
                    | patterns, None -> List.collect (fun p -> [ Operational :: p; Damaged :: p ]) patterns

                enumerate t newPatterns

        enumerate springs [ [] ]

    let fit springs sizes =
        runLength springs
        |> List.filter (fun (s, _) -> s = Damaged)
        |> List.map snd
        |> (=) sizes

    rows
    |> Seq.sumBy (fun row -> enumerate row.Springs |> Seq.filter (fun s -> fit s row.Sizes) |> Seq.length)

let part2 (rows: Row list) =
    let accept spring springOpt =
        match springOpt with
        | None -> true
        | Some(spring') -> spring = spring'

    let solve (row: Row) =
        let memo = Dictionary<_, _>()

        let rec dp springs sizes =
            match memo.TryGetValue((springs, sizes)) with
            | true, value -> value
            | false, _ ->
                let value =
                    match springs, sizes with
                    | [], [] -> 1L
                    | [], _ -> 0L
                    | springs, [] -> if List.forall (accept Operational) springs then 1L else 0L
                    | Some(Operational) :: t, sizes -> dp t sizes
                    | Some(Damaged) :: _, size :: tt ->
                        // size = 3
                        // #???*** -> ok
                        // #??.*** -> ok
                        // #??#*** -> ng

                        let damagedSprings = springs[.. (size - 1)]

                        if damagedSprings.Length = size && List.forall (accept Damaged) damagedSprings then
                            match List.tryItem size springs with
                            | None
                            | Some None
                            | Some(Some Operational) ->
                                // springs[size] は飛ばす
                                dp springs[(size + 1) ..] tt
                            | _ -> 0L
                        else
                            0L
                    | None :: t, sizes -> (dp ((Some Operational) :: t) sizes) + (dp ((Some Damaged) :: t) sizes)

                memo.Add((springs, sizes), value)
                value

        dp row.Springs row.Sizes

    rows
    |> Seq.map (fun row ->
        let springs =
            row.Springs
            @ [ None ]
            @ row.Springs
            @ [ None ]
            @ row.Springs
            @ [ None ]
            @ row.Springs
            @ [ None ]
            @ row.Springs

        let sizes = row.Sizes @ row.Sizes @ row.Sizes @ row.Sizes @ row.Sizes
        { Springs = springs; Sizes = sizes })
    |> Seq.sumBy solve

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun l ->
        let l = l.Split(' ')

        let springs =
            l[0]
            |> Seq.map (fun ch ->
                match ch with
                | '.' -> Some Operational
                | '#' -> Some Damaged
                | '?' -> None
                | _ -> failwith $"unimplemented, ch = {ch}")

        let sizes = l[1].Split(',') |> Seq.map int

        { Springs = List.ofSeq springs
          Sizes = List.ofSeq sizes })
    |> Seq.toList

module Example =
    let input =
        "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 21


    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 525152L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let rows = parse input

    part1 rows |> printfn "part1: %d"
    part2 rows |> printfn "part2: %d"

    0
