open Xunit
open FsUnit.Xunit

type Step =
    | Remove of string
    | Change of string * int

let hash (s: string) =
    (0, s) ||> Seq.fold (fun acc c -> ((acc + (int c)) * 17) % 256)

let part1 (steps: string list) = steps |> List.sumBy hash

let part2 (steps: string list) =
    let parseStep (s: string) =
        if s.EndsWith('-') then
            Remove(s.TrimEnd('-'))
        else
            match s.Split('=') with
            | [| label; focalLength |] -> Change(label, int focalLength)
            | _ -> failwithf $"unimplemented, s = {s}"

    let steps = List.map parseStep steps

    (Map.empty, steps)
    ||> List.fold (fun box step ->
        match step with
        | Remove label ->
            box
            |> Map.change (hash label) (fun lenses ->
                match lenses with
                | Some lenses ->
                    let newLenses = lenses |> List.filter (fun (label', _) -> label' <> label)
                    Some newLenses
                | None -> None)
        | Change(label, focalLength) ->
            box
            |> Map.change (hash label) (fun lenses ->
                match lenses with
                | Some lenses ->
                    let newLenses =
                        match List.tryFindIndex (fun (label', _) -> label' = label) lenses with
                        | Some i -> List.updateAt i (label, focalLength) lenses
                        | None -> (label, focalLength) :: lenses

                    Some newLenses
                | None -> Some [ label, focalLength ]))
    |> Map.toList
    |> List.sumBy (fun (k, lenses) ->
        lenses
        |> List.rev
        |> List.indexed
        |> List.sumBy (fun (i, (_, focalLength)) -> (k + 1) * (i + 1) * focalLength))

let parse (input: string) = input.Split(",") |> Array.toList

module Example =
    let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 1320

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 145

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let steps = parse input

    part1 steps |> printfn "part1: %d"
    part2 steps |> printfn "part2: %d"

    0
