open Xunit
open FsUnit.Xunit

let part1 (edges: (string * string) list) =
    let edges =
        edges |> List.collect (fun (u, v) -> [ (u, v); (v, u) ]) |> List.distinct

    let nodes = edges |> List.map fst |> List.distinct

    let start = nodes[0]

    let group1, group2 =
        nodes[1..]
        |> List.partition (fun goal ->
            let rec reachFourTimes count usedEdge =
                if count = 4 then
                    true
                else
                    let adjacent node =
                        edges
                        |> List.choose (fun (s, t) ->
                            if s = node && not (Set.contains (s, t) usedEdge) then
                                Some t
                            else
                                None)

                    let rec trip queue visitedNode =
                        if List.isEmpty queue then
                            None
                        else
                            let complete =
                                queue |> List.tryPick (fun (u, route) -> if u = goal then Some route else None)

                            match complete with
                            | Some route -> Some route
                            | None ->
                                let newQueue, newVisited =
                                    (([], visitedNode), queue)
                                    ||> List.fold (fun (newQueue, newVisited) (u, route) ->
                                        ((newQueue, newVisited), adjacent u)
                                        ||> List.fold (fun (newQueue, newVisited) v ->
                                            if Set.contains v newVisited then
                                                newQueue, newVisited
                                            else
                                                (v, v :: route) :: newQueue, (Set.add v newVisited)))

                                trip newQueue newVisited

                    match trip [ start, [ start ] ] Set.empty with
                    | None -> false
                    | Some route ->
                        let newUsedEdge =
                            usedEdge
                            |> Set.union (Set.ofList (List.pairwise route))
                            |> Set.union (Set.ofList (List.pairwise route |> List.map (fun (s, t) -> (t, s))))

                        reachFourTimes (count + 1) newUsedEdge

            reachFourTimes 0 Set.empty)

    let group1 = start :: group1

    let cut =
        edges
        |> List.filter (fun (s, t) -> List.contains s group1 && List.contains t group2)

    assert (List.length cut = 3)

    (List.length group1) * (List.length group2)

let parse (input: string) =
    input.Split("\n")
    |> Seq.collect (fun l ->
        let l = l.Split(": ")
        let a = l[0]
        l[1].Split(' ') |> Array.map (fun b -> a, b) |> Array.toList)
    |> Seq.toList

module Example =
    let input =
        "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 54

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let edges = parse input

    part1 edges |> printfn "part1: %d"

    0
