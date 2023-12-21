open System.Text.RegularExpressions

open Xunit
open FsUnit.Xunit

type MachinePart = { X: int; M: int; A: int; S: int }

type Terminate =
    | Accept
    | Reject

type Destination =
    | Next of string // workflow name
    | Terminate of Terminate

    member self.Name() =
        match self with
        | Next name -> name
        | Terminate Accept -> "A"
        | Terminate Reject -> "R"

type StepRule =
    { Category: Category
      Cmp: Cmp
      CmpValue: int
      Destination: Destination }

    member self.Meet(machinePart: MachinePart) =
        match self.Category, self.Cmp with
        | X, Less -> machinePart.X < self.CmpValue
        | M, Less -> machinePart.M < self.CmpValue
        | A, Less -> machinePart.A < self.CmpValue
        | S, Less -> machinePart.S < self.CmpValue
        | X, Greater -> machinePart.X > self.CmpValue
        | M, Greater -> machinePart.M > self.CmpValue
        | A, Greater -> machinePart.A > self.CmpValue
        | S, Greater -> machinePart.S > self.CmpValue

and Category =
    | X
    | M
    | A
    | S

and Cmp =
    | Less // <
    | Greater // >

type Rule =
    | StepRule of StepRule
    | TerminateRule of Destination

type Workflow = { Name: string; Rules: Rule list }

let part1 ((workflows, machineParts): Workflow list * MachinePart list) =
    let workflowByName = workflows |> List.map (fun w -> w.Name, w) |> Map.ofList

    let rec apply (workflow: Workflow) (machinePart: MachinePart) =
        let destination =
            workflow.Rules
            |> List.pick (fun rule ->
                match rule with
                | StepRule stepRule ->
                    if stepRule.Meet(machinePart) then
                        Some stepRule.Destination
                    else
                        None
                | TerminateRule destination -> Some destination)

        match destination with
        | Next name -> apply (workflowByName[name]) machinePart
        | Terminate terminate -> terminate

    let accept =
        machineParts
        |> List.filter (fun part ->
            let term = apply workflowByName["in"] part

            match term with
            | Accept -> true
            | Reject -> false)

    accept |> List.sumBy (fun p -> p.X + p.M + p.A + p.S)

type MachinePartSet =
    { XSet: Set<int>
      MSet: Set<int>
      ASet: Set<int>
      SSet: Set<int> }

    member self.Union(other: MachinePartSet) =
        { XSet = Set.union self.XSet other.XSet
          MSet = Set.union self.MSet other.MSet
          ASet = Set.union self.ASet other.ASet
          SSet = Set.union self.SSet other.SSet }

    member self.Intersect(other: MachinePartSet) =
        { XSet = Set.intersect self.XSet other.XSet
          MSet = Set.intersect self.MSet other.MSet
          ASet = Set.intersect self.ASet other.ASet
          SSet = Set.intersect self.SSet other.SSet }

let part2 (workflows: Workflow list) =
    let workflowByName = workflows |> List.map (fun w -> w.Name, w) |> Map.ofList

    let rec traverse (workflows: Workflow list) (parts: Map<string, MachinePartSet list>) =
        if List.isEmpty workflows then
            parts
        else
            let newWorkflows, newParts =
                (([], parts), workflows)
                ||> List.fold (fun (newWorkflows, newParts) wf ->
                    let newWorkflows, newParts, _ =
                        ((newWorkflows, newParts, parts[wf.Name]), wf.Rules)
                        ||> List.fold (fun (newWorkflows, newParts, p) rule ->
                            match rule with
                            | TerminateRule destination ->
                                let newWorkflows =
                                    match destination with
                                    | Next name -> workflowByName[name] :: newWorkflows
                                    | Terminate _ -> newWorkflows

                                let name = destination.Name()

                                let newParts =
                                    newParts
                                    |> Map.change name (fun v ->
                                        match v with
                                        | None -> Some p
                                        | Some v -> Some(v @ p))

                                newWorkflows, newParts, p
                            | StepRule stepRule ->
                                let newWorkflows =
                                    match stepRule.Destination with
                                    | Next name -> workflowByName[name] :: newWorkflows
                                    | Terminate _ -> newWorkflows

                                let ifTrueSet, elseSet =
                                    match stepRule.Cmp with
                                    | Less -> set [ 1 .. (stepRule.CmpValue - 1) ], set [ stepRule.CmpValue .. 4000 ]
                                    | Greater ->
                                        set [ (stepRule.CmpValue + 1) .. 4000 ], set [ 1 .. stepRule.CmpValue ]

                                let name = stepRule.Destination.Name()

                                let newParts =
                                    newParts
                                    |> Map.change name (fun v ->
                                        let p' =
                                            p
                                            |> List.map (fun p ->
                                                match stepRule.Category with
                                                | X ->
                                                    { p with
                                                        XSet = Set.intersect p.XSet ifTrueSet }
                                                | M ->
                                                    { p with
                                                        MSet = Set.intersect p.MSet ifTrueSet }
                                                | A ->
                                                    { p with
                                                        ASet = Set.intersect p.ASet ifTrueSet }
                                                | S ->
                                                    { p with
                                                        SSet = Set.intersect p.SSet ifTrueSet })

                                        match v with
                                        | None -> Some p'
                                        | Some v -> Some(v @ p'))

                                let newP =
                                    p
                                    |> List.map (fun p ->
                                        match stepRule.Category with
                                        | X ->
                                            { p with
                                                XSet = Set.intersect p.XSet elseSet }
                                        | M ->
                                            { p with
                                                MSet = Set.intersect p.MSet elseSet }
                                        | A ->
                                            { p with
                                                ASet = Set.intersect p.ASet elseSet }
                                        | S ->
                                            { p with
                                                SSet = Set.intersect p.SSet elseSet })

                                newWorkflows, newParts, newP)

                    newWorkflows, newParts)

            traverse newWorkflows newParts

    let parts =
        traverse
            [ workflowByName["in"] ]
            (Map
                [ "in",
                  [ { XSet = set [ 1..4000 ]
                      MSet = set [ 1..4000 ]
                      ASet = set [ 1..4000 ]
                      SSet = set [ 1..4000 ] } ] ])

    let accept = parts["A"]

#if DEBUG
    List.allPairs accept accept
    |> List.iter (fun (p, p') ->
        if p <> p' then
            let intersect = p.Intersect(p')

            // 違う経路で Accept に着く x, m, a, s の組は被らない
            // → それぞれの経路での寄与を単純に足し上げて答えが過不足なく出る
            assert
                (intersect.XSet.IsEmpty
                 || intersect.MSet.IsEmpty
                 || intersect.ASet.IsEmpty
                 || intersect.SSet.IsEmpty))
#endif

    accept
    |> List.sumBy (fun p ->
        let x = int64 p.XSet.Count
        let m = int64 p.MSet.Count
        let a = int64 p.ASet.Count
        let s = int64 p.SSet.Count
        x * m * a * s)

let parse (input: string) =
    let destination label =
        match label with
        | "A" -> Terminate Accept
        | "R" -> Terminate Reject
        | _ -> Next label

    let rec rules rest acc =
        match rest with
        | [] -> List.rev acc
        | [ last ] -> rules [] (TerminateRule(destination last) :: acc)
        | h :: t ->
            // a<2006:qkq
            let m = Regex.Match(h, @"^(x|m|a|s)(<|>)(\d+):([a-z]+|A|R)$")
            let values = m.Groups |> Seq.toList |> List.map (_.Value)

            let rule =
                match values with
                | [ _; category; cmp; value; dest ] ->
                    let value = int value
                    let dest = destination dest

                    let category, cmp =
                        match category, cmp with
                        | "x", "<" -> X, Less
                        | "m", "<" -> M, Less
                        | "a", "<" -> A, Less
                        | "s", "<" -> S, Less
                        | "x", ">" -> X, Greater
                        | "m", ">" -> M, Greater
                        | "a", ">" -> A, Greater
                        | "s", ">" -> S, Greater
                        | _ -> failwithf $"unimplemented, category = {category}, cmp = {cmp}"

                    { Category = category
                      Cmp = cmp
                      CmpValue = value
                      Destination = dest }
                | _ -> failwithf $"unimplemented, values = %A{values}"

            rules t (StepRule rule :: acc)

    let input = input.Split("\n\n")

    let workflows =
        input[0].Split("\n")
        |> Seq.map (fun l ->
            let m = Regex.Match(l, @"^([a-z]+){(.+)}$")

            match List.ofSeq m.Groups with
            | [ _; name; rules' ] ->
                let rules = rules (List.ofArray (rules'.Value.Split(','))) []
                { Name = name.Value; Rules = rules }
            | _ -> failwithf $"unimplemented, l = {l}")
        |> Seq.toList

    let machineParts =
        input[1].Split("\n")
        |> Seq.map (fun l ->
            let m = Regex.Match(l, @"^{x=(\d+),m=(\d+),a=(\d+),s=(\d+)}$")

            match List.ofSeq m.Groups with
            | [ _; x; m; a; s ] ->
                { X = int x.Value
                  M = int m.Value
                  A = int a.Value
                  S = int s.Value }
            | _ -> failwithf $"unimplemented, l = {l}")
        |> Seq.toList

    workflows, machineParts

module Example =
    let input =
        "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 19114

    [<Fact>]
    let testPart2 () =
        parse input |> fst |> part2 |> should equal 167409079868000L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let workflows, machineParts = parse input

    part1 (workflows, machineParts) |> printfn "part1: %d"
    part2 workflows |> printfn "part2: %d"

    0
