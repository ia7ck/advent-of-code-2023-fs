open Xunit
open FsUnit.Xunit

type Card =
    { Id: int
      WinningNumbers: int seq
      MyNumbers: int seq }

    member self.MyWinningNumbers() =
        self.MyNumbers |> Seq.filter (fun n -> Seq.contains n self.WinningNumbers)

let part1 (cards: Card seq) =
    cards
    |> Seq.sumBy (fun card ->
        let win = card.MyWinningNumbers() |> Seq.length
        if win = 0 then 0 else pown 2 (win - 1))

let part2 (cards: Card seq) =
    let moreCards (card: Card) =
        let win = card.MyWinningNumbers() |> Seq.length
        cards |> Seq.filter (fun c -> card.Id < c.Id && c.Id <= card.Id + win)

    let rec solve (originals: Card list) (copies: Map<int, int>) total =
        match originals with
        | h :: t ->
            let count = 1 + (Map.tryFind h.Id copies |> Option.defaultValue 0)

            let newCopies =
                (copies, moreCards h)
                ||> Seq.fold (fun acc card ->
                    acc
                    |> Map.change card.Id (fun x ->
                        match x with
                        | Some(x) -> Some(x + count)
                        | None -> Some(count)))

            solve t newCopies (total + count)
        | [] -> total

    solve (List.ofSeq cards) Map.empty 0

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun card ->
        // Card 1: ... | ...
        let i = card.IndexOf(':')
        let cardId = card[.. (i - 1)].Replace("Card", "") |> int

        let winningNumbers, myNumbers =
            match card[(i + 1) ..].Split('|') with
            | [| winningNumbers; myNumbers |] ->
                let toInts (s: string) =
                    s.Split(' ', System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map int

                (toInts winningNumbers, toInts myNumbers)
            | _ -> failwith "unimplemented"

        { Id = cardId
          WinningNumbers = winningNumbers
          MyNumbers = myNumbers })

module Example =
    let input =
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 13

    [<Fact>]
    let testPart2 () = parse input |> part2 |> should equal 30

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let cards = parse input

    part1 cards |> printfn "part1: %d"
    part2 cards |> printfn "part2: %d"

    0
