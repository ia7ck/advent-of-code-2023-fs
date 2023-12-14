open Xunit
open FsUnit.Xunit

type Card =
    | A = 100
    | K = 13
    | Q = 12
    | J = 11
    | T = 10
    | Nine = 9
    | Eight = 8
    | Seven = 7
    | Six = 6
    | Five = 5
    | Four = 4
    | Three = 3
    | Two = 2

type HandType =
    | FiveKind = 6
    | FourKind = 5
    | FullHouse = 4
    | ThreeKind = 3
    | TwoPair = 2
    | OnePair = 1
    | High = 0

type Hand =
    { C1: Card
      C2: Card
      C3: Card
      C4: Card
      C5: Card }

    member self.Type() =
        let cards =
            [ self.C1; self.C2; self.C3; self.C4; self.C5 ]
            |> List.countBy id
            |> List.map snd
            |> List.sortDescending

        match cards with
        | [ 5 ] -> HandType.FiveKind
        | [ 4; 1 ] -> HandType.FourKind
        | [ 3; 2 ] -> HandType.FullHouse
        | [ 3; 1; 1 ] -> HandType.ThreeKind
        | [ 2; 2; 1 ] -> HandType.TwoPair
        | [ 2; 1; 1; 1 ] -> HandType.OnePair
        | [ 1; 1; 1; 1; 1 ] -> HandType.High
        | _ -> failwith $"unimplemented, cards = {cards}"

    member self.JokerType() =
        let cards = [ self.C1; self.C2; self.C3; self.C4; self.C5 ]
        let jokers = cards |> List.filter ((=) Card.J)

        let cards =
            cards
            |> List.filter ((<>) Card.J)
            |> List.countBy id
            |> List.map snd
            |> List.sortDescending

        // 網羅できてる？
        match List.length jokers, cards with
        | 5, []
        | 4, [ 1 ]
        | 3, [ 2 ]
        | 2, [ 3 ]
        | 1, [ 4 ]
        | 0, [ 5 ] -> HandType.FiveKind
        | 3, [ 1; 1 ]
        | 2, [ 2; 1 ]
        | 1, [ 3; 1 ]
        | 0, [ 4; 1 ] -> HandType.FourKind
        | 1, [ 2; 2 ]
        | 0, [ 3; 2 ] -> HandType.FullHouse
        | 2, [ 1; 1; 1 ]
        | 1, [ 2; 1; 1 ]
        | 0, [ 3; 1; 1 ] -> HandType.ThreeKind
        | 0, [ 2; 2; 1 ] -> HandType.TwoPair
        | 1, [ 1; 1; 1; 1 ]
        | 0, [ 2; 1; 1; 1 ] -> HandType.OnePair
        | 0, [ 1; 1; 1; 1; 1 ] -> HandType.High
        | _ -> failwith $"unimplemented, jokers = {jokers}, cards = {cards}"



let compareHand (h: Hand) (h': Hand) =
    let t = h.Type()
    let t' = h'.Type()

    if t <> t' then compare t t'
    elif h.C1 <> h'.C1 then compare h.C1 h'.C1
    elif h.C2 <> h'.C2 then compare h.C2 h'.C2
    elif h.C3 <> h'.C3 then compare h.C3 h'.C3
    elif h.C3 <> h'.C3 then compare h.C3 h'.C3
    elif h.C4 <> h'.C4 then compare h.C4 h'.C4
    elif h.C5 <> h'.C5 then compare h.C5 h'.C5
    else 0 // equal

let compareJokerHand (h: Hand) (h': Hand) =
    let cmp (c: Card) (c': Card) =
        if c = c' then 0
        elif c = Card.J then -1
        elif c' = Card.J then 1
        else compare c c'

    let t = h.JokerType()
    let t' = h'.JokerType()

    if t <> t' then compare t t'
    elif h.C1 <> h'.C1 then cmp h.C1 h'.C1
    elif h.C2 <> h'.C2 then cmp h.C2 h'.C2
    elif h.C3 <> h'.C3 then cmp h.C3 h'.C3
    elif h.C3 <> h'.C3 then cmp h.C3 h'.C3
    elif h.C4 <> h'.C4 then cmp h.C4 h'.C4
    elif h.C5 <> h'.C5 then cmp h.C5 h'.C5
    else 0 // equal

let solve (hands: (Hand * int64) seq) (comparer: Hand -> Hand -> int) =
    hands
    |> Seq.sortWith (fun (h, _) (h', _) -> comparer h h')
    |> Seq.indexed
    |> Seq.sumBy (fun (i, (_, bid)) -> (int64 i + 1L) * bid)

let part1 (hands: (Hand * int64) seq) = solve hands compareHand

let part2 (hands: (Hand * int64) seq) = solve hands compareJokerHand

let parse (input: string) =
    input.Split("\n")
    |> Seq.map (fun s ->
        match s.Split(' ') with
        | [| hand; bid |] ->
            let card c =
                match c with
                | 'A' -> Card.A
                | 'K' -> Card.K
                | 'Q' -> Card.Q
                | 'J' -> Card.J
                | 'T' -> Card.T
                | '9'
                | '8'
                | '7'
                | '6'
                | '5'
                | '4'
                | '3'
                | '2' -> enum (int c - int '0')
                | _ -> failwith $"unimplemented, c = {c}"

            { C1 = card hand[0]
              C2 = card hand[1]
              C3 = card hand[2]
              C4 = card hand[3]
              C5 = card hand[4] },
            int64 bid
        | _ -> failwith $"unimplemented, s = {s}")

module Example =
    let input =
        "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 6440L

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 5905L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let hands = parse input

    part1 hands |> printfn "part1: %d"
    part2 hands |> printfn "part2: %d"

    0
