open Xunit
open FsUnit.Xunit

// https://learn.microsoft.com/ja-jp/dotnet/fsharp/language-reference/active-patterns#partial-active-patterns
let (|StartsWith|_|) (p: string) (s: string) =
    if s.StartsWith(p) then Some(()) else None

let parseNormalDigit (text: string) =
    match text with
    | StartsWith "1" -> Some(1)
    | StartsWith "2" -> Some(2)
    | StartsWith "3" -> Some(3)
    | StartsWith "4" -> Some(4)
    | StartsWith "5" -> Some(5)
    | StartsWith "6" -> Some(6)
    | StartsWith "7" -> Some(7)
    | StartsWith "8" -> Some(8)
    | StartsWith "9" -> Some(9)
    | _ -> None

let parseLettersDigit (text: string) =
    match text with
    | StartsWith "one" -> Some(1)
    | StartsWith "two" -> Some(2)
    | StartsWith "three" -> Some(3)
    | StartsWith "four" -> Some(4)
    | StartsWith "five" -> Some(5)
    | StartsWith "six" -> Some(6)
    | StartsWith "seven" -> Some(7)
    | StartsWith "eight" -> Some(8)
    | StartsWith "nine" -> Some(9)
    | _ -> None

let collectDigits (text: string) (parseDigit: string -> int option) =
    let rec collect t acc =
        if String.length t = 0 then
            Seq.rev acc
        else
            match parseDigit t with
            | Some v -> collect t[1..] (v :: acc)
            | None -> collect t[1..] acc

    collect text []

let solve texts parseDigit =
    texts
    |> Seq.sumBy (fun t ->
        let digits = collectDigits t parseDigit
        let first = Seq.head digits
        let last = Seq.last digits
        first * 10 + last)

let part1 (texts: string seq) = solve texts parseNormalDigit

let part2 (texts: string seq) =
    let parseDigit t =
        parseNormalDigit t
        |> Option.orElseWith (fun () ->
            // fallback
            parseLettersDigit t)

    solve texts parseDigit

let parse (input: string) = input.Split("\n")

module Example =
    [<Fact>]
    let testPart1 () =
        let input =
            "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

        parse input |> part1 |> should equal 142

    [<Fact>]
    let testCollectDigits () =
        // こういう入力は two/ne ではなく two, one と扱う
        collectDigits "twone" parseLettersDigit |> should equalSeq (seq [ 2; 1 ])

    [<Fact>]
    let testPart2 () =
        let input =
            "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen"

        parse input |> part2 |> should equal 281

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let texts = parse input

    part1 texts |> printfn "part1: %d"
    part2 texts |> printfn "part2: %d"

    0
