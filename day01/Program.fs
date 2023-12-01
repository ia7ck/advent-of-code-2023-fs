open Xunit
open FsUnit.Xunit

let tryFindDigit (text: string) (digitMap: Map<string, int>) =
    digitMap
    |> Map.tryPick (fun k v -> if text.StartsWith(k) then Some(v) else None)

let digits (text: string) digitMap =
    // text の suffix をぜんぶ見る
    [ 0 .. (text.Length - 1) ] |> Seq.map (fun i -> tryFindDigit text[i..] digitMap)

let firstDigit text digitMap =
    (text, digitMap) ||> digits |> Seq.pick id

let lastDigit text digitMap =
    (text, digitMap) ||> digits |> Seq.rev |> Seq.pick id

let solve texts digitMap =
    texts
    |> Seq.sumBy (fun t ->
        let first = firstDigit t digitMap
        let last = lastDigit t digitMap
        first * 10 + last)


let normalDigit =
    [ ("1", 1)
      ("2", 2)
      ("3", 3)
      ("4", 4)
      ("5", 5)
      ("6", 6)
      ("7", 7)
      ("8", 8)
      ("9", 9) ]

let lettersDigit =
    [ ("one", 1)
      ("two", 2)
      ("three", 3)
      ("four", 4)
      ("five", 5)
      ("six", 6)
      ("seven", 7)
      ("eight", 8)
      ("nine", 9) ]

let part1 (texts: string seq) = solve texts (Map normalDigit)

let part2 (texts: string seq) =
    solve texts (Map(normalDigit @ lettersDigit))

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
