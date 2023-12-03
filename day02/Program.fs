open Xunit
open FsUnit.Xunit

type Game = { Id: int; CubesSets: Cubes seq }

and Cubes =
    { Red: int
      Green: int
      Blue: int }

    member self.Max(other) =
        { Red = max self.Red other.Red
          Green = max self.Green other.Green
          Blue = max self.Blue other.Blue }


let part1 (games: Game seq) =
    games
    |> Seq.filter (fun game ->
        game.CubesSets
        |> Seq.forall (fun cubes -> cubes.Red <= 12 && cubes.Green <= 13 && cubes.Blue <= 14))
    |> Seq.sumBy (fun game -> game.Id)

let part2 (games: Game seq) =
    games
    |> Seq.sumBy (fun game ->
        let fewest = game.CubesSets |> Seq.reduce (fun acc cubes -> acc.Max(cubes))
        fewest.Red * fewest.Green * fewest.Blue)

let parse (input: string) =
    let parseGame (game: string) =
        let (gameId, cubesSets) =
            match game.Split(": ") with
            | [| gameId; cubesSets |] -> (gameId, cubesSets)
            | _ -> failwithf "unimplemented, game = %s" game

        let gameId = gameId.Replace("Game ", "") |> int

        let parseCubesSet (cubesSet: string) =
            ({ Red = 0; Green = 0; Blue = 0 }, cubesSet.Split(", "))
            ||> Seq.fold (fun acc cube ->
                match cube.Split(" ") with
                | [| n; "red" |] -> { acc with Red = int n }
                | [| n; "green" |] -> { acc with Green = int n }
                | [| n; "blue" |] -> { acc with Blue = int n }
                | _ -> failwithf "unimplemented, cube = %s" cube)

        let cubesSets = cubesSets.Split("; ") |> Seq.map parseCubesSet
        { Id = gameId; CubesSets = cubesSets }

    input.Split("\n") |> Seq.map parseGame

module Example =
    let input =
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

    [<Fact>]
    let testPart1 () = parse input |> part1 |> should equal 8

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 2286

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let games = parse input

    part1 games |> printfn "part1: %d"
    part2 games |> printfn "part2: %d"

    0
