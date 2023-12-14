open Xunit
open FsUnit.Xunit

type Almanac =
    { Seeds: int64 seq
      SeedToSoil: CategoryMap seq
      SoilToFertilizer: CategoryMap seq
      FertilizerToWater: CategoryMap seq
      WaterToLight: CategoryMap seq
      LightToTemperature: CategoryMap seq
      TemperatureToHumidity: CategoryMap seq
      HumidityToLocation: CategoryMap seq }

and CategoryMap =
    { DestinationRangeStart: int64
      SourceRangeStart: int64
      RangeLength: int64 }

    member self.Mapping(number: int64) =
        if
            self.SourceRangeStart <= number
            && number < self.SourceRangeStart + self.RangeLength
        then
            Some(number - self.SourceRangeStart + self.DestinationRangeStart)
        else
            None

// (Start, End]
type Range =
    { Start: int64
      End: int64 }

    member self.Intersect(other: Range) =
        if self.End <= other.Start || other.End <= self.Start then
            None
        else if self.Start <= other.Start && other.End <= self.End then
            Some other
        else if other.Start <= self.Start && self.End <= other.End then
            Some self
        else if self.Start <= other.Start then
            Some { Start = other.Start; End = self.End }
        else if other.Start <= self.Start then
            Some { Start = self.Start; End = other.End }
        else
            failwithf "self = %A, other = %A" self other

let part1 (almanac: Almanac) =
    let mapping (maps: CategoryMap seq) seed =
        maps |> Seq.tryPick (fun m -> m.Mapping seed) |> Option.defaultValue seed

    almanac.Seeds
    |> Seq.map (fun seed ->
        seed
        |> mapping almanac.SeedToSoil
        |> mapping almanac.SoilToFertilizer
        |> mapping almanac.FertilizerToWater
        |> mapping almanac.WaterToLight
        |> mapping almanac.LightToTemperature
        |> mapping almanac.TemperatureToHumidity
        |> mapping almanac.HumidityToLocation)
    |> Seq.min

let part2 (almanac: Almanac) =
    let p (map: CategoryMap) (range: Range) =
        let delta = map.DestinationRangeStart - map.SourceRangeStart

        let source =
            { Start = map.SourceRangeStart
              End = map.SourceRangeStart + map.RangeLength }

        match range.Intersect(source) with
        | None ->
            {| Source = [ range ]
               Destination = [] |}
        | Some intersect ->
            // range.Start <= intersect.Start < intersect.End <= range.End
            {| Source =
                [ { Start = range.Start
                    End = intersect.Start }
                  { Start = intersect.End
                    End = range.End } ]
                |> List.filter (fun r -> r.Start < r.End)
               Destination =
                [ { Start = intersect.Start + delta
                    End = intersect.End + delta } ] |}

    let q (map: CategoryMap) (ranges: Range list) =
        ({| Source = []; Destination = [] |}, ranges)
        ||> Seq.fold (fun acc s ->
            let x = p map s

            {| Source = x.Source @ acc.Source
               Destination = x.Destination @ acc.Destination |})

    let rec r (maps: CategoryMap list) (source: Range list) (destination: Range list) =
        match source with
        | [] -> destination
        | sh :: st ->
            let x =
                ({| Source = [ sh ]; Destination = [] |}, maps)
                ||> Seq.fold (fun acc m ->
                    let x = q m acc.Source

                    {| Source = x.Source
                       Destination = x.Destination @ acc.Destination |})

            r maps st (x.Source @ x.Destination @ destination)

    let mapping (maps: CategoryMap seq) (seedRanges: Range seq) =
        r (List.ofSeq maps) (List.ofSeq seedRanges) []

    almanac.Seeds
    |> Seq.chunkBySize 2
    |> Seq.map (fun chunk ->
        { Start = chunk[0]
          End = chunk[0] + chunk[1] })
    |> mapping almanac.SeedToSoil
    |> mapping almanac.SoilToFertilizer
    |> mapping almanac.FertilizerToWater
    |> mapping almanac.WaterToLight
    |> mapping almanac.LightToTemperature
    |> mapping almanac.TemperatureToHumidity
    |> mapping almanac.HumidityToLocation
    |> Seq.map (_.Start)
    |> Seq.min

let parse (input: string) =
    match input.Split("\n\n") with
    | [| seeds
         seedToSoil
         soilToFertilizer
         fertilizerToWater
         waterToLight
         lightToTemperature
         temperatureToHumidity
         humidityToLocation |] ->
        let categoryMaps (maps: string) =
            // seed-to-soil map:
            // ...
            maps.Split("\n")
            |> Seq.skip 1
            |> Seq.map (fun m ->
                match m.Split(' ') with
                | [| destStart; sourceStart; length |] ->
                    { DestinationRangeStart = int64 destStart
                      SourceRangeStart = int64 sourceStart
                      RangeLength = int64 length }
                | _ -> failwith "unimplemented")

        let seeds = seeds.Replace("seeds: ", "").Split(' ') |> Seq.map int64

        { Seeds = seeds
          SoilToFertilizer = categoryMaps soilToFertilizer
          SeedToSoil = categoryMaps seedToSoil
          FertilizerToWater = categoryMaps fertilizerToWater
          WaterToLight = categoryMaps waterToLight
          LightToTemperature = categoryMaps lightToTemperature
          TemperatureToHumidity = categoryMaps temperatureToHumidity
          HumidityToLocation = categoryMaps humidityToLocation }
    | _ -> failwithf "unimplemented, input = %A" input

module Example =
    let input =
        "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

    [<Fact>]
    let testPart1 () =
        parse input |> part1 |> should equal 35L

    [<Fact>]
    let testPart2 () =
        parse input |> part2 |> should equal 46L

[<EntryPoint>]
let main _ =
    let input = stdin.ReadToEnd().TrimEnd()
    let almanac = parse input

    part1 almanac |> printfn "part1: %d"
    part2 almanac |> printfn "part2: %d"

    0
