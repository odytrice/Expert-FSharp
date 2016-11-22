type Input<'T> = 
    { Data : 'T
      Features : float [] }

type Centroid = float []

module Array = 
    /// Like Seq.groupBy, but returns array
    let classifyBy f (xs : _ []) = 
        xs
        |> Seq.groupBy f
        |> Seq.map (fun (k, v) -> (k, Seq.toArray v))
        |> Seq.toArray

module Seq = 
    /// Return x, f(x), f(f(x)), f(f(f(x))), ...
    let iterate f x = x |> Seq.unfold (fun x -> Some(x, f x))

/// Compute the norm distance between an input and a Centroid
/// using the double-pipe operator
let distance (xs : Input<_>) (ys : Centroid) = 
    (xs.Features, ys)
    ||> Array.map2 (fun x y -> (x - y) * (x - y))
    |> Array.sum

/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise,
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (_, group : Input<_> []) = 
    let e0 = group.[0].Features
    [| for i in 0..e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

/// Group all the inputs by nearest centroid
let classifyIntoGroups inputs centroids = inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = 
    seq { 
        let classification = classifyIntoGroups inputs centroids
        yield classification
        let newCentroids = Array.map computeCentroidOfGroup classification
        yield! computeCentroids inputs newCentroids
    }

/// Extract the features and repeatedly classify the inputs, starting with the
/// inital centroids
let kmeans inputs featureExtractor initialCentroids = 
    let inputsWithFeatures = 
        inputs
        |> Seq.map (fun i -> 
               { Data = i
                 Features = featureExtractor i })
        |> Seq.toArray
    
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputsWithFeatures initialCentroids

open FSharp.Data.UnitSystems.SI.UnitSymbols

type Observation = 
    { Time : float<s>
      Location : float<m> }

let rnd = System.Random()
let rand() = rnd.NextDouble()
let randZ() = rnd.NextDouble() - 0.5

/// Create a point near the given point
let near p = 
    { Time = p.Time + randZ() * 20.0<s>
      Location = p.Location + randZ() * 5.0<m> }

let data = 
    [ 
        for i in 1 .. 1000 -> near { Time = 100.0<s>; Location = 60.0<m> }
        for i in 1 .. 1000 -> near { Time = 120.0<s>; Location = 80.0<m> }
        for i in 1 .. 1000 -> near { Time = 180.0<s>; Location = 30.0<m> }
        for i in 1 .. 1000 -> near { Time = 70.0<s>; Location = 40.0<m> }
        ]

let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

let initialCentroids = [for i in 0..9 -> [| rand(); rand(); |]]
let featureExtractor (p: Observation) = [| p.Time / maxTime; p.Location / maxLoc |]

kmeans data featureExtractor initialCentroids

kmeans data featureExtractor initialCentroids
|> Seq.map (Array.map (fun (c,_) -> c.[0] * maxTime, c.[1] * maxLoc))
|> Seq.item 100