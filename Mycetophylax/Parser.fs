module Parser

open System.IO
open System.Collections.Generic
open Typy

let czytajWiersze (strumienDanych:Stream) = seq {
    use sr = new StreamReader (strumienDanych)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let ParsujDane strumienDanych =
    czytajWiersze strumienDanych
    |> Seq.map (fun wiersz -> wiersz.Split([|','|])) 
    |> Seq.map (Array.map float)
    |> Seq.map ObiektDanych