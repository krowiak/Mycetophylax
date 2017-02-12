module Mrowki

open System
open Typy

let tworzMrowke (id, objDanych) =
    {Id=id; Dane=objDanych}

let TworzMrowki obiektyDanych =
    obiektyDanych |> Seq.indexed |> Seq.map tworzMrowke

let RozmiescMrowki (los:Random) (przestrzen:Przestrzen) mrowki =
    let bokPrzestrzeni = Array2D.length1 przestrzen
    let generujPozycje () = los.Next(bokPrzestrzeni)
    for mrowka in mrowki do
        let mutable posX = generujPozycje()
        let mutable posY = generujPozycje()
        while przestrzen.[posX, posY] |> Option.isSome do
            posX <- generujPozycje()
            posY <- generujPozycje()
        przestrzen.[posX, posY] <- Some mrowka
