module Ocena

open System.Collections.Generic
open Typy
open Pomocnicze
open Sasiedztwo

let TworzSlownikOdleglosci mrowki funOdleglosci =
    let slownik = new Dictionary<int * int, float>();
    let mrowkiArr = List.toArray mrowki
    let liczbaMrowek = Array.length mrowkiArr
    for i in 0..(liczbaMrowek-1) do
        for j in (i+1)..(liczbaMrowek-1) do
            let {Dane=daneI} = mrowki.[i]
            let {Dane=daneJ} = mrowki.[j]
            let odleglosc = funOdleglosci daneI daneJ
            slownik.Add((i, j), odleglosc)
    slownik

let TworzFunkcjeOceny s_x s_y funSasiedztwa (mapaOdleglosci:IDictionary<int*int, float>) funSredniejOdleglosci =
    let s_x', s_y' = float s_x, float s_y 
    let dzielnik = (2.0*s_x' + 1.0) * (2.0*s_y' + 1.0)
    let pobierzOdleglosc = PobierzOdleglosc mapaOdleglosci
    let ocen (przestrzen:Przestrzen) badanaMrowka pozycjaDoOceny =
        let mrowkiWSasiedztwie = funSasiedztwa pozycjaDoOceny |> MrowkiZSasiedztwa przestrzen
        let ocenaSkladowa mrowka2 = 1.0 - ((pobierzOdleglosc badanaMrowka mrowka2) / (funSredniejOdleglosci badanaMrowka))
        let sumaOdleglosci = Seq.map ocenaSkladowa mrowkiWSasiedztwie |> Seq.sum
        max 0.0 (sumaOdleglosci / dzielnik)
    ocen 

