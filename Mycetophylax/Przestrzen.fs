module Przestrzen

open Typy
open System.Collections.Generic

let BokPrzestrzeni liczbaMrowek = 
    let n = float liczbaMrowek
    let pierwiastek = sqrt n
    let zaokraglenie = ceil pierwiastek |> int
    2 * (zaokraglenie + 1)

let ZwrocPrzestrzen bok = 
    Array2D.init bok bok (fun _ _ -> (None:Mrowka option))    

let wypiszPrzestrzen (przestrzen:Przestrzen) funkcjaReprezentacjiMrowki =
    let dlugoscBoku = Array2D.length1 przestrzen
    printf "  "
    for y=0 to dlugoscBoku - 1 do
        printf "%5i" y
    printfn ""
    for x=0 to dlugoscBoku - 1 do
        printf "%3i" x
        for y=0 to dlugoscBoku - 1 do
            let mozeMrowka = przestrzen.[x, y]
            match mozeMrowka with
            | Some mrowka -> funkcjaReprezentacjiMrowki mrowka |> printf "|%3s|"
            | None -> printf "|   |"
        printfn "" 

let WypiszKlasyWPrzestrzeni (klasy:IDictionary<Mrowka, int>) (przestrzen:Przestrzen) =
    let okreslSymbolKlasyMrowki mrowka = klasy.[mrowka].ToString()
    wypiszPrzestrzen przestrzen okreslSymbolKlasyMrowki

let WypiszMrowkiWPrzestrzeni (przestrzen:Przestrzen) =
    wypiszPrzestrzen przestrzen (fun {Id=idMrowki} -> idMrowki.ToString())