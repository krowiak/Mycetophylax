module Klasy

open Sasiedztwo
open Typy
open System.Collections.Generic
open Pomocnicze

let OkreslNowaKlaseMrowkiUltimate (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka =
    let _, klasaLiczba = 
        MrowkiZSasiedztwa przestrzen sasiedztwo
        |> Seq.countBy (fun mrowka -> slownikKlas.[mrowka])
        |> Seq.groupBy (fun (_, liczba) -> liczba)
        |> Seq.maxBy (fun (liczba, _) -> liczba)    

    if Seq.length klasaLiczba = 1
    then (Seq.head klasaLiczba) |> fst
    else 
        let klasaMrowki = slownikKlas.[mrowka]
        match Seq.tryFind (fun (klasa, _) -> klasaMrowki = klasa) klasaLiczba with
        | Some _ -> klasaMrowki
        | None -> (Seq.head klasaLiczba) |> fst

let OkreslNowaKlaseMrowkiBasic (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka =        
    MrowkiZSasiedztwa przestrzen sasiedztwo 
    |> Seq.countBy (fun mrowka -> slownikKlas.[mrowka])
    |> Seq.maxBy (fun (_, liczWystapien) -> liczWystapien)
    |> fst

let OkreslNowaKlaseMrowkiWSposobNiezgodnyZAlgorytmemONie (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka (slownikOdleglosci:IDictionary<int*int, float>) =
    let najpodobniejszaZMrowek = 
        MrowkiZSasiedztwa przestrzen sasiedztwo
        |> Seq.map (fun mrowka2 -> mrowka2, PobierzOdleglosc slownikOdleglosci mrowka mrowka2)
        |> Seq.minBy (fun (_, ocena) -> ocena)
        |> fst
    slownikKlas.[najpodobniejszaZMrowek]    

/////////
// Określa klasę nawet, jeśli mrówka nie śpi.
// A to ci ambaras.
////////
let ZmienKlaseMrowki (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka okreslKlaseMrowki =
    let klasa = okreslKlaseMrowki przestrzen slownikKlas sasiedztwo mrowka
    slownikKlas.[mrowka] <- klasa