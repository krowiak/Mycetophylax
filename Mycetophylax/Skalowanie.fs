module Skalowanie

open Typy
    
// http://sebastianraschka.com/Articles/2014_about_feature_scaling.html#about-standardization
let MinMax min max dana : float =
    (dana - min) / (max - min)

let MinMaxWymiaru min max (dane:float[]) =
    let zScoreWymiaru = MinMax min max
    Array.map zScoreWymiaru dane

let MinMaxWymiaruZScore (_:int) (dane:float[]) =
    let min = Array.min dane
    let max = Array.max dane
    MinMaxWymiaru min max dane

let MinMaxWymiaruWzgledem zakresy wymiar dane =
    let min, max = Map.find wymiar zakresy
    MinMaxWymiaru min max dane

let SkalowaniePoWymiarach (normalizatorWymiaru:(int -> float[] -> float[])) (dane:seq<ObiektDanych>)  =
    // Tablice NIE SĄ KOPIOWANE, dane pierwotne przepadają, BYĆ CZUJNYM
    let daneTab = Seq.map (fun (ObiektDanych d) -> d) dane |> Seq.toArray
    let dlugoscWektora = daneTab.[0].Length
    let pobierz indeks tablica = Array.get tablica indeks |> Some
    for wymiar in 0..(dlugoscWektora - 1) do
        let wartosciWymiaru = Array.choose (pobierz wymiar) daneTab
        let znormalizowaneWartosci = wartosciWymiaru |> normalizatorWymiaru wymiar
        for i in 0..(daneTab.Length - 1) do
            daneTab.[i].[wymiar] <- znormalizowaneWartosci.[i]
    Seq.map (fun tab -> ObiektDanych tab) daneTab
            
let SkalowanieMinMax : (seq<ObiektDanych> -> seq<ObiektDanych>) = 
    SkalowaniePoWymiarach MinMaxWymiaruZScore

