module Standaryzacja

open Typy

// https://www.mathsisfun.com/data/standard-deviation-formulas.html
let OdchylenieStandardoweWymiaru (dane:float[]) =
    let dzielnik = Array.length dane |> float
    let srednia = Array.average dane
    let suma = 
        dane 
        |> Array.map (fun x_i -> (x_i - srednia)**2.0)
        |> Array.sum
    (suma / dzielnik) |> sqrt
    
// http://sebastianraschka.com/Articles/2014_about_feature_scaling.html#about-standardization
let ZScore srednia odchylenie x =
    (x - srednia) / odchylenie

let ZScoreWymiaru (dane:float[]) =
    let srednia = Array.average dane
    let odchylenie = OdchylenieStandardoweWymiaru dane
    dane |> Array.map (ZScore srednia odchylenie)

let StandaryzacjaZScore (dane:seq<ObiektDanych>) = 
    // Tablice NIE SĄ KOPIOWANE, dane pierwotne przepadają, BYĆ CZUJNYM
    let daneTab = Seq.map (fun (ObiektDanych d) -> d) dane |> Seq.toArray
    let dlugoscWektora = daneTab.[0].Length
    let pobierz indeks tablica = Array.get tablica indeks |> Some
    for wymiar in 0..(dlugoscWektora - 1) do
        let wartosciWymiaru = Array.choose (pobierz wymiar) daneTab
        let znormalizowaneWartosci = wartosciWymiaru |> ZScoreWymiaru
        for i in 0..(daneTab.Length - 1) do
            daneTab.[i].[wymiar] <- znormalizowaneWartosci.[i]
    Seq.map (fun tab -> ObiektDanych tab) daneTab

