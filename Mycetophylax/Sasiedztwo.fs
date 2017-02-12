module Sasiedztwo

open Pomocnicze
open Typy

let SasiedztwoMoore'a s_x s_y dlugoscBoku pozycjaMrowki =
    let m_x, m_y = pozycjaMrowki
    let modulo x = Modulo x dlugoscBoku 
    // SŁABO: porównanie przy tworzeniu każdego pola
    // Do poprawy, choć bez continue to nie będzie ładna poprawka ;-(
    [|for x in (m_x-s_x)..(m_x+s_x) do
        for y in (m_y-s_y)..(m_y+s_y) do
            if (x, y) <> (m_x, m_y) then yield (x, y) |] 
    |> Array.map (fun (x, y) -> (modulo x, modulo y))

let MrowkiZSasiedztwa (przestrzen:Przestrzen) sasiedztwo =
    Seq.choose (fun (x, y) -> przestrzen.[x, y]) sasiedztwo

let PustePolaZSasiedztwa (przestrzen:Przestrzen) sasiedztwo =
    sasiedztwo |> Seq.filter (fun (x, y) -> przestrzen.[x, y] |> Option.isNone)

let PolaMrowekZSasiedztwa (przestrzen:Przestrzen) sasiedztwo =
    sasiedztwo |> Seq.filter (fun (x, y) -> przestrzen.[x, y] |> Option.isSome)

