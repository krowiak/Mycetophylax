module Przemieszczanie

open System
open Typy
open Sasiedztwo

let PrzemiescMrowkeLosowo (los:Random) (przestrzen:Przestrzen) sasiedztwo (mrowkaX, mrowkaY) =
    let pustePola = sasiedztwo |> Array.filter (fun (x, y) -> przestrzen.[x, y] |> Option.isNone)
    let liczbaPol = Array.length pustePola
    if liczbaPol > 0 then
        let mrowka = przestrzen.[mrowkaX, mrowkaY] |> Option.get
        przestrzen.[mrowkaX, mrowkaY] <- None
        let nowyX, nowyY = pustePola.[los.Next(liczbaPol)]
        przestrzen.[nowyX, nowyY] <- Some mrowka

let przemiescZachlannieWlasciwe funOceny (przestrzen:Przestrzen) sasiedztwo (mrowkaX, mrowkaY) =
    let przemieszczanaMrowka = przestrzen.[mrowkaX, mrowkaY] |> Option.get
    let pusteSasiedztwo = sasiedztwo |> PustePolaZSasiedztwa przestrzen
    if Seq.length pusteSasiedztwo > 0 then
        przestrzen.[mrowkaX, mrowkaY] <- None
        let (najlepszeX, najlepszeY) = pusteSasiedztwo |> Seq.maxBy (funOceny przemieszczanaMrowka)
        przestrzen.[najlepszeX, najlepszeY] <- Some przemieszczanaMrowka

let PrzemiescMrowkeZachlannie funOceny szansaNaZachlannosc (los:Random) (przestrzen:Przestrzen) sasiedztwo (mrowkaX, mrowkaY) =
    if los.NextDouble() <= szansaNaZachlannosc 
    then przemiescZachlannieWlasciwe funOceny (przestrzen:Przestrzen) sasiedztwo (mrowkaX, mrowkaY)
    else PrzemiescMrowkeLosowo los przestrzen sasiedztwo (mrowkaX, mrowkaY)     
