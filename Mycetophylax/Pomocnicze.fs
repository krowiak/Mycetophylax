module Pomocnicze
open Typy
open System.Collections.Generic

let Modulo n m = ((n % m) + m) % m

let PlusLubMinus1 (maszynaLosujaca:System.Random) = maszynaLosujaca.Next(0, 2) * 2 - 1

let PrzestrzenNaSeqPol (przestrzen:Przestrzen) = seq {
    for x=0 to (Array2D.length1 przestrzen - 1) do
        for y=0 to (Array2D.length2 przestrzen - 1) do
            yield x, y
}