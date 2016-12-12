module Pomocnicze
open Typy

let Modulo n m = ((n % m) + m) % m

let PlusLubMinus1 (maszynaLosujaca:System.Random) = maszynaLosujaca.Next(0, 2) * 2 - 1

let PrzestrzenNaSeqPol (przestrzen:Przestrzen) = seq {
    for x=0 to Array2D.length1 przestrzen do
        for y=0 to Array2D.length2 przestrzen do
            yield x, y
}