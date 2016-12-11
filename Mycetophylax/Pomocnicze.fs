module Pomocnicze

let Modulo n m = ((n % m) + m) % m
let PlusLubMinus1 (maszynaLosujaca:System.Random) = maszynaLosujaca.Next(0, 2) * 2 - 1
