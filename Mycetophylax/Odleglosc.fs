module Odleglosc

open Typy

let ZabezpieczonaOdleglosc funOdleglosci d1 d2 =
    let (ObiektDanych x1), (ObiektDanych x2) = d1, d2
    if x1.Length <> x2.Length
    then invalidArg "x1, x2" "Długości wektorów nie są równe"
    else funOdleglosci d1 d2

let OdlegloscEuklidesowa (ObiektDanych x1) (ObiektDanych x2) =
    let dlugosc = x1.Length
    let mutable odleglosc = 0.0
    for i in 0..(dlugosc - 1) do
        let roznica = (x1.[i] - x2.[i])
        odleglosc <- roznica**2.0
    sqrt odleglosc

module SrednieOdleglosci =
    open System.Collections.Generic

    /////////// Uwzględnia odległość mrówki do siebie samej.
    /////////// (ten wzór jest trosik niejasny)
    let TworzStalaSredniaOdlegloscPomiedzyAgentamiPrawdopodobnieZle funOdleglosci mrowki =
        let liczbaMrowek = Seq.length mrowki
        let mutable sumaOdleglosci = 0.0
        for {Dane=mrowka1} in mrowki do
            for {Dane=mrowka2} in mrowki do
                sumaOdleglosci <- sumaOdleglosci + funOdleglosci mrowka1 mrowka2
        let najsredniejszaOdleglosc = sumaOdleglosci / float (liczbaMrowek * (liczbaMrowek - 1))
        fun mrowka -> najsredniejszaOdleglosc

    /////////// Nie uwzględnia odległości mrówki do siebie samej.
    let TworzStalaSredniaOdlegloscPomiedzyAgentami funOdleglosci mrowki =
        let liczbaMrowek = Seq.length mrowki
        let mutable sumaOdleglosci = 0.0
        for mrowka1 in mrowki do
            for mrowka2 in mrowki do
                if mrowka1.Id <> mrowka2.Id
                then sumaOdleglosci <- sumaOdleglosci + funOdleglosci mrowka1.Dane mrowka2.Dane
        let najsredniejszaOdleglosc = sumaOdleglosci / float (liczbaMrowek * (liczbaMrowek - 1))
        fun mrowka -> najsredniejszaOdleglosc

    let TworzSredniaOdlegloscDlaKazdegoAgenta funOdleglosci mrowki =
        let slownikOdleglosci = new Dictionary<Mrowka, float>()
        let liczbaMrowek = Seq.length mrowki
        for mrowka1 in mrowki do
            let mutable sumaOdleglosci = 0.0
            for mrowka2 in mrowki do
                if mrowka1 <> mrowka2
                then sumaOdleglosci <- sumaOdleglosci + funOdleglosci mrowka1.Dane mrowka2.Dane
            let sredniaOdleglosc = sumaOdleglosci / (float liczbaMrowek - 1.0)
            slownikOdleglosci.Add(mrowka1, sredniaOdleglosc)
        fun mrowka -> slownikOdleglosci.[mrowka]

    let TworzSredniaOdlegloscZaleznaOdCzasu (srednieOcenyDlaT:SrednieOcenyDlaCzasu) delta_t k_alfa poczWartosc =
        let historia = new Dictionary<int, float>()
        let funAlfa t =
            let wynik = 
                if t <= delta_t
                then poczWartosc - k_alfa * poczWartosc // ??????????????????
                else 
                    let stareT = t - delta_t
                    let wartHistoryczna = historia.[stareT]
                    let roznicaSrednichOcen = srednieOcenyDlaT.[t] - srednieOcenyDlaT.[stareT]
                    wartHistoryczna - k_alfa * roznicaSrednichOcen
            historia.[t] <- wynik
            wynik
        funAlfa