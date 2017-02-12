open System
open System.Collections.Generic
open Parser
open Typy
open Pomocnicze
open Odleglosc
open Standaryzacja
open Sasiedztwo
open Przestrzen
open Aktywacja
open Ocena
open Odleglosc.SrednieOdleglosci
open Przemieszczanie
open Klasy
open Mrowki

///////
// Cuda oraz Dziwy
///////

let Grupuj (przestrzen:Przestrzen) mrowki liczbaIteracji (los:Random) debug =
    let klasyMrowek = new Dictionary<Mrowka, int>() :> IDictionary<Mrowka, int>
    for mrowka in mrowki do
        klasyMrowek.Add(mrowka, mrowka.Id)
    let srednieOcenyDlaT = new Dictionary<int, float>() :> IDictionary<int, float>

    let bokPrzestrzeni = Array2D.length1 przestrzen
    let s_x, s_y = 1, 1
    let funSasiedztwa = SasiedztwoMoore'a s_x s_y bokPrzestrzeni
    let funOdleglosci = OdlegloscEuklidesowa
    let sloOdleglosci = TworzSlownikOdleglosci mrowki funOdleglosci
    let funSredniejOdleglosci = TworzSredniaOdlegloscDlaKazdegoAgenta funOdleglosci mrowki //TworzStalaSredniaOdlegloscPomiedzyAgentami funOdleglosci mrowki
    let funOceny = TworzFunkcjeOceny s_x s_y funSasiedztwa sloOdleglosci funSredniejOdleglosci 
    let funPresji = TworzPresjeZaleznaOdCzasu srednieOcenyDlaT 1.0 liczbaIteracji
    let funPrawdopAktywacji = SzansaAktywacji PRAWDOP_AKTYWACJI funPresji //StalaPresja
    let szansaNaZachlannosc = 0.9
    let funPrzemieszczenia przestrzen = PrzemiescMrowkeZachlannie (funOceny przestrzen) szansaNaZachlannosc los przestrzen
    let funOkreslKlaseMrowki = OkreslNowaKlaseMrowkiBasic
    let funKlasy przestrzen = funOkreslKlaseMrowki przestrzen klasyMrowek
    //let funKlasy przestrzen sasiedztwo mrowka = OkreslNowaKlaseMrowkiWSposobNiezgodnyZAlgorytmemONie przestrzen klasyMrowek sasiedztwo mrowka sloOdleglosci
    //let funZmianyKlasy przestrzen sasiedztwo mrowka = ZmienKlaseMrowki przestrzen klasyMrowek sasiedztwo mrowka funOkreslKlaseMrowki
    let mutable aktPrzestrzen = przestrzen

    for t=1 to liczbaIteracji do
        let nastepnaPrzestrzen = Array2D.copy aktPrzestrzen
        let mutable zmianyKlas = []

        let polaZMrowkami = 
            PrzestrzenNaSeqPol aktPrzestrzen 
            |> Seq.filter (PobierzZawartosc aktPrzestrzen >> Option.isSome)
            |> Seq.toList

        let ocenySrodowisk =
            polaZMrowkami
            |> List.map (fun pole -> PobierzZawartosc aktPrzestrzen pole |> Option.get, pole)
            |> List.map (fun (mrowka, pole) -> mrowka, funOceny aktPrzestrzen mrowka pole)
            |> Map.ofList
        srednieOcenyDlaT.[t] <- ocenySrodowisk |> Map.toSeq |> Seq.sumBy (fun (_, ocena) -> ocena)

        //////////
        /// Klasy zaczęły się zmieniać dla mrówek bez sąsiadów!
        /// Najpierw jest obliczana ocena, potem następuje przemieszczanie, więc czasem mrówka z oceną > 0 zostaje sama.
        /// Cóż za niefortunne zdarzenie.
        //////////
        for pole in polaZMrowkami do
            let x, y = pole
            let mrowka = aktPrzestrzen.[x, y] |> Option.get
            let ocena = Map.find mrowka ocenySrodowisk
            let pAktywacji = funPrawdopAktywacji ocena t
            let sasiedztwo = funSasiedztwa pole
            if los.NextDouble() <= pAktywacji
            then 
                funPrzemieszczenia nastepnaPrzestrzen sasiedztwo pole
                //zmianyKlas <- (mrowka, mrowka.Id)::zmianyKlas  // str. 7 - "class label same as original one"
            else zmianyKlas <- (mrowka, funKlasy aktPrzestrzen sasiedztwo mrowka)::zmianyKlas

        for (mrowka, klasa) in zmianyKlas do
            klasyMrowek.[mrowka] <- klasa
        aktPrzestrzen <- nastepnaPrzestrzen

        if debug then
            Console.Clear() |> ignore
            printfn "Iteracja %i zakończona" t
            WypiszKlasyWPrzestrzeni klasyMrowek aktPrzestrzen
            printfn ""
            System.Threading.Thread.Sleep(25)
            //Console.ReadKey() |> ignore

    aktPrzestrzen, klasyMrowek
            
    
///////
// A Niech To: przemieszczanie usuwa mrówkę przed określeniem sąsiedztwa -> mrówka może się przenieść na pole, na którym już była.
// Bez tego mrówka próbuje się porównać sama ze sobą przy przemieszczaniu.
// Coś należy uczynić.
//////
            


[<EntryPoint>]
let main argv = 
    let maszynaLosujaca = new Random()
    use dane = IO.File.OpenRead(@"E:\Pobrane\wine.data");
    //use dane = System.IO.File.OpenRead(@"E:\Dysk Google\Studia\Magisterskie\Praca magisterska\Dane, powiedzmy\irisBezSmieci.data")
    //use dane = System.IO.File.OpenRead(@"E:\Pobrane\gupieDane.txt")
    let wiersze = ParsujDane dane
    let przygotowaneDane = wiersze |> StandaryzacjaZScore
    //let przygotowaneDane = wiersze |> SkalowanieMinMax
    let mrowki = przygotowaneDane |> TworzMrowki |> Seq.toList

    let przestrzen = List.length mrowki |> BokPrzestrzeni |> ZwrocPrzestrzen
    RozmiescMrowki maszynaLosujaca przestrzen mrowki
    WypiszMrowkiWPrzestrzeni przestrzen
    printfn ""
    
    printfn "Grupowanie..."
    let debug = false
    let pogrupowanaPrzestrzen, slownikKlas = Grupuj przestrzen mrowki 5000 maszynaLosujaca debug
    
    printfn "Pogrupowano!"
    WypiszMrowkiWPrzestrzeni pogrupowanaPrzestrzen
    printfn ""
    WypiszKlasyWPrzestrzeni slownikKlas pogrupowanaPrzestrzen

    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
