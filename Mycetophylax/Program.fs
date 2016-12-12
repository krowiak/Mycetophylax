// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.Collections.Generic
open Parser
open Typy
open Pomocnicze
    
///////
//  Odległość
///////

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

///////
// Normalizacja
///////
    
// http://stats.stackexchange.com/a/180249
let ZScoreBycMoze min max dana : float =
    (dana - min) / (max - min)

let ZScoreWymiaru min max (dane:float[]) =
    let zScoreWymiaru = ZScoreBycMoze min max
    Array.map zScoreWymiaru dane

let NormalizacjaWymiaruZScore (_:int) (dane:float[]) =
    let min = Array.min dane
    let max = Array.max dane
    ZScoreWymiaru min max dane

let NormalizacjaWymiaruZScoreWzgledem zakresy wymiar dane =
    let min, max = Map.find wymiar zakresy
    ZScoreWymiaru min max dane

let NormalizacjaPoWymiarach (normalizatorWymiaru:(int -> float[] -> float[])) (dane:seq<ObiektDanych>)  =
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
            
let NormalizacjaZScoreChyba : (seq<ObiektDanych> -> seq<ObiektDanych>) = 
    NormalizacjaPoWymiarach NormalizacjaWymiaruZScore

///////
// Przestrzen
///////

let BokPrzestrzeni liczbaMrowek = 
    let n = float liczbaMrowek
    let pierwiastek = sqrt n
    let zaokraglenie = ceil pierwiastek |> int
    2 * (zaokraglenie + 1)

let ZwrocPrzestrzen bok = 
    Array2D.init bok bok (fun _ _ -> (None:Mrowka option))

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

///////
// Aktywacja
///////

let StalaPresja _ = 2.0
let PRAWDOP_AKTYWACJI = 0.1

let SzansaAktywacji pAktywacji funPresji ocena czas = 
    let presja = funPresji czas
    let licznik = pAktywacji ** presja
    let wplywOceny = ocena ** presja
    licznik / (licznik + wplywOceny)

///////
// Ocena
///////

let TworzSlownikOdleglosci mrowki funOdleglosci =
    let slownik = new Dictionary<int * int, float>();
    let mrowkiArr = List.toArray mrowki
    let liczbaMrowek = Array.length mrowkiArr
    for i in 0..(liczbaMrowek-1) do
        for j in (i+1)..(liczbaMrowek-1) do
            let {Dane=daneI} = mrowki.[i]
            let {Dane=daneJ} = mrowki.[j]
            let odleglosc = funOdleglosci daneI daneJ
            slownik.Add((i, j), odleglosc)
    slownik

let TworzFunkcjeOceny s_x s_y funSasiedztwa (mapaOdleglosci:IDictionary<int*int, float>) =
    let s_x', s_y' = float s_x, float s_y 
    let dzielnik = (2.0*s_x' + 1.0) * (2.0*s_y' + 1.0)
    let pobierzOdleglosc {Id=m1} {Id=m2} =
        let klucz = if m1 < m2 then m1, m2 else m2, m1
        mapaOdleglosci.[klucz]
    let ocen (przestrzen:Przestrzen) badanaMrowka pozycjaDoOceny =
        let mrowkiWSasiedztwie = funSasiedztwa pozycjaDoOceny |> MrowkiZSasiedztwa przestrzen
        let ocenaSkladowa mrowka2 = 1.0 - pobierzOdleglosc badanaMrowka mrowka2
        let sumaOdleglosci = Seq.map ocenaSkladowa mrowkiWSasiedztwie |> Seq.sum
        sumaOdleglosci / dzielnik
    ocen    

///////
// Mrówki
///////

let tworzMrowke (id, objDanych) =
    {Id=id; Dane=objDanych}

let TworzMrowki obiektyDanych =
    obiektyDanych |> Seq.indexed |> Seq.map tworzMrowke

let RozmiescMrowki (los:Random) (przestrzen:Przestrzen) mrowki =
    let bokPrzestrzeni = Array2D.length1 przestrzen
    let generujPozycje () = los.Next(bokPrzestrzeni)
    for mrowka in mrowki do
        let mutable posX = generujPozycje()
        let mutable posY = generujPozycje()
        while przestrzen.[posX, posY] |> Option.isSome do
            posX <- generujPozycje()
            posY <- generujPozycje()
        przestrzen.[posX, posY] <- Some mrowka

let PrzemiescMrowkeLosowo (los:Random) (przestrzen:Przestrzen) sasiedztwo (mrowkaX, mrowkaY) =
    let pustePola = sasiedztwo |> Array.filter (fun (x, y) -> przestrzen.[x, y] |> Option.isNone)
    let liczbaPol = Array.length pustePola
    if liczbaPol > 0 then
        let mrowka = przestrzen.[mrowkaX, mrowkaY] |> Option.get
        przestrzen.[mrowkaX, mrowkaY] <- None
        let nowyX, nowyY = sasiedztwo.[los.Next(liczbaPol)]
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

/////////
// Określa klasę nawet, jeśli mrówka nie śpi.
// A to ci ambaras.
////////
let ZmienKlaseMrowki (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka =
    let klasa =
        MrowkiZSasiedztwa przestrzen sasiedztwo 
        |> Seq.countBy (fun mrowka -> slownikKlas.[mrowka])
        |> Seq.maxBy (fun (_, liczWystapien) -> liczWystapien)
        |> fst
    slownikKlas.[mrowka] <- klasa

///////
// Inne Cuda oraz Dziwy
///////

let WypiszKlasyWPrzestrzeni (klasy:IDictionary<Mrowka, int>) (przestrzen:Przestrzen) =
    let dlugoscBoku = Array2D.length1 przestrzen
    for x=0 to dlugoscBoku - 1 do
        for y=0 to dlugoscBoku - 1 do
            let mozeMrowka = przestrzen.[x, y]
            match mozeMrowka with
            | Some mrowka -> printf "|%3i|" klasy.[mrowka]
            | None -> printf "|   |"
        printfn "" 

let WypiszMrowkiWPrzestrzeni (przestrzen:Przestrzen) =
    let dlugoscBoku = Array2D.length1 przestrzen
    for x=0 to dlugoscBoku - 1 do
        for y=0 to dlugoscBoku - 1 do
            let mozeMrowka = przestrzen.[x, y]
            match mozeMrowka with
            | Some {Id=idMrowki} -> printf "|%3i|" idMrowki
            | None -> printf "|   |"
        printfn "" 

let Grupuj (przestrzen:Przestrzen) mrowki liczbaIteracji (los:Random) debug =
    let klasyMrowek = new Dictionary<Mrowka, int>() :> IDictionary<Mrowka, int>
    for mrowka in mrowki do
        klasyMrowek.Add(mrowka, mrowka.Id)

    let bokPrzestrzeni = Array2D.length1 przestrzen
    let s_x, s_y = 1, 1
    let funSasiedztwa = SasiedztwoMoore'a s_x s_y bokPrzestrzeni
    let sloOdleglosci = TworzSlownikOdleglosci mrowki OdlegloscEuklidesowa
    let funOceny = TworzFunkcjeOceny s_x s_y funSasiedztwa sloOdleglosci przestrzen
    let funPrawdopAktywacji = SzansaAktywacji PRAWDOP_AKTYWACJI StalaPresja
    let szansaNaZachlannosc = 0.5
    let funPrzemieszczenia = PrzemiescMrowkeZachlannie funOceny szansaNaZachlannosc los przestrzen
    let funZmianyKlasy = ZmienKlaseMrowki przestrzen klasyMrowek    

    for t=1 to liczbaIteracji do
        for pole in PrzestrzenNaSeqPol przestrzen do
            let x, y = pole
            match przestrzen.[x, y] with
            | Some mrowka ->
                let ocena = funOceny mrowka pole
                let pAktywacji = funPrawdopAktywacji ocena t
                let sasiedztwo = funSasiedztwa pole
                if los.NextDouble() <= pAktywacji
                then funPrzemieszczenia sasiedztwo pole
                else funZmianyKlasy sasiedztwo mrowka
            | None ->
                ()

        if debug then
            printfn "Iteracja %i zakończona" t
            WypiszMrowkiWPrzestrzeni przestrzen
            printfn ""
            Console.ReadKey() |> ignore

    klasyMrowek
            
    
///////
// A Niech To: przemieszczanie usuwa mrówkę przed określeniem sąsiedztwa -> mrówka może się przenieśc na pole, na którym już była.
// Bez tego mrówka próbuje się porównać sama ze sobą przy przemieszczaniu.
// Coś należy uczynić.
//////
            


[<EntryPoint>]
let main argv = 
    let maszynaLosujaca = new Random()
    use dane = System.IO.File.OpenRead(@"E:\Pobrane\irisBezSmieci.data")
    let wiersze = ParsujDane dane
    let znormalizowaneDane = wiersze |> NormalizacjaZScoreChyba
    let mrowki = znormalizowaneDane|> TworzMrowki |> Seq.toList
    let przestrzen = List.length mrowki |> BokPrzestrzeni |> ZwrocPrzestrzen
    RozmiescMrowki maszynaLosujaca przestrzen mrowki
    WypiszMrowkiWPrzestrzeni przestrzen
    System.Console.ReadKey() |> ignore
    printfn ""

    let debug = true
    let slownikKlas = Grupuj przestrzen mrowki 100 maszynaLosujaca debug
    
    System.Console.ReadKey() |> ignore
    WypiszMrowkiWPrzestrzeni przestrzen
    printfn ""
    System.Console.ReadKey() |> ignore
    WypiszKlasyWPrzestrzeni slownikKlas przestrzen
    System.Console.ReadKey() |> ignore

    //Seq.iter (printfn "%A") mrowki
    //printfn "%A" przestrzen
    // printfn "%A" argv
    0 // return an integer exit code
