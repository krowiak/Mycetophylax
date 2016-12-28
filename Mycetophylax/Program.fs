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
// Standaryzacja
///////

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
        

///////
// Skalowanie
///////
    
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

let TworzPresjeZaleznaOdCzasu (srednieOcenyDlaT:SrednieOcenyDlaCzasu) k_lambda t_max =
    let funPresji t =
        let wplywParametru = k_lambda / srednieOcenyDlaT.[t]
        let wplywCzasu = (float t_max / float t) |> log10
        2.0 + wplywParametru * wplywCzasu
    funPresji

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

let TworzFunkcjeOceny s_x s_y funSasiedztwa (mapaOdleglosci:IDictionary<int*int, float>) funSredniejOdleglosci =
    let s_x', s_y' = float s_x, float s_y 
    let dzielnik = (2.0*s_x' + 1.0) * (2.0*s_y' + 1.0)
    let pobierzOdleglosc {Id=m1} {Id=m2} =
        let klucz = if m1 < m2 then m1, m2 else m2, m1
        mapaOdleglosci.[klucz]
    let ocen (przestrzen:Przestrzen) badanaMrowka pozycjaDoOceny =
        let mrowkiWSasiedztwie = funSasiedztwa pozycjaDoOceny |> MrowkiZSasiedztwa przestrzen
        let ocenaSkladowa mrowka2 = 1.0 - ((pobierzOdleglosc badanaMrowka mrowka2) / (funSredniejOdleglosci badanaMrowka))
        let sumaOdleglosci = Seq.map ocenaSkladowa mrowkiWSasiedztwie |> Seq.sum
        max 0.0 (sumaOdleglosci / dzielnik)
    ocen 

///////
// Średnie odległości między agentami
///////

/////////// Chyba do poprawy? Uwzględnia odległość mrówki do siebie samej.
/////////// (ten wzór jest trosik niejasny)
let TworzStalaSredniaOdlegloscPomiedzyAgentami funOdleglosci mrowki =
    let liczbaMrowek = Seq.length mrowki
    let mutable sumaOdleglosci = 0.0
    for {Dane=mrowka1} in mrowki do
        for {Dane=mrowka2} in mrowki do
            sumaOdleglosci <- sumaOdleglosci + funOdleglosci mrowka1 mrowka2
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

/////////
// Określa klasę nawet, jeśli mrówka nie śpi.
// A to ci ambaras.
////////
let OkreslNowaKlaseMrowki (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka =
    let _, klasaLiczba = 
        MrowkiZSasiedztwa przestrzen sasiedztwo
        |> Seq.countBy (fun mrowka -> slownikKlas.[mrowka])
        |> Seq.groupBy (fun (_, liczba) -> liczba)
        |> Seq.maxBy (fun (liczba, _) -> liczba)    

    if Seq.length klasaLiczba = 1
    then (Seq.head klasaLiczba) |> fst
    else 
        let klasaMrowki = slownikKlas.[mrowka]
        match Seq.tryFind (fun (klasa, _) -> klasaMrowki = klasa) klasaLiczba with
        | Some _ -> klasaMrowki
        | None -> (Seq.head klasaLiczba) |> fst

/////////
// Określa klasę nawet, jeśli mrówka nie śpi.
// A to ci ambaras.
////////
let ZmienKlaseMrowki (przestrzen:Przestrzen) (slownikKlas:IDictionary<Mrowka, int>) sasiedztwo mrowka =
    let klasa = OkreslNowaKlaseMrowki przestrzen slownikKlas sasiedztwo mrowka
    slownikKlas.[mrowka] <- klasa

///////
// Inne Cuda oraz Dziwy
///////

let wypiszPrzestrzen (przestrzen:Przestrzen) funkcjaReprezentacjiMrowki =
    let dlugoscBoku = Array2D.length1 przestrzen
    printf "  "
    for y=0 to dlugoscBoku - 1 do
        printf "%5i" y
    printfn ""
    for x=0 to dlugoscBoku - 1 do
        printf "%3i" x
        for y=0 to dlugoscBoku - 1 do
            let mozeMrowka = przestrzen.[x, y]
            match mozeMrowka with
            | Some mrowka -> funkcjaReprezentacjiMrowki mrowka |> printf "|%3s|"
            | None -> printf "|   |"
        printfn "" 

let WypiszKlasyWPrzestrzeni (klasy:IDictionary<Mrowka, int>) (przestrzen:Przestrzen) =
    let okreslSymbolKlasyMrowki mrowka = klasy.[mrowka].ToString()
    wypiszPrzestrzen przestrzen okreslSymbolKlasyMrowki

let WypiszMrowkiWPrzestrzeni (przestrzen:Przestrzen) =
    wypiszPrzestrzen przestrzen (fun {Id=idMrowki} -> idMrowki.ToString())

let Grupuj (przestrzen:Przestrzen) mrowki liczbaIteracji (los:Random) debug =
    let klasyMrowek = new Dictionary<Mrowka, int>() :> IDictionary<Mrowka, int>
    for mrowka in mrowki do
        klasyMrowek.Add(mrowka, mrowka.Id)
    let srednieOcenyDlaT = new Dictionary<int, float>() :> IDictionary<int, float>

    let bokPrzestrzeni = Array2D.length1 przestrzen
    let s_x, s_y = 2, 2
    let funSasiedztwa = SasiedztwoMoore'a s_x s_y bokPrzestrzeni
    let funOdleglosci = OdlegloscEuklidesowa
    let sloOdleglosci = TworzSlownikOdleglosci mrowki funOdleglosci
    let funSredniejOdleglosci = TworzSredniaOdlegloscDlaKazdegoAgenta funOdleglosci mrowki //TworzStalaSredniaOdlegloscPomiedzyAgentami funOdleglosci mrowki
    let funOceny = TworzFunkcjeOceny s_x s_y funSasiedztwa sloOdleglosci funSredniejOdleglosci 
    let funPresji = TworzPresjeZaleznaOdCzasu srednieOcenyDlaT 1.0 liczbaIteracji
    let funPrawdopAktywacji = SzansaAktywacji PRAWDOP_AKTYWACJI funPresji //StalaPresja
    let szansaNaZachlannosc = 0.9
    let funPrzemieszczenia przestrzen = PrzemiescMrowkeZachlannie (funOceny przestrzen) szansaNaZachlannosc los przestrzen
    let funKlasy przestrzen = OkreslNowaKlaseMrowki przestrzen klasyMrowek
    let funZmianyKlasy przestrzen = ZmienKlaseMrowki przestrzen klasyMrowek
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
            then funPrzemieszczenia nastepnaPrzestrzen sasiedztwo pole
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
    use dane = System.IO.File.OpenRead(@"E:\Pobrane\irisBezSmieci.data")
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
