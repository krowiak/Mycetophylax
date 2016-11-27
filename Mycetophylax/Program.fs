// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type ObiektDanych = ObiektDanych of float[]
type Mrowka = {Id: int; Klasa: int; Dane: ObiektDanych}
    
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


///////
// Aktywacja
///////

let StalaPresja _ = 2.0
let PrawdopodobienstwoAktywacji = 0.1

let SzansaAktywacji pAktywacji funPresji funOceny czas agent = 
    let presja = funPresji(czas)
    let licznik = pAktywacji ** presja
    let wplywOceny = funOceny(agent) ** presja
    licznik / (licznik + wplywOceny)


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
