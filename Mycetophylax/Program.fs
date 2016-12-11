// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System.Collections.Generic

type ObiektDanych = ObiektDanych of float[]
type Mrowka = {Id: int; Klasa: int; Dane: ObiektDanych}
    
type Przestrzen = Mrowka option[,]
    
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

let Modulo n m = ((n % m) + m) % m

let SasiedztwoMoore'a s_x s_y dlugoscBoku pozycjaMrowki =
    let m_x, m_y = pozycjaMrowki
    let modulo x = Modulo x dlugoscBoku 
    // SŁABO: porównanie przy tworzeniu każdego pola
    // Do poprawy, choć bez continue to nie będzie ładna poprawka ;-(
    [for x in (m_x-s_x)..(m_x+s_x) do
        for y in (m_y-s_y)..(m_y+s_y) do
            if (x, y) <> (m_x, m_y) then yield (x, y) ] 
    |> List.map (fun (x, y) -> (modulo x, modulo y))

let MrowkiZSasiedztwa (przestrzen:Przestrzen) sasiedztwo =
    List.choose (fun (x, y) -> przestrzen.[x, y]) sasiedztwo

///////
// Aktywacja
///////

let StalaPresja _ = 2.0
let PrawdopodobienstwoAktywacji = 0.1

let SzansaAktywacji pAktywacji funPresji funOceny czas agent = 
    let presja = funPresji(czas)
    let licznik = pAktywacji ** presja
    let wplywOceny = (funOceny agent) ** presja
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
    let ocen (przestrzen:Przestrzen) pozycjaMrowki =
        let badanaMrowka = pozycjaMrowki ||> Array2D.get przestrzen |> Option.get
        let mrowkiWSasiedztwie = funSasiedztwa pozycjaMrowki |> MrowkiZSasiedztwa przestrzen
        let ocenaSkladowa mrowka2 = 1.0 - pobierzOdleglosc badanaMrowka mrowka2
        let sumaOdleglosci = List.map ocenaSkladowa mrowkiWSasiedztwie |> List.sum
        sumaOdleglosci / dzielnik
    ocen
    



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
