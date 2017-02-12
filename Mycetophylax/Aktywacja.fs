module Aktywacja

open Typy

let PRAWDOP_AKTYWACJI = 0.1

let StalaPresja _ = 2.0

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