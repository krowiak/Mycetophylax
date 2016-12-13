module Typy

type ObiektDanych = ObiektDanych of float[]
type Mrowka = {Id: int; Dane: ObiektDanych}    
type Przestrzen = Mrowka option[,]
type SrednieOcenyDlaCzasu = System.Collections.Generic.IDictionary<int, float>