module Typy

type ObiektDanych = ObiektDanych of float[]
type Mrowka = {Id: int; Klasa: int; Dane: ObiektDanych}    
type Przestrzen = Mrowka option[,]