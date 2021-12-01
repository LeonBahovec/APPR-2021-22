# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza nepremičninskega trga v Sloveniji

V projektni nalogi bom analiziral nepremičninski trg v Sloveniji. Predvsem se bom osredotočil na višino najemnin in pogodbenih cen pri nepremičninskih poslih. Poizkusil  bom ugotoviti, kaj naj bi najbolj ugodno vplivalo na rentabilnost morebitne naložbe v nepremičnino. Tu si nameravam tudi bolj podrobno ogledati demografske kazalce v posamezni občini in poizkusil ugotoviti, kako slednji vplivajo na višine najemnin v njej.

Podatke o nepremičninskih poslih bom pridobil iz evidence trga nepremičnin, ki jih zagotavlja [GURS](https://egp.gu.gov.si/egp/). Podatke o demografskih kazalcih pa v [SURS-ovi podatkovni bazi](https://pxweb.stat.si/SiStatData/pxweb/sl/Data/Data/2640010S.px/). Shranjeni so v datoteki **podatki**.

Za analizo bi podatke opazoval v naslednjih tabelah:

1. Tabela najemnih poslov:
- občina *chr*
- mesecna najemnina *dbl*
- leto posla *dbl*
- površina *dbl*
- uporabna površina *dbl* 
- tip stavbe (poslovni prostor, bivalni prostor,...) *fctr*
- leto izgradnje *dbl*


2. Tabela kupoprodajnih poslov:
- občina *chr*
- pogodbena cena *dbl*
- leto posla *dbl*
- površina *dbl*
- uporabna površina *dbl* 
- tip stavbe (poslovni prostor, bivalni prostor,...) *fctr*
- leto izgradnje *dbl*

3. Tabela občin:
- občina *chr*
- površina občine *dbl*
- število prebivalcev *dbl*
- indeks staranja *dbl*
- število študentov *dbl*
- stopnja delovne aktivnosti *dbl*
- povprečna mesečna bruto plača *dbl*



## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
