# 2. faza: Uvoz podatkov

source("lib/libraries.r")

sl <- locale("sl", decimal_mark=",", grouping_mark=".")

#1. Tabela najemniških poslov:



for (i in 2013:2020){

  najem.delistavb = read_csv2(sprintf("podatki/ETN_SLO_NAJ_%s_20211127/ETN_SLO_NAJ_%s_delistavb_20211127.csv",i, i))
  
  najem.posli = read_csv2(sprintf("podatki/ETN_SLO_NAJ_%s_20211127/ETN_SLO_NAJ_%s_posli_20211127.csv", i, i))
  
  zdruzeno = left_join(najem.posli, najem.delistavb, by="ID Posla")
  
  
  
  zdruzeno = zdruzeno %>% relocate(
    "id.posla" = "ID Posla",
    "obcina" = colnames(zdruzeno)[22],
    "mesecna.najemnina" = "Pogodbena najemnina",
    "povrsina" = "Površina oddanih prostorov",
    "uporabna.povrsina" = "Uporabna površina dela stavbe",
    "tip.stavbe" = "Vrsta oddanih prostorov",
    "leto.izgradnje" = "Leto izgradnje stavbe",
    )
  
  #odstranimo veckratne vnose in odstranimo nezeljene podatke:
  prestej.vnose = function(v){
    stetje = c()
    vektor = v
    for (el in vektor){
      stetje = c(stetje, length(which(vektor == el)))
    }
    return(stetje)
  }
    
  zdruzeno = zdruzeno %>% filter(prestej.vnose(id.posla) == 1) %>% 
    select(id.posla, obcina, mesecna.najemnina, povrsina, uporabna.povrsina, tip.stavbe, leto.izgradnje) %>%
    drop_na()
  
  #dodamo letnico:
  
  zdruzeno = zdruzeno %>% mutate(leto.posla = i)
  
  if (i == 2013){
    tabela.najemnin = zdruzeno
  } else{
    tabela.najemnin = rbind(tabela.najemnin, zdruzeno)
  }
  
}
  
  
#2. Tabela kupoprodajnih poslov:

for (i in 2013:2020){
  
  nakup.delistavb = read_csv2(sprintf("podatki/ETN_SLO_KUP_%s_20211127/ETN_SLO_KUP_%s_delistavb_20211127.csv",i, i))
  
  nakup.posli = read_csv2(sprintf("podatki/ETN_SLO_KUP_%s_20211127/ETN_SLO_KUP_%s_posli_20211127.csv", i, i))
  
  zdruzeno = left_join(nakup.posli, nakup.delistavb, by="ID Posla")
  
  zdruzeno = zdruzeno %>% relocate(
    "id.posla" = "ID Posla",
    "obcina" = colnames(zdruzeno)[18],
    "prodajna.cena" = colnames(zdruzeno)[5],
    "povrsina" = colnames(zdruzeno)[34],
    "uporabna.povrsina" = colnames(zdruzeno)[47],
    "tip.stavbe" = "Vrsta dela stavbe",
    "leto.izgradnje" = "Leto izgradnje dela stavbe",
  )
  
  #odstranimo veckratne vnose in odstranimo nezeljene podatke:
  prestej.vnose = function(v){
    stetje = c()
    vektor = v
    for (el in vektor){
      stetje = c(stetje, length(which(vektor == el)))
    }
    return(stetje)
  }
  
  zdruzeno = zdruzeno %>% filter(prestej.vnose(id.posla) == 1) %>% 
    select(id.posla, obcina, prodajna.cena, povrsina, uporabna.povrsina, tip.stavbe, leto.izgradnje) %>%
    drop_na()
  
  #dodamo letnico:
  
  zdruzeno = zdruzeno %>% mutate(leto.posla = i)
  
  if (i == 2013){
    tabela.nakupov = zdruzeno
  } else{
    tabela.nakupov = rbind(tabela.nakupov, zdruzeno)
  }
  
}


#3 Tabela občin:

obcine = read_csv(
  "podatki/obcine.csv", 
  skip = 2,
  locale = locale(encoding = "Windows-1250"),
  col_types = cols(
    .default = col_guess(),
    "2008" = col_double(),
    "2009" = col_double(),
    "2010" = col_double(),
    "2011" = col_double(),
    "2012" = col_double(),
    "2013" = col_double(),
    "2014" = col_double(),
    "2021" = col_double()
  )
  )


obcine = pivot_longer(obcine,
                      cols = (colnames(obcine)[3:16]),
                      names_to = "leto",
                      values_to = "vrednost"
                      )




obcine_meritve1 = obcine %>% filter(MERITVE == "Površina (km2) - 1. januar") %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], povrsina = vrednost)
obcine_meritve2 = obcine %>% filter(MERITVE == "Število prebivalcev - 1. januar") %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], stevilo.prebivalcev = vrednost)
obcine_meritve3 = obcine %>% filter(MERITVE == "Skupni prirast") %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], skupni.prirast = vrednost)
obcine_meritve4 = obcine %>% filter(MERITVE == "Indeks staranja - 1. januar") %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], indeks.staranja = vrednost)
obcine_meritve5 = obcine %>% filter(MERITVE == obcine$MERITVE[14264]) %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], stevilo.studentov = vrednost)
obcine_meritve6 = obcine %>% filter(MERITVE == "Stopnja delovne aktivnosti (%)") %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], stopnja.delovne.aktivnosti = vrednost)
obcine_meritve7 = obcine %>% filter(MERITVE == obcine$MERITVE[20873]) %>% mutate(povrsina = vrednost) %>% select(leto, obcina = colnames(obcine)[2], bruto.mesecna.placa = vrednost)


zdruzi = obcine_meritve1
zdruzi = left_join(zdruzi, obcine_meritve2, by = c("leto", "obcina"))
zdruzi = left_join(zdruzi, obcine_meritve3, by = c("leto", "obcina"))
zdruzi = left_join(zdruzi, obcine_meritve4, by = c("leto", "obcina"))
zdruzi = left_join(zdruzi, obcine_meritve5, by = c("leto", "obcina"))
zdruzi = left_join(zdruzi, obcine_meritve6, by = c("leto", "obcina"))
obcine.koncno = left_join(zdruzi, obcine_meritve7, by = c("leto", "obcina"))










