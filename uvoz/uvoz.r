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
    tabela.najemnin = rbind(tabela1, zdruzeno)
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

  


