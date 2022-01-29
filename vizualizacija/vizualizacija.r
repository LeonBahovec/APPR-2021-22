# 3. faza: Vizualizacija podatkov

#Če pogledamo povprecne najemnine glede na obcino hitro opazimo, da so nekater podatki vnešeni v etn
#verjetno popačeni. Npr. dejstvo , da je povprecna mesečna najemnina v Izoli 68 000 €.
#Če pogledamo nazaj v tabelo najemnin opazimo, da je nekje v podatkih navedena 86 milijonska najemnina,
#kar je skoraj gotovo napačno. Primerov, kjer so navedene najemnine očitno napačne je več, podbne primere
#pa najdemo tudi v tabeli nakupov. Veliko pa je tudi primerov, kjer so najemnine/prodajne cene 0€ ali pa nekaj centov.
#Tudi tu gre verjetno za neke fiktivne posle. Nesmiselne oz. ničelne vnose najdemo tudi pri površinah.
#Zato sem se v tabeli nakupov odločil odstraniti vse kupoprodajne posle s ceno nad 10 000 € na kvadratni meter.
#V tabeli najemnin pa vse posle z mesecno najemnino nad 





#obcine.najemnine = tabela.najemnin %>% filter(tip.prostora == "Stanovanje")  %>% group_by(obcina, leto.posla) %>% mutate(najemnina.na.kvadratni.meter = mean(mesecna.najemnina / povrsina)) %>%
#  select(obcina, najemnina.na.kvadratni.meter, leto.posla) %>% distinct()
#
#obcine.nakupi = tabela.nakupov %>% filter(tip.prostora == "Stanovanje")  %>% group_by(obcina, leto.posla) %>% mutate(cena.na.kvadratni.meter = mean(prodajna.cena / povrsina)) %>%
#  select(obcina, cena.na.kvadratni.meter, leto.posla) %>% distinct()
#
#tabela.obcin.razsirjeno = left_join(tabela.obcin, obcine.nakupi) %>% left_join(obcine.najemnine)
#
#nakupi.najemnine = left_join(obcine.nakupi, obcine.najemnine) %>% 
#  mutate(razmerje.najemnine.in.cene = cena.na.kvadratni.meter / najemnina.na.kvadratni.meter)


povprecna.najemnina.tip.prostora = tabela.najemnin %>% group_by(tip.prostora) %>%
  mutate(povprecna.najemnina.na.kv.meter = mean(mesecna.najemnina / povrsina)) %>% select(tip.prostora, povprecna.najemnina.na.kv.meter) %>% distinct()

povprecna.cena.tip.prostora = tabela.nakupov %>% group_by(tip.prostora) %>%
  mutate(povprecna.cena.na.kv.meter = mean(prodajna.cena / povrsina)) %>% select(tip.prostora, povprecna.cena.na.kv.meter) %>% distinct()

povprecna.rentabilnost = povprecna.cena.tip.prostora %>% left_join(povprecna.najemnina.tip.prostora) %>% 
  mutate(povprecna.rentabilnost = 1200 * povprecna.najemnina.na.kv.meter / povprecna.cena.na.kv.meter)

povprecna.rentabilnost %>% arrange(povprecna.rentabilnost) %>% ggplot(mapping = aes(x = reorder(tip.prostora, povprecna.rentabilnost), y = povprecna.rentabilnost)) + 
  geom_bar(stat="identity") + labs(x = "Tip prostora", y = "Letna rentabilnost (%)", title = "Rentabilnost glede na tip prostora") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






















