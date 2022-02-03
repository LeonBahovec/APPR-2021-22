# 3. faza: Vizualizacija podatkov

##rentabilnost, cene in najemnine skozi cas glede na tip prostora


tip = "Stanovanje"

graf.skozi.cas = function(tip, aspekt){
  if (tip != "Vse"){
    najemnine.skozi.cas = tabela.najemnin %>% filter(tip.prostora == tip) %>% group_by(leto.posla) %>%
      mutate(povprecna.najemnina = mean(mesecna.najemnina / povrsina)) %>% select(leto.posla, povprecna.najemnina) %>% distinct()
    
    cene.skozi.cas = tabela.nakupov %>% filter(tip.prostora == tip) %>% group_by(leto.posla) %>%
      mutate(povprecna.cena = mean(prodajna.cena / povrsina)) %>% select(leto.posla, povprecna.cena) %>% distinct()
    
    cas = cene.skozi.cas %>% left_join(najemnine.skozi.cas)
    cas = cas %>% mutate("rentabilnost (%)" = 1200 * povprecna.najemnina / povprecna.cena)
    if (aspekt == "rentabilnost"){
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = `rentabilnost (%)`)) +
        geom_line() + labs(x = "Leto", y = "Rentabilnost v %", title = sprintf("Rentabilnost skozi čas (%s)", tip))
      
    } else if (aspekt == "najemnina"){
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = povprecna.najemnina)) + 
        geom_line() + labs(x = "Leto", y = bquote("Najemnina na "~ m^2), title = sprintf("Višina najemnin skozi čas (%s)", tip)) 
    } else {
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = povprecna.cena)) + 
        geom_line() + labs(x = "Leto", y = bquote("Cena na "~m^2), title = sprintf("Cena skozi čas (%s)", tip))
    }
    
  } else {
    najemnine.skozi.cas = tabela.najemnin %>% group_by(leto.posla) %>%
      mutate(povprecna.najemnina = mean(mesecna.najemnina / povrsina)) %>% select(leto.posla, povprecna.najemnina) %>% distinct()
    
    cene.skozi.cas = tabela.nakupov %>% group_by(leto.posla) %>%
      mutate(povprecna.cena = mean(prodajna.cena / povrsina)) %>% select(leto.posla, povprecna.cena) %>% distinct()
    
    cas = cene.skozi.cas %>% left_join(najemnine.skozi.cas)
    cas = cas %>% mutate("rentabilnost (%)" = 1200 * povprecna.najemnina / povprecna.cena)
    
    if (aspekt == "rentabilnost"){
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = `rentabilnost (%)`)) +
        geom_line() + labs(x = "Leto", y = "Rentabilnost v %", title = sprintf("Rentabilnost skozi čas (%s)", tip))
      
    } else if (aspekt == "najemnina"){
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = povprecna.najemnina)) + 
        geom_line() + labs(x = "Leto", y = bquote("Najemnina na "~ m^2), title = sprintf("Višina najemnin skozi čas (%s)", tip)) 
    } else {
      graf = cas %>% ggplot(mapping = aes(x = leto.posla, y = povprecna.cena)) + 
        geom_line() + labs(x = "Leto", y = bquote("Cena na "~m^2), title = sprintf("Cena skozi čas (%s)", tip))
    }
  
  }
  return(graf)
}



# zemljevid rentabilnosti glede na statistične regije


source("lib/uvozi.zemljevid.r")

slo.regije.sp = uvozi.zemljevid("http://kt.ijs.si/~ljupco/lectures/appr/zemljevidi/si/gadm36_SVN_shp.zip", "gadm36_SVN_1", mapa = "zemljevidi", encoding="UTF-8")

slo.regije.map = slo.regije.sp %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

slo.regije.poligoni = fortify(slo.regije.map)


slo.regije.poligoni = slo.regije.poligoni %>%  select(
    regija = NAME_1, long, lat, order, hole, piece, id, group
  ) %>%
  mutate(
    regija = replace(regija, regija == "Notranjsko-kraška", "Primorsko-notranjska"),
    regija = replace(regija, regija == "Spodnjeposavska", "Posavska")
  )

slo.regije.poligoni %>% write_csv("podatki/regije-poligoni.csv")


slo.regije.centroidi = slo.regije.map %>% coordinates %>% as.data.frame
colnames(slo.regije.centroidi) = c("long", "lat")

slo.regije.centroidi = slo.regije.centroidi %>% rownames_to_column() %>%
  left_join(
    rownames_to_column(slo.regije.map@data),
    by = "rowname"
  ) %>%
  select(
    regija = NAME_1, long, lat
  ) %>%
  mutate(
    regija = replace(regija, regija == "Notranjsko-kraška", "Primorsko-notranjska"),
    regija = replace(regija, regija == "Spodnjeposavska", "Posavska")
  )
slo.regije.centroidi %>% write_csv("podatki/regije-centroidi.csv")

#Pripadnost občin regijam

obcine.regije = read_csv("podatki/obcine-regije (1).csv")

obcine.regije = obcine.regije %>% mutate(obcina = str_to_sentence(obcina)) %>%
  mutate(obcina = str_replace(obcina, "([:alpha:]*)/[:alpha:]*", "\\1")) %>%
  mutate(obcina = str_replace(obcina, "([:alpha:]*)\\s-\\s([:alpha:]*)", "\\1-\\2"))

povprecna.najemnina.obcina = tabela.najemnin %>% group_by(obcina) %>%
  mutate(povprecna.najemnina = mean(mesecna.najemnina / povrsina)) %>% 
  select(obcina, povprecna.najemnina) %>% distinct()

povprecna.cena.obcina = tabela.nakupov %>% group_by(obcina) %>%
  mutate(povprecna.cena = mean(prodajna.cena / povrsina)) %>%
  select(obcina, povprecna.cena) %>% distinct()

regije.s.pripadnostjo = left_join(povprecna.najemnina.obcina, obcine.regije) %>% 
  left_join(povprecna.cena.obcina) %>% filter(obcina != "Slovenija") %>%
  group_by(regija) %>% mutate(povprecna.najemnina.regija = mean(povprecna.najemnina)) %>%
  mutate(povprecna.cena.regija = mean(povprecna.cena)) %>%
  select(regija, povprecna.najemnina = povprecna.najemnina.regija, povprecna.cena = povprecna.cena.regija) %>%
  distinct() %>% na.omit() %>% mutate("povprecna.rentabilnost" = 1200 * povprecna.najemnina / povprecna.cena) 

regije.zemljevid.rentabilnost = left_join(regije.s.pripadnostjo, slo.regije.poligoni) %>%
  ggplot() + 
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = povprecna.rentabilnost),
    color = "grey"
  ) + 
  labs(fill = "Rentabilnost v %") + 
  coord_map() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  geom_text(
    data = slo.regije.centroidi,
    mapping = aes(x = long, y = lat, label = regija),
    size = 3,
    fontface ="bold.italic",
    color = "black"
  ) +
  labs(title = "Rentabilnost glede na regijo")

regije.zemljevid.najemnine = left_join(regije.s.pripadnostjo, slo.regije.poligoni) %>%
  ggplot() + 
  geom_polygon(
    mapping = aes(long, lat, group = group, fill = povprecna.najemnina),
    color = "grey"
  ) + 
  labs(fill = bquote("Najemnina na"~ m^2)) + 
  coord_map() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) +
  geom_text(
    data = slo.regije.centroidi,
    mapping = aes(x = long, y = lat, label = regija),
    size = 3,
    fontface ="bold.italic",
    color = "black"
  ) +
  labs(title = "Višina najemnin glede na regijo")

#Povprecna donostnost glede na tip prostora

povprecna.najemnina.tip.prostora = tabela.najemnin %>% group_by(tip.prostora) %>%
  mutate(povprecna.najemnina.na.kv.meter = mean(mesecna.najemnina / povrsina)) %>% select(tip.prostora, povprecna.najemnina.na.kv.meter) %>% distinct()

povprecna.cena.tip.prostora = tabela.nakupov %>% group_by(tip.prostora) %>%
  mutate(povprecna.cena.na.kv.meter = mean(prodajna.cena / povrsina)) %>% select(tip.prostora, povprecna.cena.na.kv.meter) %>% distinct()

povprecna.rentabilnost = povprecna.cena.tip.prostora %>% left_join(povprecna.najemnina.tip.prostora) %>% 
  mutate(povprecna.rentabilnost = 1200 * povprecna.najemnina.na.kv.meter / povprecna.cena.na.kv.meter)

graf.rentabilnost.tip.prostora = povprecna.rentabilnost %>% arrange(povprecna.rentabilnost) %>% ggplot(mapping = aes(x = reorder(tip.prostora, povprecna.rentabilnost), y = povprecna.rentabilnost)) + 
  geom_bar(stat="identity") + labs(x = "Tip prostora", y = "Letna rentabilnost (%)", title = "Rentabilnost glede na tip prostora") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))





  









