# 4. faza: Napredna analiza podatkov

#priprava tabele

tabela.obcin.ucenje = tabela.obcin %>% na.omit() %>% select(-leto) %>%group_by(obcina) %>% 
  mutate(povrsina = round(mean(povrsina)), 
         stevilo.prebivalcev = round(mean(stevilo.prebivalcev)), 
         skupni.prirast = round(mean(skupni.prirast)),
         indeks.staranja = round(mean(indeks.staranja, 1)),
         stevilo.studentov = round(mean(stevilo.studentov)),
         stopnja.delovne.aktivnosti = round(mean(stopnja.delovne.aktivnosti)),
         bruto.mesecna.placa =round(mean(bruto.mesecna.placa))
         ) %>% distinct() %>% filter(obcina != "Slovenija")

#odstranimo obcine s 30 ali manj podatki o najemninah in nakupih

stetje.najemnin = tabela.najemnin %>% count(obcina) %>% filter(n >= 30)
stetje.nakupov = tabela.nakupov %>% count(obcina) %>% filter(n >= 30)
obdrzi = intersect(stetje.nakupov$obcina, stetje.najemnin$obcina)

tabela.obcin.ucenje = left_join(tabela.obcin.ucenje, povprecna.cena.obcina) %>%
  left_join(povprecna.najemnina.obcina) %>% mutate(povprecna.rentabilnost = 1200 * povprecna.najemnina / povprecna.cena) %>%
  filter(obcina %in% obdrzi)


tabela.obcin.ucenje.rentabilnost = tabela.obcin.ucenje %>% select(-povprecna.najemnina) %>% select(-povprecna.cena)
tabela.obcin.ucenje.najemnine = tabela.obcin.ucenje %>% select(-povprecna.rentabilnost) %>% select(-povprecna.cena)
tabela.obcin.ucenje.cena = tabela.obcin.ucenje %>% select(-povprecna.rentabilnost) %>% select(-povprecna.najemnina)

tabela.obcin.ucenje.rentabilnost = tabela.obcin.ucenje.rentabilnost[-1]
tabela.obcin.ucenje.najemnine = tabela.obcin.ucenje.najemnine[-1]
tabela.obcin.ucenje.cena = tabela.obcin.ucenje.cena[-1]

# na podlagi podatkov o obcinah zelimo ugotoviti linearni model za ceno, najemnino in rentabilnost


lin.model.cena = lm(povprecna.cena ~ ., data = tabela.obcin.ucenje.cena)
print(lin.model.cena)

lin.model.najemnina = lm(povprecna.najemnina ~ ., data = tabela.obcin.ucenje.najemnine)
print(lin.model.najemnina)


lm.napovedi.rentabilnost = predict(lin.model, newdata = tabela.obcin.ucenje.rentabilnost)
print(lm.napovedi.rentabilnost)

#poskusimo še napoved z naključnimi gozdovi

set.seed(420)

ng.reg.model.cena = ranger(povprecna.cena ~ ., data = tabela.obcin.ucenje.cena)
ng.reg.model.najemnina = ranger(povprecna.najemnina ~ ., data = tabela.obcin.ucenje.najemnine)
ng.reg.model.rentabilnost = ranger(povprecna.rentabilnost ~., data = tabela.obcin.ucenje.rentabilnost)
print(ng.reg.model.cena)
print(ng.reg.model.najemnina)
print(ng.reg.model.rentabilnost)


## prečno preverjanje





















