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

lin.model.najemnina = lm(250*povprecna.najemnina ~ ., data = tabela.obcin.ucenje.najemnine)
print(lin.model.najemnina)

lin.model.rentabilnost = lm(250*povprecna.rentabilnost ~ ., data = tabela.obcin.ucenje.rentabilnost)
print(lin.model.rentabilnost)


lm.napovedi.rentabilnost = predict(lin.model.rentabilnost, newdata = tabela.obcin.ucenje.rentabilnost)
print(lm.napovedi.rentabilnost)


vektor.koeficientov.cena = unname(summary(lin.model.cena)$coefficients[,1][-1])
vektor.koeficientov.najemnina = unname(summary(lin.model.najemnina)$coefficients[,1][-1])
vektor.koeficientov.rentabilnost = unname(summary(lin.model.rentabilnost)$coefficients[,1][-1])

linearni.modeli = tibble("znacilnost" = c(
  "površina",
  "število prebivalcev",
  "skupni prirast prebivalstva",
  "indeks staranja",
  "število študentov",
  "stopnja delovne aktivnosti",
  "bruto mesečna plača"
),
                         "koeficient lm cen" = round(vektor.koeficientov.cena, 2),
                         "koeficienti lm najemnin" = round(vektor.koeficientov.najemnina,2),
                         "koeficienti lm rentabilnost" = round(vektor.koeficientov.rentabilnost,2))

napaka = function(podatki, model, spremenljivka) {
  podatki %>%
    bind_cols(Yhat = predict(model, podatki)) %>%
    mutate(
      izguba = abs(spremenljivka - Yhat)
    ) %>%
    select(izguba) %>%
    unlist() %>%
    mean()
}

napaka(tabela.obcin.ucenje.cena, lin.model.cena, tabela.obcin.ucenje.cena$povprecna.cena)
napaka(tabela.obcin.ucenje.najemnine, lin.model.najemnina, tabela.obcin.ucenje.najemnine$povprecna.najemnina)














## gručenje

tip.prostora.cene = tabela.nakupov %>% group_by(tip.prostora) %>% 
  mutate(povprecna.cena = mean(prodajna.cena / povrsina)) %>% select(tip.prostora, povprecna.cena) %>%
  distinct()
tip.prostora.grucenje = tabela.najemnin %>% group_by(tip.prostora) %>% 
  mutate(povprecna.najemnina = mean(100 * mesecna.najemnina / povrsina)) %>% select(tip.prostora, povprecna.najemnina) %>%
  distinct() %>% left_join(tip.prostora.cene)


primeri = tibble(
  oznaka = tip.prostora.grucenje$tip.prostora,
  x = tip.prostora.grucenje$povprecna.najemnina,
  y = tip.prostora.grucenje$povprecna.cena
)

dendrogram = primeri[, -1] %>%
  dist() %>%
  hclust()

plot(
  dendrogram,
  labels = primeri$oznaka,
  ylab = "višina",
  main = NULL
)

tibble(
  k = 14:1,
  visina = dendrogram$height
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  geom_point(
    mapping = aes(x = k, y = visina),
    color = "red"
  ) +
  scale_x_continuous(
    breaks = 14:1
  ) +
  labs(
    x = "število skupin (k)",
    y = "višina združevanja"
  ) +
  theme_classic()

hc.kolena = function(dendrogram, od = 1, do = NULL, eps = 0.5) {
  # število primerov in nastavitev parametra do
  n = length(dendrogram$height) + 1
  if (is.null(do)) {
    do = n - 1
  }
  # k.visina je tabela s štirimi stolpci
  # (1) k, število skupin
  # (2) višina združevanja
  # (3) sprememba višine pri združevanju
  # (4) koleno: ali je točka koleno?
  k.visina = tibble(
    k = as.ordered(od:do),
    visina = dendrogram$height[do:od]
  ) %>%
    # sprememba višine
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    # ali se je intenziteta spremembe dovolj spremenila?
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}

# iz tabele k.visina vrne seznam vrednosti k,
# pri katerih opazujemo koleno
hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

# izračunamo tabelo s koleni za dendrogram
r = hc.kolena(dendrogram)

# narišemo diagram višin združevanja
diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red"
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red"
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 2
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

diagram.kolena(r)




skupine.3 = dendrogram %>% cutree(k = 3) %>% as.ordered()
print(skupine.3)

skupine = function(i){
  skup = dendrogram %>% cutree(k = i) %>% as.ordered()
}

diagram.skupine = function(podatki, oznake, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = x / 100, y = y, color = skupina
      )
    ) +
    labs(x = bquote("najemnina na"~m^2), y = bquote("cena na "~m^2), title = "Skupine tipov stanovanj") +
    geom_point() +
    geom_label(label = oznake, size = 3.5) +
    scale_color_hue() +
    theme_classic() +
    xlim(3, 14) +
    ylim(500, 1400)
  
  d
}

print(diagram.skupine(primeri, primeri$oznaka, skupine(3), 3))















