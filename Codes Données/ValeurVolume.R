

#Pour la conso finale 
# 1. Filtrer la consommation nominale (prix courants)
conso_nominale <- menage_data %>%
  filter(Part3 == "Gasto en consumo individual") %>%
    filter(Part4 == "Empleos") %>%
  filter(Part5 == "Dato base")%>%
  rename(Conso_courants = Valor) %>%
  select(Fecha, Conso_courants)

# 2. Filtrer la consommation en volume (chaîné)
conso_volume <- pib_D_vol %>%
  filter(Part3 == "Gasto en consumo final de los hogares y las ISFLSH") %>%
  select(Fecha, Dato_base)  # Indice = volume chaîné 2020=100

# 1. Moyenne 2019 pour référence
ref_mean <- conso_nominale %>%
  filter(Fecha >= as.Date("2019-01-01") & Fecha <= as.Date("2019-12-01")) %>%
  summarise(mean_ref = mean(Conso_courants, na.rm = TRUE)) %>%
  pull(mean_ref)

# 2. Déflateur normalisé
deflateur_conso <- conso_nominale %>%
  left_join(conso_volume %>% select(Fecha, Conso_volume = Dato_base), by = "Fecha") %>%
  mutate(
    Deflateur_excel = Conso_courants / ref_mean / Conso_volume * 10000
  ) %>%
  mutate(
    Deflateur_base100 = Deflateur_excel / Deflateur_excel[Fecha == as.Date("2019-10-01")] * 100
  )

head(deflateur_conso)

revenu_nominal <- menage_data2 %>%
  filter(
    Part3 == "Dato base",
    Part2 == "Renta nacional disponible bruta"
  ) %>%
  select(Fecha, Revenu_courant = Valor)  

revenu_reel <- revenu_nominal %>%
  left_join(deflateur_conso %>% select(Fecha, Deflateur_base100), by = "Fecha")%>%
  mutate(Revenu_reel = Revenu_courant / Deflateur_base100 * 100)



#Pour la FCBF

FCBF <- menage_data %>%
  filter(Part3 == "Formación bruta de capital fijo (FBCF)") %>%
  filter(Part4 == "Empleos") %>%
  filter(Part5 == "Dato base")%>%
  rename(Conso_courants = Valor) %>%
  select(Fecha, Conso_courants)

# 2. Filtrer la consommation en volume (chaîné)
conso_volume <- pib_D_vol %>%
  filter(Part3 == "Gasto en consumo final de los hogares y las ISFLSH") %>%
  select(Fecha, Indice)  # Indice = volume chaîné 2020=100

# 1. Moyenne 2019 pour référence
ref_mean <- conso_nominale %>%
  filter(Fecha >= as.Date("2019-01-01") & Fecha <= as.Date("2019-12-01")) %>%
  summarise(mean_ref = mean(Conso_courants, na.rm = TRUE)) %>%
  pull(mean_ref)

# 2. Déflateur normalisé
deflateur_conso <- conso_nominale %>%
  left_join(conso_volume %>% select(Fecha, Conso_volume = Indice), by = "Fecha") %>%
  mutate(
    Deflateur_excel = Conso_courants / ref_mean / Conso_volume * 10000
  ) %>%
  mutate(
    Deflateur_base100 = Deflateur_excel / Deflateur_excel[Fecha == as.Date("2019-10-01")] * 100
  )

head(deflateur_conso)

revenu_nominal <- menage_data2 %>%
  filter(
    Part3 == "Dato base",
    Part2 == "Renta nacional disponible bruta"
  ) %>%
  select(Fecha, Revenu_courant = Valor)  

revenu_reel <- revenu_nominal %>%
  left_join(deflateur_conso %>% select(Fecha, Deflateur_base100), by = "Fecha")%>%
  mutate(Revenu_reel = Revenu_courant / Deflateur_base100 * 100)

# PIB en valeur
pib_D_valConvertion <- pib_D_val %>%
  rename(Valeur = Dato_base) %>%
  mutate(Part3 = if_else(Part3 == "Formación bruta de capital" | grepl("FBCF", Part3), "FBCF", Part3)) %>%
  group_by(Fecha, Part3) %>%
  summarise(Valeur = sum(Valeur, na.rm = TRUE), .groups = "drop") 

# PIB en volume
pib_D_volConvertion <- pib_D_vol %>%
  mutate(Part3 = if_else(Part3 == "Formación bruta de capital" | grepl("FBCF", Part3), "FBCF", Part3)) %>%
  group_by(Fecha, Part3) %>%
  summarise(Indice = mean(Indice, na.rm = TRUE), .groups = "drop") 

# Fusionner pour calculer le volume
pib_complet <- pib_D_valConvertion %>%
  left_join(pib_D_volConvertion, by = c("Fecha","Part3")) %>%
  arrange(Part3, Fecha) %>%
  group_by(Part3) %>%
  mutate(
    PIB_Vol = lag(Valeur) * (Indice / lag(Indice))
  ) %>%
  ungroup()



# --- Calcul des variations trimestrielles et annuelles ---
pib_volume_final <- pib_complet %>%
  filter(Part3 %in% unique(pib_D_vol$Part3)) %>%  # Filtrer les composantes du PIB
  group_by(Part3) %>%                             # Calcul séparé pour chaque variable
  arrange(Fecha, .by_group = TRUE) %>%
  mutate(
    var_trimestre = (Volume / lag(Volume) - 1) * 100,      # Variation trimestrielle
    var_annee     = (Volume / lag(Volume, 4) - 1) * 100   # Variation annuelle
  ) %>%
  ungroup()



menage_deflate <- menages_net %>%
  left_join(deflateur_conso %>% select(Fecha, Deflateur_base100), by = "Fecha") %>%
  mutate(
    across(
      .cols = where(is.numeric) & !c(Fecha),   # toutes les colonnes numériques sauf Fecha
      .fns = ~ . / Deflateur_base100 * 100
    )
  ) %>%
  select(-Deflateur_base100)




# --- Préparer les données ---
library(dplyr)

# Filtrer seulement le PIB
pib_total <- pib_D_valConvertion %>%
  filter(Part3 == "Producto interior bruto a precios de mercado") %>%
  left_join(
    pib_D_volConvertion %>% filter(Part3 == "Producto interior bruto a precios de mercado"),
    by = c("Fecha", "Part3")
  ) %>%
  arrange(Fecha)

# Initialiser la première valeur officielle
val_depart <- 189189  # PIB en volume pour le premier trimestre (1995-01-01)

pib_total <- pib_total %>%
  mutate(
    PIB_Vol = NA_real_
  )

# Calcul en chaîne
pib_total$PIB_Vol[1] <- val_depart
for(i in 2:nrow(pib_total)) {
  pib_total$PIB_Vol[i] <- pib_total$PIB_Vol[i-1] * (pib_total$Indice[i] / pib_total$Indice[i-1])
}

pib_total

library(dplyr)

# Définir les valeurs officielles initiales pour chaque série au premier trimestre
valeurs_depart <- tibble(
  Part3 = c("Producto interior bruto a precios de mercado",
            "Gasto en consumo final de los hogares",
            "Gasto en consumo final de las AAPP",
            "Formación bruta de capital fijo (FBCF)",
            "Exportaciones de bienes y servicios",
            "Importaciones de bienes y servicios"),
  Valeur_init = c(189189, 34353, 114559, 37072, 36066, 35615)
)

# Filtrer les séries d’intérêt et joindre l’indice
# Dictionnaire de correspondance
# Dictionnaire de correspondance
correspondance <- tribble(
  ~Part3, ~Variable,
  "Producto interior bruto a precios de mercado", "PIB Vol",
  "Gasto en consumo final de los hogares", "C pv Vol",
  "Gasto en consumo final de las AAPP", "C pb Vol",
  "FBCF", "FBCF Vol",
  "Exportaciones de bienes y servicios", "X Vol",
  "Importaciones de bienes y servicios", "M Vol",
  "Variación de existencias y adquisiciones menos cesiones de objetos valiosos", "Variation de Stock"
)

# Ajouter le nom harmonisé
pib_toutes <- pib_D_valConvertion %>%
  inner_join(pib_D_volConvertion, by = c("Fecha", "Part3")) %>%
  inner_join(correspondance, by = "Part3") %>%
  arrange(Variable, Fecha) %>%
  group_by(Variable) %>%
  mutate(PIB_Vol = NA_real_) %>%
  ungroup()

# !!! Important : valeurs_depart doit contenir la même colonne "Variable"
valeurs_depart <- tribble(
  ~Variable,       ~Valeur_init,
  "PIB Vol",        189189,
  "C pv Vol",       34353,
  "C pb Vol",       114559,
  "FBCF Vol",       37072,
  "X Vol",          36066,
  "M Vol",          35615
)

# Boucle avec les bons noms
for(s in unique(valeurs_depart$Variable)) {
  lignes <- which(pib_toutes$Variable == s)
  if(length(lignes) > 0) {
    pib_toutes$PIB_Vol[lignes[1]] <- valeurs_depart$Valeur_init[valeurs_depart$Variable == s]
    for(i in 2:length(lignes)) {
      pib_toutes$PIB_Vol[lignes[i]] <- pib_toutes$PIB_Vol[lignes[i-1]] *
        (pib_toutes$Indice[lignes[i]] / pib_toutes$Indice[lignes[i-1]])
    }
  }
}
