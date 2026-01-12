library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)
library(scales)  # pour na.approx

# ---- 2. Joindre les pondérations ----

library(dplyr)


ponderation <- ponderation %>%
  mutate(
    Fecha = as.Date(Fecha),
    Fecha = if_else(
      month(Fecha) == 12 & day(Fecha) == 31,
      Fecha + days(1),
      Fecha
    )
  )

# Définir la plage de dates jusqu'à la dernière date de ipc_data_table
fecha_min <- min(ponderation$Fecha)
fecha_max <- max(ipc_data_table$Fecha)  # dernière date IPC

# Créer la séquence complète par mois pour chaque Indicador
ponderation_mensuelle <- ponderation %>%
  complete(
    Indicador,
    Fecha = seq.Date(from = fecha_min, to = fecha_max, by = "month"),
    fill = list(Valor = NA)
  ) %>%
  mutate(Annee = year(Fecha)) %>%
  arrange(Indicador, Fecha)

# Remplir les NA de Valor par interpolation linéaire

ponderation_mensuelle <- ponderation_mensuelle %>%
  group_by(Indicador) %>%
  arrange(Fecha) %>%
  fill(Valor, .direction = "down") %>%  # remplit les NA avec la dernière valeur connue
  ungroup()


ipc_full <- ipc_data_table %>%
  left_join(ponderation_mensuelle, by = c("Indicador", "Fecha")) %>%
  rename(Poids = Valor.y,
         Valor = Valor.x) %>%
  filter(Tipo == "Variación anual")%>%
  mutate(Contribution = (Poids / 1000) * Valor)

saveRDS(ipc_full, file = "ipc_full.rds")


ggplot(ipc_full %>% 
         filter(!Indicador %in% c(
           "Índice general", 
           "Bebidas alcohólicas y tabaco",
           "Muebles, artículos del hogar y artículos para el mantenimiento corriente del hogar",
           "Sanidad",
           "Enseñanza",
           "Ocio y cultura"
         )),
       aes(x = Fecha, y = Contribution, fill = Indicador)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(
    name = "Contribution à l’inflation (pp)",
    labels = scales::label_number(accuracy = 0.1)
  ) +
  scale_x_date(name = "") +
  scale_fill_discrete(
    name = "",  # pas de titre
    labels = c(
      "Alimentos y bebidas no alcohólicas" = "Aliments et boissons non alcoolisées",
      "Vivienda" = "Logement",
      "Transporte" = "Transport",
      "Restaurantes y hoteles" = "Restaurants et hôtels",
      "Comunicaciones" = "Communications"
    )
  ) +
  labs(
    subtitle = "Contributions en points de pourcentage (hors Indice général)"
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 6)
  )


# 1. IPC officiel
ipc_officiel <- ipc_data_table %>%
  filter(Indicador == "Índice general", Tipo == "Variación anual") %>%
  select(Fecha, Inflation_officielle = Valor)

# 2. Somme des contributions par date
ipc_calc <- ipc_contrib %>%
  group_by(Fecha) %>%
  summarise(Inflation_calc = sum(Contribution, na.rm = TRUE), .groups = "drop")

# 3. Comparer
check <- ipc_calc %>%
  left_join(ipc_officiel, by = "Fecha")




# ---- Graphique ----
ggplot(ipc_decomp, aes(x = Fecha, y = Contribution, fill = Indicador)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(
    name = "Contribution à l’inflation (pp)",
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_x_date(name = "") +
  labs(
    title = "Décomposition de l’inflation par sous-catégorie",
    subtitle = "Contributions en points de pourcentage",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 6)  # Taille réduite de la légende
  )

# Enregistrer le dernier graphique créé
ggsave(
  filename = "ipc_decomposition.png",   # Nom du fichier
  plot = last_plot(),                   # Le graphique actuel
  width = 10,                           # Largeur en pouces
  height = 6,                           # Hauteur en pouces
  dpi = 300                             # Résolution
)


ipc_general <- ipc_contrib %>%
  filter(Indicador == "Índice general")

ggplot(ipc_general, aes(x = Fecha, y = Valor)) +
  geom_line(color = "black", size = 1) +
  labs(
    title = "Inflation annuelle (Índice general)",
    y = "Variation annuelle (%)",
    x = ""
  ) +
  theme_minimal()
categories_communes <- intersect(unique(ipc_data_table$Indicador),
                                 unique(ponderation$Indicador))

ipc_with_weights <- ipc_data_table %>%
  filter(Indicador %in% categories_communes,
         Part4 == "Variación anual") %>%   # <-- filtre sur variation annuelle
  left_join(
    ponderation %>%
      filter(Indicador %in% categories_communes) %>%
      rename(Poids_milli = Valor),
    by = "Indicador"
  ) %>%
  mutate(
    Poids = parse_number(Poids_milli, locale = locale(decimal_mark = ",")) / 10
  )


ipc_contrib <- ipc_with_weights %>%
  filter(Tipo == "Variación anual") %>%
  mutate(
    Contribution = Poids * Valor / 100  # Poids (%) × taux (%)
  )




ipc_contrib <- ipc_with_weights %>%
  filter(Tipo == "Variación anual" & Indicador != "Índice general") %>%  # Exclure l'indice général
  mutate(
    Contribution = Poids_frac * Valor   # Fraction × taux annuel
  )

# Vérification
check <- ipc_contrib %>%
  group_by(Fecha) %>%
  summarise(
    Inflation_calc = sum(Contribution, na.rm = TRUE),
    Inflation_officielle = Valor[Indicador == "Índice general"][1]
  )

# 1. IPC officiel
ipc_officiel <- ipc_data_table %>%
  filter(Indicador == "Índice general", Tipo == "Variación anual") %>%
  select(Fecha, Inflation_officielle = Valor)

# 2. Somme des contributions par date
ipc_calc <- ipc_contrib %>%
  group_by(Fecha) %>%
  summarise(Inflation_calc = sum(Contribution, na.rm = TRUE), .groups = "drop")

# 3. Comparer
check <- ipc_calc %>%
  left_join(ipc_officiel, by = "Fecha")

head(check)


library(dplyr)
library(ggplot2)

# ---- 1. Regrouper en 4 grandes catégories ----
regroupement <- c(
  "Alimentos y bebidas no alcohólicas" = "Alimentation",
  "Bebidas alcohólicas y tabaco" = "Alimentation",
  
  "Vivienda, agua, electricidad, gas y otros combustibles" = "Énergie",
  "Carburantes y combustibles" = "Énergie",
  "Productos energéticos" = "Énergie",
  
  "Vestido y calzado" = "Biens manufacturés",
  "Muebles, artículos del hogar y artículos para el mantenimiento corriente del hogar" = "Biens manufacturés",
  "Ocio y cultura" = "Biens manufacturés",
  "Comunicaciones" = "Biens manufacturés",
  "Otros bienes y servicios" = "Biens manufacturés",
  
  "Sanidad" = "Services",
  "Transporte" = "Services",
  "Restaurantes y hoteles" = "Services",
  "Enseñanza" = "Services"
)

# ---- Décomposition sans indice général ----
# ---- Filtrer les 4 sous-catégories ----
ipc_decomp <- ipc_contrib %>%
  filter(Indicador %in% c(
    "Alimentos con elaboración, bebidas y tabaco",
    "Bienes industriales",
    "Servicios",
    "Carburantes y combustibles"
  ))

# ---- Graphique ----
ggplot(ipc_decomp, aes(x = Fecha, y = Contribution, fill = Indicador)) +
  geom_area(alpha = 0.85) +
  scale_y_continuous(
    name = "Contribution à l’inflation (pp)",
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_x_date(name = "") +
  labs(
    title = "Décomposition de l’inflation par sous-catégorie",
    subtitle = "Contributions en points de pourcentage",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 6)  # Taille réduite de la légende
  )

# Enregistrer le dernier graphique créé
ggsave(
  filename = "ipc_decomposition.png",   # Nom du fichier
  plot = last_plot(),                   # Le graphique actuel
  width = 10,                           # Largeur en pouces
  height = 6,                           # Hauteur en pouces
  dpi = 300                             # Résolution
)


ipc_general <- ipc_contrib %>%
  filter(Indicador == "Índice general")

ggplot(ipc_general, aes(x = Fecha, y = Valor)) +
  geom_line(color = "black", size = 1) +
  labs(
    title = "Inflation annuelle (Índice general)",
    y = "Variation annuelle (%)",
    x = ""
  ) +
  theme_minimal()
