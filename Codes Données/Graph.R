install.packages("xlsx")
install.packages("openxlsx")
library(gt)
library(glue)
library(zoo)
library(seasonal)
library(forcats)

#PIB 
dernier_index <- tail(pib_D_vol$Index2019, 1)

pib_total <- pib_D_vol %>%
  filter(Part3 == "Producto interior bruto a precios de mercado") %>%
  arrange(Fecha)

# Dernier index
dernier_index <- tail(pib_total$Index2019, 1)

# Phrase pour le graphique
phrase_graphique <- glue(
  "Au dernier trimestre, le PIB espagnol est désormais {round(dernier_index - 100, 1)} points au-dessus de son niveau de fin 2019."
)

# Phrase pour les variations (utilisation directe des deux dernières lignes)
dernieres_lignes <- pib_total %>% slice_tail(n = 2)
phrase_pib <- glue(
  "Le PIB a augmenté de {round(dernieres_lignes$Variación_trimestral[2], 1)}% en variation trimestrielle ",
  "au T{quarter(dernieres_lignes$Fecha[2])} {year(dernieres_lignes$Fecha[2])}, ",
  "après {round(dernieres_lignes$Variación_trimestral[1], 1)}% au T{quarter(dernieres_lignes$Fecha[1])} {year(dernieres_lignes$Fecha[1])}. ",
  "En variation annuelle, le PIB a augmenté de {round(dernieres_lignes$Variación_anual[2], 1)}%."
)

# Affichage
phrase_graphique
phrase_pib

#Indices du PIB par secteur 

ggplot(pib_dataS, aes(x = Fecha)) +
  geom_line(aes(y = Index2015, color = "Index 2015"), size = 1) +
  geom_line(aes(y = Index2019, color = "Index 2019"), size = 1) +
  facet_wrap(~Part3, scales = "free_y") +  # Un graphique par secteur/type de PIB
  labs(
    title = "Indices du PIB par secteur",
    x = "Date",
    y = "Indice (base 100)",
    color = "Indice"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




# Graphique
ggplot(pib_total, aes(x = Fecha)) +
  geom_line(aes(y = Index2019, color = "Index 2019"), size = 1) +
  labs(
    title = "Indice du PIB total par rapport à 2019",
    x = "Date",
    y = "Indice (base 100)",
    color = "Indice"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

dernier_index <- tail(pib_total$Index2019, 1)

phrase_graphique <- glue(
  "Au dernier trimestre, le PIB espagnol est désormais {round(dernier_index - 100, 1)} points au-dessus de son niveau de fin 2019."
)

phrase_graphique

# PIB et les composantes 

graph1 <- ggplot(aes(x = as.Date(Fecha), y = Aportación_trimestral), data = pib_D_vol) +
  geom_bar(data = pib_D_vol %>% 
             filter(Part3 %in% c(
               "Gasto en consumo final", 
               "Formación bruta de capital fijo (FBCF)", 
               "Exportaciones de bienes y servicios", 
               "Importaciones de bienes y servicios"
             )),
           aes(fill = Part3), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(pib_D_vol, Part3 == "Producto interior bruto a precios de mercado"),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: INE",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 15), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(pib_D_vol$Fecha))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))

ggsave(filename = "graph1.png", plot = graph1, width = 8, height = 6, units = "in")

quarter_label <- function(date) {
  year <- format(date, "%Y")
  month <- as.numeric(format(date, "%m"))
  quarter <- (month - 1) %/% 3 + 1
  paste0(year, "-T", quarter)
}

pib_D_vol <- pib_D_vol %>%
  mutate(Fecha = as.yearqtr(Fecha, format = "%YQ%q"),
         Fecha = as.Date(Fecha))


# Garder uniquement ces deux trimestres
traduction <- c(
  "Gasto en consumo final" = "Dépense de consommation finale",
  "Formación bruta de capital fijo (FBCF)" = "Formation brute de capital fixe (FBCF)",
  "Exportaciones de bienes y servicios" = "Exportations de biens et services",
  "Importaciones de bienes y servicios" = "Importations de biens et services"
)

dernières_dates <- pib_D_vol %>%
  distinct(Fecha) %>%
  arrange(desc(Fecha)) %>%
  slice_head(n = 4) %>%
  pull(Fecha)

# Filtrer et traduire
derniers <- pib_D_vol %>% 
  filter(Part3 %in% names(traduction),
         Fecha %in% dernières_dates) %>%   # <--- ajout du filtre ici
  group_by(Fecha) %>%
  slice_max(order_by = Aportación_trimestral, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(Fecha) %>%  # s'assurer de l'ordre chronologique
  mutate(
    Part3_fr = traduction[Part3],
    trim = quarter_label(Fecha)
  )
# Création de la phrase globale
phrase <- glue(
  "Sur les deux derniers trimestres, la croissance du PIB a été tirée par respectivement ",
  "{derniers$Part3_fr[1]} (+{round(derniers$Aportación_trimestral[1],1)} pp) ",
  "au {derniers$trim[1]}, et ",
  "{derniers$Part3_fr[2]} (+{round(derniers$Aportación_trimestral[2],1)} pp) ",
  "au {derniers$trim[2]}."
)


phrase

ggplot(aes(x = as.Date(Fecha), y = Aportación_anual), data = pib_D_vol) +
  geom_bar(data = pib_D_vol %>% 
             filter(Part3 %in% c(
               "Gasto en consumo final", 
               "Formación bruta de capital fijo (FBCF)", 
               "Exportaciones de bienes y servicios", 
               "Importaciones de bienes y servicios"
             )),
           aes(fill = Part3), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(pib_D_vol, Part3 == "Producto interior bruto a precios de mercado"),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: INE",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size = 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 12), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2010-04-01", max(pib_D_vol$Fecha))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


#Tableau de recap v0 (sans prévison)

library(gt)


pib_D_vol_tab_reel <- pib_D_vol %>%
  mutate(Part3 = recode(Part3,
                        "Producto interior bruto a precios de mercado" = "PIB",
                        "Gasto en consumo final de los hogares" = "Consommation des ménages",
                        "Gasto en consumo final de las AAPP" = "Consommation publique",
                        "Formación bruta de capital fijo (FBCF)" = "FBCF totale",
                        "Exportaciones de bienes y servicios" = "Exportations de B&S",
                        "Importaciones de bienes y servicios" = "Importations de B&S",
                        "Variación de existencias y adquisiciones menos cesiones de objetos valiosos" = "Variation de stocks",
                        "Demanda nacional" = "Demande intérieure hors stocks",
                        "Demanda externa" = "Commerce extérieur")) %>%
  filter(Part3 %in% c("PIB",
                      "Consommation des ménages",
                      "Consommation publique",
                      "FBCF totale",
                      "Exportations de B&S",
                      "Importations de B&S",
                      "Variation de stocks", 
                      "Demande intérieure hors stocks",
                      "Commerce extérieur")) %>%
  
  # prendre uniquement les 4 derniers trimestres disponibles
  filter(Fecha %in% tail(sort(unique(Fecha)), 4))

ordre_variables <- c(
  "PIB",
  "Consommation des ménages",
  "Consommation publique",
  "FBCF totale",
  "Exportations de B&S",
  "Importations de B&S",
  "Variation de stocks",
  "Demande intérieure hors stocks",
  "Commerce extérieur"
)

# ---- Transformer en tableau gt ----
tab_reel <- pib_D_vol_tab_reel %>%
  filter(Part3 %in% ordre_variables) %>%
  mutate(
    Part3 = factor(Part3, levels = ordre_variables),
    Variación_trimestral = round(Variación_trimestral, 1)
  ) %>%
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3) %>%
  gt(rowname_col = "Part3") %>%
  tab_header(
    title = md("**Variation trimestrielle**")
  ) %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "")

tab_reel

# ---- Tableau contributions ---- (ça pour l'instant on y touche pas)
# tab3_df <- dataPrev2 %>%
  filter(Part3 %in% ordre_variables2) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables2),
         Aportación_trimestral = round(Aportación_trimestral, 1)) %>%
  select(Fecha, Part3, Aportación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Aportación_trimestral) %>%
  arrange(Part3)

# ---- Fusion finale ----
  # tab_final_df <- bind_rows(tab2_df, tab3_df)

  # tab_final <- tab_final_df %>%
  gt(rowname_col = "Part3") %>%
  tab_header(
    title = md("**Variation/contrib trim**"),
  ) %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "Contribution trim") %>%
  tab_row_group(
    label = "Contributions à la croissance",
    rows = Part3 %in% c("Demande intérieure hors stocks", "Variation de stocks", "Commerce extérieur")  
  ) %>%
  tab_row_group(
    label = "Variation trimestrielle",
    rows = Part3 %in% c("PIB", "Consommation des ménages", "Consommation publique",
                        "FBCF totale", "Exportations de B&S", "Importations de B&S")  
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups(groups = c("Contributions à la croissance", "Variation trimestrielle"))
  )

  # tab_final

  # gtsave(
  # invest_tab,
  # filename = "tab_final.png"
#)

## Tableau investissement

# ---- Filtrage des investissements ----
# Branches détaillées
pib_D_vol <- pib_D_vol %>%
  mutate(across(c(Part3, Part4, Part5, Part6), ~ str_squish(trimws(.))))

# Branches détaillées
invest_branches <- pib_D_vol %>%
  mutate(Branche = case_when(
    Part4 == "Activos fijos inmateriales" & Part5 == "Productos de la propiedad intelectual" ~ "PI - Actifs incorporels",
    Part4 == "Activos fijos materiales" & Part5 == "Construcción" & Part6 == "Otros edificios y construcciones" ~ "Construction - Autres bâtiments",
    Part4 == "Activos fijos materiales" & Part5 == "Construcción" & Part6 == "Viviendas" ~ "Construction - Logements",
    Part4 == "Activos fijos materiales" & Part5 == "Maquinaria, bienes de equipo y sistemas de armamento" & Part6 == "Material de transporte" ~ "Matériel de transport & équipements",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Branche)) %>%
  filter(Fecha %in% tail(sort(unique(Fecha)), 4)) %>%
  mutate(Variación_trimestral = round(Variación_trimestral, 1)) %>%
  select(Fecha, Branche, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral)

# FBCF totale (directement de Part3)
invest_fbcf <- pib_D_vol %>%
  filter(Part3 == "Formación bruta de capital") %>%
  filter(Fecha %in% tail(sort(unique(Fecha)), 4)) %>%
  mutate(Branche = "FBCF totale",
         Variación_trimestral = round(Variación_trimestral, 1)) %>%
  select(Fecha, Branche, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral)

# Fusion + FBCF en premier
invest_tab_reel <- bind_rows(invest_fbcf, invest_branches) %>%
  mutate(Branche = fct_relevel(Branche, "FBCF totale", after = 0))


# ---- Tableau GT ----
invest_tab <- invest_tab_reel %>%
  gt(rowname_col = "Branche") %>%
  tab_header(
    title = md("**Variation trimestrielle de la FBCF par branche**"),
    subtitle = md("*4 derniers trimestres*")
  ) %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Branche = "Investissement (FBCF)")

invest_tab

gtsave(
  invest_tab,
  filename = "invest_tab.png"
)


investissement_all <- comptes_agents %>%
  filter(
    Part3 == "Formación bruta de capital",
    Part4 == "Empleos",
    Part5 == "Dato base",
    Part1 != "Total de la economía"
  ) %>%
  arrange(Part1, Fecha)

cvs <- function(v) {
  sa_series <- seas(v, force.type = "denton")
  r <- sa_series$series$s11  # Série désaisonnalisée
  return(as.numeric(r))
}

desais_et_indice <- function(df) {
  # Garder uniquement les lignes avec des valeurs
  df <- df %>% filter(!is.na(Valor))
  
  # Si moins de 8 observations, on ne traite pas
  if (nrow(df) < 8) {
    df$Valor_desais <- NA
    df$Indice <- NA
    return(df)
  }
  
  # Créer série ts
  ts_data <- ts(
    df$Valor,
    start = c(year(min(df$Fecha)), quarter(min(df$Fecha))),
    frequency = 4
  )
  
  # Désaisonnalisation
  df$Valor_desais <- tryCatch({
    cvs(ts_data)
  }, error = function(e) {
    rep(NA, nrow(df))
  })
  
  # Calcul indice base T4 2019
  base_2019 <- df$Valor_desais[df$Fecha == as.Date("2019-10-01")]
  df$Indice <- if (!is.na(base_2019) && base_2019 > 0) {
    (df$Valor_desais / base_2019) * 100
  } else {
    NA
  }
  
  return(df)
}



investissement_all_indice <- investissement_all %>%
  group_by(Part1) %>%
  group_modify(~ desais_et_indice(.x)) %>%
  ungroup()



# Graphique avec toutes les séries
ggplot(
  investissement_all_indice%>%
    filter(Part1 == "Sociedades no financieras"),
  aes(x = Fecha, y = Valor_desais, color = Part1)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Investissement selon les agents (série désaisonnalisée)",
    x = "",
    y = "Investissement désaisonnalisé (millions €)",
    color = "Agent"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

investissement_all_indice %>%
  filter(Part1 == "Hogares e Instituciones sin fines de lucro al servicio de los hogares") %>%
  ggplot(aes(x = Fecha, y = Valor_desais)) +
  geom_line(color = "blue", size = 1.2) +
  labs(
    title = "Investissement des ménages (désaisonnalisé)",
    x = "",
    y = "Investissement désaisonnalisé (millions €)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


ggplot(
  investissement_all_indice %>%
    filter(Part1 == "Administraciones públicas"),
  aes(x = Fecha, y = Valor_desais, color = Part1)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Investissement selon les agents (série désaisonnalisée)",
    x = "",
    y = "Investissement désaisonnalisé (millions €)",
    color = "Agent"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


ggplot(
  investissement_all_indice %>%
    filter(Part1 == "Total de la economía"),
  aes(x = Fecha, y = Valor_desais, color = Part1)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Investissement selon les agents (série désaisonnalisée)",
    x = "",
    y = "Investissement désaisonnalisé (millions €)",
    color = "Agent"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


p <- investissement_all_indice %>%
  filter(Fecha >= as.Date("2010-01-01")) %>%
  ggplot(aes(x = Fecha, y = Valor_desais, color = Part1)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Part1, scales = "free_y") +
  labs(
    title = "Investissement par agent (série désaisonnalisée, depuis 2010)",
    x = "",
    y = "Investissement désaisaisonnalisé (millions €)",
    color = "Agent"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Sauvegarde en PNG
ggsave("figures/investissement_par_agent.png", plot = p, width = 12, height = 8, dpi = 300)

# 📈 Graphique 1 : Renta nacional disponible bruta
ggplot(
  menage_data2 %>%
    filter(
      Part3 == "Variación anual",
      Part2 == "Renta nacional disponible bruta"
    ),
  aes(x = Fecha, y = Valor)
) +
  geom_line(color = "#1f77b4", size = 1.2) +
  labs(
    title = "Variation annuelle - Renta nacional disponible bruta",
    x = "",
    y = "Variation annuelle (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


ggplot(
  menage_data2 %>%
    filter(
      Part3 == "Dato base",
      Part2 == "Renta nacional disponible bruta"
    ),
  aes(x = Fecha, y = Valor)
) +
  geom_line(color = "#1f77b4", size = 1.2) +
  labs(
    title = "Variation annuelle - Renta nacional disponible bruta",
    x = "",
    y = "Variation annuelle (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


ggplot(revenu_reel, aes(x = Fecha, y = Revenu_reel)) +
  geom_line(color = "#1f77b4", size = 1.2) +
  labs(
    title = "Renta nacional disponible bruta - en valeur réelle",
    x = "",
    y = "Revenu disponible brut (réel, base 2019)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


base_2019T4 <- revenu_reel %>%
  filter(Fecha == as.Date("2019-10-01")) %>%
  pull(Revenu_reel)

revenu_reel <- revenu_reel %>%
  mutate(Revenu_base2019T4 = Revenu_reel / base_2019T4 * 100)

# 3. Graphique RDB
#ggplot(revenu_reel, aes(x = Fecha, y = Revenu_base2019T4)) +
#geom_line(color = "#1f77b4", size = 1.2) +
#labs(
#title = "Renta nacional disponible bruta - en valeur réelle (base 2019 T4)",
#x = "",
#y = "Revenu disponible brut (réel, base 2019 T4 = 100)"
#) +
#theme_minimal(base_size = 14) +
#theme(plot.title = element_text(face = "bold"))

#Graphique sur le taux d'épargne des menages

# 📈 Graphique 2 : Tasa de ahorro

moyenne_historique <- menage_data2 %>%
  filter(
    Part3 == "Dato base",
    Part2 == "Tasa de ahorro",
    Fecha >= as.Date("2010-01-01"),
    Fecha <= as.Date("2020-12-31")
  ) %>%
  summarise(moy = mean(Valor, na.rm = TRUE)) %>%
  pull(moy)

# Graphique avec ligne moyenne
ggplot(
  menage_data2 %>%
    filter(
      Part3 == "Dato base",
      Part2 == "Tasa de ahorro"
    ),
  aes(x = Fecha, y = Valor)
) +
  geom_line(color = "#ff7f0e", size = 1.2) +
  geom_hline(yintercept = moyenne_historique, 
             color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = as.Date("2011-01-01"), 
           y = moyenne_historique, 
           label = paste0("Moyenne 2010-2020: ", round(moyenne_historique, 1), "%"),
           vjust = -1, hjust = 0, color = "blue", size = 4) +
  labs(
    title = "Tasa de ahorro",
    x = "",
    y = "Taux du RDB"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggplot(
  menage_data2 %>%
    filter(
      Part3 == "Variación anual",
      Part2 == "Ahorro bruto"
    ),
  aes(x = Fecha, y = Valor)
) +
  geom_line(color = "#ff7f0e", size = 1.2) +
  labs(
    title = "Variation annuelle - Tasa de ahorro",
    x = "",
    y = "Variation annuelle (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))


# Filtrer les données pour l'épargne brute (Dato base)
epargne_data <- menage_data2 %>%
  filter(
    Part3 == "Dato base",
    Part2 == "Ahorro bruto" # Vérifie que c'est bien l'intitulé exact
  )

# Calcul de la moyenne historique (2010-2019)
moyenne_epargne <- epargne_data %>%
  filter(Fecha >= as.Date("2010-01-01"),
         Fecha <= as.Date("2019-12-31")) %>%
  summarise(moy = mean(Valor, na.rm = TRUE)) %>%
  pull(moy)

# Calcul de la sur-épargne par trimestre depuis 2020
sur_epargne <- epargne_data %>%
  filter(Fecha >= as.Date("2020-01-01")) %>%
  mutate(Sur_epargne = Valor - moyenne_epargne)

# Agrégation (somme de toute la sur-épargne depuis T1 2020)
total_sur_epargne <- sum(sur_epargne$Sur_epargne, na.rm = TRUE)

# Affichage des résultats
print(paste0("Moyenne historique (2010-2019): ", round(moyenne_epargne, 2)))
print(paste0("Sur-épargne cumulée depuis T1 2020: ", round(total_sur_epargne, 2)))

# Si tu veux voir le tableau de la sur-épargne par trimestre :
sur_epargne %>% select(Fecha, Valor, Sur_epargne)



#Emploi

ggplot(
  data_Emploi %>%
    filter(
      Part1 == "Ocupados",
      Part6 == "Dato base. ",
      Part3 == "Personas",
      Part5 == "Total Nacional"
    ),
  aes(x = Fecha, y = Valor)
) +
  geom_line(color = "#ff7f0e", size = 1.2) +
  labs(
    title = "Emploi total",
    x = "",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"))

ggplot(
  data_Emploi %>%
    filter(
      Part1 == "Ocupados",
      Part6 == "Dato base.",
      Part3 == "Personas",
      Part5 != "Total Nacional" # on enlève le total
    ),
  aes(x = Fecha, y = Valor, color = Part5)
) +
  geom_line(size = 1.2) +
  facet_wrap(~Part5, scales = "free_y") +
  labs(
    title = "Emploi par secteur (série Dato base)",
    x = "",
    y = "Nombre de personnes",
    color = "Secteur"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )



secteurs <- c(
  "Industria manufacturera",
  "Construcción",
  "Servicios",
  "Comercio, transporte y hostelería",
  "Información y comunicaciones",
  "Total CNAE"
)

data_Emploi_Indice <- data_Emploi %>%
  filter(
    Part1 == "Ocupados",
    Part6 == "Dato base.",
    Part3 == "Personas",
    Part5 %in% secteurs
  ) %>%
  group_by(Part5) %>%
  mutate(base_2019Q4 = Valor[Fecha == as.Date("2019-10-01")]) %>%
  mutate(Indice = (Valor / base_2019Q4) * 100) %>%
  ungroup()

# Graphique
ggplot(
  data_Emploi_Indice,
  aes(x = Fecha, y = Indice, color = Part5)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Emploi par secteur (Indice base T4 2019 = 100)",
    x = "",
    y = "Indice (T4 2019 = 100)",
    color = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal" # Étale la légende
  ) +
  guides(color = guide_legend(nrow = 2)) # 2 lignes

data_Emploi_IndiceH <- data_Emploi %>%
  filter(
    Part1 == "Ocupados",
    Part6 == "Dato base.",
    Part3 == "Horas trabajadas",
    Part5 %in% secteurs
  ) %>%
  group_by(Part5) %>%
  mutate(base_2019Q4 = Valor[Fecha == as.Date("2019-10-01")]) %>%
  mutate(Indice = (Valor / base_2019Q4) * 100) %>%
  ungroup()


ggplot(
  data_Emploi_IndiceH,
  aes(x = Fecha, y = Valor, color = Part5)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Heures travaillées par secteur",
    x = "",
    y = "",
    color = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

ggplot(
  data_Emploi_IndiceH,
  aes(x = Fecha, y = Indice, color = Part5)
) +
  geom_line(size = 1.2) +
  labs(
    title = "Heures travaillées par secteur (Indice)",
    x = "",
    y = "",
    color = ""
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"))

save.image(file = "pib_workspace.RData")


library(dplyr)
library(tidyr)
library(janitor)

# --- Recursos ---
recursos <- menage_data %>%
  filter(Part4 == "Recursos") %>%
  filter(Part3 %in% c(
    "Remuneración de los asalariados",
    "Rentas de la propiedad",
    "Cotizaciones sociales netas",
    "Impuestos corrientes sobre la renta, el patrimonio, etc.",
    "Prestaciones sociales distintas de las transferencias sociales en especie",
    "Transferencias sociales en especie",
    "Otras transferencias corrientes"
  )) %>%
  filter(Part5 == "Dato base") %>%
  select(Fecha, Part3, Valor) %>%
  pivot_wider(
    names_from = Part3,
    values_from = Valor,
    values_fn = sum,
    values_fill = 0
  ) %>%
  group_by(Fecha) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
  select(
    -`Impuestos corrientes sobre la renta, el patrimonio, etc.`,
    -`Cotizaciones sociales netas`
  ) %>%
  rename_with(~ paste0(., "_recursos"), -Fecha)   # 🔥 Suffixe "_recursos"


# --- Empleos ---
empleos <- menage_data %>%
  filter(Part4 == "Empleos") %>%
  filter(Part3 %in% c(
    "Remuneración de los asalariados",
    "Rentas de la propiedad",
    "Cotizaciones sociales netas",
    "Impuestos corrientes sobre la renta, el patrimonio, etc.",
    "Prestaciones sociales distintas de las transferencias sociales en especie",
    "Transferencias sociales en especie",
    "Otras transferencias corrientes"
  )) %>%
  filter(Part5 == "Dato base") %>%
  select(Fecha, Part3, Valor) %>%
  pivot_wider(
    names_from = Part3,
    values_from = Valor,
    values_fn = sum,
    values_fill = 0
  ) %>%
  group_by(Fecha) %>%
  summarise(across(everything(), sum, na.rm = TRUE), .groups = "drop") %>%
  select(
    -`Remuneración de los asalariados`,
    -`Prestaciones sociales distintas de las transferencias sociales en especie`
  ) %>%
  rename_with(~ paste0(., "_empleos"), -Fecha)   # 🔥 Suffixe "_empleos"


menages_net <- recursos %>%
  left_join(empleos, by = "Fecha") %>%
  mutate(
    revenu_capital_net = `Rentas de la propiedad_recursos` - `Rentas de la propiedad_empleos`,
    transferts_nets = `Otras transferencias corrientes_recursos` - `Otras transferencias corrientes_empleos`,
  )


# Calcul des contributions nettes depuis 2019
menage_contrib_net <- menages_net %>%
  # Calcul du RDB total net
  mutate(total = rowSums(across(-Fecha), na.rm = TRUE)) %>%
  
  # Pivot long pour avoir une ligne par poste et par date
  pivot_longer(cols = -c(Fecha, total), names_to = "poste", values_to = "valeur") %>%
  arrange(poste, Fecha) %>%
  group_by(poste) %>%
  
  # Index et croissances
  mutate(
    index_2019 = valeur / valeur[Fecha == as.Date("2019-10-01")] * 100,
    croissance_annuelle = (valeur - lag(valeur, 4)) / lag(valeur, 4) * 100,
    croissance_trimestrielle = (valeur - lag(valeur, 1)) / lag(valeur, 1) * 100,
    croissance_2019 = (valeur - valeur[Fecha == as.Date("2019-10-01")]) / valeur[Fecha == as.Date("2019-10-01")] * 100
  ) %>%
  ungroup() %>%
  
  # Part de chaque poste dans le total net
  group_by(Fecha) %>%
  mutate(part = valeur / total * 100) %>%
  ungroup() %>%
  
  # Contribution de chaque poste en points de pourcentage
  group_by(poste) %>%
  mutate(
    contribution_annuelle = croissance_annuelle * lag(part, 4) / 100,
    contribution_trimestrielle = croissance_trimestrielle * lag(part, 1) / 100,
    contribution_2019 = croissance_2019 * part[Fecha == as.Date("2019-10-01")] / 100
  ) %>%
  ungroup()

# Renommer les postes pour le graphique
graph_data_net <- menage_contrib %>%
  mutate(poste = recode(poste,
                        `Remuneración de los asalariados_recursos` = "Salaires",
                        `Rentas de la propiedad_recursos` = "Revenu de propriété",
                        `Prestaciones sociales distintas de las transferencias sociales en especie_recursos` = "Prestations sociales",
                        `Otras transferencias corrientes_recursos` = "Autres transferts",
                        `Cotizaciones sociales netas_empleos` = "Cotisations sociales",
                        `Impuestos corrientes sobre la renta, el patrimonio, etc._empleos` = "Impôts sur le revenu et patrimoine",
                        `revenu_capital_net` = "Revenu de propriété net",
                        `transferts_nets` = "Transferts nets"
  ))


graph_rdb_reel<- ggplot(
  data = graph_data_net %>% 
    filter(poste %in% c(
      "Salaires",
      "Prestations sociales",
      "Autres transferts",
      "Cotisations sociales",
      "Impôts sur le revenu et patrimoine",
      "Revenu de propriété net",
      "Transferts nets"
    ),
    Fecha >= as.Date("2020-01-01")
    ),
  aes(x = Fecha, y = contribution_2019, fill = poste)
) +
  geom_area() +
  scale_y_continuous(name = "Contribution au RDB net (pp)") +
  scale_x_date(name = "") +
  labs(
    title = "Contribution de chaque poste au revenu disponible net des ménages (en nominal) depuis 2020",
    fill = "Poste"
  ) +
  theme_minimal()

graph_rdb_reel <- graph_rdb_reel + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


menage_contrib_net_volume <- menage_deflate %>%
  # Calcul du RDB total net (en volume)
  mutate(total = rowSums(across(-Fecha), na.rm = TRUE)) %>%
  
  # Pivot long pour avoir une ligne par poste et par date
  pivot_longer(cols = -c(Fecha, total), names_to = "poste", values_to = "valeur") %>%
  arrange(poste, Fecha) %>%
  group_by(poste) %>%
  
  # Index et croissances
  mutate(
    index_2019 = valeur / valeur[Fecha == as.Date("2019-10-01")] * 100,
    croissance_annuelle = (valeur - lag(valeur, 4)) / lag(valeur, 4) * 100,
    croissance_trimestrielle = (valeur - lag(valeur, 1)) / lag(valeur, 1) * 100,
    croissance_2019 = (valeur - valeur[Fecha == as.Date("2019-10-01")]) / valeur[Fecha == as.Date("2019-10-01")] * 100
  ) %>%
  ungroup() %>%
  
  # Part de chaque poste dans le total net
  group_by(Fecha) %>%
  mutate(part = valeur / total * 100) %>%
  ungroup() %>%
  
  # Contribution de chaque poste en points de pourcentage
  group_by(poste) %>%
  mutate(
    contribution_annuelle = croissance_annuelle * lag(part, 4) / 100,
    contribution_trimestrielle = croissance_trimestrielle * lag(part, 1) / 100,
    contribution_2019 = croissance_2019 * part[Fecha == as.Date("2019-10-01")] / 100
  ) %>%
  ungroup()

# Renommer les postes pour le graphique
graph_data_net_volume <- menage_contrib_net_volume %>%
  mutate(poste = recode(poste,
                        `Remuneración de los asalariados_recursos` = "Salaires",
                        `Rentas de la propiedad_recursos` = "Revenu de propriété",
                        `Prestaciones sociales distintas de las transferencias sociales en especie_recursos` = "Prestations sociales",
                        `Otras transferencias corrientes_recursos` = "Autres transferts",
                        `Cotizaciones sociales netas_empleos` = "Cotisations sociales",
                        `Impuestos corrientes sobre la renta, el patrimonio, etc._empleos` = "Impôts sur le revenu et patrimoine",
                        `revenu_capital_net` = "Revenu de propriété net",
                        `transferts_nets` = "Transferts nets"
  ))

# Graphique
contribution_rdb_reel <- ggplot(
  data = graph_data_net_volume %>% 
    filter(poste %in% c(
      "Salaires",
      "Prestations sociales",
      "Autres transferts",
      "Cotisations sociales",
      "Impôts sur le revenu et patrimoine",
      "Revenu de propriété net",
      "Transferts nets"
    ),
    Fecha >= as.Date("2020-01-01")
    ),
  aes(x = Fecha, y = contribution_2019, fill = poste)
) +
  geom_area() +
  scale_y_continuous(name = "Contribution au RDB net (pp)") +
  scale_x_date(name = "") +
  labs(
    title = "Contribution de chaque poste au revenu disponible net des ménages (en réel) depuis 2020",
    fill = "Poste"
  ) +
  theme_minimal()


ggsave(
  filename = "contribution_rdb_reel.png",  # nom du fichier
  plot = contribution_rdb_reel,                   # objet ggplot
  width = 10,                              # largeur en pouces
  height = 6,                              # hauteur en pouces
  dpi = 300                                # résolution
)


