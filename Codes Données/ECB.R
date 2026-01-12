snf_rate <- data.frame(readSDMX("https://sdw-wsrest.ecb.europa.eu/service/data/MIR/M.ES.B.A2A.A.R.A.2240.EUR.N")) |>
  select(obsTime,nfc_rate = obsValue) |>
  mutate(obsTime=ym(obsTime))

menage_rate <- data.frame(readSDMX("https://sdw-wsrest.ecb.europa.eu/service/data/MIR/M.ES.B.A2C.AM.R.A.2250.EUR.N")) |>
  select(obsTime,menage_rate = obsValue) |>
  mutate(obsTime=ym(obsTime))

library(readr)
library(readxl)
library(rsdmx)
library(dplyr)

df <- read_delim("C:/Users/153003/Downloads/IRLTLT01ESM156N.csv", delim = ";")%>%
mutate(obsTime = as.Date(observation_date, format="%d/%m/%Y"))

df_merged <- df %>%
  left_join(snf_rate, by = "obsTime") %>%
  left_join(menage_rate, by = "obsTime")%>%
  select(-c(observation_date))%>%
  rename (Tauxlong = "IRLTLT01ESM156N" )

df2 <- read_xlsx("C:/Users/153003/Downloads/ECB Data Portal.xlsx") %>%
  rename(TauxECB = `Deposit facility - date of changes (raw data) - Level (FM.D.U2.EUR.4F.KR.DFR.LEV)`) %>%
  select(-`TIME PERIOD`) %>%  # Supprimer la colonne inutile
  mutate(DATE = as.Date(DATE),  # Convertir la date
         annee_mois = floor_date(DATE, "month"),
         TauxECB = as.numeric(TauxECB)) %>%  # Extraire l'année-mois
  group_by(annee_mois) %>%  
  mutate(moyenne_mensuelle = mean(TauxECB, na.rm = TRUE)) %>%  # Ajouter la moyenne sans résumer
  ungroup()%>% 
  rename( obsTime = annee_mois )

df3 <- df2[, c("moyenne_mensuelle", "obsTime")]
library(dplyr)
library(tidyverse)  

# Grouper par mois (en utilisant la colonne obsTime) et conserver une ligne par mois
df3 <- df3 %>%
  group_by(format(obsTime, "%Y-%m")) %>%  # Grouper par année-mois
  summarise(moyenne_mensuelle = first(moyenne_mensuelle),  # Conserver la moyenne mensuelle pour chaque mois
            obsTime = first(obsTime)) %>%  # Conserver la première date du mois
  ungroup()%>% 
  select(c("moyenne_mensuelle", "obsTime"))%>% 
  rename(Tauxcourt = moyenne_mensuelle)


df_merged <- df_merged %>%
  left_join(df3, by = "obsTime")

# Régression linéaire pour nfc_rate en fonction de Tauxlong et Tauxcourt
model_nfc <- lm(nfc_rate ~ Tauxlong + Tauxcourt, data = df_merged)

# Résumé du modèle
summary(model_nfc)

model_menage <- lm(menage_rate ~ Tauxlong + Tauxcourt, data = df_merged)
summary(model_menage)


# Construction de l’indicateur basé sur les coefficients de la régression de nfc_rate
df_merged <- df_merged %>%
  mutate(taux_synthetique_nfc = 1.86420 + (0.26834 * Tauxlong) + (0.60751 * Tauxcourt))

# Construction de l’indicateur basé sur les coefficients de la régression de menage_rate
df_merged <- df_merged %>%
  mutate(taux_synthetique_menage = 1.74431 + (0.28346 * Tauxlong) + (0.49114 * Tauxcourt))

df_long <- df_merged %>%
  select(obsTime, taux_synthetique_nfc, taux_synthetique_menage, menage_rate, nfc_rate) %>%
  pivot_longer(cols = -obsTime, names_to = "Type_Taux", values_to = "Valeur")%>%
filter(obsTime >= as.Date("2000-01-01")) 

# Création du graphique ggplot
ggplot(df_long, aes(x = obsTime, y = Valeur, color = Type_Taux)) +
  geom_line(size = 1) +  # Tracer les courbes
  labs(
    title = "Évolution des taux synthétiques et des taux réels depuis 2000",
    x = "",
    y = "Taux d'intérêt",
    color = "Type de Taux"
  ) +
  scale_color_manual(values = c(
    "taux_synthetique_nfc" = "blue",
    "taux_synthetique_menage" = "red",
    "menage_rate" = "purple",
    "nfc_rate" = "green"
  )) +
  theme_minimal() +
  theme(legend.position = "bottom") 
  theme_minimal()
  
ECB <- df2 %>%
    mutate(annee = year(DATE)) %>%  # Extraire l'année
    group_by(annee) %>%  
    summarise(moyenne_annuelle = mean(TauxECB, na.rm = TRUE)) %>%  # Moyenne annuelle
    ungroup()

df_annual <- df %>%
  mutate(annee = year(obsTime)) %>%  # Extraire l'année
  group_by(annee) %>%  
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%  # Moyenne annuelle pour toutes les colonnes numériques
  ungroup()

# Afficher les premières lignes
head(df_annual)

