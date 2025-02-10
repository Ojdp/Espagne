#création la base Prev _ juste copie du ficher Excel

# Création du dataframe avec les séries et leurs valeurs trimestrielles

dataPrev <- data.frame(
 Fecha = as.Date(c("2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
  "PIB" = c(0.5, 0.5, 0.5, 0.5),
  "Consommation des ménages" = c(0.6, 0.5, 0.5, 0.5),
  "Consommation publique" = c(0.7, 0.7, 0.7, 0.7),
  "FBCF totale" = c(0.7, 0.9, 0.9, 0.9),
  "Demande intérieure" = c(0.8, 1, 1, 1),
  "Variation de stocks" = c(0.5, 0.8, 0.8, 0.8),
  "Exportations de B&S" = c(0.5, 0.5, 0.5, 0.5),
  "Importations de B&S" = c(0.8, 0.9, 1, 1),
  check.names = FALSE  # Empêche R de modifier les noms de colonnes
)%>%
pivot_longer(cols = -Fecha, names_to = "Part3", values_to = "Variación_trimestral")

pib_D_vol_tab_extended <- bind_rows(pib_D_vol_tab, dataPrev)


dataPrev2 <- data.frame(
  Fecha = as.Date(c("2024-01-01", "2024-04-01", "2024-07-01", "2024-10-01","2025-01-01", "2025-04-01", "2025-07-01", "2025-10-01")),
  "Demande intérieure hors stocks" = c(0.7,0.8,0.9,1.2,0.6, 0.6, 0.6, 0.6), 
  "Variation de stocks" = c(-0.2,0,0,-0.1,0,0,0,0), 
  "Commerce extérieur" = c(0.5,0,-0.1,-0.4,0.4, -0.1, -0.1, -0.1),
check.names = FALSE  # Empêche R de modifier les noms de colonnes
)%>%
  pivot_longer(cols = -Fecha, names_to = "Part3", values_to = "Aportación_trimestral")

