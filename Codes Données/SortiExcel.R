# Installer les packages si nécessaire :
# install.packages(c("dplyr", "tidyr", "readxl", "openxlsx"))

library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(writexl)
library(purrr)
library(here)

# ------------------------------
# 1. Charger ton fichier Excel modèle
# ------------------------------
#fichier_excel <- "H:/Drive partagés/DAP/donnees_conj/Espagne/Donnees/Comptabilite nationale/D_PIB_VA.xlsx"
# <-- Remplace par le chemin réel
#excel_data <- read_excel(fichier_excel)

output_dir <- here("Codes Données", "Sorties Excel")
# ------------------------------
# 2. Table de correspondance colonnes (À COMPLÉTER)
# ------------------------------
correspondance <- tribble(
  ~col_excel, ~col_df,
  "Spain, Production Approach, Gross Domestic Product, Total, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100", 
  "Producto interior bruto a precios de mercado",
  
  "Spain, Production Approach, Value Added, Economic Activity, Agriculture and Fishing, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "VABpb Agricultura, ganadería, silvicultura y pesca (A, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Industry, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "VABpb Industria",
  
  "Spain, Production Approach, Value Added, Economic Activity, Manufacturing, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Industria manufacturera (C, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Construction, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "VABpb Construcción (F, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Services, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "VABpb Servicios",
  
  "Spain, Production Approach, Value Added, Economic Activity, Trade, Transportation and Hostelry, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Comercio, transporte y hostelería (G-I, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Information and Communication, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Información y comunicaciones (J, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Financial and Insurance Activities, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Actividades financieras y de seguros (K, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Real Estate Activities, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Actividades inmobiliarias (L, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Professional Activities, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Actividades profesionales, científicas y técnicas y otras (M-N, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Public Administration, Health and Education, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Administración pública, educación y sanidad (O-Q, CNAE 2009)",
  
  "Spain, Production Approach, Value Added, Economic Activity, Arts, Entertainment and Other Services Activities, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Actividades artísticas, recreativas y otros servicios (R-T, CNAE 2009)",
  
  "Spain, Production Approach, Taxes On Products, Taxes On Products, Total, Constant Prices, Calendar Adjusted, SA, Index, 2020 = 100",
  "Impuestos menos subvenciones sobre los productos"
  
)


# Fonction make_wide modifiée
make_wide <- function(df, value_col, suffix = "", file_name = NULL) {
  
  # Colonnes pour créer le nom des variables
  part_cols <- intersect(c("Part3", "Part4", "Part5", "Part6"), colnames(df))
  
  # Construction du data frame wide
  wide_df <- df %>%
    select(Fecha, all_of(part_cols), {{ value_col }}) %>%
    mutate(
      variable = apply(select(., all_of(part_cols)), 1,
                       function(x) paste(x[x != ""], collapse = " - ")),
      variable = paste0(variable, suffix)
    ) %>%
    select(-all_of(part_cols)) %>%
    pivot_wider(
      names_from = variable,
      values_from = {{ value_col }},
      values_fn = \(x) x[1]
    )
  
  # Écriture automatique dans le dossier output_dir si file_name fourni
  if (!is.null(file_name)) {
    write_xlsx(wide_df, file.path(output_dir, file_name))
  }
  
  wide_df
}

## ======================
## Utilisation pour PIB
## ======================

# Offre
pib_O_vol_wide <- make_wide(pib_O_vol, Dato_base, "_volume", "pib_O_vol.xlsx")
pib_O_val_wide <- make_wide(pib_O_val, Dato_base, "_valeur", "pib_O_val.xlsx")

pib_O_vol_val <- pib_O_vol_wide %>%
  left_join(pib_O_val_wide, by = "Fecha")

write_xlsx(pib_O_vol_val, file.path(output_dir, "pib_O_vol_val.xlsx"))

# Demande
pib_D_vol_wide <- make_wide(pib_D_vol, Indice, "_volume", "pib_D_vol.xlsx")
pib_D_val_wide <- make_wide(pib_D_val, Dato_base, "_value", "pib_D_val.xlsx")

pib_D_vol_val <- pib_D_vol_wide %>%
  left_join(pib_D_val_wide, by = "Fecha")

write_xlsx(pib_D_vol_val, file.path(output_dir, "pib_D_vol_val.xlsx"))


# ============================================================
# Création fichier excel update Pierre et Mag
# ============================================================
output_path <- "/GitHub/Espagne/Codes Données/Sorties Excel/PIB_VA_Secteurs_volume_valeur.xlsx"

write_xlsx(pib_vol_val_secteurs, output_path)

#emploi

emploi_filtre <- data_Emploi %>%
  filter(Part3 == "Horas trabajadas", Part1 == "Asalariados", Part6 == "Dato base.")

# Si tu veux un tableau large (dates en lignes, variables en colonnes)
emploi_wide <- emploi_filtre %>%
  select(Fecha,Part3, Part5, Part6, Valor) %>% 
  pivot_wider(names_from = Part5, values_from = Valor)%>% 
arrange(desc(Fecha))

# Exporter vers Excel
write.xlsx(emploi_wide, "emploi_heures_travaillees2.xlsx")


emploi_filtre <- data_Emploi %>%
  filter(Part3 == "Personas", Part1 == "Ocupados", Part6 == "Dato base.")

# Si tu veux un tableau large (dates en lignes, variables en colonnes)
emploi_wide <- emploi_filtre %>%
  select(Fecha,Part3, Part5, Part6, Valor) %>% 
  pivot_wider(names_from = Part5, values_from = Valor)%>% 
  arrange(desc(Fecha))

# Exporter vers Excel
write.xlsx(emploi_wide, "emploi_total.xlsx")

emploi_filtre <- data_Emploi %>%
  filter(Part3 == "Personas", Part1 == "Asalariados", Part6 == "Dato base.")

# Si tu veux un tableau large (dates en lignes, variables en colonnes)
emploi_wide <- emploi_filtre %>%
  select(Fecha,Part3, Part5, Part6, Valor, Part1) %>% 
  pivot_wider(names_from = Part5, values_from = Valor)%>% 
  arrange((Fecha))

# Exporter vers Excel
write.xlsx(emploi_wide, "emploi_total_salaries.xlsx")

# 1. Exportations du PIB et des composantes de la demande en valeur vers excel
# nettoyage de la base PIB et composantes de la demande en valeur
pib_D_val_wide <- pib_D_val %>%
  select(Fecha, Part3, Part4, Part5, Part6, Valor) %>% 
  mutate(
    variable = pmap_chr(                                              # pmap : ici pmap_chr renvoie une chaine de caractères pour chaque ligne. Permet d'appliquer une fonction à plusieurs colonnes en parallèle 
      list(Part3, Part4, Part5, Part6),                     # applique la fonction aux colonnes Part3, Part4, Part5, Part6
      ~ paste(c(...)[c(...) != ""], collapse = " - ")       # c(...) construit un vecteur c(Part3, Part4, Part5, Part6) car ... représente les arguments reçus  précédemment
    )) %>%   
  select(-Part3, -Part4, -Part5, -Part6) %>%
  pivot_wider(
    names_from = variable, 
    values_from = Valor,
    values_fn = \(x) x[1]  # prend juste la première valeur
  )

# export vers excel
write_xlsx(pib_D_val_wide, "pib_D_val.xlsx")

# 2. Exportations du PIB et des composantes de la demande en volume vers excel
# nettoyage de la base PIB et composantes de la demande en volume
pib_D_vol_wide <- pib_D_vol %>%
  select(Fecha, Part3, Part4, Part5, Part6, Indice) %>% 
  mutate(
    variable = pmap_chr(                                              # pmap : ici pmap_chr renvoie une chaine de caractères pour chaque ligne. Permet d'appliquer une fonction à plusieurs colonnes en parallèle 
   list(Part3, Part4, Part5, Part6),                     # applique la fonction aux colonnes Part3, Part4, Part5, Part6
   ~ paste(c(...)[c(...) != ""], collapse = " - ")       # c(...) construit un vecteur c(Part3, Part4, Part5, Part6) car ... représente les arguments reçus  précédemment
  )) %>%                                                      # collapse : colle les éléments avec "-" ici 
  select(-Part3, -Part4, -Part5, -Part6) %>%
  pivot_wider(
    names_from = variable, 
    values_from = Indice,
    values_fn = \(x) x[1]  # prend juste la première valeur s'il y a plusieurs combinaisons Fecha + variable
  )

# export vers excel
write_xlsx(pib_D_vol_wide, "pib_D_vol.xlsx")


menage_filtre <- menage_data %>%
  filter(Part4 == "Empleos", Part5 == "Dato base")

menage_wide <- menage_filtre %>%
  select(Fecha, Part3, Part5, Part4, Valor) %>% 
  pivot_wider(names_from = Part3, values_from = Valor)

write_xlsx(menage_wide, "compte_menage.xlsx")

# menage_data2

menage_wide2 <- menage_data2 %>%
  filter(Part3 == "Dato base")%>%
  select(Fecha, Part3, Valor, Part2) %>% 
  pivot_wider(names_from = Part2, values_from = Valor) %>%
arrange(Fecha)  

write_xlsx(menage_wide2, "compte_menage2.xlsx")


# --- 1. PIB valeurs ---
pib_D_val_clean <- pib_D_val %>%
  mutate(Variable = recode(Part3,
                           "Producto interior bruto a precios de mercado" = "PIB Val",
                           "Gasto en consumo final de los hogares" = "C pv Val",
                           "Gasto en consumo final de las AAPP" = "C pb Val",
                           "Formación bruta de capital" = "FBCF Val",
                           "Exportaciones de bienes y servicios" = "X Val",
                           "Importaciones de bienes y servicios" = "M Val",
                           "Variación de existencias y adquisiciones menos cesiones de objetos valiosos" = "Variation de Stock"
  )) %>%
  filter(Variable %in% c("PIB Val", "C pv Val", "C pb Val", 
                         "FBCF Val", "X Val", "M Val", "Variation de Stock")) %>%
  select(Fecha, Variable, Dato_base) %>%
  pivot_wider(names_from = Variable, values_from = Dato_base)

# --- 2. PIB volumes depuis pib_toutes ---
pib_D_vol_clean <- pib_D_vol %>%
  mutate(Variable = recode(Part3,
                           "Producto interior bruto a precios de mercado" = "PIB Vol",
                           "Gasto en consumo final de los hogares" = "C pv Vol",
                           "Gasto en consumo final de las AAPP" = "C pb Vol",
                           "Formación bruta de capital" = "FBCF Vol",
                           "Exportaciones de bienes y servicios" = "X Vol",
                           "Importaciones de bienes y servicios" = "M Vol",
                           "Variación de existencias y adquisiciones menos cesiones de objetos valiosos" =
                             "Variation de Stock Vol"
  )) %>%
  filter(Variable %in% c(
    "PIB Vol", "C pv Vol", "C pb Vol",
    "FBCF Vol", "X Vol", "M Vol", "Variation de Stock Vol"
  )) %>%
  select(Fecha, Variable, Indice) %>%
  pivot_wider(names_from = Variable, values_from = Indice)


# --- 3. Fusion valeurs + volumes ---
pib_final <- pib_D_val_clean %>%
  left_join(pib_D_vol_clean, by = "Fecha")

# --- 4. Ajouter chômage ---
unemployment <- chomage %>%
  filter(Part1 == "Tasa de paro de la población",
         Part2 == "Ambos sexos",
         Part3 == "Total Nacional",
         Part4 == "Todas las edades") %>%
  select(Fecha, Unemployement = Valor)

pib_final <- pib_final %>%
  left_join(unemployment, by = "Fecha")

# --- 5. Ajouter IPCH trimestriel ---
ipc_indice_trim <- ipc_harmonise %>%
  filter(Indicador == "Índice general",
         Tipo == "Índice") %>%
  mutate(Fecha = floor_date(Fecha, unit = "quarter")) %>%
  group_by(Fecha) %>%
  summarise(IPCH = mean(Valor, na.rm = TRUE), .groups = "drop")

pib_final <- pib_final %>%
  left_join(ipc_indice_trim, by = "Fecha")

# --- 6. Réordonner les colonnes ---
pib_final <- pib_final %>%
  select(Fecha, 
         `PIB Val`, `C pv Val`, `C pb Val`, `FBCF Val`, `X Val`, `M Val`,
         `PIB Vol`, `C pv Vol`, `C pb Vol`, `FBCF Vol`, `X Vol`, `M Vol`,
         `Variation de Stock`, `Variation de Stock`,
         Unemployement, IPCH)


population <- population%>%
  filter (Ambito == "Total Nacional",
          Edad == "Todas las edades",
          Sexo == "Total")%>%
  select(Valor, Fecha)%>%
  rename(population = Valor)

pib_final <- pib_final %>%
  left_join(population, by = "Fecha")

# --- 7. Export Excel ---
write.xlsx(pib_final, "pib_final_2.xlsx", overwrite = TRUE)

#Données Banque D'espagne

write_xlsx(deuda_df, "deuda_publica.xlsx")
write_xlsx(solde_df, "solde_public.xlsx")
write_xlsx(macro_df, "macro_solde_dette.xlsx")
write_xlsx(entreprise, "comptes_entreprises.xlsx")
