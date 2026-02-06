# Charger les bibliothèques nécessaires

install.packages("seasonal")
install.packages("ineapir")
install.packages("openxlsx")

library(writexl)
library(openxlsx)
library(ineapir)
library(httr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)
library(ofce)
library(seasonal)
library(readxl)


ls("package:ineapir")

# Récupérer les données de la série pour le PIB (table 67821)

# Directly call the API URL
response <- GET("https://servicios.ine.es/wstempus/js/ES/DATOS_SERIE?nult=1&det=0&ver=3")

# Récupérer les données de la série IPC avec les paramètres définis
ipc_dataAnnuel <- get_data_series(
  codSeries = "IPC251856",         # Code de la série pour l'IPC
  dateStart = "1990/01/01",        # Date de début (format yyyy/mm/dd)
  dateEnd = "2024/12/01",          # Date de fin (format yyyy/mm/dd),                       # Nombre de périodes à récupérer
  lang = "ES",                     # Langue des données (ES pour espagnol)
  tip = "A",                       # Format de sortie lisible (A pour lisible)
  validate = TRUE,                 # Validation des paramètres
  verbose = TRUE,                  # Informations supplémentaires (URL générée)
  unnest = TRUE                    # Renvoyer un dataframe unique
) %>%
  separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = ymd_hms(Fecha, tz = "Europe/Paris")) %>%
  select(-Nombre, -T3_Periodo,-Anyo, -T3_TipoDato, -Nivel)


ipc_dataMensuel <- get_data_series(
  codSeries = "IPC251855",         # Code de la série pour l'IPC
  dateStart = "1990/01/01",        # Date de début (format yyyy/mm/dd)
  dateEnd = "2024/12/01",          # Date de fin (format yyyy/mm/dd),                       # Nombre de périodes à récupérer
  lang = "ES",                     # Langue des données (ES pour espagnol)
  tip = "A",                       # Format de sortie lisible (A pour lisible)
  validate = TRUE,                 # Validation des paramètres
  verbose = TRUE,                  # Informations supplémentaires (URL générée)
  unnest = TRUE                    # Renvoyer un dataframe unique
) %>%
  separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = as_date(ymd_hms(Fecha)))%>%
  select(-Nombre, -T3_Periodo,-Anyo, -T3_TipoDato, -Nivel)


# Récupérer les données
ipc_data2 <- get_data_series(
  codSeries = "IPC50903",
  dateStart = "1990/01/01",        
  dateEnd = "2000/12/01",          
  lang = "ES",                     
  tip = "A",                       
  validate = TRUE,                 
  verbose = TRUE,                  
  unnest = TRUE                    
)

# Exemple d'utilisation de get_data_table avec des filtres
ipc_data_table2 <- get_data_table(
  idTable = "50907",            # Table ID for PIB
  filter = NULL,                # No additional filters
  nlast = NULL,                 # Retrieve all available periods
  det = 0,                      # Detail level: 0 for basic information
  tip = "A",                    # Readable output format
  lang = "ES",                  # Language: Spanish
  validate = TRUE,              # Validate parameters
  verbose = TRUE,               # Display the generated URL
  unnest = TRUE                 # Return data as a single dataframe
) %>%
separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = as_date(ymd_hms(Fecha)),
         Fecha = floor_date(Fecha, unit = "month"))%>%
  select(-Nombre, -T3_Periodo,-Anyo, -T3_TipoDato, -Nivel, -T3_Escala, -T3_Unidad)

ipc_data_table <- get_data_table(
  idTable = "50902",            # Table ID for PIB
  filter = NULL,                # No additional filters
  nlast = NULL,                 # Retrieve all available periods
  det = 0,                      # Detail level: 0 for basic information
  tip = "A",                    # Readable output format
  lang = "ES",                  # Language: Spanish
  validate = TRUE,              # Validate parameters
  verbose = TRUE,               # Display the generated URL
  unnest = TRUE                 # Return data as a single dataframe
) %>%
  separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = as_date(ymd_hms(Fecha)),
         Fecha = floor_date(Fecha, unit = "month"))%>%
  select(-Nombre, -T3_Periodo,-Anyo, -T3_TipoDato, -Nivel, -T3_Escala, -T3_Unidad)



#ipc Harmonisé 

ipc_harmonise <- get_data_series(
  codSeries = "IPCA1847",
  dateStart = "1900/01/01",   # format correct
  dateEnd   = format(Sys.Date(), "%Y/%m/%d"),  # la date d’aujourd’hui au bon format
  lang      = "ES",
  tip       = "A",
  validate  = TRUE,
  verbose   = TRUE,
  unnest    = TRUE
)%>% 
separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = as_date(ymd_hms(Fecha)))%>%
  select(-Nombre, -T3_Periodo,-Anyo, -T3_TipoDato, -Nivel, -T3_Escala, -T3_Unidad, -Notas, -COD)

ponderation <- get_data_table(
  idTable = "50946",            # Table ID for PIB
  filter = NULL,                # No additional filters
  nlast = NULL,                 # Retrieve all available periods
  det = 0,                      # Detail level: 0 for basic information
  tip = "A",                    # Readable output format
  lang = "ES",                  # Language: Spanish
  validate = TRUE,              # Validate parameters
  verbose = TRUE,               # Display the generated URL
  unnest = TRUE                 # Return data as a single dataframe
) %>%
  separate(Nombre, into = c("Nivel", "Indicador", "Tipo"), sep = "\\. ", remove = FALSE) %>%
  mutate(Fecha = as_date(ymd_hms(Fecha)))%>%
  select(-Nombre,-COD, -Nivel, -Tipo,-T3_Periodo,-Anyo, -T3_TipoDato, -T3_Escala, -T3_Unidad)

# Fetch data from the table for PIB, Current prices
pib_dataS <- get_data_table(
idTable = "67821",            # Table ID for PIB
filter = NULL,                # No additional filters
nlast = NULL,                 # Retrieve all available periods
det = 0,                      # Detail level: 0 for basic information
tip = "A",                    # Readable output format
lang = "ES",                  # Language: Spanish
validate = TRUE,              # Validate parameters
verbose = TRUE,               # Display the generated URL
 unnest = TRUE                 # Return data as a single dataframe
 )%>%
 separate(
 col = Nombre,               # La colonne à séparer
into = c("Part1", "Part2", "Part3", "Part4", "Part5"),  # Noms des nouvelles colonnes
 sep = "\\. ",               # Séparateur : point suivi d'un espace
 fill = "right",             # Remplit les colonnes manquantes avec NA si besoin
extra = "merge"             # Conserve les parties non divisées dans la dernière colonne
 )%>%
filter(Part2 == "Datos ajustados de estacionalidad y calendario")%>%
select (-c(Part1,T3_TipoDato,T3_Periodo, Anyo ))%>%
mutate(
Fecha = as.Date(ymd_hms(Fecha)),
Part3 = ifelse(Part3 %in% c("VABpb Servicios", "VABpb Industria"), Part4, Part3),
Part4 = Part4 %>%
str_replace("Industria manufacturera \\(C, CNAE 2009\\)", "") %>%
str_replace("Precios corrientes", "") %>%  # Correction ici pour supprimer "Precios corrientes"
str_trim() %>%
ifelse(. %in% c("Dato base", "Variación trimestral", "Variación anual"), ., "") %>%
 ifelse(. == "", Part5, .),
 Part4 = str_replace(Part4, "Variación trimestral\\. Precios corrientes\\.", "Variación trimestral"),
Part4 = str_replace(Part4, "Variación anual\\. Precios corrientes\\.", "Variación anual"),
Part4 = str_replace(Part4, "Dato base\\. Precios corrientes\\.", "Dato base"),
T3_Unidad = ifelse(T3_Unidad == "Euros", "Millones Euros", T3_Unidad) ) %>%
arrange(Fecha)%>%
group_by(Part3)%>%
group_by(Part4)%>%
select(-c(T3_Escala, Part5, Part2))

pib_data_wider <- pib_dataS %>%
  filter(Part4 == "Variación trimestral") %>%  # Filtrer uniquement les lignes correspondantes
  pivot_wider(
    names_from = Part4,    # Transformer les valeurs de Part4 en noms de colonnes
    values_from = Valor    # Les valeurs associées proviennent de la colonne Valor
  )

pib_data_wider2 <- pib_dataS %>%
  filter(Part4 == "Variación anual") %>%  # Filtrer uniquement les lignes correspondantes
  pivot_wider(
    names_from = Part4,    # Transformer les valeurs de Part4 en noms de colonnes
    values_from = Valor    # Les valeurs associées proviennent de la colonne Valor
  )

pib_data_wider3 <- pib_dataS %>%
  filter(Part4 == "Dato base") %>%  # Filtrer uniquement les lignes correspondantes
  pivot_wider(
    names_from = Part4,    # Transformer les valeurs de Part4 en noms de colonnes
    values_from = Valor    # Les valeurs associées proviennent de la colonne Valor
  )


pib_data_temp <- full_join(pib_data_wider, pib_data_wider2, by = c("Fecha", "Part3", "T3_Unidad")) %>%  
  select(-c(COD.y, T3_Unidad))

pib_dataS <- full_join(pib_data_temp, pib_data_wider3, by = c("Fecha", "Part3"))%>%
select(-c(COD.x, T3_Unidad))%>%
  rename(Valor =`Dato base`)%>%
  group_by(Part3)%>%
  arrange(Fecha)%>%
  mutate(
  Mean2015 = mean(Valor[lubridate::year(Fecha) == 2015], na.rm = TRUE),
Index2015 = Valor / Mean2015 * 100,
Index2019 = Valor/ Valor[Fecha=="2019-12-31"]*100)


#PIB côté demande, prix courant

get_pib_data <- function(idTable, unit_filter, keyword, rename_col) {
  get_data_table(
    idTable = idTable,  
    filter = NULL,   
    nlast = NULL,    
    det = 0,         
    tip = "A",       
    lang = "ES",     
    validate = TRUE, 
    verbose = TRUE,  
    unnest = TRUE    
  ) %>%
    filter(T3_Unidad == unit_filter) %>%
    filter(str_detect(Nombre, "Datos ajustados de estacionalidad y calendario")) %>%
    filter(str_detect(Nombre, keyword)) %>%
    separate(Nombre, into = paste0("Part", 1:8), sep = "\\. ", fill = "right", extra = "merge") %>%
    mutate(Part4 = ifelse(Part4 %in% c("Variación trimestral", "Variación anual", "Dato base", "Aportación anual", "Aportación trimestral"), "", Part4)) %>% 
    mutate(Part5 = ifelse(Part5 %in% c("Variación trimestral", "Variación anual", "Dato base", "Aportación anual", "Aportación trimestral"), "", Part5)) %>%
    mutate(Part6 = ifelse(Part6 %in% c("Variación trimestral", "Variación anual", "Dato base", "Aportación anual", "Aportación trimestral"), "", Part6)) %>%# Remplacement par ""
    mutate(Fecha = as.Date(sub("T.*", "", Fecha))) %>%
    rename(!!rename_col := Valor) %>%
    select(Fecha, Part3, Part4, Part5, Part6, !!rename_col)
}



#PIB côté demande
pib_dato_base <- get_pib_data("67823", "Euros", "Datos ajustados de estacionalidad y calendario", "Dato_base")
pib_var_trimestral <- get_pib_data("67823", "Tasas", "Variación trimestral", "Variación_trimestral")
pib_var_anual <- get_pib_data("67823", "Tasas", "Variación anual", "Variación_anual")

pib_D_val <- merge(pib_dato_base, pib_var_trimestral, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_var_anual, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE)

pib_dato_base_vol <- get_pib_data("67824", "Índice", "Datos ajustados de estacionalidad y calendario", "Dato_base")
pib_var_trimestral_vol <- get_pib_data("67824", "Tasas", "Variación trimestral", "Variación_trimestral")
pib_var_anual_vol <- get_pib_data("67824", "Tasas", "Variación anual", "Variación_anual")
pib_aport_anual_vol <- get_pib_data("67824", "Puntos porcentuales", "Aportación anual", "Aportación_anual")
pib_aport_trimestral_vol <- get_pib_data("67824", "Puntos porcentuales", "Aportación trimestral", "Aportación_trimestral")

pib_D_vol <- pib_dato_base_vol %>%
  merge(pib_var_trimestral_vol, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_var_anual_vol, by = c("Fecha", "Part3", "Part4", "Part5","Part6"), all = TRUE) %>%
  merge(pib_aport_anual_vol, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_aport_trimestral_vol, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE)


pib_D_vol <- pib_D_vol %>%
  rename(Indice = Dato_base) %>%
  mutate(
    Part4 = ifelse(str_detect(Part4, "Índices de volumen encadenados"), "", Part4),
    Part5 = ifelse(str_detect(Part5, "Índices de volumen encadenados"), "", Part5),
    Part6 = ifelse(str_detect(Part6, "Índices de volumen encadenados"), "", Part6)
  ) %>%
  arrange(Part3, Fecha) %>%
  group_by(Part3) %>%
  mutate(
    # Date de référence ajustée pour chaque groupe si nécessaire
    Index2019 = Indice / Indice[Fecha == as.Date("2019-10-01")] * 100
  ) %>%
  ungroup()



#PIB côté offre

pib_dato_base2 <- get_pib_data("67821", "Euros", "Datos ajustados de estacionalidad y calendario", "Dato_base")
pib_var_trimestral2 <- get_pib_data("67821", "Tasas", "Variación trimestral", "Variación_trimestral")
pib_var_anual2 <- get_pib_data("67821", "Tasas", "Variación anual", "Variación_anual")

pib_O_val <- merge(pib_dato_base2, pib_var_trimestral2, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_var_anual2, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE)%>%
select(-c(Part5,Part6))


pib_dato_base_vol2 <- get_pib_data("67822", "Índice", "Datos ajustados de estacionalidad y calendario", "Dato_base")
pib_var_trimestral_vol2 <- get_pib_data("67822", "Tasas", "Variación trimestral", "Variación_trimestral")
pib_var_anual_vol2 <- get_pib_data("67822", "Tasas", "Variación anual", "Variación_anual")
pib_aport_anual_vol2 <- get_pib_data("67822", "Puntos porcentuales", "Aportación anual", "Aportación_anual")
pib_aport_trimestral_vol2 <- get_pib_data("67822", "Puntos porcentuales", "Aportación trimestral", "Aportación_trimestral")

pib_O_vol <- pib_dato_base_vol2 %>%
  merge(pib_var_trimestral_vol2, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_var_anual_vol2, by = c("Fecha", "Part3", "Part4", "Part5","Part6"), all = TRUE) %>%
  merge(pib_aport_anual_vol2, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_aport_trimestral_vol2, by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE)%>%
  select(-c(Part5,Part6))

#PIB salaires

# Données en valeur absolue (euros)
pib_dato_base_sal <- get_pib_data("67825", "Euros", "Datos ajustados de estacionalidad y calendario", "Dato_base")
pib_var_trimestral_sal <- get_pib_data("67825", "Tasas", "Variación trimestral", "Variación_trimestral")
pib_var_anual_sal <- get_pib_data("67825", "Tasas", "Variación anual", "Variación_anual")

# Fusion des séries (dato base + variations)
pib_salaires <- merge(pib_dato_base_sal, pib_var_trimestral_sal, 
                      by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE) %>%
  merge(pib_var_anual_sal, 
        by = c("Fecha", "Part3", "Part4", "Part5", "Part6"), all = TRUE)

# Nettoyage et calculs supplémentaires si nécessaire
pib_salaires <- pib_salaires %>%
  rename(Salarios = Dato_base) %>%
  arrange(Part3, Fecha) %>%
  group_by(Part3) %>%
  mutate(
    # Exemple : indexation base 2019 T4 = 100
    Index2019 = Salarios / Salarios[Fecha == as.Date("2019-10-01")] * 100
  ) %>%
  ungroup()


#Emploi

data_Emploi <- get_data_table(
  idTable = "67827",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:6), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  select(-c(COD,Part2, Part4,T3_TipoDato, T3_Periodo, T3_Escala,Anyo, T3_Unidad))

data_Emploi <- data_Emploi %>%
  mutate(
    across(c(Part5, Part6), str_trim) # supprime espaces avant/après
  )


chomage <- get_data_table(
  idTable = "65334",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  separate(Nombre, into = paste0("Part", 1:6), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  select(-c(COD,Part5, Part6,T3_TipoDato, T3_Periodo, T3_Escala,Anyo, T3_Unidad))
  
#Ménages

menage_data <- get_data_table(
  idTable = 62274,
  filter = NULL,
  nlast = NULL,
  det = 0,
  tip = "A",
  lang = "ES",
  validate = TRUE,
  verbose = TRUE,
  unnest = TRUE
) %>%
  separate(
    col = Nombre,
    into = c("Part1", "Part2", "Part3", "Part4", "Part5", "Part6"),
    sep = "\\. ",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha))) %>%
  select(-c(T3_Escala, COD, Part1, T3_TipoDato, Part2, T3_Unidad, Anyo, T3_Periodo))%>%
filter(str_detect(Part6, "Datos ajustados de estacionalidad y calendario"))%>%
select(-c(Part6))

menage_data2 <- get_data_table(
  idTable = 62275,
  filter = NULL,
  nlast = NULL,
  det = 0,
  tip = "A",
  lang = "ES",
  validate = TRUE,
  verbose = TRUE,
  unnest = TRUE
) %>%
  separate(
    col = Nombre,
    into = c("Part1", "Part2", "Part3", "Part4", "Part5", "Part6"),
    sep = "\\. ",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha))) %>%
  filter(str_detect(Part5, "Datos ajustados de estacionalidad y calendario"))%>%
  select(-c(T3_Escala, COD, Part1, Part4, Part5, T3_Unidad, Anyo, T3_Periodo, Part6, T3_TipoDato))
  

#Compte par agent 

comptes_agents <- get_data_table(
  idTable = 62265,
  filter = NULL,
  nlast = NULL,
  det = 0,
  tip = "A",
  lang = "ES",
  validate = TRUE,
  verbose = TRUE,
  unnest = TRUE
) %>%
  separate(
    col = Nombre,
    into = c("Part1", "Part2", "Part3", "Part4", "Part5", "Part6", "Part7"),
    sep = "\\. ",
    fill = "right",
    extra = "merge"
  ) %>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha))) %>%
  select(c(Part1, Part3, Part4, Part5, Fecha, Valor))


#Conjoncture

#Indice de Production Industrielle

ipi_data <- get_data_table(
  idTable = "60273",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)
  
ipi_data <- ipi_data %>%
  select(COD, T3_Periodo, Anyo, Valor, Nombre)%>%
  mutate(
    mois_num = as.integer(sub("M", "", T3_Periodo)),
    mois_str = sprintf("%02d", mois_num),
    Date = as.Date(paste0(Anyo, "-", mois_str, "-01"))
  ) %>%  
  separate(Nombre, into = c("Part1", "Secteur", "Variation"), sep = "\\.") %>% 
select(COD, Variation, Secteur, Date, Valor)

ipi_index2 <- ipi_data %>%
  filter(trimws(Variation) == "Índice",
         trimws(Secteur) %in% c("Industria manufacturera")) %>%
  arrange(Date)

start_year <- year(min(ipi_index2$Date))
start_month <- month(min(ipi_index2$Date))

Valor_ts <- ts(ipi_index2$Valor,
               start = c(start_year, start_month),
               frequency = 12)

m <- seas(Valor_ts)
sa_series <- final(m)   # récupère la série désaisonnalisée

# Ajouter au data.frame
ipi_index2 <- ipi_index2 %>%
  mutate(obsValue_sa = as.numeric(sa_series))

# Graphique
ggplot(ipi_index2, aes(x = Date, y = obsValue_sa)) +
  geom_line(color = "firebrick") +
  geom_point(color = "firebrick", size = 0.5) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Indice de production industrielle (Espagne) - désaisonnalisé",
    x = "Date",
    y = "Indice"
  )

#Indice de Cifra de Negocios Empresarial

indiceCAemp_data <- get_data_table(
  idTable = "61782",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)

indiceCAemp_data <- indiceCAemp_data %>%
  select(COD, T3_Periodo, Anyo, Valor, Nombre)%>%
  mutate(
    mois_num = as.integer(sub("M", "", T3_Periodo)),
    mois_str = sprintf("%02d", mois_num),
    Date = as.Date(paste0(Anyo, "-", mois_str, "-01"))
  ) %>%  
  separate(Nombre, into = c("Part1", "Secteur", "Variation"), sep = "\\.") %>% 
  select(COD, Variation, Secteur, Date, Valor)



#Indice de Cifra de Negocios 

indiceCA_data <- get_data_table(
  idTable = "60288",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)

indiceCA_data <- indiceCA_data %>%
  select(COD, T3_Periodo, Anyo, Valor, Nombre) %>%
  mutate(
    mois_num = as.integer(sub("M", "", T3_Periodo)),
    mois_str = sprintf("%02d", mois_num),
    Date = as.Date(paste0(Anyo, "-", mois_str, "-01"))
  ) %>%
  separate(Nombre, into = c("Part1", "Secteur", "Serie", "Variation"), sep = "\\.") %>%
  select(COD, Serie, Variation, Secteur, Date, Valor)

indice_sa <- indiceCA_data %>%
  filter(trimws(Variation) == "Índice") %>%
  group_by(Secteur, Variation) %>%
  arrange(Date) %>%
  group_modify(~ {
    df <- .
    df$obsValue_sa <- NA_real_  # Valeur par défaut
    if(any(!is.na(df$Valor))){
      ts_data <- ts(df$Valor,
                    start = c(year(min(df$Date)), month(min(df$Date))),
                    frequency = 12)
      sa_fit <- tryCatch(seas(ts_data), error = function(e) NULL)
      if (!is.null(sa_fit)) {
        # Extraire les dates disponibles dans final(sa_fit)
        sa_values <- as.numeric(final(sa_fit))
        n_sa <- length(sa_values)
        n_df <- nrow(df)
        # Assigner uniquement aux dernières n_sa lignes
        df$obsValue_sa[(n_df - n_sa + 1):n_df] <- sa_values
      }
    }
    df
  }) %>%
  ungroup()

save(indice_sa, ipi_index2, file = "data_for_plots.RData")

secteurs_selection <- c(
  "Comercio al por mayor y al por menor; reparación de vehículos de motor y motocicletas",
  "Transporte y almacenamiento",
  "Servicios de información",
  "Venta y reparación de vehículos de motor y motocicletas",
  "Información y comunicaciones",
  "Hostelería"
)



ggplot(
  data = indice_sa %>%
    filter(trimws(Secteur) %in% secteurs_selection,
           trimws(Variation) == "Índice",
           Date >= as.Date("2010-01-01")) %>%  # <-- Filtre pour dates à partir de 2010
    arrange(Date),
  aes(x = Date, y = obsValue_sa)
) +
  geom_line(color = "steelblue", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Indice de chiffre d’affaires (Espagne)",
    subtitle = "Saisonnalité corrigée par secteur depuis 2010",
    x = "Date",
    y = "Indice corrigé"
  ) +
  facet_wrap(~Secteur, scales = "free_y")

ggplot(
  data = indice_sa %>%
    filter(trimws(Secteur) == "Total servicios",
           trimws(Variation) == "Índice",
           Date >= as.Date("2010-01-01")) %>%  # si tu veux limiter depuis 2010
    arrange(Date),
  aes(x = Date, y = obsValue_sa)
) +
  geom_line(color = "steelblue", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Indice de chiffre d’affaires (Espagne)",
    subtitle = "Secteur : Total servicios (saisonnalité corrigée depuis 2010)",
    x = "Date",
    y = "Indice corrigé"
  )


#Construction


#Nombre d'hypothèque

hypotheque_data <- get_data_table(
  idTable = "13896",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)

hypotheque_data <- hypotheque_data %>%
  select(COD, T3_Periodo, Anyo, Valor, Nombre)%>%
  mutate(
    mois_num = as.integer(sub("M", "", T3_Periodo)),
    mois_str = sprintf("%02d", mois_num),
    Date = as.Date(paste0(Anyo, "-", mois_str, "-01"))
  ) %>%  
  separate(Nombre, into = c("Tipo", "CCAA", "Indicador", "Base", "Frecuencia"), sep = "\\.")%>% 
  select(COD, Tipo, CCAA, Indicador, Date, Valor)

#Total des biens transférés

bienstransfere_data <- get_data_table(
  idTable = "6146",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)



#Population

population <- get_data_table(
  idTable = "56934",     # Table INE pour le PIB
  filter = NULL,         # Aucun filtre appliqué ici
  nlast = NULL,          # Toutes les périodes disponibles
  det = 0,               # Détail minimal
  tip = "A",             # Format lisible (A = amigable)
  lang = "ES",           # Langue espagnole
  validate = TRUE,       # Validation des paramètres
  verbose = TRUE,        # Affiche l’URL construite
  unnest = TRUE          # Retourne un seul data.frame
)

population <- population %>%
  select(Anyo, Valor, Nombre, Fecha) %>%
  mutate(Nombre = str_trim(Nombre)) %>%
  separate(
    Nombre,
    into = c("Ambito", "Edad", "Sexo", "Indicador", "Unidad"),
    sep = "\\.\\s*",
    extra = "drop",
    fill = "right"
  ) %>%
  mutate(
    Fecha = as.Date(Fecha),
    Trimestre = paste0("T", quarter(Fecha)),
    Periode_trim = paste0(year(Fecha), "T", quarter(Fecha))
  )%>%
select(-Indicador, -Unidad, -Anyo, -Trimestre, -Periode_trim) 


# Sauvegarder tous les dataframes dans un fichier .RData
save(list = df_objects, file = "all_dataframes.RData")




file_path <- "C:/Users/153003/Downloads/10752.xlsx"

ponderations <- read_excel(file_path, skip = 6) %>%
  rename(Ponderation = 1) %>%
  slice(1:29)

# Afficher les premières lignes
head(data)


#comptes des SNF à télécharger : 62271

