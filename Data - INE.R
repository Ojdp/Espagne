# Charger les bibliothèques nécessaires

library(ineapir)
library(httr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)
library(ofce)


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
)

ipc_dataMensuel <- get_data_series(
  codSeries = "IPC251855",         # Code de la série pour l'IPC
  dateStart = "1990/01/01",        # Date de début (format yyyy/mm/dd)
  dateEnd = "2024/12/01",          # Date de fin (format yyyy/mm/dd),                       # Nombre de périodes à récupérer
  lang = "ES",                     # Langue des données (ES pour espagnol)
  tip = "A",                       # Format de sortie lisible (A pour lisible)
  validate = TRUE,                 # Validation des paramètres
  verbose = TRUE,                  # Informations supplémentaires (URL générée)
  unnest = TRUE                    # Renvoyer un dataframe unique
)

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
ipc_data_table <- get_data_table(
  idTable = "IPC25333",               # Identifiant de la table IPC
  filter = list("variable1" = "valeur1", "variable2" = "valeur2"),  # Filtres sur les variables (à ajuster selon vos besoins)
  nlast = NULL,                        # Nombre de périodes à récupérer, NULL pour toutes
  det = 0,                             # Niveau de détail (0, 1 ou 2)
  tip = "M",                           # Format lisible
  lang = "ES",                         # Langue des données (espagnol dans cet exemple)
  validate = TRUE,                     # Validation des paramètres
  verbose = TRUE,                      # Afficher l'URL et des informations supplémentaires
  unnest = TRUE,                       # Retourner un data frame unique
  metanames = TRUE,                    # Inclure les noms des variables
  metacodes = TRUE                     # Inclure les codes des variables
)


# Afficher la réponse brute
print(ipc_data2)





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
    T3_Unidad = ifelse(T3_Unidad == "Euros", "Millones Euros", T3_Unidad)
  ) %>%
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
  select(-c(Part2, Part4,T3_TipoDato, T3_Periodo, T3_Escala,Anyo))



  

