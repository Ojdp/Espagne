# Charger les bibliothèques nécessaires

library(ineapir)
library(httr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)


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





# Fetch data from the table for PIB
pib_data <- get_data_table(
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
      str_trim() %>%
      ifelse(. %in% c("Dato base", "Variación trimestral", "Variación anual"), ., "") %>%
      ifelse(. == "", Part5, .),
    T3_Unidad = ifelse(T3_Unidad == "Euros", "Millones Euros", T3_Unidad)
  ) %>%
  select(-c(T3_Escala, Part5))


