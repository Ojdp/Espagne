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

pib_dataD_Val <- get_data_table(
  idTable = "67823",            # Table ID for PIB
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
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) 

#jai pas run tout ca
pib_dataD_Vol1 <- get_data_table(
  idTable = "67824",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(T3_Unidad== "Índice")%>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:9), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(
    # Trouver la position actuelle de "Dato base" et "Índices de volumen encadenado"
    pos_dato_base = apply(., 1, function(row) which(row == "Dato base")[1]),
    pos_indices = apply(., 1, function(row) which(row == "Índices de volumen encadenados")[1]),
    
    # Déplacer "Dato base" à la colonne 8 et "Índices de volumen encadenado" à la colonne 9
    Part8 = ifelse(!is.na(pos_dato_base), "Dato base", NA),
    Part9 = ifelse(!is.na(pos_indices), "Índices de volumen encadenados", NA)
  ) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Dato base", "Índices de volumen encadenados"), NA, .)))%>%
  mutate(Part8 = "Dato base",
         Part9 = "Índices de volumen encadenados")%>%
  rename( "Dato_base" = Valor)%>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  select(-c(Part8,Part7, Part1, Part2, T3_Escala,T3_TipoDato,T3_Periodo, Anyo,T3_Unidad, pos_dato_base, pos_indices))

head(pib_dataD_Vol1$Fecha)

pib_dataD_Vol2 <- get_data_table(
  idTable = "67824",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(T3_Unidad== "Tasas")%>%
  filter(str_detect(Nombre,"Variación trimestral")) %>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:9), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(
    pos_dato_base = apply(., 1, function(row) which(row == "Variación trimestral")[1]),
    pos_indices = apply(., 1, function(row) which(row == "Índices de volumen encadenados")[1]),
    Part8 = ifelse(!is.na(pos_dato_base), "Variación trimestral", NA),
    Part9 = ifelse(!is.na(pos_indices), "Índices de volumen encadenados", NA)
  ) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Variación trimestral", "Índices de volumen encadenados"), NA, .)))%>%
  mutate(Part8 = "Variación trimestral",
         Part9 = "Índices de volumen encadenados")%>%
  rename( "Variación_trimestral" = Valor)%>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  select(-c(Part8,Part7, Part1, Part2, T3_Escala,T3_TipoDato,T3_Periodo, Anyo,T3_Unidad, pos_dato_base, pos_indices))

pib_dataD_Vol3 <- get_data_table(
  idTable = "67824",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(T3_Unidad== "Tasas")%>%
  filter(str_detect(Nombre,"Variación anual")) %>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:9), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(
    pos_dato_base = apply(., 1, function(row) which(row == "Variación anual")[1]),
    pos_indices = apply(., 1, function(row) which(row == "Índices de volumen encadenados")[1]),
    Part8 = ifelse(!is.na(pos_dato_base), "Variación anual", NA),
    Part9 = ifelse(!is.na(pos_indices), "Índices de volumen encadenados", NA)
  ) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Variación anual", "Índices de volumen encadenados"), NA, .)))%>%
  mutate(Part8 = "Variación anual",
         Part9 = "Índices de volumen encadenados")%>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  rename( "Variación_anual" = Valor)%>%
  select(-c(Part8,Part7, Part1, Part2, T3_Escala,T3_TipoDato,T3_Periodo, Anyo,T3_Unidad, pos_dato_base, pos_indices))

pib_dataD_Vol4 <- get_data_table(
  idTable = "67824",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(T3_Unidad== "Puntos porcentuales")%>%
  filter(str_detect(Nombre,"Aportación anual")) %>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:9), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(
    pos_dato_base = apply(., 1, function(row) which(row == "Aportación anual")[1]),
    pos_indices = apply(., 1, function(row) which(row == "Índices de volumen encadenados")[1]),
    Part8 = ifelse(!is.na(pos_dato_base), "Aportación anual", NA),
    Part9 = ifelse(!is.na(pos_indices), "Índices de volumen encadenados", NA)
  ) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Aportación anual", "Índices de volumen encadenados"), NA, .)))%>%
  mutate(Part8 = "Aportación anual",
         Part9 = "Índices de volumen encadenados")%>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  rename( "Aportación_anual" = Valor)%>%
  select(-c(Part8,Part7, Part1, Part2, T3_Escala,T3_TipoDato,T3_Periodo, Anyo,T3_Unidad, pos_dato_base, pos_indices))

pib_dataD_Vol5 <- get_data_table(
  idTable = "67824",  
  filter = NULL,   
  nlast = NULL,    
  det = 0,         
  tip = "A",       
  lang = "ES",     
  validate = TRUE, 
  verbose = TRUE,  
  unnest = TRUE    
) %>%
  filter(T3_Unidad== "Puntos porcentuales")%>%
  filter(str_detect(Nombre,"Aportación trimestral"))%>%
  filter(str_detect(Nombre,"Datos ajustados de estacionalidad y calendario")) %>%
  separate(Nombre, into = paste0("Part", 1:9), sep = "\\. ", fill = "right", extra = "merge") %>%
  mutate(
    pos_dato_base = apply(., 1, function(row) which(row == "Aportación trimestral")[1]),
    pos_indices = apply(., 1, function(row) which(row == "Índices de volumen encadenados")[1]),
    Part8 = ifelse(!is.na(pos_dato_base), "Aportación trimestral", NA),
    Part9 = ifelse(!is.na(pos_indices), "Índices de volumen encadenados", NA)
  ) %>%
  mutate(across(everything(), ~ ifelse(. %in% c("Aportación trimestral", "Índices de volumen encadenados"), NA, .)))%>%
  mutate(Part8 = "Aportación trimestral",
         Part9 = "Índices de volumen encadenados")%>%
  mutate(Fecha = as.Date(sub("T.*", "", Fecha)))%>%
  rename( "Aportación_trimestral" = Valor)%>%
  select(-c(Part8,Part7, Part1, Part2, T3_Escala,T3_TipoDato,T3_Periodo, Anyo,T3_Unidad, pos_dato_base, pos_indices))

merged_data <- merge(pib_dataD_Vol1, pib_dataD_Vol2, by = c("Part3","Part4","Part5", "Part6","Fecha", "Part9"))%>%
  select(-c(COD.x, COD.y))
merged_data2 <- merge(merged_data, pib_dataD_Vol3, by = c("Part3","Part4","Part5", "Part6","Fecha", "Part9"))%>%
  select(-c(COD))
merged_data3 <- merge(merged_data2, pib_dataD_Vol4, by = c("Part3","Part4","Part5", "Part6","Fecha", "Part9"))%>%
  select(-c(COD))
pib_dataD_Vol <- merge(merged_data3, pib_dataD_Vol5, by = c("Part3","Part4","Part5", "Part6","Fecha", "Part9"))%>%
  select(-c(COD)) 

ggplot(aes(x = as.Date(Fecha), y = Aportación_trimestral), data = pib_dataD_Vol) +
  geom_bar(data = pib_dataD_Vol %>% 
             filter(Part3 %in% c("Demanda nacional", "Formación bruta de capital fijo (FBCF)", "Exportaciones de bienes y servicios", "Importaciones de bienes y servicios" ,"Variación de existencias y adquisiciones menos cesiones de objetos valiosos" )),
           aes(fill = Part3), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(pib_dataD_Vol, Part3 == "Producto interior bruto a precios de mercado"),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: INE",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 12), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2010-04-01", max(pib_dataD_Vol$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


ggplot(aes(x = as.Date(Fecha), y = Aportación_anual), data = pib_dataD_Vol) +
  geom_bar(data = pib_dataD_Vol %>% 
             filter(Part3 %in% c("Demanda nacional", "Formación bruta de capital fijo (FBCF)", "Exportaciones de bienes y servicios", "Importaciones de bienes y servicios" ,"Variación de existencias y adquisiciones menos cesiones de objetos valiosos" )),
           aes(fill = Part3), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(pib_dataD_Vol, Part3 == "Producto interior bruto a precios de mercado"),
            color = "black", size = 0.8) + 
  labs(
    caption = "Source: INE",
    title = "",
    x = "",
    y = "Contribution en pourcentage à la croissance totale"
  ) +
  theme_ofce(panel.background = element_blank(), text = element_text(family = "Arial", size= 8.5)) +
  theme(legend.position = "bottom") +  
  scale_y_continuous(limits = c(-5, 12), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2010-04-01", max(pib_dataD_Vol$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))
