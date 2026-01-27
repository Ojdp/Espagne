# packages
library(readxl)
library(dplyr)
library(httr)
library(lubridate)
library(stringr)

url <- "https://www.bde.es/webbe/es/estadisticas/compartido/datos/xlsx/be1107.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))

deuda_df <- read_excel("be1107.xlsx", sheet = 1) %>%
  select(Fecha = 1, dette = `DTNPDE2010_P0000P_PS_APU`) %>%
  slice(-1:-5) %>% 
  mutate(
    Fecha = str_replace(Fecha, "DIC", "DEC"),  # remplace seulement "DIC"
    Fecha = paste0("01-", Fecha),              # ajoute le jour pour former "01-DEC 1995"
    Fecha = dmy(Fecha)                         # convertit en date R
  )%>% 
filter(!is.na(Fecha))


url <- "https://www.bde.es/webbe/es/estadisticas/compartido/datos/xlsx/be1109.xlsx"

temp_file <- tempfile(fileext = ".xlsx")
GET(url, write_disk(temp_file, overwrite = TRUE))
raw_df <- read_excel(temp_file, sheet = 1)

solde_df <- read_excel(temp_file, sheet = 1) %>%
  select(
    Fecha = 1,
    solde = `DTNSEC2010_S000NP_APU`
  ) %>%
  slice(-1:-5) %>%
  mutate(
    Fecha = str_replace(Fecha, "DIC", "DEC"),
    Fecha = paste0("01-", Fecha),
    Fecha = dmy(Fecha),
    solde = -as.numeric(solde)
  ) %>%
  filter(!is.na(Fecha))

macro_df <- solde_df %>%
  inner_join(deuda_df, by = "Fecha")

write.csv(deuda_clean, "deuda_PDE_pct_PIB.csv", row.names = FALSE)

head(deuda_clean)


