library(ggiraph)
library(rsdmx)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readsdmx)
library(eurostat)
library(ofce)   # si tu l’utilises
library(tidyverse)




# Structure des datasets --------------------------------------------------

ESTAT_structure_eesi <- tibble(data.frame(readSDMX('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/ei_bsee_m_r2')@datastructures[[1]]@Components))
ESTAT_structure_eesi

ESTAT_structure_bcsQ <- data.frame(readSDMX('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/ei_bsin_q_r2')@datastructures[[1]]@Components)
ESTAT_structure_bcsQ

ESTAT_structure_bcs_services_Q <- data.frame(readSDMX('https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/datastructure/ESTAT/ei_bsse_q_r2')@datastructures[[1]]@Components)
ESTAT_structure_bcs_services_Q


# Indicateurs BCS ESTAT ---------------------------------------------------

bcs_indusq <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bsin_q_r2/Q.BS-FLP1-PC+BS-FLP2-PC+BS-FLP3-PC.SA.ES")) |> 
  select(obsTime,indic,obsValue) |> 
  mutate(obsTime = yq(obsTime),
         indic = case_when(grepl("FLP1",indic) ~ "Pas de frein",
                           grepl("FLP2",indic) ~ "Demande",
                           grepl("FLP3",indic) ~ "Main-d'oeuvre"),
         sector = "Industrie")

bcs_constm <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bsbu_m_r2/M.BS-FLBA1-PC+BS-FLBA2-PC+BS-FLBA4-PC.SA.ES")) |> 
  select(obsTime,indic,obsValue) |> 
  mutate(obsTime = yq(quarter(ym(obsTime), type = "year.quarter")),
         indic = case_when(grepl("FLBA1",indic) ~ "Pas de frein",
                           grepl("FLBA2",indic) ~ "Demande",
                           grepl("FLBA4",indic) ~ "Main-d'oeuvre"),
         sector = "Construction") |> 
  group_by(obsTime,indic,sector) |>
  summarise(obsValue = mean(obsValue) ) |> 
  ungroup()

bcs_servicesq <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bsse_q_r2/Q.BS-FLB1-PC+BS-FLB2-PC+BS-FLB3-PC.SA.BAL.ES")) |> 
  select(obsTime,indic,obsValue) |> 
  mutate(obsTime = yq(obsTime),
         indic = case_when(grepl("FLB1",indic) ~ "Pas de frein",
                           grepl("FLB2",indic) ~ "Demande",
                           grepl("FLB3",indic) ~ "Main-d'oeuvre"),
         sector = "Services")

bcs_indics <- bind_rows(bcs_indusq,bcs_constm, bcs_servicesq) |> 
  mutate(tooltip_bcs = str_c((obsTime),"<br>",indic,"<br>",round(obsValue,1)))




graph_bcs_freins <- girafe(
  ggobj = bcs_indics |> 
    filter(obsTime > "2018-12-01") |> 
    ggplot(aes(x = obsTime, y = obsValue, colour = indic)) +
    facet_wrap(~sector) +
    geom_line() +
    geom_point_interactive(aes(tooltip = tooltip_bcs), size = 1) +
    theme_ofce(base_family = "sans") +
    ylab("") +
    xlab("") +
    labs(colour = "Type de frein") +   # <- Titre de la légende
    scale_colour_manual(
      values = c("Pas de frein" = "forestgreen",
                 "Demande" = "steelblue",
                 "Main-d'oeuvre" = "firebrick")
    )
)




# Industrie quartely 

indus_quartely <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bsin_q_r2/Q.BS-ICU-PC+BS-ICPC-BAL+BS-INO-BAL.SA.ES")) |> 
  select(obsTime,indic,obsValue) |> 
  mutate(obsTime = yq(obsTime),
         indic = case_when(grepl("ICU",indic) ~ "Current level of capacity utilization (%)",
                           grepl("ICPC",indic) ~ "Assessment of current production capacity",
                           grepl("INO",indic) ~ "New orders in recent months"),
         sector = "Industrie")


# Capacité d'utilisation
p1 <- indus_quartely %>%
  filter(indic == "Current level of capacity utilization (%)") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(color = "firebrick", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Taux d’utilisation des capacités (Industrie, Espagne)",
    x = "Date", y = "%"
  )

# Capacité de production
p2 <- indus_quartely %>%
  filter(indic == "Assessment of current production capacity") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Capacité de production actuelle (Industrie, Espagne)",
    x = "", y = ""
  )

# Nouvelles commandes
p3 <- indus_quartely %>%
  filter(indic == "New orders in recent months") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(color = "forestgreen", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Nouvelles commandes récentes (Industrie, Espagne)",
    x = "", y = ""
  )

# Afficher les trois graphiques
p1
p2
p3



# Construction quartely 

cons_quartely <- data.frame(readSDMX("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bsbu_m_r2/M.BS-CEME-BAL+BS-CTA-BAL+BS-COB-BAL+BS-CPE-BAL.SA.ES")) |> 
  select(obsTime,indic,obsValue) |> 
  mutate(obsTime = yq(obsTime),
         indic = case_when(grepl("CEME",indic) ~ "Employment expectations over the next 3 months",
                           grepl("CTA",indic) ~ "Building activity development over the past 3 months",
                           grepl("COB",indic) ~ "Evolution of the current overall order books",
                           grepl("CPE",indic) ~ "Price expectations over the next 3 months"),
         sector = "Construction")

# Emploi attendu
p1c <- cons_quartely %>%
  filter(indic == "Employment expectations over the next 3 months") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "firebrick", size = 1) +
  geom_point(color = "firebrick", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Emploi attendu à 3 mois (Construction, Espagne)",
    x = "Date", y = "Solde d’opinion"
  )

# Activité passée
p2c <- cons_quartely %>%
  filter(indic == "Building activity development over the past 3 months") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Activité passée sur 3 mois (Construction, Espagne)",
    x = "", y = ""
  )

# Carnets de commandes
p3c <- cons_quartely %>%
  filter(indic == "Evolution of the current overall order books") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_point(color = "forestgreen", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Carnets de commandes (Construction, Espagne)",
    x = "", y = ""
  )

# Prix attendus
p4c <- cons_quartely %>%
  filter(indic == "Price expectations over the next 3 months") %>%
  ggplot(aes(x = obsTime, y = obsValue)) +
  geom_line(color = "darkorange", size = 1) +
  geom_point(color = "darkorange", size = 1) +
  theme_minimal(base_family = "sans") +
  labs(
    title = "Prix attendus à 3 mois (Construction, Espagne)",
    x = "", y = ""
  )

# Pour afficher les graphiques un par un
p1c
p2c
p3c
p4c





url_indic <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/codelist/ESTAT/INDIC"
codelist_indic <- readSDMX(url_indic)
df_indic <- as.data.frame(codelist_indic) %>% 
  select(id, label.en) %>% 
  rename(code = id, description = label.en)
print(head(df_indic, 20))

toc <- get_eurostat_toc()


conf <- get_eurostat("teibs020", filters = list(geo = "ES"), time_format = "date")

# Filtrer par indicateur (ex: services)
conf_sci <- conf %>%
  filter(indic == "BS-SCI") %>%
  select(time, values) %>%
  mutate(indicateur = "Services confidence indicator")

conf_all <- get_eurostat("teibs020", time_format = "raw")
range(conf_all$TIME_PERIOD)
names(conf_all)

url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/teibs020?startPeriod=2000"

conf_all <- readSDMX(url) %>%
  as.data.frame() %>%
  mutate(date = yq(obsTime))

search_eurostat("confidence indicator")




bcs_construction <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-CCI-BAL.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Construction"
  )

ggplot(bcs_construction, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Construction Business Survey",
    x = "Date",
    y = "Valeur"
  )


bcs_industrie <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-ICI-BAL.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Industrie"
  )


ggplot(bcs_industrie, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Industrie Business Survey",
    x = "Date",
    y = "Valeur"
  )


bcs_service <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-SCI-BAL.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Service"
  )


ggplot(bcs_service, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Service Business Survey",
    x = "Date",
    y = "Valeur"
  )


bcs_retail <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-RCI-BAL.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Retail"
  )


ggplot(bcs_retail, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Retail Business Survey",
    x = "Date",
    y = "Valeur"
  )


bcs_all <- bind_rows(bcs_construction, bcs_industrie, bcs_service, bcs_retail)

confidence <- ggplot(bcs_all, aes(x = obsTime, y = obsValue, colour = sector)) +
  geom_line() +
  theme_ofce() +
  labs(
    title = "Confidence Indicator: Construction, Industrie, Services et Retail",
    x = "",
    y = "Valeur",
    colour = "Secteur"
  )



bcs_climate <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-ESI-I.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Economic Sentiment"
  )



ggplot(bcs_climate, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Economic Sentiment Indicator",
    x = "Date",
    y = "Valeur"
  )

bcs_consummer <- readSDMX(
  "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/ei_bssi_m_r2/M.BS-CSMCI-BAL.SA.ES"
) %>%
  as.data.frame() %>%
  mutate(
    obsTime = ym(obsTime),
    sector = "Consummer"
  )

ggplot(bcs_consummer, aes(x = obsTime, y = obsValue, colour = geo)) +
  geom_line() +
  geom_point(size = 0.5) +
  theme_ofce() +
  labs(
    title = "Consummer Confidence Indicator",
    x = "Date",
    y = "Valeur"
  )




