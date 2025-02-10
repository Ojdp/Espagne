install.packages("xlsx")
install.packages("openxlsx")
library(gt)

graph1 <- ggplot(aes(x = as.Date(Fecha), y = Aportación_trimestral), data = pib_D_vol) +
  geom_bar(data = pib_D_vol%>% 
             filter(Part3 %in% c("Gasto en consumo final", "Formación bruta de capital fijo (FBCF)", "Exportaciones de bienes y servicios", "Importaciones de bienes y servicios")),
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

ggsave(filename = "graph1.svg", plot = graph1, width = 8, height = 6, units = "in")

ggplot(aes(x = as.Date(Fecha), y = Aportación_anual), data = pib_D_vol) +
  geom_bar(data = pib_D_vol %>% 
             filter(Part3 %in% c("Gasto en consumo final", "Formación bruta de capital fijo (FBCF)", "Exportaciones de bienes y servicios", "Importaciones de bienes y servicios")),
           aes(fill = Part3), 
           position = "stack", stat = "identity", alpha = .8) +  
  geom_line(data = filter(pib_final_vol, Part3 == "Producto interior bruto a precios de mercado"),
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
  scale_x_date(limits = as.Date(c("2010-04-01", max(pib_D_vol$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))

#Tableau de recap v0

library(gt)


# Renommage des variables dans pib_D_vol pour éviter la répétition


# Tableau principal tab
tab <- pib_D_vol %>%
  filter(Part3 %in% ordre_variables) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables)) %>%
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3) %>%
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  )

tab

pib_D_vol_tab <- pib_D_vol %>%
  mutate(Part3 = recode(Part3,
                        "Producto interior bruto a precios de mercado" = "PIB",
                        "Gasto en consumo final de los hogares" = "Consommation des ménages",
                        "Gasto en consumo final de las AAPP" = "Consommation publique",
                        "Formación bruta de capital fijo (FBCF)" = "FBCF totale",
                        "Exportaciones de bienes y servicios" = "Exportations de B&S",
                        "Importaciones de bienes y servicios" = "Importations de B&S", 
                        "Variación de existencias y adquisiciones menos cesiones de objetos valiosos" = "Variation de stocks", 
                        "Demanda nacional" = "Demande intérieure hors stocks")) %>%
  filter(Part3 %in% c("PIB",
                      "Consommation des ménages",
                      "Consommation publique",
                      "FBCF totale",
                      "Exportations de B&S",
                      "Importations de B&S",
                      "Variation de stocks", "Demande intérieure hors stocks")) %>%
  filter(Fecha >= "2024-01-01") 


ordre_variables <- c("PIB", "Consommation des ménages", "Consommation publique",
                     "FBCF totale", "Exportations de B&S", "Importations de B&S")

ordre_variables2 <- c("Demande intérieure hors stocks", "Variation de stocks","Commerce extérieur")
# Tableau tab2 avec les variations trimestrielles arrondies
tab2 <- pib_D_vol_tab %>%
  filter(Part3 %in% ordre_variables) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables),
         Variación_trimestral = round(Variación_trimestral, 1)) %>%
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3) %>%
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "")

tab2

# Tableau tab3 avec "Commerce Extérieur"
# Calculer le commerce extérieur quand jaurais fait la prev 
# pib_D_vol_tab2 <- pib_D_vol_tab %>%
# filter(Part3 %in% c("Exportations de B&S", "Importations de B&S")) %>%
# group_by(Fecha) %>%
# summarise(
# Exportations = sum(Aportación_anual[Part3 == "Exportations de B&S"]),
# Importations = sum(Aportación_anual[Part3 == "Importations de B&S"]),
# Aportación_trimestral_export = sum(Aportación_trimestral[Part3 == "Exportations de B&S"]),
# Aportación_trimestral_import = sum(Aportación_trimestral[Part3 == "Importations de B&S"])
# ) %>%
# mutate(
# Part3 = "Commerce extérieur",
# Aportación_anual = Exportations + Importations,
# Aportación_trimestral = Aportación_trimestral_export + Aportación_trimestral_import
# ) %>%
# select(Fecha, Part3, Aportación_anual, Aportación_trimestral) %>%
# bind_rows(pib_D_vol_tab)


tab3 <- dataPrev2%>%
  filter(Part3 %in% ordre_variables2) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables2),
         Aportación_trimestral = round(Aportación_trimestral, 1)) %>%
  select(Fecha, Part3, Aportación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Aportación_trimestral) %>%
  arrange(Part3) %>%
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "Contribution trim")

tab3

# Transformer tab2 en data frame sans gt()
tab2_df <- pib_D_vol_tab %>%
  filter(Part3 %in% ordre_variables) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables),
         Variación_trimestral = round(Variación_trimestral, 1)) %>%
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3)

# Transformer tab3 en data frame sans gt()
tab3_df <- dataPrev2 %>%
  filter(Part3 %in% ordre_variables2) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables2),
         Aportación_trimestral = round(Aportación_trimestral, 1)) %>%
  select(Fecha, Part3, Aportación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Aportación_trimestral) %>%
  arrange(Part3)

tab_final_df <- bind_rows(tab2_df, tab3_df)

tab_final <- tab_final_df %>%
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  tab_header(
    title = md("**Compte V0**"),
    subtitle = md("*Contributions à la croissance*")
  ) %>%
  cols_label(Part3 = "") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = Part3 == ""
    )
  )

tab_final

