install.packages("xlsx")
install.packages("openxlsx")

ggplot(aes(x = as.Date(Fecha), y = Aportación_trimestral), data = pib_D_vol) +
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
  scale_y_continuous(limits = c(-5, 12), labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(limits = as.Date(c("2015-01-01", max(pib_D_vol$date))), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_brewer(palette = "Set1") +
  guides(fill = guide_legend(title = NULL))


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


ordre_variables <- c("Producto interior bruto a precios de mercado","Gasto en consumo final de los hogares",  "Gasto en consumo final de las AAPP","Formación bruta de capital fijo (FBCF)","Exportaciones de bienes y servicios","Importaciones de bienes y servicios")

tab <- pib_D_vol %>%
  filter(Part3 %in% ordre_variables) %>%
  mutate(Part3 = factor(Part3, levels = ordre_variables)) %>%  # Assurer l'ordre des lignes
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3) %>%  # Trier selon l'ordre défini
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  )

tab

tab2 <- pib_D_vol %>%
  filter(Part3 %in% ordre_variables) %>%
  filter(Fecha >= "2022-01-01")%>%
  mutate(Part3 = factor(Part3, levels = ordre_variables),
         Variación_trimestral = round(Variación_trimestral, 1) # Arrondi à 1 décimale
  ) %>%  # Assurer l'ordre des lignes
  select(Fecha, Part3, Variación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Variación_trimestral) %>%
  arrange(Part3) %>%  # Trier selon l'ordre défini
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "")
tab2



tab3 <- pib_D_vol %>%
  filter(Part3 %in% c("Demanda nacional", "Variación de existencias y adquisiciones menos cesiones de objetos valiosos")) %>%
  filter(Fecha >= "2022-01-01") %>%
  mutate(
    Aportación_trimestral = round(Aportación_trimestral, 1), # Arrondi à 1 décimale
    Part3 = recode(Part3, "Variación de existencias y adquisiciones menos cesiones de objetos valiosos" = "Stock")  # Renommer la valeur "Variación de existencias..." en "Stock"
  ) %>%
  select(Fecha, Part3, Aportación_trimestral) %>%
  pivot_wider(names_from = Fecha, values_from = Aportación_trimestral) %>%
  arrange(Part3) %>%  # Trier selon l'ordre défini
  gt() %>%
  tab_options(
    table.font.size = "small",
    quarto.disable_processing = TRUE,
    table.width = "85%"
  ) %>%
  cols_label(Part3 = "Contribution trimestrielle")  # Enlever le nom de la colonne Part3

  
tab3

