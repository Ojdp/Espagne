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