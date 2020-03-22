library(tidyverse)
library(lubridate)
library(ggrepel)


municipios = c("Presidente Franco", "Hernandarias", "Minga Guazú")

trans <- transferencias_fonacide %>% 
  filter(str_detect(descripcionEntidad, regex(str_c(municipios, collapse = "|"), ignore_case = TRUE))) %>%
  mutate(nombre = str_split(descripcionEntidad, " DE ")) %>% 
  mutate(nombre = map_chr(nombre, ~  .[[2]])) %>% 
  mutate(nombre = str_to_title(nombre)) %>% 
  group_by(anio, nombre) %>% 
  summarise(suma_monto = sum(montoTransferido))

#line_plot <-  ggplot() + 
  ggplot() + 
  geom_line(data = trans, aes(x = anio, y = suma_monto, color = nombre),
            #color = "grey",
            stat = "identity",
            size = 1.3,
            ) +
    geom_text_repel(data = trans %>% 
                ungroup %>% 
                filter(anio == max(anio)),
              aes(
                x = anio,
                y = suma_monto, 
                color = nombre, 
                label = paste0(" ", nombre)),
              hjust = 0,
              segment.alpha = 0,
              fontface = "bold",
              size = 4,
              vjust = "outward", nudge_x = 0.05) +
    #scale_colour_manual(values = c("Hernandarias" = "#208fce", "Minga Guazú" = "#c2b7af","Presidente Franco" = "#c2b7af")) +
    scale_colour_manual(values = c("Hernandarias" = "#208fce", rep("#c2b7af", 2))) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(3,15,3,3,"mm")
    ) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "₲", prefix = "")) +
    scale_x_continuous(expand = expansion(add = c(0,1)), breaks = seq(2012, 2020, 1)) +
    labs(x = "",
         y = "",
         title = "The Kākāpō came in a close-second to the Yellow-eyed penguin",
         subtitle = "The Kākā was also a distant fourth in total votes",
         caption = "\nPlot: @thomas_mock | Data: NZ Forest & Bird") 
    

### TEST 
  
  ggplot() + 
    geom_line(data = trans, aes(x = anio, y = suma_monto, color = nombre),
              #color = "grey",
              stat = "identity",
              size = 1.3,
    ) +
    geom_text(data = trans %>% 
                ungroup %>% 
                filter(anio == max(anio)),
              aes(
                x = anio,
                y = suma_monto, 
                color = nombre, 
                label = paste0(" ", nombre)),
              hjust = 0,
              #color = "grey",
              fontface = "bold",
              size = 4) +
    #scale_colour_manual(values = c("Hernandarias" = "#208fce", "Minga Guazú" = "#c2b7af","Presidente Franco" = "#c2b7af")) +
    scale_colour_manual(values = c("Hernandarias" = "#208fce", rep("gray", 2))) +
    theme_minimal() +
    theme(
      legend.position = "none",
    )

  
##########  
trans_hernandarias <- trans %>% filter(municipios == 'Hernandarias')

ggplot(trans_hernandarias) + geom_bar(
  aes(x = anio, y = trans),
  stat = "identity",
  position = "dodge"
) + coord_flip()

# Resumen por año ---------------------------------------------------------
# transferencias_fonacide %>% 
#   group_by(anio) %>% 
#   summarise(sum (montoTransferido))

# Filtro solo CDE ---------------------------------------------------------
# CDE <- transferencias_fonacide %>% 
#   filter(grepl(pattern="ciudad del este", tolower(descripcionEntidad)))


# print("Resumen de transferencias FONACIDE a CDE")
# CDE %>% 
#   group_by(anio) %>% 
#   summarise(sum(montoTransferido))
