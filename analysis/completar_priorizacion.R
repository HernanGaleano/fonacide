#Completar con priorización la planilla de contratos de Franco
# Steps
# Estirar la planilla
# Leer la hoja completo
# Ejcutar un map(año, cod_estb, cod_inst)
# Una opción es hacer algo parecido a lo de python, cada resultado de la función
# es un diccionario (en este caso list), y despues se hace un bind y se tiene un
# tibble con todos los restultados

library(tidyverse)
library(googlesheets)

#URL de planila resumen completo contratos CDE
#https://docs.google.com/spreadsheets/d/1rBEXwOoUDyHVOXC_ZWm9jslJlFXPbaGSGtna21hoLxA/edit#gid=0

key <- "1rBEXwOoUDyHVOXC_ZWm9jslJlFXPbaGSGtna21hoLxA" #Key de la planilla
sheet <- "completo" #Nombre la hoja con lista de instituciones

#Nombre las columnas en la planilla
fecha <- "Fecha de contrato"
codigo_establecimiento <- "Código de establecimiento"
codigo_institucion <- "Código de institución"
nombre_institucion <- "Institución matriculaciones"


gd_completo <- gs_key(key)
contratos_franco <-  gs_read(gd_completo, ws = sheet)

escuelas <- contratos_franco %>%
  mutate(año = stringr::str_extract(get(fecha), "\\d{4}$")) %>% #Capaz cambiar el "get" o ver si hay algo equivalente en tidyeval
  select(
        año,
        !! codigo_establecimiento,
        !! codigo_institucion,
        !! nombre_institucion
        )

a <- purrr::pmap(escuelas, ~ get_prio(..1, ..2, ..3))
output <- purrr::transpose(a) %>% as_tibble(.name_repair = "minimal") %>% unnest()

#Celda a escribir
columna <- LETTERS[[ncol(contratos_franco) + 1]] #Se escribe en la siguiente columna a la última
celda <- paste0(columna, 1)

gs_edit_cells(gd_completo, ws = sheet, input = output, anchor = celda)

#Interesante
#priorizacion_Franco %>% purrr::map(list("Aulas", "Nombre de la Institución", 2))

