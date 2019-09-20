#library(tidyverse)
#library(devtools)



 #AHora hacer el main que estira el google docs y escribe el nombre y los codigos
 
 #path matriculaciones
 #matriculaciones_2018.xlsx
 # path <- paste(getwd(), "/data/matriculaciones_2018.xlsx", sep = "")
 # matriculaciones <- leer_planilla(path, "stablecimient", "0000001")
 # matriculaciones <- matriculaciones[[1]]
 # 
 # matriAlto <- matriculaciones %>%
 #   dplyr::filter(stringr::str_detect(Departamento, "Alto")) %>%
 #   mutate(nombre_institucion = stri_trans_general(`Nombre de la Institución`, "Latin-ASCII")) 
 # 
 # contratos <- readRDS("data/contratos_20190720.rds")
 # 
 # matriCDE <- matriculaciones %>% 
 #   dplyr::filter(stringr::str_detect(Distrito, "Ciudad"))
