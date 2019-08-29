
library(tidyverse)
library(googlesheets)

#URL de planila resumen completo contratos CDE
#https://docs.google.com/spreadsheets/d/1cq8BFwRsGN6IcQpzZ_SUgZRICkLLq_CZVYS2IMscwKo/edit#gid=0

gd_completo <- gs_key("1cq8BFwRsGN6IcQpzZ_SUgZRICkLLq_CZVYS2IMscwKo")

contratos <-  gs_read(gd_completo, ws = "Completo")

#Guardé 
#saveRDS(contratos, file = "contratos_20190720.rds")
#contratos <- readRDS("contratos_20190720.rds")

#Primero tengo que filtrar los que tengan en area FONACIDE=Alimentacion, pavimentos y 
#los que no tengan(casa de la cultura, etc)
#Se van a quedar Aulas, Sanitarios, Otros Espacios, Mobilarios

areas_filtrar <-  c("Alimentación Escolar", "Pavimentos",NA)
new_contratos <-  contratos %>% 
  filter(!`Area de FONACIDE 1` %in% c(areas_filtrar))
  

##Cambiar los NA en areas de fonacide por "no tiene"
new_contratos$`Area de FONACIDE 2`[is.na(new_contratos$`Area de FONACIDE 2`)] <- 'No tiene'
new_contratos$`Area de FONACIDE 3`[is.na(new_contratos$`Area de FONACIDE 3`)] <- 'No tiene'


#Estoy pensando en crear un columna que sea algo como "merecia la obra" TRuE/FALSE
#Si la institución estaba en el top 10 en el area que se hizo la obra va a ser TRUE
#Condiciones. Si TRUE NO es mimada

mimadas <- new_contratos %>% 
  mutate(merecian =
    ((`Area de FONACIDE 1` == "Aulas" |
        `Area de FONACIDE 2` == "Aulas" | 
        `Area de FONACIDE 3` == "Aulas") & Aulas > 0  & Aulas <= 10) |
      ((`Area de FONACIDE 1` == "Sanitarios" |
          `Area de FONACIDE 2` == "Sanitarios" | 
          `Area de FONACIDE 3` == "Sanitarios") & Sanitarios > 0  & Sanitarios <= 10) |
      ((`Area de FONACIDE 1` == "Otros Espacios" |
          `Area de FONACIDE 2` == "Otros Espacios" | 
          `Area de FONACIDE 3` == "Otros Espacios") & `Otros Espacios` > 0  &  `Otros Espacios` <= 10) |
      ((`Area de FONACIDE 1` == "Equipamientos" |
          `Area de FONACIDE 2` == "Equipamientos" | 
          `Area de FONACIDE 3` == "Equipamientos") & Equipamientos > 0  & Equipamientos <= 10)
  )


mimadas %>% 
  group_by(`Código de establecimiento`,`Institución beneficiada (contratos)`) %>% 
  count(`Código de establecimiento`) %>% 
  arrange(desc(n))

mimadas %>% 
       group_by(`Código de establecimiento`) %>% 
       count() %>% 
       arrange(desc(n))

mimadas %>% 
  +     group_by(`Código de establecimiento`,`Institución beneficiada (contratos)`) %>% 
  +     count(`Código de establecimiento`) %>% 
  +     arrange(desc(n))

#########IDEAS------------------------------

#Crear una función que agarre los valores en las columnas "Area de FONACIDE 1,2,3"
# y tire en un vector/lista. Después que utilice cada item como id de columna
# y vea si en esas columnas el registro está entre 1 y 10, y que en ese caso
# devuelva valor TRUE
# Entonces 
