
library(tidyverse)
#library(googlesheets)

#URL de planila resumen completo contratos CDE
#https://docs.google.com/spreadsheets/d/1cq8BFwRsGN6IcQpzZ_SUgZRICkLLq_CZVYS2IMscwKo/edit#gid=0

# Hernandarias
# https://docs.google.com/spreadsheets/d/1OYdjA4XrHg2BhWMtBkH0DUCvpsOgd_T8OhmkBK4cfGE/edit#gid=1435881268


# ggsheets4 ---------------------------------------------------------------

url_contratos_hernandarias <- "1OYdjA4XrHg2BhWMtBkH0DUCvpsOgd_T8OhmkBK4cfGE"

contratos <- googlesheets4::sheets_read(
  url_contratos_hernandarias, 
  sheet = "Completo", 
  )

# Nombres de columnas a utilizar
area_fonacide_1 <- "Area de FONACIDE 1" %>% rlang::sym()
area_fonacide_2 <- "Area de FONACIDE 2" %>% rlang::sym()
area_fonacide_3 <- "Area de FONACIDE 3" %>% rlang::sym()

priorizacion_aulas <- "Aulas...23"
priorizacion_sanitarios <- "Sanitarios...24"
priorizacion_equipamientos <- "Equipamientos...25"
priorizacion_otros_espacios <- "Otros Espacios...26"

# Primero tengo que filtrar los que tengan en area FONACIDE=Alimentacion, pavimentos y 
# los que no tengan(casa de la cultura, etc)
# Se van a quedar Aulas, Sanitarios, Otros Espacios, Mobilarios

areas_excluidas <-  c("Alimentación Escolar", "Pavimentos", NA)

new_contratos <-  contratos %>% 
  filter(!(!!rlang::sym(area_fonacide_1)) %in% c(areas_excluidas))

# New Tests ---------------------------------------------------------------

test_contratos <- contratos %>% select(priorizacion_aulas, area_fonacide_1, area_fonacide_2) %>% slice(1:10)
test_contratos %>% filter_at(vars(contains("Area")), any_vars(. == "Aulas"))


##Cambiar los NA en areas de fonacide por "no tiene"  #### No recuerdo por qué o para qué
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
