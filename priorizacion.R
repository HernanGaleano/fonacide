library(readr)
library(dplyr)
library(readxl)


# Hace una lista de archivos en la carpeta "csvs". El parámetro "full.names"
# añade el path a la lista
files <- list.files("csvs", full.names = TRUE)

files <- list.files("planillas_priorizacion", full.names = TRUE)
path <- "planillas_priorizacion/2015.xlsx"

fon_leerpriorizacion(path)





# Otras Ideas -------------------------------------------------------------


path <- "planillas_priorizacion/2019.xlsx"
path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)


#Verifica el nombre de la hoja y de acuerdo a eso asigna el área FONACIDE correspondiente
#Busca la celda "N° de prioridad" y guarda el número de fila/columna que se encuentra
for (i in test){
  print(i)
}


#Vuelve a leer el archivo desde la fila que se encontró en el paso anterior, asignando
#nombre de columnas


