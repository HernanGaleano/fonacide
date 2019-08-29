


fon_leerpriorizacion <- function(path) {


# Esto funciona -----------------------------------------------------------
hojas <- excel_sheets(path)
names(output) <- hojas
output <- vector("list",length(hojas))
for (sheet in hojas) {
  ### Leer las primeras 20 filas para detectar dónde están los datos
  encabezado <- read_excel(path =  path,
                           sheet = sheet,
                           range = cell_rows(1:20),
                           col_types = NULL,
                           .name_repair = "unique"
  )
  ### Busca donde comienza el encabezado (Prioridad) y donde comienzan los datos (1)
  inicio <- str_which(encabezado[[1]],"Prioridad|^1$") 
  
  ## Leer nombre de columnas ----------------------------------
  ### Subset del nombre de las columnas
  nomb <- encabezado[seq(inicio[[1]],inicio[[2]]-1), ] 
  ### Rellena las celdas combinadas
  m <- dim(nomb)[[1]] #numero de filas
  n <- dim(nomb)[[2]] #numero de columnas
  for (i in m:2) {
    for (j in 1:n) {
      if (!is.na(nomb[[i, j]])) {
        if (is.na(nomb[[i - 1, j]])) {
          if (is.na(nomb[[i, j - 1]])) {
            nomb[[i - 1, j]] <- nomb[[i, j]]
            nomb[[i, j]] <- NA#nomb[[i, j - 1]] #llenar con NA
          } else{
            nomb[[i - 1, j]] <- nomb[[i - 1, j - 1]]
          }
        }
      }
    }
  }
  ### Concatenación del encabezado para nombres de columnas
  nombres <- map_chr(nomb, ~ na.omit(.) %>% str_c(collapse = " - "))
  
  #Verificar nombres duplicados, concatenar con el de la izquierda
  duplicados <- duplicated(nombres)
  nombres <- map_chr(seq_along(nombres), ~ ifelse(duplicados[[.]], str_c(nombres[[.-1]], nombres[[.]], sep = " - "),nombres[[.]]))
  nombres <- enc2native(nombres)
  
  ## Leer los datos sin el encabezado ----------------------------------
  planilla <- read_excel(
    path =  path,
    sheet = sheet,
    skip = inicio[[2]],
    trim_ws = TRUE,
    col_names = FALSE,
    col_types = NULL,
    .name_repair = "minimal"#~ sample(letters, 1, replace = FALSE)
  )
  #Se descartan posibles columnas sin datos
  planilla <- planilla %>% select_if(~ is.na(.) %>% mean < 0.9) 
  #Se descartan posibles filas sin datos, como explicaciones al final de la planilla
  planilla <- planilla %>% filter(str_detect(.[[1]],"^\\d+$"))  
  
  ## Nombrar correctamente las columnas ----------------------------------
  pla_nomb <- planilla %>% rename_all(~ nombres)
  output[[sheet]] <- pla_nomb
}

## Nombrar correctamente la lista con el area FONACIDE correspondiente ----------
areas_fonacide <- str_replace_all(hojas, 
                                  c(".*ula.*" = "Aulas",
                                    ".*anitari.*" = "Sanitario", 
                                    ".*spacio.*" = "Otros Espacios", 
                                    ".*quipam.*" = "Equipamientos", 
                                    ".*obiliario.*" = "Equipamientos", 
                                    ".*limentac.*|.*scola.*" = "Alimentación escolar"))
names(output) <- areas_fonacide

return(output)
}


#Falta ver como cada hojas toma los valores NA (- capaz)