library(readr)
library(dplyr)

warn=1
# Hace una lista de archivos en la carpeta "csvs". El parámetro "full.names"
# añade el path a la lista

files <- list.files("csvs", full.names = TRUE)

# Expresión regular para extrar el año y mes del nombre de los archivos
reg_exp <- regmatches(files, regexec("([0-9]+)-([0-9]+).csv", files))

#Crea el tible con años y meses procesados
year_month <-
  setNames(do.call(rbind.data.frame, reg_exp), c("", "Año", "Mes"))[2:3]

#Se crea el tibble "completo" leyendo el primer archivo
completo <-
  read_csv(files[1],
           locale = locale(encoding = 'ISO-8859-1'),
           col_types = cols())

#Capaz sea mejor crear un tible vacío con la cantidad de columnas 


#########
#Para mejorar lo que está abajo:
#Usar map/purrr
#O al leer la primera vez para verificar los encabezados, agregar ya la cantidad
#de columnas al tibble "completo"

################# Como mejorar
# You might be generating a big data frame. Instead of sequentially rbind()ing in each iteration, save the output in a list, then use dplyr::bind_rows(output) to combine the output into a single data frame.


#Bucle para leer todos la lista de archivos en "files". Se utiliza el enconding ISO para evitar
#problemas con los acentos y la "ñ"
for (i in files) {
  csv <-
    read_csv(i,
             locale = locale(encoding = 'ISO-8859-1'),
             col_types = cols())
  #Se verifica que los encabezados sea iguales. En caso verdadero se unen
  if (identical(names(csv), names(completo))) {
    completo <- bind_rows(csv, completo)
  } else {
    cat("El siguiente archivo tiene encabezado diferente.\n",i)
    stop("Verificar CSV")
  }
}

######Mejorado
files <- dir("csvs/", pattern = "\\.csv$", full.names = TRUE)
#names(output) <- files #No ayuda porque despues se hace bind
output <- vector("list", length(files))
for (i in seq_along(files)) {
  output[[i]] <- read_csv(files[[i]],
                          locale = locale(encoding = 'ISO-8859-1'),
                          col_types = cols())
}
completo <- bind_rows(output)

#Se busca la máxima fecha de corte por año y mes
MaxFechaCorte <- completo %>%
  group_by(anio, mes) %>%
  summarise(maxfecha = max(fechaCorte))

#Se dejan sólo las filas que correspondan a la máxima fecha encontrada en el paso anterior
completo <-
  inner_join(completo,
             MaxFechaCorte,
             by = c("anio" = "anio", "mes" = "mes", "fechaCorte" = "maxfecha")
  )

#Se borran entradas duplicadas
completo <- completo %>% 
  distinct()


completo %>% 
  group_by(anio) %>% 
  summarise(sum (montoTransferido))

CDE <- completo %>% 
  filter(grepl(pattern="ciudad del este", tolower(descripcionEntidad)))

write_csv(CDE,"completo.csv")
write_csv(CDE,"TransferenciasCDE.csv")

print("Resumen de transferencias FONACIDE a CDE")
CDE %>% 
  group_by(anio) %>% 
  summarise(sum(montoTransferido))