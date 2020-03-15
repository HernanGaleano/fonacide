library(readr)
library(dplyr)

# Hace una lista de archivos en la carpeta "data-raw/transferencias_fonacide".
# El parámetro "full.names" añade el path a la lista

files <- list.files("data-raw/transferencias_fonacide", full.names = TRUE)


# No se usa (la idea era imprimir mensajes sobre los años leidos) ----------------
# Expresión regular para extrar el año y mes del nombre de los archivos
reg_exp <- regmatches(files, regexec("([0-9]+)-([0-9]+).csv", files))
#Crea el tible con años y meses procesados
year_month <-
  setNames(do.call(rbind.data.frame, reg_exp), c("", "Año", "Mes"))[2:3]

cantidad <- length(files)
cat("Se encontraron", cantidad, "archivos.\n")

# Leer CSVs ---------------------------------------------------------------
#files <- dir("transferencias_fonacide/", pattern = "\\.csv$", full.names = TRUE)
output <- vector("list", length(files))
for (i in seq_along(files)) {
  # Se utiliza el enconding ISO para evitar problemas con los acentos y la "ñ"
  output[[i]] <- readr::read_csv(files[[i]],
                          locale = locale(encoding = 'ISO-8859-1'),
                          col_types = cols())
}
transferencias_fonacide <- bind_rows(output)

#Se busca la máxima fecha de corte por año y mes
max_fecha_corte <- transferencias_fonacide %>%
  group_by(anio, mes) %>%
  summarise(maxfecha = max(fechaCorte))

#Se dejan sólo las filas que correspondan a la máxima fecha encontrada en el paso anterior
transferencias_fonacide <-
  inner_join(transferencias_fonacide,
             max_fecha_corte,
             by = c("anio" = "anio", "mes" = "mes", "fechaCorte" = "maxfecha")
  )

#Se borran entradas duplicadas
transferencias_fonacide <- transferencias_fonacide %>% 
  distinct()

#write_csv(CDE,"transferencias_fonacide.csv")

usethis::use_data(transferencias_fonacide, name = transferencias_fonacide)


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