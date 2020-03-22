files <- dir(
  path = here::here("data-raw", "transferencias_fonacide"),
  pattern = "*.csv",
  full.names = TRUE
)

transferencias_fonacide <- files %>%
  purrr::map_df(~ readr::read_csv(
    file = .x,
    locale = readr::locale(encoding = "ISO-8859-1"),
    col_types = readr::cols()
  ))

cantidad <- length(files)
cat("Se encontraron", cantidad, "archivos.\n")

# Se busca la máxima fecha de corte por año y mes
max_fecha_corte <- transferencias_fonacide %>%
  dplyr::group_by(anio, mes) %>%
  dplyr::summarise(maxfecha = max(fechaCorte))

# Se dejan sólo las filas que correspondan a la máxima fecha encontrada en el paso anterior
transferencias_fonacide <-
  dplyr::inner_join(transferencias_fonacide,
                    max_fecha_corte,
                    by = c("anio" = "anio", "mes" = "mes", "fechaCorte" = "maxfecha")
                    )

# Se borran entradas duplicadas
transferencias_fonacide <- transferencias_fonacide %>% 
  dplyr::distinct()

usethis::use_data(transferencias_fonacide,
                  name = transferencias_fonacide,
                  overwrite = TRUE
                  )

# No se usa (la idea era imprimir mensajes sobre los años leidos) ----------------
# Expresión regular para extrar el año y mes del nombre de los archivos
#reg_exp <- regmatches(files, regexec("([0-9]+)-([0-9]+).csv", files))


#Crea el tible con años y meses procesados
#year_month <-
#  stats::setNames(do.call(rbind.data.frame, reg_exp), c("", "Año", "Mes"))[2:3]


