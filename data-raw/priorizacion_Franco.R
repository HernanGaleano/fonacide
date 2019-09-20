
municipio <- "Presidente Franco"
folder_path <- paste0("data-raw/planillas_priorizacion/", municipio)
files <- list.files(folder_path, full.names = TRUE)

#Nombres(años) para la lista
anhos <- sub(pattern = "\\..*$", replacement = "", basename(files))

priorizacion_Franco <- purrr::map(files, ~ leer_planilla(.))
priorizacion_Franco <- purrr::set_names(priorizacion_Franco, anhos)

usethis::use_data(priorizacion_Franco, name = priorizacion_Franco, overwrite = TRUE)