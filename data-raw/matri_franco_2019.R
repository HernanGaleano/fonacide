#Descarga y lee los datos de matriculaciones de Franco

#link: https://docs.google.com/spreadsheets/d/1P7uqCdtT0fPZSo5T9KWEvts60PIaZd9yGn9jJYfoO_0

key <- '1P7uqCdtT0fPZSo5T9KWEvts60PIaZd9yGn9jJYfoO_0'
matri_franco_2019 <-  leer_planilla_gs(key, "Cód. Establecimiento Escolar", "1002039")
matri_franco_2019 <- matri_franco_2019[[1]] #Agarrar el tible de la lista

usethis::use_data(matri_franco_2019, name = matri_franco_2019, overwrite = TRUE)




