install.packages("tesseract")

library(tesseract)

pngfile <- pdftools::pdf_convert('pdf/contrato.pdf', dpi = 600)
