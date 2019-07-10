library(testthat)
library(here)



context("Debe estar el archivo de la licencia del paquete")
        files <- fs::dir_ls(path = ("C:/Users/Deiby S/Desktop/Programacion avanzada R/Clase 3/solanotlc"), glob = "*.md")
      testthat::expect_true(length(files) == 1)


context("Debe haber al menos las dos funciones creadas para el paquete")
         files <- fs::dir_ls(path = ("C:/Users/Deiby S/Desktop/Programacion avanzada R/Clase 3/solanotlc/man"), glob = "*.Rd")
        testthat::expect_true(length(files) >= 2)
