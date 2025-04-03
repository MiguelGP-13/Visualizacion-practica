# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# Cargar módulos
source("modules/data_processing.R")
source("modules/ui_module.R")
source("modules/server_module.R")

# Preparar los datos
datos <- cargar_datos()  # Función del módulo de procesamiento de datos

# Ejecutar la app
shinyApp(ui = ui(datos$pacientes), server = server(datos))

