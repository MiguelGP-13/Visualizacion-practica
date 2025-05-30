# app.R
library(shiny)
library(shinythemes)

library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readxl)


# Cargar módulos
source("modules/data_processing.R")
source("modules/ui_module.R")
source("modules/server_module.R")

# Preparar los datos
datos <- cargar_datos()  # Función del módulo de procesamiento de datos
View(datos$eventos_totales_completos)

# Ejecutar la app
shinyApp(ui = ui(datos), server = server(datos))

