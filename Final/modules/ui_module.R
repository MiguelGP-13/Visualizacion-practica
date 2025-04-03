# modules/ui_module.R

# Cargar vistas
source("modules/views/main.R")
source("modules/views/grafico_eventos_ui.R")
source("modules/views/detalle_tramo_ui.R")

# Definir la UI principal
ui <- function(pacientes) {
  navbarPage(
    title = "Aplicación Interactiva",
    tabPanel("Inicio", main_ui()),                      # Ventana de inicio
    tabPanel("Gráfico Eventos", grafico_eventos_ui(pacientes)), # Ventana del gráfico principal
    tabPanel("Detalle Tramo", detalle_tramo_ui()),      # Ventana para detalles del tramo
  )
}

