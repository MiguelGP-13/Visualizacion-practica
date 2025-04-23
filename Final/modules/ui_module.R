# modules/ui_module.R

# Cargar vistas
source("modules/views/main.R")
source("modules/views/grafico_eventos_ui.R")
source("modules/views/descripcion_pacientes_ui.R")
source("modules/views/detalle_tramo_ui.R")

# Definir la UI principal
ui <- function(pacientes) {
  navbarPage(
    title = "Aplicación Interactiva",
    id = "nav",  #ID
    theme = shinytheme("cerulean"),
    tabPanel("Inicio", main_ui()),
    tabPanel("Descripción pacientes", grafico_descripcion_pacientes_ui(pacientes)),
    tabPanel("Gráfico Eventos", grafico_eventos_ui(pacientes)),
    tabPanel("Detalle del Tramo", detalle_tramo_ui(pacientes))
  )
  
}
