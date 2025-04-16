# modules/ui_module.R

# Cargar vistas
source("modules/views/main.R")
source("modules/views/grafico_eventos_ui.R")
source("modules/views/descripcion_pacientes_ui.R")

# Definir la UI principal
ui <- function(pacientes) {
  navbarPage(
    title = "Aplicación Interactiva",
    # theme = shinytheme("cerulean"),
    tabPanel("Inicio", main_ui()),                      # Ventana de inicio
    tabPanel("Descripción pacientes", grafico_descripcion_pacientes_ui(pacientes)), # Resumen de pacientes
    tabPanel("Gráfico Eventos", grafico_eventos_ui(pacientes)), # Ventana del gráfico eventos
  )
}
