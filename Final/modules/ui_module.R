# modules/ui_module.R

# Cargar vistas
source("modules/views/main.R")
source("modules/views/grafico_eventos_ui.R")
source("modules/views/sankey_terapias_ui.R")
source("modules/views/bubble_diagnosticos_ui.R")
# source("modules/views/perfil_individual_ui.R")

# Definir la UI principal
ui <- function(pacientes) {
  navbarPage(
    title = "Aplicación Interactiva",
    theme = shinytheme("cerulean"),
    tabPanel("Inicio", main_ui()),                      # Ventana de inicio
    tabPanel("Gráfico Eventos", grafico_eventos_ui(pacientes)), # Ventana del gráfico eventos
    # tabPanel("Perfil Individual", perfil_ui("perfil")), # Reporte de un paciente
  )
}

