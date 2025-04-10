# modules/server_module.R

# Cargar las gráficas
source("modules/visualizations/grafico_eventos.R")
# source("modules/visualizations/perfil_individual.R")

server <- function(datos) {
  function(input, output, session) {
    # Reactivos compartidos
    ranges <- reactiveValues(x = NULL)
    click_info <- reactiveValues(tipo = NULL, tramo = NULL)

    # Delegar visualización principal
    crear_grafico_eventos(input, output, session, datos, ranges, click_info)
    # perfil_server("perfil", datos$pacientes)
  }
}
