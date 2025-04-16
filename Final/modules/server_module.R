# modules/server_module.R

# Cargar las gráficas
source("modules/visualizations/grafico_eventos.R")
source("modules/visualizations/descripcion_pacientes.R")

server <- function(datos) {
  function(input, output, session) {
    # Reactivos compartidos
    ranges <- reactiveValues(x = NULL)
    click_info <- reactiveValues(tipo = NULL, tramo = NULL)

    # Delegar visualización principal
    crear_grafico_eventos(input, output, session, datos, ranges, click_info)
    crear_grafico_descripcion_pacientes(input, output, session, datos, ranges, click_info) 
  }
}
