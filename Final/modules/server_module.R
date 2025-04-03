# modules/server_module.R

# Cargar las gráficas
source("modules/visualizations/grafico_eventos.R")
source("modules/visualizations/detalle_tramo.R")

server <- function(datos) {
  function(input, output, session) {
    # Reactivos compartidos
    ranges <- reactiveValues(x = NULL)
    click_info <- reactiveValues(tipo = NULL, tramo = NULL)

    # Delegar visualización principal
    crear_grafico_eventos(input, output, session, datos, ranges, click_info)

    # Delegar detalles del tramo clicado
    crear_detalle_tramo(input, output, session, datos, click_info)
  }
}
