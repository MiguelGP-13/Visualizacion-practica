# modules/server_module.R

# Cargar las gráficas
source("modules/visualizations/grafico_eventos.R")
source("modules/visualizations/descripcion_pacientes.R")
source("modules/visualizations/detalle_tramo.R")

server <- function(datos) {
  function(input, output, session) {
    # Reactivos compartidos
    ranges <- reactiveValues(x = NULL)
    click_info <- reactiveValues(tipo = NULL, tramo = NULL)

    # Delegar visualización principal
    crear_grafico_eventos(input, output, session, datos, ranges, click_info)
    crear_grafico_descripcion_pacientes(input, output, session, datos, ranges, click_info)

    # Navegación con los botones de la pantalla de inicio
    observeEvent(input$go_grafico, {
      updateNavbarPage(session, "nav", selected = "Gráfico Eventos")
    })

    observeEvent(input$go_detalle, {
      updateNavbarPage(session, "nav", selected = "Detalle del Tramo")
    })

    observeEvent(input$go_nueva_visualizacion, {
      updateNavbarPage(session, "nav", selected = "Descripción pacientes")
    })
  }
}
