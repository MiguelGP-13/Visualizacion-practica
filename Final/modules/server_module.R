# modules/server_module.R

# Cargar las gráficas
source("modules/visualizations/grafico_eventos.R")
source("modules/visualizations/descripcion_pacientes.R")
source("modules/visualizations/correlaciones_sangrado.R")
source("modules/visualizations/correlaciones_trombotico.R")
source("modules/visualizations/comparacion_eventos.R")


server <- function(datos) {
  function(input, output, session) {
    # Reactivos compartidos
    ranges <- reactiveValues(x = NULL)
    click_info <- reactiveValues(tipo = NULL, tramo = NULL)

    # Delegar visualización principal
    crear_grafico_eventos(input, output, session, datos, ranges, click_info)
    crear_grafico_descripcion_pacientes(input, output, session, datos, ranges, click_info)
    crear_correlaciones_sangrado(input, output, session, datos)
    crear_correlaciones_trombotico(input, output, session, datos)
    crear_comparacion_eventos(input, output, session, datos)

    # Navegación con los botones de la pantalla de inicio
    observeEvent(input$go_grafico, {
      updateNavbarPage(session, "nav", selected = "Gráfico Eventos")
    })

    observeEvent(input$go_comparacion, {
      updateNavbarPage(session, "nav", selected = "Comparación Tramos")

    })

    observeEvent(input$go_descripcion_pacientes, {
      updateNavbarPage(session, "nav", selected = "Descripción pacientes")
    })
    
    observeEvent(input$go_correlaciones, {
      updateNavbarPage(session, "nav", selected = "Correlaciones")
    })
  }
}
