# modules/views/main.R
main_ui <- function() {
  fluidPage(
    titlePanel("Bienvenido a la Aplicación Interactiva"),
    sidebarLayout(
      sidebarPanel(
        h4("Selecciona una sección para explorar:"),
        actionButton("go_descripcion_pacientes", "Detalle de los pacientes"),
        actionButton("go_grafico", "Gráfico de Eventos"),
        actionButton("go_comparacion", "Comparación Tramos"),
        actionButton("go_correlaciones", "Correlaciones")
        
      ),
      mainPanel(
        h3("¿Qué deseas hacer?"),
        p("Usa los botones del panel lateral para navegar por las secciones.")
      )
    )
  )
}
