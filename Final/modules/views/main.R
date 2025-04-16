# modules/views/main.R

main_ui <- function() {
  fluidPage(
    titlePanel("Bienvenido a la Aplicación Interactiva"),
    sidebarLayout(
      sidebarPanel(
        h4("Selecciona una sección para explorar:"),
        actionButton("go_grafico", "Gráfico de Eventos"),
        actionButton("go_detalle", "Detalle del Tramo"),
        actionButton("go_nueva_visualizacion", "Detalle de los pacientes")
      ),
      mainPanel(
        h3("¿Qué deseas hacer?"),
        p("Usa los botones del panel lateral para navegar por las secciones.")
      )
    )
  )
}
