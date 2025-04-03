# modules/views/detalle_tramo_ui.R

detalle_tramo_ui <- function() {
  fluidPage(
    titlePanel("Detalles del Tramo Seleccionado"),
    fluidRow(
      h3("Información específica del tramo"),
      plotOutput("detalle_tramo")
    )
  )
}
