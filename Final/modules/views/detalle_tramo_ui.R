detalle_tramo_ui <- function(pacientes) {
  fluidPage(
    titlePanel("Análisis de Enfermedades vs. Eventos"),
    sidebarLayout(
      sidebarPanel(
        sliderInput("edad_detalle", "Rango de edad", min = 30, max = 90, value = c(50, 80)),
        selectInput("sexo_detalle", "Sexo", choices = c("Todos", "Hombre", "Mujer")),
        checkboxGroupInput("enfermedades_detalle", "Selecciona enfermedades a cruzar",
                           choices = c("HTA", "DM", "EPOC", "Valvulopatía", "Insuficiencia cardíaca")), # las que tengas
        actionButton("actualizar_detalle", "Actualizar visualización")
      ),
      mainPanel(
        plotlyOutput("heatmap_tramo"),
        DTOutput("tabla_detalle_eventos")
      )
    )
  )
}
