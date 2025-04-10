# modules/views/grafico_eventos_ui.R

grafico_eventos_ui <- function(pacientes) {
  fluidPage(
    titlePanel("Visualización del Gráfico de Eventos"),
    sidebarLayout(
      sidebarPanel(
        selectInput("tipo_evento", "Tipo de Evento:", 
                    choices = c("Todos", "Sangrado", "Trombotico")),
        selectInput("sexo", "Género:", 
                    choices = c("Todos", unique(pacientes$Sexo))),
        sliderInput("edad", "Edad:", 
                    min = min(pacientes$Edad), max = max(pacientes$Edad), 
                    value = c(min(pacientes$Edad), max(pacientes$Edad))),
        conditionalPanel(
          condition = "input.tipo_evento == 'Sangrado'",
          checkboxGroupInput("gravedad", "Gravedad:", choices = c())
        ),
        conditionalPanel(
          condition = "input.tipo_evento == 'Trombotico'",
          checkboxGroupInput("trombotico", "Tipo de evento trombótico:", choices = c())
        )
      ),
      mainPanel(
        plotOutput("grafico_eventos", 
                   brush = brushOpts(id = "plot_brush"), 
                   dblclick = "plot_dblclick", 
                   click = "plot_click"),
        uiOutput("apartado2")
      )
    )
  )
}
