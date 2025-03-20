library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)

# Cargar datos
pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)
eventos_sangrado <- read_excel("data/eventos.xlsx", sheet = "sangrado")
eventos_tromboticos <- read_excel("data/eventos.xlsx", sheet = "trombotico")

# Agregar edad y sexo a las tablas de eventos
eventos_sangrado <- eventos_sangrado %>%
  inner_join(pacientes, by = "Paciente") %>%
  select(Paciente, Edad, Sexo, `Tipo de sangrado`, `Gravedad de la hemorragia (TIMI)`, `Gravedad de la hemorragia (GUSTO)`, `Gravedad de la hemorragia (BARC)`, `Procedimientos terapéuticos`, `Descenso de hemoglobina`, `¿El paciente ha subido una trasfusión?`)

eventos_tromboticos <- eventos_tromboticos %>%
  inner_join(pacientes, by = "Paciente") %>%
  select(Paciente, Edad, Sexo, `¿El paciente ha sufrido un evento trombótico previo a la inclusión?`, `Tipo de evento trombótico`, `Tipo de invervención`, `TYPE_THROMBOTIC_PRE`, `Numero  anticoagulantes`, `Numero  antiagregantes`, `Otro medicamentos`)

# Unir ambas tablas en una sola
eventos_totales <- bind_rows(
  eventos_sangrado %>% mutate(Evento = "Sangrado"),
  eventos_tromboticos %>% mutate(Evento = "Trombótico")
)

# UI
ui <- fluidPage(
  titlePanel("Visualización Interactiva de Eventos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_evento", "Tipo de Evento:", choices = c("Todos", "Sangrado", "Trombótico")),
      selectInput("sexo", "Género:", choices = c("Todos", unique(pacientes$Sexo))),
      sliderInput("edad", "Edad:", min = min(pacientes$Edad), max = max(pacientes$Edad), value = c(min(pacientes$Edad), max(pacientes$Edad))),
      checkboxInput("filtro_gravedad", "Filtrar por gravedad (sólo para sangrado)", FALSE)
    ),
    mainPanel(
      plotlyOutput("grafico_eventos")
    )
  )
)

# Server
server <- function(input, output) {
  output$grafico_eventos <- renderPlotly({
    datos_filtrados <- eventos_totales %>%
      filter((input$tipo_evento == "Todos" | Evento == input$tipo_evento) &
               (input$sexo == "Todos" | Sexo == input$sexo) &
               (Edad >= input$edad[1] & Edad <= input$edad[2]))
    
    if (input$filtro_gravedad & input$tipo_evento == "Sangrado") {
      datos_filtrados <- datos_filtrados %>%
        filter(!is.na(`Gravedad de la hemorragia (TIMI)`))
    }
    
    p <- ggplot(datos_filtrados, aes(x = Edad, fill = Evento)) +
      geom_histogram(binwidth = 5, position = "dodge") +
      labs(title = "Distribución de Eventos por Edad", x = "Edad", y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

