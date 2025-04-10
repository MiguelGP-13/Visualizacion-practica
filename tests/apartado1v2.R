library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# Cargar datos
pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)
eventos_sangrado <- read_excel("data/eventos.xlsx", sheet = "sangrado")
eventos_tromboticos <- read_excel("data/eventos.xlsx", sheet = "trombotico")

# Agregar edad y sexo a las tablas de eventos
eventos_sangrado <- eventos_sangrado %>%
  inner_join(pacientes, by = "Paciente") %>%
  select(Paciente, Edad, Sexo, `Tipo de sangrado`, 
         `Gravedad de la hemorragia (TIMI)`, `Gravedad de la hemorragia (GUSTO)`, 
         `Gravedad de la hemorragia (BARC)`, `Procedimientos terapéuticos`, 
         `Descenso de hemoglobina`, `¿El paciente ha subido una trasfusión?`)

eventos_tromboticos <- eventos_tromboticos %>%
  inner_join(pacientes, by = "Paciente") %>%
  select(Paciente, Edad, Sexo, `¿El paciente ha sufrido un evento trombótico previo a la inclusión?`, 
         `Tipo de evento trombótico`, `Tipo de invervención`, `TYPE_THROMBOTIC_PRE`, 
         `Numero  anticoagulantes`, `Numero  antiagregantes`, `Otro medicamentos`)

# Unir ambas tablas en una sola
eventos_totales <- bind_rows(
  eventos_sangrado %>% mutate(Evento = "Sangrado"),
  eventos_tromboticos %>% mutate(Evento = "Trombotico")
)

# UI
ui <- fluidPage(
  titlePanel("Visualización Interactiva de Eventos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tipo_evento", "Tipo de Evento:", 
                  choices = c("Todos", "Sangrado", "Trombotico")),
      selectInput("sexo", "Género:", 
                  choices = c("Todos", unique(pacientes$Sexo))),
      sliderInput("edad", "Edad:", 
                  min = min(pacientes$Edad), max = max(pacientes$Edad), 
                  value = c(min(pacientes$Edad), max(pacientes$Edad))),
      # Mostrar el control de "Gravedad" solo si se selecciona "Sangrado"
      conditionalPanel(
        condition = "input.tipo_evento == 'Sangrado'",
        checkboxGroupInput("gravedad", "Gravedad:", choices = c())
      ),
      # Mostrar el control para "Tipo de evento trombótico" solo si se selecciona "Trombotico"
      conditionalPanel(
        condition = "input.tipo_evento == 'Trombotico'",
        checkboxGroupInput("trombotico", "Tipo de evento trombótico:", choices = c())
      )
    ),
    mainPanel(
      # Se habilita el brush para hacer zoom y doble clic para reiniciar el zoom
      plotOutput("grafico_eventos", brush = brushOpts(id = "plot_brush"), dblclick = "plot_dblclick")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value para almacenar los límites del eje x
  ranges <- reactiveValues(x = NULL)
  
  # Actualizar los checkboxes cuando cambia el tipo de evento
  observeEvent(input$tipo_evento, {
    if (input$tipo_evento == "Sangrado") {
      niveles_gravedad <- unique(na.omit(eventos_sangrado$`Gravedad de la hemorragia (TIMI)`))
      niveles_gravedad <- as.character(niveles_gravedad)
      updateCheckboxGroupInput(session, "gravedad", 
                               choices = niveles_gravedad, 
                               selected = c())
      # Limpiar el otro input
      updateCheckboxGroupInput(session, "trombotico", choices = c(), selected = c())
    } else if (input$tipo_evento == "Trombotico") {
      niveles_trombotico <- unique(na.omit(eventos_tromboticos$`Tipo de evento trombótico`))
      niveles_trombotico <- as.character(niveles_trombotico)
      updateCheckboxGroupInput(session, "trombotico", 
                               choices = niveles_trombotico, 
                               selected = c())
      # Limpiar el input de gravedad
      updateCheckboxGroupInput(session, "gravedad", choices = c(), selected = c())
    } else {  # Si se selecciona "Todos"
      updateCheckboxGroupInput(session, "gravedad", choices = c(), selected = c())
      updateCheckboxGroupInput(session, "trombotico", choices = c(), selected = c())
    }
    # Reiniciar el zoom al cambiar el tipo de evento
    ranges$x <- NULL
  })
  
  # Actualizar el zoom según el brush
  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
    }
  })
  
  # Reiniciar el zoom al hacer doble clic
  observeEvent(input$plot_dblclick, {
    ranges$x <- NULL
  })
  
  output$grafico_eventos <- renderPlot({
    # Filtrar según el tipo de evento, género y edad
    datos_filtrados <- eventos_totales %>%
      filter((input$tipo_evento == "Todos" | Evento == input$tipo_evento) &
               (input$sexo == "Todos" | Sexo == input$sexo) &
               (Edad >= input$edad[1] & Edad <= input$edad[2]))
    
    # Filtrar adicionalmente según la selección de gravedad o trombótico
    if (input$tipo_evento == "Sangrado" && !is.null(input$gravedad) && length(input$gravedad) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Gravedad de la hemorragia (TIMI)` %in% input$gravedad)
    }
    if (input$tipo_evento == "Trombotico" && !is.null(input$trombotico) && length(input$trombotico) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Tipo de evento trombótico` %in% input$trombotico)
    }
    
    # Crear la gráfica con cortes fijos de 5 en 5 entre 48 y 98
    p <- ggplot(datos_filtrados, aes(x = Edad, fill = Evento)) +
      geom_histogram(breaks = seq(48, 98, by = 5), position = "dodge", color = "black") +
      labs(title = "Distribución de Eventos por Edad", x = "Edad", y = "Frecuencia") +
      theme_minimal()
    
    # Aplicar zoom en el eje x si se definieron límites mediante brush
    if (!is.null(ranges$x)) {
      p <- p + xlim(ranges$x)
    }
    p
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

