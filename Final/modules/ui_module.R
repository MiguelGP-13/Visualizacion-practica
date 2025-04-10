# modules/views/main.R

library(shiny)
library(bslib)
library(shinyjs)

main_ui <- function() {
  fluidPage(
    useShinyjs(), # Habilitar shinyjs para la manipulación de la interfaz
    
    titlePanel(
      h2("Bienvenido a la Aplicación Interactiva", align = "center"),
      windowTitle = "Aplicación de Análisis de Datos"
    ),
    
    # Barra lateral con opciones para elegir entre "Pacientes" o "Eventos"
    sidebarLayout(
      sidebarPanel(
        h4("Selecciona una sección para explorar:", align = "center"),
        
        # Selector para elegir entre "Pacientes" o "Eventos"
        selectInput("tipo_analisis", "Elige el tipo de análisis:",
                    choices = c("Pacientes", "Eventos"), selected = "Pacientes"),
        
        # Botones con íconos
        actionButton("go_grafico", label = "Gráfico de Eventos", icon = icon("chart-bar"), class = "btn-primary"),
        actionButton("go_detalle", label = "Detalle del Tramo", icon = icon("file-alt"), class = "btn-info"),
        actionButton("go_nueva_visualizacion", label = "Nueva Visualización", icon = icon("image"), class = "btn-success")
      ),
      
      mainPanel(
        h3(textOutput("bienvenida_titulo")),
        p("Usa los botones del panel lateral para navegar por las secciones."),
        
        # Contenido dinámico basado en la elección de 'Pacientes' o 'Eventos'
        conditionalPanel(
          condition = "input.tipo_analisis == 'Pacientes'",
          h4("Analizar Pacientes:"),
          p("Aquí puedes ver los análisis relacionados con los pacientes.")
        ),
        
        conditionalPanel(
          condition = "input.tipo_analisis == 'Eventos'",
          h4("Analizar Eventos:"),
          p("Aquí puedes ver los análisis relacionados con los eventos.")
        )
      )
    )
  )
}

