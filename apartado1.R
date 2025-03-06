library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)

# Cargar datos
pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)

# UI
ui <- fluidPage(
  titlePanel("Distribución de Pacientes por Edad y Sexo"),
  sidebarLayout(
    sidebarPanel(
      helpText("Gráfico de pacientes según edad y sexo")
    ),
    mainPanel(
      plotlyOutput("edadPlot"),
      plotlyOutput("edadGrupoPlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$edadPlot <- renderPlotly({
    # Agrupar datos por edad y sexo
    datos_agrupados <- pacientes %>%
      group_by(Edad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico
    p <- ggplot(datos_agrupados, aes(x = Edad, y = Conteo, color = Sexo, group = Sexo, text = paste("Edad:", Edad, "\nPacientes:", Conteo))) +
      geom_line() +
      geom_point() +
      labs(title = "Distribución de Pacientes por Edad y Sexo",
           x = "Edad",
           y = "Número de Pacientes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$edadGrupoPlot <- renderPlotly({
    # Agrupar edades en rangos de 3 en 3
    pacientes <- pacientes %>%
      mutate(GrupoEdad = (Edad %/% 3) * 3)
    
    # Agrupar datos por grupo de edad y sexo
    datos_agrupados <- pacientes %>%
      group_by(GrupoEdad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico
    p <- ggplot(datos_agrupados, aes(x = GrupoEdad, y = Conteo, color = Sexo, group = Sexo, text = paste("Grupo de Edad:", GrupoEdad, "\nPacientes:", Conteo))) +
      geom_line() +
      geom_point() +
      labs(title = "Distribución de Pacientes por Grupos de Edad y Sexo",
           x = "Grupo de Edad",
           y = "Número de Pacientes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)

