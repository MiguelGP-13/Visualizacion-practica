library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)

# Cargar datos
pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)
eventos <- read_excel("data/eventos.xlsx", sheet = 1)

# UI
ui <- fluidPage(
  titlePanel("Distribución de Pacientes por Edad y Sexo"),
  sidebarLayout(
    sidebarPanel(
      helpText("Gráfico de pacientes según edad y sexo, y eventos de sangrado")
    ),
    mainPanel(
      plotlyOutput("edadPlot"),
      plotlyOutput("edadGrupoPlot"),
      plotlyOutput("sangradoPlot")  # Nuevo gráfico para eventos de sangrado
    )
  )
)

# Server
server <- function(input, output) {
  
  # Gráfico de dispersión (scatter plot) para distribución por Edad y Sexo
  output$edadPlot <- renderPlotly({
    # Agrupar datos por edad y sexo
    datos_agrupados <- pacientes %>%
      group_by(Edad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico de dispersión
    p <- ggplot(datos_agrupados, aes(x = Edad, y = Conteo, color = Sexo, text = paste("Edad:", Edad, "\nPacientes:", Conteo))) +
      geom_point(size = 4) +  # Gráfico de dispersión (solo puntos)
      labs(title = "Distribución de Pacientes por Edad y Sexo",
           x = "Edad",
           y = "Número de Pacientes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico de burbuja para distribución por Edad y Sexo
  output$edadPlot <- renderPlotly({
    # Agrupar datos por edad y sexo
    datos_agrupados <- pacientes %>%
      group_by(Edad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico de burbuja
    p <- ggplot(datos_agrupados, aes(x = Edad, y = Conteo, color = Sexo, size = Conteo, text = paste("Edad:", Edad, "\nPacientes:", Conteo))) +
      geom_point(alpha = 0.6) +  # Gráfico de burbuja
      labs(title = "Distribución de Pacientes por Edad y Sexo",
           x = "Edad",
           y = "Número de Pacientes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico de burbuja para distribución por Grupo de Edad y Sexo
  output$edadGrupoPlot <- renderPlotly({
    # Agrupar edades en rangos de 3 en 3
    pacientes <- pacientes %>%
      mutate(GrupoEdad = (Edad %/% 3) * 3)
    
    # Agrupar datos por grupo de edad y sexo
    datos_agrupados <- pacientes %>%
      group_by(GrupoEdad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico de burbuja
    p <- ggplot(datos_agrupados, aes(x = GrupoEdad, y = Conteo, color = Sexo, size = Conteo, text = paste("Grupo de Edad:", GrupoEdad, "\nPacientes:", Conteo))) +
      geom_point(alpha = 0.6) +  # Gráfico de burbuja
      labs(title = "Distribución de Pacientes por Grupos de Edad y Sexo",
           x = "Grupo de Edad",
           y = "Número de Pacientes") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  # Gráfico para visualizar eventos de sangrado
  output$sangradoPlot <- renderPlotly({
    # Unir los datos de pacientes con los eventos de sangrado
    datos_sangrado <- pacientes %>%
      inner_join(eventos, by = "Paciente") %>%
      filter(!is.na(`Tipo de sangrado`) & `Tipo de sangrado` != "") %>%
      group_by(Edad, Sexo) %>%
      summarise(Conteo = n(), .groups = "drop")
    
    # Crear gráfico de dispersión para eventos de sangrado
    p <- ggplot(datos_sangrado, aes(x = Edad, y = Conteo, color = Sexo, text = paste("Edad:", Edad, "\nPacientes con Sangrado:", Conteo))) +
      geom_point(size = 4) +  # Gráfico de dispersión
      labs(title = "Eventos de Sangrado por Edad y Sexo",
           x = "Edad",
           y = "Número de Pacientes con Sangrado") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)