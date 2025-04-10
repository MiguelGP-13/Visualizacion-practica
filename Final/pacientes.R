library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2) # Para heatmap
library(readxl)

### data_processing.R
pacientes <- read_excel("data/pacientes.xlsx", sheet = 1)
cardiovascular <- read.csv('data/historia_cardiovascular_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)
habitos <- read_excel("data/pacientes.xlsx", sheet = 6)
factores_riesgo <- read.csv('data/factores_riesgo_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)

datos <- pacientes %>%
  inner_join(habitos, by = c("Paciente" = "Codígo")) %>%
  inner_join(factores_riesgo, by = c("Paciente" = "Codígo")) %>%
  inner_join(cardiovascular, by = "Paciente")

datos <- datos %>%
  mutate(
    Tabaquismo_num = case_when(
      Tabaquismo == "NEVER_SMOKER" ~ 0,
      Tabaquismo == "EX_SMOKER" ~ 1,
      Tabaquismo == "SMOKER" ~ 2
    ),
    Consumo_diario_de_alcohol_num = case_when(
      `Consumo diario de alcohol` == "NO" ~ 0,
      `Consumo diario de alcohol` == "YES" ~ 1
    )
  )

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Relación entre Hábitos, Factores de Riesgo y Enfermedades"), # Ventana principal renombrada
  sidebarLayout(
    sidebarPanel(
      selectInput("enfermedades", "Selecciona una Enfermedad:", 
                  choices = c("Hipertensión arterial", "Enfermedad coronaria", "Arritmias", 
                              "Insuficiencia cardíaca", "Miocardiopatía", 
                              "Enfermedad vascular periférica", "Valvulopatía", 
                              "Enfermedad aórtica", "Enfermedad carótidea"),
                  selected = "Hipertensión arterial", multiple = FALSE),
      selectInput("factores", "Selecciona un Factor de Riesgo:", 
                  choices = c("Tabaquismo", "Consumo diario de alcohol", 
                              "Minutos semanales de actividad aeróbica", 
                              "Dieta Mediterránea", "Deficit sensorial"),
                  selected = "Tabaquismo", multiple = FALSE),
      checkboxInput("porcentajes", "Colorear por porcentajes en lugar de valores absolutos", TRUE) # Checkbox agregado
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("Boxplot", plotOutput("boxplot"))
      )
    )
  )
)

# Servidor
server <- function(input, output) {
  
  # Generar Heatmap con opción de porcentajes o valores absolutos
  output$heatmap <- renderPlot({
    datos_filtrados <- datos %>%
      select(all_of(c(input$enfermedades, input$factores))) %>%
      drop_na()
    
    # Calcular frecuencias y porcentajes
    tabla_frecuencias <- datos_filtrados %>%
      count(across(all_of(input$factores)), across(all_of(input$enfermedades))) %>%
      group_by(across(all_of(input$factores))) %>%
      mutate(total_factor = sum(n)) %>%
      ungroup() %>%
      mutate(porcentaje = n / total_factor * 100)
    
    # Elegir tipo de coloración
    fill_var <- if (input$porcentajes) "porcentaje" else "n"
    label_var <- if (input$porcentajes) "Porcentaje" else "Conteo"
    
    # Crear el heatmap
    ggplot(tabla_frecuencias, aes(x = !!sym(input$factores), y = !!sym(input$enfermedades), fill = !!sym(fill_var))) +
      geom_tile() +
      geom_text(aes(label = if (input$porcentajes) paste0(round(porcentaje, 1), "%\n(", n, ")") else n), 
                color = "black", size = 5) + # Etiquetas ajustadas según selección
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Heatmap de Hábitos y Factores de Riesgo",
           x = input$factores,
           y = input$enfermedades,
           fill = label_var) +
      theme_minimal()
  })

  # Generar Boxplot
  output$boxplot <- renderPlot({
    ggplot(datos, aes(
      x = !!sym(input$factores), 
      y = !!sym("Max valor P. Sistólica"), 
      fill = !!sym(input$factores)
    )) +
      geom_boxplot() +
      labs(title = "Boxplot por Factor de Riesgo",
           x = input$factores,
           y = "Max valor P. Sistólica") +
      theme_minimal()
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
