library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2) # Para heatmap
library(readxl)

# Cargar datos
pacientes <- read_excel("../data/pacientes.xlsx", sheet = 1)
cardiovascular <- read.csv('../data/historia_cardiovascular_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)
habitos <- read_excel("../data/pacientes.xlsx", sheet = 6)
factores_riesgo <- read.csv('../data/factores_riesgo_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)

# Procesar datos
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
    ),
    Minutos_intervalo = cut(`Minutos semanales de actividad aeróbica`, 
                           breaks = seq(0, max(datos$`Minutos semanales de actividad aeróbica`), by = 60), 
                           include.lowest = TRUE, right = FALSE, labels = FALSE)
  )

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Relación entre Hábitos, Factores de Riesgo y Enfermedades"), 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Heatmap' || input.tabs == 'Gráfico de Burbujas'", 
        selectInput("enfermedades", "Selecciona una Enfermedad:", 
                    choices = c("Hipertensión arterial", "Enfermedad coronaria", "Arritmias", 
                                "Insuficiencia cardíaca", "Miocardiopatía", 
                                "Enfermedad vascular periférica", "Valvulopatía", 
                                "Enfermedad aórtica", "Enfermedad carótidea"),
                    selected = "Hipertensión arterial", multiple = FALSE)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Heatmap' || input.tabs == 'Boxplot'", 
        selectInput("factores", "Selecciona un Factor de Riesgo:", 
                    choices = c("Tabaquismo", "Consumo diario de alcohol", 
                                "Dieta Mediterránea", "Deficit sensorial"),
                    selected = "Tabaquismo", multiple = TRUE)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Heatmap'", 
        checkboxInput("porcentajes", "Colorear por porcentajes en lugar de valores absolutos", TRUE)
      ),
      textOutput("tabSelectedValue")  # Mostrar el valor del tab seleccionado
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Heatmap", plotOutput("heatmap")),
        tabPanel("Boxplot", plotOutput("boxplot")),
        tabPanel("Gráfico de Burbujas", plotOutput("bubblePlot"))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Columnas combinadas:
  crear_combinacion_factores <- reactive({
    req(input$factores)
    
    datos %>%
      mutate(Grupo_Factores = apply(select(., all_of(input$factores)), 1, function(row) {
        paste(row, collapse = " & ")
      }))
  })

  
  # Generar Heatmap con opción de porcentajes o valores absolutos
  output$heatmap <- renderPlot({
  req(input$factores, input$enfermedades)
  
  datos_filtrados <- crear_combinacion_factores() %>%
    select(Grupo_Factores, all_of(input$enfermedades)) %>%
    drop_na()
  
  tabla_frecuencias <- datos_filtrados %>%
    count(Grupo_Factores, !!sym(input$enfermedades)) %>%
    group_by(Grupo_Factores) %>%
    mutate(total_factor = sum(n)) %>%
    ungroup() %>%
    mutate(porcentaje = n / total_factor * 100)
  
  fill_var <- if (input$porcentajes) "porcentaje" else "n"
  label_var <- if (input$porcentajes) "Porcentaje" else "Conteo"
  
  ggplot(tabla_frecuencias, aes(x = Grupo_Factores, y = !!sym(input$enfermedades), fill = !!sym(fill_var))) +
    geom_tile() +
    geom_text(aes(label = if (input$porcentajes) paste0(round(porcentaje, 1), "%\n(", n, ")") else n), 
              color = "black", size = 5) + 
    scale_fill_gradient(low = "white", high = "blue") +
    labs(title = "Heatmap de Combinaciones de Factores de Riesgo",
         x = "Combinación de Factores",
         y = input$enfermedades,
         fill = label_var) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


  # Generar Boxplot
  output$boxplot <- renderPlot({
  datos_combinados <- crear_combinacion_factores()
  
  ggplot(datos_combinados, aes(x = Grupo_Factores, y = `Max valor P. Sistólica`, fill = Grupo_Factores)) +
    geom_boxplot() +
    labs(title = "Boxplot por Combinación de Factores de Riesgo",
         x = "Combinación de Factores",
         y = "Max valor P. Sistólica") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})


  # Generar Gráfico de Burbujas
  output$bubblePlot <- renderPlot({
    datos_filtrados <- datos %>%
      group_by(Minutos_intervalo, !!sym(input$enfermedades)) %>%
      summarise(cantidad = n()) %>%
      ungroup()

    ggplot(datos_filtrados, aes(x = Minutos_intervalo, y = !!sym(input$enfermedades), size = cantidad)) +
      geom_point(alpha = 0.5) +
      labs(title = paste("Gráfico de Burbujas: ", input$enfermedades, "vs Minutos de Actividad Aeróbica"),
           x = "Intervalos de Minutos semanales de actividad aeróbica",
           y = input$enfermedades, size = "Cantidad de personas") +
      theme_minimal() +
      scale_size_continuous(range = c(5, 50))
  })

  # Observar el cambio de la pestaña activa y mostrarlo
  observe({
    # Mostrar el valor de la pestaña seleccionada
    selected_tab <- input$tabs
    output$tabSelectedValue <- renderText({
      paste("Tab seleccionado:", selected_tab)
    })
  })
}

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
