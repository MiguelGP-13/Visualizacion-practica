# app.R

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)
library(gridExtra)

ui <- fluidPage(
  titlePanel("Dashboard de Eventos de Sangrado y Trombóticos"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Asegúrate de que los archivos estén en la carpeta 'data'.  
                Se combinan la hoja 'datos del paciente' y 'historia cardiovascular'.")
      # Aquí puedes agregar inputs para filtrar o elegir variables.
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Eventos Sangrado", 
                 plotOutput("histPlot")),
        tabPanel("Trombóticos vs Sangrado", 
                 plotOutput("correlPlot"),
                 verbatimTextOutput("corText")),
        tabPanel("Antecedentes Cardiovasculares", 
                 plotOutput("cardioPlot")),
        tabPanel("Demografía", 
                 plotOutput("demoPlot"))
      )
    )
  )
)

server <- function(input, output) {
  # Cargar la información de pacientes:
  # Hoja "datos del paciente" con Edad, Paciente y Sexo.
  # Hoja "historia cardiovascular" con información complementaria.
  pacientes_base <- read_excel("data/pacientes.xlsx", sheet = 1) %>% 
    clean_names()  # Convertirá "Paciente" a "paciente", "Edad" a "edad", etc.
  
  historia <- read_excel("data/pacientes.xlsx", sheet = 2) %>% 
    clean_names()
  
  # Unir ambos dataframes por la columna "paciente"
  pacientes <- left_join(pacientes_base, historia, by = "paciente")
  
  # Cargar eventos (sangrado y trombóticos) y limpiar nombres
  eventos_sangrado <- read_excel("data/eventos.xlsx", sheet = "sangrado") %>% 
    clean_names()  # Se espera que tenga la columna "paciente"
  eventos_tromboticos <- read_excel("data/eventos.xlsx", sheet = "trombotico") %>% 
    clean_names()
  
  # Calcular eventos de sangrado por paciente
  sangrado_count <- eventos_sangrado %>%
    group_by(paciente) %>%
    summarise(n_sangrado = n())
  
  pacientes <- left_join(pacientes, sangrado_count, by = "paciente")
  
  # Asignar 0 a los pacientes sin eventos de sangrado
  pacientes$n_sangrado[is.na(pacientes$n_sangrado)] <- 0
  
  # Calcular eventos trombóticos por paciente
  trombo_count <- eventos_tromboticos %>%
    group_by(paciente) %>%
    summarise(n_trombo = n())
  
  pacientes <- left_join(pacientes, trombo_count, by = "paciente")
  
  pacientes$n_trombo[is.na(pacientes$n_trombo)] <- 0
  
  # Derivar variable "antecedentes_cardio"
  # Usando las columnas provenientes de "historia cardiovascular":
  # Se consideran: hipertension_arterial, enfermedad_coronaria, valvulopatia y arritmias.
  # Se asume que en estos campos se registran valores "Si" o "No".
  pacientes <- pacientes %>%
    mutate(
      antecedentes_cardio = if_else(
        (hipertension_arterial == "Si") |
          (enfermedad_coronaria == "Si") |
          (valvulopatia == "Si") |
          (arritmias == "Si"),
        "Con antecedentes", "Sin antecedentes"
      )
    )
  
  # 1. Histograma de eventos de sangrado por paciente
  output$histPlot <- renderPlot({
    ggplot(pacientes, aes(x = n_sangrado)) +
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
      labs(title = "Distribución de eventos de sangrado por paciente", 
           x = "Número de eventos de sangrado", 
           y = "Cantidad de pacientes")
  })
  
  # 2. Relación entre eventos trombóticos y de sangrado
  output$correlPlot <- renderPlot({
    ggplot(pacientes, aes(x = n_trombo, y = n_sangrado)) +
      geom_point(color = "#FF4500", size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Relación entre eventos trombóticos y sangrado",
           x = "Número de eventos trombóticos",
           y = "Número de eventos de sangrado")
  })
  
  output$corText <- renderPrint({
    correlation <- cor(pacientes$n_trombo, pacientes$n_sangrado, use = "complete.obs")
    cat("Coeficiente de correlación:", round(correlation, 2))
  })
  
  # 3. Boxplot de eventos de sangrado según antecedentes cardiovasculares
  output$cardioPlot <- renderPlot({
    ggplot(pacientes, aes(x = antecedentes_cardio, y = n_sangrado)) +
      geom_boxplot(fill = "#8FBC8F", alpha = 0.7) +
      labs(title = "Eventos de sangrado vs. Antecedentes cardiovasculares",
           x = "Antecedentes cardiovasculares",
           y = "Número de eventos de sangrado")
  })
  
  # 4. Gráfico Demográfico: Distribución de pacientes por Sexo
  output$demoPlot <- renderPlot({
    ggplot(pacientes, aes(x = sexo)) +
      geom_bar(fill = "#2E8B57", alpha = 0.7) +
      labs(title = "Distribución de pacientes por Sexo",
           x = "Sexo", 
           y = "Número de pacientes")
  })
}

shinyApp(ui = ui, server = server)
