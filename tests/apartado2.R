# app.R

library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)

ui <- fluidPage(
  titlePanel("Dashboard Integrado de Pacientes y Eventos"),
  sidebarLayout(
    sidebarPanel(
      helpText("Datos integrados de pacientes y eventos. Se combinan las hojas de pacientes y se analizan factores, tratamientos, diagnósticos y causas de muerte.")
      # Aquí se pueden agregar inputs para filtrar por variables, rangos de edad, etc.
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Eventos Sangrado", plotOutput("histPlot")),
        tabPanel("Trombóticos vs Sangrado",
                 plotOutput("correlPlot"),
                 verbatimTextOutput("corText")),
        tabPanel("Antecedentes Cardio", plotOutput("cardioPlot")),
        tabPanel("Factores de Riesgo", plotOutput("riesgoPlot")),
        tabPanel("Antitrombóticos", plotOutput("antitromPlot")),
        tabPanel("Diagnósticos", plotOutput("diagPlot")),
        tabPanel("Causas de Muerte", plotOutput("muertePlot")),
        tabPanel("Demografía", plotOutput("demoPlot"))
      )
    )
  )
)

server <- function(input, output) {
  ## Cargar la información de pacientes desde distintas hojas del Excel
  pacientes_base <- read_excel("data/pacientes.xlsx", sheet = 1) %>% clean_names() # datos pacientes
  historia      <- read_excel("data/pacientes.xlsx", sheet = 2) %>% clean_names()  # cardiovascular
  factores      <- read_excel("data/pacientes.xlsx", sheet = 5) %>% clean_names()  # Factores
  otros_diag    <- read_excel("data/pacientes.xlsx", sheet = 4) %>% clean_names()  # Otros diagnosticos
  antitrom      <- read_excel("data/pacientes.xlsx", sheet = 3) %>% clean_names()  # Antitromboticos
  
  # Unir la información de las distintas hojas (se unen por "paciente")
  pacientes <- pacientes_base %>%
    left_join(historia, by = "paciente") %>%
    left_join(factores, by = c("paciente" = "codigo")) %>%   # <--- Aquí se indica la correspondencia
    left_join(otros_diag, by = "paciente") %>%
    left_join(antitrom, by = "paciente")
  
  ## Cargar los eventos (de sangrado y trombóticos) desde eventos.xlsx
  eventos_sangrado   <- read_excel("data/eventos.xlsx", sheet = "sangrado") %>% clean_names()
  eventos_trombotico <- read_excel("data/eventos.xlsx", sheet = "trombotico") %>% clean_names()
  
  # Calcular el número de eventos de sangrado por paciente
  sangrado_count <- eventos_sangrado %>%
    group_by(paciente) %>%
    summarise(n_sangrado = n())
  
  pacientes <- pacientes %>%
    left_join(sangrado_count, by = "paciente")
  
  pacientes$n_sangrado[is.na(pacientes$n_sangrado)] <- 0
  
  # Calcular el número de eventos trombóticos por paciente
  trombo_count <- eventos_trombotico %>%
    group_by(paciente) %>%
    summarise(n_trombo = n())
  
  pacientes <- pacientes %>%
    left_join(trombo_count, by = "paciente")
  
  pacientes$n_trombo[is.na(pacientes$n_trombo)] <- 0
  
  ## Derivar variables adicionales
  
  # 1. Antecedentes cardiovasculares: se considera "Con antecedentes" si alguno de los campos es "Si"
  pacientes <- pacientes %>%
    mutate(antecedentes_cardio = if_else(
      hipertension_arterial == "Si" |
        enfermedad_coronaria == "Si" |
        valvulopatia == "Si" |
        arritmias == "Si",
      "Con antecedentes", "Sin antecedentes"
    ))
  
  # 2. Score de factores de riesgo: se suma 1 por cada factor “Si”
  pacientes <- pacientes %>%
    mutate(riesgo_score = 
             (if_else(riesgo_de_caida == "Si", 1, 0)) +
             (if_else(caidas_previas_conocidas == "Si", 1, 0)) +
             (if_else(toma_algun_medicamento_presente_en_esta_lista == "Si", 1, 0)) +
             (if_else(deficit_sensorial == "Si", 1, 0)) +
             (if_else(deambulacion_alterada == "Si", 1, 0)) +
             (if_else(tabaquismo == "Si", 1, 0)) +
             (if_else(consumo_diario_de_alcohol == "Si", 1, 0))
    )
  
  # 3. Tratamiento antitrombótico: se usa la columna "se_le_ha_prescrito_anticoagulante"
  # y, si es "Si", se toma el valor de "tipo_de_anticoagulante"
  # Definir la función auxiliar (si no está definida aún)
  standardize_text <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    return(x)
  }
  
  # Código ACTUALIZADO para tratamiento antitrombótico:
  pacientes <- pacientes %>%
    mutate(across(c(se_le_ha_prescrito_anticoagulante, tipo_de_anticoagulante), ~ standardize_text(as.character(.)))) %>%
    mutate(tratamiento_anticoagulante = if_else(
      se_le_ha_prescrito_anticoagulante == "si",
      case_when(
        tipo_de_anticoagulante == "avk" ~ "AVK",
        tipo_de_anticoagulante == "acod" ~ "ACOD",
        tipo_de_anticoagulante == "other" ~ "OTHER",
        TRUE ~ "Desconocido"
      ),
      "No prescrito"
    ))
  
  
  # 4. Causa de muerte: a título de ejemplo, se deriva de dos campos en historia cardiovascular
  pacientes <- pacientes %>%
    mutate(causa_muerte = case_when(
      muerte_subita_cardiaca == "Si" ~ "Muerte súbita cardiaca",
      insuficiencia_cardiaca == "Si" ~ "Insuficiencia cardiaca",
      TRUE ~ "No fallecido"
    ))
  
  ## Visualizaciones
  
  # Tab 1. Eventos de Sangrado: Histograma
  output$histPlot <- renderPlot({
    ggplot(pacientes, aes(x = n_sangrado)) +
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
      labs(title = "Distribución de Eventos de Sangrado",
           x = "Número de Eventos de Sangrado",
           y = "Cantidad de Pacientes")
  })
  
  # Tab 2. Trombóticos vs. Sangrado: Scatter plot con línea de regresión
  output$correlPlot <- renderPlot({
    ggplot(pacientes, aes(x = n_trombo, y = n_sangrado)) +
      geom_point(color = "#FF4500", size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      labs(title = "Eventos Trombóticos vs. Sangrado",
           x = "Número de Eventos Trombóticos",
           y = "Número de Eventos de Sangrado")
  })
  
  output$corText <- renderPrint({
    correlation <- cor(pacientes$n_trombo, pacientes$n_sangrado, use = "complete.obs")
    cat("Coeficiente de correlación:", round(correlation, 2))
  })
  
  # Tab 3. Antecedentes Cardiovasculares: Boxplot de sangrado según antecedentes
  output$cardioPlot <- renderPlot({
    ggplot(pacientes, aes(x = antecedentes_cardio, y = n_sangrado)) +
      geom_boxplot(fill = "#8FBC8F", alpha = 0.7) +
      labs(title = "Eventos de Sangrado vs. Antecedentes Cardiovasculares",
           x = "Antecedentes Cardiovasculares",
           y = "Número de Eventos de Sangrado")
  })
  
  # Tab 4. Factores de Riesgo: Boxplot de sangrado según score de riesgo
  output$riesgoPlot <- renderPlot({
    ggplot(pacientes, aes(x = as.factor(riesgo_score), y = n_sangrado)) +
      geom_boxplot(fill = "#FFA07A", alpha = 0.7) +
      labs(title = "Eventos de Sangrado vs. Score de Factores de Riesgo",
           x = "Score de Factores de Riesgo",
           y = "Número de Eventos de Sangrado")
  })
  
  # Tab 5. Antitrombóticos: Boxplot de sangrado según tratamiento antitrombótico
  output$antitromPlot <- renderPlot({
    ggplot(pacientes, aes(x = tratamiento_anticoagulante, y = n_sangrado)) +
      geom_boxplot(fill = "#404080", alpha = 0.7) +
      labs(title = "Eventos de Sangrado vs. Tratamiento Antitrombótico",
           x = "Tipo de Anticoagulante",
           y = "Número de Eventos de Sangrado") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tab 6. Diagnósticos: Frecuencia de condiciones en la hoja "otros diagnósticos"
  # Define la función para estandarizar textos:
  standardize_text <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- tolower(x)
    x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
    return(x)
  }
  
  # Estandarizamos todos los valores (excepto el identificador "paciente")
  otros_diag_std <- otros_diag %>% 
    mutate(across(-paciente, ~ standardize_text(as.character(.))))  
  
  # Pivotamos y filtramos solo aquellos diagnósticos con valor "yes"
  diag_data <- otros_diag_std %>%
    pivot_longer(
      cols = -paciente,
      names_to = "diagnostico",
      values_to = "valor"
    ) %>%
    filter(valor == "yes") %>%         # Ahora filtramos por "yes"
    group_by(diagnostico) %>%
    summarise(cantidad = n(), .groups = "drop")
  
  # Visualizamos la frecuencia de diagnósticos
  output$diagPlot <- renderPlot({
    ggplot(diag_data, aes(x = reorder(diagnostico, -cantidad), y = cantidad)) +
      geom_bar(stat = "identity", fill = "#2E8B57") +
      labs(title = "Frecuencia de Diagnósticos (Otros Diagnósticos)",
           x = "Diagnóstico",
           y = "Número de Pacientes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tab 7. Causas de Muerte: Barplot
  muerte_data <- pacientes %>%
    group_by(causa_muerte) %>%
    summarise(cantidad = n())
  
  output$muertePlot <- renderPlot({
    ggplot(muerte_data, aes(x = reorder(causa_muerte, -cantidad), y = cantidad)) +
      geom_bar(stat = "identity", fill = "#D2691E") +
      labs(title = "Principales Causas de Muerte",
           x = "Causa de Muerte",
           y = "Número de Pacientes") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tab 8. Demografía: Distribución por Sexo
  output$demoPlot <- renderPlot({
    ggplot(pacientes, aes(x = sexo)) +
      geom_bar(fill = "#2E8B57", alpha = 0.7) +
      labs(title = "Distribución de Pacientes por Sexo",
           x = "Sexo",
           y = "Número de Pacientes")
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

