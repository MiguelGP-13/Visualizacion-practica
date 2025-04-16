grafico_descripcion_pacientes_ui <- function(datos) {
  pacientes <- datos$pacientes
  fluidPage(
    titlePanel("Relación entre Hábitos, Factores de Riesgo y Enfermedades"),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.tabs == 'Boxplot'",
          h4("Máxima P. Sistólica para pacientes con hipertensión arterial")
        ),
        conditionalPanel(
          condition = "input.tabs == 'Heatmap' || input.tabs == 'Gráfico de Burbujas'",
          selectInput("enfermedades", "Selecciona una Enfermedad:",
                      choices = c("Hipertensión arterial", "Enfermedad coronaria", "Arritmias",
                                  "Insuficiencia cardíaca", "Miocardiopatía",
                                  "Enfermedad vascular periférica", "Valvulopatía",
                                  "Enfermedad aórtica", "Enfermedad carótidea"),
                      selected = "Hipertensión arterial")
        ),
        conditionalPanel(
          condition = "input.tabs == 'Heatmap' || input.tabs == 'Boxplot'",
          selectInput("factores", "Selecciona un Factor de Riesgo:",
                      choices = c("Tabaquismo", "Consumo diario de alcohol",
                                  "Dieta Mediterránea", "Deficit sensorial"),
                      selected = "Tabaquismo", multiple = TRUE)
        ),
        selectInput("sexo", "Género:", 
                    choices = c("Todos", unique(pacientes$Sexo))),
        sliderInput("edad", "Edad:", 
                    min = min(pacientes$Edad), max = max(pacientes$Edad), 
                    value = c(min(pacientes$Edad), max(pacientes$Edad))),
        conditionalPanel(
          condition = "input.tabs == 'Heatmap'",
          checkboxInput("porcentajes", "Colorear por porcentajes en lugar de valores absolutos", TRUE)
        )
      ),
      mainPanel(
        tabsetPanel(id = "tabs",
          tabPanel("Heatmap", plotlyOutput("heatmap")),
          tabPanel("Boxplot", plotlyOutput("boxplot")),
          tabPanel("Gráfico de Burbujas", plotlyOutput("bubblePlot"))
        )
      )
    )
  )
}
