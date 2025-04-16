# modules/views/descripcion_pacientes_ui.R

grafico_descripcion_pacientes_ui <- function() {
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
        conditionalPanel(
            condition = "input.tabs == 'Heatmap'",
            checkboxInput("porcentajes", "Colorear por porcentajes en lugar de valores absolutos", TRUE)
        )
        ),
        mainPanel(
        tabsetPanel(id = "tabs",
            tabPanel("Heatmap", plotlyOutput("heatmap")),
            # tabPanel("Boxplot", plotlyOutput("boxplot")),
            tabPanel("Boxplot",
                    #  h3("Máxima P. Sistólica para pacientes con hipertensión arterial"),  # Añade el texto aquí
                    plotlyOutput("boxplot")),
            tabPanel("Gráfico de Burbujas", plotlyOutput("bubblePlot"))
        )
        )
    )
    )

}
