library(shiny)
library(dplyr)
library(DT)
library(urltools)
library(readxl)

# Cargar los mismos datos que en la app principal
pacientes <- read_excel("../data/pacientes.xlsx", sheet = 1)
cardiovascular <- read.csv('../data/historia_cardiovascular_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)
habitos <- read_excel("../data/pacientes.xlsx", sheet = 6)
factores_riesgo <- read.csv('../data/factores_riesgo_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)

# Unir datos
datos <- pacientes %>%
  inner_join(habitos, by = c("Paciente" = "Codígo")) %>%
  inner_join(factores_riesgo, by = c("Paciente" = "Codígo")) %>%
  inner_join(cardiovascular, by = "Paciente") %>%
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
                            breaks = seq(0, max(`Minutos semanales de actividad aeróbica`, na.rm = TRUE), by = 60),
                            include.lowest = TRUE, right = FALSE, labels = FALSE),
    Grupo_Factores = paste(Tabaquismo, `Consumo diario de alcohol`, `Dieta Mediterránea`, `Deficit sensorial`, sep = " & ")
  )

ui <- fluidPage(
  titlePanel("Pacientes del Grupo Seleccionado"),
  sidebarLayout(
    sidebarPanel(
      helpText("Puedes usar los filtros de la tabla para refinar la búsqueda."),
      selectInput("columna_filtro", "Agrupar por columna:",
                  choices = names(datos), selected = "Sexo"),
      checkboxInput("mostrar_agrupado", "Mostrar conteo por grupo", FALSE)
    ),
    mainPanel(
      DTOutput("tabla_pacientes")
    )
  )
)

server <- function(input, output, session) {
  query <- parseQueryString(session$clientData$url_search)

  grupo <- URLdecode(query$grupo)
  yvalor <- URLdecode(query$y)

  datos_filtrados <- reactive({
    if (!is.null(grupo)) {
      datos %>% filter(Grupo_Factores == grupo)
    } else {
      datos
    }
  })

  output$tabla_pacientes <- renderDT({
    datos_mostrar <- datos_filtrados()

    if (input$mostrar_agrupado) {
      datos_mostrar <- datos_mostrar %>%
        group_by(.data[[input$columna_filtro]]) %>%
        summarise(Conteo = n(), .groups = "drop")
    }

    datatable(
      datos_mostrar,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        searchHighlight = TRUE
      ),
      filter = "top",
      rownames = FALSE,
      extensions = 'Buttons'
    )
  })
}

shinyApp(ui, server)
