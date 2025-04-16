### Archivo principal: app.R

library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

source("datos_pacientes.R")

# Cargar datos
pacientes <- read_excel("../data/pacientes.xlsx", sheet = 1)
cardiovascular <- read.csv('../data/historia_cardiovascular_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)
habitos <- read_excel("../data/pacientes.xlsx", sheet = 6)
factores_riesgo <- read.csv('../data/factores_riesgo_limpio.csv', header = TRUE, sep = ",", check.names = FALSE)

# Unir y procesar datos
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
                            include.lowest = TRUE, right = FALSE, labels = FALSE)
  )

# Guardar datos globales
assign("datos_global", datos, envir = .GlobalEnv)

# UI
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
        tabPanel("Boxplot", plotlyOutput("boxplot")),
        tabPanel("Gráfico de Burbujas", plotlyOutput("bubblePlot"))
      )
    )
  ),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('jsCode', function(message) {
      eval(message.code);
    });
  "))
)

# Server
server <- function(input, output, session) {

  crear_combinacion_factores <- reactive({
    req(input$factores)
    datos %>% mutate(Grupo_Factores = apply(select(., all_of(input$factores)), 1, paste, collapse = " & "))
  })

  # Lookup de niveles únicos en el orden mostrado en los gráficos
  niveles_grupos <- reactive({
    crear_combinacion_factores() %>%
      pull(Grupo_Factores) %>%
      unique() %>%
      sort()
  })

  output$heatmap <- renderPlotly({
    datos_filtrados <- crear_combinacion_factores() %>%
      select(Grupo_Factores, all_of(input$enfermedades)) %>% drop_na()

    tabla <- datos_filtrados %>%
      count(Grupo_Factores, !!sym(input$enfermedades)) %>%
      group_by(Grupo_Factores) %>%
      mutate(total = sum(n), porcentaje = n / total * 100) %>%
      ungroup()

    fill <- if (input$porcentajes) "porcentaje" else "n"

    gg <- ggplot(tabla, aes(x = Grupo_Factores, y = !!sym(input$enfermedades), fill = .data[[fill]],
                            text = paste("Grupo:", Grupo_Factores, "<br>", input$enfermedades, ":", !!sym(input$enfermedades), "<br>n:", n))) +
      geom_tile() +
      geom_text(aes(label = if (input$porcentajes) paste0(round(porcentaje, 1), "%\n(", n, ")") else n), color = "black") +
      scale_fill_gradient(low = "white", high = "blue") +
      labs(x = "Combinación de Factores", y = input$enfermedades, fill = fill) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(gg, tooltip = "text") %>% 
      event_register("plotly_click")
  })

  output$boxplot <- renderPlotly({
    datos_combinados <- crear_combinacion_factores()
    p <- ggplot(datos_combinados, aes(x = Grupo_Factores, y = `Max valor P. Sistólica`, fill = Grupo_Factores,
                                      text = paste("Grupo:", Grupo_Factores, "<br>Presión:", `Max valor P. Sistólica`))) +
      geom_boxplot() +
      labs(x = "Grupo de Factores", y = "Max valor P. Sistólica") +
      theme_minimal()

    ggplotly(p, tooltip = "text") %>%
      event_register("plotly_click")
  })

  output$bubblePlot <- renderPlotly({
    datos_plot <- datos %>%
      group_by(Minutos_intervalo, !!sym(input$enfermedades)) %>%
      summarise(cantidad = n(), .groups = "drop")

    g <- ggplot(datos_plot, aes(x = Minutos_intervalo, y = !!sym(input$enfermedades), size = cantidad,
                                text = paste("Minutos:", Minutos_intervalo, "<br>Enfermedad:", !!sym(input$enfermedades), "<br>n:", cantidad))) +
      geom_point(alpha = 0.6, color = "steelblue") +
      theme_minimal()

    ggplotly(g, tooltip = "text") %>%
      event_register("plotly_click")
  })

observeEvent(event_data("plotly_click"), {
      click <- event_data("plotly_click")
      print(click)

      if (!is.null(click)) {
        # Recuperar el nombre real del grupo desde los niveles
        grupo_idx <- as.numeric(click$x)
        grupo_nombres <- niveles_grupos()
        grupo_real <- grupo_nombres[grupo_idx]

        yvalor <- as.character(click$y)
        enfermedad <- input$enfermedades

        # Convertir el valor de yvalor de acuerdo a la codificación correcta
        if (yvalor %in% c("1", "2")) {
          yvalor <- ifelse(yvalor == "1", "NO", "YES")
        }

        datos_con_grupo <- crear_combinacion_factores()

        datos_filtrados <- datos_con_grupo %>%
          filter(Grupo_Factores == grupo_real) %>%
          filter(as.character(.data[[enfermedad]]) == yvalor)

        # Excluir columnas técnicas
        columnas_excluir <- c("Grupo_Factores", "Tabaquismo_num", "Consumo_diario_de_alcohol_num", "Minutos_intervalo")
        columnas_mostrar <- setdiff(names(datos_filtrados), columnas_excluir)

        showModal(modalDialog(
          title = paste("Detalles para:", grupo_real, "|", enfermedad, "=", yvalor),
          DT::dataTableOutput("tabla_detalles"),
          easyClose = TRUE,
          size = "l"
        ))

        output$tabla_detalles <- DT::renderDataTable({
          DT::datatable(datos_filtrados[, columnas_mostrar, drop = FALSE],
                        options = list(pageLength = 10, scrollX = TRUE))
        })
      }
  })



}

# Run app
shinyApp(ui, server)
