# modules/visualizations/grafico_eventos.R

crear_grafico_eventos <- function(input, output, session, datos, ranges, click_info) {
  observeEvent(input$tipo_evento, {
    if (input$tipo_evento == "Sangrado") {
      niveles_gravedad <- unique(na.omit(datos$eventos_sangrado$`Gravedad de la hemorragia (TIMI)`))
      updateCheckboxGroupInput(session, "gravedad", choices = niveles_gravedad, selected = c())
      updateCheckboxGroupInput(session, "trombotico", choices = c(), selected = c())
    } else if (input$tipo_evento == "Trombotico") {
      niveles_trombotico <- unique(na.omit(datos$eventos_tromboticos$`Tipo de evento trombótico`))
      updateCheckboxGroupInput(session, "trombotico", choices = niveles_trombotico, selected = c())
      updateCheckboxGroupInput(session, "gravedad", choices = c(), selected = c())
    } else {
      updateCheckboxGroupInput(session, "gravedad", choices = c(), selected = c())
      updateCheckboxGroupInput(session, "trombotico", choices = c(), selected = c())
    }
    ranges$x <- NULL
  })

  observeEvent(input$plot_brush, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
    }
  })

  observeEvent(input$plot_dblclick, {
    ranges$x <- NULL
  })

  observeEvent(input$plot_click, {
    click_x <- round(input$plot_click$x)
    click_fill <- input$tipo_evento
    click_info$tipo <- click_fill
    click_info$tramo <- click_x
  })

  output$grafico_eventos <- renderPlot({
    datos_filtrados <- datos$eventos_totales %>%
      filter((input$tipo_evento == "Todos" | Evento == input$tipo_evento) &
               (input$sexo == "Todos" | Sexo == input$sexo) &
               (Edad >= input$edad[1] & Edad <= input$edad[2]))
    
    if (input$tipo_evento == "Sangrado" && !is.null(input$gravedad) && length(input$gravedad) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Gravedad de la hemorragia (TIMI)` %in% input$gravedad)
    }
    if (input$tipo_evento == "Trombotico" && !is.null(input$trombotico) && length(input$trombotico) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Tipo de evento trombótico` %in% input$trombotico)
    }

    ggplot(datos_filtrados, aes(
      x = Edad, 
      fill = case_when(
        input$tipo_evento == "Todos" ~ Evento,
        Evento == "Sangrado" ~ as.character(`Gravedad de la hemorragia (TIMI)`),
        Evento == "Trombotico" ~ as.character(`Tipo de evento trombótico`),
        TRUE ~ Evento
      )
    )) +
      geom_histogram(breaks = seq(48, 98, by = 5), position = "stack", color = "black") +
      labs(title = "Distribución de Eventos por Edad", x = "Edad", y = "Frecuencia", fill = "Leyenda") +
      theme_minimal() +
      coord_cartesian(xlim = ranges$x)
  })
}
