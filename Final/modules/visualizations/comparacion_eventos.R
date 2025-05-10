crear_comparacion_eventos <- function(input, output, session, datos) {
  ranges <- reactiveValues(a = NULL, b = NULL)
  
  # Filtros Histograma A
  make_plot_a <- function(rng) {
    datos_filtrados <- datos$eventos_totales %>%
      filter((input$cmp_tipo_evento_a == "Todos" | Evento == input$cmp_tipo_evento_a) &
               (input$cmp_sexo_a == "Todos" | Sexo == input$cmp_sexo_a) &
               (Edad >= input$cmp_edad[1] & Edad <= input$cmp_edad[2])) %>%
      { if (input$cmp_tipo_evento_a == "Sangrado" && length(input$cmp_gravedad_a) > 0)
        filter(., `Gravedad de la hemorragia (TIMI)` %in% input$cmp_gravedad_a) else . } %>%
      { if (input$cmp_tipo_evento_a == "Trombotico" && length(input$cmp_trombo_a) > 0)
        filter(., `Tipo de evento trombótico` %in% input$cmp_trombo_a) else . }
    
    ggplot(datos_filtrados, aes(x = Edad, fill = case_when(
      input$cmp_tipo_evento_a == "Todos" ~ Evento,
      Evento == "Sangrado" ~ as.character(`Gravedad de la hemorragia (TIMI)`),
      Evento == "Trombotico" ~ as.character(`Tipo de evento trombótico`),
      TRUE ~ Evento
    ))) +
      geom_histogram(breaks = seq(input$cmp_edad[1], input$cmp_edad[2], by = 5), position = "stack", color = "black") +
      theme_minimal() +
      labs(title = "Histograma A", x = "Edad", y = "Frecuencia", fill = "Leyenda") +
      coord_cartesian(xlim = rng)
  }
  
  # Filtros Histograma B
  make_plot_b <- function(rng) {
    datos_filtrados <- datos$eventos_totales %>%
      filter((input$cmp_tipo_evento_b == "Todos" | Evento == input$cmp_tipo_evento_b) &
               (input$cmp_sexo_b == "Todos" | Sexo == input$cmp_sexo_b) &
               (Edad >= input$cmp_edad[1] & Edad <= input$cmp_edad[2])) %>%
      { if (input$cmp_tipo_evento_b == "Sangrado" && length(input$cmp_gravedad_b) > 0)
        filter(., `Gravedad de la hemorragia (TIMI)` %in% input$cmp_gravedad_b) else . } %>%
      { if (input$cmp_tipo_evento_b == "Trombotico" && length(input$cmp_trombo_b) > 0)
        filter(., `Tipo de evento trombótico` %in% input$cmp_trombo_b) else . }
    
    ggplot(datos_filtrados, aes(x = Edad, fill = case_when(
      input$cmp_tipo_evento_b == "Todos" ~ Evento,
      Evento == "Sangrado" ~ as.character(`Gravedad de la hemorragia (TIMI)`),
      Evento == "Trombotico" ~ as.character(`Tipo de evento trombótico`),
      TRUE ~ Evento
    ))) +
      geom_histogram(breaks = seq(input$cmp_edad[1], input$cmp_edad[2], by = 5), position = "stack", color = "black") +
      theme_minimal() +
      labs(title = "Histograma B", x = "Edad", y = "Frecuencia", fill = "Leyenda") +
      coord_cartesian(xlim = rng)
  }
  
  observeEvent(input$brush_a, {
    brush <- input$brush_a
    if (!is.null(brush)) ranges$a <- c(brush$xmin, brush$xmax)
  })
  observeEvent(input$brush_b, {
    brush <- input$brush_b
    if (!is.null(brush)) ranges$b <- c(brush$xmin, brush$xmax)
  })
  
  # Generar los gráficos
  output$cmp_plot_a <- renderPlot({ make_plot_a(ranges$a) })
  output$cmp_plot_b <- renderPlot({ make_plot_b(ranges$b) })
}
