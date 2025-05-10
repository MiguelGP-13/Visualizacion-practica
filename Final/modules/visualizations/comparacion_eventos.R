# modules/visualizations/comparacion_eventos.R
crear_comparacion_eventos <- function(input, output, session, datos, ranges) {
  observeEvent(input$cmp_edad, {
    # Restablecer el rango en base al rango de edad seleccionado
    ranges$x <- c(input$cmp_edad[1], input$cmp_edad[2])
  })
  
  output$cmp_plot_a <- renderPlot({
    # Filtrar los datos en función de los filtros seleccionados
    datos_filtrados <- datos$eventos_totales %>%
      filter((input$cmp_tipo_evento_a == "Todos" | Evento == input$cmp_tipo_evento_a) &
               (input$cmp_sexo_a == "Todos" | Sexo == input$cmp_sexo_a) &
               (Edad >= input$cmp_edad[1] & Edad <= input$cmp_edad[2]))
    
    # Aplicar los filtros específicos según el tipo de evento seleccionado
    if (input$cmp_tipo_evento_a == "Sangrado" && !is.null(input$cmp_gravedad_a) && length(input$cmp_gravedad_a) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Gravedad de la hemorragia (TIMI)` %in% input$cmp_gravedad_a)
    }
    if (input$cmp_tipo_evento_a == "Trombotico" && !is.null(input$cmp_trombo_a) && length(input$cmp_trombo_a) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Tipo de evento trombótico` %in% input$cmp_trombo_a)
    }
    
    # Grafico
    ggplot(datos_filtrados, aes(
      x = Edad, 
      fill = case_when(
        input$cmp_tipo_evento_a == "Todos" ~ Evento,
        Evento == "Sangrado" ~ as.character(`Gravedad de la hemorragia (TIMI)`),
        Evento == "Trombotico" ~ as.character(`Tipo de evento trombótico`),
        TRUE ~ Evento
      )
    )) +
      geom_histogram(breaks = seq(input$cmp_edad[1], input$cmp_edad[2], by = 5), position = "stack", color = "black") +
      labs(title = "Distribución de Eventos por Edad", x = "Edad", y = "Frecuencia", fill = "Leyenda") +
      theme_minimal() +
      coord_cartesian(xlim = ranges$x)  # Rango fijo en base a la selección de edad
  })
  
  output$cmp_plot_b <- renderPlot({
    # Filtrar los datos en función de los filtros seleccionados
    datos_filtrados <- datos$eventos_totales %>%
      filter((input$cmp_tipo_evento_b == "Todos" | Evento == input$cmp_tipo_evento_b) &
               (input$cmp_sexo_b == "Todos" | Sexo == input$cmp_sexo_b) &
               (Edad >= input$cmp_edad[1] & Edad <= input$cmp_edad[2]))
    
    # Aplicar los filtros específicos según el tipo de evento seleccionado
    if (input$cmp_tipo_evento_b == "Sangrado" && !is.null(input$cmp_gravedad_b) && length(input$cmp_gravedad_b) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Gravedad de la hemorragia (TIMI)` %in% input$cmp_gravedad_b)
    }
    if (input$cmp_tipo_evento_b == "Trombotico" && !is.null(input$cmp_trombo_b) && length(input$cmp_trombo_b) > 0) {
      datos_filtrados <- datos_filtrados %>%
        filter(`Tipo de evento trombótico` %in% input$cmp_trombo_b)
    }
    
    # Grafico
    ggplot(datos_filtrados, aes(
      x = Edad, 
      fill = case_when(
        input$cmp_tipo_evento_b == "Todos" ~ Evento,
        Evento == "Sangrado" ~ as.character(`Gravedad de la hemorragia (TIMI)`),
        Evento == "Trombotico" ~ as.character(`Tipo de evento trombótico`),
        TRUE ~ Evento
      )
    )) +
      geom_histogram(breaks = seq(input$cmp_edad[1], input$cmp_edad[2], by = 5), position = "stack", color = "black") +
      labs(title = "Distribución de Eventos por Edad", x = "Edad", y = "Frecuencia", fill = "Leyenda") +
      theme_minimal() +
      coord_cartesian(xlim = ranges$x)  # Rango fijo en base a la selección de edad
  })
}
