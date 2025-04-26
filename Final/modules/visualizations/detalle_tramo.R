crear_detalle_tramo <- function(input, output, session, datos) {
  
  datos_pacientes <- datos$full_pacientes
  datos_eventos <- datos$eventos_totales
  
  datos_combinados <- reactive({
    req(input$enfermedades_detalle)
    
    pacientes_filtrados <- datos_pacientes %>%
      filter(Edad >= input$edad_detalle[1],
             Edad <= input$edad_detalle[2],
             Sexo == input$sexo_detalle | input$sexo_detalle == "Todos")
    
    eventos_filtrados <- datos_eventos %>%
      filter(Paciente %in% pacientes_filtrados$Paciente)
    
    combinados <- pacientes_filtrados %>%
      select(Paciente, all_of(input$enfermedades_detalle)) %>%
      pivot_longer(-Paciente, names_to = "Enfermedad", values_to = "Presencia") %>%
      left_join(eventos_filtrados, by = "Paciente")
    
    combinados
  })
  
  observeEvent(input$actualizar_detalle, {
    output$heatmap_tramo <- renderPlotly({
      df <- datos_combinados() %>%
        group_by(Enfermedad, Evento) %>%
        summarise(Casos = n(), .groups = "drop")
      
      gg <- ggplot(df, aes(x = Enfermedad, y = Evento, fill = Casos,
                           text = paste("Casos:", Casos))) +
        geom_tile() +
        geom_text(aes(label = Casos)) +
        scale_fill_gradient(low = "white", high = "red") +
        theme_minimal()
      
      ggplotly(gg, tooltip = "text")
    })
    
    output$tabla_detalle_eventos <- renderDT({
      datatable(datos_combinados(),
                options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}
