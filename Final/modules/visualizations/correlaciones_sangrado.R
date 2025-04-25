crear_correlaciones_sangrado <- function(input, output, session, datos) {
  
  output$correlaciones_plot_sangrado <- renderPlot({
    
    req(input$var_x_sangrado, input$var_y_sangrado)
    
    df <- datos$eventos_sangrado %>%
      select(Paciente, all_of(c(input$var_x_sangrado, input$var_y_sangrado)))
    
    tabla <- table(df[[input$var_y_sangrado]], df[[input$var_x_sangrado]])
    tabla_df <- as.data.frame(tabla)
    colnames(tabla_df) <- c("VarY", "VarX", "Freq")
    
    ggplot(tabla_df, aes(x = VarX, y = VarY, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      geom_text(aes(label = Freq), color = "black", size = 4) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16)
      ) +
      labs(x = input$var_x_sangrado, y = input$var_y_sangrado, title = "Mapa de frecuencias (Sangrado)")
    
  })
}
