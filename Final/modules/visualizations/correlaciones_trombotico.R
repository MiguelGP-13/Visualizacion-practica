crear_correlaciones_trombotico <- function(input, output, session, datos) {
  
  output$correlaciones_plot_trombotico <- renderPlot({
    
    req(input$var_x_trombotico, input$var_y_trombotico)
    
    df <- datos$eventos_tromboticos %>%
      select(Paciente, all_of(c(input$var_x_trombotico, input$var_y_trombotico)))
    
    tabla <- table(df[[input$var_y_trombotico]], df[[input$var_x_trombotico]])
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
      labs(x = input$var_x_trombotico, y = input$var_y_trombotico, title = "Mapa de frecuencias (Trombótico)")
    
  })
}
