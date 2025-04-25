crear_correlaciones_sangrado <- function(input, output, session, datos) {
  
  output$correlaciones_plot_sangrado <- renderPlot({
    
    req(input$vars_x_sangrado, input$vars_y_sangrado)
    
    eventos <- datos$eventos_sangrado
    full_pacientes <- datos$full_pacientes
    
    eventos <- eventos %>%
      left_join(full_pacientes %>% select(Paciente, Minutos_intervalo, Tabaquismo_num), by = "Paciente")
    
    variables <- unique(c(input$vars_x_sangrado, input$vars_y_sangrado))
    if (length(variables) < 2) return(NULL)
    
    datos_corr <- eventos %>%
      select(all_of(variables)) %>%
      mutate(across(everything(), ~ as.numeric(as.factor(.))))
    
    cor_matrix <- cor(datos_corr, use = "pairwise.complete.obs", method = "spearman")
    
    cor_df <- as.data.frame(as.table(cor_matrix)) %>%
      filter(Var1 %in% input$vars_y_sangrado, Var2 %in% input$vars_x_sangrado)
    
    ggplot(cor_df, aes(x = Var2, y = Var1, fill = Freq)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Correlación") +
      geom_text(aes(label = round(Freq, 2)), color = "black", size = 4) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 16)
      ) +
      labs(x = "Variables X", y = "Variables Y", title = "Correlaciones en Sangrado")
    
  })
}
