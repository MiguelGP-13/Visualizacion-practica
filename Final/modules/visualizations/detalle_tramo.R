# modules/visualizations/detalle_tramo.R

crear_detalle_tramo <- function(input, output, session, datos, click_info) {
  output$apartado2 <- renderUI({
    if (!is.null(click_info$tipo) && !is.null(click_info$tramo)) {
      fluidRow(
        h3(paste0("Detalle de ", click_info$tipo, " en el tramo ", click_info$tramo)),
        plotOutput("detalle_tramo")
      )
    }
  })

  output$detalle_tramo <- renderPlot({
    if (!is.null(click_info$tipo) && !is.null(click_info$tramo)) {
      datos_detalle <- datos$eventos_totales %>%
        filter(Evento == click_info$tipo & 
                 Edad >= (click_info$tramo - 2.5) & 
                 Edad < (click_info$tramo + 2.5))

      ggplot(datos_detalle, aes(x = Sexo, fill = Sexo)) +
        geom_bar(color = "black") +
        labs(title = paste0("Distribución de ", click_info$tipo, " en el tramo ", click_info$tramo),
             x = "Sexo", y = "Frecuencia") +
        theme_minimal()
    }
  })
}
