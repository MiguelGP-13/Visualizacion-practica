# modules/visualizations/grafico_eventos.R

crear_grafico_descripcion_pacientes <- function(input, output, session, data, ranges, click_info) {
  datos <- data$full_pacientes
  columnas_mostrar <- c(
    "Paciente",
    "Edad",
    "Sexo",
    "Realiza control regular de la presión?",
    "Minutos semanales de actividad aeróbica",
    "Dieta Mediterránea",
    "Antecedentes familiares de cardiopatía isquémica precoz (menor de 50 en varones y menor de 55 en mujeres)",
    "Riesgo de caída",
    "Caidas previas conocidas",
    "Toma algún medicamento presente en esta lista",
    "Deficit sensorial",
    "Estado mental",
    "Deambulación alterada",
    "Tabaquismo",
    "Consumo diario de alcohol",
    "Hipertensión arterial",
    "Max valor P. Sistólica",
    "Hypertensión controlada",
    "Medicamento Antihipertensivo",
    "Valvulopatía",
    "Enfermedad coronaria",
    "Tipo",
    "Cuantos?",
    "Tipo de intervencion",
    "Enfermedad aórtica",
    "Enfermedad carótidea",
    "Enfermedad vascular periférica",
    "Arritmias",
    "Arritmia auricular",
    "Taquicardia ventricular",
    "Taquicardia paroxística supraventicular",
    "Muerte súbita cardíaca",
    "Ablación",
    "Insuficiencia cardíaca",
    "Etiología",
    "NYHA",
    "Fracción de Eyección del Ventrículo Izquierdo",
    "Hipertensión pulmonar",
    "Presencia de crepitantes",
    "Ingurgitación de vena yugular",
    "Transplante cardíaco",
    "Resincronización",
    "Miocardiopatía"
    )
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

    # Reactive value to track if a click has been handled
    click_handled <- reactiveVal(FALSE)

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
                                text = paste("Grupo:", Grupo_Factores, "<br>", input$enfermedades, ":", !!sym(input$enfermedades), "<br>n:", n),
                                customdata = "heatmap")) +
        geom_tile() +
        geom_text(aes(label = if (input$porcentajes) paste0(round(porcentaje, 1), "%\n(", n, ")") else n), color = "black") +
        scale_fill_gradient(low = "white", high = "blue") +
        labs(x = "Combinación de Factores", y = input$enfermedades, fill = fill) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

        ggplotly(gg, tooltip = "text", customdata = ~customdata) %>%
        event_register("plotly_click")
    })

    output$boxplot <- renderPlotly({
        datos_combinados <- crear_combinacion_factores() %>%
        drop_na(`Max valor P. Sistólica`)  # Ensure no NA values in the y-axis variable

        p <- ggplot(datos_combinados, aes(x = Grupo_Factores, y = `Max valor P. Sistólica`, fill = Grupo_Factores,
                                        text = paste("Grupo:", Grupo_Factores, "<br>Presión:", `Max valor P. Sistólica`),
                                        customdata = "boxplot")) +
        geom_boxplot() +
        labs(x = "Grupo de Factores", y = "Max valor P. Sistólica") +
        theme_minimal()

        ggplotly(p, tooltip = "text", customdata = ~customdata) %>%
        event_register("plotly_click")
    })

    output$bubblePlot <- renderPlotly({
        datos_plot <- datos %>%
        group_by(Minutos_intervalo, !!sym(input$enfermedades)) %>%
        summarise(cantidad = n(), .groups = "drop")

        g <- ggplot(datos_plot, aes(x = Minutos_intervalo, y = !!sym(input$enfermedades), size = cantidad ,
                                    text = paste("Minutos:", Minutos_intervalo, "<br>Enfermedad:", !!sym(input$enfermedades), "<br>n:", cantidad),
                                    customdata = "bubble")) +
        geom_point(alpha = 0.6, color = "steelblue")+
        scale_size(range = c(3, 20)) +
        theme_minimal()

        ggplotly(g, tooltip = "text", customdata = ~customdata) %>%
        event_register("plotly_click")
    })

    observeEvent(event_data("plotly_click"), {
        click <- event_data("plotly_click")
        print(click)

        if (!is.null(click)) {
        plot_type <- unique(click$customdata)

        if (length(plot_type) == 1) {
            if (plot_type == "heatmap") {
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

            } else if (plot_type == "boxplot") {
            grupo_idx <- as.numeric(head(click,1)$x)
            print(grupo_idx)
            grupo_nombres <- niveles_grupos()
            grupo_real <- grupo_nombres[grupo_idx]
            print(grupo_real)

            datos_con_grupo <- crear_combinacion_factores()

            datos_filtrados <- datos_con_grupo %>%
                filter(Grupo_Factores == grupo_real)

            showModal(modalDialog(
                title = paste("Detalles para:", grupo_real),
                DT::dataTableOutput("tabla_detalles"),
                easyClose = TRUE,
                size = "l"
            ))

            output$tabla_detalles <- DT::renderDataTable({
                DT::datatable(datos_filtrados[, columnas_mostrar, drop = FALSE],
                            options = list(pageLength = 10, scrollX = TRUE))
            })

            } else if (plot_type == "bubble") {
            minutos_intervalo <- click$x
            enfermedad <- input$enfermedades
            yvalor <- as.character(click$y)

            # Convertir el valor de yvalor de acuerdo a la codificación correcta
            if (yvalor %in% c("1", "2")) {
                yvalor <- ifelse(yvalor == "1", "NO", "YES")
            }

            datos_filtrados <- datos %>%
                filter(Minutos_intervalo == minutos_intervalo) %>%
                filter(as.character(.data[[enfermedad]]) == yvalor)

            showModal(modalDialog(
                title = paste("Detalles para:", minutos_intervalo, "|", enfermedad, "=", yvalor),
                DT::dataTableOutput("tabla_detalles"),
                easyClose = TRUE,
                size = "l"
            ))

            output$tabla_detalles <- DT::renderDataTable({
                DT::datatable(datos_filtrados[, columnas_mostrar, drop = FALSE],
                            options = list(pageLength = 10, scrollX = TRUE))
            })
            }
        }
        }
    })

    # Reset click_handled when the modal is closed
    observeEvent(input$tabla_detalles_cell_clicked, {
        click_handled(FALSE)
    })

}
