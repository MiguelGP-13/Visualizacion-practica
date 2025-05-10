comparacion_eventos_ui <- function(datos) {
  fluidPage(
    titlePanel("Comparación de Tramos"),
    
    # Slider Edad común
    fluidRow(
      column(12, 
             sliderInput("cmp_edad", "Edad:",
                         min = min(datos$pacientes$Edad),
                         max = max(datos$pacientes$Edad),
                         value = c(min(datos$pacientes$Edad), max(datos$pacientes$Edad)))
      )
    ),
    
    # Primera fila: Filtros A + Gráfico A
    fluidRow(
      column(3,
             h4("Filtros Histograma A"),
             selectInput("cmp_tipo_evento_a", "Tipo de Evento:", choices = c("Todos", "Sangrado", "Trombotico")),
             selectInput("cmp_sexo_a", "Sexo:", choices = c("Todos", unique(datos$pacientes$Sexo))),
             conditionalPanel(
               condition = "input.cmp_tipo_evento_a == 'Sangrado'",
               checkboxGroupInput("cmp_gravedad_a", "Gravedad (TIMI):",
                                  choices = unique(na.omit(datos$eventos_sangrado$`Gravedad de la hemorragia (TIMI)`)))
             ),
             conditionalPanel(
               condition = "input.cmp_tipo_evento_a == 'Trombotico'",
               checkboxGroupInput("cmp_trombo_a", "Tipo Trombotico:",
                                  choices = unique(na.omit(datos$eventos_tromboticos$`Tipo de evento trombótico`)))
             )
      ),
      column(9,
             h3("Histograma A"),
             plotOutput("cmp_plot_a", height = "300px",
                        brush = brushOpts(id = "brush_a", resetOnNew = TRUE))
      )
    ),
    
    # Segunda fila: Filtros B + Gráfico B
    fluidRow(
      column(3,
             h4("Filtros Histograma B"),
             selectInput("cmp_tipo_evento_b", "Tipo de Evento:", choices = c("Todos", "Sangrado", "Trombotico")),
             selectInput("cmp_sexo_b", "Sexo:", choices = c("Todos", unique(datos$pacientes$Sexo))),
             conditionalPanel(
               condition = "input.cmp_tipo_evento_b == 'Sangrado'",
               checkboxGroupInput("cmp_gravedad_b", "Gravedad (TIMI):",
                                  choices = unique(na.omit(datos$eventos_sangrado$`Gravedad de la hemorragia (TIMI)`)))
             ),
             conditionalPanel(
               condition = "input.cmp_tipo_evento_b == 'Trombotico'",
               checkboxGroupInput("cmp_trombo_b", "Tipo Trombotico:",
                                  choices = unique(na.omit(datos$eventos_tromboticos$`Tipo de evento trombótico`)))
             )
      ),
      column(9,
             h3("Histograma B"),
             plotOutput("cmp_plot_b", height = "300px",
                        brush = brushOpts(id = "brush_b", resetOnNew = TRUE))
      )
    )
  )
}
