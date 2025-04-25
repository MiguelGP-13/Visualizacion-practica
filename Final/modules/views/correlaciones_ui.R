correlaciones_ui <- function(datos) {
  fluidPage(
    titlePanel("Correlaciones por tipo de evento"),
    tabsetPanel(
      tabPanel("Sangrado",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   tags$style(HTML("
                     .control-label {
                       font-size: 14px;
                     }
                     .selectize-input {
                       min-height: 35px;
                     }
                   ")),
                   h4("Variables para Sangrado"),
                   selectInput("var_x_sangrado", "Variable en eje X", 
                               choices = c(
                                 "ANTICOAGULANT_STRING",
                                 "ANTIPLATELET_STRING",
                                 "Caracterización de la hemorragia",
                                 "Procedimientos terapéuticos",
                                 "Descenso de hemoglobina",
                                 "Gravedad de la hemorragia (TIMI)",
                                 "Tipo de sangrado",
                                 "¿El paciente ha subido una trasfusión?"
                               ),
                               multiple = FALSE),
                   selectInput("var_y_sangrado", "Variable en eje Y",
                               choices = c(
                                 "ANTICOAGULANT_STRING",
                                 "ANTIPLATELET_STRING",
                                 "Caracterización de la hemorragia",
                                 "Procedimientos terapéuticos",
                                 "Descenso de hemoglobina",
                                 "Gravedad de la hemorragia (TIMI)",
                                 "Tipo de sangrado",
                                 "¿El paciente ha subido una trasfusión?"
                               ),
                               multiple = FALSE)
                 ),
                 mainPanel(
                   plotOutput("correlaciones_plot_sangrado", height = "600px")
                 )
               )
      ),
      tabPanel("Trombótico",
               sidebarLayout(
                 sidebarPanel(
                   width = 3,
                   tags$style(HTML("
                     .control-label {
                       font-size: 14px;
                     }
                     .selectize-input {
                       min-height: 35px;
                     }
                   ")),
                   h4("Variables para Trombótico"),
                   selectInput("var_x_trombotico", "Variable en eje X", 
                               choices = c(
                                 "ANTICOAGULANT_STRING",
                                 "ANTIPLATELET_STRING",
                                 "TYPE_THROMBOTIC_PRE",
                                 "Tipo de evento trombótico",
                                 "Tipo de invervención"
                               ),
                               multiple = FALSE),
                   selectInput("var_y_trombotico", "Variable en eje Y",
                               choices = c(
                                 "ANTICOAGULANT_STRING",
                                 "ANTIPLATELET_STRING",
                                 "TYPE_THROMBOTIC_PRE",
                                 "Tipo de evento trombótico",
                                 "Tipo de invervención"
                               ),
                               multiple = FALSE)
                 ),
                 mainPanel(
                   plotOutput("correlaciones_plot_trombotico", height = "600px")
                 )
               )
      )
    )
  )
}
