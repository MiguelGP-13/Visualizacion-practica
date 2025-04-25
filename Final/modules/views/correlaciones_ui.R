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
                   selectInput("vars_x_sangrado", "Eje X", 
                               choices = names(datos$eventos_sangrado),
                               multiple = TRUE),
                   selectInput("vars_y_sangrado", "Eje Y",
                               choices = names(datos$eventos_sangrado),
                               multiple = TRUE)
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
                   selectInput("vars_x_trombotico", "Eje X", 
                               choices = names(datos$eventos_tromboticos),
                               multiple = TRUE),
                   selectInput("vars_y_trombotico", "Eje Y",
                               choices = names(datos$eventos_tromboticos),
                               multiple = TRUE)
                 ),
                 mainPanel(
                   plotOutput("correlaciones_plot_trombotico", height = "600px")
                 )
               )
      )
    )
  )
}
