sankey_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("sankey_plot"))
  )
}

sankey_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$sankey_plot <- renderPlot({
      create_sankey(data)
    })
  })
}
