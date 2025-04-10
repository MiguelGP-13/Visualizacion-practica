bubble_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("bubble_plot"))
  )
}

bubble_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$bubble_plot <- renderPlot({
      create_bubble_chart(data)
    })
  })
}
