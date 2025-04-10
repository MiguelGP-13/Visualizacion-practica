perfil_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("paciente"), "Seleccione un Paciente:", choices = unique(data$Paciente))
      ),
      mainPanel(
        tableOutput(ns("perfil_table"))
      )
    )
  )
}

perfil_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$perfil_table <- renderTable({
      create_individual_profile(data, input$paciente)
    })
  })
}
