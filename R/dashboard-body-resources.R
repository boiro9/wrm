# resources_body --------------------------------------------------------------
resources_body <- function(){
  shinydashboard::tabItem(
    tabName = "Aircraft",
    shinydashboard::box(
      title = shiny::tags$b("Resources information"),
      status = "primary", solidHeader = TRUE,
      shiny::fileInput('resources_file', 'Choose the resources.csv file', 
                       accept=c('.csv')),
      shiny::tags$hr(),
      shiny::br(), 
      rhandsontable::rHandsontableOutput("resources_table"),
      width = "100%"
    )
  )
}
# --------------------------------------------------------------------------- #
