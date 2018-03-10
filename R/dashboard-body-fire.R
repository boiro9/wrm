# fire_body -------------------------------------------------------------------
fire_body <- function(){
  shinydashboard::tabItem(
    tabName = "Fire",
    shinydashboard::box(
      title = shiny::tags$b("Periods information"),
      status = "primary", solidHeader = TRUE,
      shiny::numericInput(
        "PeriodTime", "Minutes per period", value = 10
      ),
      width = "100%"
    ),
    shinydashboard::box(
      title = shiny::tags$b("Wildfire information"),
      status = "primary", solidHeader = TRUE,
      shiny::fileInput('fire_file', 'Choose the .csv file', accept=c('.csv')),
      shiny::tags$hr(),
      rhandsontable::rHandsontableOutput("fire_table"),
      shiny::br(),
      shiny::wellPanel(
        shiny::h3("Add a column"),
        shiny::div(class='row', 
                   shiny::div(class="col-sm-5", 
                              shiny::uiOutput("ui_newcolname"),
                              shiny::actionButton("addcolumn", "Add")),
                   shiny::div(class="col-sm-4", 
                              shiny::radioButtons("newcolumntype", "Type", 
                                                  c("integer", "double", 
                                                    "character"))),
                   shiny::div(class="col-sm-3")
        )
      ),
      width = "100%"
    )
  )
}
# --------------------------------------------------------------------------- #