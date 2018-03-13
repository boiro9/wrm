# fire_body -------------------------------------------------------------------
fire_body <- function(){
  
  fire_params_info <- data.frame(
    Parameter=c(
      "Period",
      "PER",
      "NVC",
      "EF.r",
      "nMin.g",
      "nMax.g"),
    Description=c(
      "Time period.",
      "Increment of the wildfire perimeter (km).",
      "Increase in the costs of the wildfire (affected area costs, 
      reforestation, urbane damages, ...) in the period.",
      "Efficiency of the resource r in the period (0 <= EF <= 1).",
      "Minimum number of resources of the group 'g' working on the wildfire in 
      the period.",
      "Maximum number of resources of the group 'g' working on the wildfire in 
      the period."
    )
  )
  
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
      width = "100%"
    ),
    shinydashboard::box(
      title = shiny::tags$b("Definition of the wildfire parameters"),
      #status = "primary", solidHeader = TRUE,
      width = "100%",
      shiny::div(shiny::HTML(table_info(fire_params_info)))
    )
  )
}
# --------------------------------------------------------------------------- #