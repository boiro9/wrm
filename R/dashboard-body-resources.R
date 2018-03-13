# resources_body --------------------------------------------------------------
resources_body <- function(){
  
  res_params_info <- data.frame(
    Parameter=c(
      "Name",
      "G",
      "ITW",
      "IOW",
      "A",
      "CWP",
      "CRP",
      "CUP",
      "BPR",
      "P",
      "C",
      "TRP",
      "WP",
      "RP",
      "UP"),
    Description=c(
      "Resource name.",
      "Resources group name.",
      "True if the resource is working in this wildfire.",
      "True if the resource is working in other wildfire.",
      "Total time that the resource needs to reach to the wildfire (min).",
      "Current total time since the last break of the resource (min).",
      "Current total resting time, if the resource is in a break (min).",
      "Current total usage time in the day (min).",
      "Maximum resource performance, i.e. kilometres cointained in a hour (km/h).",
      "Fix cost for use resource (€).",
      "Cost per hour for use the resource (€/h).",
      "Time that the resource needs to go to the resting point from the fire, and vice versa.",
      "Maximum time working without breaks (min).",
      "Necessary time of rest (min).",
      "Maximum daily time working (including rests, ...) (min)."
    )
  )
  
  shinydashboard::tabItem(
    tabName = "Resources",
    shinydashboard::box(
      title = shiny::tags$b("Resources information"),
      status = "primary", solidHeader = TRUE,
      width = "100%",
      shiny::fileInput('resources_file', 'Choose the resources.csv file', 
                       accept=c('.csv')),
      rhandsontable::rHandsontableOutput("resources_table"),
      shiny::tags$hr()
    ),
    shinydashboard::box(
      title = shiny::tags$b("Definition of the resources parameters"),
      #status = "primary", solidHeader = TRUE,
      width = "100%",
      shiny::div(shiny::HTML(table_info(res_params_info)))
    )
  )
}
# --------------------------------------------------------------------------- #
