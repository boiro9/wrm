# body ------------------------------------------------------------------------
body <- function(){
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      # Home
      home_body(),
      
      # Resource data
      resources_body(),
      
      # Fire
      fire_body(),
      
      # Results
      results_body()
    )
  )
}
# --------------------------------------------------------------------------- #


# home_body -------------------------------------------------------------------
home_body <- function(){
  shinydashboard::tabItem(
    tabName = "Home",
    shiny::h2("Wildfire Resources Management."),
    shiny::br(),
    "Determine optimal planning that includes the number and type of",
    "resources needed to extinguishing a forest fire is a difficult task",
    "In this app, a general integer programming model is programed",
    "which also includes the allocation of resources to different periods of",
    "time in a day embedded in extinguishing a fire, with the goal of meeting",
    "the Spanish regulations regarding non-neglect of fronts and periods of",
    "rest of the pilots. We consider two types to get a solution, the exact",
    "method and also an heuristic algorithm designed specifically for",
    "this problem which allows to obtain a quality solution quickly in",
    "real problems.",
    shiny::br(),
    shiny::br(),
    "For more information",
    shiny::tags$a(
      href="http://eio.usc.es/pub/mte/descargas/ProyectosFinMaster/Proyecto_1095.pdf", 
      "click here!"
    ),
    shiny::HTML(
      "<head>",
      "<script type='text/x-mathjax-config'>",
      "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});",
      "</script>",
      "<script type='text/javascript' async",
      "src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_CHTML'>",
      "</script>",
      "</head>"
    )
  )
}
# --------------------------------------------------------------------------- #


# resources_body --------------------------------------------------------------
resources_body <- function(){
  shinydashboard::tabItem(
    tabName = "Aircraft",
    box(
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


# fire_body -------------------------------------------------------------------
fire_body <- function(){
  shinydashboard::tabItem(
    tabName = "Fire",
    box(
      title = shiny::tags$b("Periods information"),
      status = "primary", solidHeader = TRUE,
      shiny::numericInput(
        "PeriodTime", "Minutes per period", value = 10
      ),
      width = "100%"
    ),
    box(
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


# results_body ----------------------------------------------------------------
results_body <- function(){
  shinydashboard::tabItem(
    tabName = "results",
    shiny::fluidRow(
      
      # Info Boxes: Cost, Contention_Period, Time
      # =========================================
      infoBoxOutput("cost"),
      infoBoxOutput("period"),
      infoBoxOutput("time"),
      
      # Scheduling
      # ==========
      shinydashboard::tabBox(
        width = 12,
        title =  shiny::h3("Scheduling"),
        id = "scheduling",
        
        # Plot
        # ----
        shiny::tabPanel(
          "Plot",
          plotly::plotlyOutput("WRF.plot", height = 300)
        ),
        
        # Data
        # ----
        shiny::tabPanel(
          "Data",
          shiny::fluidRow(
            
            # Aircraft selection
            shiny::column(
              6,
              shiny::checkboxGroupInput(
                "WRF.rows", "Selected aircraft:",
                c()
              )
            ),
            
            # Perio Selection
            shiny::column(
              6,
              shiny::sliderInput(
                "Period.slider", "Period range:",
                min = 1, max = 10, value = c(1, 6)
              )
            )
          ),
          
          shiny::tags$hr(),
          
          # Table
          shiny::fluidRow(
            shiny::column(width = 1), # to center
            shiny::column(width=10, DT::dataTableOutput("WRF.data")),
            shiny::column(width = 1) # to center
          )
        )
      ),
      
      # Contention
      # ==========
      shinydashboard::tabBox(
        width = 12,
        title =  shiny::h3("Contention"),
        id = "Contention",
        
        # Plot
        # ----
        shiny::tabPanel(
          "Plot",
          plotly::plotlyOutput("contention.plot", height = 300)
        ),
        
        # Data
        # ----
        shiny::tabPanel(
          "Data",
          shiny::fluidRow(
            shiny::column(width = 1), # to center
            shiny::column(width=10, DT::dataTableOutput("contention.data")),
            shiny::column(width = 1)  # to center
          )
        )
      ),
      
      # Number of Aircraft
      # ==================
      shinydashboard::tabBox(
        width = 12,
        title =  shiny::h3("Number of Aircraft"),
        id = "num.aircraft",
        
        # Plot
        # ----
        shiny::tabPanel(
          "Plot",
          plotly::plotlyOutput("num.aircraft.plot", height = 300)
        ),
        
        # Data
        # ----
        shiny::tabPanel(
          "Data",
          shiny::fluidRow(
            shiny::column(width = 1), # to center
            shiny::column(width=10, DT::dataTableOutput("num.aircraft.data")),
            shiny::column(width = 1)  # to center
          )
        )
      ),
      
      # Performance
      # ===========
      shinydashboard::tabBox(
        width = 12,
        title =  shiny::h3("Performance"),
        id = "performance",
        
        # Plot
        # ----
        shiny::tabPanel(
          "Plot",
          plotly::plotlyOutput("performance.plot", height = 300)
        ),
        
        # Data
        # ----
        shiny::tabPanel(
          "Data",
          shiny::fluidRow(
            shiny::column(width = 1), # to center
            shiny::column(width=10, DT::dataTableOutput("performance.data")),
            shiny::column(width = 1)  # to center
          )
        )
      )
    )
  )
}
# --------------------------------------------------------------------------- #