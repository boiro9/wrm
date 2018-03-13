# results_body ----------------------------------------------------------------
results_body <- function(){
  shinydashboard::tabItem(
    tabName = "results",
    shiny::fluidRow(
      
      # Info Boxes: Cost, Contention_Period, Time
      # =========================================
      shinydashboard::infoBoxOutput("cost"),
      shinydashboard::infoBoxOutput("period"),
      shinydashboard::infoBoxOutput("time"),
      
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
            
            # Resources selection
            shiny::column(
              6,
              shiny::checkboxGroupInput(
                "WRF.rows", "Selected resources:",
                c()
              )
            ),
            
            # Period Selection
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
      
      # Number of Resources
      # ==================
      shinydashboard::tabBox(
        width = 12,
        title =  shiny::h3("Number of Resources"),
        id = "num.resources",
        
        # Plot
        # ----
        shiny::tabPanel(
          "Plot",
          plotly::plotlyOutput("num.resources.plot", height = 300)
        ),
        
        # Data
        # ----
        shiny::tabPanel(
          "Data",
          shiny::fluidRow(
            shiny::column(width = 1), # to center
            shiny::column(width=10, DT::dataTableOutput("num.resources.data")),
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