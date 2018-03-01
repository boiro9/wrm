#-------------------------------------------------------------------------------
# Dashboard
#-------------------------------------------------------------------------------

#' Dashboard to run the Aircraft Selection and Allocation (ASA) package on the shiny app
#'
#' @return shinydashboard ui.
#'
#' @import plotly
#' @import shinydashboard
#'
#' @export
#'
#' @examples
#' SaA_dashboard()
asa_dashboard <- function(){
  if(
    requireNamespace("plotly", quietly = TRUE) &
    requireNamespace("shiny", quietly = TRUE) &
    requireNamespace("shinyjs", quietly = TRUE) &
    requireNamespace("shinydashboard", quietly = TRUE)
  ){

  ui <- dashboardPage(

      #=========================================================================
      # HEADER
      #=========================================================================
      dashboardHeader(
        #-----------------------------------------------------------------------
        # Title
        #-----------------------------------------------------------------------
        title = shiny::h3(shiny::icon("plane"), "ASA"),

        #-----------------------------------------------------------------------
        # Tasks
        #-----------------------------------------------------------------------
        dropdownMenu(
          type = "tasks", badgeStatus = "success",
          taskItem(value = 0, color = "red", "Añadir tiempo de retorno al destino final (desde el incendio)."),
          taskItem(value = 0, color = "red", "Añadir periodos mas periodos al final para que las aeroanves puedan hacer el vuelo de retorno."),
          taskItem(value = 0, color = "red", "Coger unidades de tiempo"),
          taskItem(value = 0, color = "red", "Save image (Rdata, JSON, YAML?)"),
          taskItem(value = 0, color = "red", "nMax parameter per period"),
          taskItem(value = 0, color = "green", "Read csv header"),
          taskItem(value = 100, color = "green", "Reports"),
          taskItem(value = 20, color = "red", "Remove warnings package"),
          taskItem(value = 70, color = "green", "Documentation"),
          taskItem(value = 30, color = "green", "Imports (functions)"),
          taskItem(value = 0, color = "green", "Simulations"),
          taskItem(value = 0, color = "aqua", "Help"),
          taskItem(value = 0, color = "aqua", "GitHub"),
          taskItem(value = 25, color = "yellow", "Paper")
          )
      ),



      #=========================================================================
      # Side bar
      #=========================================================================
      dashboardSidebar(

        sidebarMenu(
          #---------------------------------------------------------------------
          # Home
          #---------------------------------------------------------------------
          shinydashboard::menuItem(
            "Home", tabName = "Home", icon = shiny::icon("home"), selected = T
          ),
          #---------------------------------------------------------------------
          # Datas
          #---------------------------------------------------------------------
          shinydashboard::menuItem(
            "Data", tabName = "datas", icon = shiny::icon("database"),

            # Aircraft data
            #-------------------------------------------------------------------
            menuSubItem(
              "Aircraft",
              tabName = "Aircraft",
              icon = shiny::icon("plane")
            ),

            # Fire data
            #-------------------------------------------------------------------
            menuSubItem(
              "Fire",
              tabName = "Fire",
              icon = shiny::icon("fire")
            )
          ),

          #---------------------------------------------------------------------
          # Solver Options
          #---------------------------------------------------------------------
          shinydashboard::menuItem(
            "Options", tabName = "options", icon = shiny::icon("gear"),

            # Penalty
            #-------------------------------------------------------------------
            shiny::sliderInput(
              "M", "Penalty", 1000, 1000000, 1000
            ),

            # Solver
            #-------------------------------------------------------------------
            shiny:: radioButtons(
              "solver", "Solver",
              c(
                "gurobi" = "gurobi",
                "lpSolve" = "lpSolve",
                "Rsymphony" = "Rsymphony"
              ),
              inline = T
            ),

            # Method
            #-------------------------------------------------------------------
            shiny:: radioButtons(
              "method", "Method",
              c(
                "Exact" = "exact",
                "Heuristic" = "heuristic"
              ),
              inline = T,
              selected = "exact"
            ),

            # Maximum number of iterations
            #-------------------------------------------------------------------
            shiny::conditionalPanel(
              condition = "input.method == 'heuristic'",
              shiny::numericInput(
                "IterMax", "Maximum number of iterations", 10
              )
            )
          ),


          #---------------------------------------------------------------------
          # Status Icon
          #---------------------------------------------------------------------

          # Solving
          #---------------------------------------------------------------------
          shinyjs::hidden(
            shiny::absolutePanel(
              id="status_load" ,left="85%", top="auto",
              shiny::h4(shiny::icon("spinner", class = "fa-pulse"))
            )
          ),

          # Feasible/Optimal
          #---------------------------------------------------------------------
          shinyjs::hidden(
            shiny::absolutePanel(
              id="status_opt", left="85%", top="auto",
              shiny::h4(shiny::icon("check"))
            )
          ),

          # Infeasible/limit iterations
          #---------------------------------------------------------------------
          shinyjs::hidden(
            shiny::absolutePanel(
              id="status_inf", left="85%", top="auto",
              shiny::h4(shiny::icon("remove"))
            )
          ),

          #---------------------------------------------------------------------
          # Solve Button
          #---------------------------------------------------------------------
          shiny::tags$div(
            class="header", id="solve_div",
            shiny::absolutePanel(
              left="10%",
              shinydashboard::menuItem(
                tabName = "solve_optimal",
                shiny::actionButton(
                  "solve", "Solve", shiny::icon("calculator")
                )
              )
            ),
            shiny::br(),shiny::br() # empty lines
          ),


          #---------------------------------------------------------------------
          # Results
          #---------------------------------------------------------------------
          shinydashboard::menuItem(
            "Results", tabName = "results", icon = shiny::icon("briefcase")
          ),


          #---------------------------------------------------------------------
          # Report
          #---------------------------------------------------------------------
          shiny::tags$div(
            shiny::absolutePanel(
              bottom = "5%",
              left="10%",

              shiny::downloadButton("report", "Generate report")
            )
          )

        )
      ),

      #=========================================================================
      # Body
      #=========================================================================
      shinydashboard::dashboardBody(
        shinyjs::useShinyjs(),
        shinydashboard::tabItems(
          #---------------------------------------------------------------------
          # Home
          #---------------------------------------------------------------------
          shinydashboard::tabItem(
            tabName = "Home",
            shiny::h2("Wildfire: Aircraft Selection and Allocation."),
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
            shiny::tags$a(href="http://eio.usc.es/pub/mte/descargas/ProyectosFinMaster/Proyecto_1095.pdf", "click here!"),
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
          ),

          #---------------------------------------------------------------------
          # Aircraft
          #---------------------------------------------------------------------
          shinydashboard::tabItem(
            tabName = "Aircraft",
            box(
              title = shiny::tags$b("Aircraft information"),
              status = "primary", solidHeader = TRUE,
              shiny::fileInput('aerofile', 'Choose the .csv file', accept=c('.csv')),
              shiny::tags$hr(),
              DT::dataTableOutput("aero.table"),
              width = "100%"
            )
          ),

          #---------------------------------------------------------------------
          # Fire
          #---------------------------------------------------------------------
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
              shiny::fileInput('fire.file', 'Choose the .csv file', accept=c('.csv')),
              shiny::tags$hr(),
              DT::dataTableOutput("fire.table"),
              width = "100%"
            )
          ),

          #---------------------------------------------------------------------
          # Results
          #---------------------------------------------------------------------
          shinydashboard::tabItem(
            tabName = "results",
            shiny::fluidRow(

              # Info Boxes: Cost, Contention_Period, Time
              #-----------------------------------------------------------------
              infoBoxOutput("cost"),
              infoBoxOutput("period"),
              infoBoxOutput("time"),

              # Scheduling
              #-----------------------------------------------------------------
              shinydashboard::tabBox(
                width = 12,
                title =  shiny::h3("Scheduling"),
                id = "scheduling",

                # Plot
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("WRF.plot", height = 300)
                ),

                # Data
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
              #-----------------------------------------------------------------
              shinydashboard::tabBox(
                width = 12,
                title =  shiny::h3("Contention"),
                id = "Contention",

                # Plot
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("contention.plot", height = 300)
                ),

                # Data
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
              #-----------------------------------------------------------------
              shinydashboard::tabBox(
                width = 12,
                title =  shiny::h3("Number of Aircraft"),
                id = "num.aircraft",

                # Plot
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("num.aircraft.plot", height = 300)
                ),

                # Data
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Data",
                  shiny::fluidRow(
                    shiny::column(width = 1), # to center
                    shiny::column(width=10, DT::dataTableOutput("num.aircraft.data")),
                    shiny::column(width = 1)  # to center
                  )
                )
              ),

              # Yield
              #-----------------------------------------------------------------
              shinydashboard::tabBox(
                width = 12,
                title =  shiny::h3("Yield"),
                id = "yield",

                # Plot
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Plot",
                  plotly::plotlyOutput("yield.plot", height = 300)
                ),

                # Data
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                shiny::tabPanel(
                  "Data",
                  shiny::fluidRow(
                    shiny::column(width = 1), # to center
                    shiny::column(width=10, DT::dataTableOutput("yield.data")),
                    shiny::column(width = 1)  # to center
                  )
                )
              )
            )
          )
        )
      )
    )
    return(ui)
  }
}

