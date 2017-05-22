#-------------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------------

#' Server to run the Selection and Allocation (SaA) package on the shiny app
#'
#' @return shiny server.
#'
#' @import plotly
#' @import slam
#' @export
#' 
#' @examples
#' asa_server
asa_server <- function(input, output, session) {
  if(
    requireNamespace("plotly", quietly = TRUE) &
    requireNamespace("shinydashboard", quietly = TRUE) &
    requireNamespace("shiny", quietly = TRUE) &
    requireNamespace("shinyjs", quietly = TRUE)
  ){

    #===========================================================================
    # Initialization: Boxes
    #===========================================================================
    shiny::observe({
      #-------------------------------------------------------------------------
      # Solve Time Box
      #-------------------------------------------------------------------------
      output$time <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Solve Time (sec)", c(), icon = shiny::icon("clock-o"),
          color = "yellow", fill = F
        )
      })

      #-------------------------------------------------------------------------
      # Contention Period Box
      #-------------------------------------------------------------------------
      output$period <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Contention Period", c(), icon = shiny::icon("check-square-o"),
          color = "green", fill = F
        )
      })

      #-------------------------------------------------------------------------
      # Cost Box
      #-------------------------------------------------------------------------
      output$cost <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
          "Cost", c(), icon = shiny::icon("euro"),
          color = "blue", fill = F
        )
      })
    })


    #===========================================================================
    # Load Datas
    #===========================================================================

    #---------------------------------------------------------------------------
    # Aircraft
    #---------------------------------------------------------------------------
    data.aero <- shiny::reactive({
      aero.inFile <- input$aerofile
      if (is.null(aero.inFile)) {
        return(NULL)
      }
      aero <- read.csv(aero.inFile$datapath,
                       header = T,
                       sep = ";",
                       stringsAsFactors =FALSE)
      return(list(aero=aero))
    })

    output$aero.table <- DT::renderDataTable({
      # require that data() is available
      if(!is.null(data.aero())){
        DT::datatable(data.aero()$aero)
      }
    })

    #---------------------------------------------------------------------------
    # Fire
    #---------------------------------------------------------------------------
    data.fire <- shiny::reactive({

      fire.inFile <- input$fire.file
      if (is.null(fire.inFile)) {
        return(NULL)
      }
      fire <- read.csv(fire.inFile$datapath,
                       header = T,
                       sep = ";",
                       stringsAsFactors =FALSE)
      return(list(fire=fire))
    })

    output$fire.table <- DT::renderDataTable({
      # require that data() is available
      if(!is.null(data.fire())){
        DT::datatable(data.fire()$fire)
      }
    })


    #===========================================================================
    # Solve button
    #===========================================================================

    #---------------------------------------------------------------------------
    # Initialization
    #---------------------------------------------------------------------------

    shiny::observe({
      shinyjs::disable("report")
    })

    observe({
      # If no datas can't solve
      if(is.null(input$fire.file) || is.null(input$aerofile)){
        shinyjs::disable("solve")
      }else{
        shinyjs::enable("solve")
      }
    })

    #---------------------------------------------------------------------------
    # Push
    #---------------------------------------------------------------------------
    observeEvent(input$solve, {

      #-------------------------------------------------------------------------
      # Solve status initialization: solving
      #-------------------------------------------------------------------------
      shinyjs::hide(id="status_opt")
      shinyjs::hide(id="status_inf")
      shinyjs::show(id = "status_load")

      #-------------------------------------------------------------------------
      # Load law
      #-------------------------------------------------------------------------
      normativa <- list(PeriodTime = input$PeriodTime,
                       FP_min = input$FP_min,
                       RP_min = input$RP_min,
                       DFP_min = input$DFP_min,
                       FBRP_min = input$FBRP_min,
                       nMax = input$nMax
      )

      #-------------------------------------------------------------------------
      # get data
      #-------------------------------------------------------------------------
      problem.info <- get_data(data.aero()$aero,
                              data.fire()$fire,
                              input)

      #-------------------------------------------------------------------------
      # Solve model
      #-------------------------------------------------------------------------
      if(input$method=="exact"){
        #-----------------------------------------------------------------------
        # Exact algorithm
        #-----------------------------------------------------------------------
        results <- asa::asa(problem.info,
                      M_prime=input$M,
                      solver=input$solver)
      }else if(input$method=="heuristic"){
        #-----------------------------------------------------------------------
        # Heuristic algorithm
        #-----------------------------------------------------------------------
        results <- asa::asa_h(problem.info,
                        M_prime=input$M,
                        niters=input$IterMax,
                        solver=input$solver)
      }

      #-------------------------------------------------------------------------
      # Update solve status
      #-------------------------------------------------------------------------
      shinyjs::hide(id = "status_load")
      if(results$sol_result=="OPTIMAL"){
        shinyjs::show(id = "status_opt")
        shinyjs::enable("report")
        results$contper <- max(which(results$Y==1))
      }else{
        shinyjs::show(id="status_inf")
        shinyjs::enable("report")
        results$contper <- "Unknown"
      }


      #=========================================================================
      # Scheduling
      #-------------------------------------------------------------------------

      # Get data
      #-------------------------------------------------------------------------
      WRF <- asa::data.scheduling(results)

      # Data selection
      #-------------------------------------------------------------------------
      updateCheckboxGroupInput(session,
        "WRF.rows",
        label = "Selected aircraft:",
        choices = row.names(WRF),
        selected = row.names(WRF)
      )

      updateSliderInput(session,
        "Period.slider",
        label = "Period range:",
        min = 1,
        max = dim(WRF)[2],
        value = c(1, min(5, dim(WRF)[2]))
      )

      # Data output
      #-------------------------------------------------------------------------
      output$WRF.data <- DT::renderDataTable({
        DT::datatable(data.scheduling.selection(WRF, input))
      })

      # Graph output
      #-------------------------------------------------------------------------
      output$WRF.plot <- renderPlotly({
        asa::plotscheduling(WRF)
      })


      #=========================================================================
      # Contention
      #-------------------------------------------------------------------------

      # Get data
      #-------------------------------------------------------------------------
      contention.data <- data.contention(problem.info, results)

      # Data output
      #-------------------------------------------------------------------------
      output$contention.data <- DT::renderDataTable({
        DT::datatable(contention.data)
      })

      # Graph output
      #-------------------------------------------------------------------------
      output$contention.plot <- renderPlotly({
        asa::plotcontention(contention.data)
      })


      #=========================================================================
      # Number of Aircraft
      #-------------------------------------------------------------------------

      # Get data
      #-------------------------------------------------------------------------
      num.aircraft.data <- asa::data.num.aircraft(results)

      # Data output
      #-------------------------------------------------------------------------
      output$num.aircraft.data <- DT::renderDataTable({
        DT::datatable(num.aircraft.data)
      })

      # Graph output
      #-------------------------------------------------------------------------
      output$num.aircraft.plot <- renderPlotly({
        asa::plotnumaircraft(num.aircraft.data)
      })


      #=========================================================================
      # Yield
      #-------------------------------------------------------------------------

      # Get data
      #-------------------------------------------------------------------------
      yield.data <- data.yield(problem.info, results)

      # Data output
      #-------------------------------------------------------------------------
      output$yield.data <- DT::renderDataTable({
        DT::datatable(yield.data)
      })

      # Graph output
      #-------------------------------------------------------------------------
      output$yield.plot <- renderPlotly({
        asa::plotyield(yield.data)
      })


      #=========================================================================
      # Report
      #-------------------------------------------------------------------------
      output$report <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = "report.html",
        content = function(file) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)

          # Set up parameters to pass to Rmd document
          params <- list(info=problem.info, results=results, input=input)

          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )


      #=========================================================================
      # Boxes
      #=========================================================================

      #=========================================================================
      # OPTIMAL
      #-------------------------------------------------------------------------
      if(results$sol_result=="OPTIMAL"){
        #-----------------------------------------------------------------------
        # Cost
        #-----------------------------------------------------------------------
        output$cost <- renderInfoBox({
          infoBox(
            "Cost",
            round(results$cost, digits = 2),
            icon = icon("euro"),
            color = "blue",
            fill = F
          )
        })

        #-----------------------------------------------------------------------
        # Contention Period
        #-----------------------------------------------------------------------
        output$period <- renderInfoBox({
          infoBox(
            "Contention Period",
            results$contper,
            icon = icon("fire-extinguisher"),
            color = "green",
            fill = F
          )
        })

      #=========================================================================
      # INFEASIBLE
      #-------------------------------------------------------------------------
      }else{
        #-----------------------------------------------------------------------
        # Cost
        #-----------------------------------------------------------------------
        output$cost <- renderInfoBox({
          infoBox(
            "Cost",
            round(results$cost, digits = 2),
            icon = icon("euro"),
            color = "red",
            fill = F
          )
        })

        #-----------------------------------------------------------------------
        # Contention Period
        #-----------------------------------------------------------------------
        output$period <- renderInfoBox({
          infoBox(
            "Contention Period",
            results$contper,
            icon = icon("fire-extinguisher"),
            color = "red",
            fill = F
          )
        })
      }

      #=========================================================================
      # ALWAYS
      #-------------------------------------------------------------------------

      #-------------------------------------------------------------------------
      # Solve Time
      #-------------------------------------------------------------------------
      output$time <- renderInfoBox({
        infoBox(
          "Solve Time (sec)",
          round(results$time, digits = 2),
          icon = icon("clock-o"),
          color = "yellow",
          fill = F
        )
      })

    })
  }
}
