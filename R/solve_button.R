solve_button <- function(input, output, session, data){
  #--------------------------------------------------------------------------
  # Initialization
  #--------------------------------------------------------------------------
  
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
    # get data
    #-------------------------------------------------------------------------
    problem.info <-get_data(data$data.aero()$aero,
                            data$data.fire()$fire,
                            input)
    
    #-------------------------------------------------------------------------
    # Solve model
    #-------------------------------------------------------------------------
    results <- asa::asa(problem.info,
                        M_prime=input$M,
                        input$method,
                        niters=input$IterMax,
                        solver=input$solver)
    
    #-------------------------------------------------------------------------
    # Update solve status
    #-------------------------------------------------------------------------
    shinyjs::hide(id = "status_load")
    if(results$st_model$sol_result=="OPTIMAL"){
      results <- results$st_model
      shinyjs::show(id = "status_opt")
      shinyjs::enable("report")
      
      results$contper <- max(which(results$Y==1))+1
    }else{
      results$nd_model$time <- results$st_model$time + results$nd_model$time
      results <- results$nd_model
      shinyjs::show(id="status_inf")
      shinyjs::enable("report")
      results$contper <- "Unknown"
    }
    
    #=========================================================================
    # Scheduling
    #-------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    
    WRF <- asa::data.scheduling(results)
    
    # Data selection
    #------------------------------------------------------------------------
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
    #------------------------------------------------------------------------
    output$WRF.data <- DT::renderDataTable({
      DT::datatable(data.scheduling.selection(WRF, input))
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$WRF.plot <- renderPlotly({
      asa::plotscheduling(WRF)
    })
    
    
    #========================================================================
    # Contention
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    contention.data <- data.contention(problem.info, results)
    
    # Data output
    #------------------------------------------------------------------------
    output$contention.data <- DT::renderDataTable({
      DT::datatable(contention.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$contention.plot <- renderPlotly({
      asa::plotcontention(contention.data)
    })
    
    
    #========================================================================
    # Number of Aircraft
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    num.aircraft.data <- asa::data.num.aircraft(results)
    
    # Data output
    #------------------------------------------------------------------------
    output$num.aircraft.data <- DT::renderDataTable({
      DT::datatable(num.aircraft.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$num.aircraft.plot <- renderPlotly({
      asa::plotnumaircraft(num.aircraft.data)
    })
    
    
    #========================================================================
    # Yield
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    yield.data <- data.yield(problem.info, results)
    
    # Data output
    #------------------------------------------------------------------------
    output$yield.data <- DT::renderDataTable({
      DT::datatable(yield.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$yield.plot <- renderPlotly({
      asa::plotyield(yield.data)
    })
    
    
    #========================================================================
    # Report
    #------------------------------------------------------------------------
    
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
    
    
    #========================================================================
    # Boxes
    #========================================================================
    
    #========================================================================
    # OPTIMAL
    #------------------------------------------------------------------------
    if(results$contper!="Unknown"){
      #----------------------------------------------------------------------
      # Cost
      #----------------------------------------------------------------------
      output$cost <- renderInfoBox({
        infoBox(
          "Cost",
          round(results$cost, digits = 2),
          icon = icon("euro"),
          color = "blue",
          fill = F
        )
      })
      
      #----------------------------------------------------------------------
      # Contention Period
      #----------------------------------------------------------------------
      output$period <- renderInfoBox({
        infoBox(
          "Contention Period",
          results$contper,
          icon = icon("fire-extinguisher"),
          color = "green",
          fill = F
        )
      })
      
      #========================================================================
      # INFEASIBLE
      #------------------------------------------------------------------------
    }else{
      #----------------------------------------------------------------------
      # Cost
      #----------------------------------------------------------------------
      output$cost <- renderInfoBox({
        infoBox(
          "Cost",
          round(results$cost, digits = 2),
          icon = icon("euro"),
          color = "red",
          fill = F
        )
      })
      
      #----------------------------------------------------------------------
      # Contention Period
      #----------------------------------------------------------------------
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
    
    #========================================================================
    # ALWAYS
    #------------------------------------------------------------------------
    
    #------------------------------------------------------------------------
    # Solve Time
    #------------------------------------------------------------------------
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