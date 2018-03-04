#' Solve button
#'
#' @param input 
#' @param output 
#' @param session 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
solve_button <- function(input, output, session, values){
  #--------------------------------------------------------------------------
  # Initialization
  #--------------------------------------------------------------------------
  
  shiny::observe({
    shinyjs::disable("report")
  })
  
  observe({
    # If no datas can't solve
    print(class(values[["resources"]]()[["resources"]]))
    print(class(values[["fire"]]()[["fire"]]))
    if(is.null(values[["resources"]]()) || is.null(values[["fire"]]())){
      shinyjs::disable("solve")
    }else{
      if(is.list(values[["resources"]]()) & is.list(values[["fire"]]())){
        shinyjs::enable("solve")
      }
    }
  })
  
  #---------------------------------------------------------------------------
  # Push
  #---------------------------------------------------------------------------
  observeEvent(input$solve, {
    
    #-------------------------------------------------------------------------
    # Solve status initialization: solving
    #-------------------------------------------------------------------------
    shinyjs::hide(id = "status_opt")
    shinyjs::hide(id = "status_inf")
    shinyjs::show(id = "status_load")
    #-------------------------------------------------------------------------
    # get data
    #-------------------------------------------------------------------------
    print(values[["fire"]]())
    problem.info <- WildfireResources::get_data(values[["resources"]]()[["resources"]],
                                                values[["fire"]]()[["fire"]],
                                                input)
  
    #-------------------------------------------------------------------------
    # Solve model
    #-------------------------------------------------------------------------
    results <- WildfireResources::wildfire_resources(problem.info,
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
    
    WRF <- WildfireResources::data.scheduling(results)
    
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
      DT::datatable(asa::data.scheduling.selection(WRF, input))
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$WRF.plot <- renderPlotly({
      WildfireResources::plotscheduling(WRF)
    })
    
    
    #========================================================================
    # Contention
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    contention.data <- WildfireResources::data.contention(problem.info, results)
    
    # Data output
    #------------------------------------------------------------------------
    output$contention.data <- DT::renderDataTable({
      DT::datatable(contention.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$contention.plot <- renderPlotly({
      WildfireResources::plotcontention(contention.data)
    })
    
    
    #========================================================================
    # Number of Aircraft
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    num.aircraft.data <- WildfireResources::data.num.aircraft(results)
    
    # Data output
    #------------------------------------------------------------------------
    output$num.aircraft.data <- DT::renderDataTable({
      DT::datatable(num.aircraft.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$num.aircraft.plot <- renderPlotly({
      WildfireResources::plotnumaircraft(num.aircraft.data)
    })
    
    
    #========================================================================
    # Performance
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    performance.data <- WildfireResources::data.performance(problem.info, 
                                                            results)
    
    # Data output
    #------------------------------------------------------------------------
    output$performance.data <- DT::renderDataTable({
      DT::datatable(performance.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$performance.plot <- renderPlotly({
      WildfireResources::plotperformance(performance.data)
    })
    
    
    #========================================================================
    # Report
    #------------------------------------------------------------------------
    
    output$report <- asa::report(problem.info, results, input)
    
    
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