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
solve_button <- function(input, output, session){
  #--------------------------------------------------------------------------
  # Initialization
  #--------------------------------------------------------------------------
  
  shiny::observe({
    shinyjs::disable("report")
  })
  
  observe({
    # If no datas can't solve
    if(is.null(input$resources_table) || is.null(input$fire_table)){#is.null(values[["resources"]]()) || is.null(values[["fire"]]())){
      shinyjs::disable("solve")
    }else{
      if(any(is.na(rhandsontable::hot_to_r(input$resources_table))) | 
         any(is.na(rhandsontable::hot_to_r(input$fire_table)))){#is.list(values[["resources"]]()) & is.list(values[["fire"]]())){
        shinyjs::disable("solve")
      }else{
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
    problem.info <- WildfireResources::get_data(
      rhandsontable::hot_to_r(input$resources_table),
      rhandsontable::hot_to_r(input$fire_table),
      input$PeriodTime)
  
    #-------------------------------------------------------------------------
    # Solve model
    #-------------------------------------------------------------------------
    results <- WildfireResources::wildfire_resources(
      problem.info,
      solver=input$solver)
    
    #-------------------------------------------------------------------------
    # Update solve status
    #-------------------------------------------------------------------------
    shinyjs::hide(id = "status_load")
    if(results$st_model$sol_result=="OPTIMAL"){
      results <- results$st_model
      shinyjs::show(id = "status_opt")
      shinyjs::enable("report")
      
      results$contper <- max(which(results$Y==1), 0)+1
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
                             label = "Selected resources:",
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
      DT::datatable(wrm::data.scheduling.selection(WRF, input))
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$WRF.plot <- plotly::renderPlotly({
      p <- WildfireResources::plotscheduling(WRF)
      p$elementId <- NULL
      p
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
    output$contention.plot <- plotly::renderPlotly({
      p <- WildfireResources::plotcontention(contention.data)
      p$elementId <- NULL
      p
    })
    
    
    #========================================================================
    # Number of Resources
    #------------------------------------------------------------------------
    
    # Get data
    #------------------------------------------------------------------------
    num.resources.data <- WildfireResources::data_num_resources(results)
    
    # Data output
    #------------------------------------------------------------------------
    output$num.resources.data <- DT::renderDataTable({
      DT::datatable(num.resources.data)
    })
    
    # Graph output
    #------------------------------------------------------------------------
    output$num.resources.plot <- plotly::renderPlotly({
      p <- WildfireResources::plot_num_resources(num.resources.data)
      p$elementId <- NULL
      p
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
    output$performance.plot <- plotly::renderPlotly({
      p <- WildfireResources::plotperformance(performance.data)
      p$elementId <- NULL
      p
    })
    
    
    #========================================================================
    # Report
    #------------------------------------------------------------------------
    
    output$report <- wrm::report(problem.info, results, input)
    
    
    #========================================================================
    # Boxes
    #========================================================================
    box_status(results, input, output, session)
    
  })
}