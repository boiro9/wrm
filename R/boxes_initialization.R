

#' Initialization.
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
initialization <- function(input, output, session){
  shiny::observe({
    #--------------------------------------------------------------------------
    # Solve Time Box
    #--------------------------------------------------------------------------
    output$time <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Solve Time (sec)", c(), icon = shiny::icon("clock-o"),
        color = "yellow", fill = F
      )
    })
    
    #--------------------------------------------------------------------------
    # Contention Period Box
    #--------------------------------------------------------------------------
    output$period <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Contention Period", c(), icon = shiny::icon("check-square-o"),
        color = "green", fill = F
      )
    })
    
    #--------------------------------------------------------------------------
    # Cost Box
    #--------------------------------------------------------------------------
    output$cost <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        "Cost", c(), icon = shiny::icon("euro"),
        color = "blue", fill = F
      )
    })
  })
}
# --------------------------------------------------------------------------- #