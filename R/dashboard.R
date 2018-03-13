#-------------------------------------------------------------------------------
# Dashboard
#-------------------------------------------------------------------------------

#' Dashboard to run the Wildfire Resources Management (WRM) package on the shiny app
#'
#' @return shinydashboard ui.
#'
#' @export
#'
#' @examples
#' wrm_dashboard()
wrm_dashboard <- function(){
  ui <- shinydashboard::dashboardPage(header(), sidebar(), body())
  
  return(ui)
}

