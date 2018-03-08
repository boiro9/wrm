#-------------------------------------------------------------------------------
# Dashboard
#-------------------------------------------------------------------------------

#' Dashboard to run the Aircraft Selection and Allocation (ASA) package on the shiny app
#'
#' @return shinydashboard ui.
#'
#' @export
#'
#' @examples
#' SaA_dashboard()
asa_dashboard <- function(){
  ui <- shinydashboard::dashboardPage(header(), sidebar(), body())
  
  return(ui)
}

