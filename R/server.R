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
    
    #==========================================================================
    # Initialization: Boxes
    #==========================================================================
    initialization(input, output, session)
    
    #==========================================================================
    # Load Datas
    #==========================================================================
    data <- load_data(input, output, session)

    #==========================================================================
    # Solve button
    #==========================================================================
    solve_button(input, output, session, data)

  }
}




