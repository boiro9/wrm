
#' Load data
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @import rhandsontable
#' @return
#' @export
#'
#' @examples
load_data <- function(input, output, session){
  # Resources
  data_resources <- shiny::reactive({
    if (is.null(input$resources_file)) {
      return(NULL)
    }
    resources <- WildfireResources::load_resources_data(
      input$resources_file$datapath)
    return(list(resources=resources))
  })
  
  output$resources_table <- rhandsontable::renderRHandsontable({
    rhandsontable(data_resources()[["resources"]], stretchH = "all")
  })
  
  # Fire
  data_fire <- shiny::reactive({
    
    if (is.null(input$fire_file)) {
      return(NULL)
    }
    fire <- WildfireResources::load_fire_data(
      input$fire_file$datapath)
    return(list(fire=fire))
  })
  
  output$fire_table <- rhandsontable::renderRHandsontable({
    rhandsontable(data_fire()[["fire"]], stretchH = "all")
  })
  
  return(list(resources=data_resources, fire=data_fire))
}
# --------------------------------------------------------------------------- #

