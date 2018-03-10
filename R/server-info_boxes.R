
#' Info boxes initialization.
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
box_init <- function(input, output, session){
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


# box_status ------------------------------------------------------------------
box_status <- function(results, input, output, session){
  if(results$contper!="Unknown"){
    box_opt(results, input, output, session)
  }else{
    box_inf(results, input, output, session)
  }
}
# --------------------------------------------------------------------------- #


# box_opt ---------------------------------------------------------------------
box_opt <- function(results, input, output, session){
  # Cost
  # ----
  output$cost <- renderInfoBox({
    infoBox(
      "Cost",
      round(results$cost, digits = 2),
      icon = icon("euro"),
      color = "blue",
      fill = F
    )
  })
  
  # Contention Period
  # -----------------
  output$period <- renderInfoBox({
    infoBox(
      "Contention Period",
      results$contper,
      icon = icon("fire-extinguisher"),
      color = "green",
      fill = F
    )
  })
  
  # Solve Time
  # ----------
  output$time <- renderInfoBox({
    infoBox(
      "Solve Time (sec)",
      round(results$time, digits = 2),
      icon = icon("clock-o"),
      color = "yellow",
      fill = F
    )
  })
}
# --------------------------------------------------------------------------- #


# box_inf ---------------------------------------------------------------------
box_inf <- function(results, input, output, session){
  # Cost
  # ----
  output$cost <- renderInfoBox({
    infoBox(
      "Cost",
      round(results$cost, digits = 2),
      icon = icon("euro"),
      color = "red",
      fill = F
    )
  })
  
  # Contention Period
  # -----------------
  output$period <- renderInfoBox({
    infoBox(
      "Contention Period",
      results$contper,
      icon = icon("fire-extinguisher"),
      color = "red",
      fill = F
    )
  })
  
  # Solve Time
  # ----------
  output$time <- renderInfoBox({
    infoBox(
      "Solve Time (sec)",
      round(results$time, digits = 2),
      icon = icon("clock-o"),
      color = "yellow",
      fill = F
    )
  })
}
# --------------------------------------------------------------------------- #