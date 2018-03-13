# body ------------------------------------------------------------------------
body <- function(){
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    shinydashboard::tabItems(
      # Home
      home_body(),
      
      # Resource data
      resources_body(),
      
      # Fire
      fire_body(),
      
      # Results
      results_body()
    )
  )
}
# --------------------------------------------------------------------------- #
