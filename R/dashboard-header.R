
# header ----------------------------------------------------------------------
header <- function(){
  shinydashboard::dashboardHeader(
    # Title
    title = title(),
    
    # Tasks
    tasks()
  )
}
# --------------------------------------------------------------------------- #


# title -----------------------------------------------------------------------
title <- function(){
  # Icones: leaf globe, stats tree-conifer tree-deciduous grain
  shiny::h3(shiny::icon("leaf"), "WRM") # Wildfire Resources Management
}
# --------------------------------------------------------------------------- #


# tasks -----------------------------------------------------------------------
tasks <- function(){
  shinydashboard::dropdownMenu(
    type = "tasks", badgeStatus = "success",
    shinydashboard::taskItem(value = 0, color = "red", 
                             "Coger unidades de tiempo"),
    shinydashboard::taskItem(value = 0, color = "red", 
                             "Save image (Rdata, JSON, YAML?)"),
    shinydashboard::taskItem(value = 0, color = "green", "Read csv header"),
    shinydashboard::taskItem(value = 50, color = "green", "Reports"),
    shinydashboard::taskItem(value = 20, color = "red", 
                             "Remove warnings package"),
    shinydashboard::taskItem(value = 70, color = "green", "Documentation"),
    shinydashboard::taskItem(value = 30, color = "green", 
                             "Imports (functions)"),
    shinydashboard::taskItem(value = 0, color = "aqua", "Help"),
    shinydashboard::taskItem(value = 25, color = "yellow", "Paper")
  )
}
# --------------------------------------------------------------------------- #