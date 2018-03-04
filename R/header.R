
# header ----------------------------------------------------------------------
header <- function(){
  dashboardHeader(
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
  dropdownMenu(
    type = "tasks", badgeStatus = "success",
    taskItem(value = 0, color = "red", "Coger unidades de tiempo"),
    taskItem(value = 0, color = "red", "Save image (Rdata, JSON, YAML?)"),
    taskItem(value = 0, color = "green", "Read csv header"),
    taskItem(value = 50, color = "green", "Reports"),
    taskItem(value = 20, color = "red", "Remove warnings package"),
    taskItem(value = 70, color = "green", "Documentation"),
    taskItem(value = 30, color = "green", "Imports (functions)"),
    taskItem(value = 0, color = "aqua", "Help"),
    taskItem(value = 25, color = "yellow", "Paper")
  )
}
# --------------------------------------------------------------------------- #