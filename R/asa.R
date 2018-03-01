#' Model to solve the Aircraft Selection and Allocation problem (ASA) for the containment of a wildfire.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param M_prime penalization for the breach of the minimum aircrafts on a wildfire.
#' @param method desired method, exact or heuristic. Options 'exact' or 'heuristic'.
#' @param niters number of iterations for heuristic approach.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#'
#' @return information about the selection and allocation of the aircrafts.
#' @export
#'
#' @examples
#' data <- asa::example_data()
#' asa::asa(data, solver="lpSolveAPI")
asa <- function(
  data, 
  M_prime=0, 
  method="exact", 
  niters=10, 
  solver="gurobi", 
  solver_params=list(TimeLimit=600, OutputFlag=0)
  ){
  
  if(method=="exact"){
    results <- WildfireResources::exact_model(
      data, solver=solver, solver_params=solver_params)
  }else if(method=="heuristic"){
    results <- asa::heuristic_model(data, M_prime, niters, solver, solver_params)
  }else{
    stop("Unknown method. Options: 'exact' or 'heuristic'.")
  }
  
  if(results$sol_result == "OPTIMAL"){
    return(list(st_model = results, nd_model = NULL))
  }else{
    inf_model_result <- asa::infeasible_solution(data, solver, M_prime)
  }
  
  return(list(st_model = results, nd_model = inf_model_result))
}