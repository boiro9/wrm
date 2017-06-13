# ---------------------------------------------------------------------------- #
# infeasible_solution
# ---------------------------------------------------------------------------- #
#' Function that solves the rest model problem when the ASA model solution is infeasible.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#' @param M_prime penalization for the breach of the minimum aircrafts on a wildfire.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#'
#' @return information about the selection and allocation of the aircrafts.
#'
#' @export
#'
#' @examples
#' data <- asa::example_data()
#' asa::infeasible_solution(data, solver="lpSolveAPI")
infeasible_solution <- function(data, solver, M_tilde=1000000000, solver_params=list(TimeLimit=600, OutputFlag=0)){
  start.time <- Sys.time()
  
  params <- get_model_params(data)
  
  n<-length(params$I)       # Number of aircrafts
  m<-length(params$Periods) # Number of periods
  
  I_select = rep(T,n)
  m_select = m
  S_fix_hist = list()

  sol = rest_model(I_select, m_select, S_fix_hist, params, M_tilde, solver, solver_params)
  
  if(sol$sol_result=="OPTIMAL"){
    cost=rowSums(sol$U)%*%params$C + sum(params$NVC) + rowSums(sol$S)%*%params$P
    sol<-list(
      model = "rest_model",
      time=difftime(Sys.time(), start.time, units="secs"),
      feas=sum(sol$mu>0) + sum(sol$MU>0),
      solver_result = sol$sol_result,
      obj=cost-sol$penalty,
      cost=cost,
      penalty=sol$penalty,
      Start=sol$S,
      End=sol$E,
      Rest=sol$R,
      Fly=sol$FL,
      Work=sol$W,
      Scheduling=sol$U,
      Selection=row_sums(sol$S),
      mu=sol$mu,
      MU=sol$MU,
      Y=rep(1,m+1)
    )
  }else{
    sol<-list(
      model = "rest_model",
      time=difftime(Sys.time(), start.time, units="secs"),
      sol_result="INFEASIBLE"
    )
  }
  return(sol)
}
