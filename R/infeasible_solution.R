# ---------------------------------------------------------------------------- #
# infeasible_solution
# ---------------------------------------------------------------------------- #

infeasible_solution <- function(params, solver, start.time, n, m, iter=0, M_tilde=1000000000){
  I_select = rep(T,n)
  m_select = m
  S_fix_hist = list()

  sol = rest_model(I_select, m_select, S_fix_hist, params, M_tilde, solver)

  cost = rowSums(sol$U)%*%params$C + sum(params$NVC) + rowSums(sol$S)%*%params$P

  if(sol$sol_result=="OPTIMAL"){
    sol<-list(
      iter=iter,
      time=Sys.time() - start.time,
      feas=sum(sol$mu>0) + sum(sol$MU>0),
      sol_result="INFEASIBLE",
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
      iter=iter,
      time=Sys.time() - start.time,
      sol_result="INFEASIBLE"
    )
  }
  return(sol)
}
