# ---------------------------------------------------------------------------- #
# Model 1: Contention_model
# ---------------------------------------------------------------------------- #
#' Contention model
#'
#' @description Solve the contention model.
#'
#' @param W_fix Work matrix.
#' @param S_fix Start matrix.
#' @param params model params.
#' @param M high value for the content_yes_no constraint.
#' @param M_prime penalty.
#' @param solver solver name. Options: 'gurobi', 'lpSolve' or 'Rsymphony'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#'
#' @return optimal values.
#'
#' @import lpSolveAPI
#' @import Rsymphony
#' @import slam
#'
#' @export
contention_model <- function(W_fix, S_fix, params, M, M_prime, solver="gurobi", solver_params=list(TimeLimit=600, OutputFlag=0)){
  #-----------------------------------------------------------------------------
  # Datos
  #-----------------------------------------------------------------------------
  I=params$I
  Periods=params$Periods
  DFP=params$DFP
  FBRP=params$FBRP
  A=params$A
  CTFP=params$CTFP
  C=params$C
  P=params$P
  SP=params$SP
  NVC=params$NVC
  PR=params$PR
  PER=params$PER
  nMax=params$nMax
  nMin=params$nMin

  n=length(I)
  m=length(Periods)

  #-----------------------------------------------------------------------------
  # Modelo matemático 1
  #-----------------------------------------------------------------------------
  # Orden de las variables de decision:
  # E[i,t] : 0*(n*m)+t+(i-1)*m
  # W[i,t] : 1*(n*m)+t+(i-1)*m
  # U[i,t] : 2*(n*m)+t+(i-1)*m
  #   Z[i] : 3*(n*m)+i
  #  mu[t] : 3*(n*m)+n+t
  #  MU[t] : 3*(n*m)+n+m+t
  # Y[t-1] : 3*(n*m)+n+2*m+t
  #   Y[m] : 3*(n*m)+n+3*m+1

  n_var<-3*(n*m)+n+3*m+1                     # numero de variables
  n_cons<-(n*m)+(n*m)+(n*m)+1+1+m+n+n+n+m+m  # numero de restricciones

  # Type
  type = c(rep("B", n*m),  #  E[i,t]
            rep("B", n*m),  #  W[i,t]
            rep("B", n*m),  #  U[i,t]
            rep("B", n)  ,  #  Z[i]
            rep("C", m)  ,  # mu[t]
            rep("C", m)  ,  # MU[t]
            rep("B", m)  ,  #  Y[t-1]
            rep("B", 1)  )  #  Y[m]

  # Objective function
  cost <- numeric(n_var)
  penalization <- numeric(n_var)

  # Constraints
  constr_W <- matrix(0, nrow = n*m, ncol = n_var)
  sense_W <- rep("=", n*m)
  rhs_W <- numeric(n*m)

  constr_C <- matrix(0, nrow = n*m, ncol = n_var)
  sense_C <- rep("=", n*m)
  rhs_C <- numeric(n*m)

  constr_working <- matrix(0, nrow = n*m, ncol = n_var)
  sense_working <- rep(">=", n*m)
  rhs_working <- numeric(n*m)

  constr_no_content <- numeric(n_var)
  sense_no_content <- "="
  rhs_no_content <- 1

  constr_content <- numeric(n_var)
  sense_content <- "<="
  rhs_content <- numeric(1)

  constr_content_yes_no <- matrix(0, nrow = m, ncol = n_var)
  sense_content_yes_no <- rep(">=", m)
  rhs_content_yes_no <- numeric(m)

  constr_one_exit <- matrix(0, nrow = n, ncol = n_var)
  sense_one_exit <- rep("=", n)
  rhs_one_exit <- numeric(n)

  constr_start_end <- matrix(0, nrow = n, ncol = n_var)
  sense_start_end <- rep(">=", n)
  rhs_start_end <- numeric(n)

  constr_flight_time <- matrix(0, nrow = n, ncol = n_var)
  sense_flight_time <- rep("<=", n)
  rhs_flight_time <- DFP-CTFP

  constr_n_aircraft_min <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aircraft_min <- rep(">=", m)
  rhs_n_aircraft_min <- numeric(m)

  constr_n_aircraft_max <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aircraft_max <- rep("<=", m)
  rhs_n_aircraft_max <- numeric(m)


  for(i in 1:n){
    for(t in 1:m){
      #=========================================================================
      # var W {i in I, t in T} = W_fix[i,t] * (sum{j in 1..t} S_fix[i,j]*Z[i] - sum{j in 1..(t-1): j+FBRP<=m} E[i,j+FBRP])
      #-------------------------------------------------------------------------
      constr_W[t+(i-1)*m, varh1_i("W",c(i,t),n,m)]   <-  1 # W[i,t]
      constr_W[t+(i-1)*m, varh1_i("Z",c(i),n,m)] <- - W_fix[i,t]*sum(S_fix[i,1:t]) # sum{j in 1..t} W_fix[i,t]*S_fix[i,j]*Z[i]

      for(j in 1:(t-1)){
        if(j+FBRP<=m){
          constr_W[t+(i-1)*m, varh1_i("E",c(i,j+FBRP),n,m)] <-  W_fix[i,t] # - sum{j in 1..(t-1): j+FBRP<=m} W_fix[i,t]*E[i,j+FBRP])
        }
      }
      #=========================================================================

      #=========================================================================
      # var U {i in I, t in T} = sum{j in 1..t} S_fix[i,j]*Z[i] - sum{j in 1..(t-1)} E[i,j];
      #-------------------------------------------------------------------------
      constr_C[t+(i-1)*m, varh1_i("U",c(i,t),n,m)]   <-  1 # var U {i in I, t in T}
      constr_C[t+(i-1)*m, varh1_i("Z",c(i),n,m)]   <- -sum(S_fix[i,1:t]) # - sum{j in 1..t} S_fix[i,j]*Z[i]
      for(j in 1:(t-1)){
        if(j>0){
          constr_C[t+(i-1)*m, varh1_i("E",c(i,j),n,m)] <-  1 # + sum{j in 1..(t-1)} E[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to working {i in I, t in T}:
      #   U[i,t] >= W[i,t]
      #-------------------------------------------------------------------------
      constr_working[t+(i-1)*m, varh1_i("U",c(i,t),n,m)] <- 1  # U[i,t]
      constr_working[t+(i-1)*m, varh1_i("W",c(i,t),n,m)] <- -1 # - W[i,t]
      #=========================================================================

      #=========================================================================
      # minimize Cost: Coste + Penalization =
      #   sum{i in I, t in T} C[i]*U[i,t] + sum{t in T} NVC[t]*Y[t-1] + sum{i in I} P[i]*Z[i]
      #   + sum{t in T} M_prime*mu[t] + sum{t in T} M_prime/2*MU[t] + Y[m]
      #-------------------------------------------------------------------------
      cost[varh1_i("U",c(i,t),n,m)] <- C[i]     # sum{i in I, t in T} C[i]*U[i,t]
      cost[varh1_i("Y",c(t-1),n,m)] <- NVC[t]   # + sum{t in T} NVC[t]*Y[t-1]
      cost[varh1_i("Z",c(i),n,m)]   <- P[i]     # + sum{i in I} P[i]*Z[i]

      penalization[varh1_i("mu",c(t),n,m)] <- M_prime   # + sum{t in T} M_prime*mu[t]
      penalization[varh1_i("MU",c(t),n,m)] <- 10*M_prime   # + sum{t in T} 10*M_prime*MU[t]
      penalization[varh1_i("Y", c(m),n,m)] <- 1         # + Y[m]
      #=========================================================================

      #=========================================================================
      # subject to no_content:
      #   Y[0] = 1;
      #-------------------------------------------------------------------------
      constr_no_content[varh1_i("Y", 0,n,m)] <- 1 # Y[0]
      #=========================================================================

      #=========================================================================
      # subject to content:
      #   sum{t in T} PER[t]*Y[t-1] <= sum{i in I, t in T} PR[i,t]*W[i,t]

      #-------------------------------------------------------------------------
      constr_content[varh1_i("Y",c(t-1),n,m)] <-  PER[t]  #   sum{t in T} PER[t]*Y[t-1]
      constr_content[varh1_i("W",c(i,t),n,m)] <- -PR[i,t] # - sum{i in I, t in T} PR[i,t]*W[i,t]
      #=========================================================================

      #=========================================================================
      # subject to content_yes_no {t in T}:
      #  sum{i in I, j in 1..t} PR[i,j]*W[i,j] >= SP[t]*Y[t-1] - M*Y[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        for(j in 1:t){
          constr_content_yes_no[t, varh1_i("W",c(i1,j),n,m)] <- PR[i1, j] # sum{i in I, j in 1..t} PR[i,j]*W[i,j]
        }
      }
      constr_content_yes_no[t, varh1_i("Y",c(t-1),n,m)]      <- -SP[t]    # - SP[t]*Y[t-1]
      constr_content_yes_no[t, varh1_i("Y",c(t),n,m)]        <-  M        # + M*Y[t]
      #=========================================================================

      #=========================================================================
      # subject to one_exit {i in I}:
      #   sum{t in T} E[i,t] = Z[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_one_exit[i, varh1_i("E",c(i,j),n,m)] <- 1 # sum{t in T} E[i,t]
      }
      constr_one_exit[i, varh1_i("Z",c(i),n,m)] <- -1 # - Z[i]
      #=========================================================================

      #=========================================================================
      # subject to start_end {i in I}:
      #   sum{t in T} t*E[i,t] >= sum{t in T} t*S_fix[i,t]*Z[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_start_end[i, varh1_i("E",c(i,j),n,m)] <- j # sum{t in T} t*E[i,t]
      }
      constr_start_end[i, varh1_i("Z",c(i),n,m)] <- -sum((1:m)*S_fix[i,]) # - sum{t in T} t*S_fix[i,t]*Z[i]
      #=========================================================================

      #=========================================================================
      # subject to flight_time {i in I}:
      #   sum{t in T} U[i,t] <= DFP-CTFP[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_flight_time[i, varh1_i("U",c(i,j),n,m)] <- 1 # sum{t in T} U[i,t]
      }
      #=========================================================================

      #=========================================================================
      # subject to n_aircraft_min {t in T}:
      #   sum{i in I} W[i,t] >= nMin[t]*Y[t-1] - mu[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aircraft_min[t, varh1_i("W",c(i1,t),n,m)] <- 1     # sum{i in I} W[i,t]
      }
      constr_n_aircraft_min[t, varh1_i("Y",c(t-1),n,m)] <- -nMin[t] # - nMin[t]*Y[t-1]
      constr_n_aircraft_min[t, varh1_i("mu",c(t),n,m)]  <-  1       # + mu[t]
      #=========================================================================

      #=========================================================================
      # subject to n_aircraft_max {t in T}:
      #   sum{i in I} W[i,t] <= nMax*Y[t-1] + MU[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aircraft_max[t, varh1_i("W",c(i1,t),n,m)] <- 1  # sum{i in I} W[i,t]
      }
      constr_n_aircraft_max[t, varh1_i("Y",c(t-1),n,m)] <- -nMax # - nMax*Y[t-1]
      constr_n_aircraft_max[t, varh1_i("MU",c(t),n,m)]  <- -1    # - MU[t]
      #=========================================================================
    }
  }

  obj = cost+penalization

  constr = rbind(constr_W,
                  constr_C,
                  constr_working,
                  constr_no_content,
                  constr_content,
                  constr_content_yes_no,
                  constr_one_exit,
                  constr_start_end,
                  constr_flight_time,
                  constr_n_aircraft_min,
                  constr_n_aircraft_max
  )

  sense = c(sense_W,
             sense_C,
             sense_working,
             sense_no_content,
             sense_content,
             sense_content_yes_no,
             sense_one_exit,
             sense_start_end,
             sense_flight_time,
             sense_n_aircraft_min,
             sense_n_aircraft_max
  )

  rhs = c(rhs_W,
           rhs_C,
           rhs_working,
           rhs_no_content,
           rhs_content,
           rhs_content_yes_no,
           rhs_one_exit,
           rhs_start_end,
           rhs_flight_time,
           rhs_n_aircraft_min,
           rhs_n_aircraft_max
  )
  require_gurobi=require("gurobi")
  if(solver=="gurobi" &
     requireNamespace("slam", quietly = TRUE) &
     require_gurobi){

    heuristic<-list()
    heuristic$A<-constr
    heuristic$obj<-obj
    heuristic$sense<-sense
    heuristic$rhs<-rhs
    heuristic$vtypes<-type
    heuristic$lb<-numeric(n_var)
    heuristic$modelsense<-"min"

    sol<-gurobi(heuristic, solver_params)
    x<-sol$x
    obj_value <- sol$objval
    sol_result<-sol$status
  }else if(solver=="lpSolve" &
           requireNamespace("lpSolveAPI", quietly = TRUE)){

    heuristic<-make.lp(n_cons, n_var)

    set.objfn(heuristic, obj)

    for(j in 1:n_cons) set.row(heuristic, j, constr[j,])
    set.rhs(heuristic, rhs)
    set.constr.type(heuristic, sense)

    type_C <- which(type=="C")
    type_B <- which(type=="B")

    set.type(heuristic, type_C, "real")
    set.type(heuristic, type_B, "binary")

    resolver<-solve(heuristic)
    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
    obj_value <- get.objective(heuristic)
    x<-get.variables(heuristic)
  }else if(solver=="Rsymphony" &
              requireNamespace("Rsymphony", quietly = TRUE)){

    sense[sense=="="] <- "=="

    sol <- Rsymphony_solve_LP(obj, constr, sense, rhs, types = type, max = F)

    obj_value <- sol$objval
    x <- sol$solution
    resolver <- sol$status

    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
  }

  if(sol_result=="OPTIMAL"){
    #  E[i,t] : 0*(n*m)+t+(i-1)*m
    E = matrix(x[1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(E) <- I
    colnames(E) <- Periods

    #  W[i,t] : 1*(n*m)+t+(i-1)*m
    W = matrix(x[1*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(W) <- I
    colnames(W) <- Periods

    # U[i,t] : 2*(n*m)+t+(i-1)*m
    U = matrix(x[2*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(U) <- I
    colnames(U) <- Periods

    # Z[i]   : 3*(n*m)+i
    Z = matrix(x[3*(n*m)+1:(n)],nrow = n, byrow = T)
    row.names(Z) <- I

    #  mu[t]   : 3*(n*m)+n+t
    mu = matrix(x[3*(n*m)+n+1:(m)], ncol = m, byrow = T)
    colnames(mu) <- Periods

    #  MU[t]   : 3*(n*m)+n+m+t
    MU = matrix(x[3*(n*m)+n+m+1:(m)], ncol = m, byrow = T)
    colnames(MU) <- Periods

    #   Y[t-1] : 7*(n*m)+n+m+t
    #   Y[m]   : 7*(n*m)+n+m+m+1
    Y = matrix(x[3*(n*m)+n+2*m+1:(m+1)], ncol = m+1, byrow = T)
    colnames(Y) <- c("0",Periods)

    results <- list(sol_result=sol_result,
                    obj=obj_value,
                    cost=t(cost)%*%x,
                    penalty=t(penalization)%*%x,
                    E=E,
                    W=W,
                    U=U,
                    Z=Z,
                    mu=mu,
                    MU=MU,
                    Y=Y)

    return(results)
  }else{
    return(list(sol_result="INFEASIBLE"))
  }
}
