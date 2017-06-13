# ---------------------------------------------------------------------------- #
# Heuristic
# ---------------------------------------------------------------------------- #

#' Model to solve the problem of Aircraft Selection and Allocation (ASA) for the containment of a wildfire in a heuristic way.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param M_prime penalization for the breach of the minimum aircrafts on a wildfire.
#' @param niters maximum number of iterations.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#' @param verbose logging level information. If 0 no information.
#'
#' @return information about the selection and allocation of the aircrafts.
#'
#' @export
#'
#' @examples
#' data <- example_data()
#' asa::heuristic_model(data, solver="lpSolveAPI")
heuristic_model <- function(data, M_prime=1000000, niters=10, solver="gurobi", solver_params=list(TimeLimit=600, OutputFlag=0), verbose=0){
  #-----------------------------------------------------------------------------
  # Start time
  #-----------------------------------------------------------------------------

  start.time <- Sys.time()

  #-----------------------------------------------------------------------------
  # Load data
  #-----------------------------------------------------------------------------
  I=data$I
  Periods=data$Periods
  FP=data$FP
  RP=data$RP
  DFP=data$DFP
  FBRP=data$FBRP
  A=data$A
  CFP=data$CFP
  CRP=data$CRP
  CTFP=data$CTFP
  C=data$C
  P=data$P
  BPR=data$BPR
  SP=data$SP
  NVC=data$NVC
  EF=data$EF
  nMax=data$nMax
  nMin=data$nMin


  #-----------------------------------------------------------------------------
  # Numero aeronaves y periodos
  #-----------------------------------------------------------------------------

  n<-length(I)        # Number of aircrafts
  m<-length(Periods) # Number of periods


  #-----------------------------------------------------------------------------
  # Compute model params
  #-----------------------------------------------------------------------------
  params <- get_model_params(data)
  PR = params$PR

  #-----------------------------------------------------------------------------
  # Other ones
  #-----------------------------------------------------------------------------
  # Penalizations
  M_prime <- max(100*(sum(C)+sum(NVC)), M_prime)
  M <- max(SP)+sum(EF%*%t(PR))

  # Selected aircrafts (all)
  I_select = rep(T,n)

  # Start period
  S_fix_hist <- list()
  S_fix = matrix(c(rep(1,n), numeric(n*m-n)), nrow=n, ncol=m)
  row.names(S_fix) <- I

  # Work matrix
  W_fix = matrix(0, nrow=n, ncol=m)

  # Fly matrix
  FL_fix = matrix(0, nrow=n, ncol=m)

  # Rest matrix
  R_fix = matrix(0, nrow=n, ncol=m)

  # Update Work matrix
  up_WFR = update_work(I_select, m, S_fix, W_fix, FL_fix, R_fix, params)

  W_fix <- up_WFR$W_fix
  FL_fix <- up_WFR$FL_fix
  R_fix <- up_WFR$R_fix

  # Update the minimum number of aircrafts
  params$nMin <- update_nMin(nMin, W_fix, FBRP, m)


  #-----------------------------------------------------------------------------
  # Solve Selection model
  #-----------------------------------------------------------------------------
  sol1 = contention_model(W_fix, S_fix, params, M, M_prime, solver, solver_params)

  iter = 0
  if(sol1$sol_result=="OPTIMAL"){
    #-----------------------------------------------------------------------------
    # Selection
    #-----------------------------------------------------------------------------
    I_select = sol1$Z==1
    m_select = max(which(sol1$Y==1))
    feas <- sum(sol1$mu>0) + sum(sol1$MU>0)
    sol_result <- sol1$sol_result

    #-----------------------------------------------------------------------------
    # Bucle until the model changed to feasible
    #-----------------------------------------------------------------------------
    while(feas>0 & iter<=niters & sol_result=="OPTIMAL"){
      if(verbose>0){
        cat("----------------------------------------\n")
        cat("Iter:", iter,"\n")
        cat("Selection:", I[I_select], "\n")
        cat("Contention Period:", m_select, "\n")
        cat("Feasibility:", feas, "\n")
        cat("----------------------------------------\n\n")
      }
      iter <- iter+1

      #---------------------------------------------------------------------------
      # Solve rest model
      #---------------------------------------------------------------------------
      S_select <- S_fix
      S_select[!I_select] <- 0
      S_fix_hist[[iter]] <- S_select

      sol2 = rest_model(I_select, m_select, S_fix_hist, params, M_prime, solver, solver_params)

      #-------------------------------------------------------------------------
      # If the solution is OPTIMAL
      #-------------------------------------------------------------------------
      sol_result <- sol2$sol_result

      if(sol_result=="OPTIMAL"){
        #-----------------------------------------------------------------------
        # Update rest params
        #-----------------------------------------------------------------------

        I_select[I_select] <- rowSums(sol2$S) == 1

        S_fix[I_select, 1:m_select] <- sol2$S[rowSums(sol2$S) == 1, ]
        FL_fix[I_select, ] <- 0
        R_fix[I_select, ] <- 0

        up_WFR = update_work(I_select, m, S_fix, W_fix, FL_fix, R_fix, params)

        W_fix <- up_WFR$W_fix
        FL_fix <- up_WFR$FL_fix
        R_fix <- up_WFR$R_fix

        #-------------------------------------------------------------------------
        # Solve Selection model
        #-------------------------------------------------------------------------
        sol1 = contention_model(W_fix, S_fix, params, M, M_prime, solver, solver_params)

        #-------------------------------------------------------------------------
        # If the solution is OPTIMAL
        #-------------------------------------------------------------------------
        sol_result <- sol1$sol_result
        if(sol_result=="OPTIMAL"){
          #-----------------------------------------------------------------------
          # Update selection params
          #-----------------------------------------------------------------------
          I_select = sol1$Z==1
          m_select = max(which(sol1$Y==1))
          feas <- sum(sol1$mu>0) + sum(sol1$MU>0)
        }
      }
    }

    #-----------------------------------------------------------------------------
    # Solution
    #-----------------------------------------------------------------------------
    if(sol_result=="OPTIMAL"){
      if(feas==0){
        if(verbose>0){
          cat("========================================\n")
          cat("FEASIBLE SOLUTION\n")
          cat("----------------------------------------\n")
          cat("Iter:", iter,"\n")
          cat("Cost:", sol1$cost,"\n")
          cat("Selection:", I[I_select], "\n")
          cat("Contention Period:", m_select, "\n")
          cat("Feasibility:", feas, "\n")
          cat("========================================\n")
        }
        sol<-list(
          model="heuristic",
          iter=iter,
          time=difftime(Sys.time(), start.time, units="secs"),
          feas=feas,
          sol_result=sol_result,
          obj=sol1$obj,
          cost=sol1$cost,
          penalty=sol1$penalty,
          Start=S_fix,
          End=sol1$E,
          Rest=R_fix*sol1$U,
          Fly=sol1$U-R_fix*sol1$U-sol1$W,
          Work=sol1$W,
          Scheduling=sol1$U,
          Selection=sol1$Z,
          mu=sol1$mu,
          MU=sol1$MU,
          Y=sol1$Y
        )

      }else{
        sol<- list(model="heuristic",
                   sol_result="INFEASIBLE", 
                   solver_result="MAXITERS",
                   cost=NA,
                   time = difftime(Sys.time(), start.time, units="secs")
                   )
      }
    }else{
      sol<- list(model="heuristic",
                 sol_result="INFEASIBLE", 
                 solver_result="INFEASIBLE, MAXITERS",
                 cost=NA,
                 time = difftime(Sys.time(), start.time, units="secs")
      )
    }

  }else{
    sol<- list(model="heuristic",
               sol_result="INFEASIBLE", 
               solver_result="INFEASIBLE",
               cost=NA,
               time = difftime(Sys.time(), start.time, units="secs")
    )
  }
  return(sol)
}

