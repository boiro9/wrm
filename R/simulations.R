# ------------------------------------------------------------------------------
#' Simulates and solves a battery of problems.
#'
#' @param list_num_air number of aircrafts list.
#' @param list_num_per number of periods list.
#' @param n_sims number of simulations.
#' @param seed seed number.
#' @param outfile output file. 
#'
#' @return two csv files: ´´results.csv´´ with iterations information and ´´summary.csv´´ with a summary of these iterations results.
#' @export
#'
#' @examples
#' execute_instances(c(5, 10, 15), c(5, 10, 15), 10)
simulations <- function(list_num_air, list_num_per, n_sims, seed=1, outfile="results.csv"){
  counter = 0
  for(num_air in list_num_air){
    for(num_per in list_num_per){
      for(i in seq(n_sims)){
        set.seed(seed)
        seed = seed+1
        counter = counter+1
        cat(" ============================================================================== \n")
        cat("Aircrafts: ", num_air)
        cat(" Periods: ", num_per)
        cat(" Iteration: ", i, "\n")
        cat(" ------------------------------------------------------------------------------ \n")
        data = random_instance(num_air, num_per)
        
        cat("Solving exact problem\n")
        results_e = asa::exact_model(data, M_prime = 10000, solver="gurobi")

        sub_count = 1
        while(results_e$sol_result != 'OPTIMAL'){
          seed = seed+1
          cat("Repeat: ", sub_count, "\n")
          data = random_instance(num_air, num_per)
          results_e = asa::exact_model(data, M_prime = 10000, solver="gurobi")
          sub_count = sub_count+1
        }
        
        cat("Status:\t", results_e$sol_result, "\n")
        cat(" ------------------------------------------------------------------------------ \n")
        
        cat("Solving heuristic problem\n")
        results_h = asa::heuristic_model(data, M_prime = 10000, niters = 10, solver="gurobi")
        cat("Status:\t", results_h$sol_result, "\n")
        cat(" ------------------------------------------------------------------------------ \n\n")
        
        results = data.frame(matrix(ncol=11, nrow=1))
        colnames(results) <- c("iter", "num_air", "num_per", 
                               "cost_e", "cost_h", "cost_prop", "time_e",
                               "time_h", "time_prop", "status_e", "status_h")
        
        results[1, "iter"] = i
        results[1, "num_air"] = num_air
        results[1, "num_per"] = num_per
        results[1, "cost_e"] = results_e$cost[1]
        results[1, "cost_h"] = results_h$cost[1]
        results[1, "cost_prop"] = results_h$cost[1]/results_e$cost[1]
        results[1, "time_e"] = as.numeric(results_e$time)
        results[1, "time_h"] = as.numeric(results_h$time)
        results[1, "time_prop"] = as.numeric(results_e$time)/as.numeric(results_h$time)
        results[1, "status_e"] = results_e$sol_result
        results[1, "status_h"] = results_h$sol_result
        
        if(counter==1){
          write.table(results, file = outfile, sep = ";", 
                      row.names = FALSE, col.names = TRUE)
        }else{
          write.table(results, file = outfile, sep = ";", 
                      row.names = FALSE, col.names = FALSE, append = TRUE)
        }
      }
    }
  }
}


#' Summarize simulation results.
#'
#' @param csv_file csv file input.
#' @param out_file csv file output
#'
#' @return csv with the summary of each instance.
#' @export
summary_table <- function(csv_file, out_file="summary.csv"){
  
  results <- read.table(csv_file, header = T, sep = ";")
  
  opt_e <- results["status_e"]=="OPTIMAL"
  optimality_e <- numeric(dim(results)[1])
  optimality_e[opt_e] <- 100
  results["status_e"] <- optimality_e
  
  opt_h <- results["status_h"]=="OPTIMAL"
  optimality_h <- numeric(dim(results)[1])
  optimality_h[opt_h] <- 100
  results["status_h"] <- optimality_h
  
  summary_results = aggregate(results[, 6:11], 
                              list(num_air=results$num_air, 
                                   num_per=results$num_per), 
                              mean, na.rm=T)
  
  write.table(summary_results, file = out_file, sep = ";", 
              row.names = FALSE, col.names = TRUE)
} 