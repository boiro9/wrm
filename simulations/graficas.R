


results <- read.table("results.csv", header = T, sep = ";")

opt_e <- results["status_e"]=="OPTIMAL"
optimality_e <- numeric(dim(results)[1])
optimality_e[opt_e] <- 100
results["status_e"] <- optimality_e

opt_h <- results["status_h"]=="OPTIMAL"
optimality_h <- numeric(dim(results)[1])
optimality_h[opt_h] <- 100
results["status_h"] <- optimality_h

library(dplyr)
library(plotly)


results <- read.table("results_format.csv", header = T, sep = ";")

results <- results[results$num_air%%10==0 & results$num_per%%10==0,]

infeasibles = results[results$status == "INFEASIBLE", c("iter","num_air","num_per")]

for(i in seq(1,dim(infeasibles)[1])){
  results = results[!(results$iter==infeasibles$iter[i] & results$num_air==infeasibles$num_air[i] & results$num_per==infeasibles$num_per[i]),]
}



results$index = paste("A", results$num_air, "P", results$num_per, sep="")

results <- results[results$method=="Exact",]

plot_ly(results, x=~index, y=~time, color = I("black"), type = "box") %>% layout(xaxis=list(title=""), yaxis=list(title="time (s)"))
plot_ly(results, x=~index, y=~cost, color=~method, type = "box")


