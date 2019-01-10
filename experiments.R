#load packages

library(GA)
library(cec2013)

function_indexes <- seq(from = 1, to = 28, length = 28)
function_optima <- c(seq(from = -1400, to = -100, length = 14),
                     seq(from = 100, to = 1400, length = 14))
function_dim = 10

runs_num = 50
functions_num = 28
dimensions_num = 1
set.seed(1)

populationSize = 50
probabilityCrossover = 0.8
probabilityMutation = 0.1



for(i in 1:functions_num)
{
  results = c()
  for(j in 1:runs_num)
  {
    f <- function(z) {
      cec_val <- cec2013(i, z)
      fit_func <- -1 * (cec_val - function_optima[i])
      return(pmax(fit_func, -1e12))
    }
    
    result <- ga(type="real-valued",
                 maxiter = 100,
                 fitness=f,
                 popSize=populationSize,
                 pcrossover = probabilityCrossover, 
                 pmutation = probabilityMutation,
                 lower=rep(-100, function_dim),
                 upper=rep(100, function_dim))
    solution <- summary(result)$solution
    error <- cec2013(i, solution) - function_optima[i]
    results <- c(results, error[1])
  }
  print(results)
}